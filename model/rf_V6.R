##############################
## 0. 安装并加载所需包
##############################
needed_pkgs <- c("caret", "randomForest", "MLmetrics", "e1071", "ggplot2", 
                 "purrr", "dplyr", "tidyr", "readxl", "magrittr", "ROCR","xlsx")
new_pkgs <- needed_pkgs[!(needed_pkgs %in% installed.packages()[,"Package"])]
if(length(new_pkgs)) install.packages(new_pkgs)

# 检查并安装可选包
if (!require("rmda")) {
  install.packages("rmda")
}

# 加载所有包
lapply(needed_pkgs, library, character.only = TRUE)
library(rmda)

set.seed(100)  # 可复现的随机种子
NEWDATA=F
BALANCE_SAMPLE=F
SUB_THRESHOLD=F
SUB_ROC=F
##############################
## 1. 数据预处理
##############################
# 综合模型
# patient_df <- read_excel("../data/202601/ccp_lasso.xlsx")
# 小模型
patient_df <- read_excel('../data/202601/single_cohort_lasso.xlsx')
predictors <- setdiff(names(patient_df), "psoriasis")  # 目标变量是 "psoriasis"
p <- length(predictors)

identify_categories <- function(x) {
  category_threshold <- 9
  if (is.numeric(x)) {
    return(length(unique(x)) <= category_threshold)
  }
  return(TRUE)
}
category_vars <- names(patient_df)[sapply(patient_df, identify_categories)]
category_vars <- setdiff(category_vars, "psoriasis")  # 确保psoriasis不被当作分类变量
category_vars <- setdiff(category_vars, "Control")  # 确保psoriasis不被当作分类变量
patient_df[category_vars] <- lapply(patient_df[category_vars], as.factor)
patient_df$psoriasis <- factor(patient_df$psoriasis, 
                               levels = c("0", "1"), 
                               labels = c("No", "Yes"))

##############################
## 2. 自定义 F1-score 评估函数
##############################
f1_summary <- function(data, lev = NULL, model = NULL) {
  f1 <- F1_Score(y_true = data$obs,
                 y_pred = data$pred,
                 positive = "Yes")
  c(F1 = f1)
}

##############################
## 3. 五折交叉验证参数设定
##############################
get_balanced_index <- function(x,name,value){
  # name = as.formula(name)
  idx_a0 <- which(x[name] == value)
  idx_a1 <- which(x[name] != value)
  folds_a0 <- createFolds(x$psoriasis[idx_a0], k = 5, returnTrain = FALSE)
  folds_a1 <- createFolds(x$psoriasis[idx_a1], k = 5, returnTrain = FALSE)
  index_list <- list()
  for (i in 1:5) {
    index_list[[i]] <- c(idx_a0[folds_a0[[i]]], idx_a1[folds_a1[[i]]])
  }
  return(index_list)
}
if (BALANCE_SAMPLE){
  test_ids <- get_balanced_index(patient_df,"Control",0)
  all_idx <- seq_len(nrow(patient_df))
  train_ids <- lapply(test_ids, function(val_idx) setdiff(all_idx, val_idx))
  patient_df <- select(patient_df, -"Control")
  ctrl <- trainControl(method = "cv",
                       number = 5,
                       summaryFunction = f1_summary,
                       index = train_ids,
                       indexOut = test_ids,
                       classProbs = TRUE,
                       savePredictions = "final",
                       verboseIter = FALSE)
} else {
  patient_df <- select(patient_df, -"Control")
  ctrl <- trainControl(method = "cv",
                       number = 5,
                       summaryFunction = f1_summary,
                       classProbs = TRUE,
                       savePredictions = "final",
                       verboseIter = FALSE)
}

##############################
## 4. 训练随机森林模型 (Random Forest)
##############################
rf_grid <- expand.grid(mtry = seq(2, min(13, p), by = 2))
n_trees <- 300  # 增加树的数量以提高稳定性

rf_model <- train(psoriasis ~ .,
                  data = patient_df,
                  method = "rf",
                  trControl = ctrl,
                  tuneGrid = rf_grid,
                  ntree = n_trees,
                  importance = TRUE,
                  metric = "F1")

##############################
## 5. 提取评估结果并输出
##############################

# 提取交叉验证结果
cv_results <- rf_model$resample
mean_F1 <- mean(cv_results$F1)
best_F1 <- max(cv_results$F1)

cat("模型训练完成\n")
cat("最佳参数: mtry =", rf_model$bestTune$mtry, "\n")
cat("树的数量: ntree =", n_trees, "\n")
cat("五折平均 F1-score:", round(mean_F1, 3), "\n")
cat("最佳折 F1-score:", round(best_F1, 3), "\n")

# 提取最佳参数下的预测结果
best_params <- rf_model$bestTune
pred_best <- rf_model$pred %>%
  filter(mtry == best_params$mtry)

# 校准曲线
calib_data_rf <- pred_best %>%
  select(prob = Yes, truth = obs) %>%
  mutate(model = "Random Forest")

# 保存文件 - RDS 供多个模型汇总使用
saveRDS(calib_data_rf, file = "calibration_data_rf.rds")
##############################
## 6. ROC和PRC曲线绘制
##############################

# 6.1. 准备数据
pred_folds <- pred_best %>%
  group_split(Resample)

# 6.2. 手动计算每个fold的ROC和PRC曲线

# 存储所有fold的曲线数据
all_roc_data <- list()
all_prc_data <- list()
auc_values <- numeric(5)
prauc_values <- numeric(5)

for(i in 1:length(pred_folds)) {
  fold_data <- pred_folds[[i]]
  
  # 创建 prediction 对象
  pred_obj <- prediction(fold_data$Yes, fold_data$obs == "Yes")
  
  # ROC 曲线
  roc_perf <- performance(pred_obj, "tpr", "fpr")
  auc_perf <- performance(pred_obj, "auc")
  auc_values[i] <- auc_perf@y.values[[1]]
  
  all_roc_data[[i]] <- data.frame(
    fold = paste0("Fold ", i),
    fpr = roc_perf@x.values[[1]],
    tpr = roc_perf@y.values[[1]],
    auc = auc_values[i]
  )
  
  # PRC 曲线
  prc_perf <- performance(pred_obj, "prec", "rec")
  
  # 计算 PRAUC
  prauc_perf <- performance(pred_obj, "aucpr")
  prauc_values[i] <- prauc_perf@y.values[[1]]
  
  # 处理precision中的NA值（当recall=0时）
  precision_vals <- prc_perf@y.values[[1]]
  recall_vals <- prc_perf@x.values[[1]]
  
  # 移除NA值
  valid_idx <- !is.na(precision_vals)
  
  all_prc_data[[i]] <- data.frame(
    fold = paste0("Fold ", i),
    recall = recall_vals[valid_idx],
    precision = precision_vals[valid_idx],
    prauc = prauc_values[i]
  )
}

# 合并所有fold的数据
roc_plot_data <- bind_rows(all_roc_data) %>%
  mutate(fold_label = paste0(fold, " (AUROC = ", round(auc, 3), ")"))

prc_plot_data <- bind_rows(all_prc_data) %>%
  mutate(fold_label = paste0(fold, " (AUPRC = ", round(prauc, 3), ")"))

# 计算统计信息
mean_auroc <- mean(auc_values)
ci_auroc <- quantile(auc_values, c(0.025, 0.975))
mean_auprc <- mean(prauc_values)
ci_auprc <- quantile(prauc_values, c(0.025, 0.975))

# 6.3. 绘制 ROC 曲线
p_roc <- ggplot(roc_plot_data, aes(x = fpr, y = tpr, color = fold_label)) +
  geom_line(size = 1, alpha = 0.8) +
  geom_abline(linetype = "dashed", color = "grey50") +
  coord_equal() +
  labs(
    title = "Random Forest - 5-Fold Cross-Validation ROC Curves",
    subtitle = sprintf("Mean AUROC = %.3f (95%% CI: %.3f - %.3f)", 
                       mean_auroc, ci_auroc[1], ci_auroc[2]),
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = "Fold (AUROC)"
  ) +
  scale_color_viridis_d() +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.key.height = unit(1.2, "lines"),
    legend.box.background = element_rect(color = "black", size = 0.5),
    legend.margin = ggplot2::margin(10, 10, 10, 10),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = ggplot2::margin(10, 10, 10, 10)
  )

print(p_roc)
ggsave("ROC_5-Fold_RF.png", p_roc, width = 10, height = 7, dpi = 300)

# 6.4. 绘制 PRC 曲线

# 计算基线
total_positives <- sum(sapply(pred_folds, function(x) sum(x$obs == "Yes")))
total_samples <- sum(sapply(pred_folds, nrow))
baseline_precision <- total_positives / total_samples

p_prc <- ggplot(prc_plot_data, aes(x = recall, y = precision, color = fold_label)) +
  geom_line(size = 1, alpha = 0.8) +
  geom_hline(yintercept = baseline_precision, linetype = "dashed", color = "grey50") +
  annotate("text", x = 0.5, y = baseline_precision - 0.03, 
           label = paste("Random Classifier Baseline =", round(baseline_precision, 3)),
           color = "grey50", size = 3.5) +
  labs(
    title = "Random Forest - 5-Fold Cross-Validation PR Curves",
    subtitle = sprintf("Mean AUPRC = %.3f (95%% CI: %.3f - %.3f)", 
                       mean_auprc, ci_auprc[1], ci_auprc[2]),
    x = "Recall (Sensitivity)",
    y = "Precision",
    color = "Fold (AUPRC)"
  ) +
  scale_color_viridis_d() +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.key.height = unit(1.2, "lines"),
    legend.box.background = element_rect(color = "black", size = 0.5),
    legend.margin = ggplot2::margin(10, 10, 10, 10),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = ggplot2::margin(10, 10, 10, 10)
  ) +
  xlim(0, 1) + ylim(0, 1)

print(p_prc)
ggsave("PRC_5-Fold_RF.png", p_prc, width = 10, height = 7, dpi = 300)

# 6.5. 添加平均曲线的ROC图

# 计算平均 ROC 曲线
fpr_seq <- seq(0, 1, length.out = 100)
tpr_interp <- matrix(NA, nrow = length(fpr_seq), ncol = 5)

for(i in 1:5) {
  fold_roc <- all_roc_data[[i]]
  f <- approxfun(fold_roc$fpr, fold_roc$tpr, rule = 2)
  tpr_interp[, i] <- f(fpr_seq)
}

avg_roc <- data.frame(
  fpr = fpr_seq,
  tpr = rowMeans(tpr_interp),
  tpr_lower = apply(tpr_interp, 1, quantile, 0.025),
  tpr_upper = apply(tpr_interp, 1, quantile, 0.975)
)

# 带平均曲线的 ROC 图
p_roc_with_avg <- p_roc +
  geom_ribbon(data = avg_roc, 
              aes(x = fpr, ymin = tpr_lower, ymax = tpr_upper),
              fill = "black", alpha = 0.2, inherit.aes = FALSE) +
  geom_line(data = avg_roc, 
            aes(x = fpr, y = tpr),
            color = "black", size = 1.5, inherit.aes = FALSE)

print(p_roc_with_avg)
ggsave("ROC_5-Fold_RF_with_average.png", p_roc_with_avg, width = 8, height = 7, dpi = 300)

##############################
## 7. 基于Youden Index的验证分析
##############################

# 7.1 对每个fold计算最佳阈值和性能指标
fold_performance <- list()

for(i in 1:length(pred_folds)) {
  fold_data <- pred_folds[[i]]
  # 计算该fold的Youden Index
  thresholds <- unique(sort(fold_data$Yes, decreasing = TRUE))
  
  youden_results <- map_dfr(thresholds, function(thresh) {
    pred_bin <- ifelse(fold_data$Yes >= thresh, "Yes", "No")
    # 计算混淆矩阵
    cm <- table(Predicted = pred_bin, Actual = fold_data$obs)
    
    # 确保混淆矩阵完整
    if(nrow(cm) == 1 || ncol(cm) == 1) {
      return(NULL)
    }
    
    TP <- cm["Yes", "Yes"]
    FP <- cm["Yes", "No"]
    FN <- cm["No", "Yes"]
    TN <- cm["No", "No"]
    
    # 计算性能指标
    sensitivity <- TP / (TP + FN)
    specificity <- TN / (TN + FP)
    precision <- TP / (TP + FP)
    accuracy <- (TP + TN) / sum(cm)
    f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)
    youden <- sensitivity + specificity - 1
    
    tibble(
      threshold = thresh,
      sensitivity = sensitivity,
      specificity = specificity,
      precision = precision,
      accuracy = accuracy,
      f1_score = f1_score,
      youden = youden
    )
  }) %>%
    filter(!is.na(youden))
  
  # 找到最佳阈值
  best_threshold <- youden_results %>%
    filter(youden == max(youden, na.rm = TRUE)) %>%
    dplyr::slice(1)
  
  fold_performance[[i]] <- list(
    fold = i,
    best_threshold = best_threshold$threshold,
    sensitivity = best_threshold$sensitivity,
    specificity = best_threshold$specificity,
    precision = best_threshold$precision,
    accuracy = best_threshold$accuracy,
    f1_score = best_threshold$f1_score,
    youden_index = best_threshold$youden
  )
}

# 7.2 汇总5折的平均性能

performance_df <- bind_rows(fold_performance)

# 计算平均性能和标准差
avg_performance <- performance_df %>%
  summarise(
    mean_threshold = mean(best_threshold),
    sd_threshold = sd(best_threshold),
    mean_sensitivity = mean(sensitivity),
    sd_sensitivity = sd(sensitivity),
    mean_specificity = mean(specificity),
    sd_specificity = sd(specificity),
    mean_precision = mean(precision),
    sd_precision = sd(precision),
    mean_accuracy = mean(accuracy),
    sd_accuracy = sd(accuracy),
    mean_f1_score = mean(f1_score),
    sd_f1_score = sd(f1_score),
    mean_youden_index = mean(youden_index),
    sd_youden_index = sd(youden_index)
  )

# 7.3 输出验证结果
cat("\n", rep("=", 60), "\n", sep = "")
cat("基于Youden Index的5折交叉验证性能评估\n")
cat(rep("=", 60), "\n", sep = "")

cat("\n【各折详细结果】\n")
for(i in 1:5) {
  cat(sprintf("\nFold %d:\n", i))
  cat(sprintf("  最佳阈值: %.3f\n", fold_performance[[i]]$best_threshold))
  cat(sprintf("  灵敏度: %.3f\n", fold_performance[[i]]$sensitivity))
  cat(sprintf("  特异性: %.3f\n", fold_performance[[i]]$specificity))
  cat(sprintf("  精确度: %.3f\n", fold_performance[[i]]$precision))
  cat(sprintf("  准确率: %.3f\n", fold_performance[[i]]$accuracy))
  cat(sprintf("  F1-score: %.3f\n", fold_performance[[i]]$f1_score))
  cat(sprintf("  Youden Index: %.3f\n", fold_performance[[i]]$youden_index))
}

cat("\n【5折平均性能 (均值 ± 标准差)】\n")
cat(sprintf("最佳阈值: %.3f ± %.3f\n", avg_performance$mean_threshold, avg_performance$sd_threshold))
cat(sprintf("灵敏度 (Sensitivity): %.3f ± %.3f\n", avg_performance$mean_sensitivity, avg_performance$sd_sensitivity))
cat(sprintf("特异性 (Specificity): %.3f ± %.3f\n", avg_performance$mean_specificity, avg_performance$sd_specificity))
cat(sprintf("精确度 (Precision): %.3f ± %.3f\n", avg_performance$mean_precision, avg_performance$sd_precision))
cat(sprintf("准确率 (Accuracy): %.3f ± %.3f\n", avg_performance$mean_accuracy, avg_performance$sd_accuracy))
cat(sprintf("F1-score: %.3f ± %.3f\n", avg_performance$mean_f1_score, avg_performance$sd_f1_score))
cat(sprintf("Youden Index: %.3f ± %.3f\n", avg_performance$mean_youden_index, avg_performance$sd_youden_index))

# 7.4 可视化各折性能
performance_long <- performance_df %>%
  select(-fold, -best_threshold) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  mutate(metric = case_when(
    metric == "sensitivity" ~ "Sensitivity",
    metric == "specificity" ~ "Specificity",
    metric == "precision" ~ "Precision",
    metric == "accuracy" ~ "Accuracy",
    metric == "f1_score" ~ "F1-score",
    metric == "youden_index" ~ "Youden Index"
  ))

p_fold_performance <- ggplot(performance_long, aes(x = metric, y = value)) +
  geom_boxplot(fill = "#3498db", alpha = 0.7) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    title = "5-Fold Cross-Validation Performance Metrics",
    subtitle = "Based on Youden Index Optimal Thresholds",
    x = "Metric",
    y = "Value"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ylim(0, 1)

print(p_fold_performance)
ggsave("fold_performance_metrics_rf.png", p_fold_performance, width = 8, height = 6, dpi = 300)
saveRDS(performance_long,file="performance_rf.rds")
##############################
## 8. 变量重要性分析
##############################

merge_factor_importance <- function(importance_df, original_data, target_var = "psoriasis") {
  # 获取原始数据中的变量名（不包括目标变量）
  original_vars <- setdiff(names(original_data), target_var)
  
  # 获取分类变量列表
  factor_vars <- names(original_data)[sapply(original_data, is.factor)]
  factor_vars <- setdiff(factor_vars, target_var)
  
  # 创建映射函数
  map_to_original <- function(var_name) {
    # 首先检查是否是原始变量名
    if (var_name %in% original_vars) {
      return(var_name)
    }
    
    # 对于分类变量，检查是否是其子分层
    for (fvar in factor_vars) {
      # 检查多种可能的命名模式
      patterns <- c(
        paste0("^", fvar, "\\."),     # factor.level
        paste0("^", fvar, "[0-9]+$"), # factor1, factor2
        paste0("^", fvar, "[A-Za-z]+$") # factorA, factorB
      )
      
      if (any(sapply(patterns, function(p) grepl(p, var_name)))) {
        return(fvar)
      }
    }
    
    # 如果都不匹配，返回原始名称
    return(var_name)
  }
  
  # 应用映射
  importance_df$MainVariable <- sapply(importance_df$Variable, map_to_original)
  
  # 汇总重要性
  importance_summary <- importance_df %>%
    group_by(MainVariable) %>%
    summarise(
      Importance = mean(Importance),  # 使用sum合并
      n_sublevels = n()              # 记录子分层数量
    ) %>%
    rename(Variable = MainVariable) %>%
    arrange(desc(Importance))
  
  return(importance_summary)
}
# 提取变量重要性
rf_imp <- importance(rf_model$finalModel, type = 1)
rf_imp_df <- data.frame(
  Variable = rownames(rf_imp),
  Importance = rf_imp[, "MeanDecreaseAccuracy"]
)

# 合并子分层
rf_imp_summary <- merge_factor_importance(rf_imp_df, patient_df)

# 打印结果
print("随机森林特征重要性排名（合并后）：")
print(rf_imp_summary)

# 绘制随机森林重要性图
topN <- min(10, nrow(rf_imp_summary))
p_rf <- ggplot(rf_imp_summary[1:topN, ], 
               aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "#2ca02c", alpha = 0.8) +
  coord_flip() +
  labs(
    title = sprintf("Random Forest Variable Importance (Top %d)", topN),
    x = NULL,
    y = "Mean Decrease in Accuracy"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 10)
  )

print(p_rf)
ggsave("rf_variable_importance.png", p_rf, width = 8, height = 6, dpi = 300)
saveRDS(rf_imp_summary, "rf_var_important.rds")
##############################
## 9. 保存模型与结果
##############################

# 保存模型
saveRDS(rf_model, file = "rf_best_model.rds")

# 保存ROC数据
rf_roc_data <- list(
  roc_data = all_roc_data,
  prc_data = all_prc_data,
  auc_values = auc_values,
  prauc_values = prauc_values,
  mean_auroc = mean_auroc,
  mean_auprc = mean_auprc,
  ci_auroc = ci_auroc,
  ci_auprc = ci_auprc
)
saveRDS(rf_roc_data, file = "rf_roc_data.rds")


# 保存验证性能数据
validation_results <- list(
  fold_performance = fold_performance,
  performance_df = performance_df,
  avg_performance = avg_performance
)
saveRDS(validation_results, file = "rf_validation_results.rds")

cat("\n🟢 模型已保存为 rf_best_model.rds\n")
cat("🟢 ROC/PRC数据已保存为 rf_roc_data.rds\n")
cat("🟢 变量重要性已保存为 rf_variable_importance.rds\n")
cat("🟢 验证结果已保存为 rf_validation_results.rds\n")

##############################
## 10. 决策曲线分析（可选）
##############################

# 准备决策曲线分析所需的数据
dca_rf_df <- pred_best %>%
  select(prob = Yes, truth = obs) %>%
  mutate(
    model = "Random Forest",
    truth = ifelse(truth == "Yes", 1, 0)  # 转换为0/1格式
  )

saveRDS(dca_rf_df, "dca_rf_df.rds")
cat("🟢 决策曲线数据已保存为 dca_rf_df.rds\n")

##############################
## 11. 模型性能总结报告
##############################
cat("\n", rep("=", 60), "\n", sep = "")
cat("Random Forest 模型五折交叉验证性能总结\n")
cat(rep("=", 60), "\n", sep = "")

cat("\n1. 模型参数:\n")
cat("   - 方法: Random Forest\n")
cat("   - 最优参数: mtry =", rf_model$bestTune$mtry, "\n")
cat("   - 树的数量: ntree =", n_trees, "\n")

cat("\n2. ROC/PRC性能:\n")
cat("   - 平均 AUROC:", round(mean_auroc, 3), 
    sprintf("(95%% CI: %.3f-%.3f)\n", ci_auroc[1], ci_auroc[2]))
cat("   - 平均 AUPRC:", round(mean_auprc, 3), 
    sprintf("(95%% CI: %.3f-%.3f)\n", ci_auprc[1], ci_auprc[2]))

cat("\n3. 基于Youden Index的验证性能 (均值 ± 标准差):\n")
cat("   - 最佳阈值:", sprintf("%.3f ± %.3f\n", avg_performance$mean_threshold, avg_performance$sd_threshold))
cat("   - Sensitivity:", sprintf("%.3f ± %.3f\n", avg_performance$mean_sensitivity, avg_performance$sd_sensitivity))
cat("   - Specificity:", sprintf("%.3f ± %.3f\n", avg_performance$mean_specificity, avg_performance$sd_specificity))
cat("   - Precision:", sprintf("%.3f ± %.3f\n", avg_performance$mean_precision, avg_performance$sd_precision))
cat("   - Accuracy:", sprintf("%.3f ± %.3f\n", avg_performance$mean_accuracy, avg_performance$sd_accuracy))
cat("   - F1-score:", sprintf("%.3f ± %.3f\n", avg_performance$mean_f1_score, avg_performance$sd_f1_score))
cat("   - Youden Index:", sprintf("%.3f ± %.3f\n", avg_performance$mean_youden_index, avg_performance$sd_youden_index))

cat("\n5. 输出文件:\n")
cat("   - 模型文件: rf_best_model.rds\n")
cat("   - ROC/PRC数据: rf_roc_data.rds\n")
cat("   - 验证结果: rf_validation_results.rds\n")
cat("   - 决策曲线数据: dca_rf_df.rds\n")
cat("   - 变量重要性: rf_variable_importance.rds\n")
cat("   - 图形文件:\n")
cat("     * ROC_5-Fold_RF.png\n")
cat("     * PRC_5-Fold_RF.png\n")
cat("     * ROC_5-Fold_RF_with_average.png\n")
cat("     * fold_performance_metrics.png\n")
cat("     * variable_importance_rf.png\n")

cat("\n", rep("=", 60), "\n", sep = "")
# 完成提示
cat("\n✅ Random Forest模型分析完成！\n")


##### 如果还要计算分层指标,并以分层特征作为基准调整阈值   ###### 
##### 如果需要具备特定分层特征患者的ROC和PRC              ######
if (SUB_ROC){
  # 准备数据
pred_folds <- pred_best %>%
  group_split(Resample)

# 手动计算每个fold的ROC和PRC曲线

# 存储所有fold的曲线数据
all_roc_data <- list()
all_prc_data <- list()
auc_values <- numeric(5)
prauc_values <- numeric(5)

for(i in 1:length(pred_folds)) {
  select_fold_data <- pred_folds[[i]]
  select_fold_data <- select_fold_data[patient_df[select_fold_data$rowIndex,]$Diagnosis == 2, ]
  # 创建 prediction 对象
  pred_obj <- prediction(select_fold_data$Yes, select_fold_data$obs == "Yes")
  
  # ROC 曲线
  roc_perf <- performance(pred_obj, "tpr", "fpr")
  auc_perf <- performance(pred_obj, "auc")
  auc_values[i] <- auc_perf@y.values[[1]]
  
  all_roc_data[[i]] <- data.frame(
    fold = paste0("Fold ", i),
    fpr = roc_perf@x.values[[1]],
    tpr = roc_perf@y.values[[1]],
    auc = auc_values[i]
  )
  
  # PRC 曲线
  prc_perf <- performance(pred_obj, "prec", "rec")
  
  # 计算 PRAUC
  prauc_perf <- performance(pred_obj, "aucpr")
  prauc_values[i] <- prauc_perf@y.values[[1]]
  
  # 处理precision中的NA值（当recall=0时）
  precision_vals <- prc_perf@y.values[[1]]
  recall_vals <- prc_perf@x.values[[1]]
  
  # 移除NA值
  valid_idx <- !is.na(precision_vals)
  
  all_prc_data[[i]] <- data.frame(
    fold = paste0("Fold ", i),
    recall = recall_vals[valid_idx],
    precision = precision_vals[valid_idx],
    prauc = prauc_values[i]
  )
}

# 合并所有fold的数据
roc_plot_data <- bind_rows(all_roc_data) %>%
  mutate(fold_label = paste0(fold, " (AUROC = ", round(auc, 3), ")"))

prc_plot_data <- bind_rows(all_prc_data) %>%
  mutate(fold_label = paste0(fold, " (AUPRC = ", round(prauc, 3), ")"))

# 计算统计信息
mean_auroc <- mean(auc_values)
ci_auroc <- quantile(auc_values, c(0.025, 0.975))
mean_auprc <- mean(prauc_values)
ci_auprc <- quantile(prauc_values, c(0.025, 0.975))

# 绘制 ROC 曲线
p_roc <- ggplot(roc_plot_data, aes(x = fpr, y = tpr, color = fold_label)) +
  geom_line(size = 1, alpha = 0.8) +
  geom_abline(linetype = "dashed", color = "grey50") +
  coord_equal() +
  labs(
    title = "Random Forest - 5-Fold Cross-Validation ROC Curves",
    subtitle = sprintf("Mean AUROC = %.3f (95%% CI: %.3f - %.3f)", 
                       mean_auroc, ci_auroc[1], ci_auroc[2]),
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = "Fold (AUROC)"
  ) +
  scale_color_viridis_d() +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.key.height = unit(1.2, "lines"),
    legend.box.background = element_rect(color = "black", size = 0.5),
    legend.margin = ggplot2::margin(10, 10, 10, 10),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = ggplot2::margin(10, 10, 10, 10)
  )

print(p_roc)
ggsave("ROC_5-Fold_RF_sub.png", p_roc, width = 10, height = 7, dpi = 300)

#  绘制 PRC 曲线

# 计算基线
total_positives <- sum(sapply(pred_folds, function(x) sum(x$obs == "Yes")))
total_samples <- sum(sapply(pred_folds, nrow))
baseline_precision <- total_positives / total_samples

p_prc <- ggplot(prc_plot_data, aes(x = recall, y = precision, color = fold_label)) +
  geom_line(size = 1, alpha = 0.8) +
  geom_hline(yintercept = baseline_precision, linetype = "dashed", color = "grey50") +
  annotate("text", x = 0.5, y = baseline_precision - 0.03, 
           label = paste("Random Classifier Baseline =", round(baseline_precision, 3)),
           color = "grey50", size = 3.5) +
  labs(
    title = "Random Forest - 5-Fold Cross-Validation PR Curves",
    subtitle = sprintf("Mean AUPRC = %.3f (95%% CI: %.3f - %.3f)", 
                       mean_auprc, ci_auprc[1], ci_auprc[2]),
    x = "Recall (Sensitivity)",
    y = "Precision",
    color = "Fold (AUPRC)"
  ) +
  scale_color_viridis_d() +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.key.height = unit(1.2, "lines"),
    legend.box.background = element_rect(color = "black", size = 0.5),
    legend.margin = ggplot2::margin(10, 10, 10, 10),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = ggplot2::margin(10, 10, 10, 10)
  ) +
  xlim(0, 1) + ylim(0, 1)

print(p_prc)
ggsave("PRC_5-Fold_RF_sub.png", p_prc, width = 10, height = 7, dpi = 300)
sub_rf_roc_data <- list(
  roc_data = all_roc_data,
  prc_data = all_prc_data,
  auc_values = auc_values,
  prauc_values = prauc_values,
  mean_auroc = mean_auroc,
  mean_auprc = mean_auprc,
  ci_auroc = ci_auroc,
  ci_auprc = ci_auprc
)
saveRDS(sub_rf_roc_data, file = "sub_rf_roc_data_sub.rds")

# 提取校准曲线
pred_best_subgroup <- pred_best %>%
  filter(patient_df[rowIndex, ]$Diagnosis == 2)
calib_data_sub_rf <- pred_best_subgroup %>%
  select(prob = Yes, truth = obs) %>%
  mutate(
    model = "Random Forest",
    subgroup = "Diagnosis == 2"
  )

saveRDS(calib_data_sub_rf, "calibration_data_rf_subgroup.rds")
}

# 首先计算分层特征导向的阈值
if (SUB_THRESHOLD){
  fold_performance_sub = list()
for(i in 1:length(pred_folds)) {
  select_fold_data <- pred_folds[[i]]
  select_fold_data <- select_fold_data[patient_df[select_fold_data$rowIndex,]$Diagnosis == 2, ]
  # 计算该fold的Youden Index
  thresholds <- unique(sort(select_fold_data$Yes, decreasing = TRUE))
  
  youden_results <- map_dfr(thresholds, function(thresh) {
    pred_bin <- ifelse(select_fold_data$Yes >= thresh, "Yes", "No")
    # 计算混淆矩阵
    cm <- table(Predicted = pred_bin, Actual = select_fold_data$obs)
    
    # 确保混淆矩阵完整
    if(nrow(cm) == 1 || ncol(cm) == 1) {
      return(NULL)
    }
    
    TP <- cm["Yes", "Yes"]
    FP <- cm["Yes", "No"]
    FN <- cm["No", "Yes"]
    TN <- cm["No", "No"]
    
    # 计算性能指标
    sensitivity <- TP / (TP + FN)
    specificity <- TN / (TN + FP)
    precision <- TP / (TP + FP)
    accuracy <- (TP + TN) / sum(cm)
    f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)
    youden <- sensitivity + specificity - 1
    
    tibble(
      threshold = thresh,
      sensitivity = sensitivity,
      specificity = specificity,
      precision = precision,
      accuracy = accuracy,
      f1_score = f1_score,
      youden = youden
    )
  }) %>%
    filter(!is.na(youden))
  
  # 找到最佳阈值
  best_threshold <- youden_results %>%
    filter(youden == max(youden, na.rm = TRUE)) %>%
    dplyr::slice(1)
  
  fold_performance_sub[[i]] <- list(
    fold = i,
    best_threshold = best_threshold$threshold,
    sensitivity = best_threshold$sensitivity,
    specificity = best_threshold$specificity,
    precision = best_threshold$precision,
    accuracy = best_threshold$accuracy,
    f1_score = best_threshold$f1_score,
    youden_index = best_threshold$youden
  )
}

sub_result_df <- bind_rows(fold_performance_sub)
avg_performance_sub <- sub_result_df %>%
  summarise(
    mean_sensitivity = mean(sensitivity),
    sd_sensitivity = sd(sensitivity),
    mean_specificity = mean(specificity),
    sd_specificity = sd(specificity),
    mean_precision = mean(precision),
    sd_precision = sd(precision),
    mean_accuracy = mean(accuracy),
    sd_accuracy = sd(accuracy),
    mean_f1_score = mean(f1_score),
    sd_f1_score = sd(f1_score),
    mean_youden_index = mean(youden_index),
    sd_youden_index = sd(youden_index)
  )
sub_performance_long <- sub_result_df %>%
  select(-fold, -best_threshold) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  mutate(metric = case_when(
    metric == "sensitivity" ~ "Sensitivity",
    metric == "specificity" ~ "Specificity",
    metric == "precision" ~ "Precision",
    metric == "accuracy" ~ "Accuracy",
    metric == "f1_score" ~ "F1-score",
    metric == "youden_index" ~ "Youden Index"
  ))

p_fold_performance_sub <- ggplot(sub_performance_long, aes(x = metric, y = value)) +
  geom_boxplot(fill = "#3498db", alpha = 0.7) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    title = "5-Fold Cross-Validation Performance Metrics",
    subtitle = "Based on Youden Index Optimal Thresholds",
    x = "Metric",
    y = "Value"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ylim(0, 1)

print(p_fold_performance_sub)
ggsave("sub_threshold_peformance_rf.png", p_fold_performance_sub, width = 10, height = 7, dpi = 300)
saveRDS(sub_performance_long,file="sub_performance_rf.rds")
saveRDS(sub_result_df, file="sub_threshold_rf.rds")
}


### 全量数据模型的新数据验证
if (NEWDATA){
  new_data <- read_xlsx("../data/select_data_model0806/new_data_mapped.xlsx")
  new_data[category_vars] <- lapply(new_data[category_vars], as.factor)
  new_data$psoriasis <- factor(new_data$psoriasis, 
                               levels = c("0", "1"), 
                               labels = c("No", "Yes"))
  
  new_pred <- predict(rf_model, new_data, type = "prob")
  if (BALANCE_SAMPLE){
    write.xlsx(new_pred$Yes, "../data/select_data_model0806/new_rf.xlsx", sheetName = "2",col.names=TRUE, row.names=FALSE, append=TRUE)
  }else{
    write.xlsx(new_pred$Yes, "../data/select_data_model0806/new_rf.xlsx", sheetName = "1",col.names=TRUE, row.names=FALSE, append=TRUE)
  }
  
}

