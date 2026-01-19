library(tidyverse)
library(ggplot2)
library(patchwork)
library(cowplot)
library(ggsci)
library(readxl)   # 用于读取Excel文件
library(pROC)     # 用于计算ROC曲线和DeLong检验
library(PRROC)    # 用于计算PR曲线

# ============= 配置区域 =============
# 选择数据读取模式: "rds" 或 "excel"
data_mode <- "excel"

# 是否执行DeLong检验
perform_delong <- TRUE

# ====== DeLong检验配置（新增） ======
# 检验类型选择:
#   "two.sided" - 双侧检验（默认）
#   "greater"   - 单侧检验，H1: model1 > model2
#   "less"      - 单侧检验，H1: model1 < model2
#   "auto"      - 自动选择：跨数据集用单侧(Cross > Single)，同数据集用双侧

delong_test_type <- "auto"  # 推荐使用 "auto"

# 定义特征集名称
feature_set_large_name <- "Cross cohort"
feature_set_small_name <- "Breast only"

# 模型名称前缀（用于区分不同数据集的模型）
prefix_large <- "Cross"
prefix_small <- "Single"

# ----------- RDS模式配置（保留原有配置） -----------
model_files_large <- c(
  "LR" = "ml/2026/cross/sub_ml_roc_data.rds",
  "SVM" = "svm/2026/cross/sub_svm_roc_data.rds",
  "AdaBoost" = "adaboost/2026/cross/sub_adaboost_roc_data.rds",
  "RF" = "rf/2026/cross/sub_rf_roc_data_sub.rds",
  "NB" = "nb/2026/cross/sub_nb_roc_data.rds",
  "XGB" = "xgboost/2026/cross/sub_xgboost_roc_data.rds"
)

model_files_small <- c(
  "LR" = "ml/2026/single/ml_roc_data.rds",
  "SVM" = "svm/2026/single/svm_roc_data.rds",
  "AdaBoost" = "adaboost/2026/single/Adaboost_roc_data.rds",
  "RF" = "rf/2026/single/rf_roc_data.rds",
  "NB" = "nb/2026/single/NB_roc_data.rds",
  "XGB" = "xgboost/2026/single/xgboost_roc_data.rds"
)

# ----------- Excel模式配置 -----------
excel_file_large <- "../data/202601/result/cross_prob.xlsx"
excel_file_small <- "../data/202601/result/single_prob.xlsx"

# 标签列名
label_col <- "true_labels"

# 模型概率列映射
prob_cols_mapping <- c(
  "AdaBoost" = "adb_prob",
  "RF" = "rf_prob",
  "XGB" = "xgb_prob"
)

# Bootstrap参数
n_bootstrap <- 1000
conf_level <- 0.95

# ============= 函数定义区域 =============

#' Bootstrap计算AUPRC置信区间
bootstrap_auprc <- function(true_labels, pred_probs, n_bootstrap = 1000, conf_level = 0.95) {
  set.seed(100)
  auprc_values <- numeric(n_bootstrap)
  n <- length(true_labels)
  
  for (i in 1:n_bootstrap) {
    idx <- sample(1:n, n, replace = TRUE)
    boot_labels <- true_labels[idx]
    boot_probs <- pred_probs[idx]
    
    if (length(unique(boot_labels)) < 2) next
    
    pr_obj <- pr.curve(
      scores.class0 = boot_probs[boot_labels == 1],
      scores.class1 = boot_probs[boot_labels == 0],
      curve = FALSE
    )
    auprc_values[i] <- pr_obj$auc.integral
  }
  
  auprc_values <- auprc_values[!is.na(auprc_values)]
  alpha <- 1 - conf_level
  ci <- quantile(auprc_values, probs = c(alpha/2, 1 - alpha/2))
  
  list(
    mean_auprc = mean(auprc_values),
    ci_auprc = as.numeric(ci)
  )
}

#' 从Excel读取数据并计算ROC/PRC曲线
process_excel_data <- function(excel_file, 
                               label_col = "true_labels",
                               prob_cols_mapping = NULL,
                               n_bootstrap = 1000,
                               conf_level = 0.95) {
  
  cat("Reading Excel file:", excel_file, "\n")
  data <- read_excel(excel_file)
  
  if (!label_col %in% names(data)) {
    stop(paste("Label column", label_col, "not found in Excel file"))
  }
  true_labels <- as.numeric(data[[label_col]])
  
  if (is.null(prob_cols_mapping)) {
    prob_cols <- grep("_prob$", names(data), value = TRUE)
    if (length(prob_cols) == 0) {
      stop("No columns ending with '_prob' found in Excel file")
    }
    prob_cols_mapping <- setNames(prob_cols, gsub("_prob$", "", prob_cols))
    cat("Auto-detected model columns:", paste(prob_cols, collapse = ", "), "\n")
  } else {
    missing_cols <- setdiff(prob_cols_mapping, names(data))
    if (length(missing_cols) > 0) {
      stop(paste("Columns not found:", paste(missing_cols, collapse = ", ")))
    }
  }
  
  all_roc <- list()
  all_prc <- list()
  model_stats <- data.frame()
  roc_objects <- list()
  
  raw_data <- list(
    labels = true_labels,
    predictions = list()
  )
  
  for (model_name in names(prob_cols_mapping)) {
    col_name <- prob_cols_mapping[model_name]
    pred_probs <- as.numeric(data[[col_name]])
    
    cat("Processing model:", model_name, "\n")
    
    valid_idx <- !is.na(true_labels) & !is.na(pred_probs)
    labels_clean <- true_labels[valid_idx]
    probs_clean <- pred_probs[valid_idx]
    
    raw_data$predictions[[model_name]] <- pred_probs
    
    roc_obj <- roc(
      response = labels_clean, 
      predictor = probs_clean,
      levels = c(0, 1), 
      direction = "<",
      quiet = TRUE
    )
    
    roc_objects[[model_name]] <- roc_obj
    
    roc_df <- data.frame(
      fpr = 1 - roc_obj$specificities,
      tpr = roc_obj$sensitivities,
      model = model_name
    ) %>%
      arrange(fpr) %>%
      distinct()
    
    auroc <- as.numeric(auc(roc_obj))
    ci_auroc_obj <- ci.auc(roc_obj, conf.level = conf_level, quiet = TRUE)
    ci_auroc <- c(ci_auroc_obj[1], ci_auroc_obj[3])
    
    pr_obj <- pr.curve(
      scores.class0 = probs_clean[labels_clean == 1],
      scores.class1 = probs_clean[labels_clean == 0],
      curve = TRUE
    )
    
    prc_df <- data.frame(
      recall = pr_obj$curve[, 1],
      precision = pr_obj$curve[, 2],
      model = model_name
    ) %>%
      arrange(recall)
    
    auprc_result <- bootstrap_auprc(
      labels_clean, probs_clean, 
      n_bootstrap = n_bootstrap, 
      conf_level = conf_level
    )
    
    all_roc[[model_name]] <- roc_df
    all_prc[[model_name]] <- prc_df
    
    model_stats <- rbind(model_stats, data.frame(
      model = model_name,
      mean_auroc = auroc,
      ci_auroc_low = ci_auroc[1],
      ci_auroc_high = ci_auroc[2],
      mean_auprc = auprc_result$mean_auprc,
      ci_auprc_low = auprc_result$ci_auprc[1],
      ci_auprc_high = auprc_result$ci_auprc[2]
    ))
  }
  
  list(
    roc_data = bind_rows(all_roc),
    prc_data = bind_rows(all_prc),
    stats = model_stats,
    roc_objects = roc_objects,
    raw_data = raw_data
  )
}

#' 原有的RDS读取函数
process_model_data_rds <- function(model_files) {
  all_roc <- list()
  all_prc <- list()
  model_stats <- data.frame()
  
  for (model in names(model_files)) {
    dat <- readRDS(model_files[model])
    
    roc_df <- bind_rows(dat$roc_data) %>%
      group_by(fpr) %>%
      summarise(tpr = mean(tpr, na.rm = TRUE)) %>%
      mutate(model = model)
    
    prc_df <- bind_rows(dat$prc_data) %>%
      group_by(recall) %>%
      summarise(precision = mean(precision, na.rm = TRUE)) %>%
      mutate(model = model)
    
    all_roc[[model]] <- roc_df
    all_prc[[model]] <- prc_df
    
    model_stats <- rbind(model_stats, data.frame(
      model = model,
      mean_auroc = dat$mean_auroc,
      ci_auroc_low = dat$ci_auroc[1],
      ci_auroc_high = dat$ci_auroc[2],
      mean_auprc = dat$mean_auprc,
      ci_auprc_low = dat$ci_auprc[1],
      ci_auprc_high = dat$ci_auprc[2]
    ))
  }
  
  list(
    roc_data = bind_rows(all_roc),
    prc_data = bind_rows(all_prc),
    stats = model_stats,
    roc_objects = NULL,
    raw_data = NULL
  )
}

#' 统一的数据处理入口函数
process_model_data <- function(mode, ...) {
  if (mode == "rds") {
    process_model_data_rds(...)
  } else if (mode == "excel") {
    process_excel_data(...)
  } else {
    stop("Invalid mode. Use 'rds' or 'excel'")
  }
}

# ============= 跨数据集DeLong检验函数（支持单侧检验） =============

#' 合并两个数据集的ROC对象
merge_roc_objects <- function(data_large, data_small, prefix_large, prefix_small) {
  
  merged_roc <- list()
  merged_stats <- data.frame()
  
  if (!is.null(data_large$roc_objects)) {
    for (model in names(data_large$roc_objects)) {
      new_name <- paste0(prefix_large, "_", model)
      merged_roc[[new_name]] <- data_large$roc_objects[[model]]
    }
    
    stats_large <- data_large$stats %>%
      mutate(model = paste0(prefix_large, "_", model),
             dataset = prefix_large)
    merged_stats <- bind_rows(merged_stats, stats_large)
  }
  
  if (!is.null(data_small$roc_objects)) {
    for (model in names(data_small$roc_objects)) {
      new_name <- paste0(prefix_small, "_", model)
      merged_roc[[new_name]] <- data_small$roc_objects[[model]]
    }
    
    stats_small <- data_small$stats %>%
      mutate(model = paste0(prefix_small, "_", model),
             dataset = prefix_small)
    merged_stats <- bind_rows(merged_stats, stats_small)
  }
  
  list(
    roc_objects = merged_roc,
    stats = merged_stats
  )
}

#' 执行所有模型间的两两DeLong检验（支持单侧检验）
#' @param merged_roc_objects 合并后的ROC对象列表
#' @param merged_stats 合并后的统计信息
#' @param test_type 检验类型: "two.sided", "greater", "less", "auto"
#' @param prefix_large Cross数据集前缀
#' @param prefix_small Single数据集前缀
#' @return DeLong检验结果数据框
perform_all_delong_tests <- function(merged_roc_objects, merged_stats, 
                                     test_type = "auto",
                                     prefix_large = "Cross", 
                                     prefix_small = "Single") {
  
  if (is.null(merged_roc_objects) || length(merged_roc_objects) < 2) {
    cat("Warning: Need at least 2 ROC objects for DeLong test\n")
    return(NULL)
  }
  
  models <- names(merged_roc_objects)
  n_models <- length(models)
  results <- data.frame()
  
  cat("\n=== Performing Pairwise DeLong Tests ===\n")
  cat("Test type setting:", test_type, "\n")
  cat("Total comparisons:", n_models * (n_models - 1) / 2, "\n\n")
  
  for (i in 1:(n_models - 1)) {
    for (j in (i + 1):n_models) {
      model1 <- models[i]
      model2 <- models[j]
      
      # 判断每个模型属于哪个数据集
      dataset1 <- ifelse(grepl(paste0("^", prefix_large), model1), prefix_large, prefix_small)
      dataset2 <- ifelse(grepl(paste0("^", prefix_large), model2), prefix_large, prefix_small)
      comparison_type <- ifelse(dataset1 == dataset2, "Within", "Between")
      
      # ====== 确定检验方向（核心修改） ======
      if (test_type == "auto") {
        if (comparison_type == "Between") {
          # 跨数据集比较：确保Cross在前，使用单侧检验 H1: Cross > Single
          if (dataset1 == prefix_large && dataset2 == prefix_small) {
            # model1是Cross，model2是Single，检验 H1: model1 > model2
            alternative <- "greater"
            final_model1 <- model1
            final_model2 <- model2
          } else {
            # model1是Single，model2是Cross，交换顺序
            alternative <- "greater"
            final_model1 <- model2
            final_model2 <- model1
          }
        } else {
          # 同数据集内比较：使用双侧检验
          alternative <- "two.sided"
          final_model1 <- model1
          final_model2 <- model2
        }
      } else {
        alternative <- test_type
        final_model1 <- model1
        final_model2 <- model2
      }
      
      # 执行DeLong检验
      tryCatch({
        test_result <- roc.test(
          merged_roc_objects[[final_model1]], 
          merged_roc_objects[[final_model2]], 
          method = "delong",
          paired = TRUE,
          alternative = alternative  # 关键参数
        )
        
        auc1 <- as.numeric(auc(merged_roc_objects[[final_model1]]))
        auc2 <- as.numeric(auc(merged_roc_objects[[final_model2]]))
        
        # 确定显著性标记
        sig_level <- case_when(
          test_result$p.value < 0.001 ~ "***",
          test_result$p.value < 0.01 ~ "**",
          test_result$p.value < 0.05 ~ "*",
          test_result$p.value < 0.1 ~ ".",
          TRUE ~ "ns"
        )
        
        results <- rbind(results, data.frame(
          model1 = final_model1,
          model2 = final_model2,
          dataset1 = ifelse(grepl(paste0("^", prefix_large), final_model1), prefix_large, prefix_small),
          dataset2 = ifelse(grepl(paste0("^", prefix_large), final_model2), prefix_large, prefix_small),
          comparison_type = comparison_type,
          test_alternative = alternative,
          auc1 = round(auc1, 4),
          auc2 = round(auc2, 4),
          auc_diff = round(auc1 - auc2, 4),
          z_statistic = round(test_result$statistic, 4),
          p_value = test_result$p.value,
          p_value_formatted = format.pval(test_result$p.value, digits = 4),
          significance_level = sig_level,
          hypothesis = ifelse(alternative == "greater", 
                              paste0("H1: ", final_model1, " > ", final_model2),
                              ifelse(alternative == "less",
                                     paste0("H1: ", final_model1, " < ", final_model2),
                                     "H1: AUC1 ≠ AUC2"))
        ))
        
      }, error = function(e) {
        cat("Warning: DeLong test failed for", final_model1, "vs", final_model2, ":", e$message, "\n")
      })
    }
  }
  
  return(results)
}

#' 打印DeLong检验结果汇总（区分单侧/双侧）
print_delong_summary_all <- function(delong_results, merged_stats) {
  if (is.null(delong_results) || nrow(delong_results) == 0) {
    cat("No DeLong test results available.\n")
    return(invisible(NULL))
  }
  
  cat("\n", paste(rep("=", 100), collapse = ""), "\n")
  cat("                              DeLong Test Results Summary\n")
  cat(paste(rep("=", 100), collapse = ""), "\n\n")
  
  # 显示模型AUC排名
  cat(">>> Model AUC Ranking:\n")
  cat(paste(rep("-", 65), collapse = ""), "\n")
  ranked <- merged_stats %>% arrange(desc(mean_auroc))
  for (i in 1:nrow(ranked)) {
    cat(sprintf("  %2d. %-20s: AUC = %.4f [%.4f - %.4f]\n", 
                i, ranked$model[i], 
                ranked$mean_auroc[i],
                ranked$ci_auroc_low[i],
                ranked$ci_auroc_high[i]))
  }
  cat("\n")
  
  # 分类显示结果
  for (comp_type in c("Within", "Between")) {
    subset_results <- delong_results[delong_results$comparison_type == comp_type, ]
    
    if (nrow(subset_results) == 0) next
    
    if (comp_type == "Within") {
      cat(">>> Within-Dataset Comparisons (Two-sided test):\n")
    } else {
      cat(">>> Between-Dataset Comparisons (One-sided test: Cross > Single):\n")
      cat("    [单侧检验更易达到显著性，p值约为双侧检验的一半]\n")
    }
    
    cat(paste(rep("-", 95), collapse = ""), "\n")
    cat(sprintf("%-22s vs %-22s | ΔAUC    | Test      | P-value    | Sig\n", "Model 1", "Model 2"))
    cat(paste(rep("-", 95), collapse = ""), "\n")
    
    for (k in 1:nrow(subset_results)) {
      row <- subset_results[k, ]
      test_label <- ifelse(row$test_alternative == "two.sided", "two-sided", "one-sided")
      cat(sprintf("%-22s vs %-22s | %+.4f | %-9s | %-10s | %s\n",
                  row$model1, row$model2, 
                  row$auc_diff, test_label,
                  row$p_value_formatted, 
                  row$significance_level))
    }
    cat("\n")
  }
  
  # 汇总跨数据集比较结果
  between_results <- delong_results[delong_results$comparison_type == "Between", ]
  if (nrow(between_results) > 0) {
    sig_count <- sum(between_results$p_value < 0.05)
    total_count <- nrow(between_results)
    
    cat(">>> Cross-Dataset Summary:\n")
    cat(sprintf("    显著优于Single的比较数: %d / %d (%.1f%%)\n", 
                sig_count, total_count, 100 * sig_count / total_count))
    
    # 找出显著的比较
    sig_comparisons <- between_results[between_results$p_value < 0.05, ]
    if (nrow(sig_comparisons) > 0) {
      cat("    显著的比较:\n")
      for (k in 1:nrow(sig_comparisons)) {
        row <- sig_comparisons[k, ]
        cat(sprintf("      - %s > %s (ΔAUC = %.4f, p = %s)\n",
                    row$model1, row$model2, row$auc_diff, row$p_value_formatted))
      }
    }
    cat("\n")
  }
  
  cat("Significance: *** p<0.001, ** p<0.01, * p<0.05, . p<0.1, ns: not significant\n")
  cat(paste(rep("=", 100), collapse = ""), "\n")
}

#' 识别最佳模型（考虑单侧检验结果）
identify_best_model_global <- function(delong_results, merged_stats, 
                                       prefix_large = "Cross", prefix_small = "Single") {
  if (is.null(delong_results)) return(NULL)
  
  ranked_models <- merged_stats %>%
    arrange(desc(mean_auroc))
  
  best_model <- ranked_models$model[1]
  best_auc <- ranked_models$mean_auroc[1]
  best_dataset <- ranked_models$dataset[1]
  
  cat("\n", paste(rep("=", 75), collapse = ""), "\n")
  cat("                         Best Model Analysis\n")
  cat(paste(rep("=", 75), collapse = ""), "\n\n")
  
  cat("Overall Best Model:", best_model, "\n")
  cat("AUC:", round(best_auc, 4), "\n")
  cat("Dataset:", best_dataset, "\n\n")
  
  # 分析Cross vs Single的整体表现
  cross_models <- merged_stats[merged_stats$dataset == prefix_large, ]
  single_models <- merged_stats[merged_stats$dataset == prefix_small, ]
  
  cat(">>> Dataset-level Comparison:\n")
  cat(sprintf("    %s models - Mean AUC: %.4f (range: %.4f - %.4f)\n",
              prefix_large,
              mean(cross_models$mean_auroc),
              min(cross_models$mean_auroc),
              max(cross_models$mean_auroc)))
  cat(sprintf("    %s models - Mean AUC: %.4f (range: %.4f - %.4f)\n",
              prefix_small,
              mean(single_models$mean_auroc),
              min(single_models$mean_auroc),
              max(single_models$mean_auroc)))
  
  # 检查跨数据集比较的显著性
  between_results <- delong_results[delong_results$comparison_type == "Between", ]
  
  if (nrow(between_results) > 0) {
    cat("\n>>> Cross vs Single Significance Summary:\n")
    
    # 按Cross模型分组统计
    for (cross_model in unique(between_results$model1[between_results$dataset1 == prefix_large])) {
      model_comparisons <- between_results[between_results$model1 == cross_model, ]
      sig_wins <- sum(model_comparisons$p_value < 0.05)
      total <- nrow(model_comparisons)
      
      cat(sprintf("    %s: 显著优于 %d/%d Single模型\n", cross_model, sig_wins, total))
    }
  }
  
  cat("\n")
  cat(paste(rep("=", 75), collapse = ""), "\n")
  
  return(list(
    best_model = best_model,
    best_auc = best_auc,
    best_dataset = best_dataset,
    ranking = ranked_models
  ))
}

#' 创建统一的DeLong检验热图（区分单侧/双侧）
create_unified_delong_heatmap <- function(delong_results, merged_stats,
                                          prefix_large, prefix_small) {
  if (is.null(delong_results)) return(NULL)
  
  # 按数据集和AUC排序
  models_ordered <- merged_stats %>%
    arrange(desc(dataset == prefix_large), desc(mean_auroc)) %>%
    pull(model)
  
  n <- length(models_ordered)
  
  # 创建矩阵
  p_matrix <- matrix(1, nrow = n, ncol = n, 
                     dimnames = list(models_ordered, models_ordered))
  diff_matrix <- matrix(0, nrow = n, ncol = n, 
                        dimnames = list(models_ordered, models_ordered))
  test_type_matrix <- matrix("", nrow = n, ncol = n,
                             dimnames = list(models_ordered, models_ordered))
  
  # 填充矩阵
  for (k in 1:nrow(delong_results)) {
    row <- delong_results[k, ]
    if (row$model1 %in% models_ordered && row$model2 %in% models_ordered) {
      p_matrix[row$model1, row$model2] <- row$p_value
      p_matrix[row$model2, row$model1] <- row$p_value
      diff_matrix[row$model1, row$model2] <- row$auc_diff
      diff_matrix[row$model2, row$model1] <- -row$auc_diff
      test_type_matrix[row$model1, row$model2] <- row$test_alternative
      test_type_matrix[row$model2, row$model1] <- row$test_alternative
    }
  }
  
  # 转换为长格式
  p_long <- as.data.frame(as.table(p_matrix)) %>%
    rename(Model1 = Var1, Model2 = Var2, p_value = Freq)
  
  diff_long <- as.data.frame(as.table(diff_matrix)) %>%
    rename(Model1 = Var1, Model2 = Var2, auc_diff = Freq)
  
  test_long <- as.data.frame(as.table(test_type_matrix)) %>%
    rename(Model1 = Var1, Model2 = Var2, test_type = Freq)
  
  plot_data <- p_long %>%
    left_join(diff_long, by = c("Model1", "Model2")) %>%
    left_join(test_long, by = c("Model1", "Model2")) %>%
    mutate(
      dataset1 = ifelse(grepl(paste0("^", prefix_large), Model1), prefix_large, prefix_small),
      dataset2 = ifelse(grepl(paste0("^", prefix_large), Model2), prefix_large, prefix_small),
      comparison_type = ifelse(dataset1 == dataset2, "Within", "Between"),
      
      sig_label = case_when(
        Model1 == Model2 ~ "",
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*",
        p_value < 0.1 ~ ".",
        TRUE ~ ""
      ),
      # 单侧检验用†标记
      test_marker = ifelse(test_type == "greater" & Model1 != Model2, "†", ""),
      display_label = case_when(
        Model1 == Model2 ~ sprintf("%.3f", 
                                   merged_stats$mean_auroc[match(as.character(Model1), merged_stats$model)]),
        TRUE ~ sprintf("%.3f\n%s%s", auc_diff, sig_label, test_marker)
      ),
      is_diagonal = Model1 == Model2
    )
  
  # 设置因子顺序
  plot_data$Model1 <- factor(plot_data$Model1, levels = models_ordered)
  plot_data$Model2 <- factor(plot_data$Model2, levels = rev(models_ordered))
  
  # 计算分隔线位置
  n_cross <- sum(grepl(paste0("^", prefix_large), models_ordered))
  
  # 创建热图
  p <- ggplot(plot_data, aes(x = Model1, y = Model2)) +
    geom_tile(aes(fill = ifelse(is_diagonal, NA, -log10(p_value + 1e-10))), 
              color = "white", size = 0.5) +
    geom_tile(data = filter(plot_data, is_diagonal),
              aes(x = Model1, y = Model2), 
              fill = "grey85", color = "white", size = 0.5) +
    geom_text(aes(label = display_label, 
                  fontface = ifelse(is_diagonal, "bold", "plain"),
                  color = ifelse(p_value < 0.05 & !is_diagonal, "white", "black")), 
              size = 3) +
    scale_color_identity() +
    # 分隔线
    geom_hline(yintercept = length(models_ordered) - n_cross + 0.5, 
               color = "black", size = 1.5) +
    geom_vline(xintercept = n_cross + 0.5, 
               color = "black", size = 1.5) +
    scale_fill_gradient2(
      low = "white", 
      mid = "#FFEDA0", 
      high = "#E31A1C",
      midpoint = -log10(0.05),
      name = "-log10(p)",
      na.value = "grey85",
      limits = c(0, NA)
    ) +
    labs(
      title = "DeLong Test: Pairwise Model Comparison",
      subtitle = paste0(
        "Diagonal: AUC | Off-diagonal: ΔAUC (row - col) with significance\n",
        "† = one-sided test (H₁: Cross > Single) | ",
        "Black lines separate Cross/Single datasets"
      ),
      x = "", y = ""
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, 
                                 face = "bold", size = 9),
      axis.text.y = element_text(face = "bold", size = 9),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 9, color = "grey30"),
      panel.grid = element_blank(),
      legend.position = "right",
      plot.margin = margin(10, 10, 10, 10)
    ) +
    coord_fixed()
  
  return(p)
}

#' 创建森林图
create_auc_forest_plot <- function(merged_stats, prefix_large, prefix_small) {
  
  plot_data <- merged_stats %>%
    mutate(
      dataset = ifelse(grepl(paste0("^", prefix_large), model), prefix_large, prefix_small),
      model_short = gsub(paste0("^", prefix_large, "_|^", prefix_small, "_"), "", model)
    ) %>%
    arrange(dataset, desc(mean_auroc))
  
  plot_data$model <- factor(plot_data$model, levels = rev(plot_data$model))
  
  p <- ggplot(plot_data, aes(x = mean_auroc, y = model, color = dataset)) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = ci_auroc_low, xmax = ci_auroc_high), height = 0.3) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey60") +
    scale_color_manual(values = c("Cross" = "#E64B35", "Single" = "#4DBBD5"),
                       name = "Dataset") +
    scale_x_continuous(limits = c(0.4, 1), breaks = seq(0.4, 1, 0.1)) +
    labs(
      title = "Model AUC Comparison (with 95% CI)",
      x = "AUC",
      y = ""
    ) +
    theme_bw(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.y = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

#' 创建跨数据集比较专用图（新增）
create_cross_comparison_plot <- function(delong_results, merged_stats, 
                                         prefix_large, prefix_small) {
  
  # 只保留跨数据集比较
  between_results <- delong_results[delong_results$comparison_type == "Between", ]
  
  if (nrow(between_results) == 0) return(NULL)
  
  # 提取模型基础名称
  between_results <- between_results %>%
    mutate(
      base_model1 = gsub(paste0("^", prefix_large, "_|^", prefix_small, "_"), "", model1),
      base_model2 = gsub(paste0("^", prefix_large, "_|^", prefix_small, "_"), "", model2),
      comparison_label = paste0(base_model1, " (", dataset1, ")\nvs\n", 
                                base_model2, " (", dataset2, ")"),
      is_significant = p_value < 0.05
    )
  
  # 创建比较图
  p <- ggplot(between_results, aes(x = reorder(comparison_label, -p_value), y = -log10(p_value))) +
    geom_bar(aes(fill = is_significant), stat = "identity", width = 0.7) +
    geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red", size = 0.8) +
    geom_hline(yintercept = -log10(0.1), linetype = "dotted", color = "orange", size = 0.6) +
    geom_text(aes(label = sprintf("Δ=%.3f\np=%s", auc_diff, p_value_formatted)),
              vjust = -0.3, size = 2.8) +
    scale_fill_manual(values = c("TRUE" = "#2E8B57", "FALSE" = "#808080"),
                      labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
                      name = "Significance") +
    labs(
      title = paste0("Cross-Dataset Comparison (One-sided: ", prefix_large, " > ", prefix_small, ")"),
      subtitle = "Red dashed line: p = 0.05 | Orange dotted line: p = 0.1",
      x = "Comparison",
      y = "-log10(p-value)"
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
      plot.subtitle = element_text(hjust = 0.5, size = 9, color = "grey40"),
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom"
    ) +
    coord_flip()
  
  return(p)
}

# ============= 数据处理 =============

if (data_mode == "rds") {
  cat("=== Using RDS mode ===\n")
  data_large <- process_model_data("rds", model_files_large)
  data_small <- process_model_data("rds", model_files_small)
  
} else if (data_mode == "excel") {
  cat("=== Using Excel mode ===\n")
  data_large <- process_model_data(
    "excel", 
    excel_file = excel_file_large,
    label_col = label_col,
    prob_cols_mapping = prob_cols_mapping,
    n_bootstrap = n_bootstrap,
    conf_level = conf_level
  )
  
  data_small <- process_model_data(
    "excel",
    excel_file = excel_file_small,
    label_col = label_col,
    prob_cols_mapping = prob_cols_mapping,
    n_bootstrap = n_bootstrap,
    conf_level = conf_level
  )
  
} else {
  stop("Invalid data_mode. Please use 'rds' or 'excel'")
}

cat("Data processing completed!\n")

# ============= 执行DeLong检验（支持单侧） =============

if (perform_delong && data_mode == "excel") {
  cat("\n========== Performing DeLong Tests (with One-sided option) ==========\n")
  
  # 合并ROC对象
  merged_data <- merge_roc_objects(data_large, data_small, prefix_large, prefix_small)
  
  # 执行DeLong检验（使用配置的检验类型）
  all_delong_results <- perform_all_delong_tests(
    merged_data$roc_objects, 
    merged_data$stats,
    test_type = delong_test_type,
    prefix_large = prefix_large,
    prefix_small = prefix_small
  )
  
  # 打印汇总结果
  print_delong_summary_all(all_delong_results, merged_data$stats)
  
  # 识别最佳模型
  best_model_result <- identify_best_model_global(
    all_delong_results, merged_data$stats,
    prefix_large, prefix_small
  )
  
  # 创建统一热图
  unified_heatmap <- create_unified_delong_heatmap(
    all_delong_results, 
    merged_data$stats,
    prefix_large, 
    prefix_small
  )
  
  # 创建森林图
  forest_plot <- create_auc_forest_plot(merged_data$stats, prefix_large, prefix_small)
  
  # 创建跨数据集比较专用图
  cross_comparison_plot <- create_cross_comparison_plot(
    all_delong_results, merged_data$stats,
    prefix_large, prefix_small
  )
  
  # 组合并保存图形
  if (!is.null(unified_heatmap)) {
    # 主图：热图 + 森林图
    combined_main <- unified_heatmap / forest_plot +
      plot_layout(heights = c(2, 1)) +
      plot_annotation(
        title = "Model Performance Comparison: DeLong Test Results",
        theme = theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
        )
      )
    
    ggsave("DeLong_Unified_Heatmap.svg", combined_main, 
           width = 12, height = 14, dpi = 300, bg = "white")
    cat("\nUnified DeLong heatmap saved to: DeLong_Unified_Heatmap.svg\n")
    
    # 单独保存热图
    ggsave("DeLong_Heatmap_Only.svg", unified_heatmap, 
           width = 10, height = 9, dpi = 300, bg = "white")
  }
  
  # 保存跨数据集比较图
  if (!is.null(cross_comparison_plot)) {
    ggsave("DeLong_Cross_Comparison.svg", cross_comparison_plot,
           width = 10, height = 6, dpi = 300, bg = "white")
    cat("Cross-dataset comparison plot saved to: DeLong_Cross_Comparison.svg\n")
  }
  
  # 保存结果到CSV
  write.csv(all_delong_results, "DeLong_Test_Results_All.csv", row.names = FALSE)
  cat("DeLong test results saved to: DeLong_Test_Results_All.csv\n")
  
  write.csv(merged_data$stats, "Model_AUC_Rankings.csv", row.names = FALSE)
  cat("Model rankings saved to: Model_AUC_Rankings.csv\n")
  
} else if (perform_delong && data_mode == "rds") {
  cat("\nNote: DeLong test is currently only supported in Excel mode.\n")
}

# ============= ROC/PRC绑图代码（保持不变） =============

combined_stats <- data_large$stats %>%
  left_join(data_small$stats, by = "model", suffix = c("_large", "_small")) %>%
  mutate(
    roc_legend = sprintf(
      "%s (%s: %.3f [%.3f–%.3f]; %s: %.3f [%.3f–%.3f])",
      model,
      feature_set_large_name, mean_auroc_large, ci_auroc_low_large, ci_auroc_high_large,
      feature_set_small_name, mean_auroc_small, ci_auroc_low_small, ci_auroc_high_small
    ),
    prc_legend = sprintf(
      "%s (%s: %.3f [%.3f–%.3f]; %s: %.3f [%.3f–%.3f])",
      model,
      feature_set_large_name, mean_auprc_large, ci_auprc_low_large, ci_auprc_high_large,
      feature_set_small_name, mean_auprc_small, ci_auprc_low_small, ci_auprc_high_small
    )
  )

my_theme_refined <- theme_bw(base_size = 13) +
  theme(
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 11, family = "Arial"),
    legend.key.width = unit(1.2, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.spacing.x = unit(0.2, "cm"),
    axis.title = element_text(size = 13.5, face = "bold"),
    axis.text = element_text(size = 11, family = "Arial"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15, family = "Arial"),
    panel.grid.major = element_line(size = 0.25, color = "grey90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size = 0.8, color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    aspect.ratio = 1
  )

p_roc_large <- ggplot(data_large$roc_data, aes(x = fpr, y = tpr, color = model)) +
  geom_line(size = 0.8, alpha = 0.9, lineend = "round") +
  geom_abline(linetype = "dashed", color = "grey60", size = 0.6) +
  scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  scale_color_npg(labels = combined_stats$roc_legend, breaks = combined_stats$model) +
  labs(
    title = paste("ROC Curves -", feature_set_large_name),
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  my_theme_refined +
  guides(color = guide_legend(nrow = 3, byrow = TRUE))

p_roc_small <- ggplot(data_small$roc_data, aes(x = fpr, y = tpr, color = model)) +
  geom_line(size = 0.8, alpha = 0.9, lineend = "round") +
  geom_abline(linetype = "dashed", color = "grey60", size = 0.6) +
  scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  scale_color_npg(labels = combined_stats$roc_legend, breaks = combined_stats$model) +
  labs(
    title = paste("ROC Curves -", feature_set_small_name),
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  my_theme_refined +
  guides(color = guide_legend(nrow = 3, byrow = TRUE))

baseline_precision <- 0.5
p_prc_large <- ggplot(data_large$prc_data, aes(x = recall, y = precision, color = model)) +
  geom_line(size = 0.8, alpha = 0.9, lineend = "round") +
  geom_hline(yintercept = baseline_precision, linetype = "dashed", color = "grey60", size = 0.6) +
  scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  scale_color_npg(labels = combined_stats$prc_legend, breaks = combined_stats$model) +
  labs(
    title = paste("PR Curves -", feature_set_large_name),
    x = "Recall (Sensitivity)",
    y = "Precision"
  ) +
  my_theme_refined +
  guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
  annotate("text", x = 0.5, y = baseline_precision + 0.03,
           label = sprintf("Baseline = %.3f", baseline_precision),
           color = "grey40", size = 3, hjust = 0.5)

p_prc_small <- ggplot(data_small$prc_data, aes(x = recall, y = precision, color = model)) +
  geom_line(size = 0.8, alpha = 0.9, lineend = "round") +
  geom_hline(yintercept = baseline_precision, linetype = "dashed", color = "grey60", size = 0.6) +
  scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  scale_color_npg(labels = combined_stats$prc_legend, breaks = combined_stats$model) +
  labs(
    title = paste("PR Curves -", feature_set_small_name),
    x = "Recall (Sensitivity)",
    y = "Precision"
  ) +
  my_theme_refined +
  guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
  annotate("text", x = 0.5, y = baseline_precision + 0.03,
           label = sprintf("Baseline = %.3f", baseline_precision),
           color = "grey40", size = 3, hjust = 0.5)

row1_roc <- (p_roc_large | p_roc_small) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

row2_prc <- (p_prc_large | p_prc_small) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

final_plot <- row1_roc / row2_prc + 
  plot_layout(heights = c(1, 1))

final_plot_annotated <- final_plot +
  plot_annotation(
    title = "Model Performance Comparison: 5-Fold Cross-Validation",
    tag_levels = 'A',
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, family = "Arial"),
      plot.tag = element_text(size = 14, face = "bold", family = "Arial")
    )
  )

ggsave("MultiModel_ROC_PRC_Comparison.svg",
       final_plot_annotated,
       width = 15, height = 15, dpi = 300, bg = "white")

cat("\nAll plots saved successfully!\n")