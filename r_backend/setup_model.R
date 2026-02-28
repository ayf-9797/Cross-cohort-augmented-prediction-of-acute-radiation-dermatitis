# ============================================================================
# 模型初始化脚本 — 在部署前运行一次
# Model Setup Script — Run ONCE before deployment
# ============================================================================
#
# 功能:
#   1. 读取您的模型，自动提取模型期望的特征列名 → 保存为 model_colnames.rds
#   2. 从训练数据中提取模板数据框 → 保存为 training_template.rds (可选)
#   3. 模拟一次完整预测，验证整个流程是否通畅
#
# 使用方法:
#   在 RStudio 中打开此文件，按 Ctrl+Shift+Enter 全部运行
#   或在终端中: Rscript setup_model.R
#
# ============================================================================

cat("============================================================\n")
cat("  模型初始化脚本\n")
cat("============================================================\n\n")

# ------------------------------------------------------------------
# 0. 设置工作目录为脚本所在目录
# ------------------------------------------------------------------
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
} else {
  # 命令行运行时
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    script_dir <- dirname(normalizePath(sub("--file=", "", file_arg), winslash = "/"))
    setwd(script_dir)
  }
}

cat("[INFO] 工作目录:", getwd(), "\n\n")

# ------------------------------------------------------------------
# 1. 加载模型
# ------------------------------------------------------------------

model_path <- "Adaboost_best_model.rds"

if (!file.exists(model_path)) {
  stop(paste0(
    "\n[错误] 未找到模型文件: ", normalizePath(model_path, mustWork = FALSE, winslash = "/"),
    "\n请将您的模型文件放在此目录下，或修改 model_path 变量。"
  ))
}

cat("[步骤1] 加载模型...\n")
model <- readRDS(model_path)
cat("  模型类型:", class(model), "\n")
cat("  模型加载成功 ✓\n\n")

# ------------------------------------------------------------------
# 2. 提取模型期望的特征列名
# ------------------------------------------------------------------

cat("[步骤2] 提取模型期望的特征列名...\n")

expected_cols <- NULL

# --- 尝试各种模型类型的列名提取方式 ---

# glm / lm
if (is.null(expected_cols) && inherits(model, c("glm", "lm"))) {
  coefs <- names(coef(model))
  # 去掉截距项 (Intercept)
  expected_cols <- coefs[coefs != "(Intercept)"]
  cat("  检测到 glm/lm 模型，从 coef() 提取\n")
}

# glmnet / cv.glmnet
if (is.null(expected_cols) && inherits(model, c("glmnet", "cv.glmnet"))) {
  coef_matrix <- coef(model, s = "lambda.min")
  coefs <- rownames(coef_matrix)
  expected_cols <- coefs[coefs != "(Intercept)"]
  cat("  检测到 glmnet 模型，从 coef() 提取\n")
}

# xgboost
if (is.null(expected_cols) && inherits(model, "xgb.Booster")) {
  if (!is.null(model$feature_names)) {
    expected_cols <- model$feature_names
    cat("  检测到 xgboost 模型，从 feature_names 提取\n")
  }
}

# randomForest
if (is.null(expected_cols) && inherits(model, "randomForest")) {
  # randomForest 使用 factor 输入，不需要哑变量
  # 但如果训练时用了哑变量，可以从这里提取
  if (!is.null(model$forest$xlevels)) {
    cat("  检测到 randomForest 模型\n")
    cat("  [提示] randomForest 通常直接接受 factor 输入，\n")
    cat("         您可能不需要哑变量。请在 plumber.R 中选择方法0。\n")
    # 如果确实需要，可以从 importance 提取
    if (!is.null(model$importance)) {
      expected_cols <- rownames(model$importance)
    }
  }
}

# caret::train
if (is.null(expected_cols) && inherits(model, "train")) {
  if (!is.null(model$coefnames)) {
    expected_cols <- model$coefnames
  } else if (!is.null(model$finalModel)) {
    cat("  检测到 caret 模型，尝试从 finalModel 提取...\n")
    # 递归尝试
    if (inherits(model$finalModel, c("glm", "lm"))) {
      coefs <- names(coef(model$finalModel))
      expected_cols <- coefs[coefs != "(Intercept)"]
    }
  }
  if (is.null(expected_cols)) {
    expected_cols <- predictors(model)
  }
  cat("  检测到 caret 模型\n")
}

# 通用 fallback
if (is.null(expected_cols)) {
  cat("  [警告] 无法自动提取特征列名。\n")
  cat("  请手动指定 expected_cols，例如:\n")
  cat("    expected_cols <- c('SexFemale', 'Career1', 'Career2', ...)\n")
  cat("  或提供训练数据，从 model.matrix() 提取。\n\n")

  # ------------------------------------------------------------------
  # >>> 手动指定方式 <<<
  # 如果自动提取失败，取消下方注释并填入您模型的特征列名:
  # ------------------------------------------------------------------
  # expected_cols <- c(
  #   "SexFemale",
  #   "CareerFarmer", "CareerStaff", ...
  # )
  # ------------------------------------------------------------------
}

if (!is.null(expected_cols)) {
  cat("  特征列数:", length(expected_cols), "\n")
  cat("  前10个列名:\n")
  print(head(expected_cols, 10))

  # 保存
  saveRDS(expected_cols, "model_colnames.rds")
  cat("\n  已保存到: model_colnames.rds ✓\n\n")
} else {
  cat("\n  [跳过] 未能提取列名，不生成 model_colnames.rds\n\n")
}

# ------------------------------------------------------------------
# 3. 从训练数据生成模板 (可选)
# ------------------------------------------------------------------
# 如果您有训练数据，取消下方注释，修改路径

cat("[步骤3] 生成训练数据模板 (可选)...\n")

# >>> 取消注释以下代码块，修改训练数据路径 <<<
#
# training_data_path <- "training_data.csv"  # 或 .rds / .xlsx
#
# if (file.exists(training_data_path)) {
#   training_data <- read.csv(training_data_path, stringsAsFactors = TRUE)
#   # 或: training_data <- readRDS(training_data_path)
#
#   # 只保留模型使用的特征列（factor 格式）
#   feature_cols <- c("Sex", "Career", "Wedding", "Diabetes", "BMI_level",
#                     "Drinking", "Red_blood_cell", "Hemoglobin", "Platelets",
#                     "Hormone_therapy", "Chemotherapy", "Targeted_therapy",
#                     "Endocrine_therapy", "Diagnosis", "Treatment_site",
#                     "Assistive_Devices")
#
#   template_data <- training_data[, feature_cols, drop = FALSE]
#
#   # 确保每个 factor level 至少出现一次
#   # 策略: 对每个变量，取每个 level 的第一次出现的行
#   template_rows <- c()
#   for (col in feature_cols) {
#     if (is.factor(template_data[[col]])) {
#       for (lv in levels(template_data[[col]])) {
#         idx <- which(template_data[[col]] == lv)[1]
#         if (!is.na(idx) && !(idx %in% template_rows)) {
#           template_rows <- c(template_rows, idx)
#         }
#       }
#     }
#   }
#
#   template_subset <- template_data[template_rows, , drop = FALSE]
#   saveRDS(template_subset, "training_template.rds")
#   cat("  训练数据模板已保存:", nrow(template_subset), "行 ✓\n")
#   cat("  文件: training_template.rds\n\n")
#
# } else {
#   cat("  [跳过] 未找到训练数据文件:", training_data_path, "\n\n")
# }

cat("  [跳过] 训练数据模板功能未启用。\n")
cat("  如需启用，请编辑此文件，取消第3步中的注释。\n\n")

# ------------------------------------------------------------------
# 4. 模拟预测测试
# ------------------------------------------------------------------

cat("[步骤4] 模拟预测测试...\n")

# 加载 plumber.R 中的函数
source("plumber.R", local = TRUE)

# 模拟前端发送的数据（全部为 0 = 参考类别）
test_input <- list(
  Sex = 0, Career = 0, Wedding = 0, Diabetes = 0,
  BMI_level = 0, Drinking = 0, Red_blood_cell = 0,
  Hemoglobin = 0, Platelets = 0, Hormone_therapy = 0,
  Chemotherapy = 0, Targeted_therapy = 0, Endocrine_therapy = 0,
  Diagnosis = 0, Treatment_site = 0, Assistive_Devices = 0
)

tryCatch({
  cat("\n  模拟输入 (全部为参考类别):\n")
  cat("  ", paste(names(test_input), "=", test_input, collapse = ", "), "\n\n")

  # 预处理
  processed <- preprocess_input(test_input)
  cat("\n  预处理后数据维度:", nrow(processed), "行 x", ncol(processed), "列\n")

  # 预测
  prob <- get_prediction(model, processed)
  cat("  预测概率:", round(prob, 6), "\n")
  cat("  分类结果:", ifelse(prob >= OPTIMAL_THRESHOLD, "High Risk", "Low Risk"), "\n")

  cat("\n  ✓ 模拟预测成功！API 可以正常工作。\n")

}, error = function(e) {
  cat("\n  ✗ 模拟预测失败!\n")
  cat("  错误信息:", e$message, "\n\n")
  cat("  常见原因:\n")
  cat("  1. factor levels / labels 与训练数据不一致\n")
  cat("  2. 哑变量列名与模型期望的不匹配\n")
  cat("  3. 模型类型需要特定的 predict() 参数\n\n")
  cat("  调试建议:\n")
  cat("  - 运行 check_dummy_vars.R 检查列名对齐情况\n")
  cat("  - 在训练环境中运行: str(training_data) 确认各列类型\n")
  cat("  - 检查 plumber.R 中的 get_prediction() 函数\n")
})

cat("\n============================================================\n")
cat("  初始化完成\n")
cat("============================================================\n")
cat("\n生成的文件:\n")
if (file.exists("model_colnames.rds")) cat("  ✓ model_colnames.rds (模型特征列名)\n")
if (file.exists("training_template.rds")) cat("  ✓ training_template.rds (训练数据模板)\n")
cat("\n下一步: 运行 run_api.R 启动 API 服务\n\n")
