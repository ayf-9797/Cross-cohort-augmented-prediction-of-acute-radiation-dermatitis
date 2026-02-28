# ============================================================================
# 哑变量诊断脚本 — 逐列对比 API 生成的列名 vs 模型期望的列名
# Dummy Variable Diagnostic Script
# ============================================================================
#
# 功能:
#   1. 模拟 plumber.R 的预处理流程，生成哑变量
#   2. 读取 model_colnames.rds（或从模型中提取期望列名）
#   3. 逐列对比，找出不匹配的列
#   4. 如果不匹配，自动生成修复代码
#
# 使用方法:
#   在 RStudio 中: Ctrl+Shift+Enter
#   在终端中:     Rscript check_dummy_vars.R
#
# ============================================================================

cat("============================================================\n")
cat("  哑变量列名诊断\n")
cat("============================================================\n\n")

# 设置工作目录
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
} else {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    setwd(dirname(normalizePath(sub("--file=", "", file_arg), winslash = "/")))
  }
}

cat("[INFO] 工作目录:", getwd(), "\n\n")

# ------------------------------------------------------------------
# 加载 plumber.R 中的定义
# ------------------------------------------------------------------
cat("[步骤1] 加载 plumber.R 中的函数定义...\n")
source("plumber.R", local = TRUE)
cat("  ✓ 加载成功\n\n")

# ------------------------------------------------------------------
# 生成测试用哑变量 (使用方法A: Factor Levels 法)
# ------------------------------------------------------------------
cat("[步骤2] 使用方法A (Factor Levels 法) 生成哑变量...\n\n")

test_input <- list(
  Sex = 0, Career = 0, Wedding = 0, Diabetes = 0,
  BMI_level = 0, Drinking = 0, Red_blood_cell = 0,
  Hemoglobin = 0, Platelets = 0, Hormone_therapy = 0,
  Chemotherapy = 0, Targeted_therapy = 0, Endocrine_therapy = 0,
  Diagnosis = 0, Treatment_site = 0, Assistive_Devices = 0
)

factor_df <- integers_to_factors(test_input)
method_a_cols <- colnames(dummy_by_factor_levels(factor_df))

cat("  方法A 生成的哑变量列数:", length(method_a_cols), "\n")
cat("  列名:\n")
for (i in seq_along(method_a_cols)) {
  cat(sprintf("    [%02d] %s\n", i, method_a_cols[i]))
}

# ------------------------------------------------------------------
# 同时测试方法B (模板合并法)
# ------------------------------------------------------------------
cat("\n[步骤3] 使用方法B (模板合并法) 生成哑变量...\n\n")

method_b_cols <- colnames(dummy_by_template(factor_df))

cat("  方法B 生成的哑变量列数:", length(method_b_cols), "\n")

# ------------------------------------------------------------------
# 对比方法A vs 方法B
# ------------------------------------------------------------------
cat("\n[步骤4] 对比方法A vs 方法B...\n\n")

if (identical(method_a_cols, method_b_cols)) {
  cat("  ✓ 方法A 和 方法B 生成的列名完全一致\n")
  cat("  → 两种方法都可以使用，推荐使用方法A（更简洁）\n")
} else {
  cat("  ✗ 方法A 和 方法B 生成的列名不一致!\n\n")

  only_in_a <- setdiff(method_a_cols, method_b_cols)
  only_in_b <- setdiff(method_b_cols, method_a_cols)

  if (length(only_in_a) > 0) {
    cat("  仅在方法A中出现:\n")
    for (col in only_in_a) cat("    -", col, "\n")
  }
  if (length(only_in_b) > 0) {
    cat("  仅在方法B中出现:\n")
    for (col in only_in_b) cat("    -", col, "\n")
  }
}

# ------------------------------------------------------------------
# 对比 vs 模型期望的列名
# ------------------------------------------------------------------
cat("\n[步骤5] 对比 vs 模型期望的列名...\n\n")

expected_cols <- NULL

if (file.exists("model_colnames.rds")) {
  expected_cols <- readRDS("model_colnames.rds")
  cat("  从 model_colnames.rds 读取，期望列数:", length(expected_cols), "\n")
} else if (file.exists("model.rds")) {
  cat("  未找到 model_colnames.rds，尝试从模型中提取...\n")
  model <- readRDS("model.rds")
  if (inherits(model, c("glm", "lm"))) {
    coefs <- names(coef(model))
    expected_cols <- coefs[coefs != "(Intercept)"]
  } else if (inherits(model, c("glmnet", "cv.glmnet"))) {
    coef_matrix <- coef(model, s = "lambda.min")
    coefs <- rownames(coef_matrix)
    expected_cols <- coefs[coefs != "(Intercept)"]
  } else if (inherits(model, "xgb.Booster") && !is.null(model$feature_names)) {
    expected_cols <- model$feature_names
  }
}

if (is.null(expected_cols)) {
  cat("  [警告] 无法获取模型期望的列名。\n")
  cat("  请先运行 setup_model.R 生成 model_colnames.rds\n")
} else {
  cat("\n  模型期望的列名:\n")
  for (i in seq_along(expected_cols)) {
    cat(sprintf("    [%02d] %s\n", i, expected_cols[i]))
  }

  # 对比 (使用方法A的结果)
  generated_cols <- method_a_cols

  cat("\n  ─────────────────────────────────────────\n")
  cat("  逐列对比 (方法A生成 vs 模型期望):\n")
  cat("  ─────────────────────────────────────────\n\n")

  max_len <- max(length(generated_cols), length(expected_cols))
  mismatch_count <- 0

  for (i in seq_len(max_len)) {
    gen <- if (i <= length(generated_cols)) generated_cols[i] else "<缺失>"
    exp <- if (i <= length(expected_cols)) expected_cols[i] else "<多余>"
    match_symbol <- if (gen == exp) "✓" else "✗"

    if (gen != exp) mismatch_count <- mismatch_count + 1

    cat(sprintf("  %s [%02d] API生成: %-45s 模型期望: %s\n",
                match_symbol, i, gen, exp))
  }

  cat("\n  ─────────────────────────────────────────\n")

  if (mismatch_count == 0) {
    cat("  ✓ 所有列名完全匹配! 哑变量生成正确。\n")
  } else {
    cat(sprintf("  ✗ 有 %d 处不匹配!\n\n", mismatch_count))

    # 生成修复代码
    cat("  >>> 自动生成的修复代码 (复制到 plumber.R 的 preprocess_input 中) <<<\n\n")
    cat("  # 列名映射修复\n")
    cat("  colname_mapping <- c(\n")

    for (i in seq_len(min(length(generated_cols), length(expected_cols)))) {
      if (generated_cols[i] != expected_cols[i]) {
        comma <- if (i < min(length(generated_cols), length(expected_cols))) "," else ""
        cat(sprintf("    \"%s\" = \"%s\"%s\n",
                    expected_cols[i], generated_cols[i], comma))
      }
    }
    cat("  )\n\n")
    cat("  # 应用映射\n")
    cat("  for (new_name in names(colname_mapping)) {\n")
    cat("    old_name <- colname_mapping[[new_name]]\n")
    cat("    if (old_name %in% colnames(dummy_df)) {\n")
    cat("      colnames(dummy_df)[colnames(dummy_df) == old_name] <- new_name\n")
    cat("    }\n")
    cat("  }\n")
  }
}

cat("\n============================================================\n")
cat("  诊断完成\n")
cat("============================================================\n")
