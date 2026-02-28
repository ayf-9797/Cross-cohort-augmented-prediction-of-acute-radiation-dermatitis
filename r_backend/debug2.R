# ============================================================================
# Factor 定义（完整版 - 与模型训练一致）
# ============================================================================

FACTOR_DEFINITIONS <- list(
  
  Sex = list(
    levels = 0:1,
    labels = c("Male", "Female")
  ),
  
  Career = list(
    levels = 0:10,
    labels = c("Retiree", "Farmer", "Staff",
               "Professional/Technical Personnel",
               "Worker", "Government Employee",
               "Enterprise Manager", "Freelancer",
               "Self-employed", "Other", "Student")
  ),
  
  Wedding = list(
    levels = 0:3,
    labels = c("Married", "Unmarried", "Widowed", "Divorced")
  ),
  
  Diabetes = list(
    levels = 0:1,
    labels = c("No", "Yes")
  ),
  
  BMI_level = list(
    levels = 0:3,
    labels = c("Normal", "Overweight", "Obese", "Underweight")
  ),
  
  # ✅ 新增 NRS2002（营养风险筛查）
  # 根据哑变量 NRS20021, NRS20022, NRS200222, NRS20023，levels 应该是 0,1,2,22,3
  NRS2002 = list(
    levels = c(0, 1, 2, 22, 3),  # 注意：包含特殊值 22
    labels = c("0", "1", "2", "22", "3")
  ),
  
  Drinking = list(
    levels = 0:1,
    labels = c("No", "Yes")
  ),
  
  Red_blood_cell = list(
    levels = 0:2,
    labels = c("Normal", "Decreased", "Increased")
  ),
  
  Hemoglobin = list(
    levels = 0:2,
    labels = c("Normal", "Decreased", "Increased")
  ),
  
  Platelets = list(
    levels = 0:2,
    labels = c("Normal", "Decreased", "Increased")
  ),
  
  Hormone_therapy = list(
    levels = 0:1,
    labels = c("No", "Yes")
  ),
  
  Chemotherapy = list(
    levels = 0:1,
    labels = c("No", "Yes")
  ),
  
  Targeted_therapy = list(
    levels = 0:1,
    labels = c("No", "Yes")
  ),
  
  Endocrine_therapy = list(
    levels = 0:1,
    labels = c("No", "Yes")
  ),
  
  Diagnosis = list(
    levels = 0:11,
    labels = c("Lung Cancer", "Head and Neck Tumor",
               "Breast Cancer", "Esophageal Cancer",
               "Nasopharyngeal Cancer", "Pancreatic Cancer",
               "Liver Cancer", "Rectal Cancer",
               "Prostate Cancer", "Thymic Cancer",
               "Cervical Cancer", "Other")
  ),
  
  # ✅ 修正：Target_Volume（不是 Treatment_site）
  Target_Volume = list(
    levels = 0:11,
    labels = c("Lung", "Brain", "Head and Neck",
               "Esophagus", "Left Breast", "Nasopharynx",
               "Right Breast", "Lymph Nodes",
               "Mediastinum", "Chest Wall", "Liver", "Other")
  ),
  
  Assistive_Devices = list(
    levels = 0:11,
    labels = c("Large Mask Fixation",
               "Negative Pressure Bag Fixation",
               "Large Mask Fixation, Foam Adhesive Fixation",
               "Breast Bracket Fixation (Supine)",
               "Small Mask Fixation",
               "Proton Mask",
               "Foam Adhesive Fixation, Proton Mask",
               "Body Membrane Fixation",
               "Abdominal-Pelvic Fixator",
               "Foam Adhesive Fixation",
               "Negative Pressure Bag Fixation, Abdominal Compression",
               "Other")
  )
)

# ✅ 新增：连续变量列表（不需要哑变量转换）
CONTINUOUS_VARS <- c("Age", "Dose", "Average_dose")

# ============================================================================
# 整数编码 → Factor（支持非连续 levels，如 NRS2002 的 0,1,2,22,3）
# ============================================================================

integers_to_factors <- function(raw_data) {
  df_list <- list()
  
  for (var_name in names(FACTOR_DEFINITIONS)) {
    fdef <- FACTOR_DEFINITIONS[[var_name]]
    value <- raw_data[[var_name]]
    
    if (is.null(value)) {
      stop(sprintf("缺少变量: %s", var_name))
    }
    
    value <- as.integer(value)
    
    # 检查值是否在允许的 levels 中
    if (!(value %in% fdef$levels)) {
      stop(sprintf("变量 '%s' 的值 %d 不在允许范围 [%s] 中", 
                   var_name, value, paste(fdef$levels, collapse = ", ")))
    }
    
    # 创建 factor，使用数字作为 levels
    df_list[[var_name]] <- factor(
      value,
      levels = fdef$levels
    )
  }
  
  df <- as.data.frame(df_list, stringsAsFactors = TRUE)
  return(df)
}


# ============================================================================
# 预处理函数（支持分类变量 + 连续变量）
# ============================================================================

preprocess_input <- function(raw_data, use_dummy = TRUE) {
  
  cat("[预处理] 开始处理数据...\n")
  
  # ------------------------------------------------------------------
  # 步骤1: 处理分类变量（Factor）
  # ------------------------------------------------------------------
  factor_df <- integers_to_factors(raw_data)
  
  cat("[预处理] Factor 转换完成 (", ncol(factor_df), "个分类变量)\n")
  
  # ------------------------------------------------------------------
  # 步骤2: 分类变量转哑变量
  # ------------------------------------------------------------------
  if (use_dummy) {
    result_df <- dummy_by_factor_levels(factor_df)
    cat("[预处理] 哑变量转换完成 (", ncol(result_df), "列)\n")
  } else {
    result_df <- factor_df
  }
  
  # ------------------------------------------------------------------
  # 步骤3: 添加连续变量（不转换，直接使用数值）
  # ------------------------------------------------------------------
  for (var_name in CONTINUOUS_VARS) {
    if (!is.null(raw_data[[var_name]])) {
      result_df[[var_name]] <- as.numeric(raw_data[[var_name]])
      cat("[预处理] 添加连续变量:", var_name, "=", raw_data[[var_name]], "\n")
    } else {
      stop(sprintf("缺少连续变量: %s", var_name))
    }
  }
  
  # ------------------------------------------------------------------
  # 步骤4: 对齐到模型期望的列顺序
  # ------------------------------------------------------------------
  if (exists("model") && !is.null(model$coefnames)) {
    expected_cols <- model$coefnames
    result_df <- align_columns(result_df, expected_cols)
    cat("[预处理] 已对齐到模型期望的", length(expected_cols), "列\n")
  }
  
  cat("[预处理] 完成! 最终维度:", nrow(result_df), "x", ncol(result_df), "\n")
  
  return(result_df)
}


# ============================================================================
# 对齐列到模型期望的格式（支持连续变量）
# ============================================================================

align_columns <- function(dummy_df, expected_cols) {
  
  # 创建全零数据框
  aligned <- data.frame(matrix(0, nrow = 1, ncol = length(expected_cols)))
  colnames(aligned) <- expected_cols
  
  matched_count <- 0
  
  for (col in colnames(dummy_df)) {
    if (col %in% expected_cols) {
      aligned[[col]] <- dummy_df[[col]]
      matched_count <- matched_count + 1
    } else {
      # 尝试 make.names 转换
      clean_col <- make.names(col)
      if (clean_col %in% expected_cols) {
        aligned[[clean_col]] <- dummy_df[[col]]
        matched_count <- matched_count + 1
      }
    }
  }
  
  cat("[对齐] 匹配了", matched_count, "/", ncol(dummy_df), "列\n")
  
  # 检查是否有未匹配的期望列
  missing_in_data <- setdiff(expected_cols, colnames(dummy_df))
  if (length(missing_in_data) > 0 && length(missing_in_data) <= 10) {
    cat("[对齐] 注意: 以下列使用默认值 0:", paste(missing_in_data, collapse = ", "), "\n")
  }
  
  return(aligned)
}


# ============================================================================
# 测试完整流程
# ============================================================================

# 模拟前端发送的完整数据（包含所有变量）
test_data <- list(
  # 分类变量
  Sex = 1,
  Career = 2,
  Wedding = 0,
  Diabetes = 1,
  BMI_level = 1,
  NRS2002 = 2,           # ✅ 新增
  Drinking = 0,
  Red_blood_cell = 0,
  Hemoglobin = 1,
  Platelets = 0,
  Hormone_therapy = 0,
  Chemotherapy = 1,
  Targeted_therapy = 0,
  Endocrine_therapy = 0,
  Diagnosis = 2,
  Target_Volume = 4,     # ✅ 修正变量名
  Assistive_Devices = 3,
  
  # 连续变量
  Age = 55,              # ✅ 新增
  Dose = 60.5,           # ✅ 新增
  Average_dose = 2.1     # ✅ 新增
)

cat("========== 测试预处理 ==========\n\n")

# 执行预处理
processed_data <- preprocess_input(test_data)

cat("\n========== 处理后的数据 ==========\n")
cat("列数:", ncol(processed_data), "\n")
cat("列名:\n")
print(colnames(processed_data))

cat("\n========== 与模型期望对比 ==========\n")
expected <- model$coefnames
cat("模型期望列数:", length(expected), "\n")

missing <- setdiff(expected, colnames(processed_data))
extra <- setdiff(colnames(processed_data), expected)

if (length(missing) > 0) {
  cat("❌ 缺失的列:", paste(missing, collapse = ", "), "\n")
} else {
  cat("✅ 所有期望的列都存在\n")
}

if (length(extra) > 0) {
  cat("⚠️ 多余的列:", paste(extra, collapse = ", "), "\n")
}

cat("\n========== 测试预测 ==========\n")

probability <- tryCatch({
  # get_prediction(model, processed_data)
  predict(model,processed_data,type = "prob")
}, error = function(e) {
  cat("❌ 预测失败:", e$message, "\n")
  NA
})

if (!is.na(probability)) {
  cat("✅ 预测成功! 概率值:", round(probability, 6), "\n")
}
