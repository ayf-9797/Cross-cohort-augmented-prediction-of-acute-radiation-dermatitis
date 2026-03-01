# ============================================================================
# Plumber API - AdaBoost 预测服务
# 文件名: plumber.R
# ============================================================================

library(plumber)
library(jsonlite)

# ============================================================================
# 加载模型
# ============================================================================

model <- readRDS("Adaboost_best_model.rds")

# ============================================================================
# 配置参数
# ============================================================================

OPTIMAL_THRESHOLD <- 0.529

# ============================================================================
# 模型性能指标
# ============================================================================

MODEL_METRICS <- list(
  auc = 0.923,
  sensitivity = 0.83,
  specificity = 0.98,
  accuracy = 0.92,
  ppv = 0.95,
  npv = 0.91,
  training_cohort = "Development Cohort (n=1087)",
  validation_cohort = "External Validation Cohort (n=63)",
  total_patients = 1150
)

# ============================================================================
# Factor 定义（用于前端显示标签映射）
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
  
  NRS2002 = list(
    levels = c(0, 1, 2, 22, 3),
    labels = c("Score 0", "Score 1", "Score 2", "Score 22", "Score 3")
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

# ============================================================================
# 连续变量列表
# ============================================================================

CONTINUOUS_VARS <- c("Age", "Dose", "Average_dose")

# ============================================================================
# CORS 过滤器
# ============================================================================

#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  
  plumber::forward()
}

# ============================================================================
# 辅助函数：从训练数据获取 Factor levels
# ============================================================================

get_training_levels <- function(model, var_name) {
  if (!is.null(model$trainingData) && var_name %in% names(model$trainingData)) {
    return(levels(model$trainingData[[var_name]]))
  }
  return(NULL)
}

# ============================================================================
# 整数编码 → Factor（使用训练数据的 levels 确保一致性）
# ============================================================================

integers_to_factors <- function(raw_data, model) {
  df_list <- list()
  
  for (var_name in names(FACTOR_DEFINITIONS)) {
    value <- raw_data[[var_name]]
    
    if (is.null(value)) {
      stop(sprintf("缺少必要变量: %s", var_name))
    }
    
    # 从模型训练数据获取 levels
    train_levels <- get_training_levels(model, var_name)
    
    if (!is.null(train_levels)) {
      # 使用训练数据的 levels（确保与模型完全一致）
      df_list[[var_name]] <- factor(
        as.character(value),
        levels = train_levels
      )
    } else {
      # 备用：使用预定义的 levels
      fdef <- FACTOR_DEFINITIONS[[var_name]]
      df_list[[var_name]] <- factor(
        as.character(value),
        levels = as.character(fdef$levels)
      )
    }
  }
  
  df <- as.data.frame(df_list, stringsAsFactors = TRUE)
  return(df)
}

# ============================================================================
# 数据预处理（Factor + 连续变量）
# ============================================================================

preprocess_input <- function(raw_data, model) {
  
  cat("[预处理] 开始处理数据...\n")
  
  # ------------------------------------------------------------------
  # 步骤1: 分类变量 → Factor
  # ------------------------------------------------------------------
  result_df <- integers_to_factors(raw_data, model)
  
  cat("[预处理] Factor 转换完成 (", ncol(result_df), "个分类变量)\n")
  cat("  ", paste(names(result_df), "=", sapply(result_df, as.character), collapse = ", "), "\n")
  
  # ------------------------------------------------------------------
  # 步骤2: 添加连续变量
  # ------------------------------------------------------------------
  for (var_name in CONTINUOUS_VARS) {
    value <- raw_data[[var_name]]
    
    if (is.null(value)) {
      stop(sprintf("缺少必要连续变量: %s", var_name))
    }
    
    result_df[[var_name]] <- as.numeric(value)
    cat("[预处理] 连续变量:", var_name, "=", value, "\n")
  }
  
  cat("[预处理] 完成! 维度:", nrow(result_df), "x", ncol(result_df), "\n")
  
  return(result_df)
}

# ============================================================================
# 模型预测
# ============================================================================

get_prediction <- function(model, processed_data) {
  
  # caret 训练的 AdaBoost 模型
  # 直接传入 Factor 数据框，模型内部会自动处理哑变量转换
  prob_result <- predict(model, newdata = processed_data, type = "prob")
  
  # 提取正类概率（第二列或 "1" 列）
  if ("1" %in% colnames(prob_result)) {
    probability <- prob_result[1, "1"]
  } else if ("Yes" %in% colnames(prob_result)) {
    probability <- prob_result[1, "Yes"]
  } else {
    probability <- prob_result[1, 2]
  }
  
  return(as.numeric(probability))
}

# ============================================================================
# API 端点：健康检查
# ============================================================================

#* @get /api/health
function() {
  list(
    status = "ok",
    message = "R Prediction API is running",
    model_loaded = exists("model"),
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
}

# ============================================================================
# API 端点：获取模型信息
# ============================================================================

#* @get /api/model-info
function() {
  list(
    threshold = OPTIMAL_THRESHOLD,
    metrics = MODEL_METRICS,
    categorical_variables = names(FACTOR_DEFINITIONS),
    continuous_variables = CONTINUOUS_VARS,
    factor_definitions = FACTOR_DEFINITIONS
  )
}

# ============================================================================
# API 端点：执行预测
# ============================================================================

#* @post /api/predict
#* @serializer json
function(req) {
  
  tryCatch({
    # 解析请求体
    body <- jsonlite::fromJSON(req$postBody)
    
    # ==================================================================
    # 前端变量名 → 模型变量名 映射（兼容前端旧字段名）
    # ==================================================================
    NAME_MAPPING <- list(
      "Treatment_site" = "Target_Volume"
      # 如果还有其他前端名与模型名不一致的，在这里添加
      # "前端名" = "模型名"
    )
    
    # 执行变量名映射
    for (old_name in names(NAME_MAPPING)) {
      new_name <- NAME_MAPPING[[old_name]]
      if (!is.null(body[[old_name]]) && is.null(body[[new_name]])) {
        body[[new_name]] <- body[[old_name]]
        cat("[映射]", old_name, "→", new_name, "\n")
      }
    }
    
    # ==================================================================
    # 验证 + 解析输入
    # ==================================================================
    raw_data <- list()
    missing_vars <- c()
    
    # 解析分类变量
    for (var_name in names(FACTOR_DEFINITIONS)) {
      value <- body[[var_name]]
      if (is.null(value) || length(value) == 0) {
        missing_vars <- c(missing_vars, var_name)
      } else {
        raw_data[[var_name]] <- as.integer(value)
      }
    }
    
    # 解析连续变量
    for (var_name in CONTINUOUS_VARS) {
      value <- body[[var_name]]
      if (is.null(value) || length(value) == 0) {
        missing_vars <- c(missing_vars, var_name)
      } else {
        raw_data[[var_name]] <- as.numeric(value)
      }
    }
    
    # 如果有缺失变量，返回详细错误
    if (length(missing_vars) > 0) {
      cat("[错误] 缺少变量:", paste(missing_vars, collapse = ", "), "\n")
      return(list(
        success = FALSE,
        error = paste("Missing required variables:", 
                      paste(missing_vars, collapse = ", ")),
        required_categorical = names(FACTOR_DEFINITIONS),
        required_continuous = CONTINUOUS_VARS,
        probability = NULL,
        threshold = OPTIMAL_THRESHOLD,
        classification = NULL,
        is_high_risk = NULL,
        metrics = MODEL_METRICS
      ))
    }
    
    # ==================================================================
    # 预处理 + 预测
    # ==================================================================
    cat("\n========== 新的预测请求 ==========\n")
    cat("[输入] 分类变量:\n")
    cat("  ", paste(names(FACTOR_DEFINITIONS), "=",
                    sapply(names(FACTOR_DEFINITIONS), function(x) raw_data[[x]]),
                    collapse = ", "), "\n")
    cat("[输入] 连续变量:\n")
    cat("  ", paste(CONTINUOUS_VARS, "=",
                    sapply(CONTINUOUS_VARS, function(x) raw_data[[x]]),
                    collapse = ", "), "\n")
    
    processed_data <- preprocess_input(raw_data, model)
    probability <- get_prediction(model, processed_data)
    
    classification <- ifelse(probability >= OPTIMAL_THRESHOLD, "High Risk", "Low Risk")
    is_high_risk <- probability >= OPTIMAL_THRESHOLD
    
    cat("[结果] 预测概率:", round(probability, 6), "\n")
    cat("[结果] 分类:", classification, "\n")
    cat("====================================\n\n")
    
    list(
      success = TRUE,
      probability = round(probability, 6),
      threshold = OPTIMAL_THRESHOLD,
      classification = classification,
      is_high_risk = is_high_risk,
      metrics = MODEL_METRICS
    )
    
  }, error = function(e) {
    cat("[错误]", e$message, "\n")
    list(
      success = FALSE,
      error = paste("Prediction failed:", e$message),
      probability = NULL,
      threshold = OPTIMAL_THRESHOLD,
      classification = NULL,
      is_high_risk = NULL,
      metrics = MODEL_METRICS
    )
  })
}