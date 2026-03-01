# R Backend — 急性放射性皮炎预测 API

## 文件说明

| 文件 | 功能 | 何时运行 |
|------|------|----------|
| `plumber.R` | API 核心逻辑：数据预处理 + 哑变量生成 + 模型预测 | 由 run_api.R 自动加载 |
| `setup_model.R` | 初始化：提取模型特征列名，生成辅助文件 | **部署前运行一次** |
| `check_dummy_vars.R` | 诊断：对比哑变量列名是否与模型匹配 | 调试时运行 |
| `deploy_frontend.R` | 将前端HTML部署到www/目录 | **部署前运行一次** |
| `run_api.R` | 启动 API 服务 + 托管前端页面 | 每次启动时运行 |
| `www/index.html` | 前端页面（由 deploy_frontend.R 复制） | 自动生成 |
| `model.rds` | 您的模型文件（需自行放置） | — |
| `model_colnames.rds` | 模型期望的特征列名（由 setup_model.R 生成） | 自动生成 |
| `training_template.rds` | 训练数据模板（可选，由 setup_model.R 生成） | 自动生成 |

## 快速开始 (Windows) — 完整5步部署

### 第一步：准备模型文件

1. 将您的模型文件复制到 `r_backend\` 目录，命名为 `model.rds`
2. 如果文件名不同，修改 `plumber.R` 中的 `readRDS("model.rds")`

### 第二步：初始化（只需运行一次）

在 RStudio 中：
1. 打开 `setup_model.R`
2. 点击 **Source** 按钮（或 `Ctrl+Shift+Enter`）
3. 检查输出，确认模拟预测是否成功

### 第三步：根据模型类型调整预处理

打开 `plumber.R`，找到 `preprocess_input()` 函数：

**如果您的模型是 glm / randomForest / rpart：**
```r
# 取消 "方法0" 的注释:
return(factor_df)
```

**如果您的模型是 xgboost / glmnet 等需要数值矩阵：**
```r
# 默认使用 "方法A"，无需修改:
dummy_df <- dummy_by_factor_levels(factor_df)
```

**如果方法A的列名与模型不匹配：**
```r
# 运行 check_dummy_vars.R 诊断
# 或改用 "方法B":
dummy_df <- dummy_by_template(factor_df)
```

### 第四步：部署前端页面

在 RStudio 中：
1. 打开 `deploy_frontend.R`
2. 点击 **Source** 按钮
3. 脚本会自动将 `dist/index.html` 复制到 `r_backend/www/` 目录

> 也可以手动操作：创建 `r_backend\www\` 文件夹，将 `dist\index.html` 复制进去。

### 第五步：启动并使用

在 RStudio 中：
1. 打开 `run_api.R`
2. 点击 **Source** 按钮
3. 浏览器会自动打开 `http://127.0.0.1:8000/`
4. 在网页中选择患者特征，点击 **Generate Prediction**

```
┌─────────────────────────────────────────────────────┐
│                                                     │
│  ★ 只需访问一个地址: http://127.0.0.1:8000/        │
│    前端界面 + 后端API 已整合在同一服务上            │
│                                                     │
└─────────────────────────────────────────────────────┘
```

> **备选方案**：如果不想执行第四步，也可以直接用浏览器打开 `dist\index.html` 文件（双击），
> 它会自动连接到 `http://127.0.0.1:8000` 的 R 后端。

### 验证后端是否正常

打开浏览器访问：http://127.0.0.1:8000/api/health

应返回：
```json
{"status":"ok","message":"R Prediction API is running","model_loaded":true}
```

## 哑变量生成方法说明

### 方法0：直接使用 Factor（不生成哑变量）

```
前端 {"Sex": 1} → factor("Female", levels=c("Male","Female")) → predict(model, df)
```

适用于 R 的 `predict()` 能自动处理 factor 的模型（glm, randomForest, rpart）。

### 方法A：Factor Levels 法（推荐）

```
前端 {"Sex": 1} → factor("Female") → model.matrix() → SexFemale=1 → predict()
```

R 的 `model.matrix()` 会根据 factor 定义的 `levels` 自动生成所有哑变量列。
即使只有 1 行数据，也会为不存在的 level 生成 0 值列。

**关键前提**：`FACTOR_DEFINITIONS` 中的 `labels` 必须与训练数据的 factor levels 一致。

### 方法B：模板合并法

```
模板数据(含所有level) + 新数据 → rbind → model.matrix() → 提取最后1行 → predict()
```

当方法A的列名与模型期望不一致时使用。
通过与包含所有 level 的模板数据合并，保证 `model.matrix()` 的输出列名与训练时完全一致。

## 常见问题

### Q: 哑变量列名含有特殊字符怎么办？

R 的 `model.matrix()` 会将 factor label 直接拼接到变量名后面。如果 label 含有空格、逗号、括号等特殊字符，生成的列名也会包含这些字符。

解决方案：
1. 运行 `check_dummy_vars.R` 查看具体的列名
2. 使用 `align_columns()` 函数自动对齐（需要 `model_colnames.rds`）
3. 或修改 `FACTOR_DEFINITIONS` 中的 labels，使其与训练数据完全一致

### Q: 如何确认我的模型训练时用的 factor levels？

在您的训练环境中运行：
```r
# 查看每个变量的 factor levels
for (col in names(training_data)) {
  if (is.factor(training_data[[col]])) {
    cat(col, ": ", paste(levels(training_data[[col]]), collapse = ", "), "\n")
  }
}
```
