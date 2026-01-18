###LASSO####
# 安装并加载必要的包
#install.packages("glmnet")
#install.packages("caret")  # 可选，用于数据划分
library(glmnet)
library(caret)
library(readxl)
library(dplyr)
library(magrittr)
# 假设你的数据框名为 df
# 因变量：psoriasis （0/1）
# 连续变量：cont1, cont2, ..., cont14
# 分类变量：cat1, cat2, ..., cat6（这些是 factor 类型）

# 1. 读取Excel数据并划分训练集和验证集
# 小模型
# data <- read_excel("../data/202601/single_cohort_map.xlsx")
# 大模型
data <- read_excel("../data/202601/ccp_impulate.xlsx")
#sig_data <- read_excel("../数据分析/ref.xlsx")
set.seed(100)  # 为了可重复性
# 假设分类变量是因子类型，连续变量是数值型
identify_categories <- function(x) {
  category_threshold <- 13
  if (is.numeric(x)) {
    return(length(unique(x)) <= category_threshold)
  }
  return(TRUE)
}
identify_nums <- function(x){
  if (is.numeric(x)){
    return(TRUE)
  }
  return(FALSE)
}
category_vars <- names(data)[sapply(data, identify_categories)]
data[category_vars] <- lapply(data[category_vars], as.factor)
data$psoriasis <- as.factor(data$psoriasis)
scale_vars <- names(data)[sapply(data, identify_nums)]
data[scale_vars] <- lapply(data[scale_vars], scale)

# 2. 创建模型矩阵（自动处理哑变量）
# 去掉因变量，剩余变量生成矩阵
x <- model.matrix(psoriasis ~ ., data = data)[, -1]  # 去掉截距项

# 3. 设置因变量（必须是向量）
y <- data$psoriasis

# 4. 使用交叉验证选择最佳 lambda
cvfit <- cv.glmnet(x, y, family = "binomial", alpha = 1, nfolds = 10)


# 5. 绘制交叉验证误差图（可选）
png( 
  filename = "lasso.png", # 文件名称
  width = 10,            # 宽
  height = 10,           # 高
  units = "in",          # 单位
  bg = "white",          # 背景颜色
  res = 300)             # 分辨率
# 2. 绘图
plot(cvfit) 
# 3. 关闭画布
dev.off()
# 6. 提取最佳 lambda
best_lambda <- cvfit$lambda.min
cat("Best lambda:", best_lambda, "\n")

# 7. 查看对应 lambda 下的系数
coef_lasso <- coef(cvfit, s = "lambda.min")
print(coef_lasso)

# 8. 提取非零变量名（去掉截距项）
selected_vars <- rownames(coef_lasso)[which(coef_lasso != 0)]
selected_vars <- selected_vars[selected_vars != "(Intercept)"]
cat("Selected variables:\n")
print(selected_vars)