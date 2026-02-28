# ============================================================================
# 前端部署脚本 — 将构建好的前端页面部署到 R 后端
# Frontend Deployment Script
# ============================================================================
#
# 功能:
#   将 dist/index.html (React 构建产物) 复制到 r_backend/www/ 目录
#   这样 R Plumber 启动后可以在同一端口同时提供前端页面和 API 服务
#
# 使用方法:
#   在 RStudio 中打开此文件 → 点击 Source (Ctrl+Shift+Enter)
#
# ============================================================================

cat("============================================================\n")
cat("  前端部署脚本\n")
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

cat("[INFO] 当前目录:", getwd(), "\n\n")

# ------------------------------------------------------------------
# 查找 index.html
# ------------------------------------------------------------------

# 可能的路径 (根据您的项目结构调整)
possible_paths <- c(
  file.path("..", "dist", "index.html"),     # 默认: 上级目录的 dist/
  file.path(".", "dist", "index.html"),      # 当前目录的 dist/
  file.path("..", "..", "dist", "index.html") # 上两级目录的 dist/
)

source_file <- NULL
for (p in possible_paths) {
  if (file.exists(p)) {
    source_file <- normalizePath(p, winslash = "/")
    break
  }
}

if (is.null(source_file)) {
  cat("[错误] 未找到 index.html 文件!\n\n")
  cat("已搜索的路径:\n")
  for (p in possible_paths) {
    cat("  ", normalizePath(p, mustWork = FALSE, winslash = "/"), "\n")
  }
  cat("\n")
  cat("请手动指定路径:\n")
  cat("  1. 修改下方 source_file 变量\n")
  cat("  2. 或手动复制 index.html 到", normalizePath("www", mustWork = FALSE, winslash = "/"), "\n\n")
  
  # >>> 手动指定路径 (取消注释并修改) <<<
  # source_file <- "C:/Users/YourName/project/dist/index.html"
  
  if (is.null(source_file)) stop("未找到前端文件")
}

cat("[找到] 前端文件:", source_file, "\n")
cat("  文件大小:", round(file.info(source_file)$size / 1024, 1), "KB\n\n")

# ------------------------------------------------------------------
# 创建 www 目录并复制
# ------------------------------------------------------------------

www_dir <- file.path(getwd(), "www")
if (!dir.exists(www_dir)) {
  dir.create(www_dir)
  cat("[创建] www 目录:", normalizePath(www_dir, winslash = "/"), "\n")
}

dest_file <- file.path(www_dir, "index.html")
file.copy(source_file, dest_file, overwrite = TRUE)

if (file.exists(dest_file)) {
  cat("[成功] 前端页面已部署!\n")
  cat("  位置:", normalizePath(dest_file, winslash = "/"), "\n\n")
  cat("============================================================\n")
  cat("  部署完成!\n")
  cat("============================================================\n\n")
  cat("下一步:\n")
  cat("  1. 在 RStudio 中打开 run_api.R → 点击 Source\n")
  cat("  2. 浏览器会自动打开 http://127.0.0.1:8000/\n")
  cat("  3. 前端界面和后端 API 运行在同一个地址\n\n")
  cat("  如果浏览器没有自动打开，请手动访问:\n")
  cat("  http://127.0.0.1:8000/\n")
} else {
  cat("[错误] 复制失败!\n")
  cat("  请手动将以下文件:\n")
  cat("    ", source_file, "\n")
  cat("  复制到:\n")
  cat("    ", normalizePath(dest_file, mustWork = FALSE, winslash = "/"), "\n")
}
