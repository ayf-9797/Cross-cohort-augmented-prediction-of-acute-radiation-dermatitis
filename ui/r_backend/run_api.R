# ============================================================================
# 启动 R Plumber API 服务
# ============================================================================
#
# Windows 使用方法:
#   方式1: 在 RStudio 中打开此文件 → 点击 "Source" 按钮 (或 Ctrl+Shift+Enter)
#   方式2: 在 cmd/PowerShell 中: cd r_backend && Rscript run_api.R
#
# ============================================================================

# ------------------------------------------------------------------
# 安装依赖（首次运行时自动安装）
# ------------------------------------------------------------------
required_packages <- c("plumber", "jsonlite", "later")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("[安装]", pkg, "...\n")
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

# ------------------------------------------------------------------
# 自动定位脚本所在目录
# ------------------------------------------------------------------
get_script_dir <- function() {
  # 方式1: RStudio Source
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    return(dirname(rstudioapi::getSourceEditorContext()$path))
  }
  # 方式2: Rscript 命令行
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("--file=", "", file_arg), winslash = "/")))
  }
  # 方式3: source() 调用
  if (!is.null(sys.frames())) {
    for (i in seq_along(sys.frames())) {
      env <- sys.frames()[[i]]
      if (exists("ofile", envir = env)) {
        return(dirname(normalizePath(get("ofile", envir = env), winslash = "/")))
      }
    }
  }
  # 兜底
  return(normalizePath(".", winslash = "/"))
}

script_dir <- get_script_dir()
setwd(script_dir)

cat("============================================================\n")
cat("  Acute Radiation Dermatitis Prediction API\n")
cat("============================================================\n")
cat("  Operating System:", Sys.info()["sysname"], "\n")
cat("  R Version:       ", R.version.string, "\n")
cat("  Working Dir:     ", getwd(), "\n")
cat("============================================================\n\n")

# ------------------------------------------------------------------
# 检查模型文件
# ------------------------------------------------------------------
model_file <- "Adaboost_best_model.rds"

if (!file.exists(model_file)) {
  cat("[错误] 未找到模型文件:", normalizePath(model_file, mustWork = FALSE, winslash = "/"), "\n\n")
  cat("请将您的模型文件放在以下目录:\n")
  cat("  ", normalizePath(".", winslash = "/"), "\n\n")
  cat("然后重新运行此脚本。\n")
  stop("模型文件不存在")
}

# ------------------------------------------------------------------
# 启动 API
# ------------------------------------------------------------------
cat("[启动] 加载 plumber.R ...\n")

pr <- plumber::plumb("plumber.R")

host <- "127.0.0.1"
port <- 8000

# ------------------------------------------------------------------
# 挂载前端静态文件（核心！）
# ------------------------------------------------------------------
# 将前端 HTML 文件放在 r_backend/www/ 文件夹中
# 访问 http://127.0.0.1:8000/ 即可打开前端界面
# ------------------------------------------------------------------

www_dir <- file.path(getwd(), "www")

if (dir.exists(www_dir) && file.exists(file.path(www_dir, "index.html"))) {
  pr$mount("/", plumber::PlumberStatic$new(www_dir))
  cat("[前端] 已挂载前端页面 ✓\n")
  cat("  文件位置: ", normalizePath(file.path(www_dir, "index.html"), winslash = "/"), "\n")
  cat("  访问地址: http://", host, ":", port, "/\n\n", sep = "")
} else {
  cat("[前端] 未找到前端页面文件\n")
  cat("  期望位置: ", normalizePath(file.path(www_dir, "index.html"), mustWork = FALSE, winslash = "/"), "\n")
  cat("  请将构建好的 index.html 复制到: ", normalizePath(www_dir, mustWork = FALSE, winslash = "/"), "\n")
  cat("  或直接在浏览器中打开 index.html 文件\n\n")
}

cat("[启动] API 服务即将运行:\n")
cat("  ┌──────────────────────────────────────────────────────┐\n")
cat("  │                                                      │\n")
if (dir.exists(www_dir) && file.exists(file.path(www_dir, "index.html"))) {
cat("  │  ★ 打开浏览器访问: http://127.0.0.1:8000/           │\n")
cat("  │    前端界面 + 后端API 已整合在同一地址               │\n")
} else {
cat("  │  ★ 直接用浏览器打开 dist/index.html 文件            │\n")
cat("  │    或将其复制到 r_backend/www/ 文件夹后访问:         │\n")
cat("  │    http://127.0.0.1:8000/                            │\n")
}
cat("  │                                                      │\n")
cat("  └──────────────────────────────────────────────────────┘\n")
cat("\n")
cat("  API 端点:\n")
cat("    健康检查: GET  http://", host, ":", port, "/api/health\n", sep = "")
cat("    模型信息: GET  http://", host, ":", port, "/api/model-info\n", sep = "")
cat("    预测接口: POST http://", host, ":", port, "/api/predict\n", sep = "")
cat("\n  按 Esc (RStudio) 或 Ctrl+C (命令行) 停止服务\n")
cat("============================================================\n\n")

# ------------------------------------------------------------------
# 自动打开浏览器 (可选)
# ------------------------------------------------------------------
# 延迟1.5秒后自动打开浏览器
later::later(function() {
  url <- paste0("http://", host, ":", port, "/")
  cat("[INFO] 正在打开浏览器: ", url, "\n")
  tryCatch(
    utils::browseURL(url),
    error = function(e) {
      cat("[提示] 无法自动打开浏览器，请手动访问: ", url, "\n")
    }
  )
}, delay = 1.5)

pr$run(host = host, port = port)
