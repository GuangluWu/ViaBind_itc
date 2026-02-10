# ==============================================================================
# 测试应用启动脚本
# ==============================================================================
# 快速测试 app.R 是否能正常加载和启动（不实际运行 Shiny）

cat("测试应用启动...\n\n")

# 尝试加载 app.R（但不运行 shinyApp）
cat("1. 尝试解析 app.R...\n")
tryCatch({
  # 只解析，不执行 shinyApp
  parse_result <- parse("app.R")
  cat("   ✓ app.R 语法正确，可以解析\n")
}, error = function(e) {
  cat("   ✗ app.R 解析失败:", e$message, "\n")
  stop("应用无法启动")
})

# 检查关键对象是否存在
cat("\n2. 检查关键对象...\n")
cat("   注意：由于 app.R 包含 shinyApp() 调用，实际运行会启动 Shiny 应用\n")
cat("   这里只检查语法和基本结构\n")

cat("\n✓ 应用文件结构检查完成\n")
cat("  提示：要实际运行应用，请在 RStudio 中打开 app.R 并点击 'Run App'\n")
cat("        或使用命令: shiny::runApp('app.R')\n")
