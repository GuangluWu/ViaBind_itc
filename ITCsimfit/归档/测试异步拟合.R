# ==============================================================================
# 测试脚本：验证异步拟合功能是否正常工作
# ==============================================================================
# 
# 这个脚本用于验证：
# 1. future 和 promises 包是否正确安装
# 2. 异步拟合功能是否已启用
# 3. 代码是否能正常使用异步模式
#
# 使用方法：
# 1. 在RStudio中打开这个文件
# 2. 运行整个脚本（Ctrl+A, Ctrl+Enter）
# 3. 查看输出，确认异步拟合已启用
# ==============================================================================

cat("========================================\n")
cat("异步拟合功能测试\n")
cat("========================================\n\n")

# 步骤1：检查 future 和 promises 是否安装
cat("步骤1：检查包安装状态\n")
cat("----------------------------------------\n")

future_available <- requireNamespace("future", quietly = TRUE)
promises_available <- requireNamespace("promises", quietly = TRUE)

cat(sprintf("future 可用: %s\n", if(future_available) "✅ 是" else "❌ 否"))
if (future_available) {
  cat(sprintf("  - 版本: %s\n", as.character(packageVersion("future"))))
}

cat(sprintf("promises 可用: %s\n", if(promises_available) "✅ 是" else "❌ 否"))
if (promises_available) {
  cat(sprintf("  - 版本: %s\n", as.character(packageVersion("promises"))))
}

cat("\n")

if (!future_available || !promises_available) {
  cat("❌ 错误：future 或 promises 未安装\n")
  cat("   请运行: install.packages(c('future', 'promises'))\n")
  stop("请先安装必需的包")
}

# 步骤2：加载包并检查 future 计划
cat("步骤2：检查 future 计划配置\n")
cat("----------------------------------------\n")

library(future)
library(promises)

# 检查当前计划
current_plan <- plan()
cat(sprintf("当前 future 计划: %s\n", class(current_plan)[1]))

if (inherits(current_plan, "multisession")) {
  cat("✅ 已配置为 multisession 计划（多进程异步）\n")
} else if (inherits(current_plan, "sequential")) {
  cat("⚠️  当前是 sequential 计划（同步模式）\n")
  cat("   应用启动时会自动配置为 multisession\n")
} else {
  cat(sprintf("ℹ️  当前计划: %s\n", class(current_plan)[1]))
}

cat("\n")

# 步骤3：测试异步计算功能
cat("步骤3：测试异步计算功能\n")
cat("----------------------------------------\n")

# 设置 multisession 计划（模拟应用中的配置）
plan(multisession)

# 创建一个简单的异步任务测试
test_async <- function() {
  cat("  测试异步任务...\n")
  
  # 创建一个耗时的计算任务
  slow_task <- future({
    Sys.sleep(2)  # 模拟耗时操作
    result <- list(
      status = "success",
      value = 42,
      timestamp = Sys.time()
    )
    return(result)
  })
  
  cat("  ✅ 异步任务已启动（不会阻塞）\n")
  cat("  ✅ 可以继续执行其他操作\n")
  
  # 使用 promises 处理结果
  result <- NULL
  error_occurred <- FALSE
  
  tryCatch({
    # 等待结果（这会阻塞，但在实际应用中不会阻塞UI）
    result <- value(slow_task, wait = TRUE, timeout = 5)
    cat("  ✅ 异步任务完成\n")
    cat(sprintf("  ✅ 结果: %s\n", result$status))
  }, error = function(e) {
    cat(sprintf("  ❌ 异步任务失败: %s\n", e$message))
    error_occurred <<- TRUE
  })
  
  return(!error_occurred && !is.null(result))
}

async_test_passed <- test_async()
cat("\n")

# 步骤4：检查代码中的异步拟合实现
cat("步骤4：检查代码中的异步拟合实现\n")
cat("----------------------------------------\n")

# 检查 server.R 中是否有异步拟合代码
server_code <- readLines("server.R", warn = FALSE)
has_future_check <- any(grepl("requireNamespace.*future", server_code))
has_promises_check <- any(grepl("requireNamespace.*promises", server_code))
has_future_call <- any(grepl("future\\(", server_code))
has_promises_chain <- any(grepl("%\\.\\.\\.>%", server_code))

cat(sprintf("检查 future 可用性: %s\n", if(has_future_check) "✅ 有" else "❌ 无"))
cat(sprintf("检查 promises 可用性: %s\n", if(has_promises_check) "✅ 有" else "❌ 无"))
cat(sprintf("使用 future() 调用: %s\n", if(has_future_call) "✅ 有" else "❌ 无"))
cat(sprintf("使用 promises 链式操作: %s\n", if(has_promises_chain) "✅ 有" else "❌ 无"))

code_ok <- has_future_check && has_promises_check && has_future_call && has_promises_chain
cat("\n")

# 步骤5：总结
cat("========================================\n")
cat("测试总结\n")
cat("========================================\n\n")

all_passed <- future_available && promises_available && async_test_passed && code_ok

if (all_passed) {
  cat("✅ 所有测试通过！\n\n")
  cat("异步拟合功能已正确配置：\n")
  cat("  ✅ future 和 promises 包已安装\n")
  cat("  ✅ 异步计算功能正常\n")
  cat("  ✅ 代码中已实现异步拟合\n\n")
  cat("💡 使用建议：\n")
  cat("  1. 启动应用：shiny::runApp()\n")
  cat("  2. 导入实验数据\n")
  cat("  3. 点击拟合按钮\n")
  cat("  4. 观察：界面应该保持响应（不会卡死）\n")
  cat("  5. 可以看到进度通知\n")
  cat("  6. 拟合完成后自动更新参数\n\n")
  cat("🎉 异步拟合功能已就绪！\n")
} else {
  cat("⚠️  部分测试未通过\n\n")
  if (!future_available || !promises_available) {
    cat("  - 请先安装 future 和 promises 包\n")
  }
  if (!async_test_passed) {
    cat("  - 异步计算功能测试失败\n")
  }
  if (!code_ok) {
    cat("  - 代码中可能缺少异步拟合实现\n")
  }
}

cat("\n")
cat("========================================\n")
cat("测试完成！\n")
cat("========================================\n")
