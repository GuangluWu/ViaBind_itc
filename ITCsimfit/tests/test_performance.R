# ==============================================================================
# test_performance.R - 性能监控模块测试
# ==============================================================================

# 加载必要的模块
source("R/constants.R", local = FALSE)
source("R/utils.R", local = FALSE)
source("R/performance.R", local = FALSE)

# 测试计数器
tests_passed <- 0
tests_failed <- 0

# 测试辅助函数
test_that <- function(description, code) {
  cat(sprintf("测试: %s ... ", description))
  result <- tryCatch({
    code
    cat("✓ 通过\n")
    tests_passed <<- tests_passed + 1
    TRUE
  }, error = function(e) {
    cat(sprintf("✗ 失败: %s\n", e$message))
    tests_failed <<- tests_failed + 1
    FALSE
  })
  return(invisible(result))
}

expect_true <- function(condition, message = "条件为 FALSE") {
  if (!condition) {
    stop(message)
  }
}

expect_false <- function(condition, message = "条件为 TRUE") {
  if (condition) {
    stop(message)
  }
}

expect_equal <- function(actual, expected, tolerance = 1e-7, message = NULL) {
  if (is.numeric(actual) && is.numeric(expected)) {
    if (abs(actual - expected) > tolerance) {
      msg <- if (is.null(message)) {
        sprintf("期望 %s, 实际 %s", expected, actual)
      } else {
        message
      }
      stop(msg)
    }
  } else {
    if (!identical(actual, expected)) {
      msg <- if (is.null(message)) {
        sprintf("期望 %s, 实际 %s", expected, actual)
      } else {
        message
      }
      stop(msg)
    }
  }
}

cat("\n")
cat("==============================================================================\n")
cat("性能监控模块测试\n")
cat("==============================================================================\n\n")

# ==============================================================================
# 1. 配置测试
# ==============================================================================

cat("--- 1. 配置测试 ---\n")

test_that("PERF_CONFIG 存在且包含必要字段", {
  expect_true(exists("PERF_CONFIG"))
  expect_true("enabled" %in% names(PERF_CONFIG))
  expect_true("log_level" %in% names(PERF_CONFIG))
  expect_true("slow_threshold" %in% names(PERF_CONFIG))
})

test_that("set_perf_config 可以修改配置", {
  original_enabled <- PERF_CONFIG$enabled
  set_perf_config(enabled = FALSE)
  expect_false(PERF_CONFIG$enabled)
  set_perf_config(enabled = original_enabled)  # 恢复
})

test_that("set_perf_config 可以修改慢操作阈值", {
  original_threshold <- PERF_CONFIG$slow_threshold
  set_perf_config(slow_threshold = 2.5)
  expect_equal(PERF_CONFIG$slow_threshold, 2.5)
  set_perf_config(slow_threshold = original_threshold)  # 恢复
})

# ==============================================================================
# 2. 基础性能监控测试
# ==============================================================================

cat("\n--- 2. 基础性能监控测试 ---\n")

# 清除之前的记录
clear_perf_records()

test_that("perf_monitor 可以执行简单操作", {
  result <- perf_monitor({
    Sys.sleep(0.1)
    42
  }, label = "测试操作", category = "test", auto_log = FALSE)
  
  expect_true(!is.null(result$result))
  expect_equal(result$result, 42)
  expect_true(result$elapsed >= 0.1)
  expect_false(result$error)
})

test_that("perf_monitor 记录到全局存储", {
  clear_perf_records()
  
  perf_monitor({
    1 + 1
  }, label = "简单计算", category = "test", auto_log = FALSE)
  
  records <- get_perf_records()
  expect_true(length(records) == 1)
  expect_equal(records[[1]]$label, "简单计算")
  expect_equal(records[[1]]$category, "test")
})

test_that("perf_monitor 可以捕获错误", {
  result <- perf_monitor({
    stop("测试错误")
  }, label = "错误操作", category = "test", auto_log = FALSE)
  
  expect_true(result$error)
  expect_true(is.null(result$result))
})

test_that("禁用性能监控时直接执行", {
  set_perf_config(enabled = FALSE)
  
  result <- perf_monitor({
    99
  }, label = "禁用时的操作", category = "test")
  
  expect_equal(result$result, 99)
  expect_true(is.null(result$elapsed))
  
  set_perf_config(enabled = TRUE)  # 恢复
})

# ==============================================================================
# 3. 批量性能监控测试
# ==============================================================================

cat("\n--- 3. 批量性能监控测试 ---\n")

test_that("perf_monitor_batch 可以执行多个步骤", {
  clear_perf_records()
  
  results <- perf_monitor_batch(
    steps = list(
      list(expr = quote({ Sys.sleep(0.05); 1 }), label = "步骤1"),
      list(expr = quote({ Sys.sleep(0.05); 2 }), label = "步骤2"),
      list(expr = quote({ Sys.sleep(0.05); 3 }), label = "步骤3")
    ),
    overall_label = "批量测试",
    category = "test"
  )
  
  expect_equal(length(results$results), 3)
  expect_equal(results$results[[1]], 1)
  expect_equal(results$results[[2]], 2)
  expect_equal(results$results[[3]], 3)
  expect_true(results$summary$total_elapsed >= 0.15)
})

test_that("perf_monitor_batch 记录步骤时间", {
  results <- perf_monitor_batch(
    steps = list(
      list(expr = quote({ Sys.sleep(0.02); "a" }), label = "快速步骤"),
      list(expr = quote({ Sys.sleep(0.18); "b" }), label = "慢速步骤")
    ),
    overall_label = "步骤时间测试",
    category = "test"
  )
  
  step_timings <- results$summary$step_timings
  expect_equal(length(step_timings), 2)
  expect_true(step_timings[[2]]$elapsed >= step_timings[[1]]$elapsed)
})

# ==============================================================================
# 4. 性能记录管理测试
# ==============================================================================

cat("\n--- 4. 性能记录管理测试 ---\n")

test_that("get_perf_records 可以筛选类别", {
  clear_perf_records()
  
  perf_monitor({ 1 }, label = "类别A", category = "catA", auto_log = FALSE)
  perf_monitor({ 2 }, label = "类别B", category = "catB", auto_log = FALSE)
  perf_monitor({ 3 }, label = "类别A", category = "catA", auto_log = FALSE)
  
  records_a <- get_perf_records(category = "catA")
  expect_equal(length(records_a), 2)
  
  records_b <- get_perf_records(category = "catB")
  expect_equal(length(records_b), 1)
})

test_that("get_perf_records 可以筛选耗时", {
  clear_perf_records()

  add_perf_record(list(
    timestamp = Sys.time(),
    label = "快",
    category = "test",
    elapsed = 0.05,
    memory_used = NA_real_,
    error = FALSE,
    details = list()
  ))
  add_perf_record(list(
    timestamp = Sys.time(),
    label = "慢",
    category = "test",
    elapsed = 0.15,
    memory_used = NA_real_,
    error = FALSE,
    details = list()
  ))

  slow_records <- get_perf_records(min_elapsed = 0.1)
  expect_equal(length(slow_records), 1)
  expect_equal(slow_records[[1]]$label, "慢")
})

test_that("get_perf_records 可以限制返回数量", {
  clear_perf_records()
  
  for (i in 1:10) {
    perf_monitor({ i }, label = paste("操作", i), category = "test", auto_log = FALSE)
  }
  
  recent_5 <- get_perf_records(last_n = 5)
  expect_equal(length(recent_5), 5)
})

test_that("clear_perf_records 可以清除所有记录", {
  clear_perf_records()
  perf_monitor({ 1 }, label = "测试", category = "test", auto_log = FALSE)
  
  records_before <- get_perf_records()
  expect_true(length(records_before) > 0)
  
  clear_perf_records()
  records_after <- get_perf_records()
  expect_equal(length(records_after), 0)
})

test_that("clear_perf_records 可以清除指定类别", {
  clear_perf_records()
  
  perf_monitor({ 1 }, label = "保留", category = "keep", auto_log = FALSE)
  perf_monitor({ 2 }, label = "删除", category = "remove", auto_log = FALSE)
  
  clear_perf_records(category = "remove")
  
  records <- get_perf_records()
  expect_equal(length(records), 1)
  expect_equal(records[[1]]$category, "keep")
})

# ==============================================================================
# 5. 性能阈值检查测试
# ==============================================================================

cat("\n--- 5. 性能阈值检查测试 ---\n")

test_that("is_slow_operation 正确判断慢操作", {
  set_perf_config(slow_threshold = 1.0)
  
  expect_false(is_slow_operation(0.5))
  expect_true(is_slow_operation(1.5))
  expect_true(is_slow_operation(1.0))
})

test_that("is_slow_operation 可以使用自定义阈值", {
  expect_false(is_slow_operation(0.5, threshold = 1.0))
  expect_true(is_slow_operation(2.0, threshold = 1.0))
})

test_that("should_log 根据配置判断是否记录", {
  set_perf_config(log_level = "none")
  record_none <- list(elapsed = 0.5, error = FALSE, category = "test")
  expect_false(should_log(record_none))
  
  set_perf_config(log_level = "all")
  record_all <- list(elapsed = 0.5, error = FALSE, category = "test")
  expect_true(should_log(record_all))
  
  set_perf_config(log_level = "detailed")
})

# ==============================================================================
# 6. 性能报告测试
# ==============================================================================

cat("\n--- 6. 性能报告测试 ---\n")

test_that("generate_perf_report 可以生成报告", {
  clear_perf_records()
  
  perf_monitor({ Sys.sleep(0.05) }, label = "操作1", category = "test", auto_log = FALSE)
  perf_monitor({ Sys.sleep(0.1) }, label = "操作2", category = "test", auto_log = FALSE)
  
  # 捕获输出以避免干扰测试结果显示
  capture.output({
    report <- generate_perf_report()
  })
  
  expect_true(!is.null(report))
  expect_true(!is.null(report$data))
  expect_equal(nrow(report$data), 2)
})

test_that("generate_perf_report 可以筛选类别", {
  clear_perf_records()
  
  perf_monitor({ 1 }, label = "类别A", category = "catA", auto_log = FALSE)
  perf_monitor({ 2 }, label = "类别B", category = "catB", auto_log = FALSE)
  
  capture.output({
    report <- generate_perf_report(category = "catA")
  })
  
  expect_equal(nrow(report$data), 1)
  expect_equal(report$data$category[1], "catA")
})

# ==============================================================================
# 7. 性能分析测试
# ==============================================================================

cat("\n--- 7. 性能分析测试 ---\n")

test_that("analyze_performance 可以识别慢操作", {
  clear_perf_records()
  set_perf_config(slow_threshold = 0.08)
  
  perf_monitor({ Sys.sleep(0.05) }, label = "快速操作", category = "test", auto_log = FALSE)
  perf_monitor({ Sys.sleep(0.1) }, label = "慢速操作", category = "test", auto_log = FALSE)
  
  capture.output({
    analysis <- analyze_performance()
  })
  
  expect_true(length(analysis$slow_operations) >= 1)
})

# ==============================================================================
# 8. 函数包装器测试
# ==============================================================================

cat("\n--- 8. 函数包装器测试 ---\n")

test_that("with_perf_monitor 可以包装函数", {
  clear_perf_records()
  
  my_func <- function(x) {
    Sys.sleep(0.05)
    x * 2
  }
  
  my_func_monitored <- with_perf_monitor(my_func, label = "我的函数", category = "test")
  
  result <- my_func_monitored(21)
  expect_equal(result, 42)
  
  records <- get_perf_records()
  expect_true(length(records) > 0)
  expect_equal(records[[length(records)]]$label, "我的函数")
})

# ==============================================================================
# 9. 集成测试
# ==============================================================================

cat("\n--- 9. 集成测试 ---\n")

test_that("完整工作流程测试", {
  # 1. 清除记录并配置
  clear_perf_records()
  set_perf_config(enabled = TRUE, slow_threshold = 0.08, auto_log_slow = TRUE)
  
  # 2. 执行一系列操作
  perf_monitor({
    Sys.sleep(0.05)
    sum(1:100)
  }, label = "快速计算", category = "calculation", auto_log = FALSE)
  
  perf_monitor({
    Sys.sleep(0.1)
    sum(1:1000)
  }, label = "慢速计算", category = "calculation", auto_log = FALSE)
  
  # 3. 执行批量操作
  batch_result <- perf_monitor_batch(
    steps = list(
      list(expr = quote({ Sys.sleep(0.03); 1 }), label = "子步骤1"),
      list(expr = quote({ Sys.sleep(0.03); 2 }), label = "子步骤2")
    ),
    overall_label = "批量流程",
    category = "workflow"
  )
  
  # 4. 获取记录
  all_records <- get_perf_records()
  calc_records <- get_perf_records(category = "calculation")
  slow_records <- get_perf_records(min_elapsed = 0.08)
  
  # 5. 验证
  expect_true(length(all_records) >= 3)
  expect_equal(length(calc_records), 2)
  expect_true(length(slow_records) >= 1)
  
  # 6. 生成报告（静默）
  capture.output({
    report <- generate_perf_report()
    analysis <- analyze_performance()
  })
  
  expect_true(!is.null(report))
  expect_true(!is.null(analysis))
})

test_that("性能监控不影响正常执行", {
  # 禁用性能监控
  set_perf_config(enabled = FALSE)
  
  result1 <- perf_monitor({ 1 + 1 }, label = "测试1", category = "test")
  expect_equal(result1$result, 2)
  
  # 启用性能监控
  set_perf_config(enabled = TRUE)
  
  result2 <- perf_monitor({ 2 + 2 }, label = "测试2", category = "test", auto_log = FALSE)
  expect_equal(result2$result, 4)
  expect_true(!is.null(result2$elapsed))
})

# ==============================================================================
# 测试总结
# ==============================================================================

cat("\n")
cat("==============================================================================\n")
cat("测试总结\n")
cat("==============================================================================\n")
cat(sprintf("通过: %d\n", tests_passed))
cat(sprintf("失败: %d\n", tests_failed))
cat(sprintf("总计: %d\n", tests_passed + tests_failed))

if (tests_failed == 0) {
  cat("\n✓ 所有测试通过！\n\n")
  cat(sprintf("通过率：%.1f%%\n", 100))
} else {
  cat(sprintf("\n✗ 有 %d 个测试失败\n\n", tests_failed))
  cat(sprintf("通过率：%.1f%%\n", 100 * tests_passed / (tests_passed + tests_failed)))
}

# 清理
clear_perf_records()
