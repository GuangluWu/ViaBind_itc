# ==============================================================================
# performance.R - 性能监控和分析模块
# ==============================================================================
# 本文件提供性能监控、日志记录和性能分析工具

# ==============================================================================
# 全局性能记录器
# ==============================================================================

# 性能记录存储（全局变量）
if (!exists(".perf_records", envir = .GlobalEnv)) {
  assign(".perf_records", list(), envir = .GlobalEnv)
}

# ==============================================================================
# 性能配置
# ==============================================================================

#' 性能监控配置
PERF_CONFIG <- list(
  # 是否启用性能监控
  enabled = TRUE,
  
  # 日志级别：
  # - "none": 不记录
  # - "summary": 仅记录汇总信息
  # - "detailed": 记录详细信息
  # - "all": 记录所有信息（包括中间步骤）
  log_level = "detailed",
  
  # 慢操作阈值（秒）
  slow_threshold = 1.0,
  
  # 自动记录慢操作
  auto_log_slow = TRUE,
  
  # 最大记录数（防止内存溢出）
  max_records = 1000,
  
  # 性能日志文件路径
  # 默认不落盘；如需落盘可通过 set_perf_config(log_file = "performance.log") 显式开启
  log_file = "",
  
  # 是否在控制台打印
  console_output = FALSE,
  
  # 警告阈值（秒）
  warning_threshold = 5.0
)

#' 设置性能监控配置
#' 
#' @param enabled 是否启用性能监控
#' @param log_level 日志级别（"none", "summary", "detailed", "all"）
#' @param slow_threshold 慢操作阈值（秒）
#' @param ... 其他配置参数
#' @export
set_perf_config <- function(enabled = NULL, 
                           log_level = NULL, 
                           slow_threshold = NULL, 
                           ...) {
  if (!is.null(enabled)) PERF_CONFIG$enabled <<- enabled
  if (!is.null(log_level)) PERF_CONFIG$log_level <<- log_level
  if (!is.null(slow_threshold)) PERF_CONFIG$slow_threshold <<- slow_threshold
  
  # 处理其他参数
  other_params <- list(...)
  for (param_name in names(other_params)) {
    if (param_name %in% names(PERF_CONFIG)) {
      PERF_CONFIG[[param_name]] <<- other_params[[param_name]]
    }
  }
}

# ==============================================================================
# 核心性能监控函数
# ==============================================================================

#' 执行并监控性能（增强版）
#' 
#' @param expr 要执行的表达式
#' @param label 操作标签
#' @param category 操作类别（如 "fitting", "simulation", "data_io"）
#' @param auto_log 是否自动记录到日志
#' @param details 额外的详细信息（列表）
#' @return 列表，包含 result、elapsed、memory_used 等
#' @export
#' 
#' @examples
#' result <- perf_monitor({
#'   perform_heavy_calculation()
#' }, label = "拟合计算", category = "fitting")
perf_monitor <- function(expr, 
                        label = "Unknown Operation", 
                        category = "general",
                        auto_log = TRUE,
                        details = list()) {
  
  if (!PERF_CONFIG$enabled) {
    # 如果性能监控被禁用，直接执行
    return(list(result = eval(expr, envir = parent.frame())))
  }
  
  # 记录开始状态
  start_time <- Sys.time()
  start_mem <- tryCatch({
    gc(reset = TRUE, verbose = FALSE)
    sum(gc()[, "(Mb)"])
  }, error = function(e) NA)
  
  # 执行表达式
  error_occurred <- FALSE
  result <- tryCatch({
    eval(expr, envir = parent.frame())
  }, error = function(e) {
    error_occurred <<- TRUE
    warning(sprintf("性能监控：操作 '%s' 执行失败: %s", label, e$message))
    NULL
  })
  
  # 记录结束状态
  end_time <- Sys.time()
  end_mem <- tryCatch({
    sum(gc(verbose = FALSE)[, "(Mb)"])
  }, error = function(e) NA)
  
  # 计算性能指标
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  memory_used <- if (!is.na(start_mem) && !is.na(end_mem)) {
    end_mem - start_mem
  } else {
    NA
  }
  
  # 构建性能记录
  perf_record <- list(
    timestamp = start_time,
    label = label,
    category = category,
    elapsed = elapsed,
    memory_used = memory_used,
    error = error_occurred,
    details = details
  )
  
  # 记录到全局存储
  add_perf_record(perf_record)
  
  # 自动记录日志
  if (auto_log && should_log(perf_record)) {
    log_perf_record(perf_record)
  }
  
  # 返回结果
  return(list(
    result = result,
    elapsed = elapsed,
    memory_used = memory_used,
    error = error_occurred
  ))
}

#' 判断是否应该记录日志
#' @param record 性能记录
#' @return 逻辑值
should_log <- function(record) {
  if (PERF_CONFIG$log_level == "none") return(FALSE)
  if (PERF_CONFIG$log_level == "all") return(TRUE)
  
  # 如果是慢操作
  if (PERF_CONFIG$auto_log_slow && record$elapsed >= PERF_CONFIG$slow_threshold) {
    return(TRUE)
  }
  
  # 如果有错误
  if (record$error) return(TRUE)
  
  # 根据日志级别判断
  if (PERF_CONFIG$log_level == "detailed") return(TRUE)
  if (PERF_CONFIG$log_level == "summary") {
    # 仅记录重要操作
    return(record$category %in% c("fitting", "optimization", "simulation"))
  }
  
  return(FALSE)
}

#' 添加性能记录到全局存储
#' @param record 性能记录
add_perf_record <- function(record) {
  records <- get(".perf_records", envir = .GlobalEnv)
  
  # 如果记录数超过最大值，删除最旧的记录
  if (length(records) >= PERF_CONFIG$max_records) {
    records <- records[-1]
  }
  
  records[[length(records) + 1]] <- record
  assign(".perf_records", records, envir = .GlobalEnv)
}

#' 记录性能日志
#' @param record 性能记录
log_perf_record <- function(record) {
  # 构建日志消息
  timestamp_str <- format(record$timestamp, "%Y-%m-%d %H:%M:%S")
  
  # 基础信息
  msg <- sprintf("[PERF] %s | %s | %s | %.3fs",
                timestamp_str,
                record$category,
                record$label,
                record$elapsed)
  
  # 添加内存信息
  if (!is.na(record$memory_used)) {
    msg <- paste0(msg, sprintf(" | Mem: %+.2f MB", record$memory_used))
  }
  
  # 添加错误标记
  if (record$error) {
    msg <- paste0(msg, " | ERROR")
  }
  
  # 添加慢操作警告
  if (record$elapsed >= PERF_CONFIG$slow_threshold) {
    msg <- paste0(msg, " | SLOW")
  }
  
  if (record$elapsed >= PERF_CONFIG$warning_threshold) {
    msg <- paste0(msg, " | WARNING")
  }
  
  # 添加详细信息
  if (length(record$details) > 0) {
    details_str <- paste(names(record$details), record$details, sep = "=", collapse = ", ")
    msg <- paste0(msg, " | ", details_str)
  }
  
  # 写入日志文件（仅在显式配置了 log_file 时启用）
  log_file <- as.character(PERF_CONFIG$log_file)[1]
  if (is.na(log_file)) log_file <- ""
  if (nzchar(trimws(log_file))) {
    tryCatch({
      cat(msg, "\n", file = log_file, append = TRUE)
    }, error = function(e) {
      warning("无法写入性能日志文件: ", e$message)
    })
  }
  
  # 控制台输出
  if (PERF_CONFIG$console_output) {
    message(msg)
  }
  
  # 如果是警告级别的慢操作，使用 warning
  if (record$elapsed >= PERF_CONFIG$warning_threshold) {
    warning(sprintf("慢操作警告: %s 耗时 %.2f 秒", record$label, record$elapsed))
  }
}

# ==============================================================================
# 批量性能监控
# ==============================================================================

#' 批量性能监控（用于多个步骤的操作）
#' 
#' @param steps 步骤列表，每个元素包含 expr 和 label
#' @param overall_label 整体操作标签
#' @param category 操作类别
#' @return 列表，包含 results（每个步骤的结果）和 summary（汇总信息）
#' @export
#' 
#' @examples
#' results <- perf_monitor_batch(
#'   steps = list(
#'     list(expr = quote(step1()), label = "步骤1"),
#'     list(expr = quote(step2()), label = "步骤2")
#'   ),
#'   overall_label = "完整流程",
#'   category = "workflow"
#' )
perf_monitor_batch <- function(steps, overall_label = "Batch Operation", category = "general") {
  if (!PERF_CONFIG$enabled) {
    # 如果性能监控被禁用，直接执行所有步骤
    results <- lapply(steps, function(step) {
      eval(step$expr, envir = parent.frame())
    })
    return(list(results = results))
  }
  
  start_time <- Sys.time()
  results <- list()
  step_timings <- list()
  
  for (i in seq_along(steps)) {
    step <- steps[[i]]
    step_label <- if (!is.null(step$label)) step$label else paste("Step", i)
    
    step_result <- perf_monitor(
      step$expr,
      label = paste0(overall_label, " - ", step_label),
      category = category,
      auto_log = FALSE  # 稍后统一记录
    )
    
    results[[i]] <- step_result$result
    step_timings[[i]] <- list(
      label = step_label,
      elapsed = step_result$elapsed
    )
  }
  
  end_time <- Sys.time()
  total_elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # 记录批量操作的汇总
  summary_record <- list(
    timestamp = start_time,
    label = overall_label,
    category = category,
    elapsed = total_elapsed,
    memory_used = NA,
    error = FALSE,
    details = list(
      steps = length(steps),
      step_timings = step_timings
    )
  )
  
  add_perf_record(summary_record)
  log_perf_record(summary_record)
  
  return(list(
    results = results,
    summary = list(
      total_elapsed = total_elapsed,
      step_timings = step_timings
    )
  ))
}

# ==============================================================================
# 性能分析和报告
# ==============================================================================

#' 获取性能记录
#' 
#' @param category 筛选类别（可选）
#' @param min_elapsed 最小耗时筛选（秒，可选）
#' @param last_n 仅返回最近的 n 条记录（可选）
#' @return 性能记录列表
#' @export
get_perf_records <- function(category = NULL, min_elapsed = NULL, last_n = NULL) {
  records <- get(".perf_records", envir = .GlobalEnv)
  
  # 筛选类别
  if (!is.null(category)) {
    records <- Filter(function(r) r$category == category, records)
  }
  
  # 筛选耗时
  if (!is.null(min_elapsed)) {
    records <- Filter(function(r) r$elapsed >= min_elapsed, records)
  }
  
  # 仅返回最近的 n 条
  if (!is.null(last_n) && length(records) > last_n) {
    records <- tail(records, last_n)
  }
  
  return(records)
}

#' 生成性能报告
#' 
#' @param category 筛选类别（可选）
#' @param output_file 输出文件路径（可选，默认打印到控制台）
#' @return 性能统计数据框
#' @export
#' 
#' @examples
#' report <- generate_perf_report()
#' report <- generate_perf_report(category = "fitting", output_file = "perf_report.txt")
generate_perf_report <- function(category = NULL, output_file = NULL) {
  records <- get_perf_records(category = category)
  
  if (length(records) == 0) {
    message("没有性能记录")
    return(NULL)
  }
  
  # 转换为数据框
  df <- do.call(rbind, lapply(records, function(r) {
    data.frame(
      timestamp = as.character(r$timestamp),
      category = r$category,
      label = r$label,
      elapsed = r$elapsed,
      memory_used = ifelse(is.na(r$memory_used), 0, r$memory_used),
      error = r$error,
      stringsAsFactors = FALSE
    )
  }))
  
  # 按类别汇总统计
  stats_by_category <- aggregate(
    elapsed ~ category, 
    data = df, 
    FUN = function(x) {
      c(
        count = length(x),
        total = sum(x),
        mean = mean(x),
        median = median(x),
        min = min(x),
        max = max(x),
        sd = sd(x)
      )
    }
  )
  
  # 构建报告
  report <- list()
  report$summary <- sprintf("性能报告 - 总记录数: %d", nrow(df))
  report$data <- df
  report$stats_by_category <- stats_by_category
  
  # 找出最慢的操作
  top_slow <- head(df[order(-df$elapsed), ], 10)
  report$top_slow <- top_slow
  
  # 打印报告
  cat("\n", rep("=", 80), "\n", sep = "")
  cat(report$summary, "\n")
  cat(rep("=", 80), "\n\n", sep = "")
  
  cat("按类别统计:\n")
  cat(rep("-", 80), "\n", sep = "")
  print(stats_by_category)
  cat("\n")
  
  cat("最慢的 10 个操作:\n")
  cat(rep("-", 80), "\n", sep = "")
  print(top_slow)
  cat("\n")
  
  # 如果指定了输出文件，写入文件
  if (!is.null(output_file)) {
    tryCatch({
      sink(output_file)
      cat(report$summary, "\n\n")
      cat("按类别统计:\n")
      print(stats_by_category)
      cat("\n最慢的 10 个操作:\n")
      print(top_slow)
      sink()
      message("性能报告已保存到: ", output_file)
    }, error = function(e) {
      sink()
      warning("无法写入性能报告文件: ", e$message)
    })
  }
  
  return(invisible(report))
}

#' 清除性能记录
#' 
#' @param category 仅清除指定类别（可选，默认清除所有）
#' @export
clear_perf_records <- function(category = NULL) {
  if (is.null(category)) {
    assign(".perf_records", list(), envir = .GlobalEnv)
    message("已清除所有性能记录")
  } else {
    records <- get(".perf_records", envir = .GlobalEnv)
    records <- Filter(function(r) r$category != category, records)
    assign(".perf_records", records, envir = .GlobalEnv)
    message(sprintf("已清除类别 '%s' 的性能记录", category))
  }
}

# ==============================================================================
# 性能监控装饰器（函数包装器）
# ==============================================================================

#' 为函数添加性能监控
#' 
#' @param func 要包装的函数
#' @param label 函数标签（默认使用函数名）
#' @param category 操作类别
#' @return 包装后的函数
#' @export
#' 
#' @examples
#' my_func_monitored <- with_perf_monitor(my_func, label = "我的函数", category = "calculation")
with_perf_monitor <- function(func, label = NULL, category = "general") {
  if (is.null(label)) {
    label <- deparse(substitute(func))
  }
  
  function(...) {
    result <- perf_monitor(
      func(...),
      label = label,
      category = category
    )
    return(result$result)
  }
}

# ==============================================================================
# 辅助函数：性能阈值检查
# ==============================================================================

#' 检查操作是否超过性能阈值
#' 
#' @param elapsed 耗时（秒）
#' @param threshold 阈值（秒，默认使用配置的慢操作阈值）
#' @return 逻辑值
#' @export
is_slow_operation <- function(elapsed, threshold = NULL) {
  if (is.null(threshold)) {
    threshold <- PERF_CONFIG$slow_threshold
  }
  return(elapsed >= threshold)
}

#' 性能警告（当操作过慢时）
#' 
#' @param label 操作标签
#' @param elapsed 耗时（秒）
#' @param threshold 阈值（秒，默认使用配置的警告阈值）
perf_warning <- function(label, elapsed, threshold = NULL) {
  if (is.null(threshold)) {
    threshold <- PERF_CONFIG$warning_threshold
  }
  
  if (elapsed >= threshold) {
    warning(sprintf("性能警告: '%s' 耗时 %.2f 秒（阈值: %.2f 秒）", 
                   label, elapsed, threshold))
  }
}

# ==============================================================================
# 性能优化建议
# ==============================================================================

#' 分析性能瓶颈并提供优化建议
#' 
#' @param category 筛选类别（可选）
#' @return 优化建议列表
#' @export
analyze_performance <- function(category = NULL) {
  records <- get_perf_records(category = category)
  
  if (length(records) == 0) {
    message("没有性能记录可供分析")
    return(NULL)
  }
  
  # 找出慢操作
  slow_ops <- Filter(function(r) r$elapsed >= PERF_CONFIG$slow_threshold, records)
  
  # 按操作标签分组统计
  label_stats <- aggregate(
    elapsed ~ label,
    data = do.call(rbind, lapply(records, function(r) {
      data.frame(label = r$label, elapsed = r$elapsed)
    })),
    FUN = function(x) c(count = length(x), mean = mean(x), total = sum(x))
  )
  
  # 排序找出最耗时的操作
  label_stats <- label_stats[order(-label_stats$elapsed[, "total"]), ]
  
  cat("\n", rep("=", 80), "\n", sep = "")
  cat("性能瓶颈分析\n")
  cat(rep("=", 80), "\n\n", sep = "")
  
  cat(sprintf("总操作数: %d\n", length(records)))
  cat(sprintf("慢操作数: %d (%.1f%%)\n", 
             length(slow_ops), 
             100 * length(slow_ops) / length(records)))
  cat("\n")
  
  cat("最耗时的操作 (按总耗时排序):\n")
  cat(rep("-", 80), "\n", sep = "")
  print(head(label_stats, 10))
  cat("\n")
  
  # 优化建议
  cat("优化建议:\n")
  cat(rep("-", 80), "\n", sep = "")
  
  suggestions <- character()
  
  if (length(slow_ops) > 0) {
    suggestions <- c(suggestions, 
                    sprintf("- 发现 %d 个慢操作，考虑优化算法或使用缓存", length(slow_ops)))
  }
  
  # 检查是否有重复的慢操作
  repeated_slow <- label_stats[label_stats$elapsed[, "count"] > 1 & 
                                label_stats$elapsed[, "mean"] >= PERF_CONFIG$slow_threshold, ]
  if (nrow(repeated_slow) > 0) {
    suggestions <- c(suggestions,
                    "- 发现重复执行的慢操作，考虑添加缓存机制")
  }
  
  # 检查内存使用
  mem_records <- Filter(function(r) !is.na(r$memory_used) && r$memory_used > 100, records)
  if (length(mem_records) > 0) {
    suggestions <- c(suggestions,
                    "- 发现高内存使用操作，考虑优化数据结构或分批处理")
  }
  
  if (length(suggestions) == 0) {
    suggestions <- c("- 当前性能表现良好，继续保持！")
  }
  
  for (s in suggestions) {
    cat(s, "\n")
  }
  cat("\n")
  
  return(invisible(list(
    slow_operations = slow_ops,
    label_stats = label_stats,
    suggestions = suggestions
  )))
}
