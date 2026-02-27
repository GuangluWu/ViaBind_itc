# ==============================================================================
# utils.R - 工具函数和统一的错误处理
# ==============================================================================
# 本文件包含通用工具函数、错误处理机制和辅助函数

# 兼容模式：允许该文件被独立 source（例如 tests）时仍可工作。
if (!exists("format_itc_error", mode = "function")) {
  format_itc_error <- function(e, context = "", code = NULL) {
    msg <- tryCatch(as.character(e$message), error = function(...) "Unknown error")
    msg <- if (length(msg) > 0 && nzchar(msg[1])) msg[1] else "Unknown error"
    if (!is.null(code) && nzchar(as.character(code)[1])) msg <- paste0("[", code, "] ", msg)
    if (nzchar(context)) msg <- paste0("[", context, "] ", msg)
    msg
  }
}

append_log_entry_safe <- function(entry, primary_file, fallback_file) {
  write_entry <- function(path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    cat(paste0(entry, "\n"), file = path, append = TRUE)
  }

  ok <- tryCatch({
    write_entry(primary_file)
    TRUE
  }, error = function(e) FALSE)

  if (!isTRUE(ok)) {
    tryCatch({
      write_entry(fallback_file)
    }, error = function(e) {
      warning("Failed to append log entry: ", e$message, call. = FALSE)
    })
  }
}

if (!exists("itc_log_info", mode = "function")) {
  itc_log_info <- function(message, context = "", log_to_file = TRUE) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ctx <- if (nzchar(context)) paste0("[", context, "] ") else ""
    entry <- paste0(timestamp, " | INFO | ", ctx, as.character(message)[1])
    message(entry)
    if (isTRUE(log_to_file)) {
      log_file <- if (exists("FILE_PATHS")) FILE_PATHS$session_log else "session.log"
      fallback_file <- file.path(tempdir(), "itcsuite", "logs", "session.log")
      append_log_entry_safe(entry, log_file, fallback_file)
    }
    invisible(entry)
  }
}

if (!exists("itc_log_warn", mode = "function")) {
  itc_log_warn <- function(message, context = "", log_to_file = TRUE) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ctx <- if (nzchar(context)) paste0("[", context, "] ") else ""
    entry <- paste0(timestamp, " | WARN | ", ctx, as.character(message)[1])
    warning(entry, call. = FALSE)
    if (isTRUE(log_to_file)) {
      log_file <- if (exists("FILE_PATHS")) FILE_PATHS$error_log else "error.log"
      fallback_file <- file.path(tempdir(), "itcsuite", "logs", "error.log")
      append_log_entry_safe(entry, log_file, fallback_file)
    }
    invisible(entry)
  }
}

if (!exists("itc_log_error", mode = "function")) {
  itc_log_error <- function(message, context = "", log_to_file = TRUE) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ctx <- if (nzchar(context)) paste0("[", context, "] ") else ""
    entry <- paste0(timestamp, " | ERROR | ", ctx, as.character(message)[1])
    warning(entry, call. = FALSE)
    if (isTRUE(log_to_file)) {
      log_file <- if (exists("FILE_PATHS")) FILE_PATHS$error_log else "error.log"
      fallback_file <- file.path(tempdir(), "itcsuite", "logs", "error.log")
      append_log_entry_safe(entry, log_file, fallback_file)
    }
    invisible(entry)
  }
}

# ==============================================================================
# 错误处理函数
# ==============================================================================

#' 获取翻译的错误消息
#' 
#' 根据错误代码和语言设置获取翻译的错误标题和描述
#' 
#' @param error_code 错误代码（来自 constants.R 的 ERROR_CODES）
#' @param lang_val 语言设置（"zh" 或 "en"）
#' @return 包含 title 和 desc 的列表
#' 
#' @examples
#' get_error_message("E001", "zh")  # 返回 list(title="数据无效", desc="...")
get_error_message <- function(error_code, lang_val = "zh") {
  # 构建翻译键
  title_key <- paste0("error_", error_code, "_title")
  desc_key <- paste0("error_", error_code, "_desc")
  
  # 尝试获取翻译
  title <- if(exists("tr")) {
    tryCatch({
      tr(title_key, lang_val)
    }, error = function(e) {
      # 如果翻译失败，返回错误代码
      error_code
    })
  } else {
    error_code
  }
  
  desc <- if(exists("tr")) {
    tryCatch({
      tr(desc_key, lang_val)
    }, error = function(e) {
      # 如果翻译失败，返回空字符串
      ""
    })
  } else {
    ""
  }
  
  return(list(title = title, desc = desc))
}

#' 统一的错误处理函数
#' 
#' 提供一致的错误处理机制，包括日志记录和用户通知
#' 
#' @param e 错误对象（来自 tryCatch）
#' @param context 错误上下文描述（如 "数据加载", "拟合计算"）
#' @param show_to_user 是否向用户显示通知（默认 TRUE）
#' @param log_to_file 是否记录到日志文件（默认 TRUE）
#' @param lang_val 语言设置（"zh" 或 "en"，用于翻译错误消息）
#' @param error_code 错误代码（可选，来自 constants.R 的 ERROR_CODES）
#' @return NULL
#' 
#' @examples
#' tryCatch({
#'   # 某些可能失败的操作
#'   result <- risky_calculation()
#' }, error = function(e) {
#'   handle_error(e, context = "计算模拟结果", lang_val = "zh")
#' })
handle_error <- function(e, 
                        context = "", 
                        show_to_user = TRUE, 
                        log_to_file = TRUE,
                        lang_val = "zh",
                        error_code = NULL) {
  error_msg <- format_itc_error(e, context = "", code = error_code)
  
  # 如果提供了错误代码，获取翻译的错误信息
  error_title <- NULL
  error_desc <- NULL
  if (!is.null(error_code)) {
    error_info <- get_error_message(error_code, lang_val)
    error_title <- error_info$title
    error_desc <- error_info$desc
    
    if (nchar(error_desc) > 0) {
      translated_msg <- paste0("[", error_code, "] ", error_title, ": ", error_desc)
      # 如果原始错误消息有额外信息，也添加上
      if (nchar(e$message) > 0) {
        translated_msg <- paste0(translated_msg, " | ", e$message)
      }
      error_msg <- translated_msg
    }
  }
  
  if (isTRUE(log_to_file)) {
    itc_log_error(error_msg, context = context, log_to_file = TRUE)
  } else {
    message("ERROR: ", error_msg)
  }
  
  # 向用户显示通知
  if (show_to_user) {
    tryCatch({
      # 构建用户友好的通知消息
      if (!is.null(error_title)) {
        # 使用翻译的错误标题
        notif_title <- error_title
        notif_msg <- if(nchar(error_desc) > 0) error_desc else error_msg
      } else {
        # 使用通用错误消息
        if(exists("tr")) {
          notif_title <- tr("error_occurred", lang_val)
        } else {
          notif_title <- if(lang_val == "zh") "发生错误" else "An error occurred"
        }
        notif_msg <- error_msg
      }
      
      # 如果有上下文，添加到消息中
      if (nchar(context) > 0) {
        notif_msg <- paste0(context, ": ", notif_msg)
      }
      
      # 显示通知
      duration <- if (exists("NOTIFICATION_DURATION")) {
        NOTIFICATION_DURATION$error
      } else {
        10
      }
      
      showNotification(
        paste0(notif_title, " - ", notif_msg),
        type = "error",
        duration = duration
      )
    }, error = function(notif_e) {
      # 如果通知显示失败，至少记录警告
      if(exists("tr")) {
        warning_msg <- tr("error_notification_failed", lang_val)
      } else {
        warning_msg <- "Failed to show notification"
      }
      warning(warning_msg, ": ", notif_e$message)
    })
  }
  
  return(NULL)
}

#' 安全执行函数
#' 
#' 包装一个表达式，捕获错误并使用统一的错误处理
#' 
#' @param expr 要执行的表达式
#' @param context 错误上下文描述
#' @param default 发生错误时返回的默认值（默认 NULL）
#' @param show_error 是否向用户显示错误（默认 TRUE）
#' @param lang_val 语言设置
#' @return 表达式的结果，或发生错误时返回 default
#' 
#' @examples
#' result <- safe_execute({
#'   read.csv("data.csv")
#' }, context = "读取CSV文件", default = NULL)
safe_execute <- function(expr, 
                        context = "", 
                        default = NULL,
                        show_error = TRUE,
                        lang_val = "zh") {
  tryCatch({
    eval(expr, envir = parent.frame())
  }, error = function(e) {
    handle_error(e, context = context, show_to_user = show_error, lang_val = lang_val)
    default
  })
}

#' 记录警告到日志
#' 
#' @param message 警告消息
#' @param context 上下文
#' @param log_to_file 是否记录到文件
log_warning <- function(message, context = "", log_to_file = TRUE) {
  itc_log_warn(message = message, context = context, log_to_file = log_to_file)
}

#' 记录信息到会话日志
#' 
#' @param message 信息消息
#' @param context 上下文
log_info <- function(message, context = "") {
  itc_log_info(message = message, context = context, log_to_file = TRUE)
}

# ==============================================================================
# 输入验证和安全函数
# ==============================================================================

#' 安全获取输入值
#' 
#' 处理可能为 NULL、NA 或空值的输入，返回默认值
#' 
#' @param x 输入值
#' @param default 默认值
#' @return 有效的输入值或默认值
#' 
#' @examples
#' value <- safe_input(input$slider_value, default = 1.0)
safe_input <- function(x, default) {
  if(is.null(x) || length(x) == 0 || any(is.na(x))) {
    return(default)
  }
  return(x)
}

#' 安全获取数值输入
#' 
#' 确保输入是有效的数值，并在指定范围内
#' 
#' @param x 输入值
#' @param default 默认值
#' @param min 最小值（可选）
#' @param max 最大值（可选）
#' @return 有效的数值
safe_numeric <- function(x, default, min = -Inf, max = Inf) {
  val <- safe_input(x, default)
  
  # 确保是数值
  val <- suppressWarnings(as.numeric(val))
  if(is.na(val)) {
    return(default)
  }
  
  # 检查范围
  if(val < min) {
    log_warning(sprintf("Value %f below minimum %f, using minimum", val, min))
    return(min)
  }
  if(val > max) {
    log_warning(sprintf("Value %f above maximum %f, using maximum", val, max))
    return(max)
  }
  
  return(val)
}

#' 验证数据框
#' 
#' 检查数据框是否有效且包含必需的列
#' 
#' @param df 数据框
#' @param required_cols 必需的列名向量
#' @param min_rows 最小行数（默认 1）
#' @return 逻辑值，TRUE 表示有效
validate_dataframe <- function(df, required_cols = NULL, min_rows = 1) {
  # 检查是否为数据框
  if(!is.data.frame(df)) {
    return(FALSE)
  }
  
  # 检查行数
  if(nrow(df) < min_rows) {
    return(FALSE)
  }
  
  # 检查必需的列
  if(!is.null(required_cols)) {
    if(!all(required_cols %in% colnames(df))) {
      missing <- required_cols[!required_cols %in% colnames(df)]
      log_warning(paste("Missing required columns:", paste(missing, collapse = ", ")))
      return(FALSE)
    }
  }
  
  return(TRUE)
}

# ==============================================================================
# 数值计算辅助函数
# ==============================================================================

#' 安全的对数计算
#' 
#' 防止对零或负数取对数
#' 
#' @param x 输入值
#' @param epsilon 极小值阈值（默认使用 EPSILON_LOG）
#' @return log(x)，其中 x 被限制为 >= epsilon
safe_log <- function(x, epsilon = NULL) {
  if(is.null(epsilon)) {
    epsilon <- if(exists("EPSILON_LOG")) EPSILON_LOG else 1e-15
  }
  return(log(pmax(x, epsilon)))
}

#' 安全的除法
#' 
#' 防止除零错误
#' 
#' @param numerator 分子
#' @param denominator 分母
#' @param epsilon 极小值阈值（默认使用 EPSILON）
#' @return numerator / denominator，分母被限制为 >= epsilon
safe_divide <- function(numerator, denominator, epsilon = NULL) {
  if(is.null(epsilon)) {
    epsilon <- if(exists("EPSILON")) EPSILON else 1e-20
  }
  return(numerator / pmax(abs(denominator), epsilon))
}

#' 检查数值是否接近零
#' 
#' @param x 输入值
#' @param tolerance 容差（默认使用 EPSILON）
#' @return 逻辑值
is_near_zero <- function(x, tolerance = NULL) {
  if(is.null(tolerance)) {
    tolerance <- if(exists("EPSILON")) EPSILON else 1e-20
  }
  return(abs(x) < tolerance)
}

# ==============================================================================
# 缓存辅助函数
# ==============================================================================

#' 创建基于键的缓存检查函数
#' 
#' @param cache_val 当前缓存的值（reactiveVal）
#' @param cache_key 当前缓存的键（reactiveVal）
#' @param new_key 新的键
#' @return 逻辑值，TRUE 表示缓存命中
cache_hit <- function(cache_val, cache_key, new_key) {
  if(is.null(cache_val) || is.null(cache_key)) {
    return(FALSE)
  }
  
  return(identical(cache_key, new_key))
}

# ==============================================================================
# 字符串和格式化函数
# ==============================================================================

#' 格式化数值为字符串
#' 
#' @param x 数值
#' @param digits 小数位数（默认 2）
#' @param scientific 是否使用科学计数法（默认 FALSE）
#' @return 格式化的字符串
format_number <- function(x, digits = 2, scientific = FALSE) {
  if(is.null(x) || is.na(x)) {
    return("N/A")
  }
  
  if(scientific || abs(x) > 1e6 || (abs(x) < 1e-3 && x != 0)) {
    return(formatC(x, format = "e", digits = digits))
  } else {
    return(formatC(x, format = "f", digits = digits))
  }
}

#' 截断长字符串
#' 
#' @param str 字符串
#' @param max_length 最大长度
#' @param suffix 截断后缀（默认 "..."）
#' @return 截断后的字符串
truncate_string <- function(str, max_length = 50, suffix = "...") {
  if(is.null(str) || nchar(str) <= max_length) {
    return(str)
  }
  
  return(paste0(substr(str, 1, max_length - nchar(suffix)), suffix))
}

# ==============================================================================
# 时间和性能测量
# ==============================================================================

#' 执行并测量时间
#' 
#' @param expr 要执行的表达式
#' @param label 标签（用于日志）
#' @param log_result 是否记录结果（默认 FALSE）
#' @return 列表，包含 result（结果）和 elapsed（耗时，秒）
time_it <- function(expr, label = "", log_result = FALSE) {
  start_time <- Sys.time()
  
  result <- eval(expr, envir = parent.frame())
  
  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  if(log_result) {
    msg <- if(nchar(label) > 0) {
      sprintf("%s completed in %.3f seconds", label, elapsed)
    } else {
      sprintf("Operation completed in %.3f seconds", elapsed)
    }
    log_info(msg, context = "Performance")
  }
  
  return(list(result = result, elapsed = elapsed))
}
