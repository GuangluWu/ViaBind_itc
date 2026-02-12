# ==============================================================================
# errors.R - Unified error interface
# ==============================================================================
# [COMMENT_STD][MODULE_HEADER]
# 模块职责：统一错误对象结构、错误格式化与受控执行包装。
# 依赖：logging.R（默认 logger 为 itc_log_error）。
# 对外接口：itc_error()、format_itc_error()、itc_try()。
# 副作用：itc_try() 在异常路径会写日志；不直接弹 UI 提示。
# 变更历史：2026-02-12 - 增加 Phase 4 注释规范样板。

#' Create a structured ITC error object.
#'
#' @param message Error message.
#' @param code Optional error code.
#' @param context Optional context label.
#' @param cause Optional original condition.
#' @return Condition object with class `itc_error`.
itc_error <- function(message, code = NULL, context = "", cause = NULL) {
  structure(
    list(
      message = as.character(message)[1],
      code = if (is.null(code)) NULL else as.character(code)[1],
      context = as.character(context)[1],
      cause = cause
    ),
    class = c("itc_error", "error", "condition")
  )
}

#' Format any condition into a normalized error message.
#'
#' @param e Condition/error object.
#' @param context Optional context label.
#' @param code Optional error code.
#' @return Formatted message string.
format_itc_error <- function(e, context = "", code = NULL) {
  base_msg <- tryCatch(as.character(e$message), error = function(...) "Unknown error")
  if (length(base_msg) < 1 || is.na(base_msg[1]) || !nzchar(base_msg[1])) {
    base_msg <- "Unknown error"
  }

  out <- base_msg[1]
  if (!is.null(code) && nzchar(as.character(code)[1])) {
    out <- paste0("[", as.character(code)[1], "] ", out)
  }
  if (nzchar(context)) {
    out <- paste0("[", context, "] ", out)
  }
  out
}

#' Execute expression with standardized error handling.
#'
#' @param expr Expression to evaluate.
#' @param context Optional context label.
#' @param default Default value on error.
#' @param logger Logger function accepting `(message, context)`.
#' @param code Optional error code.
#' @return Expression result or `default`.
itc_try <- function(expr,
                    context = "",
                    default = NULL,
                    logger = itc_log_error,
                    code = NULL) {
  # [COMMENT_STD][ERROR_SEMANTICS]
  # 错误码/类别：支持调用侧传入 code；未传入时仅保留原始异常类别。
  # 触发条件：expr 求值过程中抛出 error condition。
  # 用户可见性：默认不可见，仅返回 default；上层可按需二次转译到 UI。
  # 日志级别：统一按 runtime error 写入 logger（默认 itc_log_error）。
  # 恢复动作：捕获后不中断主流程，返回 default 并保留上下文信息。
  tryCatch(
    eval(expr, envir = parent.frame()),
    error = function(e) {
      msg <- format_itc_error(e, context = context, code = code)
      if (is.function(logger)) {
        logger(msg, context = "runtime")
      }
      default
    }
  )
}
