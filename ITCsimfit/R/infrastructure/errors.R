# ==============================================================================
# errors.R - Unified error interface
# ==============================================================================

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
