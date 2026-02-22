# ==============================================================================
# logging.R - Unified logging interface
# ==============================================================================

#' Resolve log file path by level.
#'
#' @param level Log level.
#' @return File path string.
resolve_runtime_logs_dir <- function() {
  from_env <- trimws(as.character(Sys.getenv("ITCSUITE_USER_DATA_DIR", unset = ""))[1])
  base_dir <- from_env
  if (!nzchar(base_dir)) {
    base_dir <- tryCatch(
      tools::R_user_dir("itcsuite", which = "data"),
      error = function(e) getwd()
    )
  }
  candidates <- unique(c(
    tryCatch(normalizePath(file.path(base_dir, "logs"), winslash = "/", mustWork = FALSE), error = function(e) file.path(base_dir, "logs")),
    file.path(tempdir(), "itcsuite", "logs"),
    file.path(getwd(), "logs")
  ))
  for (cand in candidates) {
    ok <- tryCatch({
      dir.create(cand, recursive = TRUE, showWarnings = FALSE)
      dir.exists(cand)
    }, error = function(e) FALSE)
    if (isTRUE(ok)) return(cand)
  }
  file.path(getwd(), "logs")
}

resolve_log_file <- function(level = "INFO") {
  level_norm <- toupper(as.character(level)[1])
  has_paths <- exists("FILE_PATHS", envir = .GlobalEnv)

  if (!has_paths) {
    logs_dir <- resolve_runtime_logs_dir()
    return(if (level_norm %in% c("WARN", "ERROR")) file.path(logs_dir, "error.log") else file.path(logs_dir, "session.log"))
  }

  if (level_norm %in% c("WARN", "ERROR")) {
    return(FILE_PATHS$error_log)
  }

  FILE_PATHS$session_log
}

#' Unified logger.
#'
#' @param level INFO/WARN/ERROR/DEBUG.
#' @param message Log message.
#' @param context Optional context label.
#' @param log_to_file Whether to append log file.
#' @param file_path Optional log file override.
#' @param echo_console Whether to emit console output.
#' @return Invisible formatted log entry.
itc_log <- function(level = "INFO",
                    message,
                    context = "",
                    log_to_file = TRUE,
                    file_path = NULL,
                    echo_console = TRUE) {
  level_norm <- toupper(as.character(level)[1])
  if (!level_norm %in% c("DEBUG", "INFO", "WARN", "ERROR")) {
    level_norm <- "INFO"
  }

  msg <- as.character(message)[1]
  if (is.na(msg) || !nzchar(msg)) {
    msg <- "<empty message>"
  }

  context_txt <- if (nzchar(context)) paste0("[", context, "] ") else ""
  entry <- paste0(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " | ", level_norm, " | ", context_txt, msg
  )

  if (isTRUE(echo_console)) {
    if (level_norm %in% c("WARN", "ERROR")) {
      warning(entry, call. = FALSE)
    } else {
      message(entry)
    }
  }

  if (isTRUE(log_to_file)) {
    target_file <- if (!is.null(file_path) && nzchar(file_path)) {
      file_path
    } else {
      resolve_log_file(level_norm)
    }

    tryCatch({
      ok_dir <- tryCatch({
        dir.create(dirname(target_file), recursive = TRUE, showWarnings = FALSE)
        dir.exists(dirname(target_file))
      }, error = function(e) FALSE)
      if (!isTRUE(ok_dir)) {
        fallback_file <- file.path(tempdir(), "itcsuite", "logs", basename(target_file))
        dir.create(dirname(fallback_file), recursive = TRUE, showWarnings = FALSE)
        target_file <- fallback_file
      }
      cat(paste0(entry, "\n"), file = target_file, append = TRUE)
    }, error = function(e) {
      warning("Failed to append log entry: ", e$message, call. = FALSE)
    })
  }

  invisible(entry)
}

itc_log_info <- function(message, context = "", log_to_file = TRUE) {
  itc_log(level = "INFO", message = message, context = context, log_to_file = log_to_file)
}

itc_log_warn <- function(message, context = "", log_to_file = TRUE) {
  itc_log(level = "WARN", message = message, context = context, log_to_file = log_to_file)
}

itc_log_error <- function(message, context = "", log_to_file = TRUE) {
  itc_log(level = "ERROR", message = message, context = context, log_to_file = log_to_file)
}
