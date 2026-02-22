`%||%` <- function(x, y) if (is.null(x)) y else x

# [COMMENT_STD][MODULE_HEADER]
# Module role: structured telemetry helpers for desktop-hosted ITCSuite runtime.
# Dependencies: base R + jsonlite (optional fallback).
# Public API: telemetry_create_session(), telemetry_logs_dir(), telemetry_redact_value().
# Side effects: writes JSONL logs under user data logs directory.
# Change log: 2026-02-22 - initial telemetry foundation for desktop diagnostics export.

telemetry_scalar_chr <- function(x, default = "") {
  out <- as.character(x %||% "")[1]
  out <- trimws(out)
  if (nzchar(out)) out else default
}

telemetry_now_utc <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

telemetry_runtime_label <- function() {
  if (identical(telemetry_scalar_chr(Sys.getenv("ITCSUITE_DESKTOP", unset = ""), default = ""), "1")) {
    return("desktop")
  }
  "web"
}

telemetry_user_data_dir <- function() {
  from_env <- telemetry_scalar_chr(Sys.getenv("ITCSUITE_USER_DATA_DIR", unset = ""), default = "")
  if (nzchar(from_env)) {
    return(tryCatch(normalizePath(from_env, winslash = "/", mustWork = FALSE), error = function(e) from_env))
  }
  tryCatch(
    normalizePath(tools::R_user_dir("itcsuite", which = "data"), winslash = "/", mustWork = FALSE),
    error = function(e) tools::R_user_dir("itcsuite", which = "data")
  )
}

telemetry_logs_dir <- function() {
  candidates <- unique(c(
    file.path(telemetry_user_data_dir(), "logs"),
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

telemetry_events_file <- function() {
  file.path(telemetry_logs_dir(), "app-events.log")
}

telemetry_normalize_path <- function(path) {
  p <- telemetry_scalar_chr(path, default = "")
  if (!nzchar(p)) return("")
  tryCatch(normalizePath(p, winslash = "/", mustWork = FALSE), error = function(e) p)
}

telemetry_redact_path <- function(path) {
  p <- telemetry_normalize_path(path)
  if (!nzchar(p)) return("")

  home <- telemetry_normalize_path(path.expand("~"))
  if (nzchar(home) && startsWith(p, home)) {
    suffix <- substr(p, nchar(home) + 1L, nchar(p))
    if (startsWith(suffix, "/")) suffix <- substr(suffix, 2L, nchar(suffix))
    if (!nzchar(suffix)) return("~")
    return(file.path("~", suffix))
  }

  p
}

telemetry_redact_string <- function(x) {
  txt <- telemetry_scalar_chr(x, default = "")
  if (!nzchar(txt)) return(txt)

  home <- telemetry_normalize_path(path.expand("~"))
  if (nzchar(home)) {
    txt <- gsub(home, "~", txt, fixed = TRUE)
  }

  user_data <- telemetry_normalize_path(telemetry_user_data_dir())
  if (nzchar(user_data)) {
    txt <- gsub(user_data, "<itcsuite_user_data>", txt, fixed = TRUE)
  }

  txt
}

telemetry_redact_value <- function(x, depth = 0L, max_depth = 6L) {
  if (!is.finite(depth) || depth < 0L) depth <- 0L
  if (!is.finite(max_depth) || max_depth < 1L) max_depth <- 6L
  if (depth >= max_depth) return("<depth-limited>")

  if (is.null(x)) return(NULL)
  if (is.list(x)) {
    out <- lapply(x, telemetry_redact_value, depth = depth + 1L, max_depth = max_depth)
    nm <- names(x)
    if (!is.null(nm)) names(out) <- nm
    return(out)
  }
  if (is.data.frame(x)) {
    return(list(
      type = "data.frame",
      rows = nrow(x),
      cols = ncol(x),
      names = colnames(x)
    ))
  }
  if (is.matrix(x)) {
    return(list(
      type = "matrix",
      rows = nrow(x),
      cols = ncol(x)
    ))
  }
  if (is.character(x)) {
    vals <- as.character(x)
    if (length(vals) > 12L) vals <- vals[seq_len(12L)]
    vals <- vapply(vals, function(v) {
      redacted <- telemetry_redact_string(v)
      if (grepl("(^|[[:space:]])(/|[A-Za-z]:\\\\)", redacted)) {
        telemetry_redact_path(redacted)
      } else {
        redacted
      }
    }, character(1))
    return(unname(vals))
  }
  if (is.numeric(x) || is.integer(x) || is.logical(x)) {
    vals <- x
    if (length(vals) > 32L) vals <- vals[seq_len(32L)]
    return(unname(vals))
  }

  telemetry_redact_string(capture.output(str(x, give.attr = FALSE, vec.len = 4L))[1] %||% "<unknown>")
}

telemetry_error_payload <- function(err = NULL) {
  if (is.null(err)) return(NULL)
  msg <- telemetry_scalar_chr(tryCatch(conditionMessage(err), error = function(e) as.character(err)[1]), default = "Unknown error")
  classes <- tryCatch(class(err), error = function(e) character(0))
  if (!is.character(classes)) classes <- character(0)
  list(
    message = telemetry_redact_string(msg),
    class = classes
  )
}

telemetry_file_size <- function(path) {
  if (!file.exists(path)) return(0)
  out <- tryCatch(file.info(path)$size[[1]], error = function(e) 0)
  if (!is.finite(out)) 0 else as.numeric(out)
}

telemetry_rotate_file <- function(path) {
  if (!file.exists(path)) return(invisible(FALSE))
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  rotated <- paste0(path, ".", ts)
  ok <- tryCatch(file.rename(path, rotated), error = function(e) FALSE)
  if (!isTRUE(ok)) {
    ok_copy <- tryCatch(file.copy(path, rotated, overwrite = TRUE), error = function(e) FALSE)
    if (isTRUE(ok_copy)) try(unlink(path), silent = TRUE)
    ok <- ok_copy
  }
  invisible(isTRUE(ok))
}

telemetry_cleanup_logs <- function(logs_dir, retention_days = 30L) {
  keep_days <- suppressWarnings(as.integer(retention_days)[1])
  if (!is.finite(keep_days) || keep_days < 1L) keep_days <- 30L
  if (!dir.exists(logs_dir)) return(invisible(FALSE))

  files <- list.files(logs_dir, full.names = TRUE, recursive = FALSE, all.files = FALSE)
  if (length(files) < 1L) return(invisible(TRUE))

  cutoff <- Sys.time() - as.difftime(keep_days, units = "days")
  for (f in files) {
    info <- tryCatch(file.info(f), error = function(e) NULL)
    if (is.null(info) || nrow(info) < 1L) next
    mt <- info$mtime[[1]]
    if (!is.finite(as.numeric(mt))) next
    if (mt < cutoff) {
      try(unlink(f, force = TRUE), silent = TRUE)
    }
  }
  invisible(TRUE)
}

telemetry_serialize_line <- function(entry) {
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    return(jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null", digits = NA))
  }
  paste(capture.output(dput(entry)), collapse = "")
}

telemetry_create_session <- function(
  app_version = "0.0.0-dev",
  trace_id = NULL,
  session_id = NULL,
  runtime = telemetry_runtime_label(),
  retention_days = 30L,
  rotate_bytes = 2 * 1024 * 1024
) {
  logs_dir <- telemetry_logs_dir()
  dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

  events_file <- telemetry_events_file()
  telemetry_cleanup_logs(logs_dir, retention_days = retention_days)

  trace_val <- telemetry_scalar_chr(trace_id, default = paste0("trace_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", Sys.getpid()))
  session_val <- telemetry_scalar_chr(session_id, default = paste0("session_", as.integer(as.numeric(Sys.time())), "_", sample.int(999999L, 1L)))
  app_ver <- telemetry_scalar_chr(app_version, default = "0.0.0-dev")
  runtime_val <- telemetry_scalar_chr(runtime, default = "web")
  rotate_limit <- suppressWarnings(as.numeric(rotate_bytes)[1])
  if (!is.finite(rotate_limit)) rotate_limit <- 2 * 1024 * 1024
  if (rotate_limit < 4096) rotate_limit <- 4096

  ops <- new.env(parent = emptyenv())

  write_entry <- function(entry) {
    if (telemetry_file_size(events_file) >= rotate_limit) {
      telemetry_rotate_file(events_file)
    }
    line <- telemetry_serialize_line(entry)
    cat(paste0(line, "\n"), file = events_file, append = TRUE)
    invisible(entry)
  }

  log_event <- function(
    event,
    level = "INFO",
    module = "host",
    payload = list(),
    err = NULL,
    op_id = NULL,
    lang = "en"
  ) {
    evt <- telemetry_scalar_chr(event, default = "unknown")
    lvl <- toupper(telemetry_scalar_chr(level, default = "INFO"))
    if (!lvl %in% c("DEBUG", "INFO", "WARN", "ERROR")) lvl <- "INFO"
    mod <- telemetry_scalar_chr(module, default = "host")
    op <- telemetry_scalar_chr(op_id, default = "")
    lang_norm <- tolower(telemetry_scalar_chr(lang, default = "en"))
    if (!lang_norm %in% c("en", "zh")) lang_norm <- "en"

    entry <- list(
      ts = telemetry_now_utc(),
      level = lvl,
      event = evt,
      module = mod,
      session_id = session_val,
      trace_id = trace_val,
      op_id = if (nzchar(op)) op else NULL,
      runtime = runtime_val,
      app_version = app_ver,
      lang = lang_norm,
      payload = telemetry_redact_value(payload),
      error = telemetry_error_payload(err)
    )
    write_entry(entry)
  }

  start_op <- function(event, module = "host", payload = list(), lang = "en") {
    evt <- telemetry_scalar_chr(event, default = "operation")
    op <- paste0("op_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample.int(999999L, 1L))
    assign(op, list(
      started_at = Sys.time(),
      event = evt,
      module = telemetry_scalar_chr(module, default = "host"),
      lang = telemetry_scalar_chr(lang, default = "en")
    ), envir = ops)

    log_event(
      event = evt,
      level = "INFO",
      module = module,
      payload = c(list(stage = "start"), payload),
      op_id = op,
      lang = lang
    )
    op
  }

  finish_op <- function(op_id, outcome = "ok", payload = list(), err = NULL, level = NULL, lang = NULL) {
    op <- telemetry_scalar_chr(op_id, default = "")
    if (!nzchar(op)) return(invisible(NULL))

    meta <- if (exists(op, envir = ops, inherits = FALSE)) get(op, envir = ops, inherits = FALSE) else NULL
    if (exists(op, envir = ops, inherits = FALSE)) rm(list = op, envir = ops)
    event_name <- telemetry_scalar_chr(meta$event %||% "operation", default = "operation")
    module_name <- telemetry_scalar_chr(meta$module %||% "host", default = "host")
    lang_name <- telemetry_scalar_chr(lang %||% meta$lang %||% "en", default = "en")
    started_at <- meta$started_at %||% Sys.time()
    elapsed_ms <- as.numeric(difftime(Sys.time(), started_at, units = "secs")) * 1000
    if (!is.finite(elapsed_ms) || elapsed_ms < 0) elapsed_ms <- NA_real_

    out <- telemetry_scalar_chr(outcome, default = "ok")
    lvl <- telemetry_scalar_chr(level, default = if (identical(out, "ok")) "INFO" else "ERROR")
    log_event(
      event = event_name,
      level = lvl,
      module = module_name,
      payload = c(
        list(stage = "finish", outcome = out, elapsed_ms = elapsed_ms),
        payload
      ),
      err = err,
      op_id = op,
      lang = lang_name
    )
  }

  list(
    session_id = session_val,
    trace_id = trace_val,
    runtime = runtime_val,
    app_version = app_ver,
    logs_dir = logs_dir,
    events_file = events_file,
    log_event = log_event,
    start_op = start_op,
    finish_op = finish_op
  )
}
