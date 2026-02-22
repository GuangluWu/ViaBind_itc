`%||%` <- function(x, y) if (is.null(x)) y else x

# [COMMENT_STD][MODULE_HEADER]
# 模块职责：提供桌面原生文件选择的输入规范化、请求队列管理与结果清洗工具。
# 依赖：base R。
# 对外接口：home_desktop_* 系列函数。
# 副作用：仅在调用 pending register/take 时读写传入环境对象。
# 变更历史：2026-02-14 - 新增桌面原生选文件 helper。

home_desktop_scalar_chr <- function(x, default = "") {
  out <- as.character(x %||% "")[1]
  out <- trimws(out)
  if (nzchar(out)) out else default
}

home_desktop_capability_open_file <- function(capability) {
  if (is.null(capability)) return(FALSE)
  if (is.list(capability)) return(isTRUE(capability$open_file))
  FALSE
}

home_desktop_sanitize_filters <- function(
  filters,
  fallback_name = "Data Files",
  fallback_exts = c("xlsx")
) {
  normalize_ext <- function(values) {
    if (is.null(values)) return(character(0))
    ext <- as.character(values)
    ext <- tolower(trimws(ext))
    ext <- sub("^\\.+", "", ext)
    ext <- ext[nzchar(ext)]
    ext <- ext[grepl("^[a-z0-9]+$", ext)]
    unique(ext)
  }

  fallback <- list(
    list(
      name = home_desktop_scalar_chr(fallback_name, default = "Data Files"),
      extensions = normalize_ext(fallback_exts)
    )
  )
  if (length(fallback[[1]]$extensions) < 1) fallback[[1]]$extensions <- "xlsx"

  if (!is.list(filters) || length(filters) < 1) return(fallback)

  out <- list()
  for (entry in filters) {
    if (!is.list(entry)) next
    nm <- home_desktop_scalar_chr(entry$name, default = "Files")
    exts <- normalize_ext(entry$extensions)
    if (length(exts) < 1) next
    out[[length(out) + 1L]] <- list(name = nm, extensions = exts)
    if (length(out) >= 4L) break
  }
  if (length(out) < 1) fallback else out
}

home_desktop_next_request_id <- function(seq_num, purpose = "open_file") {
  seq_safe <- suppressWarnings(as.integer(seq_num)[1])
  if (!is.finite(seq_safe) || seq_safe < 0L) seq_safe <- 0L
  seq_safe <- seq_safe + 1L
  prefix <- gsub("[^a-z0-9_]+", "_", tolower(home_desktop_scalar_chr(purpose, default = "open_file")))
  prefix <- trimws(prefix)
  if (!nzchar(prefix)) prefix <- "open_file"
  list(
    next_seq = as.integer(seq_safe),
    request_id = sprintf("desktop_%s_%06d", prefix, seq_safe)
  )
}

home_desktop_pending_register <- function(pending_env, request_id, callbacks = list()) {
  if (!is.environment(pending_env)) return(FALSE)
  rid <- home_desktop_scalar_chr(request_id, default = "")
  if (!nzchar(rid)) return(FALSE)
  entry <- if (is.list(callbacks)) callbacks else list()
  assign(rid, entry, envir = pending_env)
  TRUE
}

home_desktop_pending_take <- function(pending_env, request_id) {
  if (!is.environment(pending_env)) return(NULL)
  rid <- home_desktop_scalar_chr(request_id, default = "")
  if (!nzchar(rid)) return(NULL)
  if (!exists(rid, envir = pending_env, inherits = FALSE)) return(NULL)
  out <- get(rid, envir = pending_env, inherits = FALSE)
  rm(list = rid, envir = pending_env)
  out
}

home_desktop_normalize_open_file_result <- function(result) {
  src <- if (is.list(result)) result else list()
  list(
    request_id = home_desktop_scalar_chr(src$request_id, default = ""),
    purpose = home_desktop_scalar_chr(src$purpose, default = ""),
    canceled = isTRUE(src$canceled),
    file_path = home_desktop_scalar_chr(src$file_path, default = ""),
    file_name = home_desktop_scalar_chr(src$file_name, default = ""),
    error = home_desktop_scalar_chr(src$error, default = "")
  )
}
