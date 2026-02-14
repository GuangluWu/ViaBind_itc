`%||%` <- function(x, y) if (is.null(x)) y else x

# [COMMENT_STD][MODULE_HEADER]
# 模块职责：提供首页最近导入记录的跨会话持久化读写（RDS）与状态清洗。
# 依赖：base R 文件 IO；可选依赖 home_trim_recent_records() 用于裁剪排序。
# 对外接口：home_recent_store_path()、home_recent_store_load()、home_recent_store_save()。
# 副作用：读写用户数据目录下 state/home_recent_imports_v1.rds。
# 变更历史：2026-02-14 - 新增跨会话最近导入持久化。

home_recent_store_schema <- function() {
  "itcsuite.home_recent.v1"
}

home_recent_store_max_records_default <- function() {
  200L
}

home_recent_store_path <- function() {
  base_dir <- trimws(as.character(Sys.getenv("ITCSUITE_USER_DATA_DIR", unset = ""))[1])
  if (!nzchar(base_dir)) {
    base_dir <- tools::R_user_dir("itcsuite", which = "data")
  }
  file.path(base_dir, "state", "home_recent_imports_v1.rds")
}

home_recent_store_now_utc <- function() {
  paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%S", tz = "UTC"), "Z")
}

home_recent_store_default_state <- function(max_records = home_recent_store_max_records_default()) {
  max_n <- suppressWarnings(as.integer(max_records)[1])
  if (!is.finite(max_n) || max_n < 1L) max_n <- home_recent_store_max_records_default()
  list(
    schema_version = home_recent_store_schema(),
    next_seq = 0L,
    import_records = list(),
    max_records = as.integer(max_n),
    updated_at = home_recent_store_now_utc()
  )
}

home_recent_store_normalize_state <- function(
  state,
  max_records = home_recent_store_max_records_default()
) {
  default_state <- home_recent_store_default_state(max_records = max_records)
  if (is.null(state) || !is.list(state)) return(default_state)

  schema <- as.character(state$schema_version %||% "")[1]
  if (!identical(schema, home_recent_store_schema())) return(default_state)

  next_seq <- suppressWarnings(as.integer(state$next_seq)[1])
  if (!is.finite(next_seq) || next_seq < 0L) next_seq <- 0L

  records <- state$import_records
  if (!is.list(records)) records <- list()

  max_n <- suppressWarnings(as.integer(max_records)[1])
  if (!is.finite(max_n) || max_n < 1L) {
    max_n <- home_recent_store_max_records_default()
  }

  if (exists("home_trim_recent_records", mode = "function")) {
    trimmed <- tryCatch(
      home_trim_recent_records(records, max_records = max_n),
      error = function(e) NULL
    )
    if (is.list(trimmed) && is.list(trimmed$records)) {
      records <- trimmed$records
    }
  } else if (length(records) > max_n) {
    records <- records[seq_len(max_n)]
  }

  updated_at <- as.character(state$updated_at %||% "")[1]
  if (!nzchar(trimws(updated_at))) updated_at <- home_recent_store_now_utc()

  record_max_seq <- suppressWarnings(max(vapply(records, function(rec) {
    if (!is.list(rec)) return(NA_integer_)
    rec_id <- as.character(rec$id %||% "")[1]
    if (!nzchar(rec_id)) return(NA_integer_)
    as.integer(sub("^home_rec_", "", rec_id))
  }, integer(1)), na.rm = TRUE))
  if (!is.finite(record_max_seq)) record_max_seq <- 0L
  if (!is.finite(next_seq) || next_seq < record_max_seq) {
    next_seq <- as.integer(record_max_seq)
  }

  list(
    schema_version = home_recent_store_schema(),
    next_seq = as.integer(next_seq),
    import_records = records,
    max_records = as.integer(max_n),
    updated_at = updated_at
  )
}

home_recent_store_load <- function(
  max_records = home_recent_store_max_records_default(),
  warn_fn = warning
) {
  path <- home_recent_store_path()
  default_state <- home_recent_store_default_state(max_records = max_records)
  if (!file.exists(path)) return(default_state)

  loaded <- tryCatch(
    readRDS(path),
    error = function(e) {
      if (is.function(warn_fn)) {
        warn_fn(sprintf("home recent store load failed at %s: %s", path, conditionMessage(e)))
      }
      NULL
    }
  )
  if (is.null(loaded)) return(default_state)
  home_recent_store_normalize_state(loaded, max_records = max_records)
}

home_recent_store_save <- function(
  state,
  max_records = home_recent_store_max_records_default(),
  warn_fn = warning
) {
  path <- home_recent_store_path()
  dir_path <- dirname(path)
  ok_dir <- tryCatch({
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    TRUE
  }, error = function(e) {
    if (is.function(warn_fn)) {
      warn_fn(sprintf("home recent store mkdir failed for %s: %s", dir_path, conditionMessage(e)))
    }
    FALSE
  })
  if (!isTRUE(ok_dir)) return(invisible(FALSE))

  normalized <- home_recent_store_normalize_state(state, max_records = max_records)
  normalized$updated_at <- home_recent_store_now_utc()
  normalized$max_records <- as.integer(suppressWarnings(as.integer(max_records)[1]))

  tmp_path <- paste0(path, ".tmp-", Sys.getpid(), "-", as.integer(as.numeric(Sys.time())))
  ok_save <- tryCatch({
    saveRDS(normalized, file = tmp_path, version = 2)
    TRUE
  }, error = function(e) {
    if (is.function(warn_fn)) {
      warn_fn(sprintf("home recent store write failed at %s: %s", tmp_path, conditionMessage(e)))
    }
    FALSE
  })
  if (!isTRUE(ok_save)) {
    try(unlink(tmp_path), silent = TRUE)
    return(invisible(FALSE))
  }

  renamed <- tryCatch(file.rename(tmp_path, path), error = function(e) FALSE)
  if (!isTRUE(renamed)) {
    copied <- tryCatch(file.copy(tmp_path, path, overwrite = TRUE), error = function(e) FALSE)
    try(unlink(tmp_path), silent = TRUE)
    if (!isTRUE(copied)) {
      if (is.function(warn_fn)) {
        warn_fn(sprintf("home recent store replace failed for %s.", path))
      }
      return(invisible(FALSE))
    }
  }

  invisible(TRUE)
}
