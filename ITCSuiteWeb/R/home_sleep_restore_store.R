`%||%` <- function(x, y) if (is.null(x)) y else x

# [COMMENT_STD][MODULE_HEADER]
# 模块职责：提供待机恢复快照的跨会话持久化读写（RDS）与状态清洗。
# 依赖：base R 文件 IO。
# 对外接口：home_sleep_restore_store_* 系列函数。
# 副作用：读写用户数据目录下 state/sleep_restore_v1.rds。
# 变更历史：2026-02-23 - 新增 Step1/Step2/Step3 待机恢复持久化。

home_sleep_restore_store_schema <- function() {
  "itcsuite.sleep_restore.v1"
}

home_sleep_restore_store_path <- function() {
  base_dir <- trimws(as.character(Sys.getenv("ITCSUITE_USER_DATA_DIR", unset = ""))[1])
  if (!nzchar(base_dir)) {
    base_dir <- tools::R_user_dir("itcsuite", which = "data")
  }
  file.path(base_dir, "state", "sleep_restore_v1.rds")
}

home_sleep_restore_store_now_utc <- function() {
  paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%S", tz = "UTC"), "Z")
}

home_sleep_restore_scalar_chr <- function(x, default = "") {
  out <- as.character(x %||% "")[1]
  out <- trimws(out)
  if (nzchar(out)) out else default
}

home_sleep_restore_scalar_lgl <- function(x, default = FALSE) {
  out <- suppressWarnings(as.logical(x)[1])
  if (isTRUE(is.na(out))) return(isTRUE(default))
  isTRUE(out)
}

home_sleep_restore_normalize_lang <- function(value, default = "en") {
  lang <- tolower(home_sleep_restore_scalar_chr(value, default = default))
  if (identical(lang, "zh")) "zh" else "en"
}

home_sleep_restore_normalize_scalar_value <- function(value) {
  if (is.null(value)) return(NULL)
  if (is.logical(value)) return(home_sleep_restore_scalar_lgl(value, default = FALSE))

  num <- suppressWarnings(as.numeric(value)[1])
  if (is.finite(num)) return(num)

  txt <- home_sleep_restore_scalar_chr(value, default = "")
  if (nzchar(txt)) return(txt)
  NULL
}

home_sleep_restore_normalize_scalar_map <- function(values) {
  if (!is.list(values)) return(list())
  out <- list()
  keys <- names(values)
  if (is.null(keys)) keys <- character(0)

  for (nm in keys) {
    key <- home_sleep_restore_scalar_chr(nm, default = "")
    if (!nzchar(key)) next
    value <- home_sleep_restore_normalize_scalar_value(values[[nm]])
    if (is.null(value)) next
    out[[key]] <- value
  }

  out
}

home_sleep_restore_normalize_step3_sheets <- function(sheets) {
  if (!is.list(sheets)) return(list())
  out <- list()
  keys <- names(sheets)
  if (is.null(keys)) keys <- character(0)

  for (nm in keys) {
    key <- home_sleep_restore_scalar_chr(nm, default = "")
    if (!nzchar(key)) next
    value <- sheets[[nm]]
    if (!is.data.frame(value)) next
    out[[key]] <- as.data.frame(value, stringsAsFactors = FALSE)
  }

  out
}

home_sleep_restore_normalize_atomic_vector <- function(value) {
  if (is.null(value)) return(NULL)
  if (is.factor(value)) value <- as.character(value)
  if (!is.atomic(value)) return(NULL)

  if (is.logical(value)) {
    out <- suppressWarnings(as.logical(value))
    out <- out[!is.na(out)]
    if (length(out) < 1L) return(NULL)
    return(as.logical(out))
  }

  if (is.numeric(value) || is.integer(value)) {
    out <- suppressWarnings(as.numeric(value))
    out <- out[is.finite(out)]
    if (length(out) < 1L) return(NULL)
    return(out)
  }

  out <- trimws(as.character(value))
  out <- out[nzchar(out)]
  if (length(out) < 1L) return(NULL)
  out
}

home_sleep_restore_normalize_vector_map <- function(values) {
  if (!is.list(values)) return(list())
  out <- list()
  keys <- names(values)
  if (is.null(keys)) keys <- character(0)

  for (nm in keys) {
    key <- home_sleep_restore_scalar_chr(nm, default = "")
    if (!nzchar(key)) next
    value <- values[[nm]]
    if (is.null(value)) next

    scalar_value <- home_sleep_restore_normalize_scalar_value(value)
    if (length(value) <= 1L && !is.null(scalar_value)) {
      out[[key]] <- scalar_value
      next
    }

    vec_value <- home_sleep_restore_normalize_atomic_vector(value)
    if (!is.null(vec_value)) {
      out[[key]] <- vec_value
    }
  }

  out
}

home_sleep_restore_normalize_table_like <- function(value) {
  if (is.null(value)) return(NULL)
  if (is.data.frame(value)) return(as.data.frame(value, stringsAsFactors = FALSE))
  if (is.matrix(value)) return(as.data.frame(value, stringsAsFactors = FALSE))

  if (is.list(value) && length(value) > 0L) {
    out <- tryCatch(as.data.frame(value, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.data.frame(out)) return(out)
  }

  vec <- home_sleep_restore_normalize_atomic_vector(value)
  if (!is.null(vec) && length(vec) > 0L) {
    return(data.frame(value = vec, stringsAsFactors = FALSE))
  }

  NULL
}

home_sleep_restore_normalize_table_list <- function(values) {
  if (!is.list(values)) return(list())
  out <- list()
  keys <- names(values)
  if (is.null(keys)) keys <- character(0)
  if (length(keys) != length(values)) keys <- rep("", length(values))

  for (i in seq_along(values)) {
    nm <- home_sleep_restore_scalar_chr(keys[[i]], default = "")
    if (!nzchar(nm)) nm <- sprintf("sheet_%03d", i)
    table_value <- home_sleep_restore_normalize_table_like(values[[i]])
    if (is.null(table_value)) next
    out[[nm]] <- table_value
  }

  out
}

home_sleep_restore_normalize_fit_bounds <- function(bounds) {
  if (!is.list(bounds)) return(list())
  out <- list()
  keys <- names(bounds)
  if (is.null(keys)) keys <- character(0)

  for (nm in keys) {
    key <- home_sleep_restore_scalar_chr(nm, default = "")
    if (!nzchar(key)) next
    pair <- bounds[[nm]]

    lower <- NA_real_
    upper <- NA_real_
    if (is.list(pair)) {
      lower <- suppressWarnings(as.numeric(pair$lower)[1])
      upper <- suppressWarnings(as.numeric(pair$upper)[1])
    } else {
      pair_num <- suppressWarnings(as.numeric(pair))
      if (length(pair_num) >= 1L) lower <- pair_num[1]
      if (length(pair_num) >= 2L) upper <- pair_num[2]
    }
    if (!is.finite(lower) || !is.finite(upper)) next
    if (lower > upper) {
      tmp <- lower
      lower <- upper
      upper <- tmp
    }
    out[[key]] <- list(lower = lower, upper = upper)
  }

  out
}

home_sleep_restore_normalize_step2_params <- function(params) {
  if (!is.list(params)) return(list())
  out <- home_sleep_restore_normalize_vector_map(params)

  path_mode <- home_sleep_restore_scalar_chr(out$path_view_mode, default = "")
  if (!path_mode %in% c("", "table", "graph")) path_mode <- ""
  if (nzchar(path_mode)) out$path_view_mode <- path_mode else out$path_view_mode <- NULL

  for (nm in c("active_paths", "fit_params")) {
    vec <- home_sleep_restore_normalize_atomic_vector(out[[nm]])
    vec_chr <- if (is.null(vec)) character(0) else trimws(as.character(vec))
    vec_chr <- unique(vec_chr[nzchar(vec_chr)])
    if (length(vec_chr) > 0L) out[[nm]] <- vec_chr else out[[nm]] <- NULL
  }

  fit_range <- home_sleep_restore_normalize_atomic_vector(out$fit_data_range)
  fit_range_num <- suppressWarnings(as.numeric(fit_range))
  fit_range_num <- fit_range_num[is.finite(fit_range_num)]
  if (length(fit_range_num) >= 2L) {
    out$fit_data_range <- fit_range_num[1:2]
  } else {
    out$fit_data_range <- NULL
  }

  for (nm in c("enable_error_analysis", "use_weighted_fitting", "use_robust_fitting")) {
    if (is.null(out[[nm]])) next
    out[[nm]] <- home_sleep_restore_scalar_lgl(out[[nm]], default = FALSE)
  }

  for (nm in c("residual_subtab")) {
    value <- home_sleep_restore_scalar_chr(out[[nm]], default = "")
    if (value %in% c("res1", "res2", "res3", "res4")) out[[nm]] <- value else out[[nm]] <- NULL
  }

  out[!vapply(out, is.null, logical(1))]
}

home_sleep_restore_normalize_step2_snapshot_table <- function(snapshot_table) {
  if (!is.list(snapshot_table)) return(list())
  rows <- home_sleep_restore_normalize_table_like(snapshot_table$rows)
  checked_ids <- home_sleep_restore_normalize_atomic_vector(snapshot_table$checked_ids)
  checked_ids <- if (is.null(checked_ids)) character(0) else trimws(as.character(checked_ids))
  checked_ids <- unique(checked_ids[nzchar(checked_ids)])
  active_row_id <- home_sleep_restore_scalar_chr(snapshot_table$active_row_id, default = "")
  row_seq <- suppressWarnings(as.integer(snapshot_table$row_seq)[1])
  if (!is.finite(row_seq) || row_seq < 0L) row_seq <- 0L

  out <- list()
  if (is.data.frame(rows) && nrow(rows) > 0L) out$rows <- rows
  if (length(checked_ids) > 0L) out$checked_ids <- checked_ids
  if (nzchar(active_row_id)) out$active_row_id <- active_row_id
  out$row_seq <- as.integer(row_seq)
  out
}

home_sleep_restore_normalize_step2_diagnostics <- function(diagnostics) {
  if (!is.list(diagnostics)) return(list())
  out <- list()

  error_analysis <- home_sleep_restore_normalize_table_like(diagnostics$error_analysis)
  if (is.data.frame(error_analysis) && nrow(error_analysis) > 0L) out$error_analysis <- error_analysis

  error_info <- home_sleep_restore_normalize_vector_map(diagnostics$error_analysis_info)
  if (length(error_info) > 0L) out$error_analysis_info <- error_info

  residuals_data <- home_sleep_restore_normalize_table_like(diagnostics$residuals_data)
  if (is.data.frame(residuals_data) && nrow(residuals_data) > 0L) out$residuals_data <- residuals_data

  corr <- home_sleep_restore_normalize_table_like(diagnostics$correlation_matrix)
  if (is.data.frame(corr) && nrow(corr) > 0L) out$correlation_matrix <- corr

  residual_subtab <- home_sleep_restore_scalar_chr(diagnostics$residual_subtab, default = "")
  if (residual_subtab %in% c("res1", "res2", "res3", "res4")) out$residual_subtab <- residual_subtab

  current_report <- home_sleep_restore_scalar_chr(diagnostics$current_report, default = "")
  if (nzchar(current_report)) out$current_report <- current_report

  out
}

home_sleep_restore_default_state <- function() {
  list(
    schema_version = home_sleep_restore_store_schema(),
    pending_restore = FALSE,
    saved_at = "",
    source_event = "",
    lang = "en",
    active_tab = "",
    steps = list(),
    restored_at = ""
  )
}

home_sleep_restore_normalize_steps <- function(steps) {
  if (!is.list(steps)) return(list())
  out <- list()

  step1 <- steps$step1
  if (is.list(step1)) {
    source_path <- home_sleep_restore_scalar_chr(step1$source_path, default = "")
    display_name <- home_sleep_restore_scalar_chr(step1$display_name, default = "")
    params <- home_sleep_restore_normalize_scalar_map(step1$params)
    if (nzchar(source_path) || nzchar(display_name) || length(params) > 0L) {
      out$step1 <- list(
        source_path = source_path,
        display_name = display_name,
        params = params
      )
    }
  }

  step2 <- steps$step2
  if (is.list(step2)) {
    source_path <- home_sleep_restore_scalar_chr(step2$source_path, default = "")
    file_name <- home_sleep_restore_scalar_chr(step2$file_name, default = "")
    source_kind <- home_sleep_restore_scalar_chr(step2$source_kind, default = "none")
    if (!source_kind %in% c("import", "step1_bridge", "sim_to_exp", "none")) source_kind <- "none"
    sheets <- home_sleep_restore_normalize_table_list(step2$sheets)
    manual_exp_data <- home_sleep_restore_normalize_table_like(step2$manual_exp_data)
    exp_data_disabled <- home_sleep_restore_scalar_lgl(step2$exp_data_disabled, default = FALSE)
    params <- home_sleep_restore_normalize_step2_params(step2$params)
    fit_bounds <- home_sleep_restore_normalize_fit_bounds(step2$fit_bounds)
    snapshot_table <- home_sleep_restore_normalize_step2_snapshot_table(step2$snapshot_table)
    diagnostics <- home_sleep_restore_normalize_step2_diagnostics(step2$diagnostics)
    was_fitting <- home_sleep_restore_scalar_lgl(step2$was_fitting, default = FALSE)

    has_payload <- nzchar(source_path) ||
      nzchar(file_name) ||
      length(sheets) > 0L ||
      is.data.frame(manual_exp_data) ||
      isTRUE(exp_data_disabled) ||
      length(params) > 0L ||
      length(fit_bounds) > 0L ||
      length(snapshot_table) > 0L ||
      length(diagnostics) > 0L ||
      isTRUE(was_fitting) ||
      !identical(source_kind, "none")

    if (isTRUE(has_payload)) {
      step2_out <- list(
        source_path = source_path,
        file_name = file_name,
        source_kind = source_kind,
        exp_data_disabled = exp_data_disabled,
        was_fitting = was_fitting
      )
      if (length(sheets) > 0L) step2_out$sheets <- sheets
      if (is.data.frame(manual_exp_data)) step2_out$manual_exp_data <- manual_exp_data
      if (length(params) > 0L) step2_out$params <- params
      if (length(fit_bounds) > 0L) step2_out$fit_bounds <- fit_bounds
      if (length(snapshot_table) > 0L) step2_out$snapshot_table <- snapshot_table
      if (length(diagnostics) > 0L) step2_out$diagnostics <- diagnostics
      out$step2 <- step2_out
    }
  }

  step3 <- steps$step3
  if (is.list(step3)) {
    source_path <- home_sleep_restore_scalar_chr(step3$source_path, default = "")
    file_name <- home_sleep_restore_scalar_chr(step3$file_name, default = "")
    settings <- home_sleep_restore_normalize_scalar_map(step3$settings)
    sheets <- home_sleep_restore_normalize_step3_sheets(step3$sheets)
    if (nzchar(source_path) || nzchar(file_name) || length(settings) > 0L || length(sheets) > 0L) {
      step3_out <- list(
        source_path = source_path,
        file_name = file_name,
        settings = settings
      )
      if (length(sheets) > 0L) {
        step3_out$sheets <- sheets
      }
      out$step3 <- step3_out
    }
  }

  out
}

home_sleep_restore_store_normalize_state <- function(state) {
  default_state <- home_sleep_restore_default_state()
  if (is.null(state) || !is.list(state)) return(default_state)

  schema <- home_sleep_restore_scalar_chr(state$schema_version, default = "")
  if (!identical(schema, home_sleep_restore_store_schema())) return(default_state)

  active_tab <- home_sleep_restore_scalar_chr(state$active_tab, default = "")
  if (!active_tab %in% c("", "home", "step1", "step2", "step3")) active_tab <- ""

  source_event <- home_sleep_restore_scalar_chr(state$source_event, default = "")
  if (!source_event %in% c("", "manual", "suspend", "autosave")) {
    source_event <- ""
  }

  pending_restore <- home_sleep_restore_scalar_lgl(state$pending_restore, default = FALSE)
  # 只允许 suspend 快照处于 pending，避免旧版本 autosave 脏状态反复触发恢复。
  if (!identical(source_event, "suspend")) {
    pending_restore <- FALSE
  }

  steps <- home_sleep_restore_normalize_steps(state$steps)

  list(
    schema_version = home_sleep_restore_store_schema(),
    pending_restore = pending_restore,
    saved_at = home_sleep_restore_scalar_chr(state$saved_at, default = ""),
    source_event = source_event,
    lang = home_sleep_restore_normalize_lang(state$lang, default = "en"),
    active_tab = active_tab,
    steps = steps,
    restored_at = home_sleep_restore_scalar_chr(state$restored_at, default = "")
  )
}

home_sleep_restore_store_load <- function(warn_fn = warning) {
  path <- home_sleep_restore_store_path()
  default_state <- home_sleep_restore_default_state()
  if (!file.exists(path)) return(default_state)

  loaded <- tryCatch(
    readRDS(path),
    error = function(e) {
      if (is.function(warn_fn)) {
        warn_fn(sprintf("sleep restore store load failed at %s: %s", path, conditionMessage(e)))
      }
      NULL
    }
  )
  if (is.null(loaded)) return(default_state)
  home_sleep_restore_store_normalize_state(loaded)
}

home_sleep_restore_store_save <- function(state, warn_fn = warning) {
  path <- home_sleep_restore_store_path()
  dir_path <- dirname(path)
  ok_dir <- tryCatch({
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    TRUE
  }, error = function(e) {
    if (is.function(warn_fn)) {
      warn_fn(sprintf("sleep restore store mkdir failed for %s: %s", dir_path, conditionMessage(e)))
    }
    FALSE
  })
  if (!isTRUE(ok_dir)) return(invisible(FALSE))

  normalized <- home_sleep_restore_store_normalize_state(state)
  tmp_path <- paste0(path, ".tmp-", Sys.getpid(), "-", as.integer(as.numeric(Sys.time())))
  ok_save <- tryCatch({
    saveRDS(normalized, file = tmp_path, version = 2)
    TRUE
  }, error = function(e) {
    if (is.function(warn_fn)) {
      warn_fn(sprintf("sleep restore store write failed at %s: %s", tmp_path, conditionMessage(e)))
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
        warn_fn(sprintf("sleep restore store replace failed for %s.", path))
      }
      return(invisible(FALSE))
    }
  }

  invisible(TRUE)
}
