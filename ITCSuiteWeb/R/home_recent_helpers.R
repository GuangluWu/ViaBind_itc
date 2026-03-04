`%||%` <- function(x, y) if (is.null(x)) y else x

# [COMMENT_STD][MODULE_HEADER]
# 模块职责：提供首页最近导入记录的类型识别、目标步骤映射与裁剪排序辅助函数。
# 依赖：base R（字符串处理、列表处理、时间转换）。
# 对外接口：home_detect_import_type()、home_target_step_from_import_type()、home_filter_existing_recent_records()、home_trim_recent_records()。
# 副作用：无（纯函数，不修改外部状态）。
# 变更历史：2026-02-14 - 新增首页最近导入 helper。

normalize_home_scalar_chr <- function(x, default = "") {
  val <- as.character(x %||% "")
  if (length(val) < 1) return(default)
  out <- trimws(val[1])
  if (!nzchar(out)) default else out
}

home_detect_import_type <- function(file_name = NULL, sheets = NULL, fallback = "processed_xlsx") {
  allowed <- c("itc", "processed_xlsx", "fitted_xlsx")
  fallback_norm <- tolower(normalize_home_scalar_chr(fallback, default = "processed_xlsx"))
  if (!fallback_norm %in% allowed) fallback_norm <- "processed_xlsx"

  file_norm <- tolower(normalize_home_scalar_chr(file_name, default = ""))
  if (grepl("\\.(itc|txt|nitc|csc|xml)$", file_norm)) return("itc")
  if (grepl("_processed_\\d{8}_\\d{4}\\.xlsx$", file_norm)) return("processed_xlsx")
  if (grepl("_fitted_\\d{8}_\\d{4}\\.xlsx$", file_norm)) return("fitted_xlsx")

  if (is.list(sheets) && length(sheets) > 0) {
    sheet_names <- tolower(names(sheets) %||% character(0))
    if (any(sheet_names %in% c("integration", "integration_rev"))) return("processed_xlsx")

    sim_idx <- match("simulation", sheet_names)
    if (is.finite(sim_idx) && !is.na(sim_idx)) {
      sim_df <- sheets[[sim_idx]]
      if (is.data.frame(sim_df)) {
        sim_cols <- tolower(names(sim_df))
        if (all(c("ratio_app", "dq_app") %in% sim_cols)) return("fitted_xlsx")
      }
    }
  }

  fallback_norm
}

home_target_step_from_import_type <- function(import_type) {
  type_norm <- tolower(normalize_home_scalar_chr(import_type, default = "processed_xlsx"))
  if (identical(type_norm, "itc")) "step1" else "step2"
}

home_parse_imported_at <- function(imported_at) {
  if (inherits(imported_at, "POSIXt")) {
    ts_num <- suppressWarnings(as.numeric(imported_at)[1])
    if (is.finite(ts_num)) return(ts_num)
  }
  if (inherits(imported_at, "Date")) {
    ts_num <- suppressWarnings(as.numeric(as.POSIXct(imported_at, tz = "UTC"))[1])
    if (is.finite(ts_num)) return(ts_num)
  }

  value <- normalize_home_scalar_chr(imported_at, default = "")
  if (!nzchar(value)) return(NA_real_)

  value_num <- suppressWarnings(as.numeric(value)[1])
  if (is.finite(value_num)) return(value_num)

  value_wo_z <- sub("Z$", "", value, ignore.case = TRUE)
  value_wo_tz <- sub("([+-][0-9]{2}:?[0-9]{2})$", "", value_wo_z)
  value_trim_fraction <- sub("\\.[0-9]+$", "", value_wo_tz)
  candidates <- unique(c(value, value_wo_z, value_wo_tz, value_trim_fraction))
  formats <- c(
    "%Y-%m-%dT%H:%M:%OS",
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%d %H:%M:%OS",
    "%Y-%m-%d %H:%M:%S",
    "%Y/%m/%d %H:%M:%S",
    "%Y-%m-%d"
  )

  for (txt in candidates) {
    if (!nzchar(txt)) next
    for (fmt in formats) {
      ts_num <- suppressWarnings(as.numeric(as.POSIXct(txt, format = fmt, tz = "UTC"))[1])
      if (is.finite(ts_num)) return(ts_num)
    }
  }

  t2 <- suppressWarnings(as.numeric(as.POSIXct(value, tz = "UTC"))[1])
  if (is.finite(t2)) return(t2)
  NA_real_
}

home_sort_recent_records <- function(records) {
  if (!is.list(records) || length(records) < 2) return(records)
  scores <- vapply(records, function(rec) {
    if (!is.list(rec)) return(NA_real_)
    home_parse_imported_at(rec$imported_at)
  }, numeric(1))
  scores[!is.finite(scores)] <- -Inf
  ord <- order(-scores, seq_along(scores))
  records[ord]
}

home_filter_existing_recent_records <- function(records) {
  if (!is.list(records) || length(records) < 1) return(list())
  Filter(function(rec) is.list(rec) && isTRUE(rec$path_exists), records)
}

home_trim_recent_records <- function(records, max_records = 200L) {
  if (!is.list(records)) records <- list()
  max_n <- suppressWarnings(as.integer(max_records)[1])
  if (!is.finite(max_n) || max_n < 1L) max_n <- 200L

  sorted <- home_sort_recent_records(records)
  if (length(sorted) <= max_n) {
    return(list(records = sorted, dropped_payload_keys = character(0)))
  }

  dropped <- sorted[seq.int(max_n + 1L, length(sorted))]
  dropped_keys <- unique(vapply(dropped, function(rec) {
    if (!is.list(rec)) return("")
    normalize_home_scalar_chr(rec$restore_payload_key, default = "")
  }, character(1)))
  dropped_keys <- dropped_keys[nzchar(dropped_keys)]

  list(
    records = sorted[seq_len(max_n)],
    dropped_payload_keys = dropped_keys
  )
}
