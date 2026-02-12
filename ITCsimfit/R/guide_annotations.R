# ==============================================================================
# guide_annotations.R - Guide annotation schema loader/validator/resolver
# ==============================================================================
# [COMMENT_STD][MODULE_HEADER]
# 模块职责：提供用户引导注释配置的加载、校验与查询能力（仅预埋，不渲染 UI）。
# 依赖：base R（read.csv / regular expressions / data.frame）。
# 对外接口：load_guide_annotations() / validate_guide_annotations() / resolve_guide_annotation()。
# 副作用：读取 CSV 文件；校验失败时 stop；不写入任何业务状态。
# 变更历史：2026-02-12 - Phase 4 初版。

`%||%` <- function(x, y) if (is.null(x)) y else x

guide_schema_version <- function() {
  if (exists("GUIDE_SCHEMA_VERSION", inherits = TRUE)) return(get("GUIDE_SCHEMA_VERSION", inherits = TRUE))
  "itcsuite.guide_annotation.v1"
}

guide_default_path <- function() {
  if (exists("GUIDE_ANNOTATIONS_PATH", inherits = TRUE)) return(get("GUIDE_ANNOTATIONS_PATH", inherits = TRUE))
  file.path("config", "guide_annotations.v1.csv")
}

guide_app_id <- function() {
  if (exists("GUIDE_APP_ID", inherits = TRUE)) return(get("GUIDE_APP_ID", inherits = TRUE))
  "ITCsimfit"
}

guide_allowed_control_types <- function() {
  if (exists("GUIDE_ALLOWED_CONTROL_TYPES", inherits = TRUE)) {
    return(tolower(as.character(get("GUIDE_ALLOWED_CONTROL_TYPES", inherits = TRUE))))
  }
  c(
    "actionbutton", "numericinput", "sliderinput", "fileinput",
    "checkboxinput", "checkboxgroupinput", "downloadbutton",
    "textinput", "plotoutput", "dtoutput", "uioutput", "other"
  )
}

guide_allowed_severity <- function() {
  if (exists("GUIDE_ALLOWED_SEVERITY", inherits = TRUE)) {
    return(tolower(as.character(get("GUIDE_ALLOWED_SEVERITY", inherits = TRUE))))
  }
  c("info", "warning", "critical")
}

guide_allowed_status <- function() {
  if (exists("GUIDE_ALLOWED_STATUS", inherits = TRUE)) {
    return(tolower(as.character(get("GUIDE_ALLOWED_STATUS", inherits = TRUE))))
  }
  c("active", "inactive", "deprecated")
}

guide_required_columns <- function() {
  c(
    "schema_version", "app", "guide_id", "control_id", "control_type",
    "lang_zh", "lang_en", "severity", "status",
    "since_version", "until_version", "order", "notes"
  )
}

is_non_empty_scalar <- function(x) {
  if (length(x) < 1) return(FALSE)
  val <- trimws(as.character(x)[1])
  nzchar(val)
}

normalize_version_value <- function(x) {
  val <- trimws(as.character(x %||% ""))
  if (length(val) < 1 || !nzchar(val[1])) return(NA_character_)
  val[1]
}

is_valid_version_string <- function(x) {
  is.na(x) || grepl("^[0-9]+(\\.[0-9]+){0,2}$", x)
}

parse_version_vec <- function(x) {
  if (is.na(x)) return(c(NA_integer_, NA_integer_, NA_integer_))
  nums <- suppressWarnings(as.integer(strsplit(x, "\\.", fixed = FALSE)[[1]]))
  if (length(nums) < 1 || any(!is.finite(nums))) return(c(NA_integer_, NA_integer_, NA_integer_))
  c(nums, rep(0L, max(0L, 3L - length(nums))))[1:3]
}

compare_versions <- function(a, b) {
  va <- parse_version_vec(a)
  vb <- parse_version_vec(b)
  if (any(is.na(va)) || any(is.na(vb))) return(NA_integer_)
  if (identical(va, vb)) return(0L)
  for (i in seq_along(va)) {
    if (va[i] < vb[i]) return(-1L)
    if (va[i] > vb[i]) return(1L)
  }
  0L
}

version_in_range <- function(app_version, since_version, until_version) {
  v <- normalize_version_value(app_version)
  if (is.na(v)) return(TRUE)

  lo <- normalize_version_value(since_version)
  hi <- normalize_version_value(until_version)

  ge_lo <- TRUE
  le_hi <- TRUE
  if (!is.na(lo)) ge_lo <- isTRUE(compare_versions(v, lo) >= 0L)
  if (!is.na(hi)) le_hi <- isTRUE(compare_versions(v, hi) <= 0L)
  isTRUE(ge_lo && le_hi)
}

resolve_guide_annotations_path <- function(path) {
  raw <- trimws(as.character(path %||% ""))
  if (length(raw) < 1 || !nzchar(raw[1])) return(NA_character_)
  raw <- raw[1]

  if (file.exists(raw)) return(raw)

  dirs <- unique(c(
    getwd(),
    dirname(getwd()),
    dirname(dirname(getwd()))
  ))
  dirs <- normalizePath(dirs, winslash = "/", mustWork = FALSE)
  dirs <- dirs[dir.exists(dirs)]

  candidates <- unique(unlist(lapply(dirs, function(d) {
    c(file.path(d, raw), file.path(d, "ITCsimfit", raw))
  }), use.names = FALSE))
  hits <- candidates[file.exists(candidates)]
  if (length(hits) < 1) return(raw)
  hits[1]
}

normalize_guide_annotations_df <- function(df) {
  if (is.null(df)) return(data.frame(stringsAsFactors = FALSE))
  out <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  req <- guide_required_columns()

  for (nm in req) {
    if (!nm %in% names(out)) next
    out[[nm]] <- as.character(out[[nm]])
    out[[nm]] <- trimws(out[[nm]])
  }

  if ("control_type" %in% names(out)) out$control_type <- tolower(out$control_type)
  if ("severity" %in% names(out)) out$severity <- tolower(out$severity)
  if ("status" %in% names(out)) out$status <- tolower(out$status)
  if ("app" %in% names(out)) out$app <- trimws(out$app)
  if ("order" %in% names(out)) out$order <- suppressWarnings(as.integer(out$order))

  out
}

validate_guide_annotations <- function(df, target_schema = guide_schema_version()) {
  errors <- character(0)
  if (is.null(df) || !is.data.frame(df)) {
    return(list(ok = FALSE, errors = c("Input must be a data.frame.")))
  }

  req <- guide_required_columns()
  missing_cols <- setdiff(req, names(df))
  if (length(missing_cols) > 0) {
    errors <- c(errors, sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
    return(list(ok = FALSE, errors = unique(errors)))
  }

  data <- normalize_guide_annotations_df(df)
  row_id <- function(i) sprintf("row %d", i)

  bad_schema <- which(data$schema_version != target_schema)
  if (length(bad_schema) > 0) {
    errors <- c(errors, sprintf("%s schema_version must be %s.", paste(row_id(bad_schema), collapse = ", "), target_schema))
  }

  empty_guide <- which(!nzchar(data$guide_id))
  if (length(empty_guide) > 0) {
    errors <- c(errors, sprintf("%s guide_id must be non-empty.", paste(row_id(empty_guide), collapse = ", ")))
  }

  if (nrow(data) > 0) {
    dup_gid <- unique(data$guide_id[duplicated(data$guide_id) & nzchar(data$guide_id)])
    if (length(dup_gid) > 0) {
      errors <- c(errors, sprintf("guide_id must be unique, duplicated: %s", paste(dup_gid, collapse = ", ")))
    }
  }

  empty_control <- which(!nzchar(data$control_id))
  if (length(empty_control) > 0) {
    errors <- c(errors, sprintf("%s control_id must be non-empty.", paste(row_id(empty_control), collapse = ", ")))
  }

  bad_control_type <- which(!(data$control_type %in% guide_allowed_control_types()))
  if (length(bad_control_type) > 0) {
    errors <- c(errors, sprintf("%s control_type invalid.", paste(row_id(bad_control_type), collapse = ", ")))
  }

  bad_severity <- which(!(data$severity %in% guide_allowed_severity()))
  if (length(bad_severity) > 0) {
    errors <- c(errors, sprintf("%s severity invalid.", paste(row_id(bad_severity), collapse = ", ")))
  }

  bad_status <- which(!(data$status %in% guide_allowed_status()))
  if (length(bad_status) > 0) {
    errors <- c(errors, sprintf("%s status invalid.", paste(row_id(bad_status), collapse = ", ")))
  }

  bad_order <- which(is.na(data$order))
  if (length(bad_order) > 0) {
    errors <- c(errors, sprintf("%s order must be integer.", paste(row_id(bad_order), collapse = ", ")))
  }

  active_rows <- which(data$status == "active")
  if (length(active_rows) > 0) {
    bad_active_lang <- active_rows[!nzchar(data$lang_zh[active_rows]) | !nzchar(data$lang_en[active_rows])]
    if (length(bad_active_lang) > 0) {
      errors <- c(errors, sprintf("%s active row must provide lang_zh and lang_en.", paste(row_id(bad_active_lang), collapse = ", ")))
    }
  }

  since_vals <- vapply(data$since_version, normalize_version_value, character(1))
  until_vals <- vapply(data$until_version, normalize_version_value, character(1))

  bad_since <- which(!vapply(since_vals, is_valid_version_string, logical(1)))
  if (length(bad_since) > 0) {
    errors <- c(errors, sprintf("%s since_version format invalid.", paste(row_id(bad_since), collapse = ", ")))
  }

  bad_until <- which(!vapply(until_vals, is_valid_version_string, logical(1)))
  if (length(bad_until) > 0) {
    errors <- c(errors, sprintf("%s until_version format invalid.", paste(row_id(bad_until), collapse = ", ")))
  }

  interval_rows <- which(!is.na(since_vals) & !is.na(until_vals))
  if (length(interval_rows) > 0) {
    bad_interval <- interval_rows[vapply(interval_rows, function(i) {
      cmp <- compare_versions(since_vals[i], until_vals[i])
      is.na(cmp) || cmp > 0L
    }, logical(1))]
    if (length(bad_interval) > 0) {
      errors <- c(errors, sprintf("%s since_version must be <= until_version.", paste(row_id(bad_interval), collapse = ", ")))
    }
  }

  list(ok = length(errors) == 0, errors = unique(errors))
}

load_guide_annotations <- function(path = guide_default_path()) {
  if (!is_non_empty_scalar(path)) stop("Guide annotation path must be non-empty.")
  resolved_path <- resolve_guide_annotations_path(path)
  if (!file.exists(resolved_path)) stop(sprintf("Guide annotation config not found: %s", path))

  raw_df <- read.csv(resolved_path, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")
  df <- normalize_guide_annotations_df(raw_df)
  check <- validate_guide_annotations(df, target_schema = guide_schema_version())
  if (!isTRUE(check$ok)) {
    stop(sprintf("Guide annotation config validation failed: %s", paste(check$errors, collapse = " | ")))
  }
  df
}

resolve_guide_annotation <- function(control_id, lang = "en", app_version = NULL) {
  ctrl <- trimws(as.character(control_id %||% ""))
  if (length(ctrl) < 1 || !nzchar(ctrl[1])) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  ctrl <- ctrl[1]

  lang_norm <- if (identical(trimws(as.character(lang %||% "en"))[1], "zh")) "zh" else "en"
  app_norm <- tolower(guide_app_id())

  df <- load_guide_annotations()
  rows <- which(
    df$status == "active" &
      df$control_id == ctrl &
      (tolower(df$app) == app_norm | tolower(df$app) == "all")
  )

  if (length(rows) < 1) {
    out <- df[0, , drop = FALSE]
    out$text <- character(0)
    out$lang_used <- character(0)
    return(out)
  }

  filtered <- df[rows, , drop = FALSE]
  if (is_non_empty_scalar(app_version)) {
    keep <- vapply(seq_len(nrow(filtered)), function(i) {
      version_in_range(
        app_version = app_version,
        since_version = filtered$since_version[i],
        until_version = filtered$until_version[i]
      )
    }, logical(1))
    filtered <- filtered[keep, , drop = FALSE]
  }

  if (nrow(filtered) < 1) {
    filtered$text <- character(0)
    filtered$lang_used <- character(0)
    return(filtered)
  }

  text_zh <- trimws(filtered$lang_zh)
  text_en <- trimws(filtered$lang_en)
  requested <- if (identical(lang_norm, "zh")) text_zh else text_en
  fallback_en <- text_en

  text_resolved <- ifelse(
    nzchar(requested),
    requested,
    ifelse(nzchar(fallback_en), fallback_en, filtered$guide_id)
  )
  lang_used <- ifelse(
    nzchar(requested),
    lang_norm,
    ifelse(nzchar(fallback_en), "en", "guide_id")
  )

  ord <- suppressWarnings(as.integer(filtered$order))
  ord[!is.finite(ord)] <- .Machine$integer.max
  filtered$text <- text_resolved
  filtered$lang_used <- lang_used
  filtered <- filtered[order(ord, filtered$guide_id), , drop = FALSE]
  rownames(filtered) <- NULL
  filtered
}
