# ==============================================================================
# export_bundle_helpers.R - Step2 export/bridge helper functions
# ==============================================================================

export_bridge_sanitize_base_name <- function(base_name = NULL, fallback_name = NULL, default_name = "ITC") {
  pick_first <- function(x) {
    if (is.null(x) || length(x) == 0) return("")
    out <- trimws(as.character(x[[1]]))
    if (!nzchar(out)) return("")
    out
  }

  base <- pick_first(base_name)
  if (!nzchar(base)) {
    base <- tools::file_path_sans_ext(basename(pick_first(fallback_name)))
  }
  base <- tools::file_path_sans_ext(base)
  base <- sub("_(processed|fitted)_\\d{8}_\\d{4}$", "", base)
  base <- gsub("[[:space:]/\\\\]+", "_", base)
  base <- gsub("_+", "_", base)
  base <- gsub("^_+|_+$", "", base)
  if (!nzchar(base)) base <- default_name
  base
}

export_bridge_build_params_snapshot_filename <- function(base_name = NULL, fallback_name = NULL, now = Sys.time()) {
  clean_base <- export_bridge_sanitize_base_name(base_name, fallback_name, default_name = "ITC")
  paste0(clean_base, "_FitParams_", format(now, "%Y%m%d_%H%M%S"), ".xlsx")
}

export_bridge_read_viabind_version <- function(default_version = "x.x.x", cwd = getwd()) {
  default_chr <- as.character(default_version)
  default_chr <- if (length(default_chr) == 0) "x.x.x" else trimws(default_chr[1])
  if (!nzchar(default_chr)) default_chr <- "x.x.x"

  from_env <- trimws(as.character(Sys.getenv("ITCSUITE_APP_VERSION", unset = ""))[1])
  if (nzchar(from_env)) return(from_env)

  cwd_chr <- as.character(cwd)
  cwd_chr <- if (length(cwd_chr) == 0) getwd() else trimws(cwd_chr[1])
  if (!nzchar(cwd_chr)) cwd_chr <- getwd()

  candidates <- unique(c(
    file.path(cwd_chr, "desktop", "package.json"),
    file.path(cwd_chr, "..", "desktop", "package.json")
  ))

  version_pattern <- '"version"[[:space:]]*:[[:space:]]*"([^"]+)"'
  for (path in candidates) {
    if (!file.exists(path)) next
    lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
    if (length(lines) == 0) next
    hit_idx <- grep(version_pattern, lines, perl = TRUE)
    if (length(hit_idx) < 1) next
    line <- lines[hit_idx[1]]
    cap <- regmatches(line, regexec(version_pattern, line, perl = TRUE))[[1]]
    if (length(cap) >= 2) {
      ver <- trimws(as.character(cap[2]))
      if (nzchar(ver)) return(ver)
    }
  }

  default_chr
}

export_bridge_build_version_signature <- function(module_name, version = NULL, default_version = "x.x.x") {
  module_chr <- as.character(module_name)
  module_chr <- if (length(module_chr) == 0) "" else trimws(module_chr[1])
  if (!nzchar(module_chr)) module_chr <- "UnknownModule"
  resolved_version <- as.character(version)
  resolved_version <- if (length(resolved_version) == 0) "" else trimws(resolved_version[1])
  if (!nzchar(resolved_version)) {
    resolved_version <- export_bridge_read_viabind_version(default_version = default_version)
  }
  paste0("ViaBind v", resolved_version, ": ", module_chr)
}

export_bridge_build_meta_signature_df <- function(module_name, version = NULL, default_version = "x.x.x") {
  data.frame(
    parameter = "generated_by",
    value = export_bridge_build_version_signature(
      module_name = module_name,
      version = version,
      default_version = default_version
    ),
    stringsAsFactors = FALSE
  )
}

export_bridge_build_fit_bounds_df <- function(fit_bounds, row_id = NULL, row_id_col = NULL) {
  row_id_col_chr <- as.character(row_id_col)
  row_id_col_chr <- if (length(row_id_col_chr) == 0L) "" else trimws(row_id_col_chr[1])
  row_id_chr <- as.character(row_id)
  row_id_chr <- if (length(row_id_chr) == 0L) "" else trimws(row_id_chr[1])
  out_cols <- c("param", "lower", "upper")
  if (nzchar(row_id_col_chr)) out_cols <- c(row_id_col_chr, out_cols)

  if (!is.list(fit_bounds) || length(fit_bounds) < 1L) {
    out <- as.data.frame(
      setNames(
        lapply(out_cols, function(col) {
          if (identical(col, "lower") || identical(col, "upper")) numeric(0) else character(0)
        }),
        out_cols
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    return(out)
  }

  rows <- vector("list", length(fit_bounds))
  row_n <- 0L
  for (nm in names(fit_bounds)) {
    param_name <- as.character(nm)
    param_name <- if (length(param_name) == 0L) "" else trimws(param_name[1])
    if (!nzchar(param_name)) next
    pair <- fit_bounds[[nm]]
    lower <- NA_real_
    upper <- NA_real_
    if (is.list(pair)) {
      lower <- suppressWarnings(as.numeric(pair$lower)[1])
      upper <- suppressWarnings(as.numeric(pair$upper)[1])
    } else {
      lower <- suppressWarnings(as.numeric(pair["lower"])[1])
      upper <- suppressWarnings(as.numeric(pair["upper"])[1])
      if (!is.finite(lower) || !is.finite(upper)) {
        pair_num <- suppressWarnings(as.numeric(pair))
        if (length(pair_num) >= 1L) lower <- pair_num[1]
        if (length(pair_num) >= 2L) upper <- pair_num[2]
      }
    }
    if (!is.finite(lower) || !is.finite(upper)) next
    if (lower > upper) {
      tmp <- lower
      lower <- upper
      upper <- tmp
    }
    row_n <- row_n + 1L
    row_item <- list(param = param_name, lower = lower, upper = upper)
    if (nzchar(row_id_col_chr)) row_item[[row_id_col_chr]] <- row_id_chr
    rows[[row_n]] <- row_item
  }

  rows <- rows[seq_len(row_n)]
  if (length(rows) < 1L) {
    out <- as.data.frame(
      setNames(
        lapply(out_cols, function(col) {
          if (identical(col, "lower") || identical(col, "upper")) numeric(0) else character(0)
        }),
        out_cols
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    return(out)
  }
  out <- do.call(rbind, lapply(rows, function(item) {
    row_df <- as.data.frame(item, stringsAsFactors = FALSE, check.names = FALSE)
    row_df[, out_cols, drop = FALSE]
  }))
  rownames(out) <- NULL
  out
}

export_bridge_build_snapshot_fit_bounds_df <- function(snapshot_fit_bounds_by_row_id,
                                                       row_ids = NULL,
                                                       row_id_col = "_snapshot_row_id") {
  row_id_col_chr <- as.character(row_id_col)
  row_id_col_chr <- if (length(row_id_col_chr) == 0L) "" else trimws(row_id_col_chr[1])
  if (!nzchar(row_id_col_chr)) row_id_col_chr <- "_snapshot_row_id"
  target_ids <- if (is.null(row_ids) || length(row_ids) == 0L) {
    names(snapshot_fit_bounds_by_row_id)
  } else {
    as.character(row_ids)
  }
  target_ids <- trimws(target_ids)
  target_ids <- unique(target_ids[nzchar(target_ids)])

  chunks <- lapply(target_ids, function(row_id) {
    export_bridge_build_fit_bounds_df(
      fit_bounds = snapshot_fit_bounds_by_row_id[[row_id]],
      row_id = row_id,
      row_id_col = row_id_col_chr
    )
  })
  chunks <- Filter(function(df) is.data.frame(df) && nrow(df) > 0L, chunks)
  if (length(chunks) < 1L) {
    return(data.frame(
      setNames(
        list(character(0), character(0), numeric(0), numeric(0)),
        c(row_id_col_chr, "param", "lower", "upper")
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }
  out <- do.call(rbind, chunks)
  rownames(out) <- NULL
  out
}

export_bridge_build_params_export_sheets <- function(export_df,
                                                     module_name = "ITCsimfit",
                                                     version = NULL,
                                                     default_version = "x.x.x",
                                                     snapshot_fit_bounds_df = NULL) {
  out <- list(
    snapshots = as.data.frame(export_df, stringsAsFactors = FALSE)
  )
  if (is.data.frame(snapshot_fit_bounds_df) && nrow(snapshot_fit_bounds_df) > 0L) {
    out[["snapshot_fit_bounds"]] <- as.data.frame(snapshot_fit_bounds_df, stringsAsFactors = FALSE)
  }
  out[["meta"]] <- export_bridge_build_meta_signature_df(
    module_name = module_name,
    version = version,
    default_version = default_version
  )
  out
}

export_bridge_build_fit_params_df <- function(safe_inp, active_paths_save, rss_info) {
  active_paths_norm <- if (exists("normalize_active_paths_with_dependencies", mode = "function", inherits = TRUE)) {
    tryCatch(normalize_active_paths_with_dependencies(active_paths_save), error = function(e) active_paths_save)
  } else {
    paths_raw <- if (is.null(active_paths_save)) character(0) else active_paths_save
    paths_raw <- unique(trimws(as.character(paths_raw)))
    paths_raw <- paths_raw[nzchar(paths_raw)]
    valid_paths <- c("rxn_D", "rxn_T", "rxn_E", "rxn_B", "rxn_F", "rxn_U")
    paths_norm <- valid_paths[valid_paths %in% paths_raw]
    if ("rxn_E" %in% paths_norm && !"rxn_T" %in% paths_norm) {
      paths_norm <- valid_paths[valid_paths %in% c(paths_norm, "rxn_T")]
    }
    if ("rxn_F" %in% paths_norm && !"rxn_D" %in% paths_norm) {
      paths_norm <- valid_paths[valid_paths %in% c(paths_norm, "rxn_D")]
    }
    paths_norm
  }
  fit_range_raw <- safe_inp("fit_data_range")
  fit_range_num <- suppressWarnings(as.numeric(fit_range_raw))
  fit_range_start <- if (length(fit_range_num) >= 1 && is.finite(fit_range_num[1])) {
    as.integer(round(fit_range_num[1]))
  } else {
    NA_integer_
  }
  fit_range_end <- if (length(fit_range_num) >= 2 && is.finite(fit_range_num[2])) {
    as.integer(round(fit_range_num[2]))
  } else {
    NA_integer_
  }

  data.frame(
    parameter = c(
      "logK1", "H1_cal_mol",
      "logK2", "H2_cal_mol",
      "logK3", "H3_cal_mol",
      "logK4", "H4_cal_mol",
      "logK5", "H5_cal_mol",
      "logK6", "H6_cal_mol",
      "logK7", "H7_cal_mol",
      "fH", "fG", "V_init_uL", "Offset_cal",
      "RSS", "RSS_method",
      "H_cell_0_mM", "G_syringe_mM", "V_cell_mL", "V_inj_uL",
      "n_inj", "FitRangeStart_Inj", "FitRangeEnd_Inj",
      "V_pre_uL", "Temp_K", "ActivePaths" # Step2 import uses this to restore active_paths.
    ),
    value = as.character(c(
      safe_inp("logK1"), safe_inp("H1"),
      if ("rxn_D" %in% active_paths_norm) safe_inp("logK2") else NA,
      if ("rxn_D" %in% active_paths_norm) safe_inp("H2") else NA,
      if ("rxn_T" %in% active_paths_norm) safe_inp("logK3") else NA,
      if ("rxn_T" %in% active_paths_norm) safe_inp("H3") else NA,
      if ("rxn_B" %in% active_paths_norm) safe_inp("logK4") else NA,
      if ("rxn_B" %in% active_paths_norm) safe_inp("H4") else NA,
      if ("rxn_F" %in% active_paths_norm) safe_inp("logK5") else NA,
      if ("rxn_F" %in% active_paths_norm) safe_inp("H5") else NA,
      if ("rxn_U" %in% active_paths_norm) safe_inp("logK6") else NA,
      if ("rxn_U" %in% active_paths_norm) safe_inp("H6") else NA,
      if ("rxn_E" %in% active_paths_norm) safe_inp("logK7") else NA,
      if ("rxn_E" %in% active_paths_norm) safe_inp("H7") else NA,
      safe_inp("factor_H"), safe_inp("factor_G"),
      safe_inp("V_init_val"), safe_inp("heat_offset"),
      if (is.null(rss_info$rss) || !is.finite(rss_info$rss)) NA else formatC(rss_info$rss, format = "e", digits = 3),
      if (is.null(rss_info$method)) "" else as.character(rss_info$method),
      safe_inp("H_cell_0"), safe_inp("G_syringe"),
      safe_inp("V_cell"), safe_inp("V_inj"),
      safe_inp("n_inj"), fit_range_start, fit_range_end,
      safe_inp("V_pre"),
      safe_inp("Temp"),
      paste(active_paths_norm, collapse = ",")
    )),
    stringsAsFactors = FALSE
  )
}

export_bridge_seed_sheet_list <- function(cached_sheets, manual_exp_data, manual_exp_source) {
  sheet_list <- list()
  cached <- cached_sheets
  if (is.null(cached)) return(sheet_list)

  drop_sheets <- if (!is.null(manual_exp_data) && identical(manual_exp_source, "sim_to_exp")) {
    c("meta", "power_corrected")
  } else {
    character(0)
  }

  for (sn in names(cached)) {
    if (!sn %in% drop_sheets) {
      sheet_list[[sn]] <- cached[[sn]]
    }
  }

  sheet_list
}

export_bridge_build_integration_rev <- function(exp_df) {
  if (is.null(exp_df) || !is.data.frame(exp_df) || nrow(exp_df) == 0) return(NULL)
  out <- data.frame(
    Injection = exp_df$Inj,
    Ratio_App = exp_df$Ratio_Raw,
    heat_cal_mol = exp_df$Heat_Raw,
    V_titrate_uL = if ("V_inj_uL" %in% names(exp_df)) exp_df$V_inj_uL else NA_real_,
    stringsAsFactors = FALSE
  )
  if ("Heat_ucal" %in% names(exp_df)) {
    out$Heat_ucal <- exp_df$Heat_ucal
  }
  out
}

export_bridge_build_meta_rev <- function(meta_cached, meta_updates) {
  if (!is.null(meta_cached) && is.data.frame(meta_cached) && all(c("parameter", "value") %in% colnames(meta_cached))) {
    meta_rev <- as.data.frame(meta_cached, stringsAsFactors = FALSE)
  } else {
    meta_rev <- data.frame(parameter = character(0), value = character(0), stringsAsFactors = FALSE)
  }

  normalize_meta_update <- function(entry) {
    to_chr_or_na <- function(x) {
      if (is.null(x) || length(x) == 0) return(NA_character_)
      val <- tryCatch(as.character(x[[1]]), error = function(e) {
        tryCatch(as.character(x[1]), error = function(e2) NA_character_)
      })
      if (length(val) == 0) return(NA_character_)
      val <- val[1]
      if (is.na(val)) return(NA_character_)
      val
    }

    if (is.list(entry) && !is.data.frame(entry)) {
      raw_value <- if ("value" %in% names(entry)) entry[["value"]] else entry[[1]]
      raw_unit <- if ("unit" %in% names(entry)) entry[["unit"]] else NA_character_
    } else {
      raw_value <- entry
      raw_unit <- NA_character_
    }

    value_chr <- to_chr_or_na(raw_value)
    unit_chr <- to_chr_or_na(raw_unit)

    list(value = value_chr, unit = unit_chr)
  }

  append_meta_row <- function(df, parameter, value, unit = NA_character_) {
    col_names <- names(df)
    row_vals <- as.list(rep(NA_character_, length(col_names)))
    names(row_vals) <- col_names
    if ("parameter" %in% col_names) row_vals$parameter <- parameter
    if ("value" %in% col_names) row_vals$value <- value
    if ("unit" %in% col_names) row_vals$unit <- if (is.na(unit)) "" else as.character(unit)[1]
    out <- rbind(df, as.data.frame(row_vals, stringsAsFactors = FALSE, check.names = FALSE))
    rownames(out) <- NULL
    out
  }

  for (nm in names(meta_updates)) {
    update_item <- normalize_meta_update(meta_updates[[nm]])
    if (nm %in% meta_rev$parameter) {
      idx <- which(meta_rev$parameter == nm)
      meta_rev$value[idx] <- update_item$value
      if ("unit" %in% names(meta_rev) && !is.na(update_item$unit)) {
        meta_rev$unit[idx] <- update_item$unit
      }
    } else {
      meta_rev <- append_meta_row(meta_rev, parameter = nm, value = update_item$value, unit = update_item$unit)
    }
  }

  rownames(meta_rev) <- NULL
  meta_rev
}

export_bridge_order_sheets <- function(sheet_list) {
  desired_order <- c(
    "power_original",
    "meta",
    "power_corrected",
    "integration",
    "meta_rev",
    "integration_rev",
    "simulation",
    "species_dist",
    "fit_params",
    "fit_bounds",
    "error_reliability",
    "error_analysis",
    "residuals",
    "correlation_matrix"
  )

  ordered_sheets <- list()
  for (nm in desired_order) {
    if (nm %in% names(sheet_list)) ordered_sheets[[nm]] <- sheet_list[[nm]]
  }
  for (nm in names(sheet_list)) {
    if (!nm %in% c(names(ordered_sheets), "report")) {
      ordered_sheets[[nm]] <- sheet_list[[nm]]
    }
  }
  if ("report" %in% names(sheet_list)) {
    ordered_sheets[["report"]] <- sheet_list[["report"]]
  }

  ordered_sheets
}

export_bridge_resolve_step2_plot_source <- function(manual_exp_source, imported_xlsx_filename, imported_xlsx_file_path) {
  source_mode <- as.character(manual_exp_source)
  source_mode <- if (length(source_mode) == 0) "" else source_mode[1]
  file_name <- as.character(imported_xlsx_filename)
  file_name <- if (length(file_name) == 0) "" else file_name[1]
  file_path <- as.character(imported_xlsx_file_path)
  file_path <- if (length(file_path) == 0) "" else file_path[1]
  has_real_file <- nzchar(file_path) && !startsWith(file_path, "bridge://")

  source <- "bridge"
  label <- "Step1 bridge"
  if (identical(source_mode, "step1_bridge")) {
    source <- "bridge"
    if (nzchar(file_name)) label <- file_name
  } else if (identical(source_mode, "sim_to_exp")) {
    source <- "sim_to_exp"
    label <- "Simulation to experiment"
  } else if (has_real_file) {
    source <- "file"
    label <- if (nzchar(file_name)) file_name else "Imported file"
  }

  list(source = source, source_label = label)
}
