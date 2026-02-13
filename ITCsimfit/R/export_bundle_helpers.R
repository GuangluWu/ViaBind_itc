# ==============================================================================
# export_bundle_helpers.R - Step2 export/bridge helper functions
# ==============================================================================

export_bridge_build_fit_params_df <- function(safe_inp, active_paths_save, rss_info) {
  data.frame(
    parameter = c(
      "logK1", "H1_cal_mol",
      "logK2", "H2_cal_mol",
      "logK3", "H3_cal_mol",
      "logK4", "H4_cal_mol",
      "logK5", "H5_cal_mol",
      "logK6", "H6_cal_mol",
      "fH", "fG", "V_init_uL", "Offset_cal",
      "RSS", "RSS_method",
      "H_cell_0_mM", "G_syringe_mM", "V_cell_mL", "V_inj_uL",
      "n_inj", "V_pre_uL", "Temp_K", "ActivePaths" # Step2 import uses this to restore active_paths.
    ),
    value = as.character(c(
      safe_inp("logK1"), safe_inp("H1"),
      if ("rxn_D" %in% active_paths_save) safe_inp("logK2") else NA,
      if ("rxn_D" %in% active_paths_save) safe_inp("H2") else NA,
      if ("rxn_T" %in% active_paths_save) safe_inp("logK3") else NA,
      if ("rxn_T" %in% active_paths_save) safe_inp("H3") else NA,
      if ("rxn_B" %in% active_paths_save) safe_inp("logK4") else NA,
      if ("rxn_B" %in% active_paths_save) safe_inp("H4") else NA,
      if ("rxn_F" %in% active_paths_save) safe_inp("logK5") else NA,
      if ("rxn_F" %in% active_paths_save) safe_inp("H5") else NA,
      if ("rxn_U" %in% active_paths_save) safe_inp("logK6") else NA,
      if ("rxn_U" %in% active_paths_save) safe_inp("H6") else NA,
      safe_inp("factor_H"), safe_inp("factor_G"),
      safe_inp("V_init_val"), safe_inp("heat_offset"),
      if (is.null(rss_info$rss) || !is.finite(rss_info$rss)) NA else formatC(rss_info$rss, format = "e", digits = 3),
      if (is.null(rss_info$method)) "" else as.character(rss_info$method),
      safe_inp("H_cell_0"), safe_inp("G_syringe"),
      safe_inp("V_cell"), safe_inp("V_inj"),
      safe_inp("n_inj"), safe_inp("V_pre"),
      safe_inp("Temp"),
      paste(active_paths_save, collapse = ",")
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
  data.frame(
    Injection = exp_df$Inj,
    Ratio_App = exp_df$Ratio_Raw,
    heat_cal_mol = exp_df$Heat_Raw,
    V_titrate_uL = if ("V_inj_uL" %in% names(exp_df)) exp_df$V_inj_uL else NA_real_,
    stringsAsFactors = FALSE
  )
}

export_bridge_build_meta_rev <- function(meta_cached, meta_updates) {
  if (!is.null(meta_cached) && is.data.frame(meta_cached) && all(c("parameter", "value") %in% colnames(meta_cached))) {
    meta_rev <- meta_cached
  } else {
    meta_rev <- data.frame(parameter = character(0), value = character(0), stringsAsFactors = FALSE)
  }

  for (nm in names(meta_updates)) {
    raw_val <- meta_updates[[nm]]
    val_chr <- if (is.null(raw_val) || is.na(raw_val)) NA_character_ else as.character(raw_val)
    if (nm %in% meta_rev$parameter) {
      meta_rev$value[meta_rev$parameter == nm] <- val_chr
    } else {
      meta_rev <- rbind(meta_rev, data.frame(parameter = nm, value = val_chr, stringsAsFactors = FALSE))
    }
  }

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
    "fit_params",
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
