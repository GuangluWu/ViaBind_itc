  # --- 5. 导出参数快照 (xlsx，参数名带单位后缀) ---
  output$export_params <- downloadHandler(
    filename = function() {
      paste("ITC_Snapshots_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx", sep="")
    },
    content = function(file) {
      export_df <- values$param_list
      # 重命名实验条件列，加单位后缀（内部名 → 文件名）
      col_rename <- c(
        "H_cell_0" = "H_cell_0_mM", "G_syringe" = "G_syringe_mM",
        "V_cell" = "V_cell_mL", "V_inj" = "V_inj_uL", "V_pre" = "V_pre_uL",
        "Temp" = "Temp_K",
        "H1" = "H1_cal_mol", "H2" = "H2_cal_mol", "H3" = "H3_cal_mol",
        "H4" = "H4_cal_mol", "H5" = "H5_cal_mol", "H6" = "H6_cal_mol",
        "V_init" = "V_init_uL", "Offset" = "Offset_cal"
      )
      for (old_name in names(col_rename)) {
        if (old_name %in% colnames(export_df)) {
          colnames(export_df)[colnames(export_df) == old_name] <- col_rename[[old_name]]
        }
      }
      writexl::write_xlsx(list(snapshots = export_df), path = file)
    }
  )
  
  build_fit_export_bundle <- function(sim = NULL) {
    sim_use <- if (is.null(sim)) tryCatch(sim_results(), error = function(e) NULL) else sim
    if (is.null(sim_use)) return(NULL)
    
    safe_inp <- function(nm) {
      tryCatch({
        v <- input[[nm]]
        if (is.null(v)) NA else v
      }, error = function(e) NA)
    }
    active_paths_save <- if (is.null(input$active_paths) || length(input$active_paths) == 0) character(0) else input$active_paths
    
    rss_info <- tryCatch(current_rss_val(), error = function(e) list(rss = NA_real_, method = ""))
    fit_params_df <- data.frame(
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
        "n_inj", "V_pre_uL", "Temp_K", "ActivePaths"
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
    
    sim_df <- as.data.frame(sim_use)
    sheet_list <- list()
    cached <- values$imported_xlsx_sheets
    if (!is.null(cached)) {
      drop_sheets <- if (!is.null(values$manual_exp_data) && identical(values$manual_exp_source, "sim_to_exp")) {
        c("meta", "power_corrected")
      } else {
        character(0)
      }
      for (sn in names(cached)) {
        if (!sn %in% drop_sheets) {
          sheet_list[[sn]] <- cached[[sn]]
        }
      }
    }
    
    exp_df <- tryCatch(exp_data_processed(), error = function(e) NULL)
    int_export <- NULL
    if (!is.null(exp_df) && nrow(exp_df) > 0) {
      int_export <- data.frame(
        Injection = exp_df$Inj,
        Ratio_App = exp_df$Ratio_Raw,
        heat_cal_mol = exp_df$Heat_Raw,
        V_titrate_uL = if ("V_inj_uL" %in% names(exp_df)) exp_df$V_inj_uL else NA_real_,
        stringsAsFactors = FALSE
      )
      sheet_list[["integration_rev"]] <- int_export
    }
    
    meta_cached <- if (!is.null(cached) && "meta" %in% names(cached)) cached[["meta"]] else NULL
    if (!is.null(meta_cached) && all(c("parameter", "value") %in% colnames(meta_cached))) {
      meta_rev <- meta_cached
    } else {
      meta_rev <- data.frame(parameter = character(0), value = character(0), stringsAsFactors = FALSE)
    }
    meta_updates <- c(
      H_cell_0_mM = safe_inp("H_cell_0"),
      G_syringe_mM = safe_inp("G_syringe"),
      V_cell_mL = safe_inp("V_cell"),
      V_inj_uL = safe_inp("V_inj"),
      n_inj = safe_inp("n_inj"),
      V_pre_uL = safe_inp("V_pre"),
      Temp_K = safe_inp("Temp")
    )
    for (nm in names(meta_updates)) {
      val_chr <- if (is.null(meta_updates[[nm]]) || is.na(meta_updates[[nm]])) NA_character_ else as.character(meta_updates[[nm]])
      if (nm %in% meta_rev$parameter) {
        meta_rev$value[meta_rev$parameter == nm] <- val_chr
      } else {
        meta_rev <- rbind(meta_rev, data.frame(parameter = nm, value = val_chr, stringsAsFactors = FALSE))
      }
    }
    
    sheet_list[["meta_rev"]] <- meta_rev
    sheet_list[["fit_params"]] <- fit_params_df
    sheet_list[["simulation"]] <- sim_df

    # Always regenerate report at export time to ensure current UI/fitting state
    # is written, instead of reusing any stale cached report text.
    report_text <- tryCatch(
      build_fitting_report_text(),
      error = function(e) {
        paste0(
          "FITTING REPORT\n",
          "Generated by ITCsimfit on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
          "Automatic report generation failed: ", conditionMessage(e), "\n"
        )
      }
    )
    values$current_report <- report_text
    report_lines <- unlist(strsplit(as.character(report_text), "\n", fixed = TRUE), use.names = FALSE)
    if (length(report_lines) == 0) report_lines <- ""
    sheet_list[["report"]] <- data.frame(
      line = seq_along(report_lines),
      text = report_lines,
      stringsAsFactors = FALSE
    )

    if (!is.null(values$error_analysis) && is.data.frame(values$error_analysis) && nrow(values$error_analysis) > 0) {
      sheet_list[["error_analysis"]] <- as.data.frame(values$error_analysis)
    }
    if (!is.null(values$error_analysis_info) && is.list(values$error_analysis_info) && length(values$error_analysis_info) > 0) {
      info_df <- data.frame(
        metric = names(values$error_analysis_info),
        value = vapply(values$error_analysis_info, function(x) {
          if (length(x) == 0 || is.null(x)) return(NA_character_)
          as.character(x[[1]])
        }, character(1)),
        stringsAsFactors = FALSE
      )
      sheet_list[["error_reliability"]] <- info_df
    }
    if (!is.null(values$residuals_data) && is.data.frame(values$residuals_data) && nrow(values$residuals_data) > 0) {
      sheet_list[["residuals"]] <- as.data.frame(values$residuals_data)
    }
    if (!is.null(values$correlation_matrix) && is.matrix(values$correlation_matrix) && nrow(values$correlation_matrix) > 0) {
      corr_mat <- values$correlation_matrix
      corr_df <- data.frame(
        Parameter = rownames(corr_mat) %||% as.character(seq_len(nrow(corr_mat))),
        as.data.frame(corr_mat, check.names = FALSE),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
      sheet_list[["correlation_matrix"]] <- corr_df
    }
    
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
    sheet_list <- ordered_sheets

    list(
      sheets = sheet_list,
      integration_rev = int_export,
      meta_rev = meta_rev,
      fit_params = fit_params_df,
      simulation = sim_df
    )
  }
  
  resolve_step2_plot_source <- function() {
    source_mode <- as.character(values$manual_exp_source %||% "")
    source_mode <- if (length(source_mode) == 0) "" else source_mode[1]
    file_name <- as.character(values$imported_xlsx_filename %||% "")
    file_name <- if (length(file_name) == 0) "" else file_name[1]
    file_path <- as.character(values$imported_xlsx_file_path %||% "")
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

  publish_step2_plot_payload <- function(sim = NULL) {
    bundle <- build_fit_export_bundle(sim = sim)
    if (is.null(bundle)) return(invisible(FALSE))
    src_info <- resolve_step2_plot_source()
    token <- next_bridge_token()
    bridge_set("step2_plot_payload", list(
      schema_version = "itcsuite.step2_plot.v1",
      created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
      token = token,
      source = src_info$source %||% "bridge",
      source_label = src_info$source_label %||% "",
      sheets = bundle$sheets %||% list(),
      integration_rev = bundle$integration_rev %||% NULL,
      meta_rev = bundle$meta_rev %||% NULL,
      fit_params = bundle$fit_params %||% data.frame(),
      simulation = bundle$simulation %||% data.frame()
    ))
    invisible(TRUE)
  }
  
  last_data_to_plot_click <- reactiveVal(0L)
  observeEvent(input$data_to_plot, {
    click_id <- suppressWarnings(as.integer(input$data_to_plot)[1])
    if (!is.finite(click_id) || click_id <= 0L) return()
    if (click_id <= last_data_to_plot_click()) return()
    last_data_to_plot_click(click_id)
    sim <- tryCatch(sim_results(), error = function(e) NULL)
    ok <- isTRUE(publish_step2_plot_payload(sim = sim))
    if (!ok) {
      showNotification("No data available for Step 3.", type = "warning", duration = 3)
      return()
    }
    tryCatch({
      updateTabsetPanel(session, "main_tabs", selected = "Step 3 Plot & Export")
    }, error = function(e) NULL)
    showNotification("Data sent to Step 3.", type = "message", duration = 2)
  }, ignoreInit = TRUE)
  
  # --- 6. 导出拟合数据（多 sheet xlsx，保留 ITCprocessor 原始数据 + 拟合结果）---
  output$simfit_downloadData <- downloadHandler(
    filename = function() {
      base <- values$imported_xlsx_base_name
      if (is.null(base) || length(base) == 0 || base == "") base <- "ITC_Fit_Data"
      paste0(base, "_fitted_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx")
    },
    content = function(file) {
      bundle <- build_fit_export_bundle()
      if (is.null(bundle)) {
        showNotification(tr("export_error_no_data", lang()), type = "error", duration = 5)
        writexl::write_xlsx(list(error = data.frame(Note = tr("export_error_no_data_note", lang()))), path = file)
        return()
      }
      writexl::write_xlsx(bundle$sheets, path = file)
    }
  )
