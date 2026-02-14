  # [COMMENT_STD][MODULE_HEADER]
  # 模块职责：负责 Step2 快照导出、Step3 bridge payload 组装与导出文件写入。
  # 依赖：writexl、snapshot helper、bridge helper、Shiny downloadHandler。
  # 对外接口：output$export_params、publish_step2_plot_payload()、output$simfit_downloadData。
  # 副作用：写出 xlsx、更新 bridge channel、触发通知与 tab 切换。
  # 变更历史：2026-02-12 - 增加 Phase 4 注释规范样板。
  last_step2_export_params_name <- reactiveVal(NULL)
  last_step2_export_fitted_name <- reactiveVal(NULL)

  normalize_step2_export_path <- function(path) {
    p <- as.character(path %||% "")[1]
    p <- trimws(p)
    if (!nzchar(p)) return("")
    tryCatch(normalizePath(p, winslash = "/", mustWork = FALSE), error = function(e) p)
  }

  observeEvent(input$export_params_trigger, {
    if (lang_switching()) return()
    checked_ids <- as.character(values$param_checked_ids %||% character(0))
    if (length(checked_ids) == 0) {
      showNotification(tr("snapshot_export_none_selected", lang()), type = "warning", duration = 4)
      return()
    }
    session$sendCustomMessage("trigger-download", "export_params")
  }, ignoreInit = TRUE)

  # --- 5. 导出参数快照 (xlsx，参数名带单位后缀) ---
  output$export_params <- downloadHandler(
    filename = function() {
      fname <- export_bridge_build_params_snapshot_filename(
        base_name = values$imported_xlsx_base_name,
        fallback_name = values$imported_xlsx_filename,
        now = Sys.time()
      )
      last_step2_export_params_name(fname)
      fname
    },
    content = function(file) {
      export_df <- as.data.frame(values$param_list)
      checked_ids <- as.character(values$param_checked_ids %||% character(0))
      if (!is.null(export_df) && nrow(export_df) > 0 && "row_id" %in% colnames(export_df)) {
        if (length(checked_ids) > 0) {
          export_df <- export_df[export_df$row_id %in% checked_ids, , drop = FALSE]
        } else {
          export_df <- export_df[0, , drop = FALSE]
        }
      }
      if ("row_id" %in% colnames(export_df)) {
        export_df$row_id <- NULL
      }
      if (length(checked_ids) == 0) return(invisible(NULL))
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

      export_name <- as.character(last_step2_export_params_name() %||% "")[1]
      if (!nzchar(trimws(export_name))) export_name <- basename(file)
      export_path <- normalize_step2_export_path(file)
      import_raw <- as.character(values$imported_xlsx_file_path %||% "")[1]
      import_raw <- trimws(import_raw)
      source_import_path <- ""
      if (nzchar(import_raw) && !startsWith(import_raw, "bridge://")) {
        source_import_path <- normalize_step2_export_path(import_raw)
      }
      if (!nzchar(source_import_path)) return(invisible(NULL))
      home_add_recent_export(
        list(
          display_name = export_name,
          file_name = export_name,
          source_step = "step2",
          target_step = "step2",
          export_type = "xlsx",
          source_path = source_import_path,
          artifact_path = export_path,
          source_path_kind = "import"
        )
      )
    }
  )
  
  # [COMMENT_STD][IO_CONTRACT]
  # 输入来源：当前 UI 输入（input）、计算结果（sim_results）、导入缓存（values$imported_xlsx_sheets）。
  # 字段/类型：bundle 为 list，含 sheets(list<data.frame>) / fit_params(data.frame) / simulation(data.frame)。
  # 单位：沿用 Step2 语义（浓度 mM、体积 uL/mL、热量 cal/mol）；导出时保持与 bridge 约定一致。
  # 空值策略：关键数据缺失时返回 NULL；导出 handler 兜底写入错误提示 sheet。
  # 输出保证：成功返回可序列化 list，且包含 Step3 所需 schema 字段。
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
    fit_params_df <- export_bridge_build_fit_params_df(
      safe_inp = safe_inp,
      active_paths_save = active_paths_save,
      rss_info = rss_info
    )
    
    sim_df <- as.data.frame(sim_use)
    sheet_list <- export_bridge_seed_sheet_list(
      cached_sheets = values$imported_xlsx_sheets,
      manual_exp_data = values$manual_exp_data,
      manual_exp_source = values$manual_exp_source
    )
    
    exp_df <- tryCatch(exp_data_processed(), error = function(e) NULL)
    int_export <- export_bridge_build_integration_rev(exp_df)
    if (!is.null(int_export)) {
      sheet_list[["integration_rev"]] <- int_export
    }
    
    cached <- values$imported_xlsx_sheets
    meta_cached <- if (!is.null(cached) && "meta" %in% names(cached)) cached[["meta"]] else NULL
    meta_rev <- export_bridge_build_meta_rev(
      meta_cached = meta_cached,
      meta_updates = list(
        H_cell_0_mM = safe_inp("H_cell_0"),
        G_syringe_mM = safe_inp("G_syringe"),
        V_cell_mL = safe_inp("V_cell"),
        V_inj_uL = safe_inp("V_inj"),
        n_inj = safe_inp("n_inj"),
        V_pre_uL = safe_inp("V_pre"),
        Temp_K = safe_inp("Temp")
      )
    )
    
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
          "Generated by ViaBind: ITCsimfit on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
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
    
    sheet_list <- export_bridge_order_sheets(sheet_list)

    list(
      sheets = sheet_list,
      integration_rev = int_export,
      meta_rev = meta_rev,
      fit_params = fit_params_df,
      simulation = sim_df
    )
  }
  
  resolve_step2_plot_source <- function() {
    export_bridge_resolve_step2_plot_source(
      manual_exp_source = values$manual_exp_source %||% "",
      imported_xlsx_filename = values$imported_xlsx_filename %||% "",
      imported_xlsx_file_path = values$imported_xlsx_file_path %||% ""
    )
  }

  resolve_step2_plot_source_path <- function() {
    path_raw <- as.character(values$imported_xlsx_file_path %||% "")[1]
    path_raw <- trimws(path_raw)
    if (!nzchar(path_raw) || startsWith(path_raw, "bridge://")) return("")
    tryCatch(normalizePath(path_raw, winslash = "/", mustWork = FALSE), error = function(e) path_raw)
  }

  publish_step2_plot_payload <- function(sim = NULL) {
    bundle <- build_fit_export_bundle(sim = sim)
    if (is.null(bundle)) return(invisible(FALSE))
    src_info <- resolve_step2_plot_source()
    source_path <- resolve_step2_plot_source_path()
    token <- next_bridge_token()
    bridge_set("step2_plot_payload", list(
      schema_version = "itcsuite.step2_plot.v1",
      created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
      token = token,
      source = src_info$source %||% "bridge",
      source_label = src_info$source_label %||% "",
      source_path = if (nzchar(source_path)) source_path else NULL,
      sheets = bundle$sheets %||% list(),
      integration_rev = bundle$integration_rev %||% NULL,
      meta_rev = bundle$meta_rev %||% NULL,
      fit_params = bundle$fit_params %||% data.frame(),
      simulation = bundle$simulation %||% data.frame()
    ))
    invisible(TRUE)
  }
  
  observeEvent(input$data_to_plot, {
    click_id <- suppressWarnings(as.integer(input$data_to_plot)[1])
    if (!is.finite(click_id) || click_id <= 0L) return()
    sim <- tryCatch(sim_results(), error = function(e) NULL)
    ok <- isTRUE(publish_step2_plot_payload(sim = sim))
    if (!ok) {
      showNotification(tr("no_data_step3", lang()), type = "warning", duration = 3)
      return()
    }
    tryCatch({
      updateTabsetPanel(session, "main_tabs", selected = "step3")
    }, error = function(e) NULL)
    showNotification(tr("data_sent_step3", lang()), type = "message", duration = 2)
  }, ignoreInit = TRUE)
  
  # --- 6. 导出拟合数据（多 sheet xlsx，保留 ITCprocessor 原始数据 + 拟合结果）---
  output$simfit_downloadData <- downloadHandler(
    filename = function() {
      base <- values$imported_xlsx_base_name
      if (is.null(base) || length(base) == 0 || base == "") base <- "ITC_Fit_Data"
      fname <- paste0(base, "_fitted_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx")
      last_step2_export_fitted_name(fname)
      fname
    },
    content = function(file) {
      bundle <- build_fit_export_bundle()
      if (is.null(bundle)) {
        showNotification(tr("export_error_no_data", lang()), type = "error", duration = 5)
        writexl::write_xlsx(list(error = data.frame(Note = tr("export_error_no_data_note", lang()))), path = file)
        return()
      }
      writexl::write_xlsx(bundle$sheets, path = file)

      export_name <- as.character(last_step2_export_fitted_name() %||% "")[1]
      if (!nzchar(trimws(export_name))) export_name <- basename(file)
      export_path <- normalize_step2_export_path(file)
      home_add_recent_export(
        list(
          display_name = export_name,
          file_name = export_name,
          source_step = "step2",
          target_step = "step2",
          export_type = "xlsx",
          source_path = export_path,
          artifact_path = export_path,
          source_path_kind = "artifact"
        )
      )
    }
  )
