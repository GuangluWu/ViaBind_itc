  # ============================================================================
  # [i18n] UI 动态文本渲染
  # ============================================================================
  
  # 左栏 - 参数快照
  output$download_data_button <- renderUI({
    downloadButton("simfit_downloadData", tr("btn_export_fit_data", lang()), class = "btn-success btn-sm btn-block")
  })

  # [新增] 模拟→实验按钮
  output$sim_to_exp_button <- renderUI({
    actionButton("sim_to_exp", 
                 label = tr("btn_sim_to_exp", lang()), 
                 class = "btn-info btn-sm")
  })

  output$rm_exp_button <- renderUI({
    actionButton("rm_exp", tr("btn_rm_exp", lang()), class = "btn-default btn-sm")
  })
  
  output$section_param_snapshot_title <- renderUI({
    paste0("5. ", tr("section_param_snapshot", lang()))
  })
  
  output$snap_name_input <- renderUI({
    textInput("snap_name", NULL, placeholder = tr("snapshot_name_placeholder", lang()), width="100%")
  })
  
  output$save_params_button <- renderUI({
    actionButton("save_params", tr("btn_save_snapshot", lang()), class = "btn-success btn-block")
  })
  
  output$delete_params_button <- renderUI({
    actionButton("delete_selected_params", tr("btn_delete_selected", lang()), class = "btn-danger btn-block")
  })
  
  output$export_params_button <- renderUI({
    tagList(
      actionButton("export_params_trigger", tr("btn_export_params", lang()), class = "btn-success btn-block"),
      tags$div(
        style = "position:absolute; left:-9999px; top:auto; width:1px; height:1px; overflow:hidden;",
        downloadButton("export_params", label = "hidden", class = "btn-default btn-xs")
      )
    )
  })
  
  output$import_params_file_input <- renderUI({
    fileInput("import_params_file", NULL, buttonLabel = tr("btn_import_params", lang()), accept = ".xlsx", multiple = FALSE)
  })
  
  # 中栏 - 路径构建
  output$section_path_build_title <- renderUI({
    HTML(paste0(
      '<span style="font-weight:bold; font-size:1.1em; margin-right:10px;">1. ',
      tr("section_path_build", lang()),
      '</span>'
    ))
  })
  
  output$section_path_build_desc <- renderUI({
    tr("section_path_build_desc", lang())
  })
  
  output$model_header_model <- renderUI({ tr("model_header_model", lang()) })
  output$model_header_param <- renderUI({ tr("model_header_param", lang()) })
  output$model_header_stoich <- renderUI({ tr("model_header_stoich", lang()) })
  output$model_header_desc <- renderUI({ tr("model_header_desc", lang()) })
  
  # 模型说明翻译
  output$model_base_note <- renderUI({ tr("model_base", lang()) })
  
  # 动态生成路径选择 checkboxGroupInput（支持翻译）
  output$active_paths_checkbox <- renderUI({
    # [修复] 使用 isolate() 读取当前选择状态，防止循环依赖
    current_selection <- isolate({
      if(!is.null(input$active_paths)) input$active_paths else character(0)
    })
    
    checkboxGroupInput("active_paths", NULL, 
                      choiceNames = list(
                        HTML(paste0("<div class='model-row'><span class='col-mod'>M+G=D</span><span class='col-par'>K2 H2</span><span class='col-sto'>H\u2081G\u2082</span><span class='col-dsc'>", tr("model_stepwise", lang()), "</span></div>")),
                        HTML(paste0("<div class='model-row'><span class='col-mod'>M+M=T</span><span class='col-par'>K3 H3</span><span class='col-sto'>H\u2082G\u2082</span><span class='col-dsc'>", tr("model_dimer", lang()), "</span></div>")),
                        HTML(paste0("<div class='model-row'><span class='col-mod'>M+H=B</span><span class='col-par'>K4 H4</span><span class='col-sto'>H\u2082G\u2081</span><span class='col-dsc'>", tr("model_reverse", lang()), "</span></div>")),
                        HTML(paste0("<div class='model-row'><span class='col-mod'>M+D=F</span><span class='col-par'>K5 H5</span><span class='col-sto'>H\u2082G\u2083</span><span class='col-dsc'>", tr("model_oligomer", lang()), "</span></div>")),
                        HTML(paste0("<div class='model-row'><span class='col-mod'>M=U</span><span class='col-par'>K6 H6</span><span class='col-sto'>H\u2081G\u2081</span><span class='col-dsc'>", tr("model_bending", lang()), "</span></div>"))
                      ),
                      choiceValues = c("rxn_D", "rxn_T", "rxn_B", "rxn_F", "rxn_U"),
                      selected = current_selection)
  })
  
  # 中栏 - 变量手调区
  output$section_manual_adjust_title <- renderUI({
    paste0("2. ", tr("section_manual_adjust", lang()))
  })
  
  output$section_manual_adjust_desc <- renderUI({
    tr("section_manual_adjust_desc", lang())
  })
  
  output$reset_defaults_button <- renderUI({
    actionButton("reset_defaults", tr("btn_reset_default", lang()), class = "btn-danger btn-xs")
  })
  
  output$param_correction_title <- renderUI({
    tr("param_correction", lang())
  })
  
  # [i18n] 手调区参数组标题（使 i18n_translation_table 中 param_base / param_stepwise 等生效）
  output$param_base_title     <- renderUI({ tr("param_base",     lang()) })
  output$param_stepwise_title <- renderUI({ tr("param_stepwise", lang()) })
  output$param_dimer_title    <- renderUI({ tr("param_dimer",    lang()) })
  output$param_reverse_title  <- renderUI({ tr("param_reverse",  lang()) })
  output$param_oligomer_title  <- renderUI({ tr("param_oligomer", lang()) })
  output$param_bending_title   <- renderUI({ tr("param_bending",  lang()) })
  
  output$factor_H_input <- renderUI({
    # [修复] 使用 isolate() 保持当前值，避免语言切换时重置
    current_value <- isolate({
      if(!is.null(input$factor_H)) input$factor_H else DEFAULT_PARAMS$fH
    })
    numericInput("factor_H", tr("param_fH", lang()), current_value, step=0.01, width="100%")
  })
  
  output$factor_G_input <- renderUI({
    # [修复] 使用 isolate() 保持当前值，避免语言切换时重置
    current_value <- isolate({
      if(!is.null(input$factor_G)) input$factor_G else DEFAULT_PARAMS$fG
    })
    numericInput("factor_G", tr("param_fG", lang()), current_value, step=0.01, width="100%")
  })
  
  output$V_init_input <- renderUI({
    # [修复] 使用 isolate() 保持当前值，避免语言切换时重置
    current_value <- isolate({
      if(!is.null(input$V_init_val)) input$V_init_val else DEFAULT_PARAMS$V_init
    })
    numericInput("V_init_val", tr("param_V_init", lang()), current_value, step=0.1, width="100%")
  })
  
  output$heat_offset_slider <- renderUI({
    # [修复] 使用 isolate() 保持当前值，避免语言切换时重置
    current_value <- isolate({
      if(!is.null(input$heat_offset)) input$heat_offset else DEFAULT_PARAMS$Offset
    })
    sliderInput("heat_offset", tr("param_heat_offset", lang()), -1500, 1500, current_value, 10, ticks=FALSE)
  })
  
  # 右栏 - 实验数据
  output$section_exp_data_title <- renderUI({
    paste0("3. ", tr("section_exp_data", lang()))
  })
  
  output$section_exp_data_desc <- renderUI({
    NULL
  })
  
  output$exp_file_input <- renderUI({
    input$sim_to_exp  # 依赖 Sim->Exp 按钮，点击时重置文件输入框
    input$rm_exp      # 依赖 rm Exp 按钮，清空实验数据后可重新导入同名文件
    if (isTRUE(desktop_open_file_enabled())) {
      return(
        actionButton(
          "desktop_pick_exp_file",
          label = tr("btn_import_data", lang()),
          class = "btn-info btn-sm btn-block"
        )
      )
    }
    fileInput("exp_file", label=NULL, buttonLabel = tr("btn_import_data", lang()), accept = ".xlsx")
  })

  # 导入后显示文件名（参考 ITCgraph 的 data_summary_ui）
  output$exp_file_summary_ui <- renderUI({
    fn <- values$imported_xlsx_filename
    if (is.null(fn) || fn == "") {
      return(div(class = "data-summary", style = "color: #888; font-size: 0.85em; margin-top: 4px;", em(tr("no_data", lang()))))
    }
    div(class = "data-summary", style = "margin-top: 4px; font-size: 0.9em;",
      tags$b(tr("file_label", lang()), ": "), fn
    )
  })

  # Expt Data input labels are now static English in ui.R.
  observeEvent(lang(), {
    current_lang <- lang()
    updateActionButton(session, "data_to_plot", label = tr("btn_data_to_plot", current_lang))
    updateNumericInput(session, "H_cell_0", label = tr("exp_H_cell", current_lang))
    updateNumericInput(session, "G_syringe", label = tr("exp_G_syringe", current_lang))
    updateNumericInput(session, "V_cell", label = tr("exp_V_cell", current_lang))
    updateNumericInput(session, "V_inj", label = tr("exp_V_inj", current_lang))
    updateNumericInput(session, "n_inj", label = tr("exp_n_inj", current_lang))
    updateNumericInput(session, "V_pre", label = tr("exp_V_pre", current_lang))
    updateNumericInput(session, "Temp", label = tr("exp_temp", current_lang))
  }, ignoreInit = FALSE)
  
  # 右栏 - 拟合
  output$section_fitting_title <- renderUI({
    paste0("4. ", tr("section_fitting", lang()))
  })
  
  # [修复] 只渲染标签，滑条在 UI 中静态定义，避免拖动时重建
  output$fit_data_range_slider_label <- renderUI({
    exp_df <- tryCatch(exp_data_processed(), error=function(e) NULL)
    if(!is.null(exp_df) && nrow(exp_df) > 0) {
      tags$label(tr("fit_range_inj", lang()), style="display: block; margin-bottom: 5px;")
    } else {
      tags$label(tr("fit_range", lang()), style="display: block; margin-bottom: 5px;")
    }
  })
  
  output$fit_1_step_button <- renderUI({
    actionButton("fit_1_step", strong(tr("btn_fit_1", lang())), class = "btn-warning btn-sm", style="flex:1 0 20%; min-width:80px;")
  })
  
  output$fit_10_step_button <- renderUI({
    actionButton("fit_10_step", strong(tr("btn_fit_10", lang())), class = "btn-warning btn-sm", style="flex:1 0 20%; min-width:80px;")
  })
  
  output$fit_full_button <- renderUI({
    actionButton("fit_full", strong(tr("btn_fit_100", lang())), class = "btn-danger btn-sm", style="flex:1 0 25%; min-width:100px;")
  })
  
  output$fit_global_button <- renderUI({
    actionButton("fit_global", strong(tr("btn_fit_global", lang())), class = "btn-primary btn-sm", style="flex:1 0 25%; min-width:100px;")
  })

  output$report_button <- renderUI({
    actionButton("report_btn", strong(tr("btn_report", lang())), class = "btn-info btn-sm", style="flex:1 0 20%; min-width:80px; background-color: #9b59b6; border-color: #8e44ad; color: white;")
  })

  build_fitting_report_text <- function() {
    l <- lang()
    txt <- function(zh, en) if (identical(l, "zh")) zh else en

    safe_num <- function(id, default = NA_real_) {
      val <- suppressWarnings(as.numeric(input[[id]]))
      if (length(val) < 1 || !is.finite(val[1])) return(default)
      val[1]
    }

    file_name <- values$imported_xlsx_filename
    if (is.null(file_name) || !nzchar(file_name)) file_name <- tr("unknown_file", lang())

    h_cell <- safe_num("H_cell_0", UI_DEFAULTS$conc_cell_default)
    v_cell <- safe_num("V_cell", UI_DEFAULTS$v_cell_default)
    g_syr <- safe_num("G_syringe", UI_DEFAULTS$conc_syringe_default)
    v_inj <- safe_num("V_inj", UI_DEFAULTS$v_inj_default * 1000)
    n_inj <- safe_num("n_inj", UI_DEFAULTS$n_inj_default)
    v_pre <- safe_num("V_pre", UI_DEFAULTS$v_pre_default)
    temp_k <- safe_num("Temp", UI_DEFAULTS$temp_default)
    f_h <- safe_num("factor_H", DEFAULT_PARAMS$factor_H)
    f_g <- safe_num("factor_G", DEFAULT_PARAMS$factor_G)
    v_init <- safe_num("V_init_val", v_pre)
    heat_offset <- safe_num("heat_offset", DEFAULT_PARAMS$offset)

    t_info <- paste0(
      txt("数据名称: ", "Data Name: "), file_name, "\n",
      txt("滴定信息:\n", "Titration Info:\n"),
      "  ", txt("样品池", "Cell"), ": [H] = ", h_cell, " mM, V = ", v_cell, " mL\n",
      "  ", txt("注射器", "Syringe"), ": [G] = ", g_syr, " mM\n",
      "  ", txt("注射参数", "Injection"), ": V_inj = ", v_inj, " uL, ", txt("针数", "Count"), " = ", n_inj, "\n",
      "  ", txt("记录首针体积", "Recorded 1st Inj Vol"), ": V_pre = ", v_pre, " uL\n",
      "  ", txt("温度", "Temperature"), ": ", temp_k, " K\n"
    )

    get_se_raw <- function(param) {
      if (!is.null(values$error_analysis) && is.data.frame(values$error_analysis) &&
          param %in% values$error_analysis$Parameter && "SE" %in% names(values$error_analysis)) {
        se_vec <- suppressWarnings(as.numeric(values$error_analysis$SE[values$error_analysis$Parameter == param]))
        se_vec <- se_vec[is.finite(se_vec)]
        if (length(se_vec) > 0) return(se_vec[[1]])
      }
      NA_real_
    }

    fmt_se <- function(se_val, unit = "") {
      if (is.na(se_val) || !is.finite(se_val)) return("")
      sprintf(" \u00B1 %.3f%s", se_val, unit)
    }

    p_info <- paste0(
      txt("全局参数:\n", "Global Parameters:\n"),
      "  ", txt("[H] 校正因子", "Correction Factor of [H]"), ": fH = ", f_h, fmt_se(get_se_raw("fH")), "\n",
      "  ", txt("[G] 校正因子", "Correction Factor of [G]"), ": fG = ", f_g, fmt_se(get_se_raw("fG")), "\n",
      "  ", txt("拟合首针体积", "Fitted 1st Inj Vol"), ": V_init = ", v_init, fmt_se(get_se_raw("V_init")), " uL\n",
      "  ", txt("基线热扣除", "Baseline Heat Subtraction"), ": Offset = ", heat_offset, fmt_se(get_se_raw("Offset")), " cal/mol\n"
    )

    R_const <- 1.9872
    T_val <- temp_k
    if (!is.finite(T_val)) T_val <- 298.15

    calc_thermo <- function(logK, dH_cal, name, logK_se, dH_se_cal) {
      if (!is.finite(logK) || !is.finite(dH_cal)) {
        return(paste0(
          txt("路径", "Path"), ": ", name, "\n",
          "  logK = NA\n",
          "  dH = NA kcal/mol\n",
          "  dG = NA kcal/mol\n",
          "  TdS = NA kcal/mol\n\n"
        ))
      }
      K <- 10^logK
      dH_kcal <- dH_cal / 1000
      dH_se_kcal <- if (is.na(dH_se_cal)) NA else dH_se_cal / 1000
      dG_kcal <- -R_const * T_val * log(K) / 1000
      dG_se_kcal <- if (is.na(logK_se)) NA else abs(R_const * T_val * log(10) / 1000) * logK_se
      TdS_kcal <- dH_kcal - dG_kcal
      TdS_se_kcal <- if (is.na(dH_se_kcal) || is.na(dG_se_kcal)) NA else sqrt(dH_se_kcal^2 + dG_se_kcal^2)

      paste0(
        txt("路径", "Path"), ": ", name, "\n",
        "  logK = ", logK, fmt_se(logK_se), "\n",
        "  dH = ", dH_kcal, fmt_se(dH_se_kcal), " kcal/mol\n",
        "  dG = ", sprintf("%.2f", dG_kcal), fmt_se(dG_se_kcal), " kcal/mol\n",
        "  TdS = ", sprintf("%.2f", TdS_kcal), fmt_se(TdS_se_kcal), " kcal/mol\n",
        "\n"
      )
    }

    path_info <- txt("热力学分析:\n", "Thermodynamic Analysis:\n")
    path_info <- paste0(path_info, calc_thermo(
      safe_num("logK1", DEFAULT_PARAMS$logK),
      safe_num("H1", DEFAULT_PARAMS$H),
      "H + G <=> M",
      get_se_raw("logK1"),
      get_se_raw("H1")
    ))

    active <- input$active_paths
    if (is.null(active)) active <- character(0)
    active <- as.character(active)

    if ("rxn_D" %in% active) path_info <- paste0(path_info, calc_thermo(
      safe_num("logK2", DEFAULT_PARAMS$logK),
      safe_num("H2", DEFAULT_PARAMS$H),
      "M + G <=> D (Stepwise)",
      get_se_raw("logK2"),
      get_se_raw("H2")
    ))
    if ("rxn_T" %in% active) path_info <- paste0(path_info, calc_thermo(
      safe_num("logK3", DEFAULT_PARAMS$logK),
      safe_num("H3", DEFAULT_PARAMS$H),
      "M + M <=> T (Dimer)",
      get_se_raw("logK3"),
      get_se_raw("H3")
    ))
    if ("rxn_B" %in% active) path_info <- paste0(path_info, calc_thermo(
      safe_num("logK4", DEFAULT_PARAMS$logK),
      safe_num("H4", DEFAULT_PARAMS$H),
      "M + H <=> B (Reverse)",
      get_se_raw("logK4"),
      get_se_raw("H4")
    ))
    if ("rxn_F" %in% active) path_info <- paste0(path_info, calc_thermo(
      safe_num("logK5", DEFAULT_PARAMS$logK),
      safe_num("H5", DEFAULT_PARAMS$H),
      "M + D <=> F (Oligomer)",
      get_se_raw("logK5"),
      get_se_raw("H5")
    ))
    if ("rxn_U" %in% active) path_info <- paste0(path_info, calc_thermo(
      safe_num("logK6", DEFAULT_PARAMS$logK),
      safe_num("H6", DEFAULT_PARAMS$H),
      "M <=> U (Bending)",
      get_se_raw("logK6"),
      get_se_raw("H6")
    ))

    report_text <- paste0(
      "==================================================\n",
      txt("                    拟合报告                    \n", "                  FITTING REPORT                  \n"),
      "==================================================\n\n",
      t_info, "\n",
      p_info, "\n",
      path_info,
      "==================================================\n",
      txt("由 ViaBind: ITCsimfit 生成于 ", "Generated by ViaBind: ITCsimfit on "), format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n"
    )

    if (length(report_text) < 1 || is.null(report_text) || !nzchar(report_text[1])) {
      report_text <- paste0(
        txt("拟合报告\n", "FITTING REPORT\n"),
        txt("由 ViaBind: ITCsimfit 生成于 ", "Generated by ViaBind: ITCsimfit on "), format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
        txt("数据名称: ", "Data Name: "), file_name, "\n"
      )
    }
    as.character(report_text[1])
  }

  observeEvent(input$report_btn, {
    full_report <- build_fitting_report_text()
    
    # Store in values for download handler
    values$current_report <- full_report
    
    js_escape <- function(x) {
      x <- as.character(x %||% "")[1]
      x <- gsub("\\\\", "\\\\\\\\", x)
      x <- gsub("'", "\\\\'", x, fixed = TRUE)
      x <- gsub("\n", "\\\\n", x, fixed = TRUE)
      x
    }
    copy_success <- js_escape(tr("report_copy_success", lang()))
    copy_failed <- js_escape(tr("report_copy_failed", lang()))

    # Show Modal
    showModal(modalDialog(
      title = tr("report_title", lang()),
      textAreaInput("report_content", NULL, value = full_report, rows = 15, width = "100%", resize = "vertical"),
      footer = tagList(
        downloadButton("save_report_txt", tr("report_save_txt", lang()), class = "btn-success"),
        # JS Copy Button
        tags$button(
          id = "copy_report_btn",
          class = "btn btn-info action-button",
          onclick = paste0("
            var copyText = document.getElementById('report_content');
            copyText.select();
            copyText.setSelectionRange(0, 99999);
            if (navigator.clipboard && navigator.clipboard.writeText) {
                navigator.clipboard.writeText(copyText.value).then(function() {
                    alert('", copy_success, "');
                }, function(err) {
                    try { document.execCommand('copy'); alert('", copy_success, "'); }
                    catch (e) { alert('", copy_failed, ": ' + err); }
                });
            } else {
                try { document.execCommand('copy'); alert('", copy_success, "'); }
                catch (e) { alert('", copy_failed, "'); }
            }
          "),
          icon("copy"), tr("report_copy_clipboard", lang())
        ),
        modalButton(tr("report_close", lang()))
      ),
      size = "l",
      easyClose = TRUE
    ))
  })

  output$save_report_txt <- downloadHandler(
    filename = function() {
      base <- values$imported_xlsx_base_name
      if (is.null(base) || !nzchar(base)) {
        fallback_name <- values$imported_xlsx_filename
        if (!is.null(fallback_name) && nzchar(fallback_name)) {
          base <- tools::file_path_sans_ext(basename(fallback_name))
        }
      }
      if (is.null(base) || !nzchar(base)) base <- "ITC"
      fname <- paste0(base, "_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
      values$last_report_export_name <- fname
      fname
    },
    content = function(file) {
      report_text <- values$current_report
      if (is.null(report_text) || !nzchar(as.character(report_text %||% "")[1])) {
        report_text <- build_fitting_report_text()
      }
      writeLines(report_text, file)

      export_name <- as.character(values$last_report_export_name %||% "")[1]
      if (!nzchar(trimws(export_name))) export_name <- basename(file)
      export_path <- tryCatch(normalizePath(file, winslash = "/", mustWork = FALSE), error = function(e) as.character(file)[1])
      import_path_raw <- as.character(values$imported_xlsx_file_path %||% "")[1]
      import_path_raw <- trimws(import_path_raw)
      import_path <- ""
      if (nzchar(import_path_raw) && !startsWith(import_path_raw, "bridge://")) {
        import_path <- tryCatch(normalizePath(import_path_raw, winslash = "/", mustWork = FALSE), error = function(e) import_path_raw)
      }
      if (!nzchar(trimws(import_path))) return(invisible(NULL))
      home_add_recent_export(
        list(
          display_name = export_name,
          file_name = export_name,
          source_step = "step2",
          target_step = "step2",
          export_type = "txt",
          source_path = import_path,
          artifact_path = export_path,
          source_path_kind = "import"
        )
      )
    }
  )


    

    

  
  output$fit_rss_label <- renderUI({
    tr("fit_rss", lang())
  })
  
  # 误差分析部分
  output$error_analysis_section <- renderUI({
    # [修复] 使用 isolate() 保持当前开关状态，避免语言切换时重置和触发循环
    current_value <- isolate({
      if(!is.null(input$enable_error_analysis)) input$enable_error_analysis else FALSE
    })
    
    tagList(
      div(class = "fit-error-head",
          h5(tr("error_analysis_title", lang()), style="font-weight: bold; margin: 0;"),
          checkboxInput("enable_error_analysis", tr("error_analysis_enable", lang()), value = current_value, width = "auto")
      ),
      div(style="margin-bottom: 8px;",
          tags$details(
            tags$summary(style="cursor: pointer; color: #2980b9; font-size: 0.8em;", 
                        tr("error_analysis_method", lang())),
            div(style="margin-top: 8px; padding: 8px; background-color: #f0f8ff; border-left: 3px solid #2980b9; font-size: 0.75em; line-height: 1.6;",
                tags$p(strong(tr("error_analysis_hessian_principle", lang())), style="margin-top: 0;"),
                tags$p("1. ", strong(tr("error_analysis_hessian_1", lang()))),
                tags$p("2. ", strong(tr("error_analysis_hessian_2", lang()))),
                tags$p("3. ", strong(tr("error_analysis_hessian_3", lang()))),
                tags$p("4. ", strong(tr("error_analysis_hessian_4", lang()))),
                tags$p(strong(tr("error_analysis_improvements", lang())), tr("error_analysis_improvements_desc", lang())),
                tags$p(strong(tr("error_analysis_applicability", lang())), 
                       tags$ul(
                         tags$li(tr("error_analysis_applicability_1", lang())),
                         tags$li(tr("error_analysis_applicability_2", lang())),
                         tags$li(tr("error_analysis_applicability_3", lang())),
                         tags$li(tr("error_analysis_applicability_4", lang()))
                       ))
            )
          )
      ),
      conditionalPanel(
        condition = "input.enable_error_analysis == true",
        uiOutput("error_analysis_reliability"),
        div(style="border: 1px solid #ddd; padding: 5px; background-color: #f9f9f9; margin-top: 8px;",
            DTOutput("error_analysis_table", width = "100%")
        )
      ),
      conditionalPanel(
        condition = "input.enable_error_analysis == false",
        p(style="color: #999; font-size:0.85em; font-style: italic; padding: 10px; text-align: center;",
          tr("error_analysis_disabled", lang()))
      )
    )
  })
  
