# ==============================================================================
# 4. Shiny Server
# ==============================================================================
server <- function(input, output, session) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  session_bridge <- tryCatch({
    b <- session$userData$itcsuite_bridge
    if (is.null(b) || !is.list(b)) NULL else b
  }, error = function(e) NULL)
  bridge_store_name <- ".ITCSUITE_BRIDGE_STORE"
  bridge_session_key <- tryCatch({
    key <- as.character(session$token)
    if (length(key) == 0 || !nzchar(key[1])) NA_character_ else key[1]
  }, error = function(e) NA_character_)
  if (is.na(bridge_session_key) || !nzchar(bridge_session_key)) {
    bridge_session_key <- paste0("session_", format(Sys.time(), "%Y%m%d%H%M%OS6"))
  }
  bridge_last_step1_token <- reactiveVal(NA_real_)
  
  bridge_store_get_all <- function() {
    x <- get0(bridge_store_name, envir = .GlobalEnv, inherits = FALSE, ifnotfound = NULL)
    if (is.null(x) || !is.list(x)) return(list())
    x
  }
  
  bridge_store_put_all <- function(x) {
    if (is.null(x) || !is.list(x)) x <- list()
    assign(bridge_store_name, x, envir = .GlobalEnv)
    invisible(NULL)
  }
  
  bridge_get <- function(channel) {
    ch <- if (!is.null(session_bridge)) session_bridge[[channel]] else NULL
    if (is.function(ch)) return(ch())
    store <- bridge_store_get_all()
    entry <- store[[bridge_session_key]]
    if (is.null(entry) || !is.list(entry)) return(NULL)
    entry[[channel]]
  }
  
  bridge_set <- function(channel, payload) {
    ch <- if (!is.null(session_bridge)) session_bridge[[channel]] else NULL
    if (is.function(ch)) {
      ch(payload)
      return(invisible(NULL))
    }
    store <- bridge_store_get_all()
    entry <- store[[bridge_session_key]]
    if (is.null(entry) || !is.list(entry)) entry <- list()
    if (is.null(payload)) {
      entry[[channel]] <- NULL
    } else {
      entry[[channel]] <- payload
      entry$updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    }
    store[[bridge_session_key]] <- entry
    bridge_store_put_all(store)
    invisible(NULL)
  }
  
  # ============================================================================
  # [i18n] 确保翻译函数可用
  # ============================================================================
  # 如果 tr 函数不存在，重新加载 i18n 模块
  if (!exists("tr", envir = .GlobalEnv)) {
    tryCatch({
      source("R/i18n.R", local = FALSE)
    }, error = function(e) {
      # 如果加载失败，创建一个简单的后备函数
      warning("Failed to load i18n.R: ", e$message)
      tr <<- function(key, lang = "zh") { return(key) }
      trf <<- function(key, lang = "zh", ...) { return(key) }
    })
  }
  
  # 再次检查，确保函数存在
  if (!exists("tr", envir = .GlobalEnv)) {
    # 最后的后备：定义简单的函数
    assign("tr", function(key, lang = "zh") { return(key) }, envir = .GlobalEnv)
    assign("trf", function(key, lang = "zh", ...) { return(key) }, envir = .GlobalEnv)
  }
  
  # ============================================================================
  # [i18n] 语言状态管理
  # ============================================================================
  current_lang <- reactiveVal("en")  # 默认英文（与 processor 一致）
  
  # [新增] 语言切换进行中标志，用于防止UI重建时的循环触发
  lang_switching <- reactiveVal(FALSE)
  
  # 语言切换响应式函数
  lang <- reactive({
    lang_val <- current_lang()
    # 确保返回值不为空
    if(is.null(lang_val) || length(lang_val) == 0 || !lang_val %in% c("zh", "en")) {
      return("en")
    }
    return(lang_val)
  })
  
  # 安全获取语言值的辅助函数（用于非reactive上下文）
  get_lang_safe <- function() {
    tryCatch({
      lang_val <- current_lang()
      if(is.null(lang_val) || length(lang_val) == 0 || !lang_val %in% c("zh", "en")) {
        return("en")
      }
      return(lang_val)
    }, error = function(e) {
      return("en")
    })
  }
  
  # 监听语言切换按钮
  observeEvent(input$lang_switch, {
    # [修复] 设置切换标志，防止重复触发
    if(lang_switching()) {
      return()  # 如果正在切换中，忽略新的点击
    }
    
    lang_switching(TRUE)
    new_lang <- if(current_lang() == "zh") "en" else "zh"
    current_lang(new_lang)
    
    # [修复] 使用 later 包延迟重置标志（如果没有 later 包，使用 Sys.sleep）
    if(requireNamespace("later", quietly = TRUE)) {
      later::later(function() { lang_switching(FALSE) }, delay = 0.5)
    } else {
      # 备用方案：使用简单的异步重置（在下次 reactive flush 后）
      # 创建一个单次触发的 observer
      local({
        reset_observer <- NULL
        reset_observer <<- observe({
          invalidateLater(500)
          isolate({
            lang_switching(FALSE)
            reset_observer$destroy()  # 自毁
          })
        }, priority = -100)  # 低优先级，确保在其他更新之后执行
      })
    }
  }, ignoreInit = TRUE)
  
  # 渲染应用标题（动态）
  output$app_title_dynamic <- renderUI({
    current_lang_val <- tryCatch(lang(), error = function(e) "en")
    if(is.null(current_lang_val) || length(current_lang_val) == 0) current_lang_val <- "en"
    tr("app_subtitle", current_lang_val)
  })
  
  # 渲染语言切换按钮（仿照 processor：小国旗 + 目标语言名）
  output$lang_switch_button <- renderUI({
    current <- lang()
    btn_label <- if(current == "en") "\U0001F1E8\U0001F1F3 \u7b80\u4f53\u4e2d\u6587" else "\U0001F1EC\U0001F1E7 English"
    actionButton("lang_switch", btn_label,
                 class = "btn-default btn-xs",
                 style = "min-width: 90px;")
  })
  
  # ============================================================================
  # [i18n] UI 动态文本渲染
  # ============================================================================
  
  # 左栏 - 参数快照
  output$download_data_button <- renderUI({
    downloadButton("simfit_downloadData", tr("btn_export_fit_data", lang()), class = "btn-success btn-xs")
  })
  
  # [新增] 模拟→实验按钮
  output$sim_to_exp_button <- renderUI({
    actionButton("sim_to_exp", 
                 label = tr("btn_sim_to_exp", lang()), 
                 class = "btn-info btn-sm")
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
  
  output$clear_params_button <- renderUI({
    actionButton("clear_params", tr("btn_clear_all", lang()), class = "btn-danger btn-block")
  })
  
  output$export_params_button <- renderUI({
    downloadButton("export_params", tr("btn_export_params", lang()), class="btn-success btn-block")
  })
  
  output$import_params_file_input <- renderUI({
    fileInput("import_params_file", NULL, buttonLabel = tr("btn_import_params", lang()), accept = ".xlsx", multiple = FALSE)
  })
  
  # 中栏 - 路径构建
  output$section_path_build_title <- renderUI({
    HTML(paste0(
      '<span style="font-weight:bold; font-size:1.1em; margin-right:10px;">1. ', 
      tr("section_path_build", lang()), 
      '</span>',
      '<span style="color:gray; font-size:0.8em;">', 
      tr("section_path_build_desc", lang()), 
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
    tr("section_exp_data_desc", lang())
  })
  
  output$exp_file_input <- renderUI({
    input$sim_to_exp  # 依赖 Sim->Exp 按钮，点击时重置文件输入框
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

  # [i18n] 实验数据区 numericInput 标签随语言更新（使 i18n_translation_table 中 exp_* 生效）
  observe({
    l <- lang()
    updateNumericInput(session, "H_cell_0",  label = tr("exp_H_cell",   l))
    updateNumericInput(session, "G_syringe",  label = tr("exp_G_syringe", l))
    updateNumericInput(session, "V_cell",    label = tr("exp_V_cell",   l))
    updateNumericInput(session, "V_inj",     label = tr("exp_V_inj",    l))
    updateNumericInput(session, "n_inj",     label = tr("exp_n_inj",    l))
    updateNumericInput(session, "V_pre",    label = tr("exp_V_pre",    l))
    updateNumericInput(session, "Temp",     label = tr("exp_temp",     l))
  })
  
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
    lbl <- if(lang() == "zh") "生成报告" else "Report"
    actionButton("report_btn", strong(lbl), class = "btn-info btn-sm", style="flex:1 0 20%; min-width:80px; background-color: #9b59b6; border-color: #8e44ad; color: white;")
  })

  observeEvent(input$report_btn, {
    # 1. Gather Data
    file_name <- values$imported_xlsx_filename
    if(is.null(file_name)) file_name <- "Unknown File"
    
    # Titration Info
    t_info <- paste0(
      "Data Name: ", file_name, "\n",
      "Titration Info:\n",
      "  Cell: [H] = ", input$H_cell_0, " mM, V = ", input$V_cell, " mL\n",
      "  Syringe: [G] = ", input$G_syringe, " mM\n",
      "  Injection: V_inj = ", input$V_inj, " uL, Count = ", input$n_inj, "\n",
      "  Recorded 1st Inj Vol: V_pre = ", input$V_pre, " uL\n",
      "  Temperature: ", input$Temp, " K\n"
    )
    
    # Fitted Parameters
    # Get SE if available (raw value)
    get_se_raw <- function(param) {
      if(!is.null(values$error_analysis) && param %in% values$error_analysis$Parameter) {
         se <- values$error_analysis$SE[values$error_analysis$Parameter == param]
         if(is.finite(se)) return(se)
      }
      return(NA)
    }

    # Format SE string
    fmt_se <- function(se_val, unit="") {
        if(is.na(se_val)) return("")
        return(sprintf(" \u00B1 %.3f%s", se_val, unit))
    }
    
    p_info <- paste0(
      "Global Parameters:\n",
      "  Correction Factor of [H]: fH = ", input$factor_H, fmt_se(get_se_raw("fH")), "\n",
      "  Correction Factor of [G]: fG = ", input$factor_G, fmt_se(get_se_raw("fG")), "\n",
      "  Fitted 1st Inj Vol: V_init = ", input$V_init_val, fmt_se(get_se_raw("V_init")), " uL\n",
      "  Baseline Heat Subtraction: Offset = ", input$heat_offset, fmt_se(get_se_raw("Offset")), " cal/mol\n"
    )
    
    # Active Paths
    # Constants
    R_const <- 1.9872 # cal/(mol K)
    T_val <- input$Temp
    if(is.null(T_val) || is.na(T_val)) T_val <- 298.15
    
    path_info <- "Thermodynamic Analysis:\n"
    
    # Helper to calc thermodynamic params
    calc_thermo <- function(logK, dH_cal, name, logK_se, dH_se_cal) {
      # 1. logK
      K <- 10^logK
      
      # 2. dH (convert to kcal/mol)
      dH_kcal <- dH_cal / 1000
      dH_se_kcal <- if(is.na(dH_se_cal)) NA else dH_se_cal / 1000
      
      # 3. dG (kcal/mol) = -RT ln K / 1000
      # dG_se = |RT ln(10) / 1000| * logK_se
      dG_kcal <- -R_const * T_val * log(K) / 1000
      dG_se_kcal <- if(is.na(logK_se)) NA else abs(R_const * T_val * log(10) / 1000) * logK_se
      
      # 4. TdS (kcal/mol) = dH - dG
      # TdS_se = sqrt(dH_se^2 + dG_se^2) (assuming independence)
      TdS_kcal <- dH_kcal - dG_kcal
      TdS_se_kcal <- if(is.na(dH_se_kcal) || is.na(dG_se_kcal)) NA else sqrt(dH_se_kcal^2 + dG_se_kcal^2)
      
      res <- paste0(
        "Path: ", name, "\n",
        "  logK = ", logK, fmt_se(logK_se), "\n",
        "  dH = ", dH_kcal, " kcal/mol", fmt_se(dH_se_kcal), "\n",
        "  dG = ", sprintf("%.2f", dG_kcal), " kcal/mol", fmt_se(dG_se_kcal), "\n",
        "  TdS = ", sprintf("%.2f", TdS_kcal), " kcal/mol", fmt_se(TdS_se_kcal), "\n",
        "\n"
      )
      return(res)
    }
    
    # M Path (Always active)
    path_info <- paste0(path_info, calc_thermo(input$logK1, input$H1, "H + G <=> M", get_se_raw("logK1"), get_se_raw("H1")))
    
    # Other paths
    active <- if(is.null(input$active_paths)) character(0) else input$active_paths
    if("rxn_D" %in% active) path_info <- paste0(path_info, calc_thermo(input$logK2, input$H2, "M + G <=> D (Stepwise)", get_se_raw("logK2"), get_se_raw("H2")))
    if("rxn_T" %in% active) path_info <- paste0(path_info, calc_thermo(input$logK3, input$H3, "M + M <=> T (Dimer)", get_se_raw("logK3"), get_se_raw("H3")))
    if("rxn_B" %in% active) path_info <- paste0(path_info, calc_thermo(input$logK4, input$H4, "M + H <=> B (Reverse)", get_se_raw("logK4"), get_se_raw("H4")))
    if("rxn_F" %in% active) path_info <- paste0(path_info, calc_thermo(input$logK5, input$H5, "M + D <=> F (Oligomer)", get_se_raw("logK5"), get_se_raw("H5")))
    if("rxn_U" %in% active) path_info <- paste0(path_info, calc_thermo(input$logK6, input$H6, "M <=> U (Bending)", get_se_raw("logK6"), get_se_raw("H6")))
    
    full_report <- paste0(
      "==================================================\n",
      "                  FITTING REPORT                  \n",
      "==================================================\n\n",
      t_info, "\n",
      p_info, "\n",
      path_info,
      "==================================================\n",
      "Generated by ITCsimfit on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n"
    )
    
    # Store in values for download handler
    values$current_report <- full_report
    
    # Show Modal
    showModal(modalDialog(
      title = if(lang() == "zh") "拟合结果报告" else "Fitting Report",
      textAreaInput("report_content", NULL, value = full_report, rows = 15, width = "100%", resize = "vertical"),
      footer = tagList(
        downloadButton("save_report_txt", if(lang() == "zh") "另存为 TXT" else "Save as TXT", class = "btn-success"),
        # JS Copy Button
        tags$button(
          id = "copy_report_btn",
          class = "btn btn-info action-button",
          onclick = "
            var copyText = document.getElementById('report_content');
            copyText.select();
            copyText.setSelectionRange(0, 99999);
            if (navigator.clipboard && navigator.clipboard.writeText) {
                navigator.clipboard.writeText(copyText.value).then(function() {
                    alert('Report copied to clipboard!');
                }, function(err) {
                    try { document.execCommand('copy'); alert('Report copied to clipboard!'); }
                    catch (e) { alert('Failed to copy: ' + err); }
                });
            } else {
                try { document.execCommand('copy'); alert('Report copied to clipboard!'); }
                catch (e) { alert('Failed to copy.'); }
            }
          ",
          icon("copy"), if(lang() == "zh") "复制到剪贴板" else "Copy to Clipboard"
        ),
        modalButton(if(lang() == "zh") "关闭" else "Close")
      ),
      size = "l",
      easyClose = TRUE
    ))
  })

  output$save_report_txt <- downloadHandler(
    filename = function() {
      paste("ITC_Report_", format(Sys.time(), "%Y%m%d_%H%M"), ".txt", sep="")
    },
    content = function(file) {
      writeLines(values$current_report, file)
    }
  )


    

    

  
  output$fit_rss_label <- renderUI({
    tr("fit_rss", lang())
  })
  
  # 高级拟合选项
  output$fit_advanced_options <- renderUI({
    tags$details(
      tags$summary(style="cursor: pointer; color: #2980b9; font-size: 0.9em; font-weight: bold; margin-bottom: 8px;",
                  tr("fit_advanced_options", lang())),
      div(style="margin-top: 8px; padding: 8px; background-color: #f9f9f9; border-left: 3px solid #2980b9;",
          checkboxInput("use_weighted_fitting", tr("fit_weighted", lang()), value = FALSE, width = "100%"),
          div(style="margin-left: 20px; margin-top: 4px; margin-bottom: 8px; font-size: 0.85em; color: #666;",
              tr("fit_weighted_desc", lang())),
          checkboxInput("use_robust_fitting", tr("fit_robust", lang()), value = FALSE, width = "100%"),
          div(style="margin-left: 20px; margin-top: 4px; margin-bottom: 8px; font-size: 0.85em; color: #666;",
              tr("fit_robust_desc", lang())),
          conditionalPanel(
            condition = "input.use_robust_fitting == true",
            div(style="margin-left: 20px; margin-top: 8px;",
                numericInput("huber_delta", tr("fit_huber_delta", lang()), value = NULL, min = 0, step = 0.1,
                             width = "100%"),
                div(style="font-size: 0.8em; color: #888; margin-top: 4px;",
                    tr("fit_huber_delta_desc", lang()))
            )
          )
      )
    )
  })
  
  # 误差分析部分
  output$error_analysis_section <- renderUI({
    # [修复] 使用 isolate() 保持当前开关状态，避免语言切换时重置和触发循环
    current_value <- isolate({
      if(!is.null(input$enable_error_analysis)) input$enable_error_analysis else FALSE
    })
    
    tagList(
      tags$hr(),
      div(style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px;",
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
  
  # ============================================================================
  # 原有代码继续
  # ============================================================================
  
  # 使用同步拟合模式：拟合时界面会暂时无响应，确保用户不会在拟合时进行其他操作
  
  values <- reactiveValues(is_fitting = FALSE, param_list = data.frame(), error_analysis = NULL, 
                           error_analysis_info = NULL,  # 存储误差分析可靠性信息
                           residuals_data = NULL,       # 存储残差数据（用于残差图）
                           correlation_matrix = NULL,   # 存储参数相关性矩阵
                           residual_subtab = "res1",    # 当前选中的残差子标签页
                           manual_exp_data = NULL,      # 手动导入的实验数据（模拟→实验功能）
                           manual_exp_source = NULL,    # 手动实验数据来源: sim_to_exp / step1_bridge
                           imported_xlsx_sheets = NULL, # 缓存 ITCprocessor 导出的原始 xlsx sheets
                           imported_xlsx_base_name = NULL,  # 导入文件的基名（去掉时间戳），用于导出命名
                           imported_xlsx_filename = NULL,  # 导入文件的显示用文件名（供界面展示）
                           v_pre_programmatic_update = FALSE)  # 程序更新 V_pre 时设为 TRUE，避免显示"第一针体积变更"提示
  
  # [缓存机制] 使用 reactiveVal 存储缓存的结果和键
  sim_cache_result <- reactiveVal(NULL)
  sim_cache_key <- reactiveVal(NULL)
  
  apply_meta_to_exp_inputs <- function(meta_df) {
    if (is.null(meta_df) || !all(c("parameter", "value") %in% colnames(meta_df))) return(invisible(FALSE))
    vals <- suppressWarnings(as.numeric(trimws(as.character(meta_df$value))))
    param_vals <- setNames(vals, trimws(as.character(meta_df$parameter)))
    
    if ("H_cell_0_mM" %in% names(param_vals) && !is.na(param_vals[["H_cell_0_mM"]])) {
      updateNumericInput(session, "H_cell_0", value = param_vals[["H_cell_0_mM"]])
    }
    if ("G_syringe_mM" %in% names(param_vals) && !is.na(param_vals[["G_syringe_mM"]])) {
      updateNumericInput(session, "G_syringe", value = param_vals[["G_syringe_mM"]])
    }
    if ("V_cell_mL" %in% names(param_vals) && !is.na(param_vals[["V_cell_mL"]])) {
      updateNumericInput(session, "V_cell", value = param_vals[["V_cell_mL"]])
    }
    if ("V_inj_uL" %in% names(param_vals) && !is.na(param_vals[["V_inj_uL"]])) {
      updateNumericInput(session, "V_inj", value = param_vals[["V_inj_uL"]])
    }
    if ("V_pre_uL" %in% names(param_vals) && !is.na(param_vals[["V_pre_uL"]])) {
      values$v_pre_programmatic_update <- TRUE
      updateNumericInput(session, "V_pre", value = param_vals[["V_pre_uL"]])
      updateNumericInput(session, "V_init_val", value = param_vals[["V_pre_uL"]])
    }
    if ("n_inj" %in% names(param_vals) && !is.na(param_vals[["n_inj"]])) {
      updateNumericInput(session, "n_inj", value = as.integer(param_vals[["n_inj"]]))
    }
    if ("Temp_K" %in% names(param_vals) && !is.na(param_vals[["Temp_K"]])) {
      updateNumericInput(session, "Temp", value = param_vals[["Temp_K"]])
    }
    invisible(TRUE)
  }
  
  consume_step1_payload <- function(payload) {
    if (is.null(payload) || !is.list(payload)) return(FALSE)
    is_finite_scalar <- function(x) length(x) == 1 && is.finite(x)
    
    token_vec <- suppressWarnings(as.numeric(payload$token))
    token <- if (length(token_vec) >= 1) token_vec[1] else NA_real_
    last_token <- bridge_last_step1_token()
    if (is_finite_scalar(token) && is_finite_scalar(last_token) && identical(token, last_token)) return(FALSE)
    
    bundle <- payload$bundle
    int_df <- NULL
    if (!is.null(bundle) && is.list(bundle) && is.data.frame(bundle$integration)) {
      int_df <- as.data.frame(bundle$integration)
    }
    if (is.null(int_df) && is.data.frame(payload$integration)) {
      int_df <- as.data.frame(payload$integration)
    }
    if (is.null(int_df) || nrow(int_df) == 0) return(FALSE)
    
    vinj_default <- suppressWarnings(as.numeric(input$V_inj))
    if (length(vinj_default) >= 1) vinj_default <- vinj_default[1]
    if (!(length(vinj_default) == 1 && is.finite(vinj_default))) vinj_default <- UI_DEFAULTS$v_inj_default * 1000
    
    g_syringe <- suppressWarnings(as.numeric(input$G_syringe))
    if (length(g_syringe) >= 1) g_syringe <- g_syringe[1]
    if (!(length(g_syringe) == 1 && is.finite(g_syringe) && g_syringe > 0)) g_syringe <- UI_DEFAULTS$conc_syringe_default
    
    V_inj_uL <- if ("V_titrate_uL" %in% colnames(int_df)) as.numeric(int_df$V_titrate_uL) else rep(vinj_default, nrow(int_df))
    V_inj_uL[is.na(V_inj_uL)] <- vinj_default
    
    Heat_Raw <- if ("heat_cal_mol" %in% colnames(int_df)) {
      as.numeric(int_df$heat_cal_mol)
    } else if ("Heat_ucal" %in% colnames(int_df)) {
      denom <- V_inj_uL * g_syringe
      ifelse(is.finite(denom) & denom > 0, 1000 * as.numeric(int_df$Heat_ucal) / denom, NA_real_)
    } else {
      rep(NA_real_, nrow(int_df))
    }
    
    Ratio_Raw <- if ("Ratio_App" %in% colnames(int_df)) as.numeric(int_df$Ratio_App) else rep(NA_real_, nrow(int_df))
    exp_df <- data.frame(
      Ratio_Raw = Ratio_Raw,
      Heat_Raw = Heat_Raw,
      V_inj_uL = V_inj_uL,
      Inj = seq_len(nrow(int_df)),
      stringsAsFactors = FALSE
    )
    exp_df <- exp_df[is.finite(exp_df$Heat_Raw), , drop = FALSE]
    if (nrow(exp_df) == 0) return(FALSE)
    exp_df$Inj <- seq_len(nrow(exp_df))
    
    meta_df <- NULL
    if (!is.null(bundle) && is.list(bundle) && is.data.frame(bundle$meta)) {
      meta_df <- as.data.frame(bundle$meta)
    }
    if (is.null(meta_df) && is.data.frame(payload$meta)) {
      meta_df <- as.data.frame(payload$meta)
    }
    
    power_df <- NULL
    if (!is.null(bundle) && is.list(bundle) && is.data.frame(bundle$power_corrected)) {
      power_df <- as.data.frame(bundle$power_corrected)
    }
    
    values$manual_exp_data <- exp_df
    values$manual_exp_source <- "step1_bridge"
    
    cached_sheets <- list(integration = int_df)
    if (!is.null(meta_df)) cached_sheets[["meta"]] <- meta_df
    if (!is.null(power_df)) cached_sheets[["power_corrected"]] <- power_df
    values$imported_xlsx_sheets <- cached_sheets
    
    source_name <- as.character(payload$source %||% "Step1")
    if (length(source_name) == 0 || !nzchar(source_name[1])) source_name <- "Step1"
    source_name <- source_name[1]
    values$imported_xlsx_filename <- source_name
    base_name <- tools::file_path_sans_ext(basename(source_name))
    values$imported_xlsx_base_name <- if (nzchar(base_name)) base_name else "ITC_data"
    token_tag <- if (is_finite_scalar(token)) {
      format(token, scientific = FALSE, trim = TRUE)
    } else {
      format(Sys.time(), "%Y%m%d%H%M%S")
    }
    values$imported_xlsx_file_path <- paste0("bridge://step1/", token_tag)
    
    apply_meta_to_exp_inputs(meta_df)
    
    if ("V_titrate_uL" %in% colnames(int_df)) {
      first_v <- suppressWarnings(as.numeric(int_df$V_titrate_uL))
      first_v <- first_v[is.finite(first_v)]
      if (length(first_v) > 0) {
        values$v_pre_programmatic_update <- TRUE
        updateNumericInput(session, "V_pre", value = first_v[1])
        updateNumericInput(session, "V_init_val", value = first_v[1])
      }
    }
    if (all(is.finite(V_inj_uL))) {
      updateNumericInput(session, "V_inj", value = as.numeric(stats::median(V_inj_uL)))
    }
    updateNumericInput(session, "n_inj", value = nrow(exp_df))
    
    if (is_finite_scalar(token)) bridge_last_step1_token(token)
    showNotification("Loaded Step 1 data into Step 2.", type = "message", duration = 2)
    TRUE
  }
  
  if (!is.null(session_bridge) && is.function(session_bridge$step1_payload)) {
    observeEvent(session_bridge$step1_payload(), {
      tryCatch(
        consume_step1_payload(session_bridge$step1_payload()),
        error = function(e) {
          showNotification(
            paste0("Step1 bridge payload skipped: ", conditionMessage(e)),
            type = "warning",
            duration = 5
          )
          FALSE
        }
      )
    }, ignoreNULL = TRUE)
  } else {
    observe({
      invalidateLater(300, session)
      tryCatch(
        consume_step1_payload(bridge_get("step1_payload")),
        error = function(e) FALSE
      )
    })
  }
  
  output$status_indicator <- renderUI({
    if(values$is_fitting) {
      tagList(div(class="status-light light-red"), 
              span(tr("fit_status_fitting", lang()), style="color:#e74c3c"))
    } else {
      tagList(div(class="status-light light-green"), 
              span(tr("fit_status_ready", lang()), style="color:#2ecc71"))
    }
  })

  # [新增] 统一的 RSS 计算逻辑 (Reactive)
  # 基于针数 (Injection Index) 对齐，而非 Ratio 插值
  current_rss_val <- reactive({
    sim_res <- sim_results()
    exp_df <- tryCatch(exp_data_processed(), error=function(e) NULL)
    range_lim <- input$fit_data_range
    
    if(is.null(sim_res) || is.null(exp_df)) return(list(rss = NA, method = ""))
    if(any(!is.finite(sim_res$dQ_App))) return(list(rss = NA, method = ""))
    
    # 确保模拟针数至少覆盖了拟合区间
    max_idx <- min(nrow(sim_res), nrow(exp_df))
    valid_idx <- range_lim[1]:range_lim[2]
    valid_idx <- valid_idx[valid_idx <= max_idx]
    
    if(length(valid_idx)==0) return(list(rss = NA, method = ""))
    
    # 计算残差
    residuals <- sim_res$dQ_App[valid_idx] - exp_df$Heat_Raw[valid_idx]
    
    # 检查是否启用了加权或鲁棒方法
    use_weighted <- isTRUE(input$use_weighted_fitting)
    use_robust <- isTRUE(input$use_robust_fitting)
    huber_delta_input <- if(!is.null(input$huber_delta) && !is.na(input$huber_delta) && input$huber_delta > 0) input$huber_delta else NULL
    
    # 构建方法标识
    method_parts <- c()
    if(isTRUE(use_weighted)) method_parts <- c(method_parts, tr("weighted_method", lang()))
    if(isTRUE(use_robust)) method_parts <- c(method_parts, tr("robust_method", lang()))
    method_str <- if(length(method_parts) > 0) paste0(" (", paste(method_parts, collapse="+"), ")") else ""
    
    # 计算损失（与拟合时使用相同的方法）
    loss <- calculate_weighted_robust_loss(
      residuals = residuals,
      weights = if(isTRUE(use_weighted)) calculate_weights_from_derivative(exp_df, valid_idx) else NULL,
      use_huber = isTRUE(use_robust),
      huber_delta = huber_delta_input
    )
    
    return(list(rss = loss, method = method_str))
  })

  output$fit_status <- renderText({
    rss_info <- current_rss_val()
    if(is.na(rss_info$rss)) {
      tr("rss_na", lang())
    } else {
      paste0(formatC(rss_info$rss, format="e", digits=3), rss_info$method)
    }
  })
  
  # [新增] 误差分析表格输出
  output$error_analysis_table <- renderDT({
    # 显式依赖 lang() 以确保语言切换时重新渲染
    current_lang_val <- lang()
    
    error_df <- values$error_analysis
    # Filter for display: only logK and H parameters
    if (!is.null(error_df)) {
      error_df <- error_df[grepl("logK|^H[0-9]$", error_df$Parameter), ]
    }
    
    if (is.null(error_df) || nrow(error_df) == 0) {
      msg_col <- tr("error_analysis_no_result", current_lang_val)
      return(datatable(data.frame(Message = msg_col), 
                       options = list(dom = 't', destroy = TRUE), rownames = FALSE))
    }
    
    # 确保 error_df 有正确的列名（原始列名，不依赖翻译）
    # 如果列名已经被重命名过，需要先恢复原始列名
    required_cols <- c("Parameter", "Value", "SE", "CI_Lower", "CI_Upper")
    if (!all(required_cols %in% colnames(error_df))) {
      # 如果列名不匹配，尝试从翻译后的列名恢复
      # 这种情况不应该发生，但为了安全起见
      msg_col <- tr("error_analysis_no_result", current_lang_val)
      return(datatable(data.frame(Message = msg_col), 
                       options = list(dom = 't', destroy = TRUE), rownames = FALSE))
    }
    
    # 格式化显示：只显示 logK 和 H 参数
    display_df <- error_df %>%
      mutate(
        # 格式化数值显示
        Value_Formatted = ifelse(
          grepl("logK", Parameter),
          sprintf("%.3f", Value),
          sprintf("%.1f", Value)
        ),
        SE_Formatted = ifelse(
          grepl("logK", Parameter),
          sprintf("±%.3f", SE),
          sprintf("±%.1f", SE)
        ),
        CI_Formatted = ifelse(
          grepl("logK", Parameter),
          sprintf("[%.3f, %.3f]", CI_Lower, CI_Upper),
          sprintf("[%.1f, %.1f]", CI_Lower, CI_Upper)
        ),
        # 参数名格式化（使用更安全的方法，避免 ifelse 的问题）
        Parameter_Formatted = sapply(Parameter, function(p) {
          if(is.null(p) || is.na(p) || p == "" || length(p) == 0) {
            return(p)
          }
          if(grepl("logK", p)) {
            result <- gsub("logK", "log K", p)
            if(is.null(result) || length(result) == 0 || result == "") p else result
          } else if(grepl("^H[0-9]$", p)) {
            result <- gsub("^H", "ΔH", p)
            if(is.null(result) || length(result) == 0 || result == "") p else result
          } else {
            p
          }
        })
      ) %>%
      select(Parameter_Formatted, Value_Formatted, SE_Formatted, CI_Formatted)
    
    # 获取翻译后的列名（确保不为空）
    param_name <- tr("error_analysis_table_param", current_lang_val)
    value_name <- tr("error_analysis_table_value", current_lang_val)
    se_name <- tr("error_analysis_table_se", current_lang_val)
    ci_name <- tr("error_analysis_table_ci", current_lang_val)
    
    # 如果翻译为空，使用默认值
    if(is.null(param_name) || length(param_name) == 0 || param_name == "") param_name <- "参数"
    if(is.null(value_name) || length(value_name) == 0 || value_name == "") value_name <- "最优值"
    if(is.null(se_name) || length(se_name) == 0 || se_name == "") se_name <- "标准误差"
    if(is.null(ci_name) || length(ci_name) == 0 || ci_name == "") ci_name <- "95% 置信区间"
    
    # 【关键修复】使用固定的内部列名，避免 DataTables 缓存问题
    # 内部列名永远使用英文，不随语言变化，这样 DataTables 就不会因为列名变化而报错
    internal_colnames <- c("Param", "Value", "SE", "CI")
    colnames(display_df) <- internal_colnames
    
    # 使用 initComplete 回调来在表格初始化完成后更新表头
    # 这样可以确保表头使用翻译后的名称，同时数据框列名保持固定
    init_complete_js <- JS(
      sprintf(
        "function(settings, json) {
          var api = this.api();
          $(api.table().header()).find('th').eq(0).text('%s');
          $(api.table().header()).find('th').eq(1).text('%s');
          $(api.table().header()).find('th').eq(2).text('%s');
          $(api.table().header()).find('th').eq(3).text('%s');
        }",
        gsub("'", "\\'", param_name),  # 转义单引号
        gsub("'", "\\'", value_name),
        gsub("'", "\\'", se_name),
        gsub("'", "\\'", ci_name)
      )
    )
    
    datatable(
      display_df,
      options = list(
        dom = 't',
        pageLength = 10,
        scrollY = "150px",
        scrollCollapse = TRUE,
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        # 添加 destroy: true 选项，确保在重新渲染时销毁旧表格
        destroy = TRUE,
        # 使用 initComplete 回调在表格初始化后更新表头
        initComplete = init_complete_js
      ),
      selection = 'none',  # 禁用行选择功能
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left; font-size: 0.9em; color: #666;',
        tr("error_analysis_note", current_lang_val)
      )
    ) %>%
      formatStyle(columns = 1:4, fontSize = '12px')
  })
  
  # [修改] 仅当用户手动调整第一针体积时提示同步 V_init；程序更新（导入、打开 app、加载快照等）不提示
  observeEvent(input$V_pre, {
    if (isTRUE(values$v_pre_programmatic_update)) {
      values$v_pre_programmatic_update <- FALSE
      return()
    }
    req(input$V_pre)
    if(input$V_pre > 0) {
      showNotification(
        tr("v_pre_change_warning", lang()), 
        type = "message", 
        duration = 8
      )
    }
  }, ignoreInit = TRUE)
  
  
  # 移除重复代码块占位符 (原有重复代码将被覆盖为空)
  
  output$dynamic_fit_params_ui <- renderUI({
    # [修复] active_paths 应该是 reactive 依赖，这样路径变化时会自动更新
    # 只有 current_selection 需要 isolate，避免循环依赖
    paths <- input$active_paths
    current_selection <- isolate({
      if(!is.null(input$fit_params)) input$fit_params else c("logK1", "H1")  # [修改] 默认只勾选基础参数
    })
    
    choices <- c("fH", "fG", "V_init", "Offset","logK1", "H1") # [修改] fpre -> V_init
    
    # [新增] 根据激活的路径自动添加对应的参数到选择中
    auto_selected <- c("logK1", "H1")  # 基础参数始终在自动选择列表中
    
    if("rxn_D" %in% paths) {
      choices <- c(choices, "logK2", "H2")
      auto_selected <- c(auto_selected, "logK2", "H2")
    }
    if("rxn_T" %in% paths) {
      choices <- c(choices, "logK3", "H3")
      auto_selected <- c(auto_selected, "logK3", "H3")
    }
    if("rxn_B" %in% paths) {
      choices <- c(choices, "logK4", "H4")
      auto_selected <- c(auto_selected, "logK4", "H4")
    }
    if("rxn_F" %in% paths) {
      choices <- c(choices, "logK5", "H5")
      auto_selected <- c(auto_selected, "logK5", "H5")
    }
    if("rxn_U" %in% paths) {
      choices <- c(choices, "logK6", "H6")
      auto_selected <- c(auto_selected, "logK6", "H6")
    }
    
    # [修改] 合并当前选择和自动选择，但只保留有效的选择项
    # 这样既保留了用户的手动选择（如 fH, fG 等），又自动添加了激活路径的参数
    valid_selection <- unique(c(
      intersect(current_selection, choices),  # 保留用户之前的选择（如果仍然有效）
      intersect(auto_selected, choices)       # 添加自动选择的参数
    ))
    
    # [新增] 添加全选/全不选按钮
    tagList(
      div(style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 5px;",
          tags$label(tr("fit_params_select", lang()), style="margin: 0; font-weight: bold;"),
          div(style="display: flex; gap: 5px;",
              actionButton("select_all_fit_params", tr("fit_params_select_all", lang()), class = "btn-default btn-xs", style="padding: 2px 8px; font-size: 11px;"),
              actionButton("deselect_all_fit_params", tr("fit_params_deselect_all", lang()), class = "btn-default btn-xs", style="padding: 2px 8px; font-size: 11px;")
          )
      ),
      checkboxGroupInput("fit_params", NULL, choices = choices, selected = valid_selection, inline = TRUE)
    )
  })
  
  # [新增] 全选拟合参数
  observeEvent(input$select_all_fit_params, {
    # [修复] 防止在语言切换时触发
    if(lang_switching()) return()
    
    paths <- input$active_paths
    choices <- c("fH", "fG", "V_init", "Offset","logK1", "H1")
    
    if("rxn_D" %in% paths) choices <- c(choices, "logK2", "H2")
    if("rxn_T" %in% paths) choices <- c(choices, "logK3", "H3")
    if("rxn_B" %in% paths) choices <- c(choices, "logK4", "H4")
    if("rxn_F" %in% paths) choices <- c(choices, "logK5", "H5")
    if("rxn_U" %in% paths) choices <- c(choices, "logK6", "H6")
    
    updateCheckboxGroupInput(session, "fit_params", selected = choices)
  }, ignoreInit = TRUE)
  
  # [新增] 全不选拟合参数
  observeEvent(input$deselect_all_fit_params, {
    # [修复] 防止在语言切换时触发
    if(lang_switching()) return()
    
    updateCheckboxGroupInput(session, "fit_params", selected = character(0))
  }, ignoreInit = TRUE)
  
  # [新增] 模拟→实验按钮点击事件处理器
  observeEvent(input$sim_to_exp, {
    sim <- sim_results()
    
    # 检查模拟数据是否有效
    if (is.null(sim)) {
      showNotification(
        tr("export_error_no_data", lang()), 
        type = "error", 
        duration = 5
      )
      return()
    }
    
    # 转换模拟数据为实验数据格式
    tryCatch({
      exp_data <- data.frame(
        Ratio_Raw = sim$Ratio_App,
        Heat_Raw = sim$dQ_App,
        V_inj_uL = if ("V_inj_uL" %in% names(sim)) sim$V_inj_uL else NA_real_,
        Inj = sim$Inj
      )
      
      # 存储到 manual_exp_data
      values$manual_exp_data <- exp_data
      values$manual_exp_source <- "sim_to_exp"
      
      # 清空导入文件信息，避免 UI 显示之前的导入文件名
      values$imported_xlsx_filename <- NULL
      values$imported_xlsx_base_name <- NULL
      values$imported_xlsx_file_path <- NULL
      values$imported_xlsx_sheets <- NULL
      
      # 显示成功通知
      showNotification(
        paste0(tr("btn_sim_to_exp", lang()), ": ", 
               nrow(exp_data), " ", 
               tr("error_analysis_data_points", lang())),
        type = "message",
        duration = 3
      )
    }, error = function(e) {
      showNotification(
        paste0(tr("error_occurred", lang()), ": ", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  # 缓存当前导入的 xlsx 文件路径，用于与 reactive 共享已读取的 sheets，避免重复读取且保证参数更新与导入同步
  values$imported_xlsx_file_path <- NULL

  exp_data_processed <- reactive({
    safe_val <- function(x, default) {
      if (is.null(x) || length(x) == 0 || is.na(x)) default else x
    }

    apply_updated_ratio <- function(d) {
      if (is.null(d) || nrow(d) == 0) return(d)
      V_inj_vec <- if ("V_inj_uL" %in% names(d)) {
        as.numeric(d$V_inj_uL)
      } else {
        rep(safe_val(input$V_inj, UI_DEFAULTS$v_inj_default * 1000), nrow(d))
      }
      if (any(is.na(V_inj_vec))) {
        V_inj_vec[is.na(V_inj_vec)] <- safe_val(input$V_inj, UI_DEFAULTS$v_inj_default * 1000)
      }
      p_ratio <- list(
        H_cell_0 = safe_val(input$H_cell_0, UI_DEFAULTS$conc_cell_default),
        G_syringe = safe_val(input$G_syringe, UI_DEFAULTS$conc_syringe_default),
        V_cell = safe_val(input$V_cell, UI_DEFAULTS$v_cell_default),
        V_inj_vec = V_inj_vec,
        n_inj = length(V_inj_vec),
        fH = safe_val(input$factor_H, DEFAULT_PARAMS$fH),
        fG = safe_val(input$factor_G, DEFAULT_PARAMS$fG)
      )
      ratio_app <- tryCatch(calculate_ratio_app(p_ratio), error = function(e) NULL)
      if (!is.null(ratio_app) && length(ratio_app) == nrow(d)) {
        d$Ratio_Raw <- ratio_app
      }
      d
    }

    build_exp_from_integration <- function(int_df) {
      if (is.null(int_df) || nrow(int_df) == 0) return(NULL)
      if (!("Heat_ucal" %in% colnames(int_df))) {
        showNotification("integration缺少Heat_ucal，无法按实时参数重算热量", type = "warning", duration = 5)
        return(NULL)
      }

      heat_ucal <- as.numeric(int_df$Heat_ucal)
      V_inj_uL <- if ("V_titrate_uL" %in% colnames(int_df)) {
        as.numeric(int_df$V_titrate_uL)
      } else {
        rep(NA_real_, nrow(int_df))
      }
      if (any(is.na(V_inj_uL))) {
        V_inj_uL[is.na(V_inj_uL)] <- safe_val(input$V_inj, UI_DEFAULTS$v_inj_default * 1000)
      }

      G_syringe <- safe_val(input$G_syringe, UI_DEFAULTS$conc_syringe_default)
      denom <- V_inj_uL * G_syringe
      heat_raw <- ifelse(!is.na(denom) & denom > 0, 1000 * heat_ucal / denom, NA_real_)

      d <- data.frame(
        Heat_Raw = heat_raw,
        V_inj_uL = V_inj_uL,
        Inj = seq_len(nrow(int_df))
      )
      d <- d[!is.na(d$Heat_Raw), ]
      d$Inj <- seq_len(nrow(d))
      apply_updated_ratio(d)
    }

    # 优先使用手动导入的数据（模拟→实验功能）
    if (!is.null(values$manual_exp_data)) {
      return(apply_updated_ratio(values$manual_exp_data))
    }
    
    req(input$exp_file)
    filepath <- input$exp_file$datapath

    # 若已在 observeEvent(input$exp_file) 中读取过同一文件，直接使用缓存数据
    if (!is.null(values$imported_xlsx_sheets) && identical(filepath, values$imported_xlsx_file_path)) {
      # 优先：ITCprocessor 格式 (integration sheet)
      int_df <- values$imported_xlsx_sheets[["integration"]]
      d <- build_exp_from_integration(int_df)
      if (!is.null(d)) return(d)
      # 备选：SimFit 导出的拟合数据 (simulation sheet)，作为实验数据导入
      sim_df <- values$imported_xlsx_sheets[["simulation"]]
      if (!is.null(sim_df) && "Ratio_App" %in% colnames(sim_df) && "dQ_App" %in% colnames(sim_df)) {
        d <- data.frame(
          Ratio_Raw = as.numeric(sim_df$Ratio_App),
          Heat_Raw  = as.numeric(sim_df$dQ_App),
          V_inj_uL  = if ("V_inj_uL" %in% colnames(sim_df)) as.numeric(sim_df$V_inj_uL) else NA_real_,
          Inj       = if ("Inj" %in% colnames(sim_df)) as.integer(sim_df$Inj) else seq_len(nrow(sim_df))
        )
        d <- d[!is.na(d$Ratio_Raw) & !is.na(d$Heat_Raw), ]
        d$Inj <- seq_len(nrow(d))
        return(apply_updated_ratio(d))
      }
    }
    
    df <- tryCatch({
      # 先读取并缓存所有 sheets
      sheet_names <- readxl::excel_sheets(filepath)
      cached_sheets <- list()
      for (sn in sheet_names) {
        cached_sheets[[sn]] <- as.data.frame(readxl::read_excel(filepath, sheet = sn))
      }
      values$imported_xlsx_sheets <- cached_sheets
      values$imported_xlsx_file_path <- filepath

      # --- 路径 1：ITCprocessor 导出格式 (integration sheet) ---
      if ("integration" %in% names(cached_sheets)) {
        int_df <- cached_sheets[["integration"]]
        d <- build_exp_from_integration(int_df)
        if (!is.null(d)) return(d)
      }

      # --- 路径 2：SimFit 导出的拟合数据 (simulation sheet)，作为实验数据导入 ---
      if ("simulation" %in% names(cached_sheets)) {
        sim_df <- cached_sheets[["simulation"]]
        if (!is.null(sim_df) && "Ratio_App" %in% colnames(sim_df) && "dQ_App" %in% colnames(sim_df)) {
          d <- data.frame(
            Ratio_Raw = as.numeric(sim_df$Ratio_App),
            Heat_Raw  = as.numeric(sim_df$dQ_App),
            V_inj_uL  = if ("V_inj_uL" %in% colnames(sim_df)) as.numeric(sim_df$V_inj_uL) else NA_real_,
            Inj       = if ("Inj" %in% colnames(sim_df)) as.integer(sim_df$Inj) else seq_len(nrow(sim_df))
          )
          d <- d[!is.na(d$Ratio_Raw) & !is.na(d$Heat_Raw), ]
          d$Inj <- seq_len(nrow(d))
          return(apply_updated_ratio(d))
        }
      }

      return(NULL)
    }, error = function(e) {
      values$imported_xlsx_sheets <- NULL
      values$imported_xlsx_file_path <- NULL
      return(NULL)
    })
    
    req(df)
    return(df)
  })
  
  # 监听文件上传：读取 xlsx、缓存 sheets，并立即根据 meta 更新实验参数（保证导入即更新）
  observeEvent(input$exp_file, {
    f <- input$exp_file
    if (is.null(f) || is.null(f$datapath)) return()

    values$manual_exp_data <- NULL
    values$manual_exp_source <- NULL
    values$imported_xlsx_sheets <- NULL
    values$imported_xlsx_file_path <- NULL
    values$imported_xlsx_base_name <- NULL
    values$imported_xlsx_filename <- NULL

    filepath <- f$datapath
    # 显示用文件名（用户选择的原始文件名）
    values$imported_xlsx_filename <- if (is.null(f$name) || f$name == "") NULL else f$name
    # 记住导入文件基名（去掉时间戳如 _processed_20250108_1234 或 _fitted_20250108_1234）
    base_raw <- tools::file_path_sans_ext(if (is.null(f$name) || f$name == "") "data" else f$name)
    values$imported_xlsx_base_name <- sub("_(processed|fitted)_\\d{8}_\\d{4}$", "", base_raw)

    sheets <- tryCatch({
      snames <- readxl::excel_sheets(filepath)
      lst <- list()
      for (sn in snames) {
        lst[[sn]] <- as.data.frame(readxl::read_excel(filepath, sheet = sn))
      }
      lst
    }, error = function(e) NULL)

    if (!is.null(sheets)) {
      values$imported_xlsx_sheets <- sheets
      values$imported_xlsx_file_path <- filepath

      # 填充实验参数：仅当有 integration 或 simulation（实验数据）时执行
      # 优先级：meta（含有效参数）> fit_params > 默认值
      exp_key_cols <- c("H_cell_0_mM", "G_syringe_mM", "V_cell_mL", "V_inj_uL", "n_inj", "V_pre_uL", "Temp_K")
      has_exp_data <- FALSE
      if ("integration" %in% names(sheets)) {
        int_df <- sheets[["integration"]]
        if (!is.null(int_df) && "Ratio_App" %in% colnames(int_df) && "heat_cal_mol" %in% colnames(int_df)) {
          has_exp_data <- TRUE
        }
      }
      if (!has_exp_data && "simulation" %in% names(sheets)) {
        sim_df <- sheets[["simulation"]]
        if (!is.null(sim_df) && "Ratio_App" %in% colnames(sim_df) && "dQ_App" %in% colnames(sim_df)) {
          has_exp_data <- TRUE
        }
      }
      if (has_exp_data) {
        meta_has_params <- FALSE
        if ("meta" %in% names(sheets)) {
          meta_df <- sheets[["meta"]]
          if (!is.null(meta_df) && all(c("parameter", "value") %in% colnames(meta_df))) {
            # 安全转换：trim 空格后转数值，非数值（空、文本等）变为 NA，避免 coercion 警告
            vals <- suppressWarnings(as.numeric(trimws(as.character(meta_df$value))))
            param_vals <- setNames(vals, meta_df$parameter)
            meta_has_params <- any(vapply(exp_key_cols, function(k) {
              k %in% names(param_vals) && !is.na(param_vals[[k]])
            }, logical(1)))
            if (meta_has_params) {
              if ("H_cell_0_mM" %in% names(param_vals) && !is.na(param_vals[["H_cell_0_mM"]])) {
                updateNumericInput(session, "H_cell_0", value = param_vals[["H_cell_0_mM"]])
              }
              if ("G_syringe_mM" %in% names(param_vals) && !is.na(param_vals[["G_syringe_mM"]])) {
                updateNumericInput(session, "G_syringe", value = param_vals[["G_syringe_mM"]])
              }
              if ("V_cell_mL" %in% names(param_vals) && !is.na(param_vals[["V_cell_mL"]])) {
                updateNumericInput(session, "V_cell", value = param_vals[["V_cell_mL"]])
              }
              if ("V_inj_uL" %in% names(param_vals) && !is.na(param_vals[["V_inj_uL"]])) {
                updateNumericInput(session, "V_inj", value = param_vals[["V_inj_uL"]])
              }
              if ("V_pre_uL" %in% names(param_vals) && !is.na(param_vals[["V_pre_uL"]])) {
                values$v_pre_programmatic_update <- TRUE
                updateNumericInput(session, "V_pre", value = param_vals[["V_pre_uL"]])
              }
              if ("n_inj" %in% names(param_vals) && !is.na(param_vals[["n_inj"]])) {
                updateNumericInput(session, "n_inj", value = param_vals[["n_inj"]])
              }
              if ("Temp_K" %in% names(param_vals) && !is.na(param_vals[["Temp_K"]])) {
                updateNumericInput(session, "Temp", value = param_vals[["Temp_K"]])
              }
              showNotification(tr("import_params_auto_filled", lang()), type = "message", duration = 3)
            }
          }
        }
        if (!meta_has_params && "fit_params" %in% names(sheets)) {
          fp_df <- sheets[["fit_params"]]
          if (!is.null(fp_df) && nrow(fp_df) >= 1 && all(c("parameter", "value") %in% colnames(fp_df))) {
            param_vals <- setNames(trimws(as.character(fp_df$value)), trimws(as.character(fp_df$parameter)))
            fp_has_params <- any(vapply(exp_key_cols, function(k) {
              k %in% names(param_vals) && !is.na(suppressWarnings(as.numeric(param_vals[[k]])))
            }, logical(1)))
            if (fp_has_params) {
              if ("H_cell_0_mM" %in% names(param_vals) && !is.na(suppressWarnings(as.numeric(param_vals[["H_cell_0_mM"]])))) {
                updateNumericInput(session, "H_cell_0", value = as.numeric(param_vals[["H_cell_0_mM"]]))
              }
              if ("G_syringe_mM" %in% names(param_vals) && !is.na(suppressWarnings(as.numeric(param_vals[["G_syringe_mM"]])))) {
                updateNumericInput(session, "G_syringe", value = as.numeric(param_vals[["G_syringe_mM"]]))
              }
              if ("V_cell_mL" %in% names(param_vals) && !is.na(suppressWarnings(as.numeric(param_vals[["V_cell_mL"]])))) {
                updateNumericInput(session, "V_cell", value = as.numeric(param_vals[["V_cell_mL"]]))
              }
              if ("V_inj_uL" %in% names(param_vals) && !is.na(suppressWarnings(as.numeric(param_vals[["V_inj_uL"]])))) {
                updateNumericInput(session, "V_inj", value = as.numeric(param_vals[["V_inj_uL"]]))
              }
              if ("V_pre_uL" %in% names(param_vals) && !is.na(suppressWarnings(as.numeric(param_vals[["V_pre_uL"]])))) {
                values$v_pre_programmatic_update <- TRUE
                updateNumericInput(session, "V_pre", value = as.numeric(param_vals[["V_pre_uL"]]))
              }
              if ("n_inj" %in% names(param_vals) && !is.na(suppressWarnings(as.numeric(param_vals[["n_inj"]])))) {
                updateNumericInput(session, "n_inj", value = as.integer(param_vals[["n_inj"]]))
              }
              if ("Temp_K" %in% names(param_vals) && !is.na(suppressWarnings(as.numeric(param_vals[["Temp_K"]])))) {
                updateNumericInput(session, "Temp", value = as.numeric(param_vals[["Temp_K"]]))
              }
              showNotification(tr("import_params_from_fit_params", lang()), type = "message", duration = 4)
            } else {
              showNotification(tr("import_params_no_source", lang()), type = "warning", duration = 5)
            }
          } else {
            showNotification(tr("import_params_no_source", lang()), type = "warning", duration = 5)
          }
        } else if (!meta_has_params) {
          showNotification(tr("import_params_no_source", lang()), type = "warning", duration = 5)
        }
        if ("integration" %in% names(sheets)) {
          int_df <- sheets[["integration"]]
          if (!is.null(int_df) && "V_titrate_uL" %in% colnames(int_df)) {
            first_v <- suppressWarnings(as.numeric(int_df$V_titrate_uL))
            first_v <- first_v[!is.na(first_v)]
            if (length(first_v) > 0) {
              values$v_pre_programmatic_update <- TRUE
              updateNumericInput(session, "V_pre", value = first_v[1])
              updateNumericInput(session, "V_init_val", value = first_v[1])
            }
          }
        }
      }
      # 若为 SimFit 导出的拟合数据（无 integration，有 simulation），提示已作为实验数据导入
      if ("simulation" %in% names(sheets) && !("integration" %in% names(sheets))) {
        sim_df <- sheets[["simulation"]]
        if (!is.null(sim_df) && "Ratio_App" %in% colnames(sim_df) && "dQ_App" %in% colnames(sim_df)) {
          showNotification(tr("import_fit_data_as_exp", lang()), type = "message", duration = 4)
        }
      }
    }
  })
  
  # 判断“当前无实验数据”：不依赖 exp_data_processed()，避免无文件时 req() 挂起导致滑条无法随 n_inj 更新
  has_exp_data <- reactive({
    if (!is.null(values$manual_exp_data) && nrow(values$manual_exp_data) > 0) return(TRUE)
    if (is.null(input$exp_file) || length(input$exp_file) == 0) return(FALSE)
    exp_df <- tryCatch(exp_data_processed(), error = function(e) NULL)
    !is.null(exp_df) && nrow(exp_df) > 0
  })
  
  # 初始化：根据 n_inj 的初始值设置滑条默认值（只在应用启动时执行一次）
  fit_slider_initialized <- reactiveVal(FALSE)
  
  observe({
    if (!has_exp_data() && !fit_slider_initialized() && !is.null(input$n_inj) && is.numeric(input$n_inj) && input$n_inj > 0) {
      isolate({
        left_default <- if (as.integer(input$n_inj) >= 2L) 2L else 1L
        updateSliderInput(session, "fit_data_range", min = 1, max = input$n_inj, value = c(left_default, as.integer(input$n_inj)), step = 1)
        fit_slider_initialized(TRUE)
      })
    }
  }, priority = 10)
  
  # 有实验数据时：更新滑条 min/max，并自动将右值扩展到最大注射数，左值默认设为 2
  observeEvent(exp_data_processed(), {
    df <- exp_data_processed()
    if (is.null(df) || nrow(df) == 0) return()
    max_inj <- nrow(df)
    left_default <- if (max_inj >= 2L) 2L else 1L
    new_value <- c(left_default, max_inj)
    updateSliderInput(session, "fit_data_range", min = 1, max = max_inj, value = new_value, step = 1)
  })
  
  # 无实验数据时：n_inj 变化时更新滑条 max 和 value（不依赖 exp_data_processed()，避免 req 挂起）
  observeEvent(input$n_inj, {
    if (!has_exp_data()) {
      if (!is.null(input$n_inj) && is.numeric(input$n_inj) && input$n_inj > 0) {
        n <- as.integer(input$n_inj)
        cur <- isolate(input$fit_data_range)
        # 若当前范围仍在新 [1, n] 内则保留，否则设为全范围
        new_value <- if (!is.null(cur) && length(cur) == 2 && cur[1] >= 1 && cur[2] <= n && cur[1] <= cur[2]) {
          c(as.integer(cur[1]), as.integer(cur[2]))
        } else {
          c(if (n >= 2L) 2L else 1L, n)
        }
        updateSliderInput(session, "fit_data_range", min = 1, max = n, value = new_value, step = 1)
      }
    }
  })
  
  # [修复] 语言切换时，标签通过 uiOutput 自动更新，不需要手动更新滑条的 label
  # 移除此 observeEvent，因为 output$fit_data_range_slider_label 已经会根据 lang() 自动更新
  
  # 模拟计算包装函数
  # 负责将 UI 参数转换为物理参数 (应用 fH, fG 修正)
  # 并计算 Apparent Ratio 用于绘图
#   calculate_simulation <- function(p, active_paths) {
#     p_true <- p
#     # [浓度修正] 真实浓度 = 名义浓度 * 修正因子
#     p_true$H_cell_0 <- p$H_cell_0 * p$fH * 1e-6
#     p_true$G_syringe <- p$G_syringe * p$fG * 1e-6
#     p_true$V_inj <- p$V_inj / 1000
#     
#     paths <- unique(c("rxn_M", active_paths))
#     res_raw <- run_sim_modular(p_true, paths)
#     if(is.null(res_raw)) return(NULL)
#     
#     # [Apparent Ratio 回算]
#     # Sim 内部使用的是真实浓度计算的 Ratio (Ratio_True)。
#     # 用户看到的 Ratio_App 是基于名义浓度的。
#     # Ratio_True = (G_nom * fG) / (H_nom * fH) = Ratio_App * (fG/fH)
#     # 所以: Ratio_App = Ratio_True * (fH/fG)
#     res_raw$Ratio_App <- res_raw$Ratio * (p$fH / p$fG)
#     
#     # [热量修正] dQ_App = dQ_True * fG + Offset
#     # (假设热量与注射物浓度成正比，故乘以 fG)
#     res_raw$dQ_App    <- (res_raw$dQ * p$fG) + p$Offset
#     return(res_raw)
#   }
  
  sim_results <- reactive({
    # 确保所有必需的输入值都已初始化
    req(input$V_cell, input$V_inj, input$H_cell_0, input$G_syringe, input$n_inj,
        input$logK1, input$H1, input$factor_H, input$factor_G)
    
    # [缓存机制] 构建参数签名作为缓存键
    # 包含所有影响模拟结果的参数
    # 安全处理 active_paths（可能为 NULL）
    active_paths_safe <- if(is.null(input$active_paths) || length(input$active_paths) == 0) {
      character(0)
    } else {
      input$active_paths
    }
    active_paths_str <- if(length(active_paths_safe) > 0) {
      paste(sort(active_paths_safe), collapse=",")
    } else {
      ""
    }
    
    # 安全获取所有输入值，使用默认值防止NULL
    safe_input <- function(x, default) {
      if(is.null(x) || length(x) == 0 || is.na(x)) default else x
    }
    
    exp_df_cache <- tryCatch(exp_data_processed(), error = function(e) NULL)
    inj_vec_cache <- if (!is.null(exp_df_cache) && "V_inj_uL" %in% names(exp_df_cache)) {
      exp_df_cache$V_inj_uL
    } else {
      rep(safe_input(input$V_inj, UI_DEFAULTS$v_inj_default * 1000), safe_input(input$n_inj, UI_DEFAULTS$n_inj_default))
    }
    n_inj_cache <- if (!is.null(exp_df_cache) && "V_inj_uL" %in% names(exp_df_cache)) {
      length(inj_vec_cache)
    } else {
      safe_input(input$n_inj, UI_DEFAULTS$n_inj_default)
    }
    inj_vec_key <- paste(inj_vec_cache, collapse = ",")

    cache_key <- paste(
      safe_input(input$H_cell_0, UI_DEFAULTS$conc_cell_default), 
      safe_input(input$G_syringe, UI_DEFAULTS$conc_syringe_default), 
      safe_input(input$V_cell, UI_DEFAULTS$v_cell_default), 
      # [注释] V_inj 在 UI 中是 uL，缓存键也用 uL 值（与 UI 一致）
      safe_input(input$V_inj, UI_DEFAULTS$v_inj_default * 1000), 
      n_inj_cache,
      safe_input(input$V_init_val, 0),
      inj_vec_key,
      safe_input(input$logK1, DEFAULT_PARAMS$logK), safe_input(input$H1, DEFAULT_PARAMS$H), 
      safe_input(input$logK2, DEFAULT_PARAMS$logK), safe_input(input$H2, DEFAULT_PARAMS$H), 
      safe_input(input$logK3, DEFAULT_PARAMS$logK), safe_input(input$H3, DEFAULT_PARAMS$H),
      safe_input(input$logK4, DEFAULT_PARAMS$logK), safe_input(input$H4, DEFAULT_PARAMS$H), 
      safe_input(input$logK5, DEFAULT_PARAMS$logK), safe_input(input$H5, DEFAULT_PARAMS$H),
      safe_input(input$logK6, DEFAULT_PARAMS$logK), safe_input(input$H6, DEFAULT_PARAMS$H),
      safe_input(input$factor_H, DEFAULT_PARAMS$fH), safe_input(input$factor_G, DEFAULT_PARAMS$fG), 
      safe_input(input$heat_offset, DEFAULT_PARAMS$Offset),
      active_paths_str,
      sep = "|"
    )
    
    # [缓存机制] 检查缓存键是否相同
    last_cache_key <- sim_cache_key()
    if (!is.null(last_cache_key) && identical(cache_key, last_cache_key)) {
      cached_result <- sim_cache_result()
      if (!is.null(cached_result)) {
        return(cached_result)
      }
    }
    
    # [缓存机制] 计算新结果
    exp_df <- tryCatch(exp_data_processed(), error = function(e) NULL)
    V_inj_vec <- if (!is.null(exp_df) && "V_inj_uL" %in% names(exp_df)) {
      exp_df$V_inj_uL
    } else {
      rep(safe_input(input$V_inj, UI_DEFAULTS$v_inj_default * 1000), safe_input(input$n_inj, UI_DEFAULTS$n_inj_default))
    }
    if (any(is.na(V_inj_vec))) {
      V_inj_vec[is.na(V_inj_vec)] <- safe_input(input$V_inj, UI_DEFAULTS$v_inj_default * 1000)
    }
    n_inj_effective <- if (!is.null(exp_df) && "V_inj_uL" %in% names(exp_df)) {
      length(V_inj_vec)
    } else {
      safe_input(input$n_inj, UI_DEFAULTS$n_inj_default)
    }
    if (length(V_inj_vec) > 0) {
      V_inj_vec[1] <- safe_input(input$V_init_val, DEFAULT_PARAMS$V_init)
    }

    p_curr <- list(
      H_cell_0=safe_input(input$H_cell_0, UI_DEFAULTS$conc_cell_default), 
      G_syringe=safe_input(input$G_syringe, UI_DEFAULTS$conc_syringe_default), 
      V_cell=safe_input(input$V_cell, UI_DEFAULTS$v_cell_default), 
      n_inj=n_inj_effective,
      # [重要] V_inj_vec 在 UI/导入中是 uL，calculate_simulation 内部会除以 1000，所以这里不转换
      V_inj_vec = V_inj_vec,
      logK1=safe_input(input$logK1, DEFAULT_PARAMS$logK), H1=safe_input(input$H1, DEFAULT_PARAMS$H), 
      logK2=safe_input(input$logK2, DEFAULT_PARAMS$logK), H2=safe_input(input$H2, DEFAULT_PARAMS$H), 
      logK3=safe_input(input$logK3, DEFAULT_PARAMS$logK), H3=safe_input(input$H3, DEFAULT_PARAMS$H),
      logK4=safe_input(input$logK4, DEFAULT_PARAMS$logK), H4=safe_input(input$H4, DEFAULT_PARAMS$H), 
      logK5=safe_input(input$logK5, DEFAULT_PARAMS$logK), H5=safe_input(input$H5, DEFAULT_PARAMS$H),
      logK6=safe_input(input$logK6, DEFAULT_PARAMS$logK), H6=safe_input(input$H6, DEFAULT_PARAMS$H),
      fH=safe_input(input$factor_H, DEFAULT_PARAMS$fH), fG=safe_input(input$factor_G, DEFAULT_PARAMS$fG), 
      Offset=safe_input(input$heat_offset, DEFAULT_PARAMS$Offset)
    )
    result <- tryCatch({
      calculate_simulation(p_curr, active_paths_safe)
    }, error = function(e) {
      # 只在非启动阶段显示错误（避免启动时的初始化错误干扰用户）
      # 检查是否是启动阶段的错误（某些输入可能还未完全初始化）
      is_startup_error <- any(
        is.null(input$V_cell) || is.null(input$V_inj) || 
        is.null(input$logK1) || is.null(input$H1) ||
        is.null(input$factor_H) || is.null(input$factor_G)
      )
      
      if (!is_startup_error) {
        # 安全获取翻译文本，确保不为空
        # 在 reactive 上下文中，直接使用 current_lang() 而不是 lang()
        current_lang_val <- tryCatch({
          lang_val <- current_lang()
          if(is.null(lang_val) || length(lang_val) == 0 || !lang_val %in% c("zh", "en")) "en" else lang_val
        }, error = function(e) "en")
        
        calc_error_text <- tr("calc_error", current_lang_val)
        if(is.null(calc_error_text) || length(calc_error_text) == 0 || calc_error_text == "" || nchar(calc_error_text) == 0) {
          calc_error_text <- "计算失败："
        }
        
        calc_suggestion_text <- tr("calc_error_suggestion", current_lang_val)
        if(is.null(calc_suggestion_text) || length(calc_suggestion_text) == 0 || calc_suggestion_text == "" || nchar(calc_suggestion_text) == 0) {
          calc_suggestion_text <- "建议：检查参数是否在合理范围内"
        }
        
        error_msg <- paste0(calc_error_text, e$message, "\n", calc_suggestion_text)
        showNotification(
          error_msg,
          type = "error",
          duration = 10
        )
      }
      return(NULL)
    })
    if (is.null(result)) return(NULL)
    if (!is.null(V_inj_vec)) {
      result$V_inj_uL <- V_inj_vec[seq_len(nrow(result))]
    }

    # [缓存机制] 更新缓存
    sim_cache_key(cache_key)
    sim_cache_result(result)

    return(result)
  })
  
  # 内部拟合函数 - 包含所有拟合逻辑
  perform_fitting_sync <- function(max_iters, use_DE, 
                                    exp_df, p_curr, fixed_p, params_to_opt, 
                                    range_lim, active_paths, enable_error_analysis, 
                                    use_weighted = FALSE, use_robust = FALSE, huber_delta = NULL,
                                    progress = NULL, lang_val = "en") {
    # 确保语言值有效
    if(is.null(lang_val) || length(lang_val) == 0 || !lang_val %in% c("zh", "en")) {
      lang_val <- "en"
    }
    par_vec <- unlist(p_curr[params_to_opt])
    fit_fixed_p <- fixed_p
    
    # 预计算权重（如果启用加权拟合）
    weights <- NULL
    if(isTRUE(use_weighted)) {
      max_idx_pre <- min(nrow(exp_df), max(range_lim))
      valid_idx_pre <- range_lim[1]:range_lim[2]
      valid_idx_pre <- valid_idx_pre[valid_idx_pre <= max_idx_pre]
      if(length(valid_idx_pre) > 0) {
        weights <- calculate_weights_from_derivative(exp_df, valid_idx_pre)
      }
    }
    
    # 目标函数 (Objective Function): 计算加权残差平方和 (RSS)
    # 采用 Needle-based 拟合：直接比较对应针数的热量值
    obj_fun <- function(par) {
      current_vars <- p_curr
      current_vars[params_to_opt] <- par
      full_p <- c(current_vars, fit_fixed_p)
      if (!is.null(full_p$V_inj_vec) && !is.null(current_vars$V_init)) {
        full_p$V_inj_vec[1] <- current_vars$V_init
      }
      
      # 1. 运行模拟
      sim_res <- tryCatch({ calculate_simulation(full_p, active_paths) }, error=function(e) NULL)
      if(is.null(sim_res) || any(!is.finite(sim_res$dQ_App))) return(1e20) # 惩罚项
      
      # 2. 针数对齐 (Injection Alignment)
      # 仅比较用户选定范围内的针数 (Needle Index)
      max_idx <- min(nrow(sim_res), nrow(exp_df))
      valid_idx <- range_lim[1]:range_lim[2]
      valid_idx <- valid_idx[valid_idx <= max_idx]
      
      if(length(valid_idx)==0) return(1e20)
      
      # 3. 计算残差
      residuals <- sim_res$dQ_App[valid_idx] - exp_df$Heat_Raw[valid_idx]
      
      # 4. 应用加权和/或鲁棒回归
      # 如果启用加权，使用预计算的权重（需要重新索引以匹配当前valid_idx）
      current_weights <- NULL
      if(isTRUE(use_weighted) && !is.null(weights)) {
        # 重新计算权重以匹配当前的valid_idx（因为valid_idx可能因模拟结果而变化）
        current_weights <- calculate_weights_from_derivative(exp_df, valid_idx)
      }
      
      # 计算损失
      loss <- calculate_weighted_robust_loss(
        residuals = residuals,
        weights = current_weights,
        use_huber = isTRUE(use_robust),
        huber_delta = huber_delta
      )
      
      return(loss)
    }
    
    # 更新进度：预检查阶段
    if(!is.null(progress)) {
      progress$set(value = 0.1, detail = tr("fit_progress_precheck", lang_val))
    }
    
    # 拟合前预检 (Pre-check) - 增强诊断信息
    # 先进行详细检查，提供更具体的错误信息
    full_p_check <- c(p_curr, fit_fixed_p)
    if (!is.null(full_p_check$V_inj_vec) && !is.null(p_curr$V_init)) {
      full_p_check$V_inj_vec[1] <- p_curr$V_init
    }
    sim_res_check <- tryCatch({ 
      calculate_simulation(full_p_check, active_paths) 
    }, error = function(e) {
      return(list(error_msg = paste(tr("fit_error_sim_calc", lang_val), e$message)))
    })
    
    # 检查1: 模拟是否成功
    if(is.list(sim_res_check) && !is.null(sim_res_check$error_msg)) {
      return(list(error = paste(tr("fit_error_cannot_start", lang_val), sim_res_check$error_msg)))
    }
    if(is.null(sim_res_check)) {
      return(list(error = tr("fit_error_sim_null", lang_val)))
    }
    
    # 检查2: 模拟结果是否有效
    if(any(!is.finite(sim_res_check$dQ_App))) {
      n_inf <- sum(!is.finite(sim_res_check$dQ_App))
      return(list(error = trf("fit_error_invalid_values", lang_val, count = n_inf)))
    }
    
    # 检查3: 拟合区间是否有效
    max_idx_check <- min(nrow(sim_res_check), nrow(exp_df))
    valid_idx_check <- range_lim[1]:range_lim[2]
    valid_idx_check <- valid_idx_check[valid_idx_check <= max_idx_check]
    if(length(valid_idx_check) == 0) {
      return(list(error = trf("fit_error_invalid_range", lang_val, 
                              sim_count = nrow(sim_res_check), 
                              exp_count = nrow(exp_df),
                              start = range_lim[1], 
                              end = range_lim[2])))
    }
    
    # 检查4: 计算初始RSS
    initial_rss <- obj_fun(par_vec)
    if(initial_rss >= 1e19) {
      # 尝试计算实际RSS以提供更多信息
      tryCatch({
        actual_rss <- sum((sim_res_check$dQ_App[valid_idx_check] - exp_df$Heat_Raw[valid_idx_check])^2)
        if(is.finite(actual_rss)) {
          return(list(error = trf("fit_error_rss_too_large", lang_val, rss = formatC(actual_rss, format="e", digits=2))))
        }
      }, error = function(e) {})
      return(list(error = tr("fit_error_no_match", lang_val)))
    }
    
    # 使用常量定义获取参数边界
    # [注释] V_inj 在 UI 中是 uL，get_parameter_bounds 也期望 uL
    v_inj_val <- safe_numeric(input$V_inj, default = UI_DEFAULTS$v_inj_default * 1000, min = 0)
    bounds <- get_parameter_bounds(names(par_vec), v_inj = v_inj_val)
    lower_b <- bounds$lower
    upper_b <- bounds$upper
    
    # 记录边界信息（用于调试）
    log_info(sprintf("参数边界设置完成，共 %d 个参数待优化", length(par_vec)), 
            context = "perform_fitting_sync")
    
    # 更新进度：开始拟合
    if(!is.null(progress)) {
      algo_name <- if(use_DE) tr("fit_progress_de", lang_val) else tr("fit_progress_lbfgsb", lang_val)
      progress$set(value = 0.2, detail = algo_name)
    }
    
    res <- NULL
    
    if (use_DE) {
      # --- DE (Differential Evolution) 全局搜索 ---
      if (!requireNamespace("DEoptim", quietly = TRUE)) {
        handle_error(
          simpleError("DEoptim 包未安装"),
          context = "依赖检查",
          lang_val = lang_val,
          show_to_user = FALSE
        )
        return(list(error = tr("fit_error_deoptim_required", lang_val)))
      }
      
      # 包装 obj_fun 以便 DEoptim 调用 (DEoptim 可能会传入无名向量)
      de_obj_fun <- function(p) {
        names(p) <- names(par_vec) # 恢复名字
        obj_fun(p)
      }
      
      # 使用常量定义计算种群大小
      pop_size <- max(
        DE_OPTIM$pop_size_multiplier * length(par_vec),
        DE_OPTIM$min_pop_size
      )
      # 限制最大种群大小（防止过大导致性能问题）
      pop_size <- min(pop_size, DE_OPTIM$max_pop_size)
      
      log_info(sprintf("DE优化参数：种群大小=%d, 最大迭代=%d", pop_size, max_iters),
              context = "perform_fitting_sync")
      
      # 使用 safe_execute 包装 DEoptim 调用
      out <- safe_execute({
        DEoptim::DEoptim(
          fn = de_obj_fun,
          lower = lower_b,
          upper = upper_b,
          control = list(
            itermax = max_iters,
            NP = pop_size,
            trace = DE_OPTIM$trace,
            F = DE_OPTIM$F,
            CR = DE_OPTIM$CR,
            strategy = DE_OPTIM$strategy
          )
        )
      },
      context = "DE全局优化",
      lang_val = lang_val,
      show_error = FALSE,  # 不立即显示错误，由后续代码处理
      default = NULL
      )
      
      # 检查优化是否成功
      if(is.null(out)) {
        log_warning("DE优化失败", context = "perform_fitting_sync")
        return(list(error = tr("fit_error_process", lang_val)))
      }
      
      # 提取结果并适配格式
      res <- list(
        par = out$optim$bestmem,
        value = out$optim$bestval
      )
      names(res$par) <- names(par_vec) # 确保名字正确
      
      log_info(sprintf("DE优化完成，目标函数值=%.4f", res$value),
              context = "perform_fitting_sync")
      
      # 更新进度：DE拟合完成
      if(!is.null(progress)) {
        progress$set(value = 0.8, detail = tr("fit_progress_de_done", lang_val))
      }
      
    } else {
      # --- 常规 L-BFGS-B 局部优化 ---
      log_info(sprintf("L-BFGS-B优化参数：最大迭代=%d", max_iters),
              context = "perform_fitting_sync")
      
      res <- safe_execute({
        optim(
          par = par_vec,
          fn = obj_fun,
          method = "L-BFGS-B",
          lower = lower_b,
          upper = upper_b,
          control = list(
            factr = LBFGS_OPTIM$factr,
            maxit = max_iters,
            pgtol = LBFGS_OPTIM$pgtol
          )
        )
      },
      context = "L-BFGS-B局部优化",
      lang_val = lang_val,
      show_error = FALSE,  # 不立即显示错误，由后续代码处理
      default = NULL
      )
      
      # 检查优化是否成功
      if(!is.null(res)) {
        log_info(sprintf("L-BFGS-B优化完成，目标函数值=%.4f, 收敛=%d", 
                        res$value, res$convergence),
                context = "perform_fitting_sync")
      } else {
        log_warning("L-BFGS-B优化失败", context = "perform_fitting_sync")
      }
      
      # 更新进度：L-BFGS-B拟合完成
      if(!is.null(progress)) {
        progress$set(value = 0.8, detail = tr("fit_progress_lbfgsb_done", lang_val))
      }
    }
    
    if(is.null(res)) {
      return(list(error = tr("fit_error_process", lang_val)))
    }
    
    # 更新进度：准备返回结果
    if(!is.null(progress)) {
      progress$set(value = 0.9, detail = tr("fit_progress_prepare", lang_val))
    }
    
    # 准备返回结果
    result <- list(
      res = res,
      obj_fun = obj_fun,
      par_vec = par_vec,
      range_lim = range_lim,
      exp_df = exp_df,
      enable_error_analysis = enable_error_analysis
    )
    
    return(result)
  }
  
  # 同步拟合函数 - 使用进度条提示用户等待
  perform_fitting <- function(max_iters, use_DE = FALSE) {
    # 允许实验数据来自文件导入或 模拟→实验；无数据时阻止拟合
    exp_df <- exp_data_processed()
    req(!is.null(exp_df), nrow(exp_df) > 0)

    V_inj_vec <- if ("V_inj_uL" %in% names(exp_df)) {
      exp_df$V_inj_uL
    } else {
      rep(safe_numeric(input$V_inj, default = UI_DEFAULTS$v_inj_default * 1000, 
                       min = UI_DEFAULTS$v_inj_min * 1000, max = UI_DEFAULTS$v_inj_max * 1000),
          nrow(exp_df))
    }
    if (any(is.na(V_inj_vec))) {
      V_inj_vec[is.na(V_inj_vec)] <- safe_numeric(input$V_inj, default = UI_DEFAULTS$v_inj_default * 1000, 
                                                  min = UI_DEFAULTS$v_inj_min * 1000, max = UI_DEFAULTS$v_inj_max * 1000)
    }
    
    # 检查是否已有拟合在进行
    if(values$is_fitting) {
      showNotification(tr("fit_error_in_progress", lang()), type = "warning", duration = 3)
      return()
    }
    
    values$is_fitting <- TRUE
    
    # 使用 safe_numeric 准备当前参数，确保输入有效
    p_curr <- list(
      logK1 = safe_numeric(input$logK1, default = DEFAULT_PARAMS$logK, 
                          min = PARAM_BOUNDS$logK["lower"], max = PARAM_BOUNDS$logK["upper"]),
      H1 = safe_numeric(input$H1, default = DEFAULT_PARAMS$H, 
                       min = PARAM_BOUNDS$H["lower"], max = PARAM_BOUNDS$H["upper"]),
      logK2 = safe_numeric(input$logK2, default = DEFAULT_PARAMS$logK, 
                          min = PARAM_BOUNDS$logK["lower"], max = PARAM_BOUNDS$logK["upper"]),
      H2 = safe_numeric(input$H2, default = DEFAULT_PARAMS$H, 
                       min = PARAM_BOUNDS$H["lower"], max = PARAM_BOUNDS$H["upper"]),
      logK3 = safe_numeric(input$logK3, default = DEFAULT_PARAMS$logK, 
                          min = PARAM_BOUNDS$logK["lower"], max = PARAM_BOUNDS$logK["upper"]),
      H3 = safe_numeric(input$H3, default = DEFAULT_PARAMS$H, 
                       min = PARAM_BOUNDS$H["lower"], max = PARAM_BOUNDS$H["upper"]),
      logK4 = safe_numeric(input$logK4, default = DEFAULT_PARAMS$logK, 
                          min = PARAM_BOUNDS$logK["lower"], max = PARAM_BOUNDS$logK["upper"]),
      H4 = safe_numeric(input$H4, default = DEFAULT_PARAMS$H, 
                       min = PARAM_BOUNDS$H["lower"], max = PARAM_BOUNDS$H["upper"]),
      logK5 = safe_numeric(input$logK5, default = DEFAULT_PARAMS$logK, 
                          min = PARAM_BOUNDS$logK["lower"], max = PARAM_BOUNDS$logK["upper"]),
      H5 = safe_numeric(input$H5, default = DEFAULT_PARAMS$H, 
                       min = PARAM_BOUNDS$H["lower"], max = PARAM_BOUNDS$H["upper"]),
      logK6 = safe_numeric(input$logK6, default = DEFAULT_PARAMS$logK, 
                          min = PARAM_BOUNDS$logK["lower"], max = PARAM_BOUNDS$logK["upper"]),
      H6 = safe_numeric(input$H6, default = DEFAULT_PARAMS$H, 
                       min = PARAM_BOUNDS$H["lower"], max = PARAM_BOUNDS$H["upper"]),
      fH = safe_numeric(input$factor_H, default = DEFAULT_PARAMS$fH, 
                       min = PARAM_BOUNDS$fH_fG["lower"], max = PARAM_BOUNDS$fH_fG["upper"]),
      fG = safe_numeric(input$factor_G, default = DEFAULT_PARAMS$fG, 
                       min = PARAM_BOUNDS$fH_fG["lower"], max = PARAM_BOUNDS$fH_fG["upper"]),
      V_init = safe_numeric(input$V_init_val, default = DEFAULT_PARAMS$V_init, min = 0),
      Offset = safe_numeric(input$heat_offset, default = DEFAULT_PARAMS$Offset, 
                           min = PARAM_BOUNDS$Offset["lower"], max = PARAM_BOUNDS$Offset["upper"])
    )
    
    # 使用 safe_numeric 准备固定参数
    # 注意：这些参数将传给 calculate_simulation，需要保持 UI 单位（浓度为 mM，体积为 uL）
    fixed_p <- list(
      H_cell_0 = safe_numeric(input$H_cell_0, default = UI_DEFAULTS$conc_cell_default, min = 0),
      G_syringe = safe_numeric(input$G_syringe, default = UI_DEFAULTS$conc_syringe_default, min = 0),
      V_cell = safe_numeric(input$V_cell, default = UI_DEFAULTS$v_cell_default, 
                           min = UI_DEFAULTS$v_cell_min, max = UI_DEFAULTS$v_cell_max),
      # [重要] V_inj_vec 在导入数据中是 uL，calculate_simulation 内部会除以 1000，所以这里不转换
      V_inj_vec = V_inj_vec,
      n_inj = length(V_inj_vec)
    )
    
    # 确定要优化的参数子集
    params_to_opt <- input$fit_params
    range_lim <- input$fit_data_range # 针数范围 c(start_idx, end_idx)
    if (length(params_to_opt) == 0) {
      values$is_fitting <- FALSE
      return()
    }
    
    # 确定算法名称
    algo_name <- if(use_DE) tr("fit_progress_de", lang()) else tr("fit_progress_lbfgsb", lang())
    
    # 创建进度对象
    progress <- Progress$new(session, min = 0, max = 1)
    progress$set(value = 0, detail = algo_name)
    
    # 确保在函数退出时关闭进度条和重置状态
    on.exit({ 
      values$is_fitting <- FALSE
      progress$close()
    })
    
    # 获取加权和鲁棒回归设置
    use_weighted <- isTRUE(input$use_weighted_fitting)
    use_robust <- isTRUE(input$use_robust_fitting)
    # [修复] 使用常量定义的 huber_delta 参数
    huber_delta_input <- if(use_robust) {
      safe_numeric(input$huber_delta, 
                   default = HUBER_PARAMS$delta_default, 
                   min = HUBER_PARAMS$delta_min, 
                   max = HUBER_PARAMS$delta_max)
    } else {
      NULL
    }
    
    # 验证 max_iters 参数
    max_iters <- safe_numeric(max_iters, 
                             default = if(use_DE) DE_OPTIM$max_iter else LBFGS_OPTIM$max_iter,
                             min = 10, 
                             max = 1000)
    
    # 安全获取当前语言值
    current_lang_val <- tryCatch(lang(), error = function(e) "en")
    if(is.null(current_lang_val) || length(current_lang_val) == 0 || !current_lang_val %in% c("zh", "en")) {
      current_lang_val <- "en"
    }
    
    result <- perform_fitting_sync(
      max_iters = max_iters,
      use_DE = use_DE,
      exp_df = exp_df,
      p_curr = p_curr,
      fixed_p = fixed_p,
      params_to_opt = params_to_opt,
      range_lim = range_lim,
      active_paths = input$active_paths,
      enable_error_analysis = if(!is.null(input$enable_error_analysis)) input$enable_error_analysis else FALSE,
      use_weighted = use_weighted,
      use_robust = use_robust,
      huber_delta = huber_delta_input,
      progress = progress,
      lang_val = current_lang_val
    )
    
    # 处理结果
    if(!is.null(result$error)) {
      progress$set(value = 1, detail = tr("fit_progress_error", lang()))
      showNotification(result$error, type = "error", duration = 10)
      return()
    }
    
    # 更新进度：处理结果
    progress$set(value = 0.95, detail = tr("fit_progress_update", lang()))
    
    # 更新参数和误差分析
    res <- result$res
    obj_fun <- result$obj_fun
    par_vec <- result$par_vec
    range_lim <- result$range_lim
    exp_df <- result$exp_df
    enable_error_analysis <- result$enable_error_analysis
    
    # 更新参数值
    new_vals <- res$par
    if (is.null(names(new_vals)) && length(new_vals) == length(par_vec)) names(new_vals) <- names(par_vec)
    for(nm in names(new_vals)) {
      val <- new_vals[[nm]]
      if(grepl("Offset", nm)) updateSliderInput(session, "heat_offset", value=val)
      else if(nm=="fH") updateNumericInput(session, "factor_H", value=val)
      else if(nm=="fG") updateNumericInput(session, "factor_G", value=val)
      else if(nm=="V_init") updateNumericInput(session, "V_init_val", value=val)
      else updateSliderInput(session, nm, value=val)
    }
    
    # 误差分析
    if (enable_error_analysis) {
      # [修复] 使用翻译函数而不是硬编码中文
      progress$set(value = 0.97, detail = tr("fit_progress_error_analysis", lang()))
      tryCatch({
        valid_idx <- range_lim[1]:range_lim[2]
        max_idx <- min(nrow(exp_df), max(valid_idx, na.rm = TRUE))
        valid_idx <- valid_idx[valid_idx <= max_idx]
        n_data <- length(valid_idx)
        
        if (n_data <= length(new_vals)) {
          values$error_analysis <- NULL
          values$error_analysis_info <- NULL
          values$correlation_matrix <- NULL
          showNotification(tr("fit_error_insufficient_data", lang()), type = "warning", duration = 3)
        } else {
          final_rss <- res$value
          error_result <- calculate_hessian_ci_robust(
            obj_fun = obj_fun,
            par_opt = new_vals,
            n_data = n_data,
            rss = final_rss,
            conf_level = 0.95
          )
          
          if (!is.null(error_result) && nrow(error_result) > 0) {
            # 保存完整的误差分析结果（用于相关性矩阵计算）
            error_result_full <- error_result
            # [Updated] Use full error result in values$error_analysis for Report and Snapshots
            # Filtering for table display is moved to renderDT
            
            if (nrow(error_result) > 0) {
              values$error_analysis <- error_result
              dof <- n_data - length(new_vals)
              # 不存储翻译后的 reliability 文案，仅存 dof/color；渲染时按当前 lang() 用 tr() 取文，以便切换语言后正确更新
              reliability_color <- if (dof >= 10) "#27ae60" else if (dof >= 5) "#f39c12" else "#e74c3c"
              
              values$error_analysis_info <- list(
                n_data = n_data,
                n_params = length(new_vals),
                dof = dof,
                reliability_color = reliability_color
              )
              
              # [新增] 提取协方差矩阵并计算相关性矩阵
              # 使用完整的误差分析结果来获取协方差矩阵（包含所有拟合参数）
              cov_matrix <- attr(error_result_full, "cov_matrix")
              if (!is.null(cov_matrix) && is.matrix(cov_matrix)) {
                tryCatch({
                  # 使用所有拟合参数（new_vals 包含所有被拟合的参数）
                  param_names <- names(new_vals)
                  
                  # 如果协方差矩阵有行列名，使用它们（更可靠）
                  if (!is.null(rownames(cov_matrix)) && !is.null(colnames(cov_matrix))) {
                    # 确保行列名与参数名匹配
                    if (all(param_names %in% rownames(cov_matrix)) && 
                        all(param_names %in% colnames(cov_matrix))) {
                      # 按照参数顺序提取协方差矩阵
                      cov_matrix_ordered <- cov_matrix[param_names, param_names, drop = FALSE]
                    } else {
                      # 如果行列名不匹配，但维度匹配，直接使用
                      if (nrow(cov_matrix) == length(param_names) && 
                          ncol(cov_matrix) == length(param_names)) {
                        cov_matrix_ordered <- cov_matrix
                        rownames(cov_matrix_ordered) <- param_names
                        colnames(cov_matrix_ordered) <- param_names
                      } else {
                        cov_matrix_ordered <- NULL
                      }
                    }
                  } else {
                    # 如果协方差矩阵没有行列名，但维度匹配，直接使用并设置行列名
                    if (nrow(cov_matrix) == length(param_names) && 
                        ncol(cov_matrix) == length(param_names)) {
                      cov_matrix_ordered <- cov_matrix
                      rownames(cov_matrix_ordered) <- param_names
                      colnames(cov_matrix_ordered) <- param_names
                    } else {
                      cov_matrix_ordered <- NULL
                    }
                  }
                  
                  if (!is.null(cov_matrix_ordered)) {
                    # 检查协方差矩阵的对角线是否都是正数（方差必须为正）
                    diag_vals <- diag(cov_matrix_ordered)
                    valid_idx <- is.finite(diag_vals) & diag_vals > 0
                    
                    if (sum(valid_idx) > 1) {  # 至少需要2个有效参数才能计算相关性
                      # 只保留有效的参数
                      param_names_valid <- rownames(cov_matrix_ordered)[valid_idx]
                      cov_matrix_filtered <- cov_matrix_ordered[valid_idx, valid_idx, drop = FALSE]
                      
                      # 再次检查对角线（防止数值误差）
                      diag_vals_filtered <- diag(cov_matrix_filtered)
                      if (all(is.finite(diag_vals_filtered)) && all(diag_vals_filtered > 0)) {
                        # 计算相关性矩阵（使用 suppressWarnings 避免警告）
                        cor_matrix <- suppressWarnings(cov2cor(cov_matrix_filtered))
                        # 检查结果是否有效
                        if (all(is.finite(cor_matrix))) {
                          rownames(cor_matrix) <- param_names_valid
                          colnames(cor_matrix) <- param_names_valid
                          values$correlation_matrix <- cor_matrix
                        } else {
                          values$correlation_matrix <- NULL
                        }
                      } else {
                        values$correlation_matrix <- NULL
                      }
                    } else {
                      values$correlation_matrix <- NULL
                    }
                  } else {
                    values$correlation_matrix <- NULL
                  }
                }, error = function(e) {
                  values$correlation_matrix <- NULL
                })
              } else {
                values$correlation_matrix <- NULL
              }
            } else {
              values$error_analysis <- NULL
              values$error_analysis_info <- NULL
              values$correlation_matrix <- NULL
            }
          } else {
            values$error_analysis <- NULL
            values$error_analysis_info <- NULL
            values$correlation_matrix <- NULL
          }
        }
      }, error = function(e) {
        values$error_analysis <- NULL
        values$error_analysis_info <- NULL
        values$correlation_matrix <- NULL
        showNotification(paste(tr("fit_error_analysis_error", lang()), e$message), type = "error", duration = 5)
      })
    } else {
      values$error_analysis <- NULL
      values$error_analysis_info <- NULL
      values$correlation_matrix <- NULL
    }
    
    # [新增] 计算残差数据（仅在误差分析启用时计算）
    if (enable_error_analysis) {
      tryCatch({
        # 使用最优参数重新构建完整参数列表
        full_p_fitted <- c(p_curr, fixed_p)
        full_p_fitted[params_to_opt] <- new_vals
        if (!is.null(full_p_fitted$V_inj_vec) && "V_init" %in% names(full_p_fitted)) {
          full_p_fitted$V_inj_vec[1] <- full_p_fitted$V_init
        }
        
        # 计算拟合后的模拟结果
        sim_res_fitted <- calculate_simulation(full_p_fitted, input$active_paths)
        
        if (!is.null(sim_res_fitted)) {
          # 使用与拟合时相同的 range_lim 和 valid_idx
          valid_idx <- range_lim[1]:range_lim[2]
          max_idx <- min(nrow(sim_res_fitted), nrow(exp_df))
          valid_idx <- valid_idx[valid_idx <= max_idx]
          
          if (length(valid_idx) > 0) {
            # 构建残差数据框（与主图一致：热量用 kcal/mol，与轴标签「kcal/mol」一致）
            fit_cal <- sim_res_fitted$dQ_App[valid_idx]
            obs_cal <- exp_df$Heat_Raw[valid_idx]
            residuals_data <- data.frame(
              Inj = valid_idx,
              Fitted = fit_cal / 1000,
              Observed = obs_cal / 1000,
              Residual = (obs_cal - fit_cal) / 1000,
              Ratio_App = sim_res_fitted$Ratio_App[valid_idx],
              stringsAsFactors = FALSE
            )
            values$residuals_data <- residuals_data
          } else {
            values$residuals_data <- NULL
          }
        } else {
          values$residuals_data <- NULL
        }
      }, error = function(e) {
        values$residuals_data <- NULL
      })
    } else {
      values$residuals_data <- NULL
    }
    
    # 完成
    progress$set(value = 1, detail = tr("fit_progress_done", lang()))
    algo_name_en <- if(use_DE) "DE Global Fit" else "L-BFGS-B Fit"
    showNotification(paste(algo_name_en, "Complete"), type="message")
  }
  
  observeEvent(input$fit_1_step, { 
    if(lang_switching()) return()  # [修复] 防止在语言切换时触发
    perform_fitting(1) 
  }, ignoreInit = TRUE)
  observeEvent(input$fit_10_step, { 
    if(lang_switching()) return()
    perform_fitting(10) 
  }, ignoreInit = TRUE)
  observeEvent(input$fit_full, { 
    if(lang_switching()) return()
    perform_fitting(100) 
  }, ignoreInit = TRUE)
  observeEvent(input$fit_global, { 
    if(lang_switching()) return()
    perform_fitting(50, use_DE = TRUE) 
  }, ignoreInit = TRUE) # DE 50代通常能找到不错的区域
  
  # [新增] 显示误差分析可靠性评价
  output$error_analysis_reliability <- renderUI({
    info <- values$error_analysis_info
    if (is.null(info)) {
      return(NULL)
    }
    
    rel_key <- if (info$dof >= 10) "error_analysis_applicability_1" else if (info$dof >= 5) "error_analysis_applicability_2" else "error_analysis_applicability_3"
    div(style = "background-color: #f8f9fa; padding: 10px; border-left: 4px solid #3498db; margin-bottom: 8px; border-radius: 4px;",
        div(style = "display: flex; align-items: center; margin-bottom: 5px;",
            strong(tr("error_analysis_reliability", lang()), style = "margin-right: 10px;"),
            span(style = paste0("color: ", info$reliability_color, "; font-weight: bold; font-size: 1.1em;"),
                 tr(rel_key, lang()))
        ),
        div(style = "font-size: 0.85em; color: #555;",
            p(style = "margin: 2px 0;", 
              paste0(tr("error_analysis_data_points", lang()), info$n_data, " | ",
                     tr("error_analysis_fit_params", lang()), info$n_params, " | ",
                     tr("error_analysis_dof", lang()), info$dof)),
            p(style = "margin: 2px 0; font-style: italic;",
              if (info$dof >= 10) {
                tr("error_analysis_reliability_good", lang())
              } else if (info$dof >= 5) {
                tr("error_analysis_reliability_moderate", lang())
              } else {
                tr("error_analysis_reliability_low", lang())
              })
        )
    )
  })
  
  output$itcPlot <- renderPlot({
    # 初始化空图
    p <- ggplot() + theme_minimal(base_size=14) + theme(legend.position="bottom")
    
    sim <- sim_results()
    exp_df <- tryCatch(exp_data_processed(), error=function(e) NULL)
    
    # 准备 X 轴标签函数 (Bottom: Sim Ratio)
    # 使用函数闭包来处理可能的 NULL sim
    fmt_ratio <- function(x) formatC(x, format="f", digits=2)
    
    # 获取最大针数，用于设置 X 轴范围
    max_inj <- 0
    if(!is.null(sim)) max_inj <- max(max_inj, nrow(sim))
    if(!is.null(exp_df)) max_inj <- max(max_inj, nrow(exp_df))
    if(max_inj == 0) {
      # [修复] 优先使用用户输入的 n_inj 作为回退值，确保与设定一致
      max_inj <- if(!is.null(input$n_inj)) input$n_inj else UI_DEFAULTS$n_inj_default
    }
    
    # 构建 Bottom Axis (Sim Ratio) 的 Breaks 和 Labels
    # 我们每隔 5 针显示一个标签，避免拥挤
    breaks_seq <- seq(1, max_inj, by=5)
    
    labels_bottom <- function(breaks) {
      if(is.null(sim)) return(breaks)
      # 找到 breaks 对应的 Ratio_App
      ratios <- sim$Ratio_App[match(breaks, sim$Inj)]
      # 处理 NA (超出范围)
      ratios[is.na(ratios)] <- 0
      fmt_ratio(ratios)
    }
    
    # 构建 Top Axis (Exp Ratio) 的 Labels
    labels_top <- function(breaks) {
      if(is.null(exp_df)) return(rep("", length(breaks)))
      ratios <- exp_df$Ratio_Raw[match(breaks, exp_df$Inj)]
      ratios[is.na(ratios)] <- 0
      fmt_ratio(ratios)
    }
    
    # 设置 X 轴 (双轴系统)
    # 主轴: Injection Index -> 显示 Sim Ratio
    # 次轴: Injection Index -> 显示 Exp Ratio
    p <- p + scale_x_continuous(
      name = "Simulated G/H Ratio",
      breaks = breaks_seq,
      labels = labels_bottom,
      sec.axis = sec_axis(~., name = "Experimental G/H Ratio", breaks = breaks_seq, labels = labels_top)
    ) + labs(y = "Heat per Inj (kcal/mol)")
    
    # 纵坐标统一为 kcal/mol：内部数据为 cal/mol，绘图时除以 1000（与 ITCprocessor 积分图一致）
    if(!is.null(sim) && nrow(sim) > 0) {
      sim_plot <- sim %>% mutate(dQ_kcal = dQ_App / 1000)
      p <- p + geom_line(data=sim_plot, aes(x=Inj, y=dQ_kcal, color="Simulation"), linewidth=1)
      sim_ok <- sim_plot[sim_plot$Fallback == 0, ]
      if(nrow(sim_ok) > 0) {
        p <- p + geom_point(data=sim_ok, aes(x=Inj, y=dQ_kcal, color="Simulation"), size=2)
      }
      sim_fb <- sim_plot[sim_plot$Fallback == 1, ]
      if(nrow(sim_fb) > 0) {
        p <- p + geom_point(data=sim_fb, aes(x=Inj, y=dQ_kcal, color="Solver Fail"), size=3, shape=1, stroke=1.5)
      }
    }
    
    if(!is.null(exp_df)) {
      try({
        range_lim <- input$fit_data_range
        ed <- exp_df %>% mutate(Sel = Inj >= range_lim[1] & Inj <= range_lim[2], Heat_kcal = Heat_Raw / 1000)
        p <- p + geom_point(data=ed[!ed$Sel,], aes(x=Inj, y=Heat_kcal, color="Excluded"), alpha=1, size=3, shape=1)
        p <- p + geom_point(data=ed[ed$Sel,], aes(x=Inj, y=Heat_kcal, color="Experiment"), size=3, shape=18)
      })
    }
    
    p + scale_color_manual(values=c("Simulation"="#e74c3c", "Experiment"="#2980b9", "Excluded"="grey", "Solver Fail"="#8e44ad"))
  })
  
  output$distPlot <- renderPlot({
    sim <- sim_results()
    if(is.null(sim)) return(NULL)
    
    cols_to_plot <- c("H_pct", "M_pct")
    if("rxn_D" %in% input$active_paths) cols_to_plot <- c(cols_to_plot, "D_pct")
    if("rxn_T" %in% input$active_paths) cols_to_plot <- c(cols_to_plot, "T_pct")
    if("rxn_B" %in% input$active_paths) cols_to_plot <- c(cols_to_plot, "B_pct")
    if("rxn_F" %in% input$active_paths && "rxn_D" %in% input$active_paths) cols_to_plot <- c(cols_to_plot, "F_pct")
    if("rxn_U" %in% input$active_paths) cols_to_plot <- c(cols_to_plot, "U_pct")
    
    # [修改] 定义图例标签映射
    lbl_map <- c(
      "H_pct" = "H",
      "M_pct" = "H1G1",
      "D_pct" = "H1G2",
      "T_pct" = "H2G2",
      "B_pct" = "H2G1",
      "F_pct" = "H2G3",
      "U_pct" = "H1G1(U)"
    )

    sim %>% select(Ratio_App, all_of(cols_to_plot)) %>%
      pivot_longer(-Ratio_App, names_to="Species", values_to="Frac") %>%
      # 将 Species 转换为因子并重命名标签，确保顺序和显示正确
      mutate(Species = factor(Species, levels = names(lbl_map), labels = lbl_map)) %>%
      ggplot(aes(x=Ratio_App, y=Frac, fill=Species)) + 
      geom_area(alpha=0.8, color="white", linewidth=0.1) +
      scale_fill_brewer(palette="Set3") + 
      labs(x="Simulated G/H Ratio", y="Fraction (based on H)") +
      theme_minimal(base_size=14) + theme(legend.position="bottom")
  })
  
  # [新增] 残差子标签页按钮事件处理
  observeEvent(input$res_subtab_1, { 
    if(lang_switching()) return()  # [修复] 防止在语言切换时触发
    values$residual_subtab <- "res1" 
  }, ignoreInit = TRUE)
  observeEvent(input$res_subtab_2, { 
    if(lang_switching()) return()
    values$residual_subtab <- "res2" 
  }, ignoreInit = TRUE)
  observeEvent(input$res_subtab_3, { 
    if(lang_switching()) return()
    values$residual_subtab <- "res3" 
  }, ignoreInit = TRUE)
  observeEvent(input$res_subtab_4, { 
    if(lang_switching()) return()
    values$residual_subtab <- "res4" 
  }, ignoreInit = TRUE)
  
  # [新增] 残差诊断按钮UI（动态高亮当前选中的按钮）
  output$residual_buttons <- renderUI({
    if (is.null(values$error_analysis)) return(NULL)
    current_subtab <- if(!is.null(values$residual_subtab)) values$residual_subtab else "res1"
    div(style = "margin-bottom: 10px; display: flex; gap: 5px; flex-wrap: wrap; align-items: center;",
        actionButton("res_subtab_1", tr("residual_tab_1", lang()), 
                   class = if(current_subtab == "res1") "btn btn-info btn-xs" else "btn btn-default btn-xs", 
                   style = "padding: 4px 8px; font-size: 11px; min-width: 100px;"),
        actionButton("res_subtab_2", tr("residual_tab_2", lang()), 
                   class = if(current_subtab == "res2") "btn btn-info btn-xs" else "btn btn-default btn-xs", 
                   style = "padding: 4px 8px; font-size: 11px; min-width: 100px;"),
        actionButton("res_subtab_3", tr("residual_tab_3", lang()), 
                   class = if(current_subtab == "res3") "btn btn-info btn-xs" else "btn btn-default btn-xs", 
                   style = "padding: 4px 8px; font-size: 11px; min-width: 100px;"),
        actionButton("res_subtab_4", tr("residual_tab_4", lang()), 
                   class = if(current_subtab == "res4") "btn btn-info btn-xs" else "btn btn-default btn-xs", 
                   style = "padding: 4px 8px; font-size: 11px; min-width: 100px;")
    )
  })
  
  # [新增] 残差诊断图输出 - 4个子图，根据子标签页选择显示
  output$residualPlot <- renderPlot({
    if (is.null(values$error_analysis) || is.null(values$residuals_data)) {
      return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, 
                     label = tr("residual_plot_required", lang()), size = 5) +
             theme_void())
    }
    
    residuals_data <- values$residuals_data
    selected_subtab <- values$residual_subtab
    
    tryCatch({
      # 根据选中的子标签页显示不同的图
      if (selected_subtab == "res1") {
        # 残差 vs 拟合值
        p <- ggplot(residuals_data, aes(x = Fitted, y = Residual)) +
          geom_point(alpha = 0.6, color = "#3498db", size = 1.5) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.6) +
          geom_smooth(method = "loess", formula = y ~ x, se = TRUE, color = "#e74c3c", linewidth = 0.8, alpha = 0.3) +
          labs(x = tr("residual_plot_x_1", lang()), y = tr("residual_plot_y_1", lang()), title = tr("residual_plot_title_1", lang())) +
          theme_minimal(base_size = 12) +
          theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                plot.margin = margin(5, 5, 5, 5, "pt"))
        print(p)
      } else if (selected_subtab == "res2") {
        # 残差 vs 针数
        p <- ggplot(residuals_data, aes(x = Inj, y = Residual)) +
          geom_point(alpha = 0.6, color = "#3498db", size = 1.5) +
          geom_line(alpha = 0.3, color = "#3498db", linewidth = 0.4) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.6) +
          geom_smooth(method = "loess", formula = y ~ x, se = TRUE, color = "#e74c3c", linewidth = 0.8, alpha = 0.3) +
          labs(x = tr("residual_plot_x_2", lang()), y = tr("residual_plot_y_2", lang()), title = tr("residual_plot_title_2", lang())) +
          theme_minimal(base_size = 12) +
          theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                plot.margin = margin(5, 5, 5, 5, "pt"))
        print(p)
      } else if (selected_subtab == "res3") {
        # 残差直方图
        p <- ggplot(residuals_data, aes(x = Residual)) +
          geom_histogram(bins = 20, fill = "#3498db", alpha = 0.7, color = "white") +
          geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.6) +
          labs(x = tr("residual_plot_x_3", lang()), y = tr("residual_plot_y_3", lang()), title = tr("residual_plot_title_3", lang())) +
          theme_minimal(base_size = 12) +
          theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                plot.margin = margin(5, 5, 5, 5, "pt"))
        print(p)
      } else if (selected_subtab == "res4") {
        # Q-Q 图
        residuals_clean <- residuals_data$Residual[is.finite(residuals_data$Residual)]
        if (length(residuals_clean) > 0) {
          qq_data <- data.frame(
            Theoretical = qqnorm(residuals_clean, plot.it = FALSE)$x,
            Sample = qqnorm(residuals_clean, plot.it = FALSE)$y
          )
          p <- ggplot(qq_data, aes(x = Theoretical, y = Sample)) +
            geom_point(alpha = 0.6, color = "#3498db", size = 1.5) +
            geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 0.6) +
            labs(x = tr("residual_plot_x_4", lang()), y = tr("residual_plot_y_4", lang()), title = tr("residual_plot_title_4", lang())) +
            theme_minimal(base_size = 12) +
            theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                  plot.margin = margin(5, 5, 5, 5, "pt"))
          print(p)
        } else {
          p <- ggplot() + annotate("text", x = 0.5, y = 0.5, label = tr("residual_no_data", lang()), size = 4) + theme_void()
          print(p)
        }
      } else {
        # 默认显示第一个图
        p <- ggplot(residuals_data, aes(x = Fitted, y = Residual)) +
          geom_point(alpha = 0.6, color = "#3498db", size = 1.5) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.6) +
          geom_smooth(method = "loess", formula = y ~ x, se = TRUE, color = "#e74c3c", linewidth = 0.8, alpha = 0.3) +
          labs(x = tr("residual_plot_x_1", lang()), y = tr("residual_plot_y_1", lang()), title = tr("residual_plot_title_1", lang())) +
          theme_minimal(base_size = 12) +
          theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                plot.margin = margin(5, 5, 5, 5, "pt"))
        print(p)
      }
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                label = paste("绘图出错:", e$message), size = 4) +
        theme_void()
    })
  })
  
  # [新增] 参数相关性矩阵图输出
  output$correlationPlot <- renderPlot({
    # 仅在误差分析完成时显示
    if (is.null(values$error_analysis) || is.null(values$correlation_matrix)) {
      return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, 
                     label = tr("residual_plot_required", lang()), size = 5) +
             theme_void())
    }
    
    tryCatch({
      plot_correlation_matrix(values$correlation_matrix, lang = lang())
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                label = paste("绘图出错:", e$message), size = 4) +
        theme_void()
    })
  })
  
  # [新增] 动态生成绘图标签页 - 所有标签页在一个tabsetPanel中，通过CSS分成两行显示
  output$plot_tabs <- renderUI({
    # 基础标签页（始终显示）
    tabs <- list(
      tabPanel(tr("plot_tab_itc", lang()), value = "tab_itc", br(), plotOutput("itcPlot", height = "460px")),
      tabPanel(tr("plot_tab_dist", lang()), value = "tab_dist", br(), plotOutput("distPlot", height = "460px"))
    )
    
    # 如果误差分析已完成，添加残差诊断和参数相关性标签页
    if (!is.null(values$error_analysis)) {
      # 残差诊断标签页：使用按钮切换不同的子图
      tabs <- c(tabs, list(
        tabPanel(tr("plot_tab_residuals", lang()), value = "tab_residuals", 
                 uiOutput("residual_buttons"),
                 # 共享的绘图区域（增加高度以确保有足够空间，减少边距）
                 plotOutput("residualPlot", height = "420px")
        ),
        tabPanel(tr("plot_tab_corr", lang()), value = "tab_corr", br(), plotOutput("correlationPlot", height = "460px"))
      ))
    }
    
    # 创建tabsetPanel并添加自定义CSS类
    tabset <- do.call(tabsetPanel, c(list(type = "tabs", id = "main_plot_tabs"), tabs))
    
    # 包装在div中，添加自定义CSS类用于控制标签页换行
    div(class = "two-row-tabs", tabset)
  })
  
  # ============================================================================
  #  [MODIFIED] Section: 参数快照、RSS计算、导入导出
  # ============================================================================
  
  # --- [修复] 保存快照 (增加命名逻辑 + 高精度 RSS) ---
  observeEvent(input$save_params, {
    # [修复] 防止在语言切换时触发
    if(lang_switching()) return()
    # 1. 直接获取当前计算好的 RSS (确保与 UI 显示一致)
    rss_info <- current_rss_val()
    current_rss <- rss_info$rss
    
    # 2. 【核心修改】构建名称 (Name)
    # 格式：YYYY-MM-DD HH:MM [- UserNote]
    time_str <- format(Sys.time(), "%Y-%m-%d %H:%M")
    user_note <- trimws(input$snap_name) # 去除首尾空格
    
    final_name <- if(nchar(user_note) > 0) paste(time_str, user_note, sep=" - ") else time_str
    
    # 3. 构造数据行 (用 Name 替代原来的 Time 列)
    # [新增] 包含实验条件，放在最后几列
    # [新增] 包含标准误差数据（如果有误差分析结果）
    
    # 获取误差分析结果（如果存在）
    error_df <- values$error_analysis
    se_values <- list()  # 存储标准误差值
    
    # 定义所有可能的参数名
    all_params <- c("logK1", "H1", "logK2", "H2", "logK3", "H3", 
                    "logK4", "H4", "logK5", "H5", "logK6", "H6",
                    "fH", "fG", "V_init", "Offset")
    
    # 为每个参数查找标准误差
    for (param in all_params) {
      if (!is.null(error_df) && param %in% error_df$Parameter) {
        se_val <- error_df$SE[error_df$Parameter == param]
        se_values[[param]] <- if(length(se_val) > 0 && is.finite(se_val)) se_val else NA
      } else {
        se_values[[param]] <- NA
      }
    }
    
    # [新增] 未激活的模型路径对应参数存 NA，其标准误差也置空
    active_paths_save <- if(is.null(input$active_paths) || length(input$active_paths) == 0) character(0) else input$active_paths
    if(!"rxn_D" %in% active_paths_save) { se_values[["logK2"]] <- NA; se_values[["H2"]] <- NA }
    if(!"rxn_T" %in% active_paths_save) { se_values[["logK3"]] <- NA; se_values[["H3"]] <- NA }
    if(!"rxn_B" %in% active_paths_save) { se_values[["logK4"]] <- NA; se_values[["H4"]] <- NA }
    if(!"rxn_F" %in% active_paths_save) { se_values[["logK5"]] <- NA; se_values[["H5"]] <- NA }
    if(!"rxn_U" %in% active_paths_save) { se_values[["logK6"]] <- NA; se_values[["H6"]] <- NA }
    
    # 格式化标准误差（logK保留3位小数，H保留1位小数，其他保留3位小数）
    format_se <- function(param, se) {
      if (is.na(se) || !is.finite(se)) return("")
      if (grepl("logK", param)) {
        return(sprintf("%.3f", se))
      } else if (grepl("^H[0-9]$", param)) {
        return(sprintf("%.1f", se))
      } else {
        return(sprintf("%.3f", se))
      }
    }
    
    # [新增] 未激活路径对应的参数存 NA
    logK2_save <- if("rxn_D" %in% active_paths_save) input$logK2 else NA
    H2_save    <- if("rxn_D" %in% active_paths_save) input$H2 else NA
    logK3_save <- if("rxn_T" %in% active_paths_save) input$logK3 else NA
    H3_save    <- if("rxn_T" %in% active_paths_save) input$H3 else NA
    logK4_save <- if("rxn_B" %in% active_paths_save) input$logK4 else NA
    H4_save    <- if("rxn_B" %in% active_paths_save) input$H4 else NA
    logK5_save <- if("rxn_F" %in% active_paths_save) input$logK5 else NA
    H5_save    <- if("rxn_F" %in% active_paths_save) input$H5 else NA
    logK6_save <- if("rxn_U" %in% active_paths_save) input$logK6 else NA
    H6_save    <- if("rxn_U" %in% active_paths_save) input$H6 else NA
    
    new_row <- data.frame(
      Name = final_name, # 第一列现在是组合名称
      RSS = if(is.na(current_rss)) "-" else formatC(current_rss, format="e", digits=3),
      # [修正] 强制包含 rxn_M，因为它总是激活的
      Model = paste(c("rxn_M", input$active_paths), collapse="+"),
      
      logK1=input$logK1, H1=input$H1, logK2=logK2_save, H2=H2_save,
      logK3=logK3_save, H3=H3_save, logK4=logK4_save, H4=H4_save,
      logK5=logK5_save, H5=H5_save, logK6=logK6_save, H6=H6_save,
      fH=input$factor_H, fG=input$factor_G, V_init=input$V_init_val,
      Offset=input$heat_offset,
      # [新增] 标准误差列
      logK1_SE = format_se("logK1", se_values[["logK1"]]),
      H1_SE = format_se("H1", se_values[["H1"]]),
      logK2_SE = format_se("logK2", se_values[["logK2"]]),
      H2_SE = format_se("H2", se_values[["H2"]]),
      logK3_SE = format_se("logK3", se_values[["logK3"]]),
      H3_SE = format_se("H3", se_values[["H3"]]),
      logK4_SE = format_se("logK4", se_values[["logK4"]]),
      H4_SE = format_se("H4", se_values[["H4"]]),
      logK5_SE = format_se("logK5", se_values[["logK5"]]),
      H5_SE = format_se("H5", se_values[["H5"]]),
      logK6_SE = format_se("logK6", se_values[["logK6"]]),
      H6_SE = format_se("H6", se_values[["H6"]]),
      fH_SE = format_se("fH", se_values[["fH"]]),
      fG_SE = format_se("fG", se_values[["fG"]]),
      V_init_SE = format_se("V_init", se_values[["V_init"]]),
      Offset_SE = format_se("Offset", se_values[["Offset"]]),
      # [新增] 实验条件列（放在最后）
      H_cell_0=input$H_cell_0, G_syringe=input$G_syringe, V_cell=input$V_cell, 
      V_inj=input$V_inj, n_inj=input$n_inj, V_pre=input$V_pre, Temp=input$Temp,
      stringsAsFactors=FALSE
    )
    
    values$param_list <- rbind(values$param_list, new_row)
    updateTextInput(session, "snap_name", value = "") 
  })

  # --- [新增] 清除所有快照 (Modal 确认) ---
  observeEvent(input$clear_params, {
    # [修复] 防止在语言切换时触发
    if(lang_switching()) return()
    showModal(modalDialog(
      title = tr("snapshot_clear_confirm_title", lang()),
      tr("snapshot_clear_confirm_msg", lang()),
      footer = tagList(
        modalButton(tr("snapshot_clear_confirm_cancel", lang())),
        actionButton("confirm_clear", tr("snapshot_clear_confirm_ok", lang()), class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_clear, {
    values$param_list <- data.frame()
    removeModal()
    showNotification(tr("snapshot_clear_success", lang()), type = "message")
  })

  # --- [新增] 恢复默认参数 (Model 确认) ---
  observeEvent(input$reset_defaults, {
    # [修复] 防止在语言切换时触发
    if(lang_switching()) return()
    showModal(modalDialog(
      title = "确认恢复默认",
      "确定要将所有变量 恢复为默认值 吗？此操作不可撤销。建议先点击 ‘保存快照’ 存储当前参数。",
      footer = tagList(
        modalButton("取消"),
        actionButton("confirm_reset", "确认", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_reset, {
    # [修复] 使用 DEFAULT_PARAMS 常量，确保与初始值一致
    updateSliderInput(session, "logK1", value=DEFAULT_PARAMS$logK)
    updateSliderInput(session, "H1", value=DEFAULT_PARAMS$H)
    updateSliderInput(session, "logK2", value=DEFAULT_PARAMS$logK)
    updateSliderInput(session, "H2", value=DEFAULT_PARAMS$H)
    updateSliderInput(session, "logK3", value=DEFAULT_PARAMS$logK)
    updateSliderInput(session, "H3", value=DEFAULT_PARAMS$H)
    updateSliderInput(session, "logK4", value=DEFAULT_PARAMS$logK)
    updateSliderInput(session, "H4", value=DEFAULT_PARAMS$H)
    updateSliderInput(session, "logK5", value=DEFAULT_PARAMS$logK)
    updateSliderInput(session, "H5", value=DEFAULT_PARAMS$H)
    updateSliderInput(session, "logK6", value=DEFAULT_PARAMS$logK)
    updateSliderInput(session, "H6", value=DEFAULT_PARAMS$H)
    
    updateNumericInput(session, "factor_H", value=DEFAULT_PARAMS$fH)
    updateNumericInput(session, "factor_G", value=DEFAULT_PARAMS$fG)
    updateSliderInput(session, "heat_offset", value=DEFAULT_PARAMS$Offset)
    
    # V_pre 和 V_init：有实验数据时恢复为第一针体积，否则用常量默认值
    exp_df <- tryCatch(exp_data_processed(), error = function(e) NULL)
    default_first <- if (!is.null(UI_DEFAULTS$v_pre_default)) UI_DEFAULTS$v_pre_default else DEFAULT_PARAMS$V_init
    first_v <- if (!is.null(exp_df) && "V_inj_uL" %in% names(exp_df)) {
      v <- exp_df$V_inj_uL[1]
      if (!is.na(v) && is.finite(v)) v else default_first
    } else {
      default_first
    }
    values$v_pre_programmatic_update <- TRUE
    updateNumericInput(session, "V_pre", value = first_v)
    updateNumericInput(session, "V_init_val", value = first_v)
    
    removeModal()
    showNotification(tr("reset_success", lang()), type = "message")
  })

  # --- 2. 渲染表格 (修改：倒序显示，最新的在最上面) ---
  output$param_table <- renderDT({ 
    df <- values$param_list
    
    # 如果数据框为空，返回空表格
    if(is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(), 
                      options=list(dom='t'), 
                      rownames=FALSE))
    }
    
    # 确保 df 是数据框且列名有效
    # 移除任何意外的列名（比如翻译后的列名）
    # 只保留预期的列名
    expected_cols <- c("Name", "RSS", "Model", "logK1", "H1", "logK2", "H2", 
                       "logK3", "H3", "logK4", "H4", "logK5", "H5", "logK6", "H6",
                       "fH", "fG", "V_init", "Offset",
                       "logK1_SE", "H1_SE", "logK2_SE", "H2_SE",
                       "logK3_SE", "H3_SE", "logK4_SE", "H4_SE",
                       "logK5_SE", "H5_SE", "logK6_SE", "H6_SE",
                       "fH_SE", "fG_SE", 
                       "V_init_SE", "Offset_SE",
                       "H_cell_0", "G_syringe", "V_cell", "V_inj", 
                       "n_inj", "V_pre", "Temp")
    
    # 只保留存在的列（避免列名不匹配的错误）
    valid_cols <- intersect(colnames(df), expected_cols)
    if(length(valid_cols) == 0) {
      # 如果没有有效列，返回空表格
      return(datatable(data.frame(), 
                      options=list(dom='t'), 
                      rownames=FALSE))
    }
    
    # 只选择有效列
    df <- df[, valid_cols, drop = FALSE]
    
    # 如果有数据，进行倒序排列
    if(nrow(df) > 0) df <- df[nrow(df):1, ]
    
    datatable(df, 
              options=list(
                dom='tp', 
                pageLength=5, 
                scrollX=TRUE,
                scrollY = "120px",  # 固定表头，设置滚动高度（减小以让两行就能滚动）
                scrollCollapse = FALSE  # 改为 FALSE，确保即使内容少也显示滚动区域
              ), 
              selection='single',
              rownames=FALSE) 
  })
  
  # --- 3. 点击表格加载参数 (修改：适配倒序逻辑) ---
  # [辅助] 判断参数是否有效（NA、空、字符串"NA"视为无效，读取时用默认值）
  is_valid_snap_param <- function(x) {
    if (is.null(x)) return(FALSE)
    if (length(x) != 1) return(FALSE)
    if (is.na(x)) return(FALSE)
    if (is.character(x) && (nchar(trimws(as.character(x))) == 0 || trimws(as.character(x)) == "NA")) return(FALSE)
    if (is.character(x)) { n <- suppressWarnings(as.numeric(x)); if (is.na(n)) return(FALSE) }
    TRUE
  }
  snap_param_value <- function(x, default) {
    if (!is_valid_snap_param(x)) return(default)
    if (is.numeric(x)) return(x)
    suppressWarnings(as.numeric(x))
  }
  
  observeEvent(input$param_table_rows_selected, {
    # 1. 构建与显示一致的倒序数据框
    df_display <- values$param_list
    if(nrow(df_display) > 0) df_display <- df_display[nrow(df_display):1, ]
    
    # 2. 从倒序数据框中提取选中行
    target_p <- df_display[input$param_table_rows_selected, ]
    
    # 3. 批量更新 UI (兼容性处理：列不存在或为NA/空时使用默认值)
    # 基础参数 (logK1, H1)
    updateSliderInput(session, "logK1", value = snap_param_value(target_p$logK1, DEFAULT_PARAMS$logK))
    updateSliderInput(session, "H1", value = snap_param_value(target_p$H1, DEFAULT_PARAMS$H))
    
    # 可选参数 (logK2–H6)：读到 NA 则自动用默认值
    updateSliderInput(session, "logK2", value = snap_param_value(target_p$logK2, DEFAULT_PARAMS$logK))
    updateSliderInput(session, "H2", value = snap_param_value(target_p$H2, DEFAULT_PARAMS$H))
    updateSliderInput(session, "logK3", value = snap_param_value(target_p$logK3, DEFAULT_PARAMS$logK))
    updateSliderInput(session, "H3", value = snap_param_value(target_p$H3, DEFAULT_PARAMS$H))
    updateSliderInput(session, "logK4", value = snap_param_value(target_p$logK4, DEFAULT_PARAMS$logK))
    updateSliderInput(session, "H4", value = snap_param_value(target_p$H4, DEFAULT_PARAMS$H))
    updateSliderInput(session, "logK5", value = snap_param_value(target_p$logK5, DEFAULT_PARAMS$logK))
    updateSliderInput(session, "H5", value = snap_param_value(target_p$H5, DEFAULT_PARAMS$H))
    updateSliderInput(session, "logK6", value = snap_param_value(target_p$logK6, DEFAULT_PARAMS$logK))
    updateSliderInput(session, "H6", value = snap_param_value(target_p$H6, DEFAULT_PARAMS$H))
    
    updateNumericInput(session, "factor_H", value = snap_param_value(target_p$fH, DEFAULT_PARAMS$fH))
    updateNumericInput(session, "factor_G", value = snap_param_value(target_p$fG, DEFAULT_PARAMS$fG))
    updateNumericInput(session, "V_init_val", value = snap_param_value(target_p$V_init, DEFAULT_PARAMS$V_init))
    updateSliderInput(session, "heat_offset", value = snap_param_value(target_p$Offset, DEFAULT_PARAMS$Offset))
    
    # [新增] 加载实验条件（兼容性处理：如果列不存在，设为NA或默认值）
    if("H_cell_0" %in% names(target_p) && !is.na(target_p$H_cell_0)) {
      updateNumericInput(session, "H_cell_0", value=target_p$H_cell_0)
    }
    if("G_syringe" %in% names(target_p) && !is.na(target_p$G_syringe)) {
      updateNumericInput(session, "G_syringe", value=target_p$G_syringe)
    }
    if("V_cell" %in% names(target_p) && !is.na(target_p$V_cell)) {
      updateNumericInput(session, "V_cell", value=target_p$V_cell)
    }
    if("V_inj" %in% names(target_p) && !is.na(target_p$V_inj)) {
      updateNumericInput(session, "V_inj", value=target_p$V_inj)
    }
    if("n_inj" %in% names(target_p) && !is.na(target_p$n_inj)) {
      updateNumericInput(session, "n_inj", value=target_p$n_inj)
    }
    if("V_pre" %in% names(target_p) && !is.na(target_p$V_pre)) {
      values$v_pre_programmatic_update <- TRUE
      updateNumericInput(session, "V_pre", value=target_p$V_pre)
    }
    if("Temp" %in% names(target_p) && !is.na(target_p$Temp)) {
      updateNumericInput(session, "Temp", value=target_p$Temp)
    } else {
      # [修复] 如果温度不存在，使用常量定义的默认值
      updateNumericInput(session, "Temp", value=UI_DEFAULTS$temp_default)
    }
    
    # 4. [新增] 自动解析并勾选模型 (Model)
    # Model 格式示例: "rxn_M+rxn_D+rxn_T"
    if ("Model" %in% names(target_p) && !is.na(target_p$Model) && nzchar(as.character(target_p$Model))) {
      tryCatch({
        saved_model_str <- as.character(target_p$Model)
        # 分割字符串 ("+" 分隔)
        saved_paths <- strsplit(saved_model_str, "\\+")[[1]]
        
        # 定义有效路径 (必须与 checkboxGroupInput 的 values 对应)
        valid_paths <- c("rxn_D", "rxn_T", "rxn_B", "rxn_F", "rxn_U")
        
        # 筛选出有效路径 (rxn_M 是基础反应，不在 checkbox 中，忽略即可)
        paths_to_select <- intersect(saved_paths, valid_paths)
        
        # 更新 CheckboxGroupInput
        updateCheckboxGroupInput(session, "active_paths", selected = paths_to_select)
        
      }, error = function(e) {
        showNotification(paste(tr("plot_error", lang()), e$message), type = "warning")
      })
    } else {
      # 如果 Model 列不存在或为空，提示用户
      showNotification(tr("import_params_warning_no_model", lang()), type = "warning")
    }
    
    showNotification(paste(tr("import_params_success", lang()), target_p$Name), type="message")
  })
  
  # --- 4. 导入参数文件 (xlsx 格式) ---
  observeEvent(input$import_params_file, {
    req(input$import_params_file)
    tryCatch({
      # 读取 xlsx（snapshots sheet）
      imported_df <- as.data.frame(readxl::read_excel(input$import_params_file$datapath, sheet = 1))
      
      # 将文件列名（带单位后缀）还原为内部列名
      file_to_internal <- c(
        "H_cell_0_mM" = "H_cell_0", "G_syringe_mM" = "G_syringe",
        "V_cell_mL" = "V_cell", "V_inj_uL" = "V_inj", "V_pre_uL" = "V_pre",
        "Temp_K" = "Temp",
        "H1_cal_mol" = "H1", "H2_cal_mol" = "H2", "H3_cal_mol" = "H3",
        "H4_cal_mol" = "H4", "H5_cal_mol" = "H5", "H6_cal_mol" = "H6",
        "V_init_uL" = "V_init", "Offset_cal" = "Offset"
      )
      for (file_name in names(file_to_internal)) {
        if (file_name %in% colnames(imported_df)) {
          colnames(imported_df)[colnames(imported_df) == file_name] <- file_to_internal[[file_name]]
        }
      }
      
      # 关键列检查
      required_cols <- c("logK1", "H1", "fH", "fG")
      if(!all(required_cols %in% colnames(imported_df))) {
        showNotification(tr("import_params_error_format", lang()), type = "error")
        return()
      }
      
      # 统一 RSS 列格式为科学计数法字符串
      if("RSS" %in% colnames(imported_df)) {
        rss_vals <- suppressWarnings(as.numeric(imported_df$RSS))
        imported_df$RSS <- ifelse(is.na(rss_vals), "-", formatC(rss_vals, format="e", digits=3))
      }
      
      # 为缺失的可选参数列添加列（用默认值）；若列存在但值为 NA 则保留 NA，加载快照时自动用默认值
      optional_param_cols <- list(
        "logK2" = 7.0, "H2" = -6000,
        "logK3" = 7.0, "H3" = -6000,
        "logK4" = 7.0, "H4" = -6000,
        "logK5" = 7.0, "H5" = -6000,
        "logK6" = 7.0, "H6" = -6000,
        "V_init" = 0, "Offset" = 0
      )
      for(col in names(optional_param_cols)) {
        if(!col %in% colnames(imported_df)) {
          imported_df[[col]] <- optional_param_cols[[col]]
        }
        # 列存在时不再把 NA 覆盖为默认值，读取快照时由 snap_param_value 自动用默认值
      }
      
      # 实验条件列（如果不存在，设为NA）
      exp_condition_cols <- c("H_cell_0", "G_syringe", "V_cell", "V_inj", "n_inj", "V_pre", "Temp")
      for(col in exp_condition_cols) {
        if(!col %in% colnames(imported_df)) {
          imported_df[[col]] <- NA
        }
      }
      if("Temp" %in% colnames(imported_df)) {
        imported_df$Temp[is.na(imported_df$Temp)] <- 298.15
      }
      
      # 内部预期列名
      expected_cols <- c("Name", "RSS", "Model", "logK1", "H1", "logK2", "H2", 
                         "logK3", "H3", "logK4", "H4", "logK5", "H5", 
                         "logK6", "H6",
                         "fH", "fG", "V_init", "Offset",
                         "logK1_SE", "H1_SE", "logK2_SE", "H2_SE",
                         "logK3_SE", "H3_SE", "logK4_SE", "H4_SE",
                         "logK5_SE", "H5_SE", "logK6_SE", "H6_SE",
                         "fH_SE", "fG_SE", 
                         "V_init_SE", "Offset_SE",
                         "H_cell_0", "G_syringe", "V_cell", "V_inj", 
                         "n_inj", "V_pre", "Temp")
      
      valid_cols <- intersect(colnames(imported_df), expected_cols)
      if(length(valid_cols) == 0) {
        showNotification(tr("import_params_error_format", lang()), type = "error")
        return()
      }
      
      imported_df <- imported_df[, valid_cols, drop = FALSE]
      values$param_list <- bind_rows(values$param_list, imported_df)
      
      showNotification(trf("import_params_success_count", lang(), count = nrow(imported_df)), type = "message")
      
    }, error = function(e) {
      showNotification(paste(tr("import_params_error", lang()), e$message), type = "error")
    })
  })
  
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
    
    fit_params_df <- data.frame(
      parameter = c(
        "logK1", "H1_cal_mol",
        "logK2", "H2_cal_mol",
        "logK3", "H3_cal_mol",
        "logK4", "H4_cal_mol",
        "logK5", "H5_cal_mol",
        "logK6", "H6_cal_mol",
        "fH", "fG", "V_init_uL", "Offset_cal",
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
    
    list(
      sheets = sheet_list,
      integration_rev = int_export,
      meta_rev = meta_rev,
      fit_params = fit_params_df,
      simulation = sim_df
    )
  }
  
  publish_step2_plot_payload <- function(sim = NULL) {
    bundle <- build_fit_export_bundle(sim = sim)
    if (is.null(bundle)) return(invisible(FALSE))
    src <- values$imported_xlsx_filename
    if (is.null(src) || !nzchar(src)) src <- "ITCsimfit"
    bridge_set("step2_plot_payload", list(
      token = as.numeric(Sys.time()),
      source = src,
      sheets = bundle$sheets,
      integration_rev = bundle$integration_rev,
      meta_rev = bundle$meta_rev,
      fit_params = bundle$fit_params,
      simulation = bundle$simulation
    ))
    invisible(TRUE)
  }
  
  observeEvent(
    list(
      input$fit_1_step,
      input$fit_10_step,
      input$fit_full,
      input$fit_global,
      input$sim_to_exp,
      input$exp_file
    ),
    {
      sim <- tryCatch(sim_results(), error = function(e) NULL)
      publish_step2_plot_payload(sim = sim)
    },
    ignoreInit = FALSE
  )
  
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
}
