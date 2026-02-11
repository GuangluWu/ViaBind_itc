library(shiny)
library(plotly)
library(DT)
library(writexl)

# 加载模块（需在项目根目录运行，如 runApp(".") 或 setwd 到项目根目录后 runApp()）
source("R/data_parser.R")
source("R/baseline.R")
source("R/integration.R")
source("R/i18n.R")

ui <- fluidPage(
  tags$head(tags$style(HTML("
    #downloadData_processor { background-color: #b8e6b8 !important; border-color: #90c890 !important; color: #333 !important; }
    #downloadData_processor:hover { background-color: #9dd89d !important; border-color: #7ab87a !important; }
    #btn_data_to_fit { background-color: #cfe6ff !important; border-color: #9bc7f7 !important; color: #123 !important; }
    #btn_data_to_fit:hover { background-color: #b5d8ff !important; border-color: #82b8ef !important; }
    .btn-file { background-color: #add8e6 !important; border-color: #87ceeb !important; color: #333 !important; }
    .btn-file:hover { background-color: #87ceeb !important; border-color: #6bb8e0 !important; }
    .control-header {
      display: grid;
      grid-template-columns: minmax(0, 1fr) auto;
      align-items: center;
      column-gap: 8px;
      width: 100%;
      margin: 6px 0 4px 0;
      box-sizing: border-box;
    }
    .control-header h4 { margin: 0; font-size: 15px; font-weight: 700; min-width: 0; }
    .control-header .btn { margin: 0 !important; max-width: 100%; box-sizing: border-box; }
    #ui_baseline_settings { overflow: hidden; }
    #ui_baseline_settings .control-header {
      width: 100% !important;
      margin-right: 0;
    }
    .plot-panel { margin-bottom: 4px; }
    .plot-panel-integration { margin-bottom: 10px; }
    .expt-panel { margin-top: 4px; margin-bottom: 4px; padding: 8px 10px; }
    .expt-panel h4 { margin: 0 0 6px 0; font-size: 14px; }
    .expt-grid {
      display: grid;
      grid-template-columns: repeat(8, minmax(110px, 1fr));
      gap: 8px;
      align-items: end;
    }
    .param-label { font-weight: 600; margin-bottom: 2px; font-size: 14px; line-height: 1.2; }
    .param-display {
      border: 1px solid #ced4da;
      border-radius: 4px;
      padding: 6px 8px;
      background-color: #e9ecef;
      min-height: 34px;
      display: flex;
      align-items: center;
      justify-content: flex-start;
      font-size: 14px;
    }
    .expt-grid .form-group { margin-bottom: 0; }
    .expt-grid .form-control { height: 36px; font-size: 14px; padding: 6px 8px; }
    .expt-grid label { margin-bottom: 2px; font-size: 14px; font-weight: 600; line-height: 1.2; }
    .datatable-wrap { width: 100%; overflow-x: auto; }
    .datatable-wrap .dataTables_wrapper .dataTables_scrollHead { position: sticky; top: 0; z-index: 3; }
    .shiny-notification { z-index: 99999 !important; }
    .action-btn-row {
      display: grid;
      grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
      gap: 6px;
      width: 100%;
      box-sizing: border-box;
      overflow: hidden;
    }
    .action-btn-row > * { min-width: 0; box-sizing: border-box; }
    .action-btn-row .btn {
      width: 100%;
      max-width: 100%;
      display: block;
      box-sizing: border-box;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      padding-left: 8px;
      padding-right: 8px;
    }
    #action_btn_ui { overflow: hidden; }
    #action_btn_ui .action-btn-row {
      width: 100% !important;
      margin-right: 0;
    }
    @media (min-width: 992px) {
      .step1-sidebar.col-sm-3 { width: 23%; }
      .step1-main.col-sm-9 { width: 77%; }
    }
  "))),
  fluidRow(
    column(
      3,
      class = "step1-sidebar",
      div(uiOutput("label_choose_file")),
      fileInput("file1", "", accept = c(".itc", ".txt")),
      uiOutput("ui_view_controls"),
      checkboxInput("zoom_baseline", "Auto-Zoom", value = TRUE),
      hr(style = "margin: 8px 0;"),
      uiOutput("ui_baseline_settings"),
      sliderInput("offset", "Anchor Offset (s):", min = 0, max = 10, value = 5, step = 1),
      sliderInput("duration", "Anchor Width (s):", min = 5, max = 60, value = 20, step = 5),
      sliderInput("spar", "Spline Spar (0-1):", min = 0, max = 1, value = 0.1, step = 0.01),
      hr(style = "margin: 8px 0;"),
      uiOutput("ui_integration_settings"),
      numericInput("int_start_offset", "Start Offset (pts to inj)", value = 0, step = 1),
      checkboxInput("limit_integration", "Limit Integration End Point", value = TRUE),
      conditionalPanel(
        condition = "input.limit_integration == true",
        numericInput("integration_window", "End Offset (pts after inj)", min = 1, value = 15, step = 1)
      ),
      hr(style = "margin: 8px 0;"),
      uiOutput("action_btn_ui")
    ),
    column(
      9,
      class = "step1-main",
      div(class = "plot-panel", plotlyOutput("plot_raw", height = "245px")),
      div(class = "plot-panel", plotlyOutput("plot_corrected", height = "245px")),
      div(class = "plot-panel plot-panel-integration", plotlyOutput("plot_integration", height = "200px")),
      wellPanel(
        class = "expt-panel",
        uiOutput("ui_expt_params"),
        div(
          class = "expt-grid",
          div(
            div(class = "param-label", textOutput("ui_param_n_inj")),
            div(class = "param-display", textOutput("display_n_inj", inline = TRUE))
          ),
          div(
            div(class = "param-label", textOutput("ui_param_interval")),
            div(class = "param-display", textOutput("display_interval_s", inline = TRUE))
          ),
          div(
            div(class = "param-label", textOutput("ui_param_temp")),
            div(class = "param-display", textOutput("display_temp_C", inline = TRUE))
          ),
          div(
            div(class = "param-label", textOutput("ui_param_cell_vol")),
            div(class = "param-display", textOutput("display_cell_vol_mL", inline = TRUE))
          ),
          numericInput("param_syringe_mM", "Syringe (mM)", value = NA, step = 0.001, width = "100%"),
          numericInput("param_cell_mM", "Cell (mM)", value = NA, step = 0.001, width = "100%"),
          numericInput("param_V_pre_ul", "V_pre (uL)", value = NA, step = 0.1, width = "100%"),
          numericInput("param_V_inj_ul", "V_inj (uL)", value = NA, step = 0.1, width = "100%")
        )
      )
    )
  ),
  fluidRow(
    column(12, div(class = "datatable-wrap", DTOutput("results_table")))
  )
)

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

  # 语言切换
  lang <- reactiveVal("en")
  observeEvent(input$lang_btn, {
    lang(if (lang() == "en") "zh" else "en")
  })

  # 动态 UI：标题、切换按钮、区块标题
  output$ui_title <- renderUI({ h3(tr("app_title", lang())) })
  output$lang_switch_ui <- renderUI({
    btn_label <- if (lang() == "en") "\U0001F1E8\U0001F1F3 \u7b80\u4f53\u4e2d\u6587" else "\U0001F1EC\U0001F1E7 English"
    actionButton("lang_btn", btn_label,
                 class = "btn btn-default btn-sm")
  })
  baseline_defaults <- reactiveValues(duration = 20, offset = 5, spar = 0.1, ready = FALSE)

  output$label_choose_file <- renderUI({ strong("Choose .itc file") })
  output$ui_view_controls <- renderUI({ h4("View Control") })
  output$ui_baseline_settings <- renderUI({
    div(
      class = "control-header",
      style = "width:100%;box-sizing:border-box;overflow:hidden;",
      h4("Baseline Settings (Spline)"),
      actionButton("reset_baseline", "Reset", class = "btn btn-default btn-xs", width = "70px")
    )
  })
  output$ui_integration_settings <- renderUI({ h4("Integration Range") })
  output$ui_expt_params <- renderUI({ h4("Expt Params") })
  output$ui_param_n_inj <- renderText({ "N inj" })
  output$ui_param_interval <- renderText({ "Interval (s)" })
  output$ui_param_temp <- renderText({ "Temp (C)" })
  output$ui_param_cell_vol <- renderText({ "Cell vol (mL)" })
  # 仅在有可导出数据时显示导出按钮，否则显示提示
  canExport <- reactive({
    tryCatch({
      if (is.null(input$file1) || is.null(input$file1$datapath)) return(FALSE)
      rawData()
      processedData()
      TRUE
    }, error = function(e) FALSE)
  })
  output$action_btn_ui <- renderUI({
    if (!isTRUE(canExport())) return(NULL)
    div(
      class = "action-btn-row",
      style = "width:100%;box-sizing:border-box;overflow:hidden;",
      div(style = "min-width:0;", actionButton("btn_data_to_fit", "Data -> Fit", width = "100%")),
      div(
        style = "min-width:0;",
        tagAppendAttributes(
          downloadButton("downloadData_processor", "Export P. Data"),
          style = "width:100%;max-width:100%;box-sizing:border-box;"
        )
      )
    )
  })

  # 语言变化时更新所有输入控件的 label
  observeEvent(lang(), {
    updateSliderInput(session, "duration", label = "Anchor Width (s)")
    updateSliderInput(session, "offset", label = "Anchor Offset (s)")
    updateSliderInput(session, "spar", label = "Spline Spar (0-1)")
    updateNumericInput(session, "int_start_offset", label = "Start Offset (pts to inj)")
    updateCheckboxInput(session, "limit_integration", label = "Limit Integration End Point")
    tryCatch({
      updateNumericInput(session, "integration_window", label = "End Offset (pts after inj)")
    }, error = function(e) NULL)
    updateCheckboxInput(session, "zoom_baseline", label = "Auto-Zoom")
    updateNumericInput(session, "param_syringe_mM", label = "Syringe (mM)")
    updateNumericInput(session, "param_cell_mM", label = "Cell (mM)")
    updateNumericInput(session, "param_V_pre_ul", label = "V_pre (uL)")
    updateNumericInput(session, "param_V_inj_ul", label = "V_inj (uL)")
  }, ignoreInit = FALSE)

  # Y 轴 zoom 相关常量
  MIN_SPAN_RAW <- 0.1
  MIN_SPAN_CORRECTED <- 0.05
  SPAN_BUFFER <- 0.5

  # 反应式存储读取的数据
  rawData <- reactive({
    req(input$file1)
    tryCatch({
      read_itc(input$file1$datapath)
    }, error = function(e) {
      stop(safeError(e))
    })
  })

  # 文件加载后，用解析出的实验参数更新侧栏输入
  observeEvent(rawData(), {
    p <- rawData()$params
    if (!is.null(p)) {
      duration_init <- 20
      offset_init <- 5
      spar_init <- 0.1

      if (!is.na(p$syringe_conc_mM)) updateNumericInput(session, "param_syringe_mM", value = p$syringe_conc_mM)
      if (!is.na(p$cell_conc_mM))    updateNumericInput(session, "param_cell_mM", value = p$cell_conc_mM)
      if (!is.na(p$V_pre_ul))        updateNumericInput(session, "param_V_pre_ul", value = p$V_pre_ul)
      if (!is.na(p$V_inj_ul))        updateNumericInput(session, "param_V_inj_ul", value = p$V_inj_ul)

      interval_s <- p$titration_interval_s
      if (!is.na(interval_s)) {
        v <- interval_s / 5
        v <- max(5, min(100, v))
        v <- round(v / 5) * 5
        duration_init <- v
        updateSliderInput(session, "duration", value = v)

        off <- v / 10
        off <- max(0, min(50, off))
        off <- round(off)
        offset_init <- off
        updateSliderInput(session, "offset", value = off)
      }

      # 根据注射点之间的典型点数间隔，自动建议积分终点偏移（以点数计）
      injections <- rawData()$injections
      if (length(injections) >= 2) {
        intervals_pts <- diff(sort(unique(injections)))
        intervals_pts <- intervals_pts[is.finite(intervals_pts) & intervals_pts > 1]
        if (length(intervals_pts) > 0) {
          typical_pts <- stats::median(intervals_pts)
          suggested <- floor(0.9 * (typical_pts - 1))
          suggested <- max(1, suggested)
          updateNumericInput(session, "integration_window", value = suggested)
        }
      }

      baseline_defaults$duration <- duration_init
      baseline_defaults$offset <- offset_init
      baseline_defaults$spar <- spar_init
      baseline_defaults$ready <- TRUE
    }
  })

  observeEvent(input$reset_baseline, {
    if (!isTRUE(baseline_defaults$ready)) return()
    updateSliderInput(session, "duration", value = baseline_defaults$duration)
    updateSliderInput(session, "offset", value = baseline_defaults$offset)
    updateSliderInput(session, "spar", value = baseline_defaults$spar)
  }, ignoreInit = TRUE)

  # 实验参数：只读显示（N inj, Interval, Temp, Cell vol）来自文件
  output$display_n_inj <- renderText({
    req(rawData())
    n <- rawData()$params$n_injections
    if (is.na(n)) n <- length(rawData()$injections)
    as.character(n)
  })
  output$display_interval_s <- renderText({
    req(rawData())
    x <- rawData()$params$titration_interval_s
    if (is.na(x)) "" else as.character(round(x, 2))
  })
  output$display_temp_C <- renderText({
    req(rawData())
    x <- rawData()$params$temperature_C
    if (is.na(x)) "" else as.character(round(x, 2))
  })
  output$display_cell_vol_mL <- renderText({
    req(rawData())
    x <- rawData()$params$cell_volume_mL
    if (is.na(x)) "" else as.character(round(x, 4))
  })

  # 文件信息与实验参数区：状态 + 参数合在一起显示
  output$status_and_params <- renderText({
    req(rawData())
    inj <- rawData()$injections
    n_inj <- rawData()$params$n_injections
    if (is.na(n_inj)) n_inj <- length(inj)

    

    l <- lang()
    paste0(tr("loaded", l), input$file1$name,
           ";  ", tr("total_points", l), nrow(rawData()$data)
           )
  })
  
  # 反应式计算基线
  baselineData <- reactive({
    req(rawData())
    df <- rawData()$data
    injections <- rawData()$injections
    inj_times <- rawData()$injection_times
    
    base <- SegmentedBaseline(df$Time, df$Power, injections,
                              injection_times = inj_times,
                              baseline_duration = input$duration,
                              baseline_offset = input$offset,
                              spar = input$spar)
    
    return(base)
  })
  
  # 反应式计算校正后的数据和积分
  processedData <- reactive({
    req(rawData(), baselineData())
    df <- rawData()$data
    base <- baselineData()
    injections <- rawData()$injections
    
    corrected <- df$Power - base
    
    # 获取积分窗口设置
    window_val <- NULL
    if (input$limit_integration) {
      window_val <- input$integration_window
    }
    
    # 传递 int_start_offset
    int_res <- integrate_peaks(df$Time, corrected, injections, 
                               integration_window = window_val,
                               start_offset = input$int_start_offset)
    
    # 根据用户要求，过滤掉 Injection 0
    # 通常第0针是pre-equilibration或dummy injection，不参与分析
    int_res <- int_res[int_res$Injection > 0, ]
    
    # 每针体积 V_titrate_uL (µL)：与表格行一一对应，行 Injection=j 对应第 (j+1) 针的体积
    vol_ul <- rawData()$injection_volumes_ul
    if (is.null(vol_ul)) vol_ul <- rep(NA_real_, length(rawData()$injections))
    int_res$V_titrate_uL <- NA_real_
    for (r in seq_len(nrow(int_res))) {
      j <- int_res$Injection[r]
      if (j + 1L <= length(vol_ul)) int_res$V_titrate_uL[r] <- vol_ul[j + 1L]
    }
    
    # 摩尔热 (cal/mol) = heat_ucal / (V_titrate_µL * syringe_mM) * 1000
    # 量纲：ucal/(µL·mM) * 1000 = (1e-6 cal)/(1e-9 mol) = 1000 cal/mol
    syringe_mM <- input$param_syringe_mM
    if (is.na(syringe_mM)) syringe_mM <- rawData()$params$syringe_conc_mM
    denom <- int_res$V_titrate_uL * syringe_mM
    int_res$heat_cal_mol <- ifelse(!is.na(denom) & denom > 0,
      1000 * int_res$Heat_ucal / denom, NA_real_)
    
    # 表观浓度：V_cell (mL)，V_titrate_uL 转为 mL（来自文件，只读）
    V_cell <- rawData()$params$cell_volume_mL
    H_0 <- input$param_cell_mM
    if (is.na(H_0)) H_0 <- rawData()$params$cell_conc_mM
    G_0 <- input$param_syringe_mM
    if (is.na(G_0)) G_0 <- rawData()$params$syringe_conc_mM
    
    V_titrate_mL <- int_res$V_titrate_uL / 1000
    H_app <- rep(NA_real_, nrow(int_res))
    G_app <- rep(NA_real_, nrow(int_res))
    if (!is.na(V_cell) && V_cell > 0 && !is.na(H_0) && !is.na(G_0)) {
      for (r in seq_len(nrow(int_res))) {
        v <- V_titrate_mL[r]
        if (is.na(v)) next
        if (r == 1L) {
          H_app[r] <- H_0 * (V_cell - v) / V_cell
          G_app[r] <- G_0 * v / V_cell
        } else {
          H_app[r] <- H_app[r - 1L] * (V_cell - v) / V_cell
          G_app[r] <- G_app[r - 1L] * (V_cell - v) / V_cell + G_0 * v / V_cell
        }
      }
    }
    int_res$H_app_mM <- H_app
    int_res$G_app_mM <- G_app
    int_res$Ratio_App <- ifelse(!is.na(H_app) & H_app != 0, G_app / H_app, NA_real_)
    
    list(
      corrected_power = corrected,
      integration = int_res,
      integration_window = window_val # 传递给作图用
    )
  })

  step1_payload <- reactive({
    rd <- tryCatch(rawData(), error = function(e) NULL)
    pd <- tryCatch(processedData(), error = function(e) NULL)
    if (is.null(rd) || is.null(pd) || is.null(pd$integration) || nrow(pd$integration) == 0) return(NULL)

    p <- rd$params %||% list()
    temp_C <- suppressWarnings(as.numeric(p$temperature_C))
    temp_K <- if (is.finite(temp_C)) temp_C + 273.15 else NA_real_
    n_inj <- suppressWarnings(as.numeric(p$n_injections))
    if (!is.finite(n_inj)) n_inj <- nrow(pd$integration)
    meta_df <- data.frame(
      parameter = c("original_itc_file", "Temp_K", "G_syringe_mM", "H_cell_0_mM", "V_pre_uL", "V_inj_uL", "n_inj", "V_cell_mL"),
      value = c(
        if (is.null(input$file1$name) || input$file1$name == "") "" else input$file1$name,
        temp_K,
        if (!is.na(input$param_syringe_mM)) input$param_syringe_mM else p$syringe_conc_mM,
        if (!is.na(input$param_cell_mM)) input$param_cell_mM else p$cell_conc_mM,
        if (!is.na(input$param_V_pre_ul)) input$param_V_pre_ul else p$V_pre_ul,
        if (!is.na(input$param_V_inj_ul)) input$param_V_inj_ul else p$V_inj_ul,
        n_inj,
        p$cell_volume_mL
      ),
      stringsAsFactors = FALSE
    )

    list(
      token = as.numeric(Sys.time()),
      integration = pd$integration,
      meta = meta_df,
      source = if (is.null(input$file1$name) || input$file1$name == "") "ITCprocessor" else input$file1$name,
      bundle = list(
        meta = meta_df,
        power_original = data.frame(
          Time_s = rd$data$Time,
          Power_original_ucal_s = rd$data$Power
        ),
        power_corrected = data.frame(
          Time_s = rd$data$Time,
          Power_corrected_ucal_s = pd$corrected_power
        ),
        integration = pd$integration
      )
    )
  })

  last_data_to_fit_click <- reactiveVal(0L)
  observeEvent(input$file1, {
    # Re-arm Data -> Fit button gate for each newly imported file.
    last_data_to_fit_click(0L)
  }, ignoreInit = TRUE)

  observeEvent(input$btn_data_to_fit, {
    click_id <- suppressWarnings(as.integer(input$btn_data_to_fit)[1])
    if (!is.finite(click_id) || click_id <= 0L) return()
    if (click_id <= last_data_to_fit_click()) return()
    last_data_to_fit_click(click_id)
    payload <- step1_payload()
    if (is.null(payload)) return()
    payload$token <- as.numeric(Sys.time())
    bridge_set("step1_payload", payload)
    tryCatch({
      updateTabsetPanel(session, "main_tabs", selected = "Step 2 Simulation & Fitting")
    }, error = function(e) NULL)
    showNotification("Data sent to Step 2.", type = "message", duration = 2)
  }, ignoreInit = TRUE)

  # ---------------------------------------------------------
  # 回滚到稳定版逻辑：全量重绘 + 状态保持尝试
  # ---------------------------------------------------------
  
  # 1. 存储用户当前的 Zoom 状态
  user_zoom <- reactiveValues(x = NULL, y = NULL)
  user_zoom_corrected <- reactiveValues(x = NULL, y = NULL)

  # 辅助函数：解析 plotly relayout 事件为 zoom 状态更新
  parse_relayout_to_zoom <- function(ed) {
    if (is.null(ed)) return(list(reset = FALSE, x = NULL, y = NULL))
    if (!is.null(ed$`xaxis.autorange`) && ed$`xaxis.autorange` == TRUE) {
      return(list(reset = TRUE, x = NULL, y = NULL))
    }
    x <- if (!is.null(ed$`xaxis.range[0]`)) c(ed$`xaxis.range[0]`, ed$`xaxis.range[1]`) else NULL
    y <- if (!is.null(ed$`yaxis.range[0]`)) c(ed$`yaxis.range[0]`, ed$`yaxis.range[1]`) else NULL
    list(reset = FALSE, x = x, y = y)
  }

  apply_zoom_update <- function(zoom_store, parsed) {
    if (parsed$reset) {
      zoom_store$x <- NULL
      zoom_store$y <- NULL
    } else {
      if (!is.null(parsed$x)) zoom_store$x <- parsed$x
      if (!is.null(parsed$y)) zoom_store$y <- parsed$y
    }
  }

  observeEvent(event_data("plotly_relayout", source = "plot_raw"), {
    parsed <- parse_relayout_to_zoom(event_data("plotly_relayout", source = "plot_raw"))
    apply_zoom_update(user_zoom, parsed)
  })

  observeEvent(event_data("plotly_relayout", source = "plot_corrected"), {
    parsed <- parse_relayout_to_zoom(event_data("plotly_relayout", source = "plot_corrected"))
    apply_zoom_update(user_zoom_corrected, parsed)
  })

  padded_range <- function(vals, min_span, bottom_pad = 0.15, top_pad = 0.45) {
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0) return(c(-min_span, min_span))
    lo <- min(vals, na.rm = TRUE)
    hi <- max(vals, na.rm = TRUE)
    span <- hi - lo
    if (!is.finite(span) || span < min_span) span <- min_span
    c(lo - span * bottom_pad, hi + span * top_pad)
  }

  auto_zoom_range <- function(vals, min_span, bottom_frac = 0.22, top_frac = 0.12, top_abs = 0) {
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0) return(c(-min_span, min_span))
    lo <- min(vals, na.rm = TRUE)
    hi <- max(vals, na.rm = TRUE)
    span <- hi - lo
    if (!is.finite(span) || span < min_span) span <- min_span
    top_room <- max(span * top_frac, top_abs)
    bottom_room <- span * bottom_frac
    c(lo - bottom_room, hi + top_room)
  }
  
  output$plot_raw <- renderPlotly({
    req(rawData(), baselineData())
    
    df <- rawData()$data
    base <- baselineData()
    injections <- rawData()$injections
    raw_inj_times <- rawData()$injection_times
    time_min <- df$Time / 60
    x_full <- range(time_min, na.rm = TRUE)
    
    inj_times <- if (length(injections) > 0) {
      if (!is.null(raw_inj_times)) raw_inj_times else df$Time[injections]
    } else numeric(0)
    inj_times_min <- inj_times / 60
    
    inj_powers <- if (length(injections) > 0) df$Power[injections] else numeric(0)

    anchor_end_times <- inj_times - input$offset
    anchor_end_times_min <- anchor_end_times / 60
    anchor_vals <- approx(df$Time, base, xout = anchor_end_times)$y

    spline_anchors <- SegmentedBaselineAnchors(df$Time, df$Power, injections,
                                               injection_times = inj_times,
                                               baseline_duration = input$duration,
                                               baseline_offset = input$offset)
    
    p <- plot_ly(x = ~time_min, source = "plot_raw") %>%
      add_lines(y = ~df$Power, name = "Raw",
                line = list(color = 'black', width = 1)) %>%
      add_lines(y = ~base, name = "Base",
                line = list(color = 'blue', width = 2))

    if (nrow(spline_anchors) > 0) {
      p <- p %>% add_markers(x = spline_anchors$x / 60, y = spline_anchors$y,
                             name = "Spline",
                             marker = list(color = 'orange', size = 7, symbol = "x"))
    }
    
    if (length(injections) > 0) {
      p <- p %>% 
        add_markers(x = inj_times_min, y = inj_powers, name = "Inj",
                    marker = list(color = 'red', size = 10, symbol = "triangle-down")) %>%
        add_markers(x = anchor_end_times_min, y = anchor_vals, name = "Anchor",
                    marker = list(color = 'green', size = 8, symbol = "circle"))
    }
    
    layout_args <- list(
      title = list(text = "Baseline", font = list(size = 16), x = 0, xanchor = "left", y = 0.98, yanchor = "top"),
      xaxis = list(
        title = list(text = "Time (min)", standoff = 7),
        side = "top",
        showgrid = TRUE,
        ticks = "outside",
        showline = TRUE,
        mirror = FALSE,
        automargin = FALSE,
        range = x_full
      ),
      yaxis = list(
        title = "Power (ucal/s)",
        showline = TRUE,
        automargin = FALSE
      ),
      margin = list(l = 72, r = 14, t = 38, b = 8),
      legend = list(
        orientation = "h",
        x = 0.995, y = 0.995,
        xanchor = "right", yanchor = "top",
        bgcolor = "rgba(255,255,255,0.60)"
      )
    )
    
    if (input$zoom_baseline) {
      layout_args$yaxis$range <- auto_zoom_range(
        base,
        MIN_SPAN_RAW,
        bottom_frac = 0.22,
        top_frac = 0.10,
        top_abs = 0.015
      )
      if (!is.null(user_zoom$x)) layout_args$xaxis$range <- user_zoom$x
    } else {
      if (!is.null(user_zoom$x)) layout_args$xaxis$range <- user_zoom$x
      if (!is.null(user_zoom$y)) {
        layout_args$yaxis$range <- user_zoom$y
      } else {
        layout_args$yaxis$range <- padded_range(c(df$Power, base), MIN_SPAN_RAW, bottom_pad = 0.15, top_pad = 0.45)
      }
    }
    
    p <- do.call(layout, c(list(p), layout_args))
    event_register(p, 'plotly_relayout')
  })
  
  
  output$plot_corrected <- renderPlotly({
    req(processedData())
    df <- rawData()$data
    corrected <- processedData()$corrected_power
    int_res <- processedData()$integration
    time_min <- df$Time / 60
    x_full <- range(time_min, na.rm = TRUE)
    
    p <- plot_ly(source = "plot_corrected") %>%
      add_lines(x = ~time_min, y = ~corrected, 
            line = list(color = 'green', width = 1), name = "Corrected") %>%
      layout(
        title = list(text = "Corrected Baseline", font = list(size = 16), x = 0, xanchor = "left", y = 0.98, yanchor = "top"),
        xaxis = list(
          title = "",
          showticklabels = FALSE,
          ticks = "",
          showline = TRUE,
          automargin = FALSE,
          range = x_full
        ),
        yaxis = list(title = "Delta Power (ucal/s)", showline = TRUE, automargin = FALSE),
        margin = list(l = 72, r = 14, t = 28, b = 8),
        legend = list(
          orientation = "h",
          x = 0.995, y = 0.995,
          xanchor = "right", yanchor = "top",
          bgcolor = "rgba(255,255,255,0.60)"
        )
      )
             
    if (nrow(int_res) > 0) {
      start_times <- int_res$StartTime_s
      end_times <- int_res$EndTime_s
      
      if (!is.null(start_times) && !is.null(end_times)) {
         fill_y <- rep(0, length(df$Time))
         
         for (i in seq_len(nrow(int_res))) {
           t_s <- start_times[i]
           t_e <- end_times[i]
           idx_range <- which(df$Time >= t_s & df$Time <= t_e)
           if (length(idx_range) > 0) {
             fill_y[idx_range] <- corrected[idx_range]
           }
         }
         
         p <- p %>% add_lines(x = ~time_min, y = fill_y,
                              name = "Area",
                              line = list(width = 0),
                              fill = 'tozeroy',
                              fillcolor = 'rgba(255, 165, 0, 0.5)',
                              hoverinfo = "skip")
        
         start_vals <- approx(df$Time, corrected, xout = start_times)$y
         end_vals <- approx(df$Time, corrected, xout = end_times)$y
         p <- p %>% add_markers(x = start_times/60, y = start_vals, 
                                name = "Int start",
                                marker = list(color = 'blue', size = 8, symbol = "circle-open")) %>%
           add_markers(x = end_times/60, y = end_vals, 
                       name = "Int end",
                       marker = list(color = 'purple', size = 8, symbol = "x"))
      }
    }
    
    layout_args <- list()
    
    if (input$zoom_baseline) {
       injections <- rawData()$injections
       raw_inj_times <- rawData()$injection_times
       inj_times <- if (length(injections) > 0) {
          if (!is.null(raw_inj_times)) raw_inj_times else df$Time[injections]
       } else numeric(0)
       
       baseline_vals <- c()
       duration <- input$duration
       offset <- input$offset
       
       for (t_inj in inj_times) {
           t_end <- t_inj - offset
           t_start <- t_end - duration
           idx_range <- which(df$Time >= t_start & df$Time <= t_end)
           if (length(idx_range) > 0) {
               baseline_vals <- c(baseline_vals, corrected[idx_range])
           }
       }
       
       t_last <- tail(df$Time, 1)
       if (length(t_last) > 0) {
           t_start_last <- t_last - duration
           idx_last <- which(df$Time >= t_start_last & df$Time <= t_last)
           if (length(idx_last) > 0) {
               baseline_vals <- c(baseline_vals, corrected[idx_last])
           }
       }
       
       baseline_vals <- baseline_vals[!is.na(baseline_vals)]
       
       if (length(baseline_vals) > 0) {
           layout_args$yaxis <- list(
             title = "Delta Power (ucal/s)",
             showline = TRUE,
             automargin = FALSE,
             range = auto_zoom_range(
               baseline_vals,
               MIN_SPAN_CORRECTED,
               bottom_frac = 0.22,
               top_frac = 0.12,
               top_abs = 0.002
             )
           )
       }
       
       if (!is.null(user_zoom_corrected$x)) {
         layout_args$xaxis <- list(
           title = "", showticklabels = FALSE, ticks = "",
           showline = TRUE, automargin = FALSE,
           range = user_zoom_corrected$x
         )
       }
       
    } else {
       if (!is.null(user_zoom_corrected$x)) {
         layout_args$xaxis <- list(
           title = "", showticklabels = FALSE, ticks = "",
           showline = TRUE, automargin = FALSE,
           range = user_zoom_corrected$x
         )
       }
       if (!is.null(user_zoom_corrected$y)) {
         layout_args$yaxis <- list(
           title = "Delta Power (ucal/s)",
           showline = TRUE,
           automargin = FALSE,
           range = user_zoom_corrected$y
         )
       } else {
         layout_args$yaxis <- list(
           title = "Delta Power (ucal/s)",
           showline = TRUE,
           automargin = FALSE,
           range = padded_range(corrected, MIN_SPAN_CORRECTED, bottom_pad = 0.15, top_pad = 0.45)
         )
       }
    }
    
    if (length(layout_args) > 0) {
      p <- do.call(layout, c(list(p), layout_args))
    }
    
    event_register(p, 'plotly_relayout')
  })
  
  output$results_table <- renderDT({
    req(processedData())
    opts <- list(
      pageLength = 30,
      scrollX = TRUE,
      scrollY = "260px",
      scrollCollapse = TRUE,
      fixedHeader = TRUE
    )
    if (lang() == "zh") opts$language <- list(url = "https://cdn.datatables.net/plug-ins/1.13.7/i18n/zh-HANS.json")
    datatable(
              processedData()$integration,
              rownames = FALSE,
              extensions = "FixedHeader",
              options = opts)
  })
  
  output$plot_integration <- renderPlotly({
    req(processedData())
    int_res <- processedData()$integration
    
    if (nrow(int_res) == 0) {
      return(plot_ly() %>% layout(
        title = list(text = "Integration", font = list(size = 16), x = 0, xanchor = "left"),
        xaxis = list(showticklabels = FALSE, title = ""),
        yaxis = list(title = "Heat (ucal)")
      ))
    }
    
    plot_ly(data = int_res, x = ~Injection, y = ~Heat_ucal, type = 'scatter', mode = 'markers',
            marker = list(size = 10, color = 'black', symbol = 'circle')) %>%
      layout(
        title = list(text = "Integration", font = list(size = 16), x = 0, xanchor = "left", y = 0.98, yanchor = "top"),
        xaxis = list(showticklabels = FALSE, ticks = "", title = "", showline = TRUE, automargin = FALSE),
        yaxis = list(title = "Heat (ucal)", showline = TRUE, automargin = FALSE),
        margin = list(l = 72, r = 14, t = 28, b = 8),
        showlegend = FALSE
      )
  })
  
  output$downloadData_processor <- downloadHandler(
    filename = function() {
      base_name <- if (is.null(input$file1$name) || input$file1$name == "") "data" else tools::file_path_sans_ext(input$file1$name)
      paste0(base_name, "_processed_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx")
    },
    content = function(file) {
      rd <- tryCatch(rawData(), error = function(e) NULL)
      pd <- tryCatch(processedData(), error = function(e) NULL)
      if (is.null(rd) || is.null(pd)) {
        showNotification(tr("export_no_data", lang()), type = "warning", duration = 5, session = session)
        writexl::write_xlsx(
          list(Note = data.frame(message = tr("export_no_data", lang()))),
          path = file
        )
        return()
      }
      p <- rd$params

      # meta: 优先用 input，否则用 params
      temp_C <- p$temperature_C
      temp_K <- if (!is.na(temp_C)) temp_C + 273.15 else NA_real_
      syringe_mM <- if (!is.na(input$param_syringe_mM)) input$param_syringe_mM else p$syringe_conc_mM
      cell_mM <- if (!is.na(input$param_cell_mM)) input$param_cell_mM else p$cell_conc_mM
      V_pre <- if (!is.na(input$param_V_pre_ul)) input$param_V_pre_ul else p$V_pre_ul
      V_inj <- if (!is.na(input$param_V_inj_ul)) input$param_V_inj_ul else p$V_inj_ul
      n_inj <- p$n_injections
      if (is.na(n_inj)) n_inj <- length(rd$injections)
      cell_vol <- p$cell_volume_mL

      original_itc_name <- if (is.null(input$file1$name) || input$file1$name == "") "" else input$file1$name
      meta_df <- data.frame(
        parameter = c("original_itc_file", "Temp_K", "G_syringe_mM", "H_cell_0_mM", "V_pre_uL", "V_inj_uL", "n_inj", "V_cell_mL"),
        value = c(original_itc_name, temp_K, syringe_mM, cell_mM, V_pre, V_inj, n_inj, cell_vol),
        unit = c("", "K", "mM", "mM", "\u00B5L", "\u00B5L", "", "mL")
      )

      power_df <- data.frame(
        Time_s = rd$data$Time,
        Power_corrected_ucal_s = pd$corrected_power
      )
      power_original_df <- data.frame(
        Time_s = rd$data$Time,
        Power_original_ucal_s = rd$data$Power
      )

      writexl::write_xlsx(
        list(
          power_original = power_original_df,
          meta = meta_df,
          power_corrected = power_df,
          integration = pd$integration
        ),
        path = file
      )
    }
  )

}

shinyApp(ui = ui, server = server)
