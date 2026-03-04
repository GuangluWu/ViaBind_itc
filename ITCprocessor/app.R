library(shiny)
library(plotly)
library(DT)
library(writexl)

# [COMMENT_STD][MODULE_HEADER]
# 模块职责：Step1 应用入口，负责原始 ITC 数据导入、基线处理、积分与向 Step2 桥接导出。
# 依赖：R/data_parser.R、R/baseline.R、R/integration.R、R/i18n.R、R/guide_annotations.R。
# 对外接口：ui、server、shinyApp(ui, server)。
# 副作用：读文件、写导出 xlsx、通过 session bridge 推送 payload。
# 变更历史：2026-02-12 - 增加 Phase 4 注释规范与 guide annotations 预埋加载。

# 加载模块（需在项目根目录运行，如 runApp(".") 或 setwd 到项目根目录后 runApp()）
source("R/data_parser.R")
source("R/baseline.R")
source("R/integration.R")
source("R/i18n.R")
source("R/guide_annotations.R")

ITCPROCESSOR_APP_DIR <- tryCatch(
  normalizePath(getwd(), winslash = "/", mustWork = TRUE),
  error = function(e) normalizePath(getwd(), winslash = "/", mustWork = FALSE)
)

if (!exists("load_guide_annotations", mode = "function")) {
  stop("Failed to load guide annotation module from R/guide_annotations.R", call. = FALSE)
}

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
    .control-header h4 { margin: 0; min-width: 0; }
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
    .step1-col1-scroll {
      height: calc(var(--itcsuite-vh, 1vh) * 100 - var(--itcsuite-host-chrome, 140px) + 5px);
      max-height: calc(var(--itcsuite-vh, 1vh) * 100 - var(--itcsuite-host-chrome, 140px) + 5px);
      overflow-y: auto;
      overflow-x: hidden;
      min-height: 0;
      padding-right: 5px;
      overscroll-behavior: contain;
    }
    @media (min-width: 992px) {
      .step1-sidebar.col-sm-3 { width: 23%; }
      .step1-main.col-sm-9 { width: 77%; }
      .step1-sidebar {
        position: sticky;
        top: 0;
        align-self: flex-start;
      }
    }
    @media (max-width: 768px) {
      .step1-col1-scroll {
        max-height: none;
        overflow: visible;
        padding-right: 0;
      }
    }
  "))),
  fluidRow(
    column(
      3,
      class = "step1-sidebar",
      div(
        class = "step1-col1-scroll",
        div(uiOutput("label_choose_file")),
        uiOutput("step1_import_input"),
        uiOutput("step1_import_summary_ui"),
        checkboxInput("zoom_baseline", tr("zoom_baseline"), value = TRUE),
        hr(style = "margin: 8px 0;"),
        uiOutput("ui_baseline_settings"),
        sliderInput("offset", tr("anchor_offset"), min = 0, max = 10, value = 5, step = 1),
        sliderInput("duration", tr("anchor_width"), min = 5, max = 60, value = 20, step = 5),
        sliderInput("spar", tr("spline_spar"), min = 0, max = 1, value = 0.1, step = 0.01),
        uiOutput("anchor_bounds_editor"),
        hr(style = "margin: 8px 0;"),
        uiOutput("ui_integration_settings"),
        numericInput("int_start_offset", tr("start_offset"), value = -3, step = 1),
        checkboxInput("limit_integration", tr("limit_integration"), value = TRUE),
        conditionalPanel(
          condition = "input.limit_integration == true",
          numericInput("integration_window", tr("end_offset"), min = 1, value = 15, step = 1)
        ),
        div(
          class = "action-btn-row",
          style = "width:100%;box-sizing:border-box;overflow:hidden;",
          div(style = "min-width:0;", actionButton("btn_data_to_fit", tr("data_to_fit"), width = "100%")),
          div(style = "min-width:0;", uiOutput("download_btn_ui"))
        ),
        hr(style = "margin: 10px 0;"),
        uiOutput("ui_baseline_integration_notes")
      )
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
          numericInput("param_syringe_mM", tr("syringe_mM"), value = NA, step = 0.001, width = "100%"),
          numericInput("param_cell_mM", tr("cell_mM"), value = NA, step = 0.001, width = "100%"),
          numericInput("param_V_pre_ul", tr("v_pre_ul"), value = NA, step = 0.1, width = "100%"),
          numericInput("param_V_inj_ul", tr("v_inj_ul"), value = NA, step = 0.1, width = "100%")
        )
      )
    )
  ),
  fluidRow(
    column(12, div(class = "datatable-wrap", DTOutput("results_table")))
  )
)

server <- function(input, output, session) {
  # [COMMENT_STD][ERROR_SEMANTICS]
  # 错误码/类别：输入文件解析/桥接导出失败属于运行时错误。
  # 触发条件：文件读取失败、必需字段缺失、bridge channel 不可用。
  # 用户可见性：通过通知与控制台错误可见；关键初始化错误直接 stop。
  # 日志级别：运行时 warning/error；初始化阶段 error。
  # 恢复动作：保持当前 UI 状态并允许重新导入或重试导出。
  `%||%` <- function(x, y) if (is.null(x)) y else x
  resolve_viabind_version <- function(default_version = "x.x.x") {
    default_chr <- as.character(default_version %||% "x.x.x")[1]
    default_chr <- trimws(default_chr)
    if (!nzchar(default_chr)) default_chr <- "x.x.x"

    from_env <- trimws(as.character(Sys.getenv("ITCSUITE_APP_VERSION", unset = ""))[1])
    if (nzchar(from_env)) return(from_env)

    candidates <- unique(c(
      file.path(getwd(), "desktop", "package.json"),
      file.path(getwd(), "..", "desktop", "package.json")
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
  build_viabind_signature <- function(module_name, version = NULL) {
    module_chr <- as.character(module_name %||% "")[1]
    module_chr <- trimws(module_chr)
    if (!nzchar(module_chr)) module_chr <- "UnknownModule"
    version_chr <- as.character(version %||% "")[1]
    version_chr <- trimws(version_chr)
    if (!nzchar(version_chr)) version_chr <- resolve_viabind_version()
    paste0("ViaBind v", version_chr, ": ", module_chr)
  }

  session_bridge <- tryCatch({
    b <- session$userData$itcsuite_bridge
    if (is.null(b) || !is.list(b)) NULL else b
  }, error = function(e) NULL)

  resolve_bridge_channel <- function(channel) {
    ch <- if (!is.null(session_bridge)) session_bridge[[channel]] else NULL
    if (is.function(ch)) return(ch)
    NULL
  }

  bridge_set <- function(channel, payload) {
    ch <- resolve_bridge_channel(channel)
    if (is.function(ch)) ch(payload)
    invisible(NULL)
  }

  session_home <- tryCatch({
    h <- session$userData$itcsuite_home
    if (is.null(h) || !is.list(h)) NULL else h
  }, error = function(e) NULL)

  home_add_recent <- function(record, payload = NULL) {
    fn <- if (!is.null(session_home)) session_home$add_recent_import else NULL
    if (is.function(fn)) fn(record, payload)
    invisible(NULL)
  }

  home_add_recent_export <- function(record, payload = NULL) {
    fn <- if (!is.null(session_home)) session_home$add_recent_export else NULL
    if (is.function(fn)) fn(record, payload)
    invisible(NULL)
  }

  home_register_restore <- function(step, handler) {
    fn <- if (!is.null(session_home)) session_home$register_restore_handler else NULL
    if (is.function(fn)) {
      return(invisible(isTRUE(fn(step, handler))))
    }
    invisible(FALSE)
  }

  session_sleep_restore <- tryCatch({
    s <- session$userData$itcsuite_sleep_restore
    if (is.null(s) || !is.list(s)) NULL else s
  }, error = function(e) NULL)

  sleep_restore_register <- function(step, collect_fn, apply_fn) {
    fn <- if (!is.null(session_sleep_restore)) session_sleep_restore$register_handler else NULL
    if (!is.function(fn)) return(invisible(FALSE))
    invisible(isTRUE(fn(step = step, collect_fn = collect_fn, apply_fn = apply_fn)))
  }

  session_desktop <- tryCatch({
    d <- session$userData$itcsuite_desktop
    if (is.null(d) || !is.list(d)) NULL else d
  }, error = function(e) NULL)

  session_telemetry <- tryCatch({
    t <- session$userData$itcsuite_telemetry
    if (is.null(t) || !is.list(t)) NULL else t
  }, error = function(e) NULL)

  telemetry_lang_safe <- function() {
    tryCatch({
      val <- lang()
      if (identical(val, "zh")) "zh" else "en"
    }, error = function(e) "en")
  }

  telemetry_log <- function(event, level = "INFO", payload = list(), err = NULL, op_id = NULL) {
    fn <- if (!is.null(session_telemetry)) session_telemetry$log_event else NULL
    if (!is.function(fn)) return(invisible(NULL))
    tryCatch(
      fn(
        event = event,
        level = level,
        module = "step1",
        payload = payload,
        err = err,
        op_id = op_id,
        lang = telemetry_lang_safe()
      ),
      error = function(e) invisible(NULL)
    )
  }

  telemetry_start <- function(event, payload = list()) {
    fn <- if (!is.null(session_telemetry)) session_telemetry$start_op else NULL
    if (!is.function(fn)) return("")
    tryCatch(
      fn(event = event, module = "step1", payload = payload, lang = telemetry_lang_safe()),
      error = function(e) ""
    )
  }

  telemetry_finish <- function(op_id, outcome = "ok", payload = list(), err = NULL, level = NULL) {
    fn <- if (!is.null(session_telemetry)) session_telemetry$finish_op else NULL
    if (!is.function(fn)) return(invisible(NULL))
    tryCatch(
      fn(
        op_id = op_id,
        outcome = outcome,
        payload = payload,
        err = err,
        level = level,
        lang = telemetry_lang_safe()
      ),
      error = function(e) invisible(NULL)
    )
    if (!identical(outcome, "ok")) {
      telemetry_log(
        event = "error.runtime",
        level = "ERROR",
        payload = list(op_id = op_id, outcome = outcome, context = "step1"),
        err = err
      )
    }
  }

  desktop_open_file_enabled <- function() {
    fn <- if (!is.null(session_desktop)) session_desktop$enabled else NULL
    if (!is.function(fn)) return(FALSE)
    isTRUE(fn())
  }

  bridge_last_token <- reactiveVal(as.numeric(Sys.time()))
  next_bridge_token <- function() {
    now_token <- as.numeric(Sys.time())
    last_token <- suppressWarnings(as.numeric(bridge_last_token())[1])
    if (is.finite(last_token) && is.finite(now_token) && now_token <= last_token) {
      now_token <- last_token + 1e-6
    }
    bridge_last_token(now_token)
    now_token
  }

  session_i18n <- tryCatch({
    x <- session$userData$itcsuite_i18n
    if (is.null(x) || !is.list(x)) NULL else x
  }, error = function(e) NULL)

  normalize_lang <- function(x) {
    lang_chr <- tolower(as.character(x %||% "en")[1])
    if (identical(lang_chr, "zh")) "zh" else "en"
  }

  local_lang <- reactiveVal("en")
  local_lang_token <- reactiveVal(0)
  restored_step1_path <- reactiveVal(NULL)
  restored_step1_name <- reactiveVal(NULL)
  step1_sleep_restore_pending_params <- reactiveVal(NULL)
  step1_active_source_path <- reactiveVal("")
  step1_active_source_kind <- reactiveVal("none")
  step1_import_started_at <- reactiveVal(NA_real_)
  step1_import_source <- reactiveVal("none")
  home_restore_inflight <- reactiveVal(FALSE)
  step1_last_export_name <- reactiveVal(NULL)
  set_local_lang <- function(value) {
    normalized <- normalize_lang(value)
    if (!identical(local_lang(), normalized)) {
      local_lang(normalized)
      local_lang_token(local_lang_token() + 1)
    }
    invisible(normalized)
  }

  set_lang <- function(value) {
    if (!is.null(session_i18n) && is.function(session_i18n$set_lang)) {
      session_i18n$set_lang(normalize_lang(value))
      return(invisible(NULL))
    }
    set_local_lang(value)
  }

  lang <- reactive({
    if (!is.null(session_i18n) && is.function(session_i18n$lang_token)) {
      session_i18n$lang_token()
    } else {
      local_lang_token()
    }
    if (!is.null(session_i18n) && is.function(session_i18n$get_lang)) {
      return(normalize_lang(session_i18n$get_lang()))
    }
    normalize_lang(local_lang())
  })

  observeEvent(input$lang_btn, {
    set_lang(if (lang() == "en") "zh" else "en")
  }, ignoreInit = TRUE)

  output$ui_title <- renderUI({ h3(tr("app_title", lang())) })
  output$lang_switch_ui <- renderUI(NULL)
  baseline_defaults <- reactiveValues(duration = 20, offset = 5, spar = 0.1, ready = FALSE)
  safe_step1_input_get <- function(id) {
    tryCatch(shiny::isolate(input[[id]]), error = function(e) NULL)
  }

  ANCHOR_BOUND_PARAM_IDS <- c("offset", "duration")
  ANCHOR_BOUND_STEPS <- c(offset = 1, duration = 5)

  get_default_anchor_bound <- function(param_name) {
    if (identical(param_name, "offset")) return(c(lower = 0, upper = 10))
    if (identical(param_name, "duration")) return(c(lower = 5, upper = 60))
    c(lower = 0, upper = 10)
  }

  build_default_anchor_param_bounds <- function() {
    out <- vector("list", length(ANCHOR_BOUND_PARAM_IDS))
    names(out) <- ANCHOR_BOUND_PARAM_IDS
    for (nm in ANCHOR_BOUND_PARAM_IDS) {
      out[[nm]] <- get_default_anchor_bound(nm)
    }
    out
  }

  anchor_param_bounds <- reactiveVal(build_default_anchor_param_bounds())
  anchor_bounds_editor_open <- reactiveVal(FALSE)

  get_anchor_bound_for_ui <- function(param_name) {
    bounds_now <- anchor_param_bounds()
    b <- bounds_now[[param_name]]
    if (is.null(b)) b <- get_default_anchor_bound(param_name)
    c(lower = as.numeric(b["lower"]), upper = as.numeric(b["upper"]))
  }

  sanitize_anchor_bound_pair <- function(lower_in, upper_in, old_lower, old_upper, step = 1) {
    lower_num <- suppressWarnings(as.numeric(lower_in)[1])
    upper_num <- suppressWarnings(as.numeric(upper_in)[1])
    if (!is.finite(lower_num)) lower_num <- old_lower
    if (!is.finite(upper_num)) upper_num <- old_upper

    if (!is.finite(lower_num)) lower_num <- 0
    if (!is.finite(upper_num)) upper_num <- lower_num + step

    lower_num <- max(0, lower_num)
    upper_num <- max(0, upper_num)

    if (lower_num > upper_num) {
      tmp <- lower_num
      lower_num <- upper_num
      upper_num <- tmp
    }

    min_span <- suppressWarnings(as.numeric(step)[1])
    if (!is.finite(min_span) || min_span <= 0) min_span <- 1
    if (abs(upper_num - lower_num) < .Machine$double.eps^0.5) {
      upper_num <- lower_num + min_span
    }

    c(lower = lower_num, upper = upper_num)
  }

  clamp_to_anchor_bound <- function(value, bound, default) {
    lower_num <- suppressWarnings(as.numeric(bound["lower"])[1])
    upper_num <- suppressWarnings(as.numeric(bound["upper"])[1])
    if (!is.finite(lower_num)) lower_num <- 0
    lower_num <- max(0, lower_num)
    if (!is.finite(upper_num) || upper_num < lower_num) upper_num <- lower_num + 1

    val_num <- suppressWarnings(as.numeric(value)[1])
    if (!is.finite(val_num)) val_num <- suppressWarnings(as.numeric(default)[1])
    if (!is.finite(val_num)) val_num <- lower_num
    min(max(val_num, lower_num), upper_num)
  }

  get_default_anchor_value <- function(param_name) {
    if (identical(param_name, "duration")) return(20)
    if (identical(param_name, "offset")) return(5)
    0
  }

  apply_anchor_bound_to_slider <- function(param_name, bound) {
    slider_id <- param_name
    current_val <- safe_step1_input_get(slider_id)
    default_val <- get_default_anchor_value(param_name)
    clamped_val <- clamp_to_anchor_bound(current_val, bound, default = default_val)
    min_num <- suppressWarnings(as.numeric(bound["lower"])[1])
    max_num <- suppressWarnings(as.numeric(bound["upper"])[1])
    if (!is.finite(min_num)) min_num <- 0
    if (!is.finite(max_num) || max_num < min_num) max_num <- min_num + 1
    updateSliderInput(
      session,
      slider_id,
      min = min_num,
      max = max_num,
      value = clamped_val
    )
    invisible(TRUE)
  }

  apply_all_anchor_bounds_to_sliders <- function() {
    bounds_now <- anchor_param_bounds()
    for (nm in ANCHOR_BOUND_PARAM_IDS) {
      b <- bounds_now[[nm]]
      if (is.null(b)) next
      apply_anchor_bound_to_slider(nm, b)
    }
    invisible(TRUE)
  }

  update_anchor_param_bound <- function(param_name, lower_in, upper_in, update_ui_inputs = TRUE) {
    current_bounds <- anchor_param_bounds()
    old_bound <- current_bounds[[param_name]]
    if (is.null(old_bound)) old_bound <- get_default_anchor_bound(param_name)

    step_val <- ANCHOR_BOUND_STEPS[[param_name]]
    if (!is.finite(step_val) || step_val <= 0) step_val <- 1

    sanitized <- sanitize_anchor_bound_pair(
      lower_in = lower_in,
      upper_in = upper_in,
      old_lower = as.numeric(old_bound["lower"]),
      old_upper = as.numeric(old_bound["upper"]),
      step = step_val
    )
    current_bounds[[param_name]] <- sanitized
    anchor_param_bounds(current_bounds)

    if (isTRUE(update_ui_inputs)) {
      min_id <- paste0("anchor_bound_", param_name, "_min")
      max_id <- paste0("anchor_bound_", param_name, "_max")
      cur_min <- suppressWarnings(as.numeric(safe_step1_input_get(min_id))[1])
      cur_max <- suppressWarnings(as.numeric(safe_step1_input_get(max_id))[1])
      lower_num <- suppressWarnings(as.numeric(sanitized["lower"])[1])
      upper_num <- suppressWarnings(as.numeric(sanitized["upper"])[1])
      if (!is.finite(cur_min) || !isTRUE(all.equal(cur_min, lower_num, tolerance = 1e-10))) {
        updateNumericInput(session, min_id, value = lower_num)
      }
      if (!is.finite(cur_max) || !isTRUE(all.equal(cur_max, upper_num, tolerance = 1e-10))) {
        updateNumericInput(session, max_id, value = upper_num)
      }
    }

    apply_anchor_bound_to_slider(param_name, sanitized)
    invisible(TRUE)
  }

  output$label_choose_file <- renderUI({ strong(tr("choose_file", lang())) })
  output$step1_import_input <- renderUI({
    if (isTRUE(desktop_open_file_enabled())) {
      return(
        actionButton(
          "step1_desktop_pick_file",
          "Choose File (Desktop)...",
          class = "btn btn-info btn-sm btn-block"
        )
      )
    }
    fileInput(
      "file1",
      "",
      accept = c(".itc", ".txt", ".nitc", ".csc", ".xml"),
      buttonLabel = "Browse...",
      placeholder = "No file selected"
    )
  })
  output$ui_baseline_settings <- renderUI({
    div(
      class = "control-header",
      style = "width:100%;box-sizing:border-box;overflow:hidden;",
      h4(tr("baseline_settings", lang())),
      actionButton("reset_baseline", tr("reset", lang()), class = "btn btn-default btn-xs", width = "70px")
    )
  })
  output$anchor_bounds_editor <- renderUI({
    header_row <- div(
      style = "display:flex; gap:8px; align-items:center; margin: 6px 0 4px 0; font-size:12px; font-weight:bold;",
      div(style = "flex: 0 0 72px;", tr("anchor_bounds_param_col", lang())),
      div(style = "flex: 1 1 120px;", tr("anchor_bounds_min_col", lang())),
      div(style = "flex: 1 1 120px;", tr("anchor_bounds_max_col", lang()))
    )

    rows <- lapply(ANCHOR_BOUND_PARAM_IDS, function(param_name) {
      bound <- get_anchor_bound_for_ui(param_name)
      min_id <- paste0("anchor_bound_", param_name, "_min")
      max_id <- paste0("anchor_bound_", param_name, "_max")
      step_val <- suppressWarnings(as.numeric(ANCHOR_BOUND_STEPS[[param_name]])[1])
      if (!is.finite(step_val) || step_val <= 0) step_val <- 1

      min_val <- isolate({
        v <- suppressWarnings(as.numeric(input[[min_id]])[1])
        if (is.finite(v)) v else as.numeric(bound["lower"])
      })
      max_val <- isolate({
        v <- suppressWarnings(as.numeric(input[[max_id]])[1])
        if (is.finite(v)) v else as.numeric(bound["upper"])
      })

      div(
        style = "display:flex; gap:8px; align-items:center; margin-bottom: 4px;",
        div(style = "flex: 0 0 72px; font-family: monospace; font-size: 12px;", param_name),
        div(style = "flex: 1 1 120px;", numericInput(min_id, NULL, value = min_val, step = step_val, width = "100%")),
        div(style = "flex: 1 1 120px;", numericInput(max_id, NULL, value = max_val, step = step_val, width = "100%"))
      )
    })

    details_tag <- tags$details(
      id = "anchor_bounds_details",
      ontoggle = "Shiny.setInputValue('anchor_bounds_editor_open', this.open, {priority: 'event'})",
      style = "margin: 6px 0 8px 0;",
      tags$summary(
        style = "cursor: pointer; color: #2980b9; font-size: 12px;",
        tr("anchor_bounds_title", lang())
      ),
      div(
        style = "margin-top: 6px; padding: 8px; border: 1px solid #e3e3e3; border-radius: 4px; background:#fafafa;",
        div(style = "font-size: 12px; color: #666; margin-bottom: 4px;", tr("anchor_bounds_hint", lang())),
        header_row,
        rows,
        div(
          style = "margin-top: 6px;",
          actionButton("reset_anchor_bounds", tr("anchor_bounds_reset_btn", lang()), class = "btn-default btn-xs")
        )
      )
    )
    if (isTRUE(anchor_bounds_editor_open())) {
      details_tag <- tagAppendAttributes(details_tag, open = "open")
    }
    details_tag
  })
  output$ui_integration_settings <- renderUI({ h4(tr("integration_settings", lang())) })
  output$ui_baseline_integration_notes <- renderUI({
    note_keys <- c(
      "param_notes_spline",
      "param_notes_assumption",
      "param_notes_window",
      "param_notes_auto",
      "param_notes_check",
      "param_notes_integration",
      "param_notes_start",
      "param_notes_end"
    )

    build_note_item <- function(index, note_text, is_last = FALSE) {
      note_text <- as.character(note_text %||% "")
      note_pattern <- "^\\s*([^:：]+[:：])\\s*(.*)$"
      margin_style <- if (is_last) "margin: 0;" else "margin: 0 0 6px 0;"

      if (grepl(note_pattern, note_text, perl = TRUE)) {
        lead <- sub(note_pattern, "\\1", note_text, perl = TRUE)
        body <- sub(note_pattern, "\\2", note_text, perl = TRUE)
        return(tags$p(
          tags$strong(paste0(index, ". ", lead, " ")),
          body,
          style = margin_style
        ))
      }

      tags$p(
        tags$strong(paste0(index, ". ")),
        note_text,
        style = margin_style
      )
    }

    note_items <- lapply(seq_along(note_keys), function(i) {
      build_note_item(
        index = i,
        note_text = tr(note_keys[[i]], lang()),
        is_last = identical(i, length(note_keys))
      )
    })

    tags$details(
      style = "margin: 0 0 8px 0;",
      tags$summary(
        style = "cursor: pointer; color: #2980b9; font-size: 0.95em;",
        tr("param_notes_title", lang())
      ),
      div(
        style = "margin-top: 8px; padding: 8px; background-color: #f0f8ff; border-left: 3px solid #2980b9; font-size: 0.9em; line-height: 1.6;",
        tagList(note_items)
      )
    )
  })
  output$ui_expt_params <- renderUI({ h4(tr("expt_params", lang())) })
  output$ui_param_n_inj <- renderText({ tr("n_inj", lang()) })
  output$ui_param_interval <- renderText({ tr("interval_s", lang()) })
  output$ui_param_temp <- renderText({ tr("temp_c", lang()) })
  output$ui_param_cell_vol <- renderText({ tr("cell_vol", lang()) })

  current_itc_name <- reactive({
    restored_name <- as.character(restored_step1_name() %||% "")[1]
    if (nzchar(trimws(restored_name))) {
      return(trimws(restored_name))
    }
    if (!is.null(input$file1$name) && nzchar(trimws(input$file1$name))) return(trimws(input$file1$name))
    ""
  })
  output$step1_import_summary_ui <- renderUI({
    file_name <- current_itc_name()
    if (!nzchar(file_name)) {
      return(
        div(
          style = "color:#888;font-size:0.85em;margin-top:4px;",
          em(tr("no_file_selected", lang()))
        )
      )
    }
    div(
      style = "margin-top:4px;font-size:0.9em;",
      tags$b(paste0(tr("file_label", lang()), ": ")),
      file_name
    )
  })

  normalize_step1_path <- function(path) {
    p <- as.character(path %||% "")[1]
    p <- trimws(p)
    if (!nzchar(p)) return("")
    tryCatch(normalizePath(p, winslash = "/", mustWork = FALSE), error = function(e) p)
  }

  resolve_step1_recent_source_path <- function() {
    active <- normalize_step1_path(step1_active_source_path())
    if (nzchar(active)) return(active)
    normalize_step1_path(tryCatch(input$file1$datapath, error = function(e) ""))
  }

  resolve_ta_source_ext <- function(filepath, display_name = NULL) {
    st <- detect_step1_source_type(filepath, display_name = display_name)
    switch(
      st,
      ta_nitc = ".nitc",
      ta_csc = ".csc",
      ta_xml = ".xml",
      ""
    )
  }

  notify_ta_parse_hint <- function(filepath, display_name = NULL) {
    ext <- resolve_ta_source_ext(filepath, display_name = display_name)
    if (!nzchar(ext)) return(invisible(FALSE))
    showNotification(
      sprintf(tr("ta_parse_hint", lang()), ext),
      type = "message",
      duration = 6
    )
    invisible(TRUE)
  }

  resolve_baseline_injection_series <- function(rd) {
    if (is.null(rd) || !is.list(rd)) {
      return(list(injections = numeric(0), injection_times = numeric(0)))
    }
    df <- rd$data %||% data.frame(Time = numeric(0))
    injections <- as.integer(rd$injections %||% numeric(0))
    injection_times <- rd$injection_times

    # TA path always inserts a pseudo injection at t=0 only to keep integration indexing.
    # Some .itc files also contain @0 pseudo injection (len(injections)=n_inj+1).
    # Baseline anchor search should always start from the first real injection.
    is_ta <- is.list(rd$source) && identical(as.character(rd$source$type %||% "")[1], "ta_xlsx")
    n_inj_param <- suppressWarnings(as.integer((rd$params %||% list())$n_injections)[1])
    has_extra_zero <- is.finite(n_inj_param) && length(injections) == (n_inj_param + 1L)
    has_pseudo_zero <- length(injections) >= 2L && injections[1L] == 1L && (is_ta || has_extra_zero)

    if (has_pseudo_zero) {
      injections <- injections[-1L]
      if (!is.null(injection_times) && length(injection_times) >= 2L) {
        injection_times <- injection_times[-1L]
      }
    }

    if (length(injections) < 1L) {
      return(list(injections = numeric(0), injection_times = numeric(0)))
    }

    if (is.null(injection_times) || length(injection_times) != length(injections)) {
      if (nrow(df) >= max(injections)) {
        injection_times <- df$Time[injections]
      } else {
        injection_times <- numeric(0)
      }
    }

    list(
      injections = as.integer(injections),
      injection_times = as.numeric(injection_times)
    )
  }

  import_step1_path <- function(filepath, display_name = NULL, source_kind = "desktop_native", mark_restore = FALSE) {
    path_norm <- normalize_step1_path(filepath)
    if (!nzchar(path_norm) || !file.exists(path_norm)) return(FALSE)

    name_norm <- as.character(display_name %||% "")[1]
    name_norm <- trimws(name_norm)
    if (!nzchar(name_norm)) name_norm <- basename(path_norm)
    if (!identical(as.character(source_kind %||% "")[1], "sleep_restore")) {
      step1_sleep_restore_pending_params(NULL)
    }
    notify_ta_parse_hint(path_norm, display_name = name_norm)

    home_restore_inflight(isTRUE(mark_restore))
    restored_step1_path(path_norm)
    restored_step1_name(name_norm)
    step1_active_source_path(path_norm)
    step1_active_source_kind(as.character(source_kind %||% "unknown")[1])
    step1_import_started_at(as.numeric(Sys.time()))
    step1_import_source(as.character(source_kind %||% "unknown")[1])
    telemetry_log(
      event = "step1.import",
      level = "INFO",
      payload = list(stage = "select", source = as.character(source_kind %||% "unknown")[1], file_name = name_norm)
    )
    TRUE
  }

  restore_step1_home_payload <- function(record) {
    if (is.null(record) || !is.list(record)) return(FALSE)
    path <- normalize_step1_path(record$source_path)
    if (!nzchar(path) || !file.exists(path)) return(FALSE)

    display_name <- as.character(record$display_name %||% "")[1]
    display_name <- trimws(display_name)
    if (!nzchar(display_name) || !grepl("\\.(itc|txt|nitc|csc|xml)$", tolower(display_name))) {
      display_name <- basename(path)
    }

    import_step1_path(
      filepath = path,
      display_name = trimws(display_name),
      source_kind = "home_restore",
      mark_restore = TRUE
    )
  }

  home_register_restore("step1", restore_step1_home_payload)

  normalize_step1_sleep_scalar_chr <- function(value, default = "") {
    out <- as.character(value %||% "")[1]
    out <- trimws(out)
    if (nzchar(out)) out else default
  }

  normalize_step1_sleep_scalar_num <- function(value, default = NA_real_) {
    out <- suppressWarnings(as.numeric(value)[1])
    if (is.finite(out)) out else default
  }

  normalize_step1_sleep_scalar_lgl <- function(value, default = FALSE) {
    out <- suppressWarnings(as.logical(value)[1])
    if (isTRUE(is.na(out))) return(isTRUE(default))
    isTRUE(out)
  }

  notify_step1_sleep_restore <- function(message_en, message_zh = message_en, type = "warning", duration = 4) {
    lang_now <- tryCatch(lang(), error = function(e) "en")
    msg <- if (identical(lang_now, "zh")) message_zh else message_en
    showNotification(msg, type = type, duration = duration)
  }

  collect_step1_sleep_restore_snapshot <- function() {
    source_path <- resolve_step1_recent_source_path()
    if (!nzchar(source_path)) return(NULL)
    display_name <- normalize_step1_sleep_scalar_chr(current_itc_name(), default = basename(source_path))
    collect_bool <- function(id) {
      raw <- safe_step1_input_get(id)
      if (is.null(raw)) return(NULL)
      value <- suppressWarnings(as.logical(raw)[1])
      if (isTRUE(is.na(value))) return(NULL)
      isTRUE(value)
    }
    collect_num <- function(id) {
      raw <- safe_step1_input_get(id)
      if (is.null(raw)) return(NULL)
      value <- suppressWarnings(as.numeric(raw)[1])
      if (is.finite(value)) value else NULL
    }

    params <- list()
    params$zoom_baseline <- collect_bool("zoom_baseline")
    params$duration <- collect_num("duration")
    params$offset <- collect_num("offset")
    params$spar <- collect_num("spar")
    params$int_start_offset <- collect_num("int_start_offset")
    params$limit_integration <- collect_bool("limit_integration")
    params$integration_window <- collect_num("integration_window")
    params$param_syringe_mM <- collect_num("param_syringe_mM")
    params$param_cell_mM <- collect_num("param_cell_mM")
    params$param_V_pre_ul <- collect_num("param_V_pre_ul")
    params$param_V_inj_ul <- collect_num("param_V_inj_ul")
    params <- params[!vapply(params, is.null, logical(1))]

    list(
      source_path = source_path,
      display_name = display_name,
      params = params
    )
  }

  apply_step1_sleep_restore_params <- function(params) {
    if (!is.list(params)) return(invisible(FALSE))

    update_checkbox_if_needed <- function(id, value) {
      value_lgl <- suppressWarnings(as.logical(value)[1])
      if (isTRUE(is.na(value_lgl))) return(invisible(FALSE))
      updateCheckboxInput(session, id, value = isTRUE(value_lgl))
      invisible(TRUE)
    }
    update_numeric_if_needed <- function(id, value, round_int = FALSE) {
      value_num <- suppressWarnings(as.numeric(value)[1])
      if (!is.finite(value_num)) return(invisible(FALSE))
      if (isTRUE(round_int)) value_num <- as.integer(round(value_num))
      updateNumericInput(session, id, value = value_num)
      invisible(TRUE)
    }
    update_slider_if_needed <- function(id, value) {
      value_num <- suppressWarnings(as.numeric(value)[1])
      if (!is.finite(value_num)) return(invisible(FALSE))
      updateSliderInput(session, id, value = value_num)
      invisible(TRUE)
    }

    update_checkbox_if_needed("zoom_baseline", params$zoom_baseline)
    update_slider_if_needed("duration", params$duration)
    update_slider_if_needed("offset", params$offset)
    update_slider_if_needed("spar", params$spar)
    update_numeric_if_needed("int_start_offset", params$int_start_offset)
    update_checkbox_if_needed("limit_integration", params$limit_integration)
    update_numeric_if_needed("integration_window", params$integration_window, round_int = TRUE)
    update_numeric_if_needed("param_syringe_mM", params$param_syringe_mM)
    update_numeric_if_needed("param_cell_mM", params$param_cell_mM)
    update_numeric_if_needed("param_V_pre_ul", params$param_V_pre_ul)
    update_numeric_if_needed("param_V_inj_ul", params$param_V_inj_ul)

    invisible(TRUE)
  }

  apply_step1_sleep_restore_snapshot <- function(snapshot) {
    if (is.null(snapshot) || !is.list(snapshot)) return(FALSE)
    source_path <- normalize_step1_path(snapshot$source_path)
    if (!nzchar(source_path) || !file.exists(source_path)) {
      notify_step1_sleep_restore(
        "Step1 restore failed: source file is missing.",
        "Step1 恢复失败：源文件不存在。",
        type = "warning",
        duration = 5
      )
      return(FALSE)
    }

    display_name <- normalize_step1_sleep_scalar_chr(snapshot$display_name, default = basename(source_path))
    ok <- isTRUE(import_step1_path(
      filepath = source_path,
      display_name = display_name,
      source_kind = "sleep_restore",
      mark_restore = TRUE
    ))
    if (!isTRUE(ok)) {
      notify_step1_sleep_restore(
        "Step1 restore failed: unable to import source file.",
        "Step1 恢复失败：无法导入源文件。",
        type = "warning",
        duration = 5
      )
      return(FALSE)
    }

    params <- if (is.list(snapshot$params)) snapshot$params else list()
    step1_sleep_restore_pending_params(params)
    TRUE
  }

  sleep_restore_register(
    "step1",
    collect_fn = collect_step1_sleep_restore_snapshot,
    apply_fn = apply_step1_sleep_restore_snapshot
  )

  for (param_name in ANCHOR_BOUND_PARAM_IDS) {
    local({
      nm <- param_name
      min_id <- paste0("anchor_bound_", nm, "_min")
      max_id <- paste0("anchor_bound_", nm, "_max")
      observeEvent(
        {
          input[[min_id]]
          input[[max_id]]
        },
        {
          update_anchor_param_bound(
            param_name = nm,
            lower_in = input[[min_id]],
            upper_in = input[[max_id]],
            update_ui_inputs = TRUE
          )
        },
        ignoreInit = TRUE
      )
    })
  }

  observeEvent(input$reset_anchor_bounds, {
    default_bounds <- build_default_anchor_param_bounds()
    anchor_param_bounds(default_bounds)

    for (nm in ANCHOR_BOUND_PARAM_IDS) {
      b <- default_bounds[[nm]]
      updateNumericInput(session, paste0("anchor_bound_", nm, "_min"), value = as.numeric(b["lower"])[1])
      updateNumericInput(session, paste0("anchor_bound_", nm, "_max"), value = as.numeric(b["upper"])[1])
    }
    apply_all_anchor_bounds_to_sliders()
    showNotification(tr("anchor_bounds_reset_done", lang()), type = "message", duration = 3)
  }, ignoreInit = TRUE)

  observeEvent(input$anchor_bounds_editor_open, {
    anchor_bounds_editor_open(isTRUE(input$anchor_bounds_editor_open))
  }, ignoreInit = TRUE)

  observeEvent(input$file1, {
    if (is.null(input$file1) || is.null(input$file1$datapath)) return()
    step1_sleep_restore_pending_params(NULL)
    notify_ta_parse_hint(input$file1$datapath, display_name = input$file1$name)
    restored_step1_path(NULL)
    restored_step1_name(NULL)
    step1_active_source_path(normalize_step1_path(input$file1$datapath))
    step1_active_source_kind("upload")
    step1_import_started_at(as.numeric(Sys.time()))
    step1_import_source("upload")
    telemetry_log(
      event = "step1.import",
      level = "INFO",
      payload = list(stage = "select", source = "upload", file_name = input$file1$name %||% "")
    )
    home_restore_inflight(FALSE)
  }, ignoreInit = TRUE)

  observeEvent(input$step1_desktop_pick_file, {
    fn <- if (!is.null(session_desktop)) session_desktop$open_file else NULL
    if (!is.function(fn) || !isTRUE(desktop_open_file_enabled())) return()
    lang_now <- lang()
    telemetry_log(
      event = "step1.import",
      level = "INFO",
      payload = list(stage = "pick_open", source = "desktop_native")
    )

    fn(
      purpose = "step1_import",
      title = if (identical(lang_now, "zh")) "选择 ITC/TA 数据文件" else "Select ITC/TA Data File",
      filters = list(list(name = "ITC/TA Data", extensions = c("itc", "txt", "nitc", "csc", "xml"))),
      on_selected = function(result) {
        result_path <- normalize_step1_path(result$file_path %||% "")
        result_name <- as.character(result$file_name %||% "")[1]
        ok <- isTRUE(import_step1_path(
          filepath = result_path,
          display_name = result_name,
          source_kind = "desktop_native",
          mark_restore = FALSE
        ))
        if (!isTRUE(ok)) {
          showNotification(
            if (identical(lang_now, "zh")) "所选文件无效或不存在。" else "Selected file is invalid or missing.",
            type = "error",
            duration = 4
          )
        }
      },
      on_cancel = function(...) invisible(NULL),
      on_error = function(message, ...) {
        msg <- as.character(message %||% "")[1]
        if (!nzchar(trimws(msg))) {
          msg <- if (identical(lang_now, "zh")) "桌面文件选择失败。" else "Desktop file picker failed."
        }
        telemetry_log(
          event = "step1.import",
          level = "ERROR",
          payload = list(stage = "pick_error", source = "desktop_native", message = msg)
        )
        showNotification(msg, type = "error", duration = 5)
      }
    )
  }, ignoreInit = TRUE)

  # 仅在有可导出数据时显示导出按钮，否则显示提示
  canExport <- reactive({
    tryCatch({
      rawData()
      processedData()
      TRUE
    }, error = function(e) FALSE)
  })
  output$download_btn_ui <- renderUI({
    if (!isTRUE(canExport())) return(NULL)
    tagAppendAttributes(
      downloadButton("downloadData_processor", tr("export_processor_data", lang())),
      style = "width:100%;max-width:100%;box-sizing:border-box;"
    )
  })

  # 语言变化时更新所有输入控件的 label
  observeEvent(lang(), {
    l <- lang()
    updateActionButton(session, "btn_data_to_fit", label = tr("data_to_fit", l))
    updateSliderInput(session, "duration", label = tr("anchor_width", l))
    updateSliderInput(session, "offset", label = tr("anchor_offset", l))
    updateSliderInput(session, "spar", label = tr("spline_spar", l))
    updateNumericInput(session, "int_start_offset", label = tr("start_offset", l))
    updateCheckboxInput(session, "limit_integration", label = tr("limit_integration", l))
    tryCatch({
      updateNumericInput(session, "integration_window", label = tr("end_offset", l))
    }, error = function(e) NULL)
    updateCheckboxInput(session, "zoom_baseline", label = tr("zoom_baseline", l))
    updateNumericInput(session, "param_syringe_mM", label = tr("syringe_mM", l))
    updateNumericInput(session, "param_cell_mM", label = tr("cell_mM", l))
    updateNumericInput(session, "param_V_pre_ul", label = tr("v_pre_ul", l))
    updateNumericInput(session, "param_V_inj_ul", label = tr("v_inj_ul", l))
  }, ignoreInit = FALSE)

  # Y 轴 zoom 相关常量
  MIN_SPAN_RAW <- 0.1
  MIN_SPAN_CORRECTED <- 0.05
  SPAN_BUFFER <- 0.5

  # 反应式存储读取的数据
  rawData <- reactive({
    read_step1 <- function(path, display_name = NULL) {
      read_step1_input(
        file_path = path,
        display_name = display_name,
        app_dir = ITCPROCESSOR_APP_DIR,
        overwrite_ta_xlsx = TRUE
      )
    }

    restored_path <- normalize_step1_path(restored_step1_path())
    if (nzchar(restored_path)) {
      req(file.exists(restored_path))
      return(tryCatch({
        read_step1(
          path = restored_path,
          display_name = normalize_step1_sleep_scalar_chr(restored_step1_name(), default = basename(restored_path))
        )
      }, error = function(e) {
        showNotification(
          as.character(conditionMessage(e) %||% "Step1 import failed."),
          type = "error",
          duration = 8
        )
        started <- suppressWarnings(as.numeric(step1_import_started_at())[1])
        elapsed_ms <- if (is.finite(started)) as.numeric(Sys.time()) * 1000 - started * 1000 else NA_real_
        telemetry_log(
          event = "step1.import",
          level = "ERROR",
          payload = list(
            stage = "read",
            source = step1_import_source(),
            source_kind = step1_active_source_kind(),
            elapsed_ms = elapsed_ms
          ),
          err = e
        )
        stop(safeError(e))
      }))
    }

    req(input$file1)
    tryCatch({
      read_step1(
        path = input$file1$datapath,
        display_name = normalize_step1_sleep_scalar_chr(input$file1$name, default = basename(input$file1$datapath))
      )
    }, error = function(e) {
      showNotification(
        as.character(conditionMessage(e) %||% "Step1 import failed."),
        type = "error",
        duration = 8
      )
      started <- suppressWarnings(as.numeric(step1_import_started_at())[1])
      elapsed_ms <- if (is.finite(started)) as.numeric(Sys.time()) * 1000 - started * 1000 else NA_real_
      telemetry_log(
        event = "step1.import",
        level = "ERROR",
        payload = list(
          stage = "read",
          source = step1_import_source(),
          source_kind = step1_active_source_kind(),
          elapsed_ms = elapsed_ms
        ),
        err = e
      )
      stop(safeError(e))
    })
  })

  # 文件加载后，用解析出的实验参数更新侧栏输入
  observeEvent(rawData(), {
    rd <- rawData()
    p <- rd$params
    skip_recent_log <- isTRUE(home_restore_inflight())
    integration_window_snapshot <- input$integration_window
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

        off <- v / 10
        off <- max(0, min(50, off))
        off <- round(off)
        offset_init <- off
      }

      duration_bound <- get_anchor_bound_for_ui("duration")
      offset_bound <- get_anchor_bound_for_ui("offset")
      duration_init <- clamp_to_anchor_bound(duration_init, duration_bound, default = baseline_defaults$duration %||% 20)
      offset_init <- clamp_to_anchor_bound(offset_init, offset_bound, default = baseline_defaults$offset %||% 5)
      updateSliderInput(session, "duration", value = duration_init)
      updateSliderInput(session, "offset", value = offset_init)

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
          integration_window_snapshot <- suggested
        }
      }

      baseline_defaults$duration <- duration_init
      baseline_defaults$offset <- offset_init
      baseline_defaults$spar <- spar_init
      baseline_defaults$ready <- TRUE

      if (!skip_recent_log) {
        src_path <- resolve_step1_recent_source_path()
        home_add_recent(
          list(
            display_name = current_itc_name(),
            file_name = current_itc_name(),
            source_step = "step1",
            import_type = "itc",
            source_path = src_path,
            source_path_kind = "import"
          )
        )
      }
    }

    pending_sleep_params <- step1_sleep_restore_pending_params()
    if (is.list(pending_sleep_params)) {
      apply_step1_sleep_restore_params(pending_sleep_params)
      step1_sleep_restore_pending_params(NULL)
    }

    if (skip_recent_log) home_restore_inflight(FALSE)

    started <- suppressWarnings(as.numeric(step1_import_started_at())[1])
    elapsed_ms <- if (is.finite(started)) as.numeric(Sys.time()) * 1000 - started * 1000 else NA_real_
    telemetry_log(
      event = "step1.import",
      level = "INFO",
      payload = list(
        stage = "read",
        outcome = "ok",
        source = step1_import_source(),
        source_kind = step1_active_source_kind(),
        data_points = nrow(rd$data %||% data.frame()),
        injections = length(rd$injections %||% numeric(0)),
        elapsed_ms = elapsed_ms
      )
    )
  })

  observeEvent(input$reset_baseline, {
    if (!isTRUE(baseline_defaults$ready)) return()
    duration_reset <- clamp_to_anchor_bound(
      baseline_defaults$duration,
      get_anchor_bound_for_ui("duration"),
      default = 20
    )
    offset_reset <- clamp_to_anchor_bound(
      baseline_defaults$offset,
      get_anchor_bound_for_ui("offset"),
      default = 5
    )
    baseline_defaults$duration <- duration_reset
    baseline_defaults$offset <- offset_reset
    updateSliderInput(session, "duration", value = duration_reset)
    updateSliderInput(session, "offset", value = offset_reset)
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
    paste0(tr("loaded", l), current_itc_name(),
           ";  ", tr("total_points", l), nrow(rawData()$data)
           )
  })
  
  # 反应式计算基线
  baselineData <- reactive({
    req(rawData())
    df <- rawData()$data
    baseline_series <- resolve_baseline_injection_series(rawData())
    injections <- baseline_series$injections
    inj_times <- baseline_series$injection_times
    
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
      parameter = c("original_itc_file", "Temp_K", "G_syringe_mM", "H_cell_0_mM", "V_pre_uL", "V_inj_uL", "n_inj", "V_cell_mL", "generated_by"),
      value = c(
        current_itc_name(),
        temp_K,
        if (!is.na(input$param_syringe_mM)) input$param_syringe_mM else p$syringe_conc_mM,
        if (!is.na(input$param_cell_mM)) input$param_cell_mM else p$cell_conc_mM,
        if (!is.na(input$param_V_pre_ul)) input$param_V_pre_ul else p$V_pre_ul,
        if (!is.na(input$param_V_inj_ul)) input$param_V_inj_ul else p$V_inj_ul,
        n_inj,
        p$cell_volume_mL,
        build_viabind_signature("ITCprocessor")
      ),
      stringsAsFactors = FALSE
    )

    list(
      schema_version = "itcsuite.step1.v1",
      created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
      token = NA_real_,
      integration = pd$integration,
      meta = meta_df,
      source = if (nzchar(current_itc_name())) current_itc_name() else "ITCprocessor",
      bundle = list(
        schema_version = "itcsuite.bundle.v1",
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

  observeEvent(input$btn_data_to_fit, {
    click_id <- suppressWarnings(as.integer(input$btn_data_to_fit)[1])
    if (!is.finite(click_id) || click_id <= 0L) return()
    payload <- step1_payload()
    if (is.null(payload)) {
      telemetry_log(
        event = "step1.bridge",
        level = "WARN",
        payload = list(action = "send_to_step2", outcome = "empty_payload")
      )
      showNotification(tr("data_to_fit_unavailable", lang()), type = "warning", duration = 3)
      return()
    }
    payload$token <- next_bridge_token()
    payload$created_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
    bridge_set("step1_payload", payload)
    rows <- if (is.data.frame(payload$integration)) nrow(payload$integration) else NA_integer_
    telemetry_log(
      event = "step1.bridge",
      level = "INFO",
      payload = list(
        action = "send_to_step2",
        outcome = "ok",
        token = payload$token,
        integration_rows = rows
      )
    )
    tryCatch({
      updateTabsetPanel(session, "main_tabs", selected = "step2")
    }, error = function(e) NULL)
    showNotification(tr("data_sent_to_step2", lang()), type = "message", duration = 2)
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
    ed_names <- names(ed)
    has_autorange <- any(vapply(ed_names, function(k) {
      grepl("autorange$", k) && isTRUE(ed[[k]])
    }, logical(1)))
    if (isTRUE(ed$autosize) || has_autorange) {
      return(list(reset = TRUE, x = NULL, y = NULL))
    }
    extract_axis_range <- function(axis_prefix) {
      k0 <- paste0(axis_prefix, ".range[0]")
      k1 <- paste0(axis_prefix, ".range[1]")
      kv <- paste0(axis_prefix, ".range")
      if (!is.null(ed[[k0]]) && !is.null(ed[[k1]])) {
        out <- c(suppressWarnings(as.numeric(ed[[k0]])[1]), suppressWarnings(as.numeric(ed[[k1]])[1]))
        if (all(is.finite(out))) return(out)
      }
      if (!is.null(ed[[kv]])) {
        vv <- suppressWarnings(as.numeric(unlist(ed[[kv]], use.names = FALSE)))
        if (length(vv) >= 2) {
          out <- vv[1:2]
          if (all(is.finite(out))) return(out)
        }
      }
      NULL
    }
    x <- extract_axis_range("xaxis")
    y <- extract_axis_range("yaxis")
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
    baseline_series <- resolve_baseline_injection_series(rawData())
    injections <- baseline_series$injections
    raw_inj_times <- baseline_series$injection_times
    time_min <- df$Time / 60
    
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
      add_lines(y = ~df$Power, name = tr("raw_data", lang()),
                line = list(color = 'black', width = 1)) %>%
      add_lines(y = ~base, name = tr("baseline", lang()),
                line = list(color = 'blue', width = 2))

    if (nrow(spline_anchors) > 0) {
      p <- p %>% add_markers(x = spline_anchors$x / 60, y = spline_anchors$y,
                             name = tr("spline_anchors", lang()),
                             marker = list(color = 'orange', size = 7, symbol = "x"))
    }
    
    if (length(injections) > 0) {
      p <- p %>% 
        add_markers(x = inj_times_min, y = inj_powers, name = tr("injection", lang()),
                    marker = list(color = 'red', size = 10, symbol = "triangle-down")) %>%
        add_markers(x = anchor_end_times_min, y = anchor_vals, name = tr("anchor", lang()),
                    marker = list(color = 'green', size = 8, symbol = "circle"))
    }
    
    layout_args <- list(
      title = list(text = tr("baseline", lang()), font = list(size = 16), x = 0, xanchor = "left", y = 0.98, yanchor = "top"),
      xaxis = list(
        title = list(text = tr("time_min", lang()), standoff = 7),
        side = "top",
        showgrid = TRUE,
        ticks = "outside",
        showline = TRUE,
        mirror = FALSE,
        automargin = FALSE,
        zeroline = FALSE
      ),
      yaxis = list(
        title = tr("power_ucal", lang()),
        showline = TRUE,
        automargin = FALSE,
        zeroline = FALSE
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
    } else {
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
    
    p <- plot_ly(source = "plot_corrected") %>%
      add_lines(x = ~time_min, y = ~corrected, 
            line = list(color = 'green', width = 1), name = tr("corrected_power", lang())) %>%
      layout(
        title = list(text = tr("corrected_baseline", lang()), font = list(size = 16), x = 0, xanchor = "left", y = 0.98, yanchor = "top"),
        xaxis = list(
          title = "",
          showticklabels = FALSE,
          ticks = "",
          showline = TRUE,
          automargin = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(title = paste0(tr("delta_power", lang()), " (ucal/s)"), showline = TRUE, automargin = FALSE, zeroline = FALSE),
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
                              name = tr("integrated_area", lang()),
                              line = list(width = 0),
                              fill = 'tozeroy',
                              fillcolor = 'rgba(255, 165, 0, 0.5)',
                              hoverinfo = "skip")
        
         start_vals <- approx(df$Time, corrected, xout = start_times)$y
         end_vals <- approx(df$Time, corrected, xout = end_times)$y
         p <- p %>% add_markers(x = start_times/60, y = start_vals, 
                                name = tr("integration_start", lang()),
                                marker = list(color = 'blue', size = 8, symbol = "circle-open")) %>%
           add_markers(x = end_times/60, y = end_vals, 
                       name = tr("integration_end", lang()),
                       marker = list(color = 'purple', size = 8, symbol = "x"))
      }
    }
    
    layout_args <- list()
    
    if (input$zoom_baseline) {
       baseline_series <- resolve_baseline_injection_series(rawData())
       injections <- baseline_series$injections
       raw_inj_times <- baseline_series$injection_times
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
             title = paste0(tr("delta_power", lang()), " (ucal/s)"),
             showline = TRUE,
             automargin = FALSE,
             zeroline = FALSE,
             range = auto_zoom_range(
               baseline_vals,
               MIN_SPAN_CORRECTED,
               bottom_frac = 0.22,
               top_frac = 0.12,
               top_abs = 0.002
             )
           )
       }
       
    } else {
       if (!is.null(user_zoom_corrected$y)) {
         layout_args$yaxis <- list(
           title = paste0(tr("delta_power", lang()), " (ucal/s)"),
           showline = TRUE,
           automargin = FALSE,
           zeroline = FALSE,
           range = user_zoom_corrected$y
         )
       } else {
         layout_args$yaxis <- list(
           title = paste0(tr("delta_power", lang()), " (ucal/s)"),
           showline = TRUE,
           automargin = FALSE,
           zeroline = FALSE,
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
        title = list(text = tr("integration", lang()), font = list(size = 16), x = 0, xanchor = "left"),
        xaxis = list(showticklabels = FALSE, title = "", zeroline = FALSE),
        yaxis = list(title = tr("heat_ucal", lang()), zeroline = FALSE)
      ))
    }
    
    plot_ly(data = int_res, x = ~Injection, y = ~Heat_ucal, type = 'scatter', mode = 'markers',
            marker = list(size = 10, color = 'black', symbol = 'circle')) %>%
      layout(
        title = list(text = tr("integration", lang()), font = list(size = 16), x = 0, xanchor = "left", y = 0.98, yanchor = "top"),
        xaxis = list(
          showticklabels = FALSE,
          ticks = "",
          title = "",
          showline = TRUE,
          automargin = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(title = tr("heat_ucal", lang()), showline = TRUE, automargin = FALSE, zeroline = FALSE),
        margin = list(l = 72, r = 14, t = 28, b = 8),
        showlegend = FALSE
      )
  })
  
  output$downloadData_processor <- downloadHandler(
    filename = function() {
      src_name <- current_itc_name()
      base_name <- if (!nzchar(src_name)) "data" else tools::file_path_sans_ext(src_name)
      fname <- paste0(base_name, "_processed_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx")
      step1_last_export_name(fname)
      fname
    },
    content = function(file) {
      op_id <- telemetry_start(
        event = "step1.export",
        payload = list(format = "xlsx", action = "export_processed")
      )
      op_finished <- FALSE
      finish_export <- function(outcome = "ok", payload = list(), err = NULL, level = NULL) {
        if (isTRUE(op_finished)) return(invisible(NULL))
        op_finished <<- TRUE
        telemetry_finish(op_id, outcome = outcome, payload = payload, err = err, level = level)
      }
      on.exit({
        if (!isTRUE(op_finished)) {
          finish_export(
            outcome = "error",
            payload = list(reason = "unexpected_exit", target_file = file),
            level = "ERROR"
          )
        }
      }, add = TRUE)

      rd <- tryCatch(rawData(), error = function(e) NULL)
      pd <- tryCatch(processedData(), error = function(e) NULL)
      if (is.null(rd) || is.null(pd)) {
        finish_export(
          outcome = "error",
          payload = list(reason = "no_data", target_file = file),
          level = "ERROR"
        )
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

      original_itc_name <- current_itc_name()
      meta_df <- data.frame(
        parameter = c("original_itc_file", "Temp_K", "G_syringe_mM", "H_cell_0_mM", "V_pre_uL", "V_inj_uL", "n_inj", "V_cell_mL", "generated_by"),
        value = c(original_itc_name, temp_K, syringe_mM, cell_mM, V_pre, V_inj, n_inj, cell_vol, build_viabind_signature("ITCprocessor")),
        unit = c("", "K", "mM", "mM", "\u00B5L", "\u00B5L", "", "mL", "")
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

      export_name <- as.character(step1_last_export_name() %||% "")[1]
      if (!nzchar(trimws(export_name))) export_name <- basename(file)
      export_path <- normalize_step1_path(file)
      import_path <- normalize_step1_path(restored_step1_path() %||% tryCatch(input$file1$datapath, error = function(e) ""))
      if (!nzchar(import_path)) {
        finish_export(
          outcome = "ok",
          payload = list(target_file = file, source_path = "", source_path_linked = FALSE)
        )
        return(invisible(NULL))
      }
      home_add_recent_export(
        list(
          display_name = export_name,
          file_name = export_name,
          source_step = "step1",
          target_step = "step1",
          export_type = "xlsx",
          source_path = import_path,
          artifact_path = export_path,
          source_path_kind = "import"
        )
      )
      finish_export(
        outcome = "ok",
        payload = list(
          target_file = file,
          source_path = import_path,
          rows_power = nrow(power_df),
          rows_integration = nrow(pd$integration %||% data.frame())
        )
      )
    }
  )

}

shinyApp(ui = ui, server = server)
