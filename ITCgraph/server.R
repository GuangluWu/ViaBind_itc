# ==============================================================================
# server.R - ITCgraph 服务端逻辑
# ==============================================================================

server <- function(input, output, session) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  # ============================================================
  # 1. 语言状态管理
  # ============================================================
  current_lang <- reactiveVal("en")  # 默认英文
  lang <- reactive({ current_lang() })
  
  observeEvent(input$lang_toggle, {
    new_lang <- if (current_lang() == "en") "zh" else "en"
    current_lang(new_lang)
  })
  
  # ============================================================
  # 2. 数据存储
  # ============================================================
  imported_data <- reactiveValues(
    power       = NULL,   # power_corrected sheet
    integration = NULL,   # integration sheet
    simulation  = NULL,   # simulation sheet
    fit_params  = NULL,   # fit_params sheet
    meta        = NULL,   # meta sheet
    filename    = NULL    # 原始文件名
  )

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
  bridge_last_token <- reactiveVal(NA_real_)

  bridge_store_get_all <- function() {
    x <- get0(bridge_store_name, envir = .GlobalEnv, inherits = FALSE, ifnotfound = NULL)
    if (is.null(x) || !is.list(x)) return(list())
    x
  }

  bridge_get <- function(channel) {
    ch <- if (!is.null(session_bridge)) session_bridge[[channel]] else NULL
    if (is.function(ch)) return(ch())
    store <- bridge_store_get_all()
    entry <- store[[bridge_session_key]]
    if (is.null(entry) || !is.list(entry)) return(NULL)
    entry[[channel]]
  }

  consume_step2_plot_payload <- function(payload) {
    if (is.null(payload) || !is.list(payload)) return(FALSE)
    token <- suppressWarnings(as.numeric(payload$token)[1])
    if (is.finite(token) && !is.na(bridge_last_token()) && identical(token, bridge_last_token())) return(FALSE)

    sheets <- payload$sheets
    if (!is.null(sheets) && is.list(sheets)) {
      if ("power_corrected" %in% names(sheets) && is.data.frame(sheets$power_corrected)) {
        imported_data$power <- as.data.frame(sheets$power_corrected)
      }
      if ("integration_rev" %in% names(sheets) && is.data.frame(sheets$integration_rev)) {
        imported_data$integration <- as.data.frame(sheets$integration_rev)
      } else if ("integration" %in% names(sheets) && is.data.frame(sheets$integration)) {
        imported_data$integration <- as.data.frame(sheets$integration)
      }
      if ("simulation" %in% names(sheets) && is.data.frame(sheets$simulation)) {
        imported_data$simulation <- as.data.frame(sheets$simulation)
      }
      if ("fit_params" %in% names(sheets) && is.data.frame(sheets$fit_params)) {
        imported_data$fit_params <- as.data.frame(sheets$fit_params)
      }
      if ("meta_rev" %in% names(sheets) && is.data.frame(sheets$meta_rev)) {
        imported_data$meta <- as.data.frame(sheets$meta_rev)
      } else if ("meta" %in% names(sheets) && is.data.frame(sheets$meta)) {
        imported_data$meta <- as.data.frame(sheets$meta)
      }
    }

    if (is.null(imported_data$integration) && !is.null(payload$integration_rev) && is.data.frame(payload$integration_rev)) {
      imported_data$integration <- as.data.frame(payload$integration_rev)
    }
    if (is.null(imported_data$simulation) && !is.null(payload$simulation) && is.data.frame(payload$simulation)) {
      imported_data$simulation <- as.data.frame(payload$simulation)
    }
    if (is.null(imported_data$fit_params) && !is.null(payload$fit_params) && is.data.frame(payload$fit_params)) {
      imported_data$fit_params <- as.data.frame(payload$fit_params)
    }
    if (is.null(imported_data$meta) && !is.null(payload$meta_rev) && is.data.frame(payload$meta_rev)) {
      imported_data$meta <- as.data.frame(payload$meta_rev)
    }

    src <- payload$source %||% "Step2 bridge"
    imported_data$filename <- as.character(src)

    if (!is.null(imported_data$fit_params) && all(c("parameter", "value") %in% names(imported_data$fit_params))) {
      fp <- setNames(as.character(imported_data$fit_params$value), trimws(as.character(imported_data$fit_params$parameter)))
      if ("Offset_cal" %in% names(fp)) {
        off <- suppressWarnings(as.numeric(fp[["Offset_cal"]]))
        if (is.finite(off)) updateNumericInput(session, "graph_heat_offset", value = off)
      }
    }

    if (is.finite(token)) bridge_last_token(token)
    TRUE
  }

  if (!is.null(session_bridge) && is.function(session_bridge$step2_plot_payload)) {
    observeEvent(session_bridge$step2_plot_payload(), {
      consume_step2_plot_payload(session_bridge$step2_plot_payload())
    }, ignoreNULL = TRUE)
  } else {
    observe({
      invalidateLater(300, session)
      consume_step2_plot_payload(bridge_get("step2_plot_payload"))
    })
  }
  
  expand_limits <- function(v, mult = 0.05) {
    r <- range(v, na.rm = TRUE)
    d <- diff(r)
    if (d < 1e-10) d <- 1
    c(r[1] - mult * d, r[2] + mult * d)
  }
  
  # ============================================================
  # 3. 动态 UI 渲染（i18n）
  # ============================================================
  
  # 标题
  output$app_title <- renderUI({
    h3(tr("app_title", lang()), style = "margin: 5px 0;")
  })
  
  # 语言按钮：小国旗 + 目标语言名（参考 ITC processor）
  observe({
    btn_label <- if (lang() == "en") {
      paste0("\U0001F1E8\U0001F1F3 ", tr("lang_switch", "en"))  # 🇨🇳 中文
    } else {
      paste0("\U0001F1EC\U0001F1E7 ", tr("lang_switch", "zh"))  # 🇬🇧 English
    }
    updateActionButton(session, "lang_toggle", label = btn_label)
  })
  
  # 预览标题
  output$preview_header <- renderUI({
    h4(tr("preview_title", lang()))
  })
  
  # 导入区域标题
  output$section_import_ui <- renderUI({
    h4(tr("section_import", lang()))
  })
  
  # 数据概要
  output$data_summary_ui <- renderUI({
    if (is.null(imported_data$filename)) {
      return(div(class = "data-summary", em(tr("no_data", lang()))))
    }
    n_power <- if (!is.null(imported_data$power)) nrow(imported_data$power) else 0
    n_int   <- if (!is.null(imported_data$integration)) nrow(imported_data$integration) else 0
    n_sim   <- if (!is.null(imported_data$simulation)) nrow(imported_data$simulation) else 0
    
    div(class = "data-summary",
      tags$b(tr("file_label", lang()), ": "), imported_data$filename, br(),
      tr("power_points", lang()), ": ", tags$span(class = "badge", n_power), br(),
      tr("integration_points", lang()), ": ", tags$span(class = "badge", n_int), br(),
      tr("simulation_points", lang()), ": ", tags$span(class = "badge", n_sim)
    )
  })
  
  # 基线热扣除、能量单位标签（与下方输入框对应）
  output$heat_offset_label_ui <- renderUI({
    l <- lang()
    fluidRow(
      column(6, tags$label(tr("heat_offset_label", l), style = "font-size: 12px;")),
      column(6, tags$label(tr("energy_unit_label", l), style = "font-size: 12px;"))
    )
  })
  
  # ---- 上 Panel 设置标签（与下方输入 4-5-3 列对齐）----
  output$section_top_ui <- renderUI({
    tagList(
      h4(tr("section_top_panel", lang())),
      fluidRow(
        column(4, tags$label(tr("top_xlab", lang()), style = "display: block; margin-bottom: 5px;")),
        column(5, tags$label(tr("top_ylab", lang()), style = "display: block; margin-bottom: 5px;")),
        column(3, tags$label(tr("time_unit_label", lang()), style = "display: block; margin-bottom: 5px;"))
      )
    )
  })
  
  # 时间单位选择器
  output$time_unit_ui <- renderUI({
    selectInput("top_time_unit", label = NULL,
      choices = c("min" = "min", "s" = "s"),
      selected = PLOT_DEFAULTS$top_time_unit
    )
  })
  
  # 上 Panel X 轴范围：标签 + Min + Max + Auto range 同一行
  output$top_xrange_ui <- renderUI({
    l <- lang()
    div(class = "range-one-row",
      tags$span(class = "range-label", paste0(tr("top_xrange", l), ":")),
      div(class = "range-min",
        numericInput("top_xmin", "Min", value = isolate(input$top_xmin) %||% 0, step = 5)),
      div(class = "range-max",
        numericInput("top_xmax", "Max", value = isolate(input$top_xmax) %||% 100, step = 5)),
      actionButton("top_auto_xrange", tr("top_auto_range", l), class = "btn-sm btn-info")
    )
  })
  
  # 上 Panel Y 轴范围：标签 + Min + Max + Auto range 同一行
  output$top_yrange_ui <- renderUI({
    l <- lang()
    div(class = "range-one-row",
      tags$span(class = "range-label", paste0(tr("top_yrange", l), ":")),
      div(class = "range-min",
        numericInput("top_ymin", "Min", value = isolate(input$top_ymin) %||% -5, step = 0.5)),
      div(class = "range-max",
        numericInput("top_ymax", "Max", value = isolate(input$top_ymax) %||% 5, step = 0.5)),
      actionButton("top_auto_yrange", tr("top_auto_range", l), class = "btn-sm btn-info")
    )
  })
  
  # 上 Panel 颜色
  output$top_color_ui <- renderUI({
    l <- lang()
    if (requireNamespace("colourpicker", quietly = TRUE)) {
      colourpicker::colourInput("top_line_color", tr("top_line_color", l),
                                value = PLOT_DEFAULTS$top_line_color, showColour = "both")
    } else {
      textInput("top_line_color", tr("top_line_color", l), value = PLOT_DEFAULTS$top_line_color)
    }
  })
  
  # 上 Panel 线宽标签
  observe({
    updateNumericInput(session, "top_line_width", label = tr("top_line_width", lang()))
  })
  
  # ---- 下 Panel 设置标签 ----
  output$section_bot_ui <- renderUI({
    tagList(
      h4(tr("section_bot_panel", lang())),
      fluidRow(
        column(6, tags$label(tr("bot_xlab", lang()))),
        column(6, tags$label(tr("bot_ylab", lang())))
      )
    )
  })
  
  # 下 Panel X 轴范围：标签 + Min + Max + Auto range 同一行
  output$bot_xrange_ui <- renderUI({
    l <- lang()
    div(class = "range-one-row",
      tags$span(class = "range-label", paste0(tr("bot_xrange", l), ":")),
      div(class = "range-min",
        numericInput("bot_xmin", "Min", value = isolate(input$bot_xmin) %||% 0, step = 0.1)),
      div(class = "range-max",
        numericInput("bot_xmax", "Max", value = isolate(input$bot_xmax) %||% 3, step = 0.1)),
      actionButton("bot_auto_xrange", tr("bot_auto_range", l), class = "btn-sm btn-info")
    )
  })
  
  # 下 Panel Y 轴范围：标签 + Min + Max + Auto range 同一行
  output$bot_yrange_ui <- renderUI({
    l <- lang()
    div(class = "range-one-row",
      tags$span(class = "range-label", paste0(tr("bot_yrange", l), ":")),
      div(class = "range-min",
        numericInput("bot_ymin", "Min", value = isolate(input$bot_ymin) %||% -20, step = 0.5)),
      div(class = "range-max",
        numericInput("bot_ymax", "Max", value = isolate(input$bot_ymax) %||% 5, step = 0.5)),
      actionButton("bot_auto_yrange", tr("bot_auto_range", l), class = "btn-sm btn-info")
    )
  })
  
  # 下 Panel 散点颜色
  output$bot_point_color_ui <- renderUI({
    l <- lang()
    if (requireNamespace("colourpicker", quietly = TRUE)) {
      colourpicker::colourInput("bot_point_color", tr("bot_point_color", l),
                                value = PLOT_DEFAULTS$bot_point_color, showColour = "both")
    } else {
      textInput("bot_point_color", tr("bot_point_color", l), value = PLOT_DEFAULTS$bot_point_color)
    }
  })
  
  # 下 Panel 散点填充颜色
  output$bot_point_fill_ui <- renderUI({
    l <- lang()
    if (requireNamespace("colourpicker", quietly = TRUE)) {
      colourpicker::colourInput("bot_point_fill", tr("bot_point_fill", l),
                                value = PLOT_DEFAULTS$bot_point_fill, showColour = "both")
    } else {
      textInput("bot_point_fill", tr("bot_point_fill", l), value = PLOT_DEFAULTS$bot_point_fill)
    }
  })
  
  # 散点大小标签
  observe({
    updateNumericInput(session, "bot_point_size", label = tr("bot_point_size", lang()))
  })
  
  observe({
    updateNumericInput(session, "bot_point_fill_alpha", label = tr("bot_point_fill_alpha", lang()))
  })
  
  # 散点形状（selectize=FALSE 使下拉由浏览器原生渲染，浮于最上层不被遮挡）
  output$bot_shape_ui <- renderUI({
    l <- lang()
    selectInput("bot_point_shape", tr("bot_point_shape", l),
      choices = c(
        "Open circle (21)"    = 21,
        "Filled circle (19)"  = 19,
        "Open square (22)"    = 22,
        "Open diamond (23)"   = 23,
        "Open triangle (24)"  = 24,
        "Cross (4)"           = 4
      ),
      selected = PLOT_DEFAULTS$bot_point_shape,
      selectize = FALSE
    )
  })
  
  # 拟合线颜色
  output$bot_line_color_ui <- renderUI({
    l <- lang()
    if (requireNamespace("colourpicker", quietly = TRUE)) {
      colourpicker::colourInput("bot_line_color", tr("bot_line_color", l),
                                value = PLOT_DEFAULTS$bot_line_color, showColour = "both")
    } else {
      textInput("bot_line_color", tr("bot_line_color", l), value = PLOT_DEFAULTS$bot_line_color)
    }
  })
  
  # 拟合线宽、线型
  observe({
    updateNumericInput(session, "bot_line_width", label = tr("bot_line_width", lang()))
  })
  
  output$bot_line_linetype_ui <- renderUI({
    l <- lang()
    choices <- setNames(
      c("solid", "dashed", "dotted", "dotdash", "longdash"),
      c(tr("linetype_solid", l), tr("linetype_dashed", l), tr("linetype_dotted", l),
        tr("linetype_dotdash", l), tr("linetype_longdash", l))
    )
    selectInput("bot_line_linetype", tr("bot_line_linetype", l),
                choices = choices, selected = PLOT_DEFAULTS$bot_line_linetype,
                selectize = FALSE)
  })
  
  output$bot_layer_order_ui <- renderUI({
    l <- lang()
    choices <- setNames(
      c("points_over_line", "line_over_points"),
      c(tr("layer_points_over_line", l), tr("layer_line_over_points", l))
    )
    selectInput("bot_layer_order", tr("bot_layer_order", l),
                choices = choices, selected = PLOT_DEFAULTS$bot_layer_order,
                selectize = FALSE)
  })
  
  output$bot_first_point_dim_ui <- renderUI({
    checkboxInput("bot_dim_first_point", tr("bot_dim_first_point", lang()),
                  value = isolate(input$bot_dim_first_point) %||% PLOT_DEFAULTS$bot_dim_first_point)
  })
  
  # ---- 全局设置标签 ----
  output$section_global_ui <- renderUI({
    l <- lang()
    tagList(
      h4(tr("section_global", l)),
      fluidRow(
        column(3, tags$label(tr("global_base_size", l))),
        column(3, tags$label(tr("top_ratio_label", l))),
        column(3, tags$label(tr("bot_ratio_label", l))),
        column(3, tags$label(tr("border_linewidth_label", l)))
      )
    )
  })
  
  # ---- 保存/导入设置：左右两列，每列上字下钮（i18n）----
  output$settings_save_hint_ui <- renderUI({
    div(class = "settings-hint", tr("settings_save_hint", lang()))
  })
  output$settings_import_hint_ui <- renderUI({
    div(class = "settings-hint", tr("settings_import_hint", lang()))
  })
  output$save_settings_ui <- renderUI({
    downloadButton("save_settings", label = tr("btn_save_settings", lang()),
                  class = "btn-success btn-save-settings")
  })
  output$import_settings_ui <- renderUI({
    fileInput("import_settings_file", label = NULL,
              accept = ".json", buttonLabel = tr("btn_load_settings", lang()), placeholder = "JSON")
  })
  
  # ---- 导出区域标签 ----
  output$section_export_ui <- renderUI({
    l <- lang()
    tagList(
      h4(tr("section_export", l)),
      fluidRow(
        column(4, tags$label(tr("export_width", l))),
        column(4, tags$label(tr("export_height", l))),
        column(4, tags$label(tr("export_dpi", l)))
      )
    )
  })
  
  # ============================================================
  # 4. 数据导入
  # ============================================================
  observeEvent(input$xlsx_file, {
    req(input$xlsx_file)
    filepath <- input$xlsx_file$datapath
    
    tryCatch({
      sheets <- readxl::excel_sheets(filepath)
      
      # 读取 power_corrected
      if ("power_corrected" %in% sheets) {
        pc <- readxl::read_excel(filepath, sheet = "power_corrected")
        if ("Time_s" %in% colnames(pc) && "Power_corrected_ucal_s" %in% colnames(pc)) {
          imported_data$power <- as.data.frame(pc)
        }
      }
      
      # 读取 integration_rev
      if ("integration_rev" %in% sheets) {
        int_df <- readxl::read_excel(filepath, sheet = "integration_rev")
        if ("Ratio_App" %in% colnames(int_df) && "heat_cal_mol" %in% colnames(int_df)) {
          imported_data$integration <- as.data.frame(int_df)
        }
      }
      
      # 读取 simulation
      if ("simulation" %in% sheets) {
        sim_df <- readxl::read_excel(filepath, sheet = "simulation")
        if ("Ratio_App" %in% colnames(sim_df) && "dQ_App" %in% colnames(sim_df)) {
          imported_data$simulation <- as.data.frame(sim_df)
        }
      }
      
      # 读取 fit_params（竖列 parameter/value）；若有 Offset_cal 则填入 Baseline heat offset，并用于下 panel 范围计算（扣完基线后定范围）
      offset_for_range <- as.numeric(input$graph_heat_offset %||% PLOT_DEFAULTS$heat_offset)
      if ("fit_params" %in% sheets) {
        imported_data$fit_params <- as.data.frame(readxl::read_excel(filepath, sheet = "fit_params"))
        fp_df <- imported_data$fit_params
        if (!is.null(fp_df) && nrow(fp_df) >= 1 && all(c("parameter", "value") %in% colnames(fp_df))) {
          param_vals <- setNames(as.character(fp_df$value), trimws(as.character(fp_df$parameter)))
          if ("Offset_cal" %in% names(param_vals)) {
            offset_num <- suppressWarnings(as.numeric(param_vals["Offset_cal"]))
            if (length(offset_num) > 0 && !is.na(offset_num[1])) {
              offset_for_range <- offset_num[1]
              updateNumericInput(session, "graph_heat_offset", value = offset_num[1])
            }
          }
        }
      }
      
      # 读取 meta_rev（可选）
      if ("meta_rev" %in% sheets) {
        imported_data$meta <- as.data.frame(readxl::read_excel(filepath, sheet = "meta_rev"))
      }
      
      imported_data$filename <- input$xlsx_file$name
      
      # 通知
      has_any <- !is.null(imported_data$power) || !is.null(imported_data$integration) || !is.null(imported_data$simulation)
      if (has_any) {
        showNotification(tr("import_success", lang()), type = "message", duration = 3)
      }
      
      # 导入后用与绘图一致的 expand 计算范围（下 panel 用扣完基线后的值），设为当前轴范围
      if (!is.null(imported_data$power)) {
        time_unit <- input$top_time_unit %||% "min"
        x_vals <- if (time_unit == "min") imported_data$power$Time_s / 60 else imported_data$power$Time_s
        r_top_x <- round(expand_limits(x_vals, 0.02), 2)
        updateNumericInput(session, "top_xmin", value = r_top_x[1])
        updateNumericInput(session, "top_xmax", value = r_top_x[2])
        y_raw <- imported_data$power$Power_corrected_ucal_s
        energy_unit <- input$energy_unit %||% PLOT_DEFAULTS$energy_unit
        if (identical(energy_unit, "J")) y_raw <- y_raw * 4.184
        r_top_y <- round(expand_limits(y_raw, 0.05), 2)
        updateNumericInput(session, "top_ymin", value = r_top_y[1])
        updateNumericInput(session, "top_ymax", value = r_top_y[2])
      }
      int_data <- imported_data$integration
      sim_data <- imported_data$simulation
      has_int <- !is.null(int_data) && nrow(int_data) > 0
      has_sim <- !is.null(sim_data) && nrow(sim_data) > 0
      if (has_int || has_sim) {
        x_vals <- c(
          if (has_int) int_data$Ratio_App else NULL,
          if (has_sim) sim_data$Ratio_App else NULL
        )
        x_vals <- x_vals[is.finite(x_vals)]
        if (length(x_vals) > 0) {
          r_bot_x <- round(expand_limits(x_vals, 0.05), 2)
          updateNumericInput(session, "bot_xmin", value = r_bot_x[1])
          updateNumericInput(session, "bot_xmax", value = r_bot_x[2])
        }
        energy_unit <- input$energy_unit %||% PLOT_DEFAULTS$energy_unit
        to_kcal <- function(v) (v - offset_for_range) / 1000
        to_display <- if (identical(energy_unit, "J")) function(v) to_kcal(v) * 4.184 else to_kcal
        y_vals <- c(
          if (has_int) to_display(int_data$heat_cal_mol) else NULL,
          if (has_sim) to_display(sim_data$dQ_App) else NULL
        )
        y_vals <- y_vals[is.finite(y_vals)]
        if (length(y_vals) > 0) {
          r_bot_y <- round(expand_limits(y_vals, 0.05), 2)
          updateNumericInput(session, "bot_ymin", value = r_bot_y[1])
          updateNumericInput(session, "bot_ymax", value = r_bot_y[2])
        }
      }
      
    }, error = function(e) {
      showNotification(paste0(tr("import_error", lang()), e$message), type = "error", duration = 8)
    })
  })
  
  # ============================================================
  # 4b. 「自动范围」按钮：按当前数据与当前单位重新计算轴范围（尤其适合换单位后）
  # ============================================================
  observeEvent(input$top_auto_xrange, {
    if (is.null(imported_data$power) || !("Time_s" %in% colnames(imported_data$power))) {
      showNotification(tr("no_data", lang()), type = "message", duration = 2)
      return(invisible())
    }
    time_unit <- input$top_time_unit %||% "min"
    x_vals <- if (time_unit == "min") imported_data$power$Time_s / 60 else imported_data$power$Time_s
    r <- round(expand_limits(x_vals, 0.02), 2)
    updateNumericInput(session, "top_xmin", value = r[1])
    updateNumericInput(session, "top_xmax", value = r[2])
  })
  
  observeEvent(input$top_auto_yrange, {
    if (is.null(imported_data$power) || !("Power_corrected_ucal_s" %in% colnames(imported_data$power))) {
      showNotification(tr("no_data", lang()), type = "message", duration = 2)
      return(invisible())
    }
    y_raw <- imported_data$power$Power_corrected_ucal_s
    energy_unit <- input$energy_unit %||% PLOT_DEFAULTS$energy_unit
    if (identical(energy_unit, "J")) y_raw <- y_raw * 4.184
    r <- round(expand_limits(y_raw, 0.05), 2)
    updateNumericInput(session, "top_ymin", value = r[1])
    updateNumericInput(session, "top_ymax", value = r[2])
  })
  
  observeEvent(input$bot_auto_xrange, {
    int_data <- imported_data$integration
    sim_data <- imported_data$simulation
    has_int <- !is.null(int_data) && nrow(int_data) > 0 && "Ratio_App" %in% colnames(int_data)
    has_sim <- !is.null(sim_data) && nrow(sim_data) > 0 && "Ratio_App" %in% colnames(sim_data)
    if (!has_int && !has_sim) {
      showNotification(tr("no_data", lang()), type = "message", duration = 2)
      return(invisible())
    }
    x_vals <- c(
      if (has_int) int_data$Ratio_App else NULL,
      if (has_sim) sim_data$Ratio_App else NULL
    )
    x_vals <- x_vals[is.finite(x_vals)]
    if (length(x_vals) == 0) return(invisible())
    r <- round(expand_limits(x_vals, 0.05), 2)
    updateNumericInput(session, "bot_xmin", value = r[1])
    updateNumericInput(session, "bot_xmax", value = r[2])
  })
  
  observeEvent(input$bot_auto_yrange, {
    int_data <- imported_data$integration
    sim_data <- imported_data$simulation
    has_int <- !is.null(int_data) && nrow(int_data) > 0
    has_sim <- !is.null(sim_data) && nrow(sim_data) > 0
    if (!has_int && !has_sim) {
      showNotification(tr("no_data", lang()), type = "message", duration = 2)
      return(invisible())
    }
    offset_for_range <- as.numeric(input$graph_heat_offset %||% PLOT_DEFAULTS$heat_offset)
    energy_unit <- input$energy_unit %||% PLOT_DEFAULTS$energy_unit
    to_kcal <- function(v) (v - offset_for_range) / 1000
    to_display <- if (identical(energy_unit, "J")) function(v) to_kcal(v) * 4.184 else to_kcal
    y_vals <- c(
      if (has_int) to_display(int_data$heat_cal_mol) else NULL,
      if (has_sim) to_display(sim_data$dQ_App) else NULL
    )
    y_vals <- y_vals[is.finite(y_vals)]
    if (length(y_vals) == 0) return(invisible())
    r <- round(expand_limits(y_vals, 0.05), 2)
    updateNumericInput(session, "bot_ymin", value = r[1])
    updateNumericInput(session, "bot_ymax", value = r[2])
  })
  
  # ============================================================
  # 5. 能量单位切换时更新轴标签，并自动执行两个 panel 的 Y 轴 auto range
  # ============================================================
  observeEvent(input$energy_unit, {
    u <- input$energy_unit %||% PLOT_DEFAULTS$energy_unit
    updateTextInput(session, "top_ylab", value = unit_label("top_ylab", u))
    updateTextInput(session, "bot_ylab", value = unit_label("bot_ylab", u))
    # 自动执行上、下 panel 的 Y 轴 auto range（数值随单位变化需重算）
    if (!is.null(imported_data$power) && "Power_corrected_ucal_s" %in% colnames(imported_data$power)) {
      y_raw <- imported_data$power$Power_corrected_ucal_s
      if (identical(u, "J")) y_raw <- y_raw * 4.184
      r <- round(expand_limits(y_raw, 0.05), 2)
      updateNumericInput(session, "top_ymin", value = r[1])
      updateNumericInput(session, "top_ymax", value = r[2])
    }
    int_data <- imported_data$integration
    sim_data <- imported_data$simulation
    has_int <- !is.null(int_data) && nrow(int_data) > 0
    has_sim <- !is.null(sim_data) && nrow(sim_data) > 0
    if (has_int || has_sim) {
      offset_for_range <- as.numeric(input$graph_heat_offset %||% PLOT_DEFAULTS$heat_offset)
      to_kcal <- function(v) (v - offset_for_range) / 1000
      to_display <- if (identical(u, "J")) function(v) to_kcal(v) * 4.184 else to_kcal
      y_vals <- c(
        if (has_int) to_display(int_data$heat_cal_mol) else NULL,
        if (has_sim) to_display(sim_data$dQ_App) else NULL
      )
      y_vals <- y_vals[is.finite(y_vals)]
      if (length(y_vals) > 0) {
        r <- round(expand_limits(y_vals, 0.05), 2)
        updateNumericInput(session, "bot_ymin", value = r[1])
        updateNumericInput(session, "bot_ymax", value = r[2])
      }
    }
  }, ignoreInit = TRUE)

  # ============================================================
  # 5b. 时间单位切换时自动执行 top panel 的 X 轴 auto range
  # ============================================================
  observeEvent(input$top_time_unit, {
    if (is.null(imported_data$power) || !("Time_s" %in% colnames(imported_data$power))) return(invisible())
    time_unit <- input$top_time_unit %||% "min"
    x_vals <- if (time_unit == "min") imported_data$power$Time_s / 60 else imported_data$power$Time_s
    r <- round(expand_limits(x_vals, 0.02), 2)
    updateNumericInput(session, "top_xmin", value = r[1])
    updateNumericInput(session, "top_xmax", value = r[2])
  }, ignoreInit = TRUE)
  
  # ============================================================
  # 6. 收集绘图参数
  # ============================================================
  plot_params <- reactive({
    energy_unit <- input$energy_unit %||% PLOT_DEFAULTS$energy_unit
    # 始终使用用户输入的轴标签（不再覆盖），默认值由 unit_label 在切换时更新
    top_ylab <- input$top_ylab %||% unit_label("top_ylab", energy_unit)
    bot_ylab <- input$bot_ylab %||% unit_label("bot_ylab", energy_unit)
    params <- list(
      # 上 Panel
      top_xlab       = input$top_xlab %||% PLOT_DEFAULTS$top_xlab,
      top_ylab       = top_ylab,
      top_line_color = input$top_line_color %||% PLOT_DEFAULTS$top_line_color,
      top_line_width = input$top_line_width %||% PLOT_DEFAULTS$top_line_width,
      top_time_unit  = input$top_time_unit %||% PLOT_DEFAULTS$top_time_unit,
      
      # 下 Panel
      bot_xlab        = input$bot_xlab %||% PLOT_DEFAULTS$bot_xlab,
      bot_ylab        = bot_ylab,
      heat_offset     = as.numeric(input$graph_heat_offset %||% PLOT_DEFAULTS$heat_offset),
      energy_unit     = energy_unit,
      bot_point_color = input$bot_point_color %||% PLOT_DEFAULTS$bot_point_color,
      bot_point_fill  = input$bot_point_fill %||% PLOT_DEFAULTS$bot_point_fill,
      bot_point_fill_alpha = input$bot_point_fill_alpha %||% PLOT_DEFAULTS$bot_point_fill_alpha,
      bot_point_size  = input$bot_point_size %||% PLOT_DEFAULTS$bot_point_size,
      bot_point_shape = as.integer(input$bot_point_shape %||% PLOT_DEFAULTS$bot_point_shape),
      bot_layer_order = input$bot_layer_order %||% PLOT_DEFAULTS$bot_layer_order,
      bot_dim_first_point = isTRUE(input$bot_dim_first_point %||% PLOT_DEFAULTS$bot_dim_first_point),
      bot_line_color   = input$bot_line_color %||% PLOT_DEFAULTS$bot_line_color,
      bot_line_width   = input$bot_line_width %||% PLOT_DEFAULTS$bot_line_width,
      bot_line_linetype = input$bot_line_linetype %||% PLOT_DEFAULTS$bot_line_linetype,
      
      # 全局
      base_size        = input$base_size %||% PLOT_DEFAULTS$base_size,
      height_ratio     = (input$height_ratio_top %||% 1) / (input$height_ratio_bot %||% 1),
      border_linewidth = input$border_linewidth %||% PLOT_DEFAULTS$border_linewidth,
      
      export_width  = input$export_width %||% PLOT_DEFAULTS$export_width,
      export_height = input$export_height %||% PLOT_DEFAULTS$export_height,
      export_dpi    = input$export_dpi %||% PLOT_DEFAULTS$export_dpi
    )
    
    # 轴范围（始终使用当前 Min/Max；导入数据时会设为与 expand 一致的初始值）
    params$top_xmin <- input$top_xmin
    params$top_xmax <- input$top_xmax
    params$top_ymin <- input$top_ymin
    params$top_ymax <- input$top_ymax
    params$bot_xmin <- input$bot_xmin
    params$bot_xmax <- input$bot_xmax
    params$bot_ymin <- input$bot_ymin
    params$bot_ymax <- input$bot_ymax
    
    params
  })
  
  # ============================================================
  # 7. 生成图形（reactive）
  # ============================================================
  current_figure <- reactive({
    # 至少需要有某种数据
    has_data <- !is.null(imported_data$power) ||
                !is.null(imported_data$integration) ||
                !is.null(imported_data$simulation)
    if (!has_data) return(NULL)
    
    create_itc_figure(
      power_data       = imported_data$power,
      integration_data = imported_data$integration,
      simulation_data  = imported_data$simulation,
      params           = plot_params()
    )
  })
  
  # ============================================================
  # 8. 预览渲染（按导出宽高比显示）
  # ============================================================
  preview_height <- reactive({
    w <- session$clientData$output_itc_preview_width
    if (is.null(w) || !is.numeric(w) || w <= 0) w <- 400
    h_export <- as.numeric(input$export_height %||% PLOT_DEFAULTS$export_height)
    w_export <- as.numeric(input$export_width %||% PLOT_DEFAULTS$export_width)
    # 按导出宽高比计算高度，上限 1000px，配合加高后的预览面板完整显示
    max(300, min(1000, w * (h_export / w_export)))
  })

  output$itc_preview <- renderPlot({
    fig <- current_figure()
    if (is.null(fig)) {
      # 空白占位图
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = tr("no_data", lang()),
                 size = 6, color = "grey60") +
        theme_void()
    } else {
      fig
    }
  }, res = 96, height = preview_height)
  
  # ============================================================
  # 9. 导出功能
  # ============================================================
  
  # 生成文件名的辅助函数
  make_filename <- function(ext) {
    base <- if (!is.null(imported_data$filename)) {
      tools::file_path_sans_ext(imported_data$filename)
    } else {
      "ITC_Figure"
    }
    paste0(base, "_", format(Sys.time(), "%Y%m%d_%H%M"), ".", ext)
  }
  
  # 导出 PDF
  output$export_pdf <- downloadHandler(
    filename = function() make_filename("pdf"),
    content = function(file) {
      fig <- current_figure()
      if (is.null(fig)) {
        showNotification(tr("no_data", lang()), type = "warning")
        return()
      }
      ggsave(file, plot = fig, device = "pdf",
             width = input$export_width %||% PLOT_DEFAULTS$export_width,
             height = input$export_height %||% PLOT_DEFAULTS$export_height,
             units = "in", dpi = input$export_dpi %||% PLOT_DEFAULTS$export_dpi)
    }
  )
  
  # 导出 PNG
  output$export_png <- downloadHandler(
    filename = function() make_filename("png"),
    content = function(file) {
      fig <- current_figure()
      if (is.null(fig)) {
        showNotification(tr("no_data", lang()), type = "warning")
        return()
      }
      ggsave(file, plot = fig, device = "png",
             width = input$export_width %||% PLOT_DEFAULTS$export_width,
             height = input$export_height %||% PLOT_DEFAULTS$export_height,
             units = "in", dpi = input$export_dpi %||% PLOT_DEFAULTS$export_dpi,
             bg = "white")
    }
  )
  
  # 导出 TIFF
  output$export_tiff <- downloadHandler(
    filename = function() make_filename("tiff"),
    content = function(file) {
      fig <- current_figure()
      if (is.null(fig)) {
        showNotification(tr("no_data", lang()), type = "warning")
        return()
      }
      ggsave(file, plot = fig, device = "tiff",
             width = input$export_width %||% PLOT_DEFAULTS$export_width,
             height = input$export_height %||% PLOT_DEFAULTS$export_height,
             units = "in", dpi = input$export_dpi %||% PLOT_DEFAULTS$export_dpi,
             compression = "lzw", bg = "white")
    }
  )
  
  # ============================================================
  # 10. 保存/导入设置（不含 baseline heat_offset）
  # ============================================================
  
  # 收集当前设置（28 个参数），用于保存为 JSON
  get_settings_list <- function() {
    list(
      version = 1L,
      top_xlab       = as.character(input$top_xlab %||% PLOT_DEFAULTS$top_xlab),
      top_ylab       = as.character(input$top_ylab %||% unit_label("top_ylab", input$energy_unit %||% "cal")),
      top_time_unit  = as.character(input$top_time_unit %||% PLOT_DEFAULTS$top_time_unit),
      top_xmin       = as.numeric(input$top_xmin %||% 0),
      top_xmax       = as.numeric(input$top_xmax %||% 100),
      top_ymin       = as.numeric(input$top_ymin %||% -5),
      top_ymax       = as.numeric(input$top_ymax %||% 5),
      top_line_color = as.character(input$top_line_color %||% PLOT_DEFAULTS$top_line_color),
      top_line_width = as.numeric(input$top_line_width %||% PLOT_DEFAULTS$top_line_width),
      bot_xlab       = as.character(input$bot_xlab %||% PLOT_DEFAULTS$bot_xlab),
      bot_ylab       = as.character(input$bot_ylab %||% unit_label("bot_ylab", input$energy_unit %||% "cal")),
      bot_xmin       = as.numeric(input$bot_xmin %||% 0),
      bot_xmax       = as.numeric(input$bot_xmax %||% 3),
      bot_ymin       = as.numeric(input$bot_ymin %||% -20),
      bot_ymax       = as.numeric(input$bot_ymax %||% 5),
      bot_point_color = as.character(input$bot_point_color %||% PLOT_DEFAULTS$bot_point_color),
      bot_point_fill  = as.character(input$bot_point_fill %||% PLOT_DEFAULTS$bot_point_fill),
      bot_point_fill_alpha = as.numeric(input$bot_point_fill_alpha %||% PLOT_DEFAULTS$bot_point_fill_alpha),
      bot_point_size  = as.numeric(input$bot_point_size %||% PLOT_DEFAULTS$bot_point_size),
      bot_point_shape = as.integer(input$bot_point_shape %||% PLOT_DEFAULTS$bot_point_shape),
      bot_layer_order = as.character(input$bot_layer_order %||% PLOT_DEFAULTS$bot_layer_order),
      bot_dim_first_point = as.logical(input$bot_dim_first_point %||% PLOT_DEFAULTS$bot_dim_first_point),
      bot_line_color   = as.character(input$bot_line_color %||% PLOT_DEFAULTS$bot_line_color),
      bot_line_width   = as.numeric(input$bot_line_width %||% PLOT_DEFAULTS$bot_line_width),
      bot_line_linetype = as.character(input$bot_line_linetype %||% PLOT_DEFAULTS$bot_line_linetype),
      energy_unit     = as.character(input$energy_unit %||% PLOT_DEFAULTS$energy_unit),
      base_size       = as.numeric(input$base_size %||% PLOT_DEFAULTS$base_size),
      height_ratio_top  = as.numeric(input$height_ratio_top %||% 1),
      height_ratio_bot  = as.numeric(input$height_ratio_bot %||% 1),
      border_linewidth = as.numeric(input$border_linewidth %||% PLOT_DEFAULTS$border_linewidth),
      export_width    = as.numeric(input$export_width %||% PLOT_DEFAULTS$export_width),
      export_height   = as.numeric(input$export_height %||% PLOT_DEFAULTS$export_height),
      export_dpi      = as.numeric(input$export_dpi %||% PLOT_DEFAULTS$export_dpi)
    )
  }
  
  output$save_settings <- downloadHandler(
    filename = function() {
      base <- if (!is.null(imported_data$filename)) {
        tools::file_path_sans_ext(imported_data$filename)
      } else {
        paste0("ITCgraph_settings_", format(Sys.time(), "%Y%m%d_%H%M"))
      }
      paste0(base, ".json")
    },
    content = function(file) {
      settings <- get_settings_list()
      write(jsonlite::toJSON(settings, pretty = TRUE, auto_unbox = TRUE), file)
      showNotification(tr("settings_saved", lang()), type = "message", duration = 2)
    }
  )
  
  observeEvent(input$import_settings_file, {
    req(input$import_settings_file)
    path <- input$import_settings_file$datapath
    tryCatch({
      raw <- readLines(path, warn = FALSE)
      s <- jsonlite::fromJSON(paste(raw, collapse = "\n"))
      if (!is.list(s)) stop("Invalid JSON structure")
      # 文本
      if (!is.null(s$top_xlab)) updateTextInput(session, "top_xlab", value = as.character(s$top_xlab))
      if (!is.null(s$top_ylab)) updateTextInput(session, "top_ylab", value = as.character(s$top_ylab))
      if (!is.null(s$bot_xlab)) updateTextInput(session, "bot_xlab", value = as.character(s$bot_xlab))
      if (!is.null(s$bot_ylab)) updateTextInput(session, "bot_ylab", value = as.character(s$bot_ylab))
      # 选择
      if (!is.null(s$top_time_unit)) updateSelectInput(session, "top_time_unit", selected = as.character(s$top_time_unit))
      if (!is.null(s$bot_point_shape)) updateSelectInput(session, "bot_point_shape", selected = as.character(as.integer(s$bot_point_shape)))
      if (!is.null(s$bot_line_linetype)) updateSelectInput(session, "bot_line_linetype", selected = as.character(s$bot_line_linetype))
      if (!is.null(s$bot_layer_order)) updateSelectInput(session, "bot_layer_order", selected = as.character(s$bot_layer_order))
      if (!is.null(s$bot_dim_first_point)) updateCheckboxInput(session, "bot_dim_first_point", value = as.logical(s$bot_dim_first_point))
      if (!is.null(s$energy_unit)) updateSelectInput(session, "energy_unit", selected = as.character(s$energy_unit))
      # 数值
      if (!is.null(s$top_xmin)) updateNumericInput(session, "top_xmin", value = as.numeric(s$top_xmin))
      if (!is.null(s$top_xmax)) updateNumericInput(session, "top_xmax", value = as.numeric(s$top_xmax))
      if (!is.null(s$top_ymin)) updateNumericInput(session, "top_ymin", value = as.numeric(s$top_ymin))
      if (!is.null(s$top_ymax)) updateNumericInput(session, "top_ymax", value = as.numeric(s$top_ymax))
      if (!is.null(s$top_line_width)) updateNumericInput(session, "top_line_width", value = as.numeric(s$top_line_width))
      if (!is.null(s$bot_xmin)) updateNumericInput(session, "bot_xmin", value = as.numeric(s$bot_xmin))
      if (!is.null(s$bot_xmax)) updateNumericInput(session, "bot_xmax", value = as.numeric(s$bot_xmax))
      if (!is.null(s$bot_ymin)) updateNumericInput(session, "bot_ymin", value = as.numeric(s$bot_ymin))
      if (!is.null(s$bot_ymax)) updateNumericInput(session, "bot_ymax", value = as.numeric(s$bot_ymax))
      if (!is.null(s$bot_point_size)) updateNumericInput(session, "bot_point_size", value = as.numeric(s$bot_point_size))
      if (!is.null(s$bot_point_fill_alpha)) updateNumericInput(session, "bot_point_fill_alpha", value = as.numeric(s$bot_point_fill_alpha))
      if (!is.null(s$bot_line_width)) updateNumericInput(session, "bot_line_width", value = as.numeric(s$bot_line_width))
      if (!is.null(s$base_size)) updateNumericInput(session, "base_size", value = as.numeric(s$base_size))
      if (!is.null(s$height_ratio_top)) updateNumericInput(session, "height_ratio_top", value = as.numeric(s$height_ratio_top))
      if (!is.null(s$height_ratio_bot)) updateNumericInput(session, "height_ratio_bot", value = as.numeric(s$height_ratio_bot))
      if (!is.null(s$border_linewidth)) updateNumericInput(session, "border_linewidth", value = as.numeric(s$border_linewidth))
      if (!is.null(s$export_width)) updateNumericInput(session, "export_width", value = as.numeric(s$export_width))
      if (!is.null(s$export_height)) updateNumericInput(session, "export_height", value = as.numeric(s$export_height))
      if (!is.null(s$export_dpi)) updateNumericInput(session, "export_dpi", value = as.numeric(s$export_dpi))
      # 颜色（colourpicker 或 text）
      if (!is.null(s$top_line_color)) {
        if (requireNamespace("colourpicker", quietly = TRUE)) {
          colourpicker::updateColourInput(session, "top_line_color", value = as.character(s$top_line_color))
        } else {
          updateTextInput(session, "top_line_color", value = as.character(s$top_line_color))
        }
      }
      if (!is.null(s$bot_point_color)) {
        if (requireNamespace("colourpicker", quietly = TRUE)) {
          colourpicker::updateColourInput(session, "bot_point_color", value = as.character(s$bot_point_color))
        } else {
          updateTextInput(session, "bot_point_color", value = as.character(s$bot_point_color))
        }
      }
      if (!is.null(s$bot_point_fill)) {
        if (requireNamespace("colourpicker", quietly = TRUE)) {
          colourpicker::updateColourInput(session, "bot_point_fill", value = as.character(s$bot_point_fill))
        } else {
          updateTextInput(session, "bot_point_fill", value = as.character(s$bot_point_fill))
        }
      }
      if (!is.null(s$bot_line_color)) {
        if (requireNamespace("colourpicker", quietly = TRUE)) {
          colourpicker::updateColourInput(session, "bot_line_color", value = as.character(s$bot_line_color))
        } else {
          updateTextInput(session, "bot_line_color", value = as.character(s$bot_line_color))
        }
      }
      showNotification(tr("settings_imported", lang()), type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste0(tr("settings_import_error", lang()), e$message), type = "error", duration = 8)
    })
  })
}
