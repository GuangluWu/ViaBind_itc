# ==============================================================================
# server.R - ITCgraph 服务端逻辑
# ==============================================================================

server <- function(input, output, session) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  # ============================================================
  # 1. Language mode (prefer host shared i18n channel)
  # ============================================================
  session_i18n <- tryCatch({
    x <- session$userData$itcsuite_i18n
    if (is.null(x) || !is.list(x)) NULL else x
  }, error = function(e) NULL)

  local_lang <- reactiveVal("en")
  local_lang_token <- reactiveVal(0)

  lang <- reactive({
    if (!is.null(session_i18n) && is.function(session_i18n$lang_token)) {
      session_i18n$lang_token()
    } else {
      local_lang_token()
    }
    if (!is.null(session_i18n) && is.function(session_i18n$get_lang)) {
      return(graph_normalize_lang(session_i18n$get_lang()))
    }
    graph_normalize_lang(local_lang())
  })

  is_step3_tab_selected <- function(tab_value) {
    tab_chr <- as.character(tab_value %||% "")[1]
    tab_chr %in% c("step3", "Step 3 Plot & Export", "步骤 3 绘图与导出", "步骤 3 绘图 & 导出")
  }

  push_static_i18n <- function(lang_val) {
    session$sendCustomMessage("itcgraph_i18n_static", list(
      import_data = graph_tr("import_data", lang_val),
      no_file_selected = graph_tr("no_file_selected", lang_val),
      load_sets = graph_tr("load_sets", lang_val),
      json_placeholder = graph_tr("json_placeholder", lang_val),
      export_pdf = graph_tr("export_pdf", lang_val),
      export_png = graph_tr("export_png", lang_val),
      export_tiff = graph_tr("export_tiff", lang_val)
    ))
  }
  
  # ============================================================
  # 2. 数据存储
  # ============================================================
  imported_data <- reactiveValues(
    power_original = NULL, # power_original sheet
    power       = NULL,   # power_corrected sheet
    integration = NULL,   # integration sheet
    simulation  = NULL,   # simulation sheet
    fit_params  = NULL,   # fit_params sheet
    ratio_fh    = 1,      # fH from fit_params
    ratio_fg    = 1,      # fG from fit_params
    meta        = NULL,   # meta sheet
    sheets      = NULL,   # all imported sheets
    source      = NULL,   # data source id
    filename    = NULL    # 原始文件名
  )

  reset_imported_data <- function() {
    imported_data$power_original <- NULL
    imported_data$power <- NULL
    imported_data$integration <- NULL
    imported_data$simulation <- NULL
    imported_data$fit_params <- NULL
    imported_data$ratio_fh <- 1
    imported_data$ratio_fg <- 1
    imported_data$meta <- NULL
    imported_data$sheets <- NULL
    imported_data$source <- NULL
    imported_data$filename <- NULL
    invisible(TRUE)
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

  bridge_pending_autorange <- reactiveVal(FALSE)
  bridge_pending_offset <- reactiveVal(NA_real_)
  ratio_correction_enabled <- reactiveVal(TRUE)

  bridge_last_token <- reactiveVal(NA_real_)
  bridge_last_source <- reactiveVal("")
  reset_settings_nonce <- reactiveVal(0L)

  safe_num_scalar <- function(x, default = NA_real_) {
    bridge_plot_safe_num_scalar(x, default = default)
  }

  rv_get <- function(rv_fun, default = NULL) {
    tryCatch(shiny::isolate(rv_fun()), error = function(e) default)
  }

  get_ratio_correction_enabled <- function(default = TRUE) {
    val <- suppressWarnings(as.logical(rv_get(ratio_correction_enabled, default = default))[1])
    if (isTRUE(is.na(val))) return(default)
    isTRUE(val)
  }

  get_bridge_pending_autorange <- function(default = FALSE) {
    isTRUE(rv_get(bridge_pending_autorange, default = default))
  }

  get_bridge_pending_offset <- function(default = NA_real_) {
    safe_num_scalar(rv_get(bridge_pending_offset, default = default), default = default)
  }

  normalize_factor <- function(v, default = 1) {
    bridge_plot_normalize_factor(v, default = default)
  }

  fit_param_map <- function(df) {
    bridge_plot_fit_param_map(df)
  }

  get_fit_param_num <- function(fp_map, key, default = NA_real_) {
    bridge_plot_get_fit_param_num(fp_map, key, default = default)
  }

  sync_ratio_factor_display <- function(fh, fg) {
    fh_txt <- format(signif(normalize_factor(fh, 1), 6), scientific = FALSE, trim = TRUE)
    fg_txt <- format(signif(normalize_factor(fg, 1), 6), scientific = FALSE, trim = TRUE)
    updateTextInput(session, "graph_fh_display", value = fh_txt)
    updateTextInput(session, "graph_fg_display", value = fg_txt)
  }

  update_color_input <- function(id, value) {
    if (requireNamespace("colourpicker", quietly = TRUE)) {
      tryCatch(
        colourpicker::updateColourInput(session, id, value = value),
        error = function(e) updateTextInput(session, id, value = value)
      )
    } else {
      updateTextInput(session, id, value = value)
    }
    invisible(TRUE)
  }

  reset_plot_controls_to_defaults <- function() {
    lang_val <- tryCatch(lang(), error = function(e) "en")
    updateTextInput(session, "top_xlab", value = graph_tr("time_min_label", lang_val))
    updateTextInput(session, "top_ylab", value = unit_label("top_ylab", PLOT_DEFAULTS$energy_unit))
    updateSelectInput(session, "top_time_unit", selected = PLOT_DEFAULTS$top_time_unit)
    update_color_input("top_line_color", PLOT_DEFAULTS$top_line_color)
    updateNumericInput(session, "top_line_width", value = PLOT_DEFAULTS$top_line_width)

    updateTextInput(session, "bot_xlab", value = PLOT_DEFAULTS$bot_xlab)
    updateTextInput(session, "bot_ylab", value = unit_label("bot_ylab", PLOT_DEFAULTS$energy_unit))
    update_color_input("bot_point_color", PLOT_DEFAULTS$bot_point_color)
    update_color_input("bot_point_fill", PLOT_DEFAULTS$bot_point_fill)
    updateNumericInput(session, "bot_point_size", value = PLOT_DEFAULTS$bot_point_size)
    updateNumericInput(session, "bot_point_fill_alpha", value = PLOT_DEFAULTS$bot_point_fill_alpha)
    updateSelectInput(session, "bot_point_shape", selected = as.character(PLOT_DEFAULTS$bot_point_shape))
    updateSelectInput(session, "bot_layer_order", selected = PLOT_DEFAULTS$bot_layer_order)
    n_inj <- get_valid_injection_count(apply_ratio = get_ratio_correction_enabled())
    no_dim_range <- resolve_no_dim_range(NULL, n_inj)
    updateSliderInput(session, "bot_no_dim_range", min = 1, max = n_inj, value = no_dim_range)
    update_color_input("bot_line_color", PLOT_DEFAULTS$bot_line_color)
    updateNumericInput(session, "bot_line_width", value = PLOT_DEFAULTS$bot_line_width)
    updateSelectInput(session, "bot_line_linetype", selected = PLOT_DEFAULTS$bot_line_linetype)

    updateNumericInput(session, "graph_heat_offset", value = PLOT_DEFAULTS$heat_offset)
    updateSelectInput(session, "energy_unit", selected = PLOT_DEFAULTS$energy_unit)
    updateCheckboxInput(session, "graph_apply_ratio_correction", value = TRUE)
    ratio_correction_enabled(TRUE)
    sync_ratio_factor_display(1, 1)

    updateNumericInput(session, "base_size", value = PLOT_DEFAULTS$base_size)
    updateNumericInput(session, "height_ratio_top", value = 1)
    updateNumericInput(session, "height_ratio_bot", value = 1)
    updateNumericInput(session, "border_linewidth", value = PLOT_DEFAULTS$border_linewidth)
    updateNumericInput(session, "export_width", value = PLOT_DEFAULTS$export_width)
    updateNumericInput(session, "export_height", value = PLOT_DEFAULTS$export_height)
    updateNumericInput(session, "export_dpi", value = PLOT_DEFAULTS$export_dpi)
    invisible(TRUE)
  }

  get_ratio_multiplier <- function(apply_ratio = get_ratio_correction_enabled()) {
    bridge_plot_ratio_multiplier(
      ratio_fh = imported_data$ratio_fh,
      ratio_fg = imported_data$ratio_fg,
      apply_ratio = apply_ratio,
      default_factor = 1
    )
  }

  get_bottom_plot_data <- function(apply_ratio = get_ratio_correction_enabled(), offset_override = NA_real_) {
    int_data <- imported_data$integration
    sim_data <- imported_data$simulation
    mult <- get_ratio_multiplier(apply_ratio = apply_ratio)
    offset <- get_effective_offset(offset_override = offset_override)

    if (!is.null(int_data) && is.data.frame(int_data)) {
      int_data <- as.data.frame(int_data)
      if ("Ratio_App" %in% names(int_data)) {
        int_data$Ratio_App <- int_data$Ratio_App * mult
      }
      if ("heat_cal_mol" %in% names(int_data)) {
        int_data$heat_cal_mol <- bridge_plot_apply_heat_correction(
          y_raw = int_data$heat_cal_mol,
          ratio_fg = imported_data$ratio_fg,
          heat_offset = offset,
          apply_ratio = apply_ratio,
          default_factor = 1
        )
      }
    }
    if (!is.null(sim_data) && is.data.frame(sim_data)) {
      sim_data <- as.data.frame(sim_data)
      if ("Ratio_App" %in% names(sim_data)) {
        sim_data$Ratio_App <- sim_data$Ratio_App * mult
      }
      if ("dQ_App" %in% names(sim_data)) {
        sim_data$dQ_App <- bridge_plot_apply_heat_correction(
          y_raw = sim_data$dQ_App,
          ratio_fg = imported_data$ratio_fg,
          heat_offset = offset,
          apply_ratio = apply_ratio,
          default_factor = 1
        )
      }
    }

    list(integration = int_data, simulation = sim_data)
  }

  get_valid_injection_count <- function(apply_ratio = get_ratio_correction_enabled()) {
    bottom_data <- get_bottom_plot_data(apply_ratio = apply_ratio)
    int_data <- bottom_data$integration
    if (is.null(int_data) || !is.data.frame(int_data)) return(1L)
    if (!all(c("Ratio_App", "heat_cal_mol") %in% names(int_data))) return(1L)

    valid_rows <- is.finite(int_data$Ratio_App) & is.finite(int_data$heat_cal_mol)
    n_inj <- sum(valid_rows, na.rm = TRUE)
    if (!is.finite(n_inj) || n_inj < 1) return(1L)
    as.integer(n_inj)
  }

  resolve_no_dim_range <- function(value, n_inj) {
    n_inj <- suppressWarnings(as.integer(n_inj)[1])
    if (!is.finite(n_inj) || n_inj < 1) n_inj <- 1L

    default_start <- suppressWarnings(as.integer(PLOT_DEFAULTS$bot_no_dim_start)[1])
    if (!is.finite(default_start)) default_start <- 1L
    default_end <- suppressWarnings(as.integer(PLOT_DEFAULTS$bot_no_dim_end)[1])
    if (!is.finite(default_end)) default_end <- n_inj

    if (!is.null(value) && length(value) >= 2) {
      start <- safe_num_scalar(value[1], default = default_start)
      end <- safe_num_scalar(value[2], default = default_end)
    } else {
      start <- default_start
      end <- default_end
    }

    start <- max(1L, min(n_inj, as.integer(round(start))))
    end <- max(1L, min(n_inj, as.integer(round(end))))
    if (start > end) {
      tmp <- start
      start <- end
      end <- tmp
    }
    c(start, end)
  }

  reset_no_dim_range_to_default <- function(apply_ratio = get_ratio_correction_enabled()) {
    n_inj <- get_valid_injection_count(apply_ratio = apply_ratio)
    no_dim_range <- resolve_no_dim_range(NULL, n_inj)
    updateSliderInput(session, "bot_no_dim_range", min = 1, max = n_inj, value = no_dim_range)
    invisible(no_dim_range)
  }

  resolve_step2_payload_source <- function(payload, token = NA_real_) {
    bridge_plot_resolve_step2_payload_source(payload, token = token)
  }

  apply_step2_payload_plot_settings <- function() {
    sync_cfg <- bridge_plot_sync_from_fit_params(
      fit_params_df = imported_data$fit_params,
      default_heat_offset = PLOT_DEFAULTS$heat_offset
    )
    offset_from_fit <- sync_cfg$heat_offset
    updateNumericInput(session, "graph_heat_offset", value = offset_from_fit)
    imported_data$ratio_fh <- sync_cfg$ratio_fh
    imported_data$ratio_fg <- sync_cfg$ratio_fg

    sync_ratio_factor_display(imported_data$ratio_fh, imported_data$ratio_fg)
    reset_no_dim_range_to_default(apply_ratio = get_ratio_correction_enabled())

    # Run auto-range immediately and queue one replay for first-time tab mount.
    schedule_bridge_autorange(offset_override = offset_from_fit)
    invisible(TRUE)
  }

  consume_step2_plot_payload <- function(payload, replay_only = FALSE) {
    if (is.null(payload) || !is.list(payload)) return(FALSE)
    token <- suppressWarnings(as.numeric(payload$token)[1])
    payload_source <- resolve_step2_payload_source(payload, token)
    if (is.finite(token) &&
        !is.na(bridge_last_token()) &&
        identical(token, bridge_last_token()) &&
        identical(payload_source, bridge_last_source())) {
      if (isTRUE(replay_only)) {
        apply_step2_payload_plot_settings()
        return(TRUE)
      }
      return(FALSE)
    }

    if (!isTRUE(replay_only)) {
      reset_imported_data()
      reset_plot_controls_to_defaults()
    }

    if (!isTRUE(replay_only)) {
      parsed <- bridge_plot_extract_step2_payload_frames(payload)

      # Replace the whole Step 3 data snapshot to avoid stale data bleed-through.
      imported_data$power_original <- parsed$power_original
      imported_data$power <- parsed$power
      imported_data$integration <- parsed$integration
      imported_data$simulation <- parsed$simulation
      imported_data$fit_params <- parsed$fit_params
      imported_data$meta <- parsed$meta
      imported_data$sheets <- parsed$sheets

      imported_data$source <- "step2_bridge"
      imported_data$filename <- payload_source
    }

    apply_step2_payload_plot_settings()

    if (is.finite(token)) bridge_last_token(token)
    bridge_last_source(payload_source)
    TRUE
  }

  latest_step2_plot_payload <- reactiveVal(NULL)
  bridge_step2_channel <- resolve_bridge_channel("step2_plot_payload")
  if (is.function(bridge_step2_channel)) {
    observeEvent(bridge_step2_channel(), {
      payload <- bridge_step2_channel()
      if (is.null(payload) || !is.list(payload)) return(invisible(FALSE))
      latest_step2_plot_payload(payload)
      consume_step2_plot_payload(payload)
    }, ignoreNULL = TRUE)
  }

  observeEvent(input$main_tabs, {
    if (!is_step3_tab_selected(input$main_tabs)) return(invisible(FALSE))
    payload <- latest_step2_plot_payload()
    if (is.null(payload) || !is.list(payload)) return(invisible(FALSE))
    consume_step2_plot_payload(payload, replay_only = TRUE)
  }, ignoreInit = TRUE)
  
  expand_limits <- function(v, mult = 0.05) {
    r <- range(v, na.rm = TRUE)
    d <- diff(r)
    if (d < 1e-10) d <- 1
    c(r[1] - mult * d, r[2] + mult * d)
  }

  get_effective_offset <- function(offset_override = NA_real_) {
    off <- suppressWarnings(as.numeric(offset_override)[1])
    if (!is.finite(off)) {
      off <- suppressWarnings(as.numeric(input$graph_heat_offset %||% PLOT_DEFAULTS$heat_offset)[1])
    }
    if (!is.finite(off)) {
      fp <- fit_param_map(imported_data$fit_params)
      off2 <- get_fit_param_num(fp, "Offset_cal", default = NA_real_)
      if (is.finite(off2)) off <- off2
    }
    if (!is.finite(off)) off <- PLOT_DEFAULTS$heat_offset
    off
  }

  apply_auto_ranges <- function(offset_override = NA_real_, apply_ratio = get_ratio_correction_enabled()) {
    if (!is.null(imported_data$power) && nrow(imported_data$power) > 0 &&
        "Time_s" %in% colnames(imported_data$power)) {
      time_unit <- input$top_time_unit %||% "min"
      x_vals <- if (time_unit == "min") imported_data$power$Time_s / 60 else imported_data$power$Time_s
      x_vals <- x_vals[is.finite(x_vals)]
      if (length(x_vals) > 0) {
        r_top_x <- round(expand_limits(x_vals, 0.02), 2)
        updateNumericInput(session, "top_xmin", value = r_top_x[1])
        updateNumericInput(session, "top_xmax", value = r_top_x[2])
      }

      if ("Power_corrected_ucal_s" %in% colnames(imported_data$power)) {
        y_raw <- imported_data$power$Power_corrected_ucal_s
        energy_unit <- input$energy_unit %||% PLOT_DEFAULTS$energy_unit
        if (identical(energy_unit, "J")) y_raw <- y_raw * 4.184
        y_raw <- y_raw[is.finite(y_raw)]
        if (length(y_raw) > 0) {
          r_top_y <- round(expand_limits(y_raw, 0.05), 2)
          updateNumericInput(session, "top_ymin", value = r_top_y[1])
          updateNumericInput(session, "top_ymax", value = r_top_y[2])
        }
      }
    }

    offset_for_range <- get_effective_offset(offset_override)
    bottom_data <- get_bottom_plot_data(apply_ratio = apply_ratio, offset_override = offset_for_range)
    int_data <- bottom_data$integration
    sim_data <- bottom_data$simulation
    has_int <- !is.null(int_data) && nrow(int_data) > 0
    has_sim <- !is.null(sim_data) && nrow(sim_data) > 0
    if (!(has_int || has_sim)) return(invisible(FALSE))

    x_vals <- c(
      if (has_int && "Ratio_App" %in% names(int_data)) int_data$Ratio_App else NULL,
      if (has_sim && "Ratio_App" %in% names(sim_data)) sim_data$Ratio_App else NULL
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
      if (has_int && "heat_cal_mol" %in% names(int_data)) to_display(int_data$heat_cal_mol) else NULL,
      if (has_sim && "dQ_App" %in% names(sim_data)) to_display(sim_data$dQ_App) else NULL
    )
    y_vals <- y_vals[is.finite(y_vals)]
    if (length(y_vals) > 0) {
      r_bot_y <- round(expand_limits(y_vals, 0.05), 2)
      updateNumericInput(session, "bot_ymin", value = r_bot_y[1])
      updateNumericInput(session, "bot_ymax", value = r_bot_y[2])
    }
    invisible(TRUE)
  }

  schedule_bridge_autorange <- function(offset_override = NA_real_) {
    off <- suppressWarnings(as.numeric(offset_override)[1])
    bridge_pending_offset(off)
    bridge_pending_autorange(TRUE)

    # 1) Immediate apply for already-mounted controls.
    tryCatch(
      apply_auto_ranges(offset_override = off, apply_ratio = get_ratio_correction_enabled()),
      error = function(e) NULL
    )

    # 2) Deferred apply after flush, for first navigation where controls may
    # not be fully bound when the bridge payload arrives.
    session$onFlushed(function() {
      off2 <- get_bridge_pending_offset()
      tryCatch(
        apply_auto_ranges(offset_override = off2, apply_ratio = get_ratio_correction_enabled()),
        error = function(e) NULL
      )
    }, once = TRUE)
  }

  observeEvent(input$main_tabs, {
    if (!isTRUE(get_bridge_pending_autorange())) return(invisible())
    if (!is_step3_tab_selected(input$main_tabs)) return(invisible())
    off <- get_bridge_pending_offset()
    tryCatch(
      apply_auto_ranges(offset_override = off, apply_ratio = get_ratio_correction_enabled()),
      error = function(e) NULL
    )
    bridge_pending_autorange(FALSE)
  }, ignoreInit = TRUE)
  
  # ============================================================
  # 3. Dynamic UI labels (i18n)
  # ============================================================
  
  output$preview_header <- renderUI({
    h4(graph_tr("preview_header", lang()))
  })
  
  output$section_import_ui <- renderUI({
    h4(graph_tr("section_import", lang()))
  })
  
  output$data_summary_ui <- renderUI({
    if (is.null(imported_data$filename)) {
      return(div(class = "data-summary", em(graph_tr("no_data_loaded", lang()))))
    }
    n_power <- if (!is.null(imported_data$power)) nrow(imported_data$power) else 0
    n_int   <- if (!is.null(imported_data$integration)) nrow(imported_data$integration) else 0
    n_sim   <- if (!is.null(imported_data$simulation)) nrow(imported_data$simulation) else 0
    
    div(class = "data-summary",
      tags$b(paste0(graph_tr("file_label", lang()), ": ")), imported_data$filename, br(),
      paste0(graph_tr("power_points", lang()), ": "), tags$span(class = "badge", n_power), br(),
      paste0(graph_tr("integration_points", lang()), ": "), tags$span(class = "badge", n_int), br(),
      paste0(graph_tr("simulation_points", lang()), ": "), tags$span(class = "badge", n_sim)
    )
  })
  
  output$heat_offset_label_ui <- renderUI({
    fluidRow(
      column(6, tags$label(graph_tr("baseline_heat_offset", lang()), style = "font-size: 12px;")),
      column(6, tags$label(graph_tr("energy_unit", lang()), style = "font-size: 12px;"))
    )
  })
  
  output$section_top_ui <- renderUI({
    tagList(
      h4(graph_tr("section_top", lang())),
      fluidRow(
        column(4, tags$label(graph_tr("x_label", lang()), style = "display: block; margin-bottom: 5px;")),
        column(5, tags$label(graph_tr("y_label", lang()), style = "display: block; margin-bottom: 5px;")),
        column(3, tags$label(graph_tr("time_unit", lang()), style = "display: block; margin-bottom: 5px;"))
      )
    )
  })
  
  # 时间单位选择器
  output$time_unit_ui <- renderUI({
    selected_unit <- isolate(input$top_time_unit) %||% PLOT_DEFAULTS$top_time_unit
    selectInput("top_time_unit", label = NULL,
      choices = c("min" = "min", "s" = "s"),
      selected = selected_unit
    )
  })
  
  # 上 Panel X 轴范围：标签 + Min + Max + Auto range 同一行
  output$top_xrange_ui <- renderUI({
    div(class = "range-one-row",
      tags$span(class = "range-label", graph_tr("x_range", lang())),
      div(class = "range-min",
        numericInput("top_xmin", graph_tr("min", lang()), value = isolate(input$top_xmin) %||% 0, step = 5)),
      div(class = "range-max",
        numericInput("top_xmax", graph_tr("max", lang()), value = isolate(input$top_xmax) %||% 100, step = 5)),
      actionButton("top_auto_xrange", graph_tr("auto_range", lang()), class = "btn-sm btn-info")
    )
  })
  
  # 上 Panel Y 轴范围：标签 + Min + Max + Auto range 同一行
  output$top_yrange_ui <- renderUI({
    div(class = "range-one-row",
      tags$span(class = "range-label", graph_tr("y_range", lang())),
      div(class = "range-min",
        numericInput("top_ymin", graph_tr("min", lang()), value = isolate(input$top_ymin) %||% -5, step = 0.5)),
      div(class = "range-max",
        numericInput("top_ymax", graph_tr("max", lang()), value = isolate(input$top_ymax) %||% 5, step = 0.5)),
      actionButton("top_auto_yrange", graph_tr("auto_range", lang()), class = "btn-sm btn-info")
    )
  })
  
  # 上 Panel 颜色
  output$top_color_ui <- renderUI({
    color_val <- isolate(input$top_line_color) %||% PLOT_DEFAULTS$top_line_color
    if (requireNamespace("colourpicker", quietly = TRUE)) {
      colourpicker::colourInput("top_line_color", graph_tr("line_color", lang()),
                                value = color_val, showColour = "both")
    } else {
      textInput("top_line_color", graph_tr("line_color", lang()), value = color_val)
    }
  })
  
  # ---- 下 Panel 设置标签 ----
  output$section_bot_ui <- renderUI({
    tagList(
      h4(graph_tr("section_bottom", lang())),
      fluidRow(
        column(6, tags$label(graph_tr("x_label", lang()))),
        column(6, tags$label(graph_tr("y_label", lang())))
      )
    )
  })
  
  # 下 Panel X 轴范围：标签 + Min + Max + Auto range 同一行
  output$bot_xrange_ui <- renderUI({
    div(class = "range-one-row",
      tags$span(class = "range-label", graph_tr("x_range", lang())),
      div(class = "range-min",
        numericInput("bot_xmin", graph_tr("min", lang()), value = isolate(input$bot_xmin) %||% 0, step = 0.1)),
      div(class = "range-max",
        numericInput("bot_xmax", graph_tr("max", lang()), value = isolate(input$bot_xmax) %||% 3, step = 0.1)),
      actionButton("bot_auto_xrange", graph_tr("auto_range", lang()), class = "btn-sm btn-info")
    )
  })
  
  # 下 Panel Y 轴范围：标签 + Min + Max + Auto range 同一行
  output$bot_yrange_ui <- renderUI({
    div(class = "range-one-row",
      tags$span(class = "range-label", graph_tr("y_range", lang())),
      div(class = "range-min",
        numericInput("bot_ymin", graph_tr("min", lang()), value = isolate(input$bot_ymin) %||% -20, step = 0.5)),
      div(class = "range-max",
        numericInput("bot_ymax", graph_tr("max", lang()), value = isolate(input$bot_ymax) %||% 5, step = 0.5)),
      actionButton("bot_auto_yrange", graph_tr("auto_range", lang()), class = "btn-sm btn-info")
    )
  })

  # Keep axis range controls alive while Step 3 tab is hidden, so bridge
  # updates from Step 2 can land on first Data -> Plot.
  outputOptions(output, "time_unit_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "top_xrange_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "top_yrange_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "bot_xrange_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "bot_yrange_ui", suspendWhenHidden = FALSE)
  
  # 下 Panel 散点颜色
  output$bot_point_color_ui <- renderUI({
    color_val <- isolate(input$bot_point_color) %||% PLOT_DEFAULTS$bot_point_color
    if (requireNamespace("colourpicker", quietly = TRUE)) {
      colourpicker::colourInput("bot_point_color", graph_tr("point_border", lang()),
                                value = color_val, showColour = "both")
    } else {
      textInput("bot_point_color", graph_tr("point_border", lang()), value = color_val)
    }
  })
  
  # 下 Panel 散点填充颜色
  output$bot_point_fill_ui <- renderUI({
    color_val <- isolate(input$bot_point_fill) %||% PLOT_DEFAULTS$bot_point_fill
    if (requireNamespace("colourpicker", quietly = TRUE)) {
      colourpicker::colourInput("bot_point_fill", graph_tr("point_fill", lang()),
                                value = color_val, showColour = "both")
    } else {
      textInput("bot_point_fill", graph_tr("point_fill", lang()), value = color_val)
    }
  })
  
  # 散点形状（selectize=FALSE 使下拉由浏览器原生渲染，浮于最上层不被遮挡）
  output$bot_shape_ui <- renderUI({
    selected_shape <- as.character(isolate(input$bot_point_shape) %||% PLOT_DEFAULTS$bot_point_shape)
    shape_choices <- setNames(
      c(21, 19, 22, 23, 24, 4),
      c(
        graph_tr("shape_open_circle", lang()),
        graph_tr("shape_filled_circle", lang()),
        graph_tr("shape_open_square", lang()),
        graph_tr("shape_open_diamond", lang()),
        graph_tr("shape_open_triangle", lang()),
        graph_tr("shape_cross", lang())
      )
    )
    selectInput("bot_point_shape", graph_tr("point_shape", lang()),
      choices = shape_choices,
      selected = selected_shape,
      selectize = FALSE
    )
  })
  
  # 拟合线颜色
  output$bot_line_color_ui <- renderUI({
    color_val <- isolate(input$bot_line_color) %||% PLOT_DEFAULTS$bot_line_color
    if (requireNamespace("colourpicker", quietly = TRUE)) {
      colourpicker::colourInput("bot_line_color", graph_tr("fit_line_color", lang()),
                                value = color_val, showColour = "both")
    } else {
      textInput("bot_line_color", graph_tr("fit_line_color", lang()), value = color_val)
    }
  })
  
  # 拟合线型
  output$bot_line_linetype_ui <- renderUI({
    selected_linetype <- as.character(isolate(input$bot_line_linetype) %||% PLOT_DEFAULTS$bot_line_linetype)
    choices <- setNames(
      c("solid", "dashed", "dotted", "dotdash", "longdash"),
      c(
        graph_tr("linetype_solid", lang()),
        graph_tr("linetype_dashed", lang()),
        graph_tr("linetype_dotted", lang()),
        graph_tr("linetype_dotdash", lang()),
        graph_tr("linetype_longdash", lang())
      )
    )
    selectInput("bot_line_linetype", graph_tr("fit_line_type", lang()),
                choices = choices, selected = selected_linetype,
                selectize = FALSE)
  })
  
  output$bot_layer_order_ui <- renderUI({
    selected_layer <- as.character(isolate(input$bot_layer_order) %||% PLOT_DEFAULTS$bot_layer_order)
    choices <- setNames(
      c("points_over_line", "line_over_points"),
      c(graph_tr("layer_points_over_line", lang()), graph_tr("layer_line_over_points", lang()))
    )
    selectInput("bot_layer_order", graph_tr("layer_order", lang()),
                choices = choices, selected = selected_layer,
                selectize = FALSE)
  })
  
  output$bot_no_dim_range_ui <- renderUI({
    n_inj <- get_valid_injection_count(apply_ratio = isTRUE(input$graph_apply_ratio_correction))
    current_range <- isolate(input$bot_no_dim_range)
    selected <- resolve_no_dim_range(current_range, n_inj)
    sliderInput(
      "bot_no_dim_range",
      graph_tr("not_dimmed_range", lang()),
      min = 1,
      max = n_inj,
      value = selected,
      step = 1,
      sep = "",
      ticks = FALSE
    )
  })
  
  # ---- Global settings labels ----
  output$section_global_ui <- renderUI({
    tagList(
      h4(graph_tr("section_global", lang())),
      fluidRow(
        column(3, tags$label(graph_tr("base_font", lang()))),
        column(3, tags$label(graph_tr("top_ratio", lang()))),
        column(3, tags$label(graph_tr("bottom_ratio", lang()))),
        column(3, tags$label(graph_tr("border_width", lang())))
      )
    )
  })
  
  # ---- Save/load settings labels ----
  output$settings_save_hint_ui <- renderUI({
    div(class = "settings-hint", graph_tr("settings_save_hint", lang()))
  })
  output$settings_import_hint_ui <- renderUI({
    div(class = "settings-hint", graph_tr("settings_import_hint", lang()))
  })
  output$save_settings_ui <- renderUI({
    downloadButton("save_settings", label = graph_tr("save_sets", lang()), icon = NULL, class = "btn-success btn-save-settings")
  })
  output$import_settings_ui <- renderUI({
    reset_settings_nonce()
    div(
      class = "settings-action-row",
      div(
        class = "settings-load-wrap",
        fileInput("import_settings_file", label = NULL,
                  accept = ".json", buttonLabel = graph_tr("load_sets", lang()), placeholder = graph_tr("json_placeholder", lang()))
      ),
      actionButton("reset_settings", label = graph_tr("reset", lang()), class = "btn-default btn-reset-settings")
    )
  })
  
  # ---- Export labels ----
  output$section_export_ui <- renderUI({
    tagList(
      h4(graph_tr("section_export", lang())),
      fluidRow(
        column(4, tags$label(graph_tr("width_in", lang()))),
        column(4, tags$label(graph_tr("height_in", lang()))),
        column(4, tags$label(graph_tr("dpi", lang())))
      )
    )
  })

  observeEvent(lang(), {
    l <- lang()
    time_unit <- input$top_time_unit %||% "min"
    time_default <- if (identical(time_unit, "s")) graph_tr("time_s_label", l) else graph_tr("time_min_label", l)
    updateTextInput(session, "top_xlab", value = time_default)

    updateNumericInput(session, "top_line_width", label = graph_tr("line_width", l))
    updateNumericInput(session, "bot_point_size", label = graph_tr("point_size", l))
    updateNumericInput(session, "bot_point_fill_alpha", label = graph_tr("point_fill_alpha", l))
    updateNumericInput(session, "bot_line_width", label = graph_tr("fit_line_width", l))
    updateTextInput(session, "graph_fh_display", label = "fH")
    updateTextInput(session, "graph_fg_display", label = "fG")
    updateCheckboxInput(session, "graph_apply_ratio_correction", label = graph_tr("apply_ratio_correction", l))
    push_static_i18n(l)
    session$onFlushed(function() {
      push_static_i18n(l)
    }, once = TRUE)
  }, ignoreInit = FALSE)
  
  # ============================================================
  # 4. 数据导入
  # ============================================================
  observeEvent(input$xlsx_file, {
    req(input$xlsx_file)
    filepath <- input$xlsx_file$datapath
    
    tryCatch({
      reset_imported_data()
      reset_plot_controls_to_defaults()
      sheets <- readxl::excel_sheets(filepath)
      imported_data$sheets <- list()
      
      # 读取 power_original（可选）
      if ("power_original" %in% sheets) {
        po <- readxl::read_excel(filepath, sheet = "power_original")
        imported_data$power_original <- as.data.frame(po)
        imported_data$sheets[["power_original"]] <- imported_data$power_original
      }

      # 读取 power_corrected
      if ("power_corrected" %in% sheets) {
        pc <- readxl::read_excel(filepath, sheet = "power_corrected")
        if ("Time_s" %in% colnames(pc) && "Power_corrected_ucal_s" %in% colnames(pc)) {
          imported_data$power <- as.data.frame(pc)
          imported_data$sheets[["power_corrected"]] <- imported_data$power
        }
      }
      
      # 读取 integration_rev / integration
      if ("integration_rev" %in% sheets) {
        int_df <- readxl::read_excel(filepath, sheet = "integration_rev")
        if ("Ratio_App" %in% colnames(int_df) && "heat_cal_mol" %in% colnames(int_df)) {
          imported_data$integration <- as.data.frame(int_df)
          imported_data$sheets[["integration_rev"]] <- imported_data$integration
        }
      } else if ("integration" %in% sheets) {
        int_df <- readxl::read_excel(filepath, sheet = "integration")
        if ("Ratio_App" %in% colnames(int_df) && "heat_cal_mol" %in% colnames(int_df)) {
          imported_data$integration <- as.data.frame(int_df)
          imported_data$sheets[["integration"]] <- imported_data$integration
        }
      }
      
      # 读取 simulation
      if ("simulation" %in% sheets) {
        sim_df <- readxl::read_excel(filepath, sheet = "simulation")
        if ("Ratio_App" %in% colnames(sim_df) && "dQ_App" %in% colnames(sim_df)) {
          imported_data$simulation <- as.data.frame(sim_df)
          imported_data$sheets[["simulation"]] <- imported_data$simulation
        }
      }
      
      # 读取 fit_params（竖列 parameter/value）；读取 Offset_cal/fH/fG
      offset_for_range <- PLOT_DEFAULTS$heat_offset
      if ("fit_params" %in% sheets) {
        imported_data$fit_params <- as.data.frame(readxl::read_excel(filepath, sheet = "fit_params"))
        imported_data$sheets[["fit_params"]] <- imported_data$fit_params
        fp_map <- fit_param_map(imported_data$fit_params)
        offset_num <- get_fit_param_num(fp_map, "Offset_cal", default = NA_real_)
        if (is.finite(offset_num)) {
          offset_for_range <- offset_num
          updateNumericInput(session, "graph_heat_offset", value = offset_num)
        }
        imported_data$ratio_fh <- normalize_factor(get_fit_param_num(fp_map, "fH", default = 1), 1)
        imported_data$ratio_fg <- normalize_factor(get_fit_param_num(fp_map, "fG", default = 1), 1)
      } else {
        imported_data$ratio_fh <- 1
        imported_data$ratio_fg <- 1
      }
      sync_ratio_factor_display(imported_data$ratio_fh, imported_data$ratio_fg)
      reset_no_dim_range_to_default(apply_ratio = isTRUE(input$graph_apply_ratio_correction))
      
      # 读取 meta_rev/meta（可选）
      if ("meta_rev" %in% sheets) {
        imported_data$meta <- as.data.frame(readxl::read_excel(filepath, sheet = "meta_rev"))
        imported_data$sheets[["meta_rev"]] <- imported_data$meta
      } else if ("meta" %in% sheets) {
        imported_data$meta <- as.data.frame(readxl::read_excel(filepath, sheet = "meta"))
        imported_data$sheets[["meta"]] <- imported_data$meta
      }
      
      imported_data$source <- "xlsx_import"
      imported_data$filename <- input$xlsx_file$name
      
      # 通知
      has_any <- !is.null(imported_data$power) || !is.null(imported_data$integration) || !is.null(imported_data$simulation)
      if (has_any) {
        showNotification(graph_tr("data_import_success", lang()), type = "message", duration = 3)
      }
      
      # 导入后统一执行自动范围（与 Step2->Step3 桥接路径一致）
      tryCatch(
        apply_auto_ranges(
          offset_override = offset_for_range,
          apply_ratio = isTRUE(input$graph_apply_ratio_correction)
        ),
        error = function(e) NULL
      )
      
    }, error = function(e) {
      showNotification(paste0(graph_tr("import_failed_prefix", lang()), e$message), type = "error", duration = 8)
    })
  })
  
  # ============================================================
  # 4b. 「自动范围」按钮：按当前数据与当前单位重新计算轴范围（尤其适合换单位后）
  # ============================================================
  observeEvent(input$top_auto_xrange, {
    if (is.null(imported_data$power) || !("Time_s" %in% colnames(imported_data$power))) {
      showNotification(graph_tr("no_data_warning", lang()), type = "message", duration = 2)
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
      showNotification(graph_tr("no_data_warning", lang()), type = "message", duration = 2)
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
    bottom_data <- get_bottom_plot_data(apply_ratio = isTRUE(input$graph_apply_ratio_correction))
    int_data <- bottom_data$integration
    sim_data <- bottom_data$simulation
    has_int <- !is.null(int_data) && nrow(int_data) > 0 && "Ratio_App" %in% colnames(int_data)
    has_sim <- !is.null(sim_data) && nrow(sim_data) > 0 && "Ratio_App" %in% colnames(sim_data)
    if (!has_int && !has_sim) {
      showNotification(graph_tr("no_data_warning", lang()), type = "message", duration = 2)
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
    offset_for_range <- get_effective_offset()
    bottom_data <- get_bottom_plot_data(
      apply_ratio = isTRUE(input$graph_apply_ratio_correction),
      offset_override = offset_for_range
    )
    int_data <- bottom_data$integration
    sim_data <- bottom_data$simulation
    has_int <- !is.null(int_data) && nrow(int_data) > 0 && "heat_cal_mol" %in% names(int_data)
    has_sim <- !is.null(sim_data) && nrow(sim_data) > 0 && "dQ_App" %in% names(sim_data)
    if (!has_int && !has_sim) {
      showNotification(graph_tr("no_data_warning", lang()), type = "message", duration = 2)
      return(invisible())
    }
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
    offset_for_range <- get_effective_offset()
    bottom_data <- get_bottom_plot_data(
      apply_ratio = isTRUE(input$graph_apply_ratio_correction),
      offset_override = offset_for_range
    )
    int_data <- bottom_data$integration
    sim_data <- bottom_data$simulation
    has_int <- !is.null(int_data) && nrow(int_data) > 0 && "heat_cal_mol" %in% names(int_data)
    has_sim <- !is.null(sim_data) && nrow(sim_data) > 0 && "dQ_App" %in% names(sim_data)
    if (has_int || has_sim) {
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

  observeEvent(input$graph_apply_ratio_correction, {
    ratio_correction_enabled(isTRUE(input$graph_apply_ratio_correction))
    tryCatch(
      apply_auto_ranges(
        offset_override = get_effective_offset(),
        apply_ratio = get_ratio_correction_enabled()
      ),
      error = function(e) NULL
    )
  }, ignoreInit = TRUE)

  # ============================================================
  # 5b. 时间单位切换时自动执行 top panel 的 X 轴 auto range
  # ============================================================
  observeEvent(input$top_time_unit, {
    time_unit <- input$top_time_unit %||% "min"
    default_xlab <- if (identical(time_unit, "s")) graph_tr("time_s_label", lang()) else graph_tr("time_min_label", lang())
    current_xlab <- isolate(input$top_xlab %||% "")
    default_labels <- unique(c(
      graph_tr("time_min_label", "en"),
      graph_tr("time_s_label", "en"),
      graph_tr("time_min_label", "zh"),
      graph_tr("time_s_label", "zh")
    ))

    # Keep custom labels untouched; only auto-sync default time labels.
    if (!nzchar(current_xlab) || current_xlab %in% default_labels) {
      updateTextInput(session, "top_xlab", value = default_xlab)
    }

    if (is.null(imported_data$power) || !("Time_s" %in% colnames(imported_data$power))) return(invisible())
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
    no_dim_range <- resolve_no_dim_range(
      input$bot_no_dim_range,
      get_valid_injection_count(apply_ratio = isTRUE(input$graph_apply_ratio_correction))
    )
    params <- list(
      # 上 Panel
      top_xlab       = input$top_xlab %||% graph_tr("time_min_label", lang()),
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
      bot_no_dim_start = as.integer(no_dim_range[1]),
      bot_no_dim_end = as.integer(no_dim_range[2]),
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
    bottom_data <- get_bottom_plot_data(apply_ratio = isTRUE(input$graph_apply_ratio_correction))
    has_data <- !is.null(imported_data$power) ||
                !is.null(bottom_data$integration) ||
                !is.null(bottom_data$simulation)
    if (!has_data) return(NULL)
    
    create_itc_figure(
      power_data       = imported_data$power,
      integration_data = bottom_data$integration,
      simulation_data  = bottom_data$simulation,
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
        annotate("text", x = 0.5, y = 0.5, label = graph_tr("no_data_loaded", lang()),
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
        showNotification(graph_tr("no_data_warning", lang()), type = "warning")
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
        showNotification(graph_tr("no_data_warning", lang()), type = "warning")
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
        showNotification(graph_tr("no_data_warning", lang()), type = "warning")
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
    no_dim_range <- resolve_no_dim_range(
      input$bot_no_dim_range,
      get_valid_injection_count(apply_ratio = isTRUE(input$graph_apply_ratio_correction))
    )
    list(
      version = 2L,
      top_xlab       = as.character(input$top_xlab %||% graph_tr("time_min_label", lang())),
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
      bot_no_dim_start = as.integer(no_dim_range[1]),
      bot_no_dim_end = as.integer(no_dim_range[2]),
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
        paste0("ViaBind_ITCgraph_settings_", format(Sys.time(), "%Y%m%d_%H%M"))
      }
      paste0(base, "_plot_settings.json")
    },
    content = function(file) {
      settings <- get_settings_list()
      write(jsonlite::toJSON(settings, pretty = TRUE, auto_unbox = TRUE), file)
      showNotification(graph_tr("settings_saved", lang()), type = "message", duration = 2)
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
      if (!is.null(s$energy_unit)) updateSelectInput(session, "energy_unit", selected = as.character(s$energy_unit))
      n_inj <- get_valid_injection_count(apply_ratio = isTRUE(input$graph_apply_ratio_correction))
      if (!is.null(s$bot_no_dim_start) || !is.null(s$bot_no_dim_end)) {
        imported_range <- c(
          safe_num_scalar(s$bot_no_dim_start, default = NA_real_),
          safe_num_scalar(s$bot_no_dim_end, default = NA_real_)
        )
        no_dim_range <- resolve_no_dim_range(imported_range, n_inj)
        updateSliderInput(session, "bot_no_dim_range", min = 1, max = n_inj, value = no_dim_range)
      } else if (!is.null(s$bot_dim_first_point)) {
        old_dim_first <- isTRUE(as.logical(s$bot_dim_first_point))
        no_dim_range <- if (old_dim_first) c(2, n_inj) else c(1, n_inj)
        no_dim_range <- resolve_no_dim_range(no_dim_range, n_inj)
        updateSliderInput(session, "bot_no_dim_range", min = 1, max = n_inj, value = no_dim_range)
      }
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
      showNotification(graph_tr("settings_imported", lang()), type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste0(graph_tr("settings_import_failed_prefix", lang()), e$message), type = "error", duration = 8)
    })
  })

  observeEvent(input$reset_settings, {
    reset_plot_controls_to_defaults()
    reset_settings_nonce(isolate(reset_settings_nonce()) + 1L)
    showNotification(graph_tr("settings_reset", lang()), type = "message", duration = 3)
  })
}
