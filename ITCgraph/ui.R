# ==============================================================================
# ui.R - ITCgraph UI 布局
# ==============================================================================

ui <- fluidPage(
  theme = NULL,
  tags$head(
    tags$style(HTML("
      /* ---- 全局基础 ---- */
      body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; }
      .well { padding: 12px; margin-bottom: 12px; }
      h4 { margin-top: 5px; font-weight: bold; font-size: 15px; }
      hr { margin-top: 8px; margin-bottom: 8px; border-top: 1px solid #ddd; }

      /* ---- 按钮样式 ---- */
      :root {
        --btn-radius: 4px;
        --btn-shadow: 0 1px 3px rgba(0,0,0,0.1);
        --c-default-bg: #f8f9fa; --c-default-text: #343a40; --c-default-border: #dae0e5;
        --c-success-bg: #d4edda; --c-success-text: #155724; --c-success-border: #c3e6cb;
        --c-info-bg:    #d1ecf1; --c-info-text: #0c5460; --c-info-border: #bee5eb;
      }
      .btn {
        border-radius: var(--btn-radius) !important;
        box-shadow: var(--btn-shadow);
        font-size: 13px !important;
        padding: 6px 12px !important;
        border: 1px solid transparent !important;
        transition: all 0.2s;
        background-image: none !important;
        text-shadow: none !important;
      }
      .btn:hover { box-shadow: 0 2px 5px rgba(0,0,0,0.15); transform: translateY(-1px); }
      .btn-default {
        background-color: var(--c-default-bg) !important;
        color: var(--c-default-text) !important;
        border-color: var(--c-default-border) !important;
      }
      .btn-success {
        background-color: var(--c-success-bg) !important;
        color: var(--c-success-text) !important;
        border-color: var(--c-success-border) !important;
      }
      .btn-info {
        background-color: var(--c-info-bg) !important;
        color: var(--c-info-text) !important;
        border-color: var(--c-info-border) !important;
      }

      /* ---- 设置面板中的紧凑排版 ---- */
      .settings-panel .form-group { margin-bottom: 8px; }
      .settings-panel label { font-size: 12px; font-weight: 500; margin-bottom: 2px; }
      .settings-panel .form-control { font-size: 12px; height: 30px; padding: 4px 8px; }

      /* ---- 颜色选择器行内布局 ---- */
      .color-row { display: flex; align-items: center; gap: 8px; margin-bottom: 6px; }
      .color-row label { font-size: 12px; font-weight: 500; margin: 0; min-width: 60px; }

      /* ---- 控制面板滚动 ---- */
      .control-scroll {
        max-height: calc(100vh - 100px);
        overflow-y: auto;
        padding-right: 5px;
      }

      /* ---- 数据概要 ---- */
      .data-summary { font-size: 12px; color: #666; padding: 5px 0; }
      .data-summary .badge { font-size: 11px; margin-left: 4px; }

      /* ---- 范围：标签 + Min + Max + Reset 同一行，输入框收窄 ---- */
      .range-one-row { display: flex; align-items: flex-end; gap: 6px; margin-bottom: 6px; flex-wrap: wrap; }
      .range-one-row .range-label { font-size: 12px; font-weight: 500; white-space: nowrap; margin-right: 2px; }
      .range-one-row .range-min, .range-one-row .range-max { width: 72px; flex-shrink: 0; }
      .range-one-row .range-min .form-group, .range-one-row .range-max .form-group { margin-bottom: 0 !important; }
      .range-one-row .form-group label { margin-bottom: 0 !important; font-size: 11px; }
      
      /* ---- 导出按钮组 ---- */
      .export-btn-group { display: flex; gap: 6px; flex-wrap: wrap; }
      .export-btn-group .btn { flex: 1; min-width: 80px; }

      /* ---- 保存/导入设置区域：左右两列，每列上字下钮对齐 ---- */
      .settings-save-import { margin-top: 10px; padding-top: 10px; border-top: 1px solid #e9ecef; }
      .settings-save-import .settings-col { display: flex; flex-direction: column; gap: 6px; }
      .settings-save-import .settings-hint { font-size: 11px; color: #6c757d; line-height: 1.3; margin: 0; }
      .settings-save-import .btn-save-settings { width: 100%; min-width: 120px; font-weight: 500; }
      .settings-save-import .shiny-input-container { width: 100%; }
      .settings-save-import .form-group { margin-bottom: 0; }

      /* ---- 下拉菜单浮于上方，不被 column 遮挡 (Point shape, Linetype 等) ---- */
      .selectize-dropdown { z-index: 99999 !important; }
      .selectize-dropdown-content { z-index: 99999 !important; }
      /* Bootstrap select 原生下拉 */
      .form-group.shiny-input-container { position: relative; }
      select.form-control:focus { z-index: 1000; }

      /* ---- 图形预览容器：足够高以完整显示图，超出时滚动 ---- */
      .preview-container {
        min-height: 75vh;
        max-height: 90vh;
        overflow: auto;
        width: 100%;
      }
      .preview-container .shiny-plot-output { max-width: 100%; }
    "))
  ),

  # ---- 标题栏 ----
  fluidRow(
    column(10, uiOutput("app_title")),
    column(2, style = "text-align: right; padding-top: 10px;",
      actionButton("lang_toggle", label = "", class = "btn-default btn-sm")
    )
  ),
  hr(),

  # ---- 主体：三列 = 预览(1列) + 参数设置(2列) ----
  fluidRow(
    # ======== 第 1 列：图形预览 ========
    column(4,
      wellPanel(
        uiOutput("preview_header"),
        div(class = "preview-container",
          plotOutput("itc_preview", width = "100%")
        )
      )
    ),

    # ======== 第 2 列：Top Panel（上）+ Bottom Panel（下） ========
    column(4,
      div(class = "control-scroll settings-panel",
        wellPanel(
          uiOutput("section_top_ui"),
          fluidRow(
            column(4, textInput("top_xlab", label = NULL, value = PLOT_DEFAULTS$top_xlab)),
            column(5, textInput("top_ylab", label = NULL,
                                value = unit_label("top_ylab", PLOT_DEFAULTS$energy_unit))),
            column(3, uiOutput("time_unit_ui"))
          ),
          uiOutput("top_xrange_ui"),
          uiOutput("top_yrange_ui"),
          fluidRow(
            column(6, uiOutput("top_color_ui")),
            column(6, numericInput("top_line_width", label = NULL,
                                   value = PLOT_DEFAULTS$top_line_width, min = 0.1, max = 5, step = 0.1))
          )
        ),
        wellPanel(
          uiOutput("section_bot_ui"),
          fluidRow(
            column(6, textInput("bot_xlab", label = NULL, value = PLOT_DEFAULTS$bot_xlab)),
            column(6, textInput("bot_ylab", label = NULL,
                               value = unit_label("bot_ylab", PLOT_DEFAULTS$energy_unit)))
          ),
          uiOutput("bot_xrange_ui"),
          uiOutput("bot_yrange_ui"),
          # Point Settings
          fluidRow(
            column(4, uiOutput("bot_point_color_ui")),
            column(4, uiOutput("bot_point_fill_ui")),
            column(4, numericInput("bot_point_size", label = NULL,
                                   value = PLOT_DEFAULTS$bot_point_size, min = 0.5, max = 10, step = 0.5))
          ),
          fluidRow(
            column(4, numericInput("bot_point_fill_alpha", label = NULL,
                                   value = PLOT_DEFAULTS$bot_point_fill_alpha, min = 0, max = 1, step = 0.05)),
            column(4, uiOutput("bot_shape_ui")),
            column(4, uiOutput("bot_layer_order_ui"))
          ),
          fluidRow(
            column(12, uiOutput("bot_first_point_dim_ui"))
          ),
          # Fit Line Settings
          fluidRow(
            column(4, uiOutput("bot_line_color_ui")),
            column(4, numericInput("bot_line_width", label = NULL,
                                   value = PLOT_DEFAULTS$bot_line_width, min = 0.1, max = 5, step = 0.1)),
            column(4, uiOutput("bot_line_linetype_ui"))
          )
        )
      )
    ),

    # ======== 第 3 列：Data Import + Global Settings ========
    column(4,
      div(class = "control-scroll settings-panel",
        wellPanel(
          uiOutput("section_import_ui"),
          fileInput("xlsx_file", label = NULL, accept = ".xlsx",
                    buttonLabel = "Browse...", placeholder = "No file selected"),
          uiOutput("data_summary_ui"),
          uiOutput("heat_offset_label_ui"),
          fluidRow(
            column(6, numericInput("graph_heat_offset", label = NULL, value = PLOT_DEFAULTS$heat_offset, step = 10)),
            column(6, selectInput("energy_unit", label = NULL,
                                  choices = c("cal" = "cal", "J" = "J"),
                                  selected = PLOT_DEFAULTS$energy_unit))
          )
        ),
        wellPanel(
          uiOutput("section_global_ui"),
          fluidRow(
            column(3, numericInput("base_size", label = NULL,
                                   value = PLOT_DEFAULTS$base_size, min = 6, max = 24, step = 1)),
            column(3, numericInput("height_ratio_top", label = NULL,
                                   value = 1, min = 0.1, max = 5, step = 0.1)),
            column(3, numericInput("height_ratio_bot", label = NULL,
                                   value = 1, min = 0.1, max = 5, step = 0.1)),
            column(3, numericInput("border_linewidth", label = NULL,
                                   value = PLOT_DEFAULTS$border_linewidth, min = 0.1, max = 3, step = 0.1))
          ),
          div(class = "settings-save-import",
            fluidRow(
              column(6, div(class = "settings-col",
                uiOutput("settings_save_hint_ui"),
                uiOutput("save_settings_ui")
              )),
              column(6, div(class = "settings-col",
                uiOutput("settings_import_hint_ui"),
                uiOutput("import_settings_ui")
              ))
            )
          )
        ),
        wellPanel(
          uiOutput("section_export_ui"),
          fluidRow(
            column(4, numericInput("export_width", label = NULL,
                                   value = PLOT_DEFAULTS$export_width, min = 1, max = 20, step = 0.5)),
            column(4, numericInput("export_height", label = NULL,
                                   value = PLOT_DEFAULTS$export_height, min = 1, max = 20, step = 0.5)),
            column(4, numericInput("export_dpi", label = NULL,
                                   value = PLOT_DEFAULTS$export_dpi, min = 72, max = 1200, step = 50))
          ),
          div(class = "export-btn-group",
            downloadButton("export_pdf", label = "PDF", class = "btn-success"),
            downloadButton("export_png", label = "PNG", class = "btn-info"),
            downloadButton("export_tiff", label = "TIFF", class = "btn-default")
          )
        )
      )
    )
  ) # end fluidRow
)
