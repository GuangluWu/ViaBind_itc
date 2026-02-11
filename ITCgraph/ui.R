# ==============================================================================
# ui.R - ITCgraph UI 布局
# ==============================================================================

ui <- fluidPage(
  theme = NULL,
  tags$head(
    tags$style(HTML("
      /* ---- Scoped base (Step 3 only) ---- */
      .graph-root { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; }
      .graph-root .well { padding: 12px; margin-bottom: 12px; }
      .graph-root h4 { margin-top: 5px; font-weight: bold; font-size: 15px; }
      .graph-root hr { margin-top: 8px; margin-bottom: 8px; border-top: 1px solid #ddd; }

      /* ---- 按钮样式 ---- */
      .graph-root {
        --btn-radius: 4px;
        --btn-shadow: 0 1px 3px rgba(0,0,0,0.1);
        --c-default-bg: #f8f9fa; --c-default-text: #343a40; --c-default-border: #dae0e5;
        --c-success-bg: #d4edda; --c-success-text: #155724; --c-success-border: #c3e6cb;
        --c-info-bg:    #d1ecf1; --c-info-text: #0c5460; --c-info-border: #bee5eb;
      }
      .graph-root .btn {
        border-radius: var(--btn-radius) !important;
        box-shadow: var(--btn-shadow);
        font-size: 13px !important;
        padding: 6px 12px !important;
        border: 1px solid transparent !important;
        transition: all 0.2s;
        background-image: none !important;
        text-shadow: none !important;
      }
      .graph-root .btn:hover { box-shadow: 0 2px 5px rgba(0,0,0,0.15); transform: translateY(-1px); }
      .graph-root .btn-default {
        background-color: var(--c-default-bg) !important;
        color: var(--c-default-text) !important;
        border-color: var(--c-default-border) !important;
      }
      .graph-root .btn-success {
        background-color: var(--c-success-bg) !important;
        color: var(--c-success-text) !important;
        border-color: var(--c-success-border) !important;
      }
      .graph-root .btn-info {
        background-color: var(--c-info-bg) !important;
        color: var(--c-info-text) !important;
        border-color: var(--c-info-border) !important;
      }

      /* ---- 设置面板中的紧凑排版 ---- */
      .graph-root .settings-panel .form-group { margin-bottom: 8px; }
      .graph-root .settings-panel label { font-size: 12px; font-weight: 500; margin-bottom: 2px; }
      .graph-root .settings-panel .form-control { font-size: 12px; height: 30px; padding: 4px 8px; }

      /* ---- 颜色选择器行内布局 ---- */
      .graph-root .color-row { display: flex; align-items: center; gap: 8px; margin-bottom: 6px; }
      .graph-root .color-row label { font-size: 12px; font-weight: 500; margin: 0; min-width: 60px; }

      /* ---- 控制面板滚动 ---- */
      .graph-root .control-scroll {
        max-height: calc(100vh - 100px);
        overflow-y: auto;
        padding-right: 5px;
      }

      /* ---- 数据概要 ---- */
      .graph-root .data-summary { font-size: 12px; color: #666; padding: 5px 0; }
      .graph-root .data-summary .badge { font-size: 11px; margin-left: 4px; }

      /* ---- 范围：标签 + Min + Max + Reset 同一行，输入框收窄 ---- */
      .graph-root .range-one-row { display: flex; align-items: flex-end; gap: 6px; margin-bottom: 6px; flex-wrap: wrap; }
      .graph-root .range-one-row .range-label { font-size: 12px; font-weight: 500; white-space: nowrap; margin-right: 2px; }
      .graph-root .range-one-row .range-min, .graph-root .range-one-row .range-max { width: 72px; flex-shrink: 0; }
      .graph-root .range-one-row .range-min .form-group, .graph-root .range-one-row .range-max .form-group { margin-bottom: 0 !important; }
      .graph-root .range-one-row .form-group label { margin-bottom: 0 !important; font-size: 11px; }
      
      /* ---- 导出按钮组 ---- */
      .graph-root .export-btn-group { display: flex; gap: 6px; flex-wrap: wrap; }
      .graph-root .export-btn-group .btn { flex: 1; min-width: 80px; }

      /* ---- 保存/导入设置区域：左右两列，每列上字下钮对齐 ---- */
      .graph-root .settings-save-import { margin-top: 10px; padding-top: 10px; border-top: 1px solid #e9ecef; }
      .graph-root .settings-save-import .settings-col { display: flex; flex-direction: column; gap: 6px; }
      .graph-root .settings-save-import .settings-hint { font-size: 11px; color: #6c757d; line-height: 1.3; margin: 0; }
      .graph-root .settings-save-import .btn-save-settings { width: 100%; min-width: 120px; font-weight: 500; }
      .graph-root .settings-save-import .shiny-input-container { width: 100%; }
      .graph-root .settings-save-import .form-group { margin-bottom: 0; }

      /* ---- 下拉菜单浮于上方，不被 column 遮挡 (Point shape, Linetype 等) ---- */
      .graph-root .selectize-dropdown { z-index: 99999 !important; }
      .graph-root .selectize-dropdown-content { z-index: 99999 !important; }
      /* Bootstrap select 原生下拉 */
      .graph-root .form-group.shiny-input-container { position: relative; }
      .graph-root select.form-control:focus { z-index: 1000; }

      /* ---- 图形预览容器：足够高以完整显示图，超出时滚动 ---- */
      .graph-root .preview-container {
        min-height: 75vh;
        max-height: 90vh;
        overflow: auto;
        width: 100%;
      }
      .graph-root .preview-container .shiny-plot-output { max-width: 100%; }

      /* Read-only fit factor fields */
      .graph-root input.readonly-fit-factor {
        background-color: #f2f2f2 !important;
        color: #555 !important;
        cursor: not-allowed;
      }

      /* File input button-only style (same interaction as Step 2) */
      .graph-root .file-input-btn-style .input-group {
        display: flex;
        width: 100%;
      }
      .graph-root .file-input-btn-style .input-group-btn {
        width: 100%;
      }
      .graph-root .file-input-btn-style .btn-file {
        width: 100%;
      }
      .graph-root .file-input-btn-style .form-control {
        display: none;
      }
      .graph-root .shiny-file-input-progress {
        display: none !important;
      }
    ")),
    tags$script(HTML("
      function markGraphReadonlyFields() {
        $('#graph_fh_display, #graph_fg_display').each(function() {
          $(this)
            .prop('readonly', true)
            .attr('readonly', 'readonly')
            .addClass('readonly-fit-factor');
        });
      }
      $(document).on('shiny:connected shiny:reconnected shown.bs.tab', function() {
        markGraphReadonlyFields();
      });
      $(document).on('shiny:value shiny:inputchanged', function() {
        markGraphReadonlyFields();
      });
    "))
  ),
  
  div(
    class = "graph-root",
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
            column(6, numericInput("top_line_width", label = "Line Width",
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
            column(4, numericInput("bot_point_size", label = "Point Size",
                                   value = PLOT_DEFAULTS$bot_point_size, min = 0.5, max = 10, step = 0.5))
          ),
          fluidRow(
            column(4, numericInput("bot_point_fill_alpha", label = "Point Fill Alpha",
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
            column(4, numericInput("bot_line_width", label = "Fit Line Width",
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
          div(
            class = "file-input-btn-style",
            fileInput("xlsx_file", label = NULL, accept = ".xlsx",
                      buttonLabel = "Import Data", placeholder = "No file selected")
          ),
          uiOutput("data_summary_ui"),
          uiOutput("heat_offset_label_ui"),
          fluidRow(
            column(6, numericInput("graph_heat_offset", label = NULL, value = PLOT_DEFAULTS$heat_offset, step = 10)),
            column(6, selectInput("energy_unit", label = NULL,
                                  choices = c("cal" = "cal", "J" = "J"),
                                  selected = PLOT_DEFAULTS$energy_unit))
          ),
          fluidRow(
            column(3, textInput("graph_fh_display", label = "fH", value = "1")),
            column(3, textInput("graph_fg_display", label = "fG", value = "1")),
            column(6, checkboxInput("graph_apply_ratio_correction", "Apply ratio correction (fG/fH)", value = TRUE))
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
)
