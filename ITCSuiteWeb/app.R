fail_fast <- function(...) {
  stop(paste0(...), call. = FALSE)
}

startup_root_dir <- dirname(getwd())
startup_simfit_dir <- file.path(startup_root_dir, "ITCsimfit")
startup_itccore_dir <- file.path(startup_root_dir, "itcCore")
startup_lockfile <- file.path(startup_root_dir, "renv.lock")

startup_required_runtime_pkgs <- c(
  "shiny", "DT", "ggplot2", "plotly", "patchwork",
  "promises", "future", "jsonlite", "readxl", "writexl",
  "rootSolve", "tidyverse"
)
startup_required_lock_pkgs <- startup_required_runtime_pkgs
startup_required_step2_files <- c(
  "R/constants.R",
  "R/utils.R",
  "R/i18n.R",
  "R/performance.R",
  "R/core_logic.R",
  "R/weighting.R",
  "R/fitting.R",
  "R/error_analysis.R",
  "R/visualization.R",
  "ui.R",
  "server.R"
)

assert_startup_environment <- function() {
  if (!dir.exists(startup_itccore_dir)) {
    fail_fast("Startup check failed: missing itcCore directory at ", startup_itccore_dir)
  }
  if (!dir.exists(file.path(startup_itccore_dir, "R"))) {
    fail_fast("Startup check failed: missing itcCore/R directory at ", file.path(startup_itccore_dir, "R"))
  }
  if (!dir.exists(startup_simfit_dir)) {
    fail_fast("Startup check failed: missing ITCsimfit directory at ", startup_simfit_dir)
  }

  missing_files <- startup_required_step2_files[
    !vapply(startup_required_step2_files, function(rel) {
      file.exists(file.path(startup_simfit_dir, rel))
    }, logical(1))
  ]
  if (length(missing_files) > 0) {
    fail_fast(
      "Startup check failed: missing required ITCsimfit files: ",
      paste(missing_files, collapse = ", ")
    )
  }

  missing_pkgs <- startup_required_runtime_pkgs[
    !vapply(startup_required_runtime_pkgs, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_pkgs) > 0) {
    fail_fast(
      "Startup check failed: missing required packages: ",
      paste(missing_pkgs, collapse = ", "),
      ". Run renv::restore() in ", startup_root_dir
    )
  }

  if (!file.exists(startup_lockfile)) {
    fail_fast("Startup check failed: missing renv.lock at ", startup_lockfile)
  }
  lock <- tryCatch(
    jsonlite::fromJSON(startup_lockfile, simplifyVector = FALSE),
    error = function(e) fail_fast("Startup check failed: cannot parse renv.lock: ", e$message)
  )
  lock_pkgs <- names(lock$Packages %||% list())
  missing_lock_pkgs <- setdiff(startup_required_lock_pkgs, lock_pkgs)
  if (length(missing_lock_pkgs) > 0) {
    fail_fast(
      "Startup check failed: renv.lock is missing required package records: ",
      paste(missing_lock_pkgs, collapse = ", "),
      ". Recreate lockfile with renv::snapshot()."
    )
  }
}

`%||%` <- function(x, y) if (is.null(x)) y else x

assert_startup_environment()

library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(patchwork)
library(promises)
library(future)
library(jsonlite)
library(readxl)
library(writexl)

future::plan(future::multisession)

load_local_itc_core <- function() {
  core_dir <- file.path(dirname(getwd()), "itcCore", "R")
  for (f in sort(list.files(core_dir, pattern = "\\.R$", full.names = TRUE))) {
    source(f, local = FALSE)
  }
}

load_local_itc_core()

if (!exists("PARAM_BOUNDS", inherits = TRUE)) {
  PARAM_BOUNDS <- list(
    logK = c(lower = 0, upper = 9),
    H = c(lower = -15000, upper = 5000)
  )
}

load_legacy_simfit_engine <- function() {
  base_dir <- dirname(getwd())
  simfit_r_dir <- file.path(base_dir, "ITCsimfit", "R")
  env <- new.env(parent = globalenv())
  for (fn in c("constants.R", "core_logic.R", "fitting.R", "weighting.R")) {
    fp <- file.path(simfit_r_dir, fn)
    if (!file.exists(fp)) {
      fail_fast("Startup check failed: legacy engine file not found: ", fp)
    }
    sys.source(fp, envir = env)
  }
  required <- c(
    "calculate_simulation",
    "get_parameter_bounds",
    "calculate_weights_from_derivative",
    "calculate_weighted_robust_loss"
  )
  if (!all(required %in% ls(env))) {
    fail_fast("Startup check failed: legacy ITCsimfit engine is incomplete.")
  }
  env
}

legacy_fit_env <- load_legacy_simfit_engine()

load_legacy_simfit_module <- function() {
  simfit_dir <- file.path(dirname(getwd()), "ITCsimfit")

  env <- new.env(parent = globalenv())
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(simfit_dir)

  needed_pkgs <- c("shiny", "rootSolve", "tidyverse", "DT", "readxl", "writexl")
  for (pkg in needed_pkgs) {
    ok <- suppressWarnings(requireNamespace(pkg, quietly = TRUE))
    if (!ok) {
      fail_fast(sprintf("Startup check failed: missing legacy package '%s'.", pkg))
    }
  }

  for (fp in c(
    "R/constants.R",
    "R/utils.R",
    "R/i18n.R",
    "R/performance.R",
    "R/core_logic.R",
    "R/weighting.R",
    "R/fitting.R",
    "R/error_analysis.R",
    "R/visualization.R",
    "ui.R",
    "server.R"
  )) {
    if (!file.exists(fp)) {
      fail_fast(sprintf("Startup check failed: legacy Step 2 file missing: %s", fp))
    }
    sys.source(fp, envir = env)
  }

  if (!is.function(env$server) || is.null(env$ui)) {
    fail_fast("Startup check failed: legacy Step 2 ui/server is not available.")
  }

  if (exists("tr", envir = env, inherits = FALSE)) assign("tr", env$tr, envir = .GlobalEnv)
  if (exists("trf", envir = env, inherits = FALSE)) assign("trf", env$trf, envir = .GlobalEnv)

  list(ui = env$ui, server = env$server, env = env)
}

legacy_step2_module <- load_legacy_simfit_module()
if (is.null(legacy_step2_module) || !is.function(legacy_step2_module$server) || is.null(legacy_step2_module$ui)) {
  fail_fast("Startup check failed: Step 2 legacy module failed to load; fallback is disabled.")
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML(" 
      .kpi { padding: 10px 12px; border-radius: 8px; border: 1px solid #dcdcdc; background: #f8fafc; }
      html { overflow-y: scroll; }
      .step-title { margin-top: 0; }
      .fit-running { color: #d9534f; font-weight: 600; }
      .fit-idle { color: #198754; font-weight: 600; }
      .main-tabs-wrap {
        position: relative;
      }
      .main-lang-switch {
        position: absolute;
        right: 8px;
        top: 6px;
        z-index: 20;
      }
      .main-tabs-wrap .nav > li > a {
        padding-right: 18px;
      }
      .step1-left {
        position: sticky;
        top: 8px;
        max-height: calc(100vh - 24px);
        overflow-y: scroll;
        padding-right: 6px;
      }
      .step1-card {
        border: 1px solid #e5e7eb;
        border-radius: 8px;
        padding: 10px;
        background: #ffffff;
      }
      .step1-plots {
        display: flex;
        flex-direction: column;
        gap: 2px;
      }
      .step1-table-wrap {
        margin-top: 6px;
      }
      .step1-expt-panel {
        margin-top: 2px;
        margin-bottom: 4px;
        padding-top: 8px;
        padding-bottom: 8px;
      }
      .step1-expt-panel h5 {
        margin-top: 0;
        margin-bottom: 6px;
      }
      .step1-btn-row {
        display: flex;
        gap: 8px;
        align-items: center;
      }
      .step1-btn-row .btn, .step1-btn-row .btn-group {
        flex: 1 1 0;
      }
      .btn.btn-default.btn-process-go {
        background-color: #337ab7 !important;
        border-color: #2e6da4 !important;
        color: #ffffff !important;
      }
      .btn.btn-default.btn-process-go:hover,
      .btn.btn-default.btn-process-go:focus {
        background-color: #286090 !important;
        border-color: #204d74 !important;
        color: #ffffff !important;
      }
      .step1-btn-row .btn, .step1-btn-row .shiny-download-link {
        width: 100%;
      }
      .step1-section-head {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 8px;
        margin-bottom: 4px;
      }
      .step1-section-head h5 {
        margin: 0;
      }
      .step1-param-row {
        display: grid;
        grid-template-columns: repeat(8, minmax(0, 1fr));
        gap: 8px;
      }
      .step1-param-cell {
        min-width: 0;
      }
      .step1-param-fixed {
        padding: 0;
      }
      .step1-param-fixed-box {
        border: 1px solid #ced4da;
        border-radius: 4px;
        background-color: #e9ecef;
        min-height: 32px;
        height: 32px;
        padding: 6px 8px;
        display: flex;
        align-items: center;
        font-size: 14px;
        line-height: 1;
      }
      .step1-param-cell .form-group {
        margin-bottom: 0;
      }
      .step1-param-cell label {
        font-size: 12px;
        margin-bottom: 2px;
      }
      .step1-param-cell input {
        height: 32px;
        padding: 4px 6px;
      }
      .step2-scroll {
        max-height: calc(100vh - 280px);
        overflow-y: auto;
        padding-right: 4px;
      }
      .step2-group {
        border-left: 4px solid #d1d5db;
        padding-left: 8px;
        margin-bottom: 10px;
      }
      .step2-grid-2 {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 6px;
      }
      .step2-grid-3 {
        display: grid;
        grid-template-columns: 1fr 1fr 1fr;
        gap: 6px;
      }
      .step2-btn-grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 6px;
      }
      .step2-subtitle {
        color: #6b7280;
        font-size: 12px;
        margin-top: -4px;
        margin-bottom: 8px;
      }
      .step2-tight .form-group {
        margin-bottom: 6px;
      }
      .param-group {
        border-left: 4px solid #eee;
        padding-left: 10px;
        margin-bottom: 10px;
      }
      .table-scroll-container {
        height: 165px;
        overflow: hidden;
        border: 1px solid #ddd;
        padding: 5px;
        background-color: #fff;
        margin-bottom: 15px;
      }
      .slider-scroll-container {
        height: 260px;
        overflow-y: auto;
        padding-right: 5px;
      }
      .header-action-row {
        display: flex;
        justify-content: space-between;
        align-items: center;
        gap: 8px;
        margin-bottom: 10px;
      }
      .header-action-row h4 {
        margin: 0;
        font-size: 16px;
        font-weight: 700;
      }
      .model-row {
        display: flex;
        align-items: center;
        width: 100%;
      }
      .col-mod { width: 75px; font-weight: 700; color: #333; }
      .col-par { width: 55px; font-family: monospace; color: #d35400; font-weight: 700; font-size: 0.95em; }
      .col-sto { width: 55px; color: #2980b9; font-style: italic; }
      .col-dsc { flex: 1; color: #7f8c8d; font-size: 0.85em; }
      .model-header {
        display: flex;
        font-weight: 700;
        color: #555;
        border-bottom: 1px solid #ddd;
        margin-bottom: 5px;
        padding-bottom: 4px;
        padding-left: 20px;
        font-size: 0.85em;
      }
      .rss-container {
        display: flex;
        align-items: center;
        gap: 10px;
        margin-top: 10px;
      }
      .rss-container h5 {
        margin: 0;
        white-space: nowrap;
        font-weight: 700;
        color: #555;
      }
      #fit_status {
        flex: 1;
        height: 34px !important;
        padding: 4px 10px !important;
        font-size: 14px !important;
        line-height: 24px !important;
        margin: 0 !important;
        background-color: #fff !important;
        border: 1px solid #ccc !important;
        border-radius: 4px !important;
        box-shadow: inset 0 1px 2px rgba(0,0,0,0.05);
        color: #333;
        overflow: hidden;
        white-space: nowrap;
        text-overflow: ellipsis;
      }
      .file-input-btn-style .form-control {
        display: none;
      }
      .file-input-btn-style .input-group-btn,
      .file-input-btn-style .btn-file {
        width: 100%;
      }
      .plot-section {
        position: relative;
      }
      .snapshot-section {
        margin-top: 10px;
      }
      @media (max-width: 1199px) {
        .step1-left {
          position: static;
          max-height: none;
          overflow: visible;
          padding-right: 0;
          margin-bottom: 10px;
        }
        .step1-plots {
          display: flex;
          flex-direction: column;
        }
        .step1-param-row {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
        .step1-btn-row {
          flex-direction: column;
        }
      }
    "))
  ),
  div(
    class = "main-tabs-wrap",
    div(class = "main-lang-switch", uiOutput("top_lang_switch")),
    tabsetPanel(
    id = "main_tabs",
    tabPanel(
      "Step 1 Process",
      fluidRow(
        column(
          3,
          div(
            class = "step1-left step1-card",
            fileInput("itc_file", "Import .itc File", accept = c(".itc", ".txt")),
            h5("View Control"),
            checkboxInput("zoom_baseline", "Auto-Zoom to Baseline Y-Axis", value = TRUE),
            hr(),
            div(
              class = "step1-section-head",
              h5("Baseline Settings (Spline)"),
              actionButton("btn_baseline_reset", "Reset", class = "btn btn-default btn-xs")
            ),
            sliderInput("baseline_offset", "Baseline Anchor Offset (s)", value = 5, min = 0, max = 10, step = 1),
            sliderInput("baseline_duration", "Baseline Anchor Width (s)", value = 20, min = 5, max = 60, step = 5),
            sliderInput("baseline_spar", "Spline Spar", min = 0, max = 1, value = 0.1, step = 0.01),
            hr(),
            h5("Integration Range"),
            numericInput("start_offset", "Start Offset (pts to inj)", value = 0, step = 1),
            checkboxInput("limit_integration", "Limit Integration End Point", value = TRUE),
            conditionalPanel(
              condition = "input.limit_integration == true",
              numericInput("integration_window", "End Offset (pts after inj)", value = 15, min = 1, step = 1)
            ),
            hr(),
            div(
              class = "step1-btn-row",
              actionButton("btn_process", "Data → Fit", class = "btn-process-go"),
              downloadButton("download_step1_bundle", "Export Processed Data")
            )
          )
        ),
        column(
          9,
          div(
            class = "step1-card",
            div(
              class = "step1-plots",
              plotlyOutput("step1_plot_raw", height = "255px"),
              plotlyOutput("step1_plot_corrected", height = "255px"),
              plotlyOutput("step1_plot_integration", height = "185px"),
              wellPanel(
                class = "step1-expt-panel",
                h5("Expt Params"),
                div(
                  class = "step1-param-row",
                  div(class = "step1-param-cell step1-param-fixed",
                      tags$label("N inj"),
                      div(class = "step1-param-fixed-box", textOutput("step1_n_inj_2", inline = TRUE))),
                  div(class = "step1-param-cell step1-param-fixed",
                      tags$label("Interval (s)"),
                      div(class = "step1-param-fixed-box", textOutput("step1_interval_s_2", inline = TRUE))),
                  div(class = "step1-param-cell step1-param-fixed",
                      tags$label("Temp (C)"),
                      div(class = "step1-param-fixed-box", textOutput("step1_temp_c_2", inline = TRUE))),
                  div(class = "step1-param-cell step1-param-fixed",
                      tags$label("Cell vol (mL)"),
                      div(class = "step1-param-fixed-box", textOutput("step1_cell_vol_2", inline = TRUE))),
                  div(class = "step1-param-cell", numericInput("param_syringe_mM", "Syringe (mM)", value = NA, step = 0.001, width = "100%")),
                  div(class = "step1-param-cell", numericInput("param_cell_mM", "Cell (mM)", value = NA, step = 0.001, width = "100%")),
                  div(class = "step1-param-cell", numericInput("param_V_pre_ul", "V_pre (uL)", value = NA, step = 0.1, width = "100%")),
                  div(class = "step1-param-cell", numericInput("param_V_inj_ul", "V_inj (uL)", value = NA, step = 0.1, width = "100%"))
                )
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(class = "step1-table-wrap", DTOutput("step1_table"))
        )
      )
    ),
    tabPanel(
      "Step 2 Sim & Fit",
      uiOutput("step2_legacy_ui"),
      if (FALSE) div(
        id = "step2_custom_wrap",
        fluidRow(
        column(
          6,
          div(class = "plot-section", style = "position: relative;",
              div(style = "position: absolute; right: 0; top: 0; z-index: 100;",
                  downloadButton("download_fit_data", "Export Fitting Data", class = "btn-success btn-xs")
              ),
              plotOutput("fit_plot", height = "520px")
          ),
          wellPanel(class = "snapshot-section",
                    div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:5px;",
                        h4("5. Parameter Snapshots"),
                        div(style = "display:flex; gap:6px; align-items:center;",
                            textInput("snap_name", NULL, placeholder = "Snapshot name", width = "220px"),
                            actionButton("save_params", "Save", class = "btn-success"),
                            actionButton("clear_params", "Clear", class = "btn-danger")
                        )
                    ),
                    div(class = "table-scroll-container", DTOutput("param_table")),
                    div(class = "flex-btn-row",
                        div(class = "flex-btn-item",
                            downloadButton("export_params", "Export Params", class = "btn-success btn-block")),
                        div(class = "flex-btn-item file-input-btn-style",
                            fileInput("import_params_file", NULL, buttonLabel = "Import Params", accept = ".xlsx", multiple = FALSE))
                    )
          )
        ),
        column(
          3,
          wellPanel(
            div(style = "margin-bottom: 10px; display:flex; justify-content:space-between; align-items:center;",
                tags$span("1. Path Build", style = "font-weight:bold; font-size:1.1em; margin-right:10px;"),
                actionButton("sim_to_exp", "Sim -> Exp", class = "btn-info btn-sm")
            ),
            tags$span("Build reactions on top of base M path.", style = "color:gray; font-size:0.8em;"),
            div(class = "model-header",
                div(class = "col-mod", "Model"),
                div(class = "col-par", "Param"),
                div(class = "col-sto", "Stoich"),
                div(class = "col-dsc", "Description")
            ),
            div(class = "checkbox", style = "color: gray;",
                tags$label(
                  tags$input(type = "checkbox", checked = NA, disabled = NA),
                  tags$span(
                    div(class = "model-row",
                        span(class = "col-mod", "H+G=M"),
                        span(class = "col-par", "K1 H1"),
                        span(class = "col-sto", "H1G1"),
                        span(class = "col-dsc", "Base path"))
                  )
                )
            ),
            br(),
            checkboxGroupInput(
              "active_paths",
              NULL,
              choiceNames = list(
                HTML("<div class='model-row'><span class='col-mod'>M+G=D</span><span class='col-par'>K2 H2</span><span class='col-sto'>H1G2</span><span class='col-dsc'>Stepwise</span></div>"),
                HTML("<div class='model-row'><span class='col-mod'>M+M=T</span><span class='col-par'>K3 H3</span><span class='col-sto'>H2G2</span><span class='col-dsc'>Dimer</span></div>"),
                HTML("<div class='model-row'><span class='col-mod'>M+H=B</span><span class='col-par'>K4 H4</span><span class='col-sto'>H2G1</span><span class='col-dsc'>Reverse</span></div>"),
                HTML("<div class='model-row'><span class='col-mod'>M+D=F</span><span class='col-par'>K5 H5</span><span class='col-sto'>H2G3</span><span class='col-dsc'>Oligomer</span></div>"),
                HTML("<div class='model-row'><span class='col-mod'>M=U</span><span class='col-par'>K6 H6</span><span class='col-sto'>H1G1</span><span class='col-dsc'>Bending</span></div>")
              ),
              choiceValues = c("rxn_D", "rxn_T", "rxn_B", "rxn_F", "rxn_U"),
              selected = character(0)
            )
          ),
          wellPanel(
            div(class = "header-action-row",
                h4("2. Manual Tuning"),
                actionButton("reset_defaults", "Reset", class = "btn-danger btn-xs")
            ),
            p("Adjust model parameters manually.", style = "color:gray; font-size:0.8em; margin-top: -5px;"),
            div(
              class = "slider-scroll-container",
              div(
                class = "param-group",
                strong("Base (M)"),
                sliderInput("logK1", "logK1", min = PARAM_BOUNDS$logK["lower"], max = PARAM_BOUNDS$logK["upper"], value = DEFAULT_PARAMS$logK, step = 0.001, ticks = FALSE),
                sliderInput("H1", "dH1", min = PARAM_BOUNDS$H["lower"], max = PARAM_BOUNDS$H["upper"], value = DEFAULT_PARAMS$H, step = 100, ticks = FALSE)
              ),
              conditionalPanel(
                "input.active_paths.includes('rxn_D')",
                div(
                  class = "param-group",
                  strong("Stepwise (D)"),
                  sliderInput("logK2", "logK2", min = PARAM_BOUNDS$logK["lower"], max = PARAM_BOUNDS$logK["upper"], value = DEFAULT_PARAMS$logK, step = 0.001, ticks = FALSE),
                  sliderInput("H2", "dH2", min = PARAM_BOUNDS$H["lower"], max = PARAM_BOUNDS$H["upper"], value = DEFAULT_PARAMS$H, step = 100, ticks = FALSE)
                )
              ),
              conditionalPanel(
                "input.active_paths.includes('rxn_T')",
                div(
                  class = "param-group",
                  strong("Dimer (T)"),
                  sliderInput("logK3", "logK3", min = PARAM_BOUNDS$logK["lower"], max = PARAM_BOUNDS$logK["upper"], value = DEFAULT_PARAMS$logK, step = 0.001, ticks = FALSE),
                  sliderInput("H3", "dH3", min = PARAM_BOUNDS$H["lower"], max = PARAM_BOUNDS$H["upper"], value = DEFAULT_PARAMS$H, step = 100, ticks = FALSE)
                )
              ),
              conditionalPanel(
                "input.active_paths.includes('rxn_B')",
                div(
                  class = "param-group",
                  strong("Reverse (B)"),
                  sliderInput("logK4", "logK4", min = PARAM_BOUNDS$logK["lower"], max = PARAM_BOUNDS$logK["upper"], value = DEFAULT_PARAMS$logK, step = 0.001, ticks = FALSE),
                  sliderInput("H4", "dH4", min = PARAM_BOUNDS$H["lower"], max = PARAM_BOUNDS$H["upper"], value = DEFAULT_PARAMS$H, step = 100, ticks = FALSE)
                )
              ),
              conditionalPanel(
                "input.active_paths.includes('rxn_F')",
                div(
                  class = "param-group",
                  strong("Oligomer (F)"),
                  sliderInput("logK5", "logK5", min = PARAM_BOUNDS$logK["lower"], max = PARAM_BOUNDS$logK["upper"], value = DEFAULT_PARAMS$logK, step = 0.001, ticks = FALSE),
                  sliderInput("H5", "dH5", min = PARAM_BOUNDS$H["lower"], max = PARAM_BOUNDS$H["upper"], value = DEFAULT_PARAMS$H, step = 100, ticks = FALSE)
                )
              ),
              conditionalPanel(
                "input.active_paths.includes('rxn_U')",
                div(
                  class = "param-group",
                  strong("Bending (U)"),
                  sliderInput("logK6", "logK6", min = PARAM_BOUNDS$logK["lower"], max = PARAM_BOUNDS$logK["upper"], value = DEFAULT_PARAMS$logK, step = 0.001, ticks = FALSE),
                  sliderInput("H6", "dH6", min = PARAM_BOUNDS$H["lower"], max = PARAM_BOUNDS$H["upper"], value = DEFAULT_PARAMS$H, step = 100, ticks = FALSE)
                )
              )
            ),
            tags$hr(),
            h4("Correction"),
            div(style = "display:flex; flex-wrap:wrap; gap:5px;",
                div(style = "flex: 1 0 30%; min-width: 85px;", numericInput("factor_H", "fH", value = DEFAULT_PARAMS$fH, step = 0.01, width = "100%")),
                div(style = "flex: 1 0 30%; min-width: 85px;", numericInput("factor_G", "fG", value = DEFAULT_PARAMS$fG, step = 0.01, width = "100%")),
                div(style = "flex: 1 0 30%; min-width: 85px;", numericInput("V_init_val", "V_init (uL)", value = DEFAULT_PARAMS$V_init, step = 0.1, width = "100%"))
            ),
            sliderInput("heat_offset", "Heat offset (cal/mol)", -1500, 1500, DEFAULT_PARAMS$Offset, 10, ticks = FALSE)
          ),
          wellPanel(
            div(class = "header-action-row",
                h4("3. Experimental Data"),
                textOutput("fit_exp_file_label", inline = TRUE)
            ),
            div(class = "file-input-btn-style",
                fileInput("fit_exp_file", NULL, buttonLabel = "Import xlsx data", accept = ".xlsx", multiple = FALSE)
            ),
            p("Initialized from Step 1. You can adjust values for fitting.", style = "color:gray; font-size:0.8em; margin-top: -5px;"),
            splitLayout(
              cellWidths = c("50%", "50%"),
              numericInput("H_cell_0", "Cell (mM)", value = 0.03, min = 0, step = 0.001),
              numericInput("G_syringe", "Syringe (mM)", value = 0.6, min = 0, step = 0.001)
            ),
            splitLayout(
              cellWidths = c("50%", "50%"),
              numericInput("V_cell", "Cell vol (mL)", value = 0.2033, min = 0, step = 0.0001),
              numericInput("V_inj", "V_inj (uL)", value = 1.5, min = 0, step = 0.1)
            ),
            splitLayout(
              cellWidths = c("33%", "33%", "34%"),
              numericInput("n_inj", "N inj", value = 20, min = 2, max = 200, step = 1),
              numericInput("V_pre", "V_pre (uL)", value = 0.3, min = 0, step = 0.1),
              numericInput("Temp", "Temp (C)", value = 25, min = 0, max = 120, step = 0.1)
            ),
            br(),
            div(class = "table-scroll-container", DTOutput("fit_exp_table"))
          ),
          wellPanel(
            div(style = "display:flex; justify-content:space-between; align-items:center;",
                h4("4. Fitting"), uiOutput("fit_status_indicator")),
            uiOutput("dynamic_fit_params_ui"),
            tags$label("Fit range (inj)", style = "display: block; margin-bottom: 5px;"),
            sliderInput("fit_data_range", NULL, min = 1, max = 10, value = c(1, 10), step = 1, ticks = FALSE),
            numericInput("fit_max_iter", "Max iterations (full)", value = 100, min = 10, max = 2000, step = 10),
            tags$details(
              tags$summary("Advanced Options"),
              checkboxInput("use_weighted_fitting", "Weighted Fit (Derivative-based)", value = FALSE, width = "100%"),
              checkboxInput("use_robust_fitting", "Robust Fit (Huber Loss)", value = FALSE, width = "100%"),
              conditionalPanel(
                condition = "input.use_robust_fitting == true",
                numericInput("huber_delta", "Huber delta (optional)", value = 1.345, min = 0.1, max = 10, step = 0.1, width = "100%")
              )
            ),
            div(style = "display:flex; flex-wrap:wrap; gap:5px; margin-top:10px;",
                actionButton("fit_1_step", strong("Fit 1"), class = "btn-warning btn-sm", style = "flex:1 0 20%; min-width:80px;"),
                actionButton("fit_10_step", strong("Fit 10"), class = "btn-warning btn-sm", style = "flex:1 0 20%; min-width:80px;"),
                actionButton("fit_full", strong("Fit Full"), class = "btn-danger btn-sm", style = "flex:1 0 25%; min-width:100px;"),
                actionButton("fit_global", strong("Global Fit"), class = "btn-primary btn-sm", style = "flex:1 0 25%; min-width:100px;"),
                actionButton("btn_cancel_fit", strong("Cancel"), class = "btn-default btn-sm", style = "flex:1 0 20%; min-width:80px;")
            ),
            div(class = "rss-container",
                h5("RSS / Status"),
                div(style = "flex: 1;", verbatimTextOutput("fit_status", placeholder = TRUE))),
            uiOutput("fit_progress_ui"),
            hr(),
            h5("Fitted Parameters"),
            DTOutput("fit_params_table"),
            h5("Residuals"),
            DTOutput("fit_residual_table")
          )
          )
        )
      )
    ),
    tabPanel(
      "Step 3 Plot & Export",
      fluidRow(
        column(
          3,
          h4("Plot Settings", class = "step-title"),
          numericInput("plot_heat_offset", "Heat offset (cal/mol)", value = 0, step = 10),
          selectInput("plot_energy_unit", "Energy unit", choices = c("cal", "J"), selected = "cal"),
          downloadButton("download_plot_png", "Download PNG"),
          br(), br(),
          downloadButton("download_plot_pdf", "Download PDF"),
          hr(),
          downloadButton("download_final_bundle", "Download Fitted Bundle")
        ),
        column(
          9,
          h4("Publication Preview", class = "step-title"),
          plotOutput("final_plot", height = "700px")
        )
      )
    ),
    tabPanel(
      "Metrics",
      fluidRow(
        column(4, div(class = "kpi", textOutput("metric_import"))),
        column(4, div(class = "kpi", textOutput("metric_fit"))),
        column(4, div(class = "kpi", textOutput("metric_error")))
      ),
      br(),
      DTOutput("events_table")
    )
  )
  )
)

server <- function(input, output, session) {
  dir.create("logs", showWarnings = FALSE)
  log_file <- file.path("logs", "itcsuiteweb.jsonl")
  if (is.null(session$userData$itcsuite_bridge) || !is.list(session$userData$itcsuite_bridge)) {
    session$userData$itcsuite_bridge <- list(
      step1_payload = reactiveVal(NULL),
      step2_plot_payload = reactiveVal(NULL)
    )
  }
  session_bridge <- session$userData$itcsuite_bridge
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

  bridge_get <- function(channel) {
    ch <- session_bridge[[channel]]
    if (is.function(ch)) {
      return(ch())
    }
    store <- bridge_store_get_all()
    entry <- store[[bridge_session_key]]
    if (is.null(entry) || !is.list(entry)) return(NULL)
    entry[[channel]]
  }

  bridge_set <- function(channel, payload) {
    ch <- session_bridge[[channel]]
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

  session$onSessionEnded(function() {
    store <- bridge_store_get_all()
    if (!is.null(store[[bridge_session_key]])) {
      store[[bridge_session_key]] <- NULL
      bridge_store_put_all(store)
    }
  })

  # Standardized Step 2 interface:
  # - input: bundle (publish_bundle)
  # - output: fit payload (fit_payload reactive)
  step2_fit_module <- local({
    build_step1_payload_from_bundle <- function(bundle, source_file = NULL) {
      if (is.null(bundle) || !is.list(bundle)) return(NULL)
      integration <- bundle$integration
      if (is.null(integration) || !is.data.frame(integration) || nrow(integration) == 0) return(NULL)
      meta <- bundle$meta
      if (is.null(meta) || !is.data.frame(meta)) {
        meta <- data.frame(parameter = character(0), value = character(0), stringsAsFactors = FALSE)
      }
      list(
        token = as.numeric(Sys.time()),
        integration = integration,
        meta = meta,
        source = if (!is.null(source_file)) as.character(source_file) else "",
        bundle = bundle
      )
    }

    publish_bundle <- function(bundle, source_file = NULL) {
      payload <- build_step1_payload_from_bundle(bundle, source_file = source_file)
      if (is.null(payload)) return(invisible(FALSE))
      bridge_set("step1_payload", payload)
      invisible(TRUE)
    }

    fit_payload <- reactive({
      bridge_get("step2_plot_payload")
    })

    list(
      publish_bundle = publish_bundle,
      fit_payload = fit_payload
    )
  })

  publish_step1_payload <- function(bundle, source_file = NULL) {
    ok <- step2_fit_module$publish_bundle(bundle = bundle, source_file = source_file)
    if (!isTRUE(ok)) return(invisible(NULL))
    invisible(NULL)
  }

  values <- reactiveValues(
    raw_itc = NULL,
    processed = NULL,
    bundle = NULL,
    fit_result = NULL,
    fit_running = FALSE,
    fit_progress = list(value = 0, message = "Idle"),
    fit_progress_file = NULL,
    fit_cancel_file = NULL,
    fit_cache_key = NULL,
    fit_cache_result = NULL,
    manual_exp_data = NULL,
    imported_fit_source = "",
    param_snapshots = data.frame(
      name = character(0),
      ts = character(0),
      active_paths = character(0),
      logK1 = numeric(0), H1 = numeric(0),
      logK2 = numeric(0), H2 = numeric(0),
      logK3 = numeric(0), H3 = numeric(0),
      logK4 = numeric(0), H4 = numeric(0),
      logK5 = numeric(0), H5 = numeric(0),
      logK6 = numeric(0), H6 = numeric(0),
      fH = numeric(0), fG = numeric(0),
      V_init = numeric(0), Offset = numeric(0),
      stringsAsFactors = FALSE
    ),
    events = data.frame(
      ts = character(0),
      step = character(0),
      status = character(0),
      duration_ms = numeric(0),
      error_code = character(0),
      detail = character(0),
      stringsAsFactors = FALSE
    )
  )

  log_event <- function(step, status, started_at, error_code = "", detail = "") {
    duration_ms <- round(as.numeric(difftime(Sys.time(), started_at, units = "secs")) * 1000, 2)
    row <- data.frame(
      ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      step = step,
      status = status,
      duration_ms = duration_ms,
      error_code = error_code,
      detail = as.character(detail),
      stringsAsFactors = FALSE
    )
    values$events <- rbind(values$events, row)
    write(jsonlite::toJSON(as.list(row[1, ]), auto_unbox = TRUE), file = log_file, append = TRUE)
    write("\n", file = log_file, append = TRUE)
  }

  output$top_lang_switch <- renderUI({
    actionButton("lang_btn", "EN", class = "btn btn-default btn-sm")
  })

  use_legacy_step2 <- !is.null(legacy_step2_module) &&
    is.function(legacy_step2_module$server) &&
    !is.null(legacy_step2_module$ui)

  build_figure <- reactive({
    req(values$bundle)
    fit_obj <- if (!is.null(values$fit_result) && !isTRUE(values$fit_result$cancelled)) values$fit_result else NULL
    pd <- build_plot_data(
      values$bundle,
      fit_result = fit_obj,
      plot_cfg = list(heat_offset = input$plot_heat_offset, energy_unit = input$plot_energy_unit)
    )
    create_itc_figure(
      power_data = pd$power_data,
      integration_data = pd$integration_data,
      simulation_data = pd$simulation_data,
      params = pd$params
    )
  })

  output$final_plot <- renderPlot({
    req(values$bundle)
    build_figure()
  })

  output$download_plot_png <- downloadHandler(
    filename = function() paste0("itc_plot_", format(Sys.time(), "%Y%m%d_%H%M"), ".png"),
    content = function(file) {
      p <- build_figure()
      ggplot2::ggsave(file, p, width = 5, height = 7, dpi = 300)
    }
  )

  output$download_plot_pdf <- downloadHandler(
    filename = function() paste0("itc_plot_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf"),
    content = function(file) {
      p <- build_figure()
      ggplot2::ggsave(file, p, width = 5, height = 7)
    }
  )

  output$download_final_bundle <- downloadHandler(
    filename = function() paste0("itc_bundle_fitted_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx"),
    content = function(file) {
      req(values$bundle)
      b <- ensure_power_original_sheet(values$bundle, values$raw_itc)
      export_bundle(b, file)
    }
  )

  output$metric_import <- renderText({
    d <- values$events
    d <- d[d$step == "process" & grepl("ok", d$status), , drop = FALSE]
    if (nrow(d) == 0) return("Import: no successful runs")
    sprintf("Import avg: %.1f ms (n=%d)", mean(d$duration_ms), nrow(d))
  })

  output$metric_fit <- renderText({
    d <- values$events
    d <- d[d$step == "fit" & grepl("ok", d$status), , drop = FALSE]
    if (nrow(d) == 0) return("Fit: no successful runs")
    sprintf("Fit avg: %.1f ms (n=%d)", mean(d$duration_ms), nrow(d))
  })

  output$metric_error <- renderText({
    d <- values$events
    if (nrow(d) == 0) return("Error rate: 0% (0/0)")
    err <- sum(d$status == "error")
    sprintf("Error rate: %.1f%% (%d/%d)", 100 * err / nrow(d), err, nrow(d))
  })

  output$events_table <- renderDT({
    datatable(values$events, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
}

shinyApp(ui, server)
