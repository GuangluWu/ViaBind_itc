fail_fast <- function(...) {
  stop(paste0(...), call. = FALSE)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

library(shiny)

detect_repo_root <- function() {
  candidates <- unique(c(getwd(), dirname(getwd())))
  for (d in candidates) {
    ok <- dir.exists(file.path(d, "ITCprocessor")) &&
      dir.exists(file.path(d, "ITCsimfit")) &&
      dir.exists(file.path(d, "ITCgraph"))
    if (isTRUE(ok)) return(normalizePath(d, winslash = "/", mustWork = TRUE))
  }
  fail_fast(
    "Startup check failed: cannot locate repo root. ",
    "Please run from ITCSuiteWeb or repo root."
  )
}

load_legacy_app <- function(app_dir, label, required_symbols = character(0)) {
  app_file <- file.path(app_dir, "app.R")
  if (!dir.exists(app_dir)) {
    fail_fast("Startup check failed: missing legacy dir for ", label, ": ", app_dir)
  }
  if (!file.exists(app_file)) {
    fail_fast("Startup check failed: missing app.R for ", label, ": ", app_file)
  }

  env <- new.env(parent = globalenv())
  env$`%||%` <- `%||%`

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(app_dir)
  sys.source("app.R", envir = env, chdir = TRUE)

  # Legacy apps use source(..., local = FALSE) heavily. Snapshot global symbols
  # into module env so each legacy server has a stable dependency scope.
  for (nm in ls(envir = .GlobalEnv, all.names = TRUE)) {
    if (!exists(nm, envir = env, inherits = FALSE)) {
      try(assign(nm, get(nm, envir = .GlobalEnv, inherits = FALSE), envir = env), silent = TRUE)
    }
  }

  if (!exists("ui", envir = env, inherits = FALSE)) {
    fail_fast("Startup check failed: legacy ", label, " did not define `ui`.")
  }
  if (!exists("server", envir = env, inherits = FALSE)) {
    fail_fast("Startup check failed: legacy ", label, " did not define `server`.")
  }

  ui_obj <- get("ui", envir = env, inherits = FALSE)
  server_fun <- get("server", envir = env, inherits = FALSE)

  if (!is.function(server_fun)) {
    fail_fast("Startup check failed: legacy ", label, " `server` is not a function.")
  }
  # Keep legacy server lookups inside its own captured scope to avoid
  # cross-module symbol pollution when multiple legacy apps are hosted together.
  environment(server_fun) <- env

  if (length(required_symbols) > 0) {
    missing <- required_symbols[!vapply(
      required_symbols,
      function(sym) exists(sym, envir = env, inherits = TRUE),
      logical(1)
    )]
    if (length(missing) > 0) {
      fail_fast(
        "Startup check failed: missing required symbols for ",
        label, ": ", paste(missing, collapse = ", "), "."
      )
    }
  }

  list(ui = ui_obj, server = server_fun, env = env, label = label)
}

bridge_bus_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    step1_payload <- reactiveVal(NULL)
    step2_plot_payload <- reactiveVal(NULL)
    list(
      step1_payload = step1_payload,
      step2_plot_payload = step2_plot_payload
    )
  })
}

bridge_step1_to_step2_server <- function(id, bridge_bus) {
  moduleServer(id, function(input, output, session) {
    latest_token <- reactiveVal(NA_real_)
    observeEvent(bridge_bus$step1_payload(), {
      payload <- bridge_bus$step1_payload()
      if (is.null(payload) || !is.list(payload)) return()
      token <- suppressWarnings(as.numeric(payload$token)[1])
      if (is.finite(token)) latest_token(token)
    }, ignoreNULL = TRUE)
    list(latest_token = latest_token)
  })
}

bridge_step2_to_step3_server <- function(id, bridge_bus) {
  moduleServer(id, function(input, output, session) {
    latest_token <- reactiveVal(NA_real_)
    observeEvent(bridge_bus$step2_plot_payload(), {
      payload <- bridge_bus$step2_plot_payload()
      if (is.null(payload) || !is.list(payload)) return()
      token <- suppressWarnings(as.numeric(payload$token)[1])
      if (is.finite(token)) latest_token(token)
    }, ignoreNULL = TRUE)
    list(latest_token = latest_token)
  })
}

repo_root <- detect_repo_root()
processor_legacy <- load_legacy_app(file.path(repo_root, "ITCprocessor"), "ITCprocessor")
simfit_legacy <- load_legacy_app(
  file.path(repo_root, "ITCsimfit"),
  "ITCsimfit",
  required_symbols = c(
    "calculate_weighted_robust_loss",
    "calculate_weights_from_derivative",
    "huber_loss",
    "calculate_huber_delta"
  )
)
graph_legacy <- load_legacy_app(file.path(repo_root, "ITCgraph"), "ITCgraph")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("\
      .main-host-wrap { margin-top: 6px; }\
      .main-host-wrap .tab-content { padding-top: 8px; }\
    "))
  ),
  div(
    class = "main-host-wrap",
    tabsetPanel(
      id = "main_tabs",
      tabPanel("Step 1 Baseline & Integration", uiOutput("legacy_processor_ui")),
      tabPanel("Step 2 Simulation & Fitting", uiOutput("legacy_simfit_ui")),
      tabPanel("Step 3 Plot & Export", uiOutput("legacy_graph_ui"))
    )
  )
)

server <- function(input, output, session) {
  bridge_bus <- bridge_bus_server("bridge_bus")
  bridge_s1s2 <- bridge_step1_to_step2_server("bridge_s1s2", bridge_bus)
  bridge_s2s3 <- bridge_step2_to_step3_server("bridge_s2s3", bridge_bus)

  session$userData$itcsuite_bridge <- list(
    step1_payload = bridge_bus$step1_payload,
    step2_plot_payload = bridge_bus$step2_plot_payload,
    bridge_s1s2_token = bridge_s1s2$latest_token,
    bridge_s2s3_token = bridge_s2s3$latest_token
  )

  output$legacy_processor_ui <- renderUI({
    processor_legacy$ui
  })

  output$legacy_simfit_ui <- renderUI({
    simfit_legacy$ui
  })

  output$legacy_graph_ui <- renderUI({
    graph_legacy$ui
  })

  processor_legacy$server(input, output, session)
  simfit_legacy$server(input, output, session)
  graph_legacy$server(input, output, session)
}

shinyApp(ui, server)
