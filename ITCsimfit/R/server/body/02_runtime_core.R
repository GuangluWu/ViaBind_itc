  # ==============================================================================
  # Runtime Core Loader
  # ==============================================================================
  runtime_core_env <- environment()

  source_runtime_core_chunk <- function(file_name) {
    impl_dir <- tryCatch(get("ITCSIMFIT_SERVER_DIR", inherits = TRUE), error = function(e) NA_character_)
    app_dir <- tryCatch(get("ITCSIMFIT_APP_DIR", inherits = TRUE), error = function(e) NA_character_)

    candidates <- unique(c(
      if (!is.na(impl_dir) && nzchar(impl_dir)) file.path(impl_dir, "body", "runtime_core", file_name),
      if (!is.na(app_dir) && nzchar(app_dir)) file.path(app_dir, "R", "server", "body", "runtime_core", file_name),
      file.path("R", "server", "body", "runtime_core", file_name),
      file.path(getwd(), "R", "server", "body", "runtime_core", file_name),
      file.path(getwd(), "ITCsimfit", "R", "server", "body", "runtime_core", file_name),
      file.path(getwd(), "..", "ITCsimfit", "R", "server", "body", "runtime_core", file_name)
    ))

    hits <- candidates[file.exists(candidates)]
    if (length(hits) < 1) {
      stop(
        sprintf(
          "Cannot find runtime core chunk: %s. Tried: %s",
          file_name,
          paste(candidates, collapse = " | ")
        )
      )
    }

    source(hits[[1]], local = runtime_core_env)
    invisible(TRUE)
  }

  source_runtime_core_chunk("01_bridge_state_inputs.R")
  source_runtime_core_chunk("02_simulation_fitting.R")
  source_runtime_core_chunk("03_plots_diagnostics.R")
