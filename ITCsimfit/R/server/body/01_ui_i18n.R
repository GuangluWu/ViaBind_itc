  # ==============================================================================
  # UI + i18n Loader
  # ==============================================================================
  ui_i18n_env <- environment()

  source_ui_i18n_chunk <- function(file_name) {
    impl_dir <- tryCatch(get("ITCSIMFIT_SERVER_DIR", inherits = TRUE), error = function(e) NA_character_)
    app_dir <- tryCatch(get("ITCSIMFIT_APP_DIR", inherits = TRUE), error = function(e) NA_character_)

    candidates <- unique(c(
      if (!is.na(impl_dir) && nzchar(impl_dir)) file.path(impl_dir, "body", "ui_i18n", file_name),
      if (!is.na(app_dir) && nzchar(app_dir)) file.path(app_dir, "R", "server", "body", "ui_i18n", file_name),
      file.path("R", "server", "body", "ui_i18n", file_name),
      file.path(getwd(), "R", "server", "body", "ui_i18n", file_name),
      file.path(getwd(), "ITCsimfit", "R", "server", "body", "ui_i18n", file_name),
      file.path(getwd(), "..", "ITCsimfit", "R", "server", "body", "ui_i18n", file_name)
    ))

    hits <- candidates[file.exists(candidates)]
    if (length(hits) < 1) {
      stop(
        sprintf(
          "Cannot find ui_i18n chunk: %s. Tried: %s",
          file_name,
          paste(candidates, collapse = " | ")
        )
      )
    }

    source(hits[[1]], local = ui_i18n_env)
    invisible(TRUE)
  }

  source_ui_i18n_chunk("01_bridge_i18n_setup.R")
  source_ui_i18n_chunk("02_ui_outputs_report.R")
