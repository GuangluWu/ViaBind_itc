# ==============================================================================
# 4. Shiny Server
# ==============================================================================
server <- function(input, output, session) {
  server_env <- environment()

  source_server_body <- function(file_name) {
    impl_dir <- tryCatch(get("ITCSIMFIT_SERVER_DIR", inherits = TRUE), error = function(e) NA_character_)
    app_dir <- tryCatch(get("ITCSIMFIT_APP_DIR", inherits = TRUE), error = function(e) NA_character_)

    rel_from_wd <- file.path(getwd(), "R", "server", "body", file_name)
    rel_from_web_host <- file.path(getwd(), "..", "ITCsimfit", "R", "server", "body", file_name)

    candidates <- unique(c(
      if (!is.na(impl_dir) && nzchar(impl_dir)) file.path(impl_dir, "body", file_name),
      if (!is.na(app_dir) && nzchar(app_dir)) file.path(app_dir, "R", "server", "body", file_name),
      file.path("R", "server", "body", file_name),
      rel_from_wd,
      file.path(getwd(), "ITCsimfit", "R", "server", "body", file_name),
      rel_from_web_host
    ))

    hits <- candidates[file.exists(candidates)]
    if (length(hits) < 1) {
      stop(
        sprintf(
          "Cannot find server body file: %s. Tried: %s",
          file_name,
          paste(candidates, collapse = " | ")
        )
      )
    }

    source(hits[[1]], local = server_env)
    invisible(TRUE)
  }

  source_server_body("01_ui_i18n.R")
  source_server_body("02_runtime_core.R")
  source_server_body("03_snapshot_export.R")
}
