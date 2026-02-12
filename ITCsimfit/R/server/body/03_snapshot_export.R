  # ==============================================================================
  # Snapshot + Export Loader
  # ==============================================================================
  snapshot_export_env <- environment()

  source_snapshot_export_chunk <- function(file_name) {
    impl_dir <- tryCatch(get("ITCSIMFIT_SERVER_DIR", inherits = TRUE), error = function(e) NA_character_)
    app_dir <- tryCatch(get("ITCSIMFIT_APP_DIR", inherits = TRUE), error = function(e) NA_character_)

    candidates <- unique(c(
      if (!is.na(impl_dir) && nzchar(impl_dir)) file.path(impl_dir, "body", "snapshot_export", file_name),
      if (!is.na(app_dir) && nzchar(app_dir)) file.path(app_dir, "R", "server", "body", "snapshot_export", file_name),
      file.path("R", "server", "body", "snapshot_export", file_name),
      file.path(getwd(), "R", "server", "body", "snapshot_export", file_name),
      file.path(getwd(), "ITCsimfit", "R", "server", "body", "snapshot_export", file_name),
      file.path(getwd(), "..", "ITCsimfit", "R", "server", "body", "snapshot_export", file_name)
    ))

    hits <- candidates[file.exists(candidates)]
    if (length(hits) < 1) {
      stop(
        sprintf(
          "Cannot find snapshot_export chunk: %s. Tried: %s",
          file_name,
          paste(candidates, collapse = " | ")
        )
      )
    }

    source(hits[[1]], local = snapshot_export_env)
    invisible(TRUE)
  }

  source_snapshot_export_chunk("01_snapshot_management.R")
  source_snapshot_export_chunk("02_export_bridge.R")
