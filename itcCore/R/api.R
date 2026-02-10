build_plot_data <- function(processed_itc, fit_result = NULL, plot_cfg = list()) {
  stopifnot(is.list(processed_itc))

  power_data <- processed_itc$power_corrected
  if (is.null(power_data) && !is.null(processed_itc$raw_data) && !is.null(processed_itc$corrected_power)) {
    power_data <- data.frame(
      Time_s = processed_itc$raw_data$Time,
      Power_corrected_ucal_s = processed_itc$corrected_power
    )
  }

  integration_data <- processed_itc$integration
  simulation_data <- if (!is.null(fit_result) && is.list(fit_result) && !isTRUE(fit_result$cancelled)) fit_result$simulation else NULL

  list(
    power_data = power_data,
    integration_data = integration_data,
    simulation_data = simulation_data,
    params = modifyList(PLOT_DEFAULTS, plot_cfg)
  )
}

update_bundle_with_fit <- function(bundle, fit_result, warnings = character(0)) {
  validate_itc_bundle(bundle)
  if (is.null(fit_result) || isTRUE(fit_result$cancelled)) {
    return(bundle)
  }

  bundle$fit_params <- fit_result$fit_params
  bundle$simulation <- fit_result$simulation

  if (is.null(bundle$audit) || nrow(bundle$audit) == 0) {
    bundle$audit <- data.frame(key = character(0), value = character(0), stringsAsFactors = FALSE)
  }

  add_or_replace <- function(df, key, value) {
    idx <- which(df$key == key)
    if (length(idx) == 0) {
      rbind(df, data.frame(key = key, value = as.character(value), stringsAsFactors = FALSE))
    } else {
      df$value[idx[1]] <- as.character(value)
      df
    }
  }

  bundle$audit <- add_or_replace(bundle$audit, "fitted_at", as.character(Sys.time()))
  bundle$audit <- add_or_replace(bundle$audit, "fit_objective", fit_result$objective %||% NA_real_)
  bundle$audit <- add_or_replace(bundle$audit, "fit_iterations", fit_result$iterations %||% NA_integer_)
  bundle$audit <- add_or_replace(bundle$audit, "warnings", paste(warnings, collapse = "; "))
  bundle
}
