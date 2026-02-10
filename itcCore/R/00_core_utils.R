`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

EPSILON <- 1e-20
EPSILON_LOG <- 1e-15

DEFAULT_PARAMS <- list(
  logK = 7,
  H = -7000,
  fH = 1,
  fG = 1,
  Offset = 0,
  V_init = 0.3
)

PLOT_DEFAULTS <- list(
  top_xlab = "Time (min)",
  top_ylab = "Delta Power (ucal/s)",
  top_line_color = "#D66666",
  top_line_width = 0.5,
  top_time_unit = "min",
  bot_xlab = "Molar Ratio",
  bot_ylab = "Heat of Injection (kcal/mol)",
  heat_offset = 0,
  energy_unit = "cal",
  bot_point_color = "#000000",
  bot_point_shape = 21,
  bot_point_size = 3,
  bot_point_fill = "#D66666",
  bot_point_fill_alpha = 0.7,
  bot_layer_order = "points_over_line",
  bot_dim_first_point = TRUE,
  bot_line_color = "#000000",
  bot_line_width = 0.6,
  bot_line_linetype = "longdash",
  base_size = 12,
  height_ratio = 1,
  border_linewidth = 0.3,
  export_width = 4,
  export_height = 6,
  export_dpi = 300
)

safe_log <- function(x, epsilon = EPSILON_LOG) {
  log(pmax(x, epsilon))
}

is_near_zero <- function(x, tolerance = EPSILON) {
  abs(x) < tolerance
}

get_param_bound <- function(param_name, v_inj = NULL) {
  if (grepl("logK", param_name)) {
    return(c(lower = 0, upper = 9))
  }
  if (grepl("^H[0-9]+$", param_name)) {
    return(c(lower = -15000, upper = 5000))
  }
  if (grepl("fH|fG", param_name)) {
    return(c(lower = 0.5, upper = 1.5))
  }
  if (grepl("Offset", param_name)) {
    return(c(lower = -1500, upper = 1500))
  }
  if (identical(param_name, "V_init")) {
    vv <- if (is.null(v_inj) || is.na(v_inj) || v_inj <= 0) 1.5 else v_inj
    return(c(lower = 0, upper = vv))
  }
  c(lower = -Inf, upper = Inf)
}

get_parameter_bounds <- function(param_names, v_inj = NULL) {
  lower <- numeric(length(param_names))
  upper <- numeric(length(param_names))
  names(lower) <- param_names
  names(upper) <- param_names
  for (i in seq_along(param_names)) {
    b <- get_param_bound(param_names[[i]], v_inj = v_inj)
    lower[[i]] <- b[["lower"]]
    upper[[i]] <- b[["upper"]]
  }
  list(lower = lower, upper = upper)
}

as_numeric_safely <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0) {
    return(default)
  }
  y <- suppressWarnings(as.numeric(x))
  if (length(y) == 0 || is.na(y[1])) {
    return(default)
  }
  y[1]
}

trim_char <- function(x) {
  trimws(as.character(x))
}
