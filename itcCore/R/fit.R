default_fit_config <- function(processed_itc) {
  list(
    active_paths = c("rxn_M"),
    params_to_opt = c("logK1", "H1"),
    initial_params = list(
      logK1 = 7,
      H1 = -7000,
      fH = 1,
      fG = 1,
      Offset = 0,
      V_init = NA_real_
    ),
    fixed_params = list(),
    range_lim = c(1L, max(1L, nrow(processed_itc$integration))),
    max_iter = 100,
    method = "L-BFGS-B"
  )
}

build_fixed_fit_params <- function(processed_itc, fit_cfg) {
  int_df <- processed_itc$integration
  meta <- processed_itc$meta %||% data.frame(parameter = character(0), value = character(0), stringsAsFactors = FALSE)
  lookup <- function(key, default = NA_real_) meta_lookup(meta, key, default)

  v_inj_vec <- if ("V_titrate_uL" %in% names(int_df) && nrow(int_df) > 0) as.numeric(int_df$V_titrate_uL) else NA_real_
  if (length(v_inj_vec) == 1 && is.na(v_inj_vec)) v_inj_vec <- rep(lookup("V_inj_uL", 1.5), max(1, nrow(int_df)))
  if (any(is.na(v_inj_vec))) v_inj_vec[is.na(v_inj_vec)] <- lookup("V_inj_uL", 1.5)

  defaults <- list(
    H_cell_0 = lookup("H_cell_0_mM", 0.03),
    G_syringe = lookup("G_syringe_mM", 0.6),
    V_cell = lookup("V_cell_mL", 0.2033),
    V_inj_vec = v_inj_vec,
    n_inj = length(v_inj_vec),
    logK1 = 7,
    H1 = -7000,
    logK2 = 7,
    H2 = -7000,
    logK3 = 7,
    H3 = -7000,
    logK4 = 7,
    H4 = -7000,
    logK5 = 7,
    H5 = -7000,
    logK6 = 7,
    H6 = -7000,
    fH = 1,
    fG = 1,
    Offset = 0,
    V_init = if (length(v_inj_vec) > 0) v_inj_vec[1] else 1.5
  )

  modifyList(defaults, fit_cfg$fixed_params %||% list())
}

fit_itc <- function(processed_itc, fit_cfg = list(), progress_cb = NULL, is_cancelled = NULL) {
  stopifnot(is.list(processed_itc), !is.null(processed_itc$integration))

  cfg <- modifyList(default_fit_config(processed_itc), fit_cfg)
  int_df <- processed_itc$integration
  if (!all(c("heat_cal_mol") %in% names(int_df))) {
    stop("processed_itc$integration must contain heat_cal_mol")
  }

  fixed <- build_fixed_fit_params(processed_itc, cfg)
  initials <- modifyList(cfg$initial_params %||% list(), as.list(fixed))

  params_to_opt <- cfg$params_to_opt %||% c("logK1", "H1")
  params_to_opt <- unique(params_to_opt)
  if (length(params_to_opt) == 0) {
    stop("fit_cfg$params_to_opt must not be empty")
  }

  par_vec <- unlist(initials[params_to_opt])
  names(par_vec) <- params_to_opt

  if ("V_init" %in% names(par_vec) && is.na(par_vec[["V_init"]])) {
    par_vec[["V_init"]] <- fixed$V_inj_vec[1]
  }

  range_lim <- as.integer(cfg$range_lim %||% c(1, nrow(int_df)))
  if (length(range_lim) != 2) range_lim <- c(1L, nrow(int_df))
  range_lim <- c(max(1L, range_lim[1]), min(nrow(int_df), range_lim[2]))

  if (range_lim[1] > range_lim[2]) {
    stop("Invalid range_lim")
  }

  max_idx <- min(nrow(int_df), fixed$n_inj)
  valid_idx <- seq.int(range_lim[1], min(range_lim[2], max_idx))
  if (length(valid_idx) == 0) {
    stop("No overlapping data points in fit range")
  }

  eval_count <- 0L
  start_time <- Sys.time()

  obj_fun <- function(par) {
    eval_count <<- eval_count + 1L
    if (!is.null(is_cancelled) && isTRUE(is_cancelled())) {
      stop("FIT_CANCELLED")
    }

    current <- as.list(fixed)
    current[names(par)] <- as.list(par)
    if (!is.null(current$V_inj_vec) && !is.null(current$V_init) && length(current$V_inj_vec) > 0) {
      current$V_inj_vec[1] <- current$V_init
    }

    sim <- tryCatch(calculate_simulation(current, cfg$active_paths), error = function(e) NULL)
    if (is.null(sim) || any(!is.finite(sim$dQ_App))) return(1e20)

    max_i <- min(nrow(sim), nrow(int_df))
    idx <- valid_idx[valid_idx <= max_i]
    if (length(idx) == 0) return(1e20)

    residuals <- sim$dQ_App[idx] - int_df$heat_cal_mol[idx]
    rss <- sum(residuals^2)

    if (!is.null(progress_cb) && eval_count %% 5L == 0L) {
      progress_cb(min(0.9, eval_count / (cfg$max_iter * 8)), sprintf("Evaluating (%d)", eval_count))
    }

    rss
  }

  bounds <- get_parameter_bounds(names(par_vec), v_inj = fixed$V_inj_vec[1])

  if (!is.null(progress_cb)) progress_cb(0.05, "Starting optimizer")
  res <- tryCatch({
    optim(
      par = par_vec,
      fn = obj_fun,
      method = cfg$method %||% "L-BFGS-B",
      lower = bounds$lower,
      upper = bounds$upper,
      control = list(maxit = cfg$max_iter %||% 100, factr = 1e7, pgtol = 1e-5)
    )
  }, error = function(e) {
    if (identical(conditionMessage(e), "FIT_CANCELLED")) {
      structure(list(cancelled = TRUE), class = "fit_cancelled")
    } else {
      stop(e)
    }
  })

  if (inherits(res, "fit_cancelled")) {
    return(list(cancelled = TRUE, message = "Fit cancelled by user"))
  }

  fitted_values <- as.list(fixed)
  fitted_values[names(res$par)] <- as.list(res$par)
  if (!is.null(fitted_values$V_inj_vec) && !is.null(fitted_values$V_init) && length(fitted_values$V_inj_vec) > 0) {
    fitted_values$V_inj_vec[1] <- fitted_values$V_init
  }

  sim_res <- calculate_simulation(fitted_values, cfg$active_paths)
  max_i <- min(nrow(sim_res), nrow(int_df))
  idx <- valid_idx[valid_idx <= max_i]
  residuals_df <- data.frame(
    Inj = idx,
    Observed = int_df$heat_cal_mol[idx],
    Fitted = sim_res$dQ_App[idx],
    Residual = int_df$heat_cal_mol[idx] - sim_res$dQ_App[idx],
    Ratio_App = sim_res$Ratio_App[idx],
    stringsAsFactors = FALSE
  )

  fit_params <- data.frame(
    parameter = names(fitted_values),
    value = vapply(fitted_values, function(x) {
      if (length(x) == 1) as.character(x) else paste(x, collapse = ",")
    }, character(1)),
    stringsAsFactors = FALSE
  )

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  if (!is.null(progress_cb)) progress_cb(1, "Fit complete")

  list(
    cancelled = FALSE,
    method = cfg$method,
    objective = res$value,
    convergence = res$convergence,
    message = res$message %||% "",
    iterations = eval_count,
    elapsed_seconds = elapsed,
    parameters = fitted_values,
    fit_params = fit_params,
    simulation = sim_res,
    residuals = residuals_df
  )
}
