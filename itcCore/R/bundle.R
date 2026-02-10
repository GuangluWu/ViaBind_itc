make_meta_sheet <- function(raw_itc = NULL, source_file = NULL, overrides = list()) {
  p <- raw_itc$params %||% list()
  temp_C <- as_numeric_safely(p$temperature_C, NA_real_)
  meta <- data.frame(
    parameter = c(
      "schema_version", "source_file", "original_itc_file", "Temp_K",
      "G_syringe_mM", "H_cell_0_mM", "V_pre_uL", "V_inj_uL", "n_inj", "V_cell_mL"
    ),
    value = c(
      "itc_bundle_v1",
      source_file %||% "",
      source_file %||% "",
      if (!is.na(temp_C)) temp_C + 273.15 else NA_real_,
      as_numeric_safely(p$syringe_conc_mM, NA_real_),
      as_numeric_safely(p$cell_conc_mM, NA_real_),
      as_numeric_safely(p$V_pre_ul, NA_real_),
      as_numeric_safely(p$V_inj_ul, NA_real_),
      as_numeric_safely(p$n_injections, NA_real_),
      as_numeric_safely(p$cell_volume_mL, NA_real_)
    ),
    stringsAsFactors = FALSE
  )

  if (length(overrides) > 0) {
    for (nm in names(overrides)) {
      idx <- which(meta$parameter == nm)
      if (length(idx) == 0) {
        meta <- rbind(meta, data.frame(parameter = nm, value = as.character(overrides[[nm]]), stringsAsFactors = FALSE))
      } else {
        meta$value[idx[1]] <- as.character(overrides[[nm]])
      }
    }
  }

  meta
}

process_itc <- function(raw_itc, baseline_cfg = list(), integration_cfg = list()) {
  stopifnot(is.list(raw_itc), !is.null(raw_itc$data), !is.null(raw_itc$injections))

  df <- raw_itc$data
  injections <- raw_itc$injections
  inj_times <- raw_itc$injection_times

  baseline_duration <- baseline_cfg$baseline_duration %||% baseline_cfg$duration %||% 20
  baseline_offset <- baseline_cfg$baseline_offset %||% baseline_cfg$offset %||% 5
  spar <- baseline_cfg$spar %||% 0.1

  base <- SegmentedBaseline(
    time = df$Time,
    power = df$Power,
    injection_indices = injections,
    injection_times = inj_times,
    baseline_duration = baseline_duration,
    baseline_offset = baseline_offset,
    spar = spar
  )

  corrected <- df$Power - base

  limit_integration <- isTRUE(integration_cfg$limit_integration %||% integration_cfg$limit)
  integration_window <- if (limit_integration) integration_cfg$integration_window %||% integration_cfg$window else NULL
  start_offset <- integration_cfg$start_offset %||% 0

  int_res <- integrate_peaks(
    time = df$Time,
    corrected_power = corrected,
    injection_indices = injections,
    integration_window = integration_window,
    start_offset = start_offset
  )

  include_injection0 <- isTRUE(integration_cfg$include_injection0)
  if (!include_injection0) {
    int_res <- int_res[int_res$Injection > 0, , drop = FALSE]
  }

  vol_ul <- raw_itc$injection_volumes_ul
  if (is.null(vol_ul)) {
    vol_ul <- rep(NA_real_, length(raw_itc$injections %||% integer(0)))
  }

  int_res$V_titrate_uL <- NA_real_
  if (nrow(int_res) > 0) {
    for (r in seq_len(nrow(int_res))) {
      j <- int_res$Injection[r]
      if (j + 1L <= length(vol_ul)) {
        int_res$V_titrate_uL[r] <- vol_ul[j + 1L]
      }
    }
  }

  syringe_mM <- integration_cfg$syringe_mM %||% raw_itc$params$syringe_conc_mM
  denom <- int_res$V_titrate_uL * syringe_mM
  int_res$heat_cal_mol <- ifelse(!is.na(denom) & denom > 0, 1000 * int_res$Heat_ucal / denom, NA_real_)

  V_cell <- raw_itc$params$cell_volume_mL
  H_0 <- integration_cfg$cell_mM %||% raw_itc$params$cell_conc_mM
  G_0 <- integration_cfg$syringe_mM %||% raw_itc$params$syringe_conc_mM

  V_titrate_mL <- int_res$V_titrate_uL / 1000
  H_app <- rep(NA_real_, nrow(int_res))
  G_app <- rep(NA_real_, nrow(int_res))
  if (!is.na(V_cell) && V_cell > 0 && !is.na(H_0) && !is.na(G_0) && nrow(int_res) > 0) {
    for (r in seq_len(nrow(int_res))) {
      v <- V_titrate_mL[r]
      if (is.na(v)) next
      if (r == 1L) {
        H_app[r] <- H_0 * (V_cell - v) / V_cell
        G_app[r] <- G_0 * v / V_cell
      } else {
        H_app[r] <- H_app[r - 1L] * (V_cell - v) / V_cell
        G_app[r] <- G_app[r - 1L] * (V_cell - v) / V_cell + G_0 * v / V_cell
      }
    }
  }

  int_res$H_app_mM <- H_app
  int_res$G_app_mM <- G_app
  int_res$Ratio_App <- ifelse(!is.na(H_app) & H_app != 0, G_app / H_app, NA_real_)

  list(
    raw_data = df,
    corrected_power = corrected,
    power_corrected = data.frame(Time_s = df$Time, Power_corrected_ucal_s = corrected),
    integration = int_res,
    baseline = base,
    baseline_cfg = list(baseline_duration = baseline_duration, baseline_offset = baseline_offset, spar = spar),
    integration_cfg = list(
      limit_integration = limit_integration,
      integration_window = integration_window,
      start_offset = start_offset,
      include_injection0 = include_injection0
    )
  )
}

create_bundle_from_processed <- function(raw_itc, processed_itc, source_file = NULL) {
  stopifnot(is.list(raw_itc), is.list(processed_itc))

  meta <- make_meta_sheet(raw_itc = raw_itc, source_file = source_file)
  power_original <- data.frame(
    Time_s = raw_itc$data$Time,
    Power_original_ucal_s = raw_itc$data$Power
  )
  power_corrected <- data.frame(
    Time_s = raw_itc$data$Time,
    Power_corrected_ucal_s = processed_itc$corrected_power
  )

  fit_params <- data.frame(parameter = character(0), value = character(0), stringsAsFactors = FALSE)
  simulation <- data.frame(
    Inj = integer(0),
    Ratio_App = numeric(0),
    dQ_App = numeric(0),
    stringsAsFactors = FALSE
  )
  audit <- data.frame(
    key = c("algorithm_version", "created_at", "warnings"),
    value = c("itcCore-0.1.0", as.character(Sys.time()), ""),
    stringsAsFactors = FALSE
  )

  bundle <- list(
    schema_version = "itc_bundle_v1",
    meta = meta,
    power_original = power_original,
    power_corrected = power_corrected,
    integration = processed_itc$integration,
    fit_params = fit_params,
    simulation = simulation,
    audit = audit
  )
  class(bundle) <- c("itc_bundle", class(bundle))
  bundle
}

validate_itc_bundle <- function(bundle) {
  required <- c("schema_version", "meta", "power_corrected", "integration", "fit_params", "simulation", "audit")
  if (!is.list(bundle)) {
    stop("bundle must be a list")
  }
  missing <- setdiff(required, names(bundle))
  if (length(missing) > 0) {
    stop(sprintf("bundle missing required fields: %s", paste(missing, collapse = ", ")))
  }
  if (!identical(bundle$schema_version, "itc_bundle_v1")) {
    stop("unsupported schema_version")
  }
  if (!all(c("parameter", "value") %in% names(bundle$meta))) {
    stop("meta must contain columns: parameter, value")
  }
  if (!all(c("Time_s", "Power_corrected_ucal_s") %in% names(bundle$power_corrected))) {
    stop("power_corrected must contain columns: Time_s, Power_corrected_ucal_s")
  }
  if (!is.null(bundle$power_original) &&
      !all(c("Time_s", "Power_original_ucal_s") %in% names(bundle$power_original))) {
    stop("power_original must contain columns: Time_s, Power_original_ucal_s")
  }
  if (nrow(bundle$integration) > 0 && !all(c("Ratio_App", "heat_cal_mol") %in% names(bundle$integration))) {
    stop("integration must contain columns: Ratio_App, heat_cal_mol")
  }
  TRUE
}

meta_lookup <- function(meta_df, key, default = NA_real_) {
  if (is.null(meta_df) || !all(c("parameter", "value") %in% names(meta_df))) {
    return(default)
  }
  idx <- which(trim_char(meta_df$parameter) == key)
  if (length(idx) == 0) return(default)
  as_numeric_safely(meta_df$value[idx[1]], default)
}

normalize_integration_sheet <- function(int_df, meta_df) {
  if (is.null(int_df) || nrow(int_df) == 0) {
    return(data.frame(
      Injection = integer(0),
      Ratio_App = numeric(0),
      heat_cal_mol = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  if (all(c("Ratio_App", "heat_cal_mol") %in% names(int_df))) {
    return(as.data.frame(int_df))
  }

  if (all(c("Heat_ucal", "V_titrate_uL") %in% names(int_df))) {
    g <- meta_lookup(meta_df, "G_syringe_mM", NA_real_)
    denom <- as.numeric(int_df$V_titrate_uL) * g
    heat_cal_mol <- ifelse(!is.na(denom) & denom > 0, 1000 * as.numeric(int_df$Heat_ucal) / denom, NA_real_)

    ratio <- if ("Ratio_App" %in% names(int_df)) as.numeric(int_df$Ratio_App) else NA_real_
    if (all(is.na(ratio)) && all(c("H_app_mM", "G_app_mM") %in% names(int_df))) {
      ratio <- as.numeric(int_df$G_app_mM) / as.numeric(int_df$H_app_mM)
    }

    out <- as.data.frame(int_df)
    out$heat_cal_mol <- heat_cal_mol
    out$Ratio_App <- ratio
    return(out)
  }

  if (all(c("Ratio_App", "dQ_App") %in% names(int_df))) {
    out <- as.data.frame(int_df)
    out$heat_cal_mol <- as.numeric(out$dQ_App)
    return(out)
  }

  out <- as.data.frame(int_df)
  if (!("Ratio_App" %in% names(out))) out$Ratio_App <- NA_real_
  if (!("heat_cal_mol" %in% names(out))) out$heat_cal_mol <- NA_real_
  out
}

export_bundle <- function(bundle, path, format = "xlsx", compat_legacy = FALSE) {
  validate_itc_bundle(bundle)
  if (!identical(format, "xlsx")) {
    stop("only xlsx format is supported")
  }

  power_original <- bundle$power_original
  if (is.null(power_original)) {
    power_original <- data.frame(
      Time_s = bundle$power_corrected$Time_s,
      Power_original_ucal_s = NA_real_
    )
  }

  sheet_list <- list(
    power_original = power_original,
    meta = bundle$meta,
    power_corrected = bundle$power_corrected,
    integration = bundle$integration,
    fit_params = bundle$fit_params,
    simulation = bundle$simulation,
    audit = bundle$audit
  )

  if (compat_legacy) {
    sheet_list$integration_rev <- bundle$integration
    sheet_list$meta_rev <- bundle$meta
  }

  writexl::write_xlsx(sheet_list, path = path)
  invisible(path)
}

import_bundle <- function(path) {
  sheets <- readxl::excel_sheets(path)
  read_sheet <- function(nm) {
    if (!nm %in% sheets) return(NULL)
    as.data.frame(readxl::read_excel(path, sheet = nm))
  }

  meta <- read_sheet("meta")
  if (is.null(meta)) meta <- read_sheet("meta_rev")
  if (is.null(meta)) {
    meta <- data.frame(parameter = c("schema_version"), value = c("itc_bundle_v1"), stringsAsFactors = FALSE)
  }

  power <- read_sheet("power_corrected")
  if (is.null(power)) {
    power <- data.frame(Time_s = numeric(0), Power_corrected_ucal_s = numeric(0))
  }
  power_original <- read_sheet("power_original")

  integration <- read_sheet("integration")
  if (is.null(integration)) integration <- read_sheet("integration_rev")
  integration <- normalize_integration_sheet(integration, meta)

  fit_params <- read_sheet("fit_params")
  if (is.null(fit_params)) {
    fit_params <- data.frame(parameter = character(0), value = character(0), stringsAsFactors = FALSE)
  }
  simulation <- read_sheet("simulation")
  if (is.null(simulation)) {
    simulation <- data.frame(
      Inj = integer(0),
      Ratio_App = numeric(0),
      dQ_App = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  audit <- read_sheet("audit")
  if (is.null(audit)) {
    audit <- data.frame(
      key = c("algorithm_version", "imported_at", "warnings"),
      value = c("unknown", as.character(Sys.time()), "Imported from legacy format"),
      stringsAsFactors = FALSE
    )
  }

  bundle <- list(
    schema_version = "itc_bundle_v1",
    meta = meta,
    power_original = power_original,
    power_corrected = power,
    integration = integration,
    fit_params = fit_params,
    simulation = simulation,
    audit = audit
  )
  class(bundle) <- c("itc_bundle", class(bundle))
  validate_itc_bundle(bundle)
  bundle
}
