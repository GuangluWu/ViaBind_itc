get_step1_payload_bundle <- function(payload) {
  if (is.null(payload) || !is.list(payload)) return(list())
  bundle <- payload$bundle
  if (is.null(bundle) || !is.list(bundle)) return(list())
  bundle
}

get_step1_payload_meta_df <- function(payload) {
  bundle <- get_step1_payload_bundle(payload)
  if (is.data.frame(bundle$meta)) return(as.data.frame(bundle$meta))
  if (is.data.frame(payload$meta)) return(as.data.frame(payload$meta))
  NULL
}

get_step1_payload_integration_df <- function(payload) {
  bundle <- get_step1_payload_bundle(payload)
  if (is.data.frame(bundle$integration)) return(as.data.frame(bundle$integration))
  if (is.data.frame(payload$integration)) return(as.data.frame(payload$integration))
  NULL
}

get_step1_payload_power_corrected_df <- function(payload) {
  bundle <- get_step1_payload_bundle(payload)
  if (is.data.frame(bundle$power_corrected)) return(as.data.frame(bundle$power_corrected))
  NULL
}

get_step1_payload_power_original_df <- function(payload) {
  bundle <- get_step1_payload_bundle(payload)
  if (is.data.frame(bundle$power_original)) return(as.data.frame(bundle$power_original))
  NULL
}

extract_step1_meta_numeric_map <- function(meta_df) {
  if (is.null(meta_df) || !is.data.frame(meta_df)) return(numeric(0))
  if (!all(c("parameter", "value") %in% colnames(meta_df))) return(numeric(0))
  vals <- suppressWarnings(as.numeric(trimws(as.character(meta_df$value))))
  setNames(vals, trimws(as.character(meta_df$parameter)))
}

extract_meta_numeric_map_with_rev_priority <- function(meta_rev_df, meta_df) {
  base_map <- extract_step1_meta_numeric_map(meta_df)
  rev_map <- extract_step1_meta_numeric_map(meta_rev_df)
  if (length(rev_map) == 0) return(base_map)

  merged <- base_map
  for (nm in names(rev_map)) {
    if (!is.na(rev_map[[nm]])) {
      merged[[nm]] <- rev_map[[nm]]
    }
  }
  merged
}

extract_fit_params_map <- function(fit_params_df) {
  if (is.null(fit_params_df) || !is.data.frame(fit_params_df)) return(NULL)
  if (!all(c("parameter", "value") %in% colnames(fit_params_df))) return(NULL)
  param_names <- trimws(as.character(fit_params_df$parameter))
  param_values <- trimws(as.character(fit_params_df$value))
  setNames(param_values, param_names)
}

get_fit_param_num <- function(fp_map, key, default = NA_real_) {
  if (is.null(fp_map) || length(fp_map) == 0) return(default)
  nm <- names(fp_map)
  idx <- match(tolower(as.character(key)[1]), tolower(nm))
  if (is.na(idx)) return(default)
  suppressWarnings({
    v <- as.numeric(fp_map[[idx]])
    if (length(v) < 1 || !is.finite(v[1])) default else v[1]
  })
}

parse_active_paths_from_fit_params <- function(fp_map) {
  raw_paths <- ""
  if (!is.null(fp_map) && length(fp_map) > 0) {
    idx <- match("activepaths", tolower(names(fp_map)))
    if (!is.na(idx)) raw_paths <- fp_map[[idx]]
  }
  if (is.null(raw_paths) || length(raw_paths) == 0) return(character(0))
  raw_chr <- trimws(as.character(raw_paths)[1])
  if (!nzchar(raw_chr)) return(character(0))

  tokens <- trimws(unlist(strsplit(raw_chr, ",", fixed = TRUE), use.names = FALSE))
  tokens <- tokens[nzchar(tokens)]
  tokens <- unique(tokens)
  valid_paths <- c("rxn_D", "rxn_T", "rxn_B", "rxn_F", "rxn_U")
  intersect(tokens, valid_paths)
}

extract_simfit_restore_params <- function(fp_map) {
  list(
    logK1 = get_fit_param_num(fp_map, "logK1"),
    H1 = get_fit_param_num(fp_map, "H1_cal_mol"),
    logK2 = get_fit_param_num(fp_map, "logK2"),
    H2 = get_fit_param_num(fp_map, "H2_cal_mol"),
    logK3 = get_fit_param_num(fp_map, "logK3"),
    H3 = get_fit_param_num(fp_map, "H3_cal_mol"),
    logK4 = get_fit_param_num(fp_map, "logK4"),
    H4 = get_fit_param_num(fp_map, "H4_cal_mol"),
    logK5 = get_fit_param_num(fp_map, "logK5"),
    H5 = get_fit_param_num(fp_map, "H5_cal_mol"),
    logK6 = get_fit_param_num(fp_map, "logK6"),
    H6 = get_fit_param_num(fp_map, "H6_cal_mol"),
    fH = get_fit_param_num(fp_map, "fH"),
    fG = get_fit_param_num(fp_map, "fG"),
    V_init_uL = get_fit_param_num(fp_map, "V_init_uL"),
    Offset_cal = get_fit_param_num(fp_map, "Offset_cal"),
    H_cell_0_mM = get_fit_param_num(fp_map, "H_cell_0_mM"),
    G_syringe_mM = get_fit_param_num(fp_map, "G_syringe_mM"),
    V_cell_mL = get_fit_param_num(fp_map, "V_cell_mL"),
    V_inj_uL = get_fit_param_num(fp_map, "V_inj_uL"),
    n_inj = get_fit_param_num(fp_map, "n_inj"),
    V_pre_uL = get_fit_param_num(fp_map, "V_pre_uL"),
    Temp_K = get_fit_param_num(fp_map, "Temp_K")
  )
}

get_preferred_integration_sheet <- function(sheets) {
  if (is.null(sheets) || !is.list(sheets)) return(NULL)
  if ("integration_rev" %in% names(sheets) && is.data.frame(sheets[["integration_rev"]])) {
    return(as.data.frame(sheets[["integration_rev"]]))
  }
  if ("integration" %in% names(sheets) && is.data.frame(sheets[["integration"]])) {
    return(as.data.frame(sheets[["integration"]]))
  }
  NULL
}

build_current_vinj_ul <- function(n, v_pre, v_inj) {
  n_num <- suppressWarnings(as.integer(n)[1])
  if (!is.finite(n_num) || n_num <= 0) return(numeric(0))

  v_pre_num <- suppressWarnings(as.numeric(v_pre)[1])
  v_inj_num <- suppressWarnings(as.numeric(v_inj)[1])
  if (!is.finite(v_inj_num)) v_inj_num <- NA_real_
  if (!is.finite(v_pre_num)) v_pre_num <- v_inj_num

  vinj_vec <- rep(v_inj_num, n_num)
  vinj_vec[1] <- v_pre_num
  vinj_vec
}

calc_heat_cal_mol_from_ucal <- function(heat_ucal, vinj_ul, g_syringe, fallback_heat_cal_mol = NULL) {
  heat_ucal_num <- suppressWarnings(as.numeric(heat_ucal))
  vinj_ul_num <- suppressWarnings(as.numeric(vinj_ul))
  g_syringe_num <- suppressWarnings(as.numeric(g_syringe)[1])
  denom <- vinj_ul_num * g_syringe_num
  heat_from_ucal <- ifelse(
    is.finite(heat_ucal_num) & is.finite(denom) & denom > 0,
    1000 * heat_ucal_num / denom,
    NA_real_
  )
  if (is.null(fallback_heat_cal_mol)) return(heat_from_ucal)

  fallback_num <- suppressWarnings(as.numeric(fallback_heat_cal_mol))
  if (length(fallback_num) == 1 && length(heat_from_ucal) > 1) {
    fallback_num <- rep(fallback_num, length(heat_from_ucal))
  }
  out <- heat_from_ucal
  use_fallback <- !is.finite(out) & is.finite(fallback_num)
  out[use_fallback] <- fallback_num[use_fallback]
  out
}

calc_heat_ucal_from_cal_mol <- function(heat_cal_mol, vinj_ul, g_syringe) {
  heat_cal_mol_num <- suppressWarnings(as.numeric(heat_cal_mol))
  vinj_ul_num <- suppressWarnings(as.numeric(vinj_ul))
  g_syringe_num <- suppressWarnings(as.numeric(g_syringe)[1])
  if (!is.finite(g_syringe_num) || g_syringe_num <= 0) {
    return(rep(NA_real_, length(heat_cal_mol_num)))
  }
  ifelse(
    is.finite(heat_cal_mol_num) & is.finite(vinj_ul_num) & vinj_ul_num > 0,
    heat_cal_mol_num * vinj_ul_num * g_syringe_num / 1000,
    NA_real_
  )
}

build_sim_to_exp_exp_df <- function(sim_df, v_pre, v_inj, g_syringe) {
  if (is.null(sim_df) || !is.data.frame(sim_df) || nrow(sim_df) == 0) return(NULL)
  if (!("dQ_App" %in% names(sim_df))) return(NULL)

  n <- nrow(sim_df)
  V_inj_uL <- build_current_vinj_ul(n = n, v_pre = v_pre, v_inj = v_inj)
  Heat_Raw <- suppressWarnings(as.numeric(sim_df$dQ_App))
  Heat_ucal <- calc_heat_ucal_from_cal_mol(
    heat_cal_mol = Heat_Raw,
    vinj_ul = V_inj_uL,
    g_syringe = g_syringe
  )
  Ratio_Raw <- if ("Ratio_App" %in% names(sim_df)) suppressWarnings(as.numeric(sim_df$Ratio_App)) else rep(NA_real_, n)

  exp_df <- data.frame(
    Ratio_Raw = Ratio_Raw,
    Heat_Raw = Heat_Raw,
    Heat_ucal = Heat_ucal,
    V_inj_uL = V_inj_uL,
    Inj = if ("Inj" %in% names(sim_df)) suppressWarnings(as.integer(sim_df$Inj)) else seq_len(n),
    stringsAsFactors = FALSE
  )
  exp_df <- exp_df[is.finite(exp_df$Heat_Raw), , drop = FALSE]
  if (nrow(exp_df) == 0) return(NULL)
  exp_df$Inj <- seq_len(nrow(exp_df))
  exp_df
}

build_step2_exp_df_from_integration <- function(int_df, v_pre, v_inj, g_syringe, prefer_heat_ucal = TRUE) {
  if (is.null(int_df) || !is.data.frame(int_df) || nrow(int_df) == 0) return(NULL)

  has_heat_cal_mol <- "heat_cal_mol" %in% colnames(int_df)
  has_heat_ucal <- "Heat_ucal" %in% colnames(int_df)
  if (!has_heat_cal_mol && !has_heat_ucal) return(NULL)

  n <- nrow(int_df)
  V_inj_uL <- build_current_vinj_ul(n = n, v_pre = v_pre, v_inj = v_inj)
  fallback_heat <- if (has_heat_cal_mol) as.numeric(int_df$heat_cal_mol) else rep(NA_real_, n)
  heat_ucal <- if (has_heat_ucal) as.numeric(int_df$Heat_ucal) else rep(NA_real_, n)
  heat_raw <- if (isTRUE(prefer_heat_ucal) && has_heat_ucal) {
    calc_heat_cal_mol_from_ucal(
      heat_ucal = heat_ucal,
      vinj_ul = V_inj_uL,
      g_syringe = g_syringe,
      fallback_heat_cal_mol = fallback_heat
    )
  } else if (has_heat_cal_mol) {
    fallback_heat
  } else {
    calc_heat_cal_mol_from_ucal(
      heat_ucal = heat_ucal,
      vinj_ul = V_inj_uL,
      g_syringe = g_syringe
    )
  }

  Ratio_Raw <- if ("Ratio_App" %in% colnames(int_df)) as.numeric(int_df$Ratio_App) else rep(NA_real_, n)
  exp_df <- data.frame(
    Ratio_Raw = Ratio_Raw,
    Heat_Raw = heat_raw,
    V_inj_uL = V_inj_uL,
    Heat_ucal = heat_ucal,
    Inj = seq_len(n),
    stringsAsFactors = FALSE
  )
  exp_df <- exp_df[is.finite(exp_df$Heat_Raw), , drop = FALSE]
  if (nrow(exp_df) == 0) return(NULL)
  exp_df$Inj <- seq_len(nrow(exp_df))
  exp_df
}

build_step1_bridge_exp_df <- function(int_df, vinj_default, g_syringe, v_pre = vinj_default) {
  if (is.null(int_df) || !is.data.frame(int_df) || nrow(int_df) == 0) return(NULL)

  vinj_default_num <- suppressWarnings(as.numeric(vinj_default)[1])
  if (!is.finite(vinj_default_num)) vinj_default_num <- NA_real_

  g_syringe_num <- suppressWarnings(as.numeric(g_syringe)[1])
  if (!is.finite(g_syringe_num)) g_syringe_num <- NA_real_
  v_pre_num <- suppressWarnings(as.numeric(v_pre)[1])
  if (!is.finite(v_pre_num)) v_pre_num <- vinj_default_num
  build_step2_exp_df_from_integration(
    int_df = int_df,
    v_pre = v_pre_num,
    v_inj = vinj_default_num,
    g_syringe = g_syringe_num,
    prefer_heat_ucal = TRUE
  )
}

resolve_step1_bridge_source_name <- function(payload, default = "Step1") {
  src <- as.character(payload$source)
  if (length(src) == 0 || !nzchar(src[1])) return(default)
  src[1]
}

resolve_step1_bridge_token_tag <- function(token) {
  tok <- suppressWarnings(as.numeric(token)[1])
  if (is.finite(tok)) return(format(tok, scientific = FALSE, trim = TRUE))
  format(Sys.time(), "%Y%m%d%H%M%S")
}

normalize_step1_bridge_source <- function(source, default = "bridge") {
  src <- as.character(if (is.null(source)) "" else source)
  src <- if (length(src) == 0) "" else trimws(src[1])
  if (!nzchar(src)) return(default)
  src
}

build_step1_bridge_signature <- function(payload) {
  if (is.null(payload) || !is.list(payload)) return(NA_character_)
  tok <- suppressWarnings(as.numeric(payload$token)[1])
  if (!is.finite(tok)) return(NA_character_)
  src <- normalize_step1_bridge_source(payload$source, default = "bridge")
  paste0(format(tok, scientific = FALSE, trim = TRUE), "|", src)
}
