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

build_step1_bridge_exp_df <- function(int_df, vinj_default, g_syringe) {
  if (is.null(int_df) || !is.data.frame(int_df) || nrow(int_df) == 0) return(NULL)

  vinj_default_num <- suppressWarnings(as.numeric(vinj_default)[1])
  if (!is.finite(vinj_default_num)) vinj_default_num <- NA_real_

  g_syringe_num <- suppressWarnings(as.numeric(g_syringe)[1])
  if (!is.finite(g_syringe_num)) g_syringe_num <- NA_real_

  V_inj_uL <- if ("V_titrate_uL" %in% colnames(int_df)) as.numeric(int_df$V_titrate_uL) else rep(vinj_default_num, nrow(int_df))
  V_inj_uL[!is.finite(V_inj_uL)] <- vinj_default_num

  Heat_Raw <- if ("heat_cal_mol" %in% colnames(int_df)) {
    as.numeric(int_df$heat_cal_mol)
  } else if ("Heat_ucal" %in% colnames(int_df)) {
    denom <- V_inj_uL * g_syringe_num
    ifelse(is.finite(denom) & denom > 0, 1000 * as.numeric(int_df$Heat_ucal) / denom, NA_real_)
  } else {
    rep(NA_real_, nrow(int_df))
  }

  Ratio_Raw <- if ("Ratio_App" %in% colnames(int_df)) as.numeric(int_df$Ratio_App) else rep(NA_real_, nrow(int_df))

  exp_df <- data.frame(
    Ratio_Raw = Ratio_Raw,
    Heat_Raw = Heat_Raw,
    V_inj_uL = V_inj_uL,
    Inj = seq_len(nrow(int_df)),
    stringsAsFactors = FALSE
  )
  exp_df <- exp_df[is.finite(exp_df$Heat_Raw), , drop = FALSE]
  if (nrow(exp_df) == 0) return(NULL)

  exp_df$Inj <- seq_len(nrow(exp_df))
  exp_df
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
