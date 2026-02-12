`%||%` <- function(x, y) if (is.null(x)) y else x

normalize_bridge_token <- function(token) {
  tok <- suppressWarnings(as.numeric(token)[1])
  if (!is.finite(tok)) return(NA_real_)
  tok
}

normalize_bridge_scalar_chr <- function(x) {
  val <- as.character(x %||% "")
  val <- if (length(val) == 0) "" else trimws(val[1])
  if (!nzchar(val)) return(NA_character_)
  val
}

is_valid_created_at <- function(x) {
  created <- normalize_bridge_scalar_chr(x)
  !is.na(created)
}

sanitize_step1_payload <- function(payload) {
  if (is.null(payload) || !is.list(payload)) return(NULL)
  if (!identical(normalize_bridge_scalar_chr(payload$schema_version), "itcsuite.step1.v1")) return(NULL)
  if (!isTRUE(is_valid_created_at(payload$created_at))) return(NULL)

  token <- normalize_bridge_token(payload$token)
  if (!is.finite(token)) return(NULL)

  bundle <- payload$bundle
  if (!is.list(bundle)) return(NULL)
  if (!identical(normalize_bridge_scalar_chr(bundle$schema_version), "itcsuite.bundle.v1")) return(NULL)

  integration_df <- NULL
  if (is.data.frame(bundle$integration)) {
    integration_df <- as.data.frame(bundle$integration)
  }
  if (is.null(integration_df) || nrow(integration_df) == 0) return(NULL)
  if (!("Ratio_App" %in% names(integration_df))) return(NULL)
  if (!(("heat_cal_mol" %in% names(integration_df)) || ("Heat_ucal" %in% names(integration_df)))) return(NULL)

  meta_df <- NULL
  if (is.data.frame(bundle$meta)) {
    meta_df <- as.data.frame(bundle$meta)
  }
  if (!is.null(meta_df) && !all(c("parameter", "value") %in% names(meta_df))) return(NULL)

  created_at <- normalize_bridge_scalar_chr(payload$created_at)
  source <- normalize_bridge_scalar_chr(payload$source)
  if (is.na(source)) source <- "Step1"

  list(
    schema_version = "itcsuite.step1.v1",
    created_at = created_at,
    token = token,
    source = source,
    integration = integration_df,
    meta = meta_df,
    bundle = list(
      schema_version = "itcsuite.bundle.v1",
      meta = meta_df,
      power_original = if (is.data.frame(bundle$power_original)) as.data.frame(bundle$power_original) else NULL,
      power_corrected = if (is.data.frame(bundle$power_corrected)) as.data.frame(bundle$power_corrected) else NULL,
      integration = integration_df
    )
  )
}

sanitize_step2_plot_payload <- function(payload) {
  if (is.null(payload) || !is.list(payload)) return(NULL)
  if (!identical(normalize_bridge_scalar_chr(payload$schema_version), "itcsuite.step2_plot.v1")) return(NULL)
  if (!isTRUE(is_valid_created_at(payload$created_at))) return(NULL)

  token <- normalize_bridge_token(payload$token)
  if (!is.finite(token)) return(NULL)

  sheets <- payload$sheets
  if (!is.list(sheets)) sheets <- list()

  has_payload_body <- length(sheets) > 0 ||
    is.data.frame(payload$integration_rev) ||
    is.data.frame(payload$simulation) ||
    is.data.frame(payload$fit_params)
  if (!isTRUE(has_payload_body)) return(NULL)

  created_at <- normalize_bridge_scalar_chr(payload$created_at)
  source <- normalize_bridge_scalar_chr(payload$source)
  if (is.na(source) || !source %in% c("bridge", "file", "sim_to_exp")) return(NULL)
  source_label <- as.character(payload$source_label %||% "")
  source_label <- if (length(source_label) == 0) "" else source_label[1]

  list(
    schema_version = "itcsuite.step2_plot.v1",
    created_at = created_at,
    token = token,
    source = source,
    source_label = source_label,
    sheets = sheets,
    integration_rev = if (is.data.frame(payload$integration_rev)) as.data.frame(payload$integration_rev) else NULL,
    meta_rev = if (is.data.frame(payload$meta_rev)) as.data.frame(payload$meta_rev) else NULL,
    fit_params = if (is.data.frame(payload$fit_params)) as.data.frame(payload$fit_params) else data.frame(),
    simulation = if (is.data.frame(payload$simulation)) as.data.frame(payload$simulation) else data.frame()
  )
}

make_bridge_channel <- function(validator_fn, label) {
  store <- shiny::reactiveVal(NULL)
  function(value) {
    if (missing(value)) return(store())
    if (is.null(value)) {
      store(NULL)
      return(invisible(NULL))
    }
    normalized <- validator_fn(value)
    if (is.null(normalized)) {
      warning("Bridge payload rejected for ", label, ".")
      return(invisible(NULL))
    }
    store(normalized)
    invisible(NULL)
  }
}
