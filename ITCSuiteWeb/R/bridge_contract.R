`%||%` <- function(x, y) if (is.null(x)) y else x

# [COMMENT_STD][MODULE_HEADER]
# 模块职责：定义 Step1/Step2 bridge payload 的严格清洗与通道写入约束。
# 依赖：shiny::reactiveVal（用于通道存储）。
# 对外接口：sanitize_step1_payload()、sanitize_step2_plot_payload()、make_bridge_channel()。
# 副作用：无效 payload 会触发 warning；有效 payload 写入 reactive store。
# 变更历史：2026-02-12 - 增加 Phase 4 注释规范样板。

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
  # [COMMENT_STD][IO_CONTRACT]
  # 输入来源：Step1 导出的桥接消息（session bridge channel）。
  # 字段/类型：payload/list，要求 schema_version/token/created_at/bundle 等关键字段。
  # 单位：token 为 numeric（时间戳语义）；integration 热量列兼容 Heat_ucal/heat_cal_mol。
  # 空值策略：任一关键字段缺失或类型不符时返回 NULL。
  # 输出保证：返回规范化 list，包含 schema_version=itcsuite.step1.v1 与标准 bundle 结构。
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
  # [COMMENT_STD][IO_CONTRACT]
  # 输入来源：Step2 导出的绘图桥接消息。
  # 字段/类型：payload/list，要求 schema_version/token/source，且至少包含一种 payload body。
  # 单位：token 为 numeric；其余数据帧字段单位沿用 Step2 导出语义。
  # 空值策略：不满足最小结构时返回 NULL；source 非白名单时返回 NULL。
  # 输出保证：返回规范化 list，包含 schema_version=itcsuite.step2_plot.v1 与标准 data.frame 字段。
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
