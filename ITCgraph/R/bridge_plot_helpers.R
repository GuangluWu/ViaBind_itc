# ==============================================================================
# bridge_plot_helpers.R - Step2 -> Step3 bridge helper functions
# ==============================================================================
# [COMMENT_STD][MODULE_HEADER]
# 模块职责：提供 Step2 到 Step3 的 payload 归一、来源解析与参数同步辅助函数。
# 依赖：base R；由 server.R 调用。
# 对外接口：bridge_plot_* 系列纯函数。
# 副作用：无外部副作用，函数返回新对象。
# 变更历史：2026-02-12 - 增加 Phase 4 注释规范样板。

bridge_plot_safe_num_scalar <- function(x, default = NA_real_) {
  v <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(v)) return(default)
  v
}

bridge_plot_normalize_factor <- function(v, default = 1) {
  vv <- bridge_plot_safe_num_scalar(v, default = default)
  if (!is.finite(vv) || vv <= 0) return(default)
  vv
}

bridge_plot_fit_param_map <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(NULL)
  if (!all(c("parameter", "value") %in% names(df))) return(NULL)
  setNames(as.character(df$value), trimws(as.character(df$parameter)))
}

bridge_plot_get_fit_param_num <- function(fp_map, key, default = NA_real_) {
  if (is.null(fp_map) || length(fp_map) == 0) return(default)
  key_lc <- tolower(as.character(key)[1])
  nms <- names(fp_map)
  idx <- match(key_lc, tolower(nms))
  if (is.na(idx)) return(default)
  bridge_plot_safe_num_scalar(fp_map[[idx]], default = default)
}

bridge_plot_ratio_multiplier <- function(ratio_fh, ratio_fg, apply_ratio = TRUE, default_factor = 1) {
  if (!isTRUE(apply_ratio)) return(default_factor)
  fh <- bridge_plot_normalize_factor(ratio_fh, default = default_factor)
  fg <- bridge_plot_normalize_factor(ratio_fg, default = default_factor)
  fg / fh
}

bridge_plot_apply_heat_correction <- function(y_raw, ratio_fg, heat_offset = 0, apply_ratio = TRUE, default_factor = 1) {
  y_num <- suppressWarnings(as.numeric(y_raw))
  if (!isTRUE(apply_ratio)) return(y_num)

  fg <- bridge_plot_normalize_factor(ratio_fg, default = default_factor)
  off <- bridge_plot_safe_num_scalar(heat_offset, default = 0)
  ((y_num - off) / fg) + off
}

bridge_plot_resolve_step2_payload_source <- function(payload, token = NA_real_) {
  source_raw <- if (is.null(payload$source)) "bridge" else payload$source
  source_mode <- as.character(source_raw)
  source_mode <- if (length(source_mode) == 0) "bridge" else trimws(source_mode[1])
  if (!source_mode %in% c("bridge", "file", "sim_to_exp")) source_mode <- "bridge"

  source_label_raw <- if (is.null(payload$source_label)) "" else payload$source_label
  source_label <- as.character(source_label_raw)
  source_label <- if (length(source_label) == 0) "" else trimws(source_label[1])

  if (identical(source_mode, "file") && nzchar(source_label)) {
    return(source_label)
  }
  if (identical(source_mode, "sim_to_exp")) {
    return("Simulation to experiment")
  }
  if (nzchar(source_label)) {
    return(source_label)
  }
  if (is.finite(token)) return(paste0("Step2 bridge @ ", format(token, scientific = FALSE, trim = TRUE)))
  "Step2 bridge"
}

bridge_plot_extract_step2_payload_frames <- function(payload) {
  # [COMMENT_STD][IO_CONTRACT]
  # 输入来源：Step2 bridge payload（list）或其 sheets 子结构。
  # 字段/类型：期望 data.frame 字段如 power_original/power_corrected/integration_rev/simulation/fit_params/meta_rev。
  # 单位：沿用 Step2 导出语义（功率、热量与浓度单位不在此函数做换算）。
  # 空值策略：缺失字段返回 NULL，不抛异常；优先使用 sheets，其次回退 payload 顶层字段。
  # 输出保证：返回统一 list，固定包含 sheets/power_original/power/integration/simulation/fit_params/meta。
  sheets <- payload$sheets
  power_original_df <- NULL
  power_df <- NULL
  integration_df <- NULL
  simulation_df <- NULL
  fit_params_df <- NULL
  meta_df <- NULL

  if (!is.null(sheets) && is.list(sheets)) {
    if ("power_original" %in% names(sheets) && is.data.frame(sheets$power_original)) {
      power_original_df <- as.data.frame(sheets$power_original)
    }
    if ("power_corrected" %in% names(sheets) && is.data.frame(sheets$power_corrected)) {
      power_df <- as.data.frame(sheets$power_corrected)
    }
    if ("integration_rev" %in% names(sheets) && is.data.frame(sheets$integration_rev)) {
      integration_df <- as.data.frame(sheets$integration_rev)
    } else if ("integration" %in% names(sheets) && is.data.frame(sheets$integration)) {
      integration_df <- as.data.frame(sheets$integration)
    }
    if ("simulation" %in% names(sheets) && is.data.frame(sheets$simulation)) {
      simulation_df <- as.data.frame(sheets$simulation)
    }
    if ("fit_params" %in% names(sheets) && is.data.frame(sheets$fit_params)) {
      fit_params_df <- as.data.frame(sheets$fit_params)
    }
    if ("meta_rev" %in% names(sheets) && is.data.frame(sheets$meta_rev)) {
      meta_df <- as.data.frame(sheets$meta_rev)
    } else if ("meta" %in% names(sheets) && is.data.frame(sheets$meta)) {
      meta_df <- as.data.frame(sheets$meta)
    }
  }

  if (is.null(integration_df) && !is.null(payload$integration_rev) && is.data.frame(payload$integration_rev)) {
    integration_df <- as.data.frame(payload$integration_rev)
  }
  if (is.null(simulation_df) && !is.null(payload$simulation) && is.data.frame(payload$simulation)) {
    simulation_df <- as.data.frame(payload$simulation)
  }
  if (is.null(fit_params_df) && !is.null(payload$fit_params) && is.data.frame(payload$fit_params)) {
    fit_params_df <- as.data.frame(payload$fit_params)
  }
  if (is.null(meta_df) && !is.null(payload$meta_rev) && is.data.frame(payload$meta_rev)) {
    meta_df <- as.data.frame(payload$meta_rev)
  }

  list(
    sheets = if (!is.null(sheets) && is.list(sheets)) sheets else NULL,
    power_original = power_original_df,
    power = power_df,
    integration = integration_df,
    simulation = simulation_df,
    fit_params = fit_params_df,
    meta = meta_df
  )
}

bridge_plot_sync_from_fit_params <- function(fit_params_df, default_heat_offset = 0) {
  fp <- bridge_plot_fit_param_map(fit_params_df)
  if (is.null(fp)) {
    return(list(
      heat_offset = default_heat_offset,
      ratio_fh = 1,
      ratio_fg = 1
    ))
  }

  off <- bridge_plot_get_fit_param_num(fp, "Offset_cal", default = default_heat_offset)
  list(
    heat_offset = off,
    ratio_fh = bridge_plot_normalize_factor(bridge_plot_get_fit_param_num(fp, "fH", default = 1), 1),
    ratio_fg = bridge_plot_normalize_factor(bridge_plot_get_fit_param_num(fp, "fG", default = 1), 1)
  )
}
