# ==============================================================================
# R/fitting.R - 拟合相关函数
# ==============================================================================
# 包含拟合相关的辅助函数

#' 模拟计算包装函数
#' 
#' @description
#' 负责将 UI 参数转换为物理参数 (应用 fH, fG 修正)
#' 并计算 Apparent Ratio 用于绘图
#' 
#' @param p list 包含所有参数的列表
#' @param active_paths character vector 激活的反应路径
#' @return data.frame 模拟结果，包含 Ratio_App 和 dQ_App
calculate_simulation <- function(p, active_paths) {
  # 参数验证
  if(is.null(p) || !is.list(p)) {
    stop("Parameter p must be a non-null list")
  }
  
  # 安全处理 active_paths
  if(is.null(active_paths)) {
    active_paths <- character(0)
  }
  if(length(active_paths) == 0) {
    active_paths <- character(0)
  }
  
  if (is.null(p$V_inj_vec) && !is.null(p$V_inj) && !is.null(p$n_inj)) {
    p$V_inj_vec <- rep(p$V_inj, p$n_inj)
  }

  p_true <- p
  # [浓度修正] 真实浓度 = 名义浓度 * 修正因子 (mM → M)
  p_true$H_cell_0 <- p$H_cell_0 * p$fH * 1e-3
  p_true$G_syringe <- p$G_syringe * p$fG * 1e-3
  p_true$V_inj_vec <- p$V_inj_vec / 1000
  
  # 安全构建路径列表
  if(length(active_paths) == 0) {
    paths <- "rxn_M"
  } else {
    paths <- unique(c("rxn_M", active_paths))
  }
  
  res_raw <- run_sim_modular(p_true, paths)
  if(is.null(res_raw)) return(NULL)
  
  # [Apparent Ratio 回算]
  # Sim 内部使用的是真实浓度计算的 Ratio (Ratio_True)。
  # 用户看到的 Ratio_App 是基于名义浓度的。
  # Ratio_True = (G_nom * fG) / (H_nom * fH) = Ratio_App * (fG/fH)
  # 所以: Ratio_App = Ratio_True * (fH/fG)
  res_raw$Ratio_App <- res_raw$Ratio * (p$fH / p$fG)
  
  # [热量修正] dQ_App = dQ_True * fG + Offset
  # (假设热量与注射物浓度成正比，故乘以 fG)
  res_raw$dQ_App <- (res_raw$dQ * p$fG) + p$Offset
  return(res_raw)
}

#' 计算实验用 Ratio_App（基于当前浓度与注射体积）
#'
#' @description
#' 仅基于浓度和体积更新计算每针 Ratio_App，不涉及结合模型求解。
#' 逻辑与 run_sim_modular 的浓度更新一致。
#'
#' @param p list 参数列表，需包含 H_cell_0, G_syringe, V_cell, V_inj_vec 或 V_inj+n_inj, fH, fG
#' @return numeric 每针 Ratio_App 向量
calculate_ratio_app <- function(p) {
  if (is.null(p) || !is.list(p)) {
    stop("Parameter p must be a non-null list")
  }
  if (is.null(p$V_inj_vec) && !is.null(p$V_inj) && !is.null(p$n_inj)) {
    p$V_inj_vec <- rep(p$V_inj, p$n_inj)
  }
  if (is.null(p$V_inj_vec) || length(p$V_inj_vec) == 0) {
    return(numeric(0))
  }
  n_inj <- if (!is.null(p$n_inj) && is.finite(p$n_inj)) p$n_inj else length(p$V_inj_vec)
  if (length(p$V_inj_vec) != n_inj) {
    n_inj <- length(p$V_inj_vec)
  }

  p_true <- p
  # [浓度修正] 真实浓度 = 名义浓度 * 修正因子 (mM → M)
  p_true$H_cell_0 <- p$H_cell_0 * p$fH * 1e-3
  p_true$G_syringe <- p$G_syringe * p$fG * 1e-3
  V_inj_vec <- p$V_inj_vec / 1000
  V_cell <- p$V_cell

  dilution_factor_vec <- (V_cell - V_inj_vec) / V_cell
  injection_contribution_vec <- p_true$G_syringe * V_inj_vec / V_cell

  G_t_real_vec <- numeric(n_inj)
  H_t_real_vec <- numeric(n_inj)

  H_t_real <- p_true$H_cell_0
  G_t_real <- 0

  G_t_real_vec[1] <- (G_t_real * dilution_factor_vec[1] + injection_contribution_vec[1])
  H_t_real_vec[1] <- (H_t_real * dilution_factor_vec[1])
  if (n_inj >= 2) {
    for (i in 2:n_inj) {
      G_t_real_vec[i] <- (G_t_real_vec[i-1] * dilution_factor_vec[i] + injection_contribution_vec[i])
      H_t_real_vec[i] <- (H_t_real_vec[i-1] * dilution_factor_vec[i])
    }
  }

  ratio_true_vec <- G_t_real_vec / H_t_real_vec
  ratio_app_vec <- ratio_true_vec * (p$fH / p$fG)
  return(ratio_app_vec)
}
