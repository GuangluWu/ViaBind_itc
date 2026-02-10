# ==============================================================================
# R/weighting.R - 加权残差和鲁棒回归函数
# ==============================================================================
# 包含加权拟合和Huber Loss相关的函数

#' 基于导数计算权重
#' 
#' @description
#' 根据实验数据的变化率（一阶导数）计算权重。
#' 变化越快的地方（拐点附近）权重越高，平坦区域权重较低。
#' 
#' @param exp_data data.frame 实验数据，必须包含 Heat_Raw 列
#' @param valid_idx numeric vector 有效的索引（针数范围）
#' @param smooth_window integer 平滑窗口大小（默认3，用于减少权重波动）
#' @return numeric vector 归一化后的权重向量，长度等于 valid_idx
calculate_weights_from_derivative <- function(exp_data, valid_idx, smooth_window = 3) {
  if(length(valid_idx) < 2) {
    # 如果只有一个或没有数据点，返回均匀权重
    return(rep(1.0, length(valid_idx)))
  }
  
  # 提取有效范围内的热量值
  heat_values <- exp_data$Heat_Raw[valid_idx]
  
  # 计算一阶导数（差分）
  # 对于第i个点，使用前向差分：|heat[i+1] - heat[i]|
  # 对于最后一个点，使用后向差分
  n_points <- length(heat_values)
  derivatives <- numeric(n_points)
  
  if(n_points == 1) {
    derivatives[1] <- 1.0
  } else {
    # 前向差分（除了最后一个点）
    for(i in 1:(n_points - 1)) {
      derivatives[i] <- abs(heat_values[i + 1] - heat_values[i])
    }
    # 最后一个点使用后向差分
    derivatives[n_points] <- abs(heat_values[n_points] - heat_values[n_points - 1])
  }
  
  # 平滑处理：使用移动平均减少权重波动
  if(smooth_window > 1 && n_points >= smooth_window) {
    smoothed_derivatives <- numeric(n_points)
    half_window <- floor(smooth_window / 2)
    
    for(i in 1:n_points) {
      start_idx <- max(1, i - half_window)
      end_idx <- min(n_points, i + half_window)
      smoothed_derivatives[i] <- mean(derivatives[start_idx:end_idx])
    }
    derivatives <- smoothed_derivatives
  }
  
  # 添加小的正数避免权重为0（对于完全平坦的区域）
  derivatives <- derivatives + 1e-6 * max(derivatives, na.rm = TRUE)
  
  # 归一化：使权重总和等于数据点数量
  # 这样加权RSS的尺度与普通RSS保持一致
  mean_weight <- mean(derivatives, na.rm = TRUE)
  if(mean_weight > 0) {
    weights <- derivatives / mean_weight
  } else {
    weights <- rep(1.0, n_points)
  }
  
  return(weights)
}

#' Huber Loss 函数
#' 
#' @description
#' 鲁棒回归损失函数。对于小残差使用平方损失，对于大残差使用线性损失，
#' 从而降低异常点的影响。
#' 
#' @param residual numeric 残差值
#' @param delta numeric 阈值参数。当 |residual| <= delta 时使用平方损失，
#'              当 |residual| > delta 时使用线性损失
#' @return numeric 损失值
huber_loss <- function(residual, delta) {
  abs_res <- abs(residual)
  ifelse(abs_res <= delta,
         residual^2,                    # 平方损失
         delta * (2 * abs_res - delta)) # 线性损失
}

#' 计算 Huber Loss 阈值（delta）
#' 
#' @description
#' 根据残差的标准差自动计算合适的 delta 值。
#' 默认使用 2 * sd(residuals)，这是 Huber Loss 的常用选择。
#' 
#' @param residuals numeric vector 残差向量
#' @param multiplier numeric 倍数因子（默认2.0）
#' @param min_delta numeric 最小 delta 值（默认1e-6，避免过小）
#' @return numeric delta 值
calculate_huber_delta <- function(residuals, multiplier = 2.0, min_delta = 1e-6) {
  if(length(residuals) < 2) {
    return(max(min_delta, abs(residuals[1]) * multiplier))
  }
  
  residual_sd <- sd(residuals, na.rm = TRUE)
  delta <- multiplier * residual_sd
  
  # 确保 delta 不为0或过小
  delta <- max(delta, min_delta)
  
  return(delta)
}

#' 计算加权且鲁棒的目标函数值
#' 
#' @description
#' 组合加权残差和Huber Loss，计算最终的目标函数值。
#' 这是 obj_fun 内部使用的辅助函数。
#' 
#' @param residuals numeric vector 残差向量
#' @param weights numeric vector 权重向量（可选，如果为NULL则不使用加权）
#' @param use_huber logical 是否使用Huber Loss（默认FALSE）
#' @param huber_delta numeric Huber Loss 阈值（如果use_huber=TRUE且为NULL则自动计算）
#' @return numeric 目标函数值（损失总和）
calculate_weighted_robust_loss <- function(residuals, weights = NULL, 
                                           use_huber = FALSE, huber_delta = NULL) {
  # 计算基础损失
  if(use_huber) {
    # 使用 Huber Loss
    if(is.null(huber_delta)) {
      huber_delta <- calculate_huber_delta(residuals)
    }
    losses <- huber_loss(residuals, huber_delta)
  } else {
    # 使用平方损失
    losses <- residuals^2
  }
  
  # 应用权重（如果提供）
  if(!is.null(weights)) {
    if(length(weights) != length(residuals)) {
      stop("权重向量长度必须与残差向量长度相同")
    }
    losses <- losses * weights
  }
  
  # 返回总和
  return(sum(losses))
}
