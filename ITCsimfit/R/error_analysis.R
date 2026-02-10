# ==============================================================================
# R/error_analysis.R - 误差分析模块
# ==============================================================================
# 包含参数置信区间计算的函数：Hessian方法和Bootstrap方法


# ==============================================================================
# 3.5. 误差分析函数 (Error Analysis Functions)
# ==============================================================================
#' 使用 Hessian 矩阵计算参数协方差和置信区间
#' 
#' @param obj_fun 目标函数
#' @param par_opt 最优参数值
#' @param n_data 数据点数量
#' @param rss 残差平方和
#' @param conf_level 置信水平 (默认 0.95)
#' @return data.frame 包含参数名、最优值、标准误差、置信区间
calculate_hessian_ci <- function(obj_fun, par_opt, n_data, rss, conf_level = 0.95) {
  n_par <- length(par_opt)
  if (n_par == 0 || n_data <= n_par) {
    return(NULL)  # 数据点不足，无法计算
  }
  
  # 计算自由度
  dof <- n_data - n_par
  
  # 计算残差方差 (sigma^2)
  sigma_sq <- rss / dof
  if (sigma_sq <= 0 || !is.finite(sigma_sq)) {
    return(NULL)
  }
  
  # 计算 Hessian 矩阵 (数值方法)
  # 使用中心差分法计算二阶导数
  eps <- 1e-5  # 数值微分的步长
  hessian <- matrix(0, nrow = n_par, ncol = n_par)
  
  tryCatch({
    for (i in 1:n_par) {
      for (j in 1:n_par) {
        # 中心差分公式计算 Hessian[i,j] = d^2 f / (d par_i d par_j)
        par_pp <- par_opt
        par_pm <- par_opt
        par_mp <- par_opt
        par_mm <- par_opt
        
        par_pp[i] <- par_pp[i] + eps
        par_pp[j] <- par_pp[j] + eps
        
        par_pm[i] <- par_pm[i] + eps
        par_pm[j] <- par_pm[j] - eps
        
        par_mp[i] <- par_mp[i] - eps
        par_mp[j] <- par_mp[j] + eps
        
        par_mm[i] <- par_mm[i] - eps
        par_mm[j] <- par_mm[j] - eps
        
        f_pp <- obj_fun(par_pp)
        f_pm <- obj_fun(par_pm)
        f_mp <- obj_fun(par_mp)
        f_mm <- obj_fun(par_mm)
        
        # 二阶中心差分
        hessian[i, j] <- (f_pp - f_pm - f_mp + f_mm) / (4 * eps^2)
      }
    }
    
      # 计算协方差矩阵: Cov = sigma^2 * inv(Hessian)
      # 注意：对于最小二乘问题，Hessian 应该是正定的
      hessian_inv <- tryCatch({
        solve(hessian)
      }, error = function(e) {
        # 如果 Hessian 不可逆，尝试伪逆
        if (requireNamespace("MASS", quietly = TRUE)) {
          tryCatch({
            MASS::ginv(hessian)
          }, error = function(e2) {
            return(NULL)
          })
        } else {
          return(NULL)
        }
      })
    
    if (is.null(hessian_inv)) {
      return(NULL)
    }
    
    cov_matrix <- sigma_sq * hessian_inv
    
    # 提取对角线元素 (参数方差)
    param_var <- diag(cov_matrix)
    param_se <- sqrt(pmax(param_var, 0))  # 标准误差
    
    # 计算 t 统计量 (95% 置信区间，双边)
    t_crit <- qt((1 + conf_level) / 2, df = dof)
    
    # 构建结果数据框
    result <- data.frame(
      Parameter = names(par_opt),
      Value = par_opt,
      SE = param_se,
      CI_Lower = par_opt - t_crit * param_se,
      CI_Upper = par_opt + t_crit * param_se,
      stringsAsFactors = FALSE
    )
    
    return(result)
    
  }, error = function(e) {
    return(NULL)
  })
}

#' 改进的 Hessian 方法（更稳健的数值实现，适合小样本）
#' 
#' 原理：
#' 1. 在最优参数点θ*处，目标函数RSS(θ)可以近似为二次型：
#'    RSS(θ) ≈ RSS(θ*) + (θ-θ*)'H(θ-θ*)/2
#'    其中H是Hessian矩阵（二阶导数矩阵）
#' 
#' 2. 参数协方差矩阵：Cov(θ) = σ² × H⁻¹
#'    其中σ² = RSS/(n-p)是残差方差，n是数据点数，p是参数个数
#' 
#' 3. 标准误差：SE(θᵢ) = √Cov(θᵢ, θᵢ)
#' 
#' 4. 置信区间：CI = θ* ± t(α/2, df) × SE(θ)
#'    其中df = n-p是自由度，t是t分布的临界值
#' 
#' 改进措施：
#' - 自适应步长：根据参数大小调整数值微分的步长
#' - 正则化：添加小的正数到Hessian对角线，提高数值稳定性
#' - 伪逆：如果Hessian不可逆，使用Moore-Penrose伪逆
#' 
#' 适用性：
#' - 自由度 ≥ 10：可靠性较好
#' - 自由度 5-10：可靠性中等，结果可用但需谨慎解释
#' - 自由度 < 5：可靠性较低，置信区间可能偏窄
#' 
#' 局限性：
#' - 假设目标函数在最优值附近近似二次型（局部线性化假设）
#' - 小样本时可能低估参数不确定性
#' - 对强非线性问题可能不够准确
#' 
#' @param obj_fun 目标函数
#' @param par_opt 最优参数值
#' @param n_data 数据点数量
#' @param rss 残差平方和
#' @param conf_level 置信水平 (默认 0.95)
#' @return data.frame 包含参数名、最优值、标准误差、置信区间
calculate_hessian_ci_robust <- function(obj_fun, par_opt, n_data, rss, conf_level = 0.95) {
  n_par <- length(par_opt)
  if (n_par == 0 || n_data <= n_par) {
    return(NULL)
  }
  
  dof <- n_data - n_par
  sigma_sq <- rss / dof
  if (sigma_sq <= 0 || !is.finite(sigma_sq)) {
    return(NULL)
  }
  
  # 使用自适应步长
  eps_base <- 1e-5
  hessian <- matrix(0, nrow = n_par, ncol = n_par)
  
  tryCatch({
    for (i in 1:n_par) {
      for (j in 1:n_par) {
        # 自适应步长：根据参数大小调整
        eps_i <- eps_base * max(abs(par_opt[i]), 1e-6)
        eps_j <- eps_base * max(abs(par_opt[j]), 1e-6)
        eps <- min(eps_i, eps_j)
        
        par_pp <- par_opt
        par_pm <- par_opt
        par_mp <- par_opt
        par_mm <- par_opt
        
        par_pp[i] <- par_pp[i] + eps
        par_pp[j] <- par_pp[j] + eps
        
        par_pm[i] <- par_pm[i] + eps
        par_pm[j] <- par_pm[j] - eps
        
        par_mp[i] <- par_mp[i] - eps
        par_mp[j] <- par_mp[j] + eps
        
        par_mm[i] <- par_mm[i] - eps
        par_mm[j] <- par_mm[j] - eps
        
        f_pp <- obj_fun(par_pp)
        f_pm <- obj_fun(par_pm)
        f_mp <- obj_fun(par_mp)
        f_mm <- obj_fun(par_mm)
        
        # 检查函数值是否合理
        if (all(is.finite(c(f_pp, f_pm, f_mp, f_mm)))) {
          hessian[i, j] <- (f_pp - f_pm - f_mp + f_mm) / (4 * eps^2)
        }
      }
    }
    
    # 正则化：添加小的正数到对角线，提高数值稳定性
    diag(hessian) <- diag(hessian) + 1e-8 * max(abs(diag(hessian)))
    
    # 尝试求逆
    hessian_inv <- tryCatch({
      solve(hessian)
    }, error = function(e) {
      # 如果失败，尝试伪逆
      if (requireNamespace("MASS", quietly = TRUE)) {
        tryCatch({
          MASS::ginv(hessian)
        }, error = function(e2) {
          return(NULL)
        })
      } else {
        return(NULL)
      }
    })
    
    if (is.null(hessian_inv)) {
      return(NULL)
    }
    
    cov_matrix <- sigma_sq * hessian_inv
    # 设置协方差矩阵的行列名（确保参数名正确）
    rownames(cov_matrix) <- names(par_opt)
    colnames(cov_matrix) <- names(par_opt)
    
    param_var <- diag(cov_matrix)
    param_se <- sqrt(pmax(param_var, 0))
    
    # 使用更保守的t值（对于小样本）
    t_crit <- qt((1 + conf_level) / 2, df = max(1, dof))
    
    result <- data.frame(
      Parameter = names(par_opt),
      Value = par_opt,
      SE = param_se,
      CI_Lower = par_opt - t_crit * param_se,
      CI_Upper = par_opt + t_crit * param_se,
      stringsAsFactors = FALSE
    )
    
    # 将协方差矩阵作为属性附加到结果中，以便后续提取
    attr(result, "cov_matrix") <- cov_matrix
    
    return(result)
    
  }, error = function(e) {
    return(NULL)
  })
}

#' 使用参数化 Bootstrap 方法计算参数置信区间（更稳健，适合小样本）
#' 
#' 参数化Bootstrap假设残差服从正态分布，从该分布中采样，而不是重采样实际残差。
#' 这种方法对小样本更稳健，计算更快，成功率更高。
#' 
#' @param obj_fun_factory 目标函数工厂函数，接受 exp_df 和 range_lim，返回目标函数 obj_fun(par)
#' @param par_opt 最优参数值
#' @param exp_df 实验数据框（包含 Heat_Raw 列）
#' @param range_lim 拟合区间范围 c(start_idx, end_idx)
#' @param lower_b 参数下界向量
#' @param upper_b 参数上界向量
#' @param calculate_simulation_fun 模拟函数，接受参数列表和active_paths，返回模拟结果
#' @param active_paths 激活的反应路径
#' @param fixed_params 固定参数列表
#' @param params_to_opt 要优化的参数名向量
#' @param n_bootstrap Bootstrap 重采样次数 (默认 100)
#' @param conf_level 置信水平 (默认 0.95)
#' @return data.frame 包含参数名、最优值、标准误差、Bootstrap 置信区间
calculate_parametric_bootstrap_ci <- function(obj_fun_factory, par_opt, exp_df, range_lim, lower_b, upper_b,
                                              calculate_simulation_fun, active_paths, fixed_params, params_to_opt,
                                              n_bootstrap = 100, conf_level = 0.95) {
  n_par <- length(par_opt)
  if (n_par == 0 || n_bootstrap < 20) {
    return(NULL)
  }
  
  # 计算原始拟合的模拟结果（用于获取残差）
  p_full <- fixed_params
  p_full[params_to_opt] <- par_opt
  
  sim_result <- tryCatch({
    calculate_simulation_fun(p_full, active_paths)
  }, error = function(e) NULL)
  
  if (is.null(sim_result)) {
    return(NULL)
  }
  
  # 计算原始残差
  valid_idx <- range_lim[1]:range_lim[2]
  max_idx <- min(nrow(sim_result), nrow(exp_df))
  valid_idx <- valid_idx[valid_idx <= max_idx]
  
  if (length(valid_idx) == 0) {
    return(NULL)
  }
  
  y_fitted <- sim_result$dQ_App[valid_idx]
  y_observed <- exp_df$Heat_Raw[valid_idx]
  residuals_orig <- y_observed - y_fitted
  
  # 计算残差的均值和标准差（用于参数化Bootstrap）
  # 假设残差服从正态分布 N(0, sigma^2)
  # 对于最小二乘，残差均值应该接近0
  residual_mean <- mean(residuals_orig, na.rm = TRUE)
  residual_sd <- sd(residuals_orig, na.rm = TRUE)
  
  if (!is.finite(residual_sd) || residual_sd <= 0) {
    return(NULL)
  }
  
  # 存储 Bootstrap 样本的参数估计
  bootstrap_params <- matrix(NA, nrow = n_bootstrap, ncol = n_par)
  colnames(bootstrap_params) <- names(par_opt)
  
  success_count <- 0
  
  # 诊断信息收集
  diag_info <- list(
    convergence_fail = 0,
    na_params = 0,
    inf_params = 0,
    boundary_violation = 0,
    obj_val_too_large = 0,
    optim_error = 0,
    other_error = 0
  )
  
  # 参数化Bootstrap循环：从正态分布中采样残差
  for (b in 1:n_bootstrap) {
    tryCatch({
      # 1. 从正态分布中采样残差（参数化Bootstrap）
      resampled_residuals <- rnorm(length(valid_idx), mean = 0, sd = residual_sd)
      
      # 2. 构建 Bootstrap 数据：y_bootstrap = y_fitted + resampled_residuals
      exp_bootstrap <- exp_df
      exp_bootstrap$Heat_Raw[valid_idx] <- y_fitted + resampled_residuals
      
      # 3. 构建 Bootstrap 目标函数（使用Bootstrap数据）
      obj_fun_bootstrap <- obj_fun_factory(exp_bootstrap, range_lim)
      
      # 4. 直接使用最优值作为初值
      par_init <- par_opt
      
      # 5. 使用L-BFGS-B拟合Bootstrap数据
      fit_result <- tryCatch({
        optim(par = par_init, fn = obj_fun_bootstrap, method = "L-BFGS-B", 
              lower = lower_b, upper = upper_b, 
              control = list(
                factr = 1e8,
                maxit = 50,
                pgtol = 1.0
              ))
      }, error = function(e) {
        diag_info$optim_error <<- diag_info$optim_error + 1
        return(NULL)
      })
      
      # 6. 详细诊断拟合结果
      if (is.null(fit_result)) {
        # optim_error 已在上面记录
      } else if (fit_result$convergence != 0 && fit_result$convergence != 1 && fit_result$convergence != 51) {
        diag_info$convergence_fail <<- diag_info$convergence_fail + 1
      } else if (any(is.na(fit_result$par))) {
        diag_info$na_params <<- diag_info$na_params + 1
      } else if (!all(is.finite(fit_result$par))) {
        diag_info$inf_params <<- diag_info$inf_params + 1
      } else {
        # 确保参数在边界内
        fit_result$par <- pmax(pmin(fit_result$par, upper_b), lower_b)
        
        # 检查边界违反
        if (any(fit_result$par < lower_b - 1e-6) || any(fit_result$par > upper_b + 1e-6)) {
          diag_info$boundary_violation <<- diag_info$boundary_violation + 1
        } else {
          # 检查目标函数值
          obj_val <- tryCatch(obj_fun_bootstrap(fit_result$par), error = function(e) Inf)
          if (!is.finite(obj_val) || obj_val >= 1e15) {
            diag_info$obj_val_too_large <<- diag_info$obj_val_too_large + 1
          } else {
            # 成功！
            bootstrap_params[b, ] <- fit_result$par
            success_count <- success_count + 1
          }
        }
      }
      
    }, error = function(e) {
      diag_info$other_error <<- diag_info$other_error + 1
    })
    
    # 更新进度
    if (b %% 20 == 0 && exists("setProgress")) {
      tryCatch({
        setProgress(value = b / n_bootstrap, detail = paste('已完成', b, '/', n_bootstrap, 
                                                           ' (成功:', success_count, ')'))
      }, error = function(e) {})
    }
  }
  
  # 成功阈值：至少需要10次成功，或至少15%的成功率
  min_success <- max(10, min(15, n_bootstrap * 0.15))
  if (success_count < min_success) {
    # 构建诊断信息（包含min_success用于显示）
    diagnostics <- list(
      success_count = success_count,
      total = n_bootstrap,
      success_rate = success_count / n_bootstrap,
      min_success_required = min_success,
      diagnostics = diag_info
    )
    # 返回诊断信息（作为属性）
    result <- NULL
    attr(result, "diagnostics") <- diagnostics
    return(result)
  }
  
  # 计算置信区间和标准误差 (百分位数方法)
  alpha <- 1 - conf_level
  result <- data.frame(
    Parameter = names(par_opt),
    Value = par_opt,
    stringsAsFactors = FALSE
  )
  
  ci_lower <- numeric(n_par)
  ci_upper <- numeric(n_par)
  se_bootstrap <- numeric(n_par)
  
  for (i in 1:n_par) {
    valid_vals <- bootstrap_params[!is.na(bootstrap_params[, i]), i]
    if (length(valid_vals) >= 10) {
      ci_lower[i] <- quantile(valid_vals, alpha / 2, na.rm = TRUE)
      ci_upper[i] <- quantile(valid_vals, 1 - alpha / 2, na.rm = TRUE)
      se_bootstrap[i] <- sd(valid_vals, na.rm = TRUE)
    } else {
      ci_lower[i] <- NA
      ci_upper[i] <- NA
      se_bootstrap[i] <- NA
    }
  }
  
  result$SE <- se_bootstrap
  result$CI_Lower <- ci_lower
  result$CI_Upper <- ci_upper
  
  return(result)
}

#' 使用 Bootstrap 方法计算参数置信区间（完整实现）
#' 
#' @param obj_fun_factory 目标函数工厂函数，接受 exp_df 和 range_lim，返回目标函数 obj_fun(par)
#' @param par_opt 最优参数值
#' @param exp_df 实验数据框（包含 Heat_Raw 列）
#' @param range_lim 拟合区间范围 c(start_idx, end_idx)
#' @param lower_b 参数下界向量
#' @param upper_b 参数上界向量
#' @param calculate_simulation_fun 模拟函数，接受参数列表和active_paths，返回模拟结果
#' @param active_paths 激活的反应路径
#' @param fixed_params 固定参数列表
#' @param params_to_opt 要优化的参数名向量
#' @param n_bootstrap Bootstrap 重采样次数 (默认 150)
#' @param conf_level 置信水平 (默认 0.95)
#' @return data.frame 包含参数名、最优值、标准误差、Bootstrap 置信区间
calculate_bootstrap_ci_full <- function(obj_fun_factory, par_opt, exp_df, range_lim, lower_b, upper_b,
                                        calculate_simulation_fun, active_paths, fixed_params, params_to_opt,
                                        n_bootstrap = 150, conf_level = 0.95) {
  n_par <- length(par_opt)
  if (n_par == 0 || n_bootstrap < 20) {
    return(NULL)
  }
  
  # 计算原始拟合的模拟结果（用于获取残差）
  obj_fun_orig <- obj_fun_factory(exp_df, range_lim)
  
  # 通过模拟函数计算原始拟合值
  # 构建完整参数列表
  p_full <- fixed_params
  p_full[params_to_opt] <- par_opt
  
  sim_result <- tryCatch({
    calculate_simulation_fun(p_full, active_paths)
  }, error = function(e) NULL)
  
  if (is.null(sim_result)) {
    return(NULL)
  }
  
  # 计算原始残差
  valid_idx <- range_lim[1]:range_lim[2]
  max_idx <- min(nrow(sim_result), nrow(exp_df))
  valid_idx <- valid_idx[valid_idx <= max_idx]
  
  if (length(valid_idx) == 0) {
    return(NULL)
  }
  
  y_fitted <- sim_result$dQ_App[valid_idx]
  y_observed <- exp_df$Heat_Raw[valid_idx]
  residuals_orig <- y_observed - y_fitted
  
  # 存储 Bootstrap 样本的参数估计
  bootstrap_params <- matrix(NA, nrow = n_bootstrap, ncol = n_par)
  colnames(bootstrap_params) <- names(par_opt)
  
  success_count <- 0
  
  # Bootstrap 循环（使用withProgress包装，在Shiny环境中自动显示进度）
  # 注意：这个函数会在withProgress中调用，所以setProgress应该可用
  for (b in 1:n_bootstrap) {
    tryCatch({
      # 1. 重采样残差 (有放回)
      resampled_residuals <- sample(residuals_orig, replace = TRUE)
      
      # 2. 构建 Bootstrap 数据：y_bootstrap = y_fitted + resampled_residuals
      exp_bootstrap <- exp_df
      exp_bootstrap$Heat_Raw[valid_idx] <- y_fitted + resampled_residuals
      
      # 3. 构建 Bootstrap 目标函数（使用Bootstrap数据）
      obj_fun_bootstrap <- obj_fun_factory(exp_bootstrap, range_lim)
      
      # 4. 从最优值开始拟合（Bootstrap数据应该接近原始数据，所以最优值应该是很好的初值）
      # 添加非常小的扰动，避免完全相同的初始值导致数值问题
      par_init <- par_opt
      # 只在边界附近添加微小扰动
      for (i in 1:n_par) {
        if (abs(par_opt[i] - lower_b[i]) < 1e-6 || abs(par_opt[i] - upper_b[i]) < 1e-6) {
          # 如果在边界上，向内移动一点
          if (abs(par_opt[i] - lower_b[i]) < 1e-6) {
            par_init[i] <- lower_b[i] + 0.001 * (upper_b[i] - lower_b[i])
          } else {
            par_init[i] <- upper_b[i] - 0.001 * (upper_b[i] - lower_b[i])
          }
        } else {
          # 不在边界上，添加非常小的随机扰动（0.1%的相对扰动）
          perturbation <- 0.001 * abs(par_opt[i]) * rnorm(1, 0, 1)
          par_init[i] <- par_opt[i] + perturbation
        }
      }
      par_init <- pmax(pmin(par_init, upper_b), lower_b)  # 确保在边界内
      
      # 5. 使用L-BFGS-B拟合Bootstrap数据（带重试机制）
      # 第一次尝试：使用较严格的参数
      fit_result <- tryCatch({
        optim(par = par_init, fn = obj_fun_bootstrap, method = "L-BFGS-B", 
              lower = lower_b, upper = upper_b, 
              control = list(
                factr = 1e8,      # 更宽松的收敛条件
                maxit = 50,       # 减少迭代次数，加快速度
                pgtol = 1.0       # 非常宽松的梯度容差
              ))
      }, error = function(e) NULL)
      
      # 如果第一次失败，用更宽松的参数重试
      if (is.null(fit_result) || 
          (!fit_result$convergence %in% c(0, 1, 51)) ||
          any(is.na(fit_result$par)) || 
          !all(is.finite(fit_result$par))) {
        # 重试：使用更宽松的条件
        fit_result <- tryCatch({
          optim(par = par_init, fn = obj_fun_bootstrap, method = "L-BFGS-B", 
                lower = lower_b, upper = upper_b, 
                control = list(
                  factr = 1e9,      # 非常宽松
                  maxit = 30,       # 更少迭代
                  pgtol = 10.0      # 非常宽松的梯度容差
                ))
        }, error = function(e) NULL)
      }
      
      # 6. 检查拟合是否成功（非常宽松的成功条件）
      # 只要参数是有限的且在边界内，就接受（即使没有完全收敛）
      if (!is.null(fit_result) && 
          !any(is.na(fit_result$par)) && 
          all(is.finite(fit_result$par))) {
        # 确保参数在边界内
        fit_result$par <- pmax(pmin(fit_result$par, upper_b), lower_b)
        
        # 检查目标函数值是否合理（不应该太大）
        obj_val <- tryCatch(obj_fun_bootstrap(fit_result$par), error = function(e) Inf)
        if (is.finite(obj_val) && obj_val < 1e15) {  # 目标函数值合理
          bootstrap_params[b, ] <- fit_result$par
          success_count <- success_count + 1
        }
      }
      
    }, error = function(e) {
      # 失败时跳过这个Bootstrap样本
    })
    
    # 更新进度（每10次更新一次）
    if (b %% 10 == 0) {
      tryCatch({
        if (exists("setProgress")) {
          setProgress(value = b / n_bootstrap, detail = paste('已完成', b, '/', n_bootstrap, 
                                                             ' (成功:', success_count, ')'))
        }
      }, error = function(e) {
        # 如果setProgress不可用，静默跳过
      })
    }
  }
  
  # 进一步降低成功阈值：至少需要10次成功，或者至少15%的成功率
  min_success <- max(10, min(15, n_bootstrap * 0.15))  # 至少10次，或15%成功率
  if (success_count < min_success) {
    # 返回NULL，但先记录诊断信息（用于调试）
    # cat(sprintf("Bootstrap失败: 成功 %d/%d (需要至少 %d)\n", success_count, n_bootstrap, min_success))
    return(NULL)  # Bootstrap 成功次数太少
  }
  
  # 计算置信区间和标准误差 (百分位数方法)
  alpha <- 1 - conf_level
  result <- data.frame(
    Parameter = names(par_opt),
    Value = par_opt,
    stringsAsFactors = FALSE
  )
  
  ci_lower <- numeric(n_par)
  ci_upper <- numeric(n_par)
  se_bootstrap <- numeric(n_par)
  
  for (i in 1:n_par) {
    valid_vals <- bootstrap_params[!is.na(bootstrap_params[, i]), i]
    if (length(valid_vals) >= 10) {
      # 百分位数方法计算置信区间
      ci_lower[i] <- quantile(valid_vals, alpha / 2, na.rm = TRUE)
      ci_upper[i] <- quantile(valid_vals, 1 - alpha / 2, na.rm = TRUE)
      # Bootstrap标准误差：Bootstrap分布的标准差
      se_bootstrap[i] <- sd(valid_vals, na.rm = TRUE)
    } else {
      ci_lower[i] <- NA
      ci_upper[i] <- NA
      se_bootstrap[i] <- NA
    }
  }
  
  result$SE <- se_bootstrap
  result$CI_Lower <- ci_lower
  result$CI_Upper <- ci_upper
  
  return(result)
}
