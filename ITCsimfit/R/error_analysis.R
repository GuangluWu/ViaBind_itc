# ==============================================================================
# R/error_analysis.R - 误差分析模块
# ==============================================================================
# 包含参数置信区间计算的函数：Hessian方法和Bootstrap方法


# ==============================================================================
# 3.5. 误差分析函数 (Error Analysis Functions)
# ==============================================================================

ERROR_ANALYSIS_METHOD_VERSION <- "2.0"

# Build an error-analysis table even when uncertainty cannot be estimated.  Keeping
# the point estimates visible while returning NA uncertainty is safer than turning
# an invalid/indefinite covariance matrix into zero standard errors.
build_error_analysis_result <- function(par_opt, se = NULL, ci_lower = NULL, ci_upper = NULL,
                                        cov_matrix = NULL, diagnostics = list()) {
  original_names <- names(par_opt)
  par_opt <- as.numeric(par_opt)
  param_names <- original_names
  if (is.null(param_names) || any(!nzchar(param_names))) {
    param_names <- paste0("par", seq_along(par_opt))
  }

  n_par <- length(par_opt)
  fill_numeric <- function(x) {
    if (is.null(x) || length(x) != n_par) return(rep(NA_real_, n_par))
    as.numeric(x)
  }

  result <- data.frame(
    Parameter = param_names,
    Value = par_opt,
    SE = fill_numeric(se),
    CI_Lower = fill_numeric(ci_lower),
    CI_Upper = fill_numeric(ci_upper),
    stringsAsFactors = FALSE
  )

  if (!is.null(cov_matrix) && is.matrix(cov_matrix)) {
    rownames(cov_matrix) <- colnames(cov_matrix) <- param_names
    attr(result, "cov_matrix") <- cov_matrix
  }
  attr(result, "diagnostics") <- diagnostics
  result
}

normalize_uncertainty_bounds <- function(bounds, par_opt, default) {
  n_par <- length(par_opt)
  if (is.null(bounds) || length(bounds) != n_par) {
    out <- rep(default, n_par)
  } else {
    out <- suppressWarnings(as.numeric(bounds))
    out[is.na(out)] <- default
  }
  names(out) <- names(par_opt)
  out
}

# Numerically differentiate the residual vector, rather than the scalar RSS.
# This avoids the factor-of-two ambiguity in an RSS Hessian and permits stable,
# independently scaled steps for parameters such as logK, H and Offset.
calculate_residual_jacobian <- function(residual_fun, par_opt, lower_b = NULL, upper_b = NULL,
                                        rel_step = .Machine$double.eps^(1 / 3)) {
  original_names <- names(par_opt)
  par_opt <- as.numeric(par_opt)
  names(par_opt) <- if (is.null(original_names)) paste0("par", seq_along(par_opt)) else original_names
  n_par <- length(par_opt)
  lower_b <- normalize_uncertainty_bounds(lower_b, par_opt, -Inf)
  upper_b <- normalize_uncertainty_bounds(upper_b, par_opt, Inf)

  eval_residuals <- function(par) {
    names(par) <- names(par_opt)
    value <- residual_fun(par)
    value <- suppressWarnings(as.numeric(value))
    if (length(value) == 0L || any(!is.finite(value))) {
      stop("Residual function returned non-finite or empty values")
    }
    value
  }

  r0 <- eval_residuals(par_opt)
  jacobian <- matrix(NA_real_, nrow = length(r0), ncol = n_par,
                     dimnames = list(NULL, names(par_opt)))
  steps <- numeric(n_par)
  schemes <- character(n_par)
  boundary <- logical(n_par)

  for (i in seq_len(n_par)) {
    span <- upper_b[i] - lower_b[i]
    span_scale <- if (is.finite(span) && span > 0) 0.1 * span else 0
    par_scale <- max(abs(par_opt[i]), span_scale, 1)
    h <- rel_step * par_scale
    if (is.finite(span) && span > 0) h <- min(h, span / 10)
    if (!is.finite(h) || h <= 0) stop("Unable to determine a finite-difference step")

    can_minus <- par_opt[i] - h >= lower_b[i]
    can_plus <- par_opt[i] + h <= upper_b[i]
    can_minus2 <- par_opt[i] - 2 * h >= lower_b[i]
    can_plus2 <- par_opt[i] + 2 * h <= upper_b[i]

    if (can_minus && can_plus) {
      p_minus <- p_plus <- par_opt
      p_minus[i] <- p_minus[i] - h
      p_plus[i] <- p_plus[i] + h
      r_minus <- eval_residuals(p_minus)
      r_plus <- eval_residuals(p_plus)
      if (length(r_minus) != length(r0) || length(r_plus) != length(r0)) {
        stop("Residual length changed during numerical differentiation")
      }
      jacobian[, i] <- (r_plus - r_minus) / (2 * h)
      schemes[i] <- "central"
    } else if (can_plus2) {
      p1 <- p2 <- par_opt
      p1[i] <- p1[i] + h
      p2[i] <- p2[i] + 2 * h
      r1 <- eval_residuals(p1)
      r2 <- eval_residuals(p2)
      if (length(r1) != length(r0) || length(r2) != length(r0)) {
        stop("Residual length changed during numerical differentiation")
      }
      jacobian[, i] <- (-3 * r0 + 4 * r1 - r2) / (2 * h)
      schemes[i] <- "forward"
      boundary[i] <- TRUE
    } else if (can_minus2) {
      p1 <- p2 <- par_opt
      p1[i] <- p1[i] - h
      p2[i] <- p2[i] - 2 * h
      r1 <- eval_residuals(p1)
      r2 <- eval_residuals(p2)
      if (length(r1) != length(r0) || length(r2) != length(r0)) {
        stop("Residual length changed during numerical differentiation")
      }
      jacobian[, i] <- (3 * r0 - 4 * r1 + r2) / (2 * h)
      schemes[i] <- "backward"
      boundary[i] <- TRUE
    } else {
      stop(sprintf("Parameter %s has insufficient room inside its bounds", names(par_opt)[i]))
    }
    steps[i] <- h
  }

  # Also flag parameters close enough to a bound that a symmetric Wald interval
  # should not be presented as an ordinary two-sided confidence interval.
  finite_span <- is.finite(upper_b - lower_b) & (upper_b > lower_b)
  bound_tol <- pmax(10 * steps, ifelse(finite_span, (upper_b - lower_b) * 1e-7, 0))
  boundary <- boundary |
    (is.finite(lower_b) & par_opt - lower_b <= bound_tol) |
    (is.finite(upper_b) & upper_b - par_opt <= bound_tol)

  list(
    residuals = r0,
    jacobian = jacobian,
    steps = steps,
    schemes = schemes,
    boundary = boundary,
    lower = lower_b,
    upper = upper_b
  )
}

invert_information_matrix <- function(information, condition_limit = 1e12) {
  information <- (information + t(information)) / 2
  diagonal <- diag(information)
  if (length(diagonal) == 0L || any(!is.finite(diagonal)) || any(diagonal <= 0)) {
    return(list(
      inverse = NULL,
      rank = 0L,
      condition_number = Inf,
      positive_definite = FALSE,
      ill_conditioned = TRUE,
      eigenvalues = rep(NA_real_, ncol(information))
    ))
  }

  # Equilibrate by the information diagonal before decomposition.  This keeps
  # diagnostics meaningful when parameters use very different units (for
  # example logK versus cal/mol) and improves the numerical inverse.
  scale <- sqrt(diagonal)
  scaled_information <- information / outer(scale, scale)
  scaled_information <- (scaled_information + t(scaled_information)) / 2
  eig <- eigen(scaled_information, symmetric = TRUE)
  eigenvalues <- as.numeric(eig$values)
  max_eigen <- if (length(eigenvalues) > 0L) max(eigenvalues) else NA_real_
  rank_tol <- if (is.finite(max_eigen) && max_eigen > 0) {
    max(dim(information)) * .Machine$double.eps * max_eigen
  } else {
    Inf
  }
  rank <- sum(eigenvalues > rank_tol)
  full_rank <- rank == ncol(information)
  condition_number <- if (full_rank) max_eigen / min(eigenvalues) else Inf
  positive_definite <- full_rank && all(eigenvalues > 0)

  inverse <- NULL
  if (positive_definite) {
    scaled_inverse <- eig$vectors %*% diag(1 / eigenvalues, nrow = length(eigenvalues)) %*% t(eig$vectors)
    inverse <- scaled_inverse / outer(scale, scale)
    inverse <- (inverse + t(inverse)) / 2
  }

  list(
    inverse = inverse,
    rank = rank,
    condition_number = condition_number,
    positive_definite = positive_definite,
    ill_conditioned = !is.finite(condition_number) || condition_number > condition_limit,
    eigenvalues = eigenvalues
  )
}

#' Calculate parameter uncertainty from a residual-vector function
#'
#' Ordinary nonlinear least squares uses sigma^2 (J'J)^-1. Weighted and/or
#' Huber fits use a finite-sample-corrected sandwich covariance so the scalar
#' weighted/robust loss is never mislabeled as an ordinary RSS variance.
#'
#' @param residual_fun Function mapping a named parameter vector to raw residuals.
#' @param par_opt Named optimum parameter vector.
#' @param lower_b,upper_b Optional parameter bounds.
#' @param weights Optional fixed weights evaluated for the fitted observations.
#' @param use_huber Whether the fitted objective used Huber loss.
#' @param huber_delta Fixed Huber threshold used by the fit.
#' @param conf_level Confidence level.
#' @param optimizer_converged Whether the optimizer reported convergence.
#' @return A data frame with uncertainty columns and diagnostic attributes.
calculate_parameter_uncertainty <- function(residual_fun, par_opt, lower_b = NULL, upper_b = NULL,
                                            weights = NULL, use_huber = FALSE, huber_delta = NULL,
                                            conf_level = 0.95, optimizer_converged = TRUE) {
  original_names <- names(par_opt)
  par_opt <- suppressWarnings(as.numeric(par_opt))
  names(par_opt) <- if (is.null(original_names)) paste0("par", seq_along(par_opt)) else original_names
  n_par <- length(par_opt)
  warning_codes <- character(0)

  base_diagnostics <- list(
    method_version = ERROR_ANALYSIS_METHOD_VERSION,
    status = "invalid",
    method = if (isTRUE(use_huber) && !is.null(weights)) {
      "weighted_huber_sandwich"
    } else if (isTRUE(use_huber)) {
      "huber_sandwich"
    } else if (!is.null(weights)) {
      "weighted_sandwich"
    } else {
      "nls_jacobian"
    },
    warning_codes = "",
    n_data = NA_integer_,
    n_params = n_par,
    dof = NA_integer_,
    rank = NA_integer_,
    condition_number = NA_real_,
    boundary_params = ""
  )

  if (n_par == 0L || any(!is.finite(par_opt))) {
    base_diagnostics$warning_codes <- "invalid_parameters"
    return(build_error_analysis_result(par_opt, diagnostics = base_diagnostics))
  }

  fd <- tryCatch(
    calculate_residual_jacobian(residual_fun, par_opt, lower_b = lower_b, upper_b = upper_b),
    error = function(e) e
  )
  if (inherits(fd, "error")) {
    base_diagnostics$warning_codes <- "jacobian_failed"
    base_diagnostics$detail <- conditionMessage(fd)
    return(build_error_analysis_result(par_opt, diagnostics = base_diagnostics))
  }

  residuals <- fd$residuals
  jacobian <- fd$jacobian
  n_data <- length(residuals)
  dof <- n_data - n_par
  boundary_names <- names(par_opt)[fd$boundary]
  base_diagnostics$n_data <- n_data
  base_diagnostics$dof <- dof
  base_diagnostics$boundary_params <- paste(boundary_names, collapse = ",")

  if (dof <= 0L) {
    base_diagnostics$warning_codes <- "insufficient_dof"
    return(build_error_analysis_result(par_opt, diagnostics = base_diagnostics))
  }
  if (!isTRUE(optimizer_converged)) warning_codes <- c(warning_codes, "optimizer_not_converged")
  if (length(boundary_names) > 0L) warning_codes <- c(warning_codes, "boundary_parameters")

  weight_vec <- if (is.null(weights)) rep(1, n_data) else suppressWarnings(as.numeric(weights))
  if (length(weight_vec) != n_data || any(!is.finite(weight_vec)) || any(weight_vec <= 0)) {
    base_diagnostics$warning_codes <- paste(c(warning_codes, "invalid_weights"), collapse = "|")
    return(build_error_analysis_result(par_opt, diagnostics = base_diagnostics))
  }

  if (isTRUE(use_huber)) {
    delta <- suppressWarnings(as.numeric(huber_delta)[1])
    if (!is.finite(delta) || delta <= 0) {
      delta <- if (exists("calculate_huber_delta", mode = "function")) {
        calculate_huber_delta(residuals)
      } else {
        max(2 * stats::sd(residuals), 1e-6)
      }
    }
    psi <- ifelse(abs(residuals) <= delta, residuals, delta * sign(residuals))
    psi_prime <- as.numeric(abs(residuals) <= delta)
    information <- crossprod(jacobian, jacobian * (weight_vec * psi_prime))
    scores <- jacobian * (weight_vec * psi)
    warning_codes <- c(warning_codes, "sandwich_approximation")
  } else if (!is.null(weights)) {
    information <- crossprod(jacobian, jacobian * weight_vec)
    scores <- jacobian * (weight_vec * residuals)
    warning_codes <- c(warning_codes, "sandwich_approximation")
  } else {
    information <- crossprod(jacobian)
    scores <- NULL
  }

  matrix_info <- invert_information_matrix(information)
  base_diagnostics$rank <- matrix_info$rank
  base_diagnostics$condition_number <- matrix_info$condition_number
  if (!matrix_info$positive_definite) warning_codes <- c(warning_codes, "rank_deficient")
  if (matrix_info$ill_conditioned) warning_codes <- c(warning_codes, "ill_conditioned")

  if (is.null(matrix_info$inverse) || matrix_info$ill_conditioned) {
    base_diagnostics$warning_codes <- paste(unique(warning_codes), collapse = "|")
    return(build_error_analysis_result(par_opt, diagnostics = base_diagnostics))
  }

  if (is.null(scores)) {
    rss <- sum(residuals^2)
    sigma_sq <- rss / dof
    if (!is.finite(sigma_sq) || sigma_sq < 0) {
      warning_codes <- c(warning_codes, "invalid_residual_variance")
      base_diagnostics$warning_codes <- paste(unique(warning_codes), collapse = "|")
      return(build_error_analysis_result(par_opt, diagnostics = base_diagnostics))
    }
    cov_matrix <- sigma_sq * matrix_info$inverse
  } else {
    meat <- crossprod(scores)
    hc1 <- n_data / dof
    cov_matrix <- hc1 * matrix_info$inverse %*% meat %*% matrix_info$inverse
  }

  cov_matrix <- (cov_matrix + t(cov_matrix)) / 2
  if (any(!is.finite(cov_matrix))) {
    warning_codes <- c(warning_codes, "invalid_covariance")
    base_diagnostics$warning_codes <- paste(unique(warning_codes), collapse = "|")
    return(build_error_analysis_result(par_opt, diagnostics = base_diagnostics))
  }
  cov_eigen <- eigen(cov_matrix, symmetric = TRUE, only.values = TRUE)$values
  cov_tol <- max(1, max(abs(cov_eigen))) * max(dim(cov_matrix)) * .Machine$double.eps
  if (any(cov_eigen < -cov_tol) || any(diag(cov_matrix) < 0)) {
    warning_codes <- c(warning_codes, "invalid_covariance")
    base_diagnostics$warning_codes <- paste(unique(warning_codes), collapse = "|")
    return(build_error_analysis_result(par_opt, diagnostics = base_diagnostics))
  }

  param_se <- sqrt(pmax(diag(cov_matrix), 0))
  t_crit <- stats::qt((1 + conf_level) / 2, df = dof)
  ci_lower <- par_opt - t_crit * param_se
  ci_upper <- par_opt + t_crit * param_se
  interval_crosses_bounds <-
    (is.finite(fd$lower) & ci_lower < fd$lower) |
    (is.finite(fd$upper) & ci_upper > fd$upper)
  suppress_interval <- fd$boundary | interval_crosses_bounds
  if (any(interval_crosses_bounds)) {
    warning_codes <- c(warning_codes, "interval_crosses_bounds")
  }
  if (any(suppress_interval)) {
    ci_lower[suppress_interval] <- NA_real_
    ci_upper[suppress_interval] <- NA_real_
  }

  status <- if (dof < 5L || any(c("optimizer_not_converged", "boundary_parameters", "ill_conditioned", "interval_crosses_bounds") %in% warning_codes)) {
    "low"
  } else if (length(warning_codes) > 0L || dof < 10L) {
    "moderate"
  } else {
    "good"
  }
  base_diagnostics$status <- status
  base_diagnostics$warning_codes <- paste(unique(warning_codes), collapse = "|")

  build_error_analysis_result(
    par_opt,
    se = param_se,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    cov_matrix = cov_matrix,
    diagnostics = base_diagnostics
  )
}

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
    
      # 原始 RSS 的 Hessian 约为 2 J'J，因此 Cov = 2 sigma^2 H^-1。
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
    
    # obj_fun is raw RSS, whose Hessian is approximately 2 J'J.
    cov_matrix <- 2 * sigma_sq * hessian_inv
    
    # 提取对角线元素 (参数方差)
    param_var <- diag(cov_matrix)
    if (any(!is.finite(param_var)) || any(param_var < 0)) return(NULL)
    param_se <- sqrt(param_var)  # 标准误差
    
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
#' 2. 参数协方差矩阵：Cov(θ) = 2σ² × H⁻¹（H 为原始 RSS 的 Hessian）
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
        eps_i <- eps_base * max(abs(par_opt[i]), 1)
        eps_j <- eps_base * max(abs(par_opt[j]), 1)
        
        par_pp <- par_opt
        par_pm <- par_opt
        par_mp <- par_opt
        par_mm <- par_opt
        
        par_pp[i] <- par_pp[i] + eps_i
        par_pp[j] <- par_pp[j] + eps_j
        
        par_pm[i] <- par_pm[i] + eps_i
        par_pm[j] <- par_pm[j] - eps_j
        
        par_mp[i] <- par_mp[i] - eps_i
        par_mp[j] <- par_mp[j] + eps_j
        
        par_mm[i] <- par_mm[i] - eps_i
        par_mm[j] <- par_mm[j] - eps_j
        
        f_pp <- obj_fun(par_pp)
        f_pm <- obj_fun(par_pm)
        f_mp <- obj_fun(par_mp)
        f_mm <- obj_fun(par_mm)
        
        # 检查函数值是否合理
        if (all(is.finite(c(f_pp, f_pm, f_mp, f_mm)))) {
          hessian[i, j] <- (f_pp - f_pm - f_mp + f_mm) / (4 * eps_i * eps_j)
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
    
    # obj_fun is raw RSS, whose Hessian is approximately 2 J'J.
    cov_matrix <- 2 * sigma_sq * hessian_inv
    # 设置协方差矩阵的行列名（确保参数名正确）
    rownames(cov_matrix) <- names(par_opt)
    colnames(cov_matrix) <- names(par_opt)
    
    param_var <- diag(cov_matrix)
    if (any(!is.finite(param_var)) || any(param_var < 0)) return(NULL)
    param_se <- sqrt(param_var)
    
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
