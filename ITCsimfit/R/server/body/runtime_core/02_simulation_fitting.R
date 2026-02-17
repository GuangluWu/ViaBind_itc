  # [修复] 语言切换时，标签通过 uiOutput 自动更新，不需要手动更新滑条的 label
  # 移除此 observeEvent，因为 output$fit_data_range_slider_label 已经会根据 lang() 自动更新
  
  # 模拟计算包装函数
  # 负责将 UI 参数转换为物理参数 (应用 fH, fG 修正)
  # 并计算 Apparent Ratio 用于绘图
#   calculate_simulation <- function(p, active_paths) {
#     p_true <- p
#     # [浓度修正] 真实浓度 = 名义浓度 * 修正因子
#     p_true$H_cell_0 <- p$H_cell_0 * p$fH * 1e-6
#     p_true$G_syringe <- p$G_syringe * p$fG * 1e-6
#     p_true$V_inj <- p$V_inj / 1000
#     
#     paths <- unique(c("rxn_M", active_paths))
#     res_raw <- run_sim_modular(p_true, paths)
#     if(is.null(res_raw)) return(NULL)
#     
#     # [Apparent Ratio 回算]
#     # Sim 内部使用的是真实浓度计算的 Ratio (Ratio_True)。
#     # 用户看到的 Ratio_App 是基于名义浓度的。
#     # Ratio_True = (G_nom * fG) / (H_nom * fH) = Ratio_App * (fG/fH)
#     # 所以: Ratio_App = Ratio_True * (fH/fG)
#     res_raw$Ratio_App <- res_raw$Ratio * (p$fH / p$fG)
#     
#     # [热量修正] dQ_App = dQ_True * fG + Offset
#     # (假设热量与注射物浓度成正比，故乘以 fG)
#     res_raw$dQ_App    <- (res_raw$dQ * p$fG) + p$Offset
#     return(res_raw)
#   }
  
  sim_results <- reactive({
    # 确保所有必需的输入值都已初始化
    req(input$V_cell, input$V_inj, input$H_cell_0, input$G_syringe, input$n_inj,
        input$logK1, input$H1, input$factor_H, input$factor_G)
    
    # [缓存机制] 构建参数签名作为缓存键
    # 包含所有影响模拟结果的参数
    # 安全处理 active_paths（可能为 NULL）
    active_paths_safe <- if(is.null(input$active_paths) || length(input$active_paths) == 0) {
      character(0)
    } else {
      input$active_paths
    }
    active_paths_str <- if(length(active_paths_safe) > 0) {
      paste(sort(active_paths_safe), collapse=",")
    } else {
      ""
    }
    
    # 安全获取所有输入值，使用默认值防止NULL
    safe_input <- function(x, default) {
      if(is.null(x) || length(x) == 0 || is.na(x)) default else x
    }
    
    exp_df_cache <- tryCatch(exp_data_processed(), error = function(e) NULL)
    inj_vec_cache <- if (!is.null(exp_df_cache) && "V_inj_uL" %in% names(exp_df_cache)) {
      exp_df_cache$V_inj_uL
    } else {
      rep(safe_input(input$V_inj, UI_DEFAULTS$v_inj_default * 1000), safe_input(input$n_inj, UI_DEFAULTS$n_inj_default))
    }
    n_inj_cache <- if (!is.null(exp_df_cache) && "V_inj_uL" %in% names(exp_df_cache)) {
      length(inj_vec_cache)
    } else {
      safe_input(input$n_inj, UI_DEFAULTS$n_inj_default)
    }
    inj_vec_key <- paste(inj_vec_cache, collapse = ",")

    cache_key <- paste(
      safe_input(input$H_cell_0, UI_DEFAULTS$conc_cell_default), 
      safe_input(input$G_syringe, UI_DEFAULTS$conc_syringe_default), 
      safe_input(input$V_cell, UI_DEFAULTS$v_cell_default), 
      # [注释] V_inj 在 UI 中是 uL，缓存键也用 uL 值（与 UI 一致）
      safe_input(input$V_inj, UI_DEFAULTS$v_inj_default * 1000), 
      n_inj_cache,
      safe_input(input$V_init_val, 0),
      inj_vec_key,
      safe_input(input$logK1, DEFAULT_PARAMS$logK), safe_input(input$H1, DEFAULT_PARAMS$H), 
      safe_input(input$logK2, DEFAULT_PARAMS$logK), safe_input(input$H2, DEFAULT_PARAMS$H), 
      safe_input(input$logK3, DEFAULT_PARAMS$logK), safe_input(input$H3, DEFAULT_PARAMS$H),
      safe_input(input$logK4, DEFAULT_PARAMS$logK), safe_input(input$H4, DEFAULT_PARAMS$H), 
      safe_input(input$logK5, DEFAULT_PARAMS$logK), safe_input(input$H5, DEFAULT_PARAMS$H),
      safe_input(input$logK6, DEFAULT_PARAMS$logK), safe_input(input$H6, DEFAULT_PARAMS$H),
      safe_input(input$factor_H, DEFAULT_PARAMS$fH), safe_input(input$factor_G, DEFAULT_PARAMS$fG), 
      safe_input(input$heat_offset, DEFAULT_PARAMS$Offset),
      active_paths_str,
      sep = "|"
    )
    
    # [缓存机制] 检查缓存键是否相同
    last_cache_key <- sim_cache_key()
    if (!is.null(last_cache_key) && identical(cache_key, last_cache_key)) {
      cached_result <- sim_cache_result()
      if (!is.null(cached_result)) {
        return(cached_result)
      }
    }
    
    # [缓存机制] 计算新结果
    exp_df <- tryCatch(exp_data_processed(), error = function(e) NULL)
    V_inj_vec <- if (!is.null(exp_df) && "V_inj_uL" %in% names(exp_df)) {
      exp_df$V_inj_uL
    } else {
      rep(safe_input(input$V_inj, UI_DEFAULTS$v_inj_default * 1000), safe_input(input$n_inj, UI_DEFAULTS$n_inj_default))
    }
    if (any(is.na(V_inj_vec))) {
      V_inj_vec[is.na(V_inj_vec)] <- safe_input(input$V_inj, UI_DEFAULTS$v_inj_default * 1000)
    }
    n_inj_effective <- if (!is.null(exp_df) && "V_inj_uL" %in% names(exp_df)) {
      length(V_inj_vec)
    } else {
      safe_input(input$n_inj, UI_DEFAULTS$n_inj_default)
    }
    if (length(V_inj_vec) > 0) {
      V_inj_vec[1] <- safe_input(input$V_init_val, DEFAULT_PARAMS$V_init)
    }

    p_curr <- list(
      H_cell_0=safe_input(input$H_cell_0, UI_DEFAULTS$conc_cell_default), 
      G_syringe=safe_input(input$G_syringe, UI_DEFAULTS$conc_syringe_default), 
      V_cell=safe_input(input$V_cell, UI_DEFAULTS$v_cell_default), 
      n_inj=n_inj_effective,
      # [重要] V_inj_vec 在 UI/导入中是 uL，calculate_simulation 内部会除以 1000，所以这里不转换
      V_inj_vec = V_inj_vec,
      logK1=safe_input(input$logK1, DEFAULT_PARAMS$logK), H1=safe_input(input$H1, DEFAULT_PARAMS$H), 
      logK2=safe_input(input$logK2, DEFAULT_PARAMS$logK), H2=safe_input(input$H2, DEFAULT_PARAMS$H), 
      logK3=safe_input(input$logK3, DEFAULT_PARAMS$logK), H3=safe_input(input$H3, DEFAULT_PARAMS$H),
      logK4=safe_input(input$logK4, DEFAULT_PARAMS$logK), H4=safe_input(input$H4, DEFAULT_PARAMS$H), 
      logK5=safe_input(input$logK5, DEFAULT_PARAMS$logK), H5=safe_input(input$H5, DEFAULT_PARAMS$H),
      logK6=safe_input(input$logK6, DEFAULT_PARAMS$logK), H6=safe_input(input$H6, DEFAULT_PARAMS$H),
      fH=safe_input(input$factor_H, DEFAULT_PARAMS$fH), fG=safe_input(input$factor_G, DEFAULT_PARAMS$fG), 
      Offset=safe_input(input$heat_offset, DEFAULT_PARAMS$Offset)
    )
    result <- tryCatch({
      calculate_simulation(p_curr, active_paths_safe)
    }, error = function(e) {
      # 只在非启动阶段显示错误（避免启动时的初始化错误干扰用户）
      # 检查是否是启动阶段的错误（某些输入可能还未完全初始化）
      is_startup_error <- any(
        is.null(input$V_cell) || is.null(input$V_inj) || 
        is.null(input$logK1) || is.null(input$H1) ||
        is.null(input$factor_H) || is.null(input$factor_G)
      )
      
      if (!is_startup_error) {
        # 安全获取翻译文本，确保不为空
        # 在 reactive 上下文中，直接使用 current_lang() 而不是 lang()
        current_lang_val <- tryCatch({
          lang_val <- current_lang()
          if(is.null(lang_val) || length(lang_val) == 0 || !lang_val %in% c("zh", "en")) "en" else lang_val
        }, error = function(e) "en")
        
        calc_error_text <- tr("calc_error", current_lang_val)
        if(is.null(calc_error_text) || length(calc_error_text) == 0 || calc_error_text == "" || nchar(calc_error_text) == 0) {
          calc_error_text <- "Calculation failed: "
        }
        
        calc_suggestion_text <- tr("calc_error_suggestion", current_lang_val)
        if(is.null(calc_suggestion_text) || length(calc_suggestion_text) == 0 || calc_suggestion_text == "" || nchar(calc_suggestion_text) == 0) {
          calc_suggestion_text <- "Suggestion: check whether parameter values are within a valid range."
        }
        
        error_msg <- paste0(calc_error_text, e$message, "\n", calc_suggestion_text)
        showNotification(
          error_msg,
          type = "error",
          duration = 10
        )
      }
      return(NULL)
    })
    if (is.null(result)) return(NULL)
    if (!is.null(V_inj_vec)) {
      result$V_inj_uL <- V_inj_vec[seq_len(nrow(result))]
    }

    # [缓存机制] 更新缓存
    sim_cache_key(cache_key)
    sim_cache_result(result)

    return(result)
  })
  
  # 内部拟合函数 - 包含所有拟合逻辑
  perform_fitting_sync <- function(max_iters, use_DE, 
                                    exp_df, p_curr, fixed_p, params_to_opt, 
                                    range_lim, active_paths, enable_error_analysis, 
                                    use_weighted = FALSE, use_robust = FALSE, huber_delta = NULL,
                                    progress = NULL, lang_val = "en") {
    # 确保语言值有效
    if(is.null(lang_val) || length(lang_val) == 0 || !lang_val %in% c("zh", "en")) {
      lang_val <- "en"
    }
    par_vec <- unlist(p_curr[params_to_opt])
    fit_fixed_p <- fixed_p
    
    # 预计算权重（如果启用加权拟合）
    weights <- NULL
    if(isTRUE(use_weighted)) {
      max_idx_pre <- min(nrow(exp_df), max(range_lim))
      valid_idx_pre <- range_lim[1]:range_lim[2]
      valid_idx_pre <- valid_idx_pre[valid_idx_pre <= max_idx_pre]
      if(length(valid_idx_pre) > 0) {
        weights <- calculate_weights_from_derivative(exp_df, valid_idx_pre)
      }
    }
    
    # 目标函数 (Objective Function): 计算加权残差平方和 (RSS)
    # 采用 Needle-based 拟合：直接比较对应针数的热量值
    obj_fun <- function(par) {
      current_vars <- p_curr
      current_vars[params_to_opt] <- par
      full_p <- c(current_vars, fit_fixed_p)
      if (!is.null(full_p$V_inj_vec) && !is.null(current_vars$V_init)) {
        full_p$V_inj_vec[1] <- current_vars$V_init
      }
      
      # 1. 运行模拟
      sim_res <- tryCatch({ calculate_simulation(full_p, active_paths) }, error=function(e) NULL)
      if(is.null(sim_res) || any(!is.finite(sim_res$dQ_App))) return(1e20) # 惩罚项
      
      # 2. 针数对齐 (Injection Alignment)
      # 仅比较用户选定范围内的针数 (Needle Index)
      max_idx <- min(nrow(sim_res), nrow(exp_df))
      valid_idx <- range_lim[1]:range_lim[2]
      valid_idx <- valid_idx[valid_idx <= max_idx]
      
      if(length(valid_idx)==0) return(1e20)
      
      # 3. 计算残差
      residuals <- sim_res$dQ_App[valid_idx] - exp_df$Heat_Raw[valid_idx]
      
      # 4. 应用加权和/或鲁棒回归
      # 如果启用加权，使用预计算的权重（需要重新索引以匹配当前valid_idx）
      current_weights <- NULL
      if(isTRUE(use_weighted) && !is.null(weights)) {
        # 重新计算权重以匹配当前的valid_idx（因为valid_idx可能因模拟结果而变化）
        current_weights <- calculate_weights_from_derivative(exp_df, valid_idx)
      }
      
      # 计算损失
      loss <- calculate_weighted_robust_loss(
        residuals = residuals,
        weights = current_weights,
        use_huber = isTRUE(use_robust),
        huber_delta = huber_delta
      )
      
      return(loss)
    }
    
    # 更新进度：预检查阶段
    if(!is.null(progress)) {
      progress$set(value = 0.1, detail = tr("fit_progress_precheck", lang_val))
    }
    
    # 拟合前预检 (Pre-check) - 增强诊断信息
    # 先进行详细检查，提供更具体的错误信息
    full_p_check <- c(p_curr, fit_fixed_p)
    if (!is.null(full_p_check$V_inj_vec) && !is.null(p_curr$V_init)) {
      full_p_check$V_inj_vec[1] <- p_curr$V_init
    }
    sim_res_check <- tryCatch({ 
      calculate_simulation(full_p_check, active_paths) 
    }, error = function(e) {
      return(list(error_msg = paste(tr("fit_error_sim_calc", lang_val), e$message)))
    })
    
    # 检查1: 模拟是否成功
    if(is.list(sim_res_check) && !is.null(sim_res_check$error_msg)) {
      return(list(error = paste(tr("fit_error_cannot_start", lang_val), sim_res_check$error_msg)))
    }
    if(is.null(sim_res_check)) {
      return(list(error = tr("fit_error_sim_null", lang_val)))
    }
    
    # 检查2: 模拟结果是否有效
    if(any(!is.finite(sim_res_check$dQ_App))) {
      n_inf <- sum(!is.finite(sim_res_check$dQ_App))
      return(list(error = trf("fit_error_invalid_values", lang_val, count = n_inf)))
    }
    
    # 检查3: 拟合区间是否有效
    max_idx_check <- min(nrow(sim_res_check), nrow(exp_df))
    valid_idx_check <- range_lim[1]:range_lim[2]
    valid_idx_check <- valid_idx_check[valid_idx_check <= max_idx_check]
    if(length(valid_idx_check) == 0) {
      return(list(error = trf("fit_error_invalid_range", lang_val, 
                              sim_count = nrow(sim_res_check), 
                              exp_count = nrow(exp_df),
                              start = range_lim[1], 
                              end = range_lim[2])))
    }
    
    # 检查4: 计算初始RSS
    initial_rss <- obj_fun(par_vec)
    if(initial_rss >= 1e19) {
      # 尝试计算实际RSS以提供更多信息
      tryCatch({
        actual_rss <- sum((sim_res_check$dQ_App[valid_idx_check] - exp_df$Heat_Raw[valid_idx_check])^2)
        if(is.finite(actual_rss)) {
          return(list(error = trf("fit_error_rss_too_large", lang_val, rss = formatC(actual_rss, format="e", digits=2))))
        }
      }, error = function(e) {})
      return(list(error = tr("fit_error_no_match", lang_val)))
    }
    
    # 使用会话有效边界（用户可编辑）获取参数边界
    # [注释] V_inj 在 UI 中是 uL，get_effective_parameter_bounds 也期望 uL
    v_inj_val <- safe_numeric(input$V_inj, default = UI_DEFAULTS$v_inj_default * 1000, min = 0)
    bounds <- get_effective_parameter_bounds(names(par_vec), v_inj = v_inj_val)
    lower_b <- bounds$lower
    upper_b <- bounds$upper
    
    # 记录边界信息（用于调试）
    log_info(sprintf("参数边界设置完成，共 %d 个参数待优化", length(par_vec)), 
            context = "perform_fitting_sync")
    
    # 更新进度：开始拟合
    if(!is.null(progress)) {
      algo_name <- if(use_DE) tr("fit_progress_de", lang_val) else tr("fit_progress_lbfgsb", lang_val)
      progress$set(value = 0.2, detail = algo_name)
    }
    
    res <- NULL
    
    if (use_DE) {
      # --- DE (Differential Evolution) 全局搜索 ---
      if (!requireNamespace("DEoptim", quietly = TRUE)) {
        handle_error(
          simpleError("DEoptim 包未安装"),
          context = "依赖检查",
          lang_val = lang_val,
          show_to_user = FALSE
        )
        return(list(error = tr("fit_error_deoptim_required", lang_val)))
      }
      
      # 包装 obj_fun 以便 DEoptim 调用 (DEoptim 可能会传入无名向量)
      de_obj_fun <- function(p) {
        names(p) <- names(par_vec) # 恢复名字
        obj_fun(p)
      }
      
      # 使用常量定义计算种群大小
      pop_size <- max(
        DE_OPTIM$pop_size_multiplier * length(par_vec),
        DE_OPTIM$min_pop_size
      )
      # 限制最大种群大小（防止过大导致性能问题）
      pop_size <- min(pop_size, DE_OPTIM$max_pop_size)
      
      log_info(sprintf("DE优化参数：种群大小=%d, 最大迭代=%d", pop_size, max_iters),
              context = "perform_fitting_sync")
      
      # 使用 safe_execute 包装 DEoptim 调用
      out <- safe_execute({
        DEoptim::DEoptim(
          fn = de_obj_fun,
          lower = lower_b,
          upper = upper_b,
          control = list(
            itermax = max_iters,
            NP = pop_size,
            trace = DE_OPTIM$trace,
            F = DE_OPTIM$F,
            CR = DE_OPTIM$CR,
            strategy = DE_OPTIM$strategy
          )
        )
      },
      context = "DE全局优化",
      lang_val = lang_val,
      show_error = FALSE,  # 不立即显示错误，由后续代码处理
      default = NULL
      )
      
      # 检查优化是否成功
      if(is.null(out)) {
        log_warning("DE优化失败", context = "perform_fitting_sync")
        return(list(error = tr("fit_error_process", lang_val)))
      }
      
      # 提取结果并适配格式
      res <- list(
        par = out$optim$bestmem,
        value = out$optim$bestval
      )
      names(res$par) <- names(par_vec) # 确保名字正确
      
      log_info(sprintf("DE优化完成，目标函数值=%.4f", res$value),
              context = "perform_fitting_sync")
      
      # 更新进度：DE拟合完成
      if(!is.null(progress)) {
        progress$set(value = 0.8, detail = tr("fit_progress_de_done", lang_val))
      }
      
    } else {
      # --- 常规 L-BFGS-B 局部优化 ---
      log_info(sprintf("L-BFGS-B优化参数：最大迭代=%d", max_iters),
              context = "perform_fitting_sync")
      
      res <- safe_execute({
        optim(
          par = par_vec,
          fn = obj_fun,
          method = "L-BFGS-B",
          lower = lower_b,
          upper = upper_b,
          control = list(
            factr = LBFGS_OPTIM$factr,
            maxit = max_iters,
            pgtol = LBFGS_OPTIM$pgtol
          )
        )
      },
      context = "L-BFGS-B局部优化",
      lang_val = lang_val,
      show_error = FALSE,  # 不立即显示错误，由后续代码处理
      default = NULL
      )
      
      # 检查优化是否成功
      if(!is.null(res)) {
        log_info(sprintf("L-BFGS-B优化完成，目标函数值=%.4f, 收敛=%d", 
                        res$value, res$convergence),
                context = "perform_fitting_sync")
      } else {
        log_warning("L-BFGS-B优化失败", context = "perform_fitting_sync")
      }
      
      # 更新进度：L-BFGS-B拟合完成
      if(!is.null(progress)) {
        progress$set(value = 0.8, detail = tr("fit_progress_lbfgsb_done", lang_val))
      }
    }
    
    if(is.null(res)) {
      return(list(error = tr("fit_error_process", lang_val)))
    }
    
    # 更新进度：准备返回结果
    if(!is.null(progress)) {
      progress$set(value = 0.9, detail = tr("fit_progress_prepare", lang_val))
    }
    
    # 准备返回结果
    result <- list(
      res = res,
      obj_fun = obj_fun,
      par_vec = par_vec,
      range_lim = range_lim,
      exp_df = exp_df,
      enable_error_analysis = enable_error_analysis
    )
    
    return(result)
  }
  
  # 同步拟合函数 - 使用进度条提示用户等待
  perform_fitting <- function(max_iters, use_DE = FALSE) {
    # 允许实验数据来自文件导入或 模拟→实验；无数据时阻止拟合
    exp_df <- exp_data_processed()
    req(!is.null(exp_df), nrow(exp_df) > 0)

    V_inj_vec <- if ("V_inj_uL" %in% names(exp_df)) {
      exp_df$V_inj_uL
    } else {
      rep(safe_numeric(input$V_inj, default = UI_DEFAULTS$v_inj_default * 1000, 
                       min = UI_DEFAULTS$v_inj_min * 1000, max = UI_DEFAULTS$v_inj_max * 1000),
          nrow(exp_df))
    }
    if (any(is.na(V_inj_vec))) {
      V_inj_vec[is.na(V_inj_vec)] <- safe_numeric(input$V_inj, default = UI_DEFAULTS$v_inj_default * 1000, 
                                                  min = UI_DEFAULTS$v_inj_min * 1000, max = UI_DEFAULTS$v_inj_max * 1000)
    }
    
    # 检查是否已有拟合在进行
    if(values$is_fitting) {
      showNotification(tr("fit_error_in_progress", lang()), type = "warning", duration = 3)
      return()
    }
    
    values$is_fitting <- TRUE
    
    # 使用 safe_numeric 准备当前参数，确保输入有效
    get_bound <- function(param_name) {
      get_effective_param_bound(param_name)
    }
    b_logK1 <- get_bound("logK1"); b_H1 <- get_bound("H1")
    b_logK2 <- get_bound("logK2"); b_H2 <- get_bound("H2")
    b_logK3 <- get_bound("logK3"); b_H3 <- get_bound("H3")
    b_logK4 <- get_bound("logK4"); b_H4 <- get_bound("H4")
    b_logK5 <- get_bound("logK5"); b_H5 <- get_bound("H5")
    b_logK6 <- get_bound("logK6"); b_H6 <- get_bound("H6")
    b_Offset <- get_bound("Offset")

    p_curr <- list(
      logK1 = safe_numeric(input$logK1, default = DEFAULT_PARAMS$logK, 
                          min = b_logK1["lower"], max = b_logK1["upper"]),
      H1 = safe_numeric(input$H1, default = DEFAULT_PARAMS$H, 
                       min = b_H1["lower"], max = b_H1["upper"]),
      logK2 = safe_numeric(input$logK2, default = DEFAULT_PARAMS$logK, 
                          min = b_logK2["lower"], max = b_logK2["upper"]),
      H2 = safe_numeric(input$H2, default = DEFAULT_PARAMS$H, 
                       min = b_H2["lower"], max = b_H2["upper"]),
      logK3 = safe_numeric(input$logK3, default = DEFAULT_PARAMS$logK, 
                          min = b_logK3["lower"], max = b_logK3["upper"]),
      H3 = safe_numeric(input$H3, default = DEFAULT_PARAMS$H, 
                       min = b_H3["lower"], max = b_H3["upper"]),
      logK4 = safe_numeric(input$logK4, default = DEFAULT_PARAMS$logK, 
                          min = b_logK4["lower"], max = b_logK4["upper"]),
      H4 = safe_numeric(input$H4, default = DEFAULT_PARAMS$H, 
                       min = b_H4["lower"], max = b_H4["upper"]),
      logK5 = safe_numeric(input$logK5, default = DEFAULT_PARAMS$logK, 
                          min = b_logK5["lower"], max = b_logK5["upper"]),
      H5 = safe_numeric(input$H5, default = DEFAULT_PARAMS$H, 
                       min = b_H5["lower"], max = b_H5["upper"]),
      logK6 = safe_numeric(input$logK6, default = DEFAULT_PARAMS$logK, 
                          min = b_logK6["lower"], max = b_logK6["upper"]),
      H6 = safe_numeric(input$H6, default = DEFAULT_PARAMS$H, 
                       min = b_H6["lower"], max = b_H6["upper"]),
      fH = safe_numeric(input$factor_H, default = DEFAULT_PARAMS$fH, 
                       min = PARAM_BOUNDS$fH_fG["lower"], max = PARAM_BOUNDS$fH_fG["upper"]),
      fG = safe_numeric(input$factor_G, default = DEFAULT_PARAMS$fG, 
                       min = PARAM_BOUNDS$fH_fG["lower"], max = PARAM_BOUNDS$fH_fG["upper"]),
      V_init = safe_numeric(input$V_init_val, default = DEFAULT_PARAMS$V_init, min = 0),
      Offset = safe_numeric(input$heat_offset, default = DEFAULT_PARAMS$Offset, 
                           min = b_Offset["lower"], max = b_Offset["upper"])
    )
    
    # 使用 safe_numeric 准备固定参数
    # 注意：这些参数将传给 calculate_simulation，需要保持 UI 单位（浓度为 mM，体积为 uL）
    fixed_p <- list(
      H_cell_0 = safe_numeric(input$H_cell_0, default = UI_DEFAULTS$conc_cell_default, min = 0),
      G_syringe = safe_numeric(input$G_syringe, default = UI_DEFAULTS$conc_syringe_default, min = 0),
      V_cell = safe_numeric(input$V_cell, default = UI_DEFAULTS$v_cell_default, 
                           min = UI_DEFAULTS$v_cell_min, max = UI_DEFAULTS$v_cell_max),
      # [重要] V_inj_vec 在导入数据中是 uL，calculate_simulation 内部会除以 1000，所以这里不转换
      V_inj_vec = V_inj_vec,
      n_inj = length(V_inj_vec)
    )
    
    # 确定要优化的参数子集
    params_to_opt <- input$fit_params
    range_lim <- input$fit_data_range # 针数范围 c(start_idx, end_idx)
    if (length(params_to_opt) == 0) {
      values$is_fitting <- FALSE
      return()
    }
    
    # 确定算法名称
    algo_name <- if(use_DE) tr("fit_progress_de", lang()) else tr("fit_progress_lbfgsb", lang())
    
    # 创建进度对象
    progress <- Progress$new(session, min = 0, max = 1)
    progress$set(value = 0, detail = algo_name)
    
    # 确保在函数退出时关闭进度条和重置状态
    on.exit({ 
      values$is_fitting <- FALSE
      progress$close()
    })
    
    # 获取加权和鲁棒回归设置
    use_weighted <- isTRUE(input$use_weighted_fitting)
    use_robust <- isTRUE(input$use_robust_fitting)
    # [修复] 使用常量定义的 huber_delta 参数
    huber_delta_input <- if(use_robust) {
      safe_numeric(input$huber_delta, 
                   default = HUBER_PARAMS$delta_default, 
                   min = HUBER_PARAMS$delta_min, 
                   max = HUBER_PARAMS$delta_max)
    } else {
      NULL
    }
    
    # 验证 max_iters 参数
    max_iters <- safe_numeric(max_iters, 
                             default = if(use_DE) DE_OPTIM$max_iter else LBFGS_OPTIM$max_iter,
                             min = 10, 
                             max = 1000)
    
    # 安全获取当前语言值
    current_lang_val <- tryCatch(lang(), error = function(e) "en")
    if(is.null(current_lang_val) || length(current_lang_val) == 0 || !current_lang_val %in% c("zh", "en")) {
      current_lang_val <- "en"
    }
    
    result <- perform_fitting_sync(
      max_iters = max_iters,
      use_DE = use_DE,
      exp_df = exp_df,
      p_curr = p_curr,
      fixed_p = fixed_p,
      params_to_opt = params_to_opt,
      range_lim = range_lim,
      active_paths = input$active_paths,
      enable_error_analysis = if(!is.null(input$enable_error_analysis)) input$enable_error_analysis else FALSE,
      use_weighted = use_weighted,
      use_robust = use_robust,
      huber_delta = huber_delta_input,
      progress = progress,
      lang_val = current_lang_val
    )
    
    # 处理结果
    if(!is.null(result$error)) {
      progress$set(value = 1, detail = tr("fit_progress_error", lang()))
      showNotification(result$error, type = "error", duration = 10)
      return()
    }
    
    # 更新进度：处理结果
    progress$set(value = 0.95, detail = tr("fit_progress_update", lang()))
    
    # 更新参数和误差分析
    res <- result$res
    obj_fun <- result$obj_fun
    par_vec <- result$par_vec
    range_lim <- result$range_lim
    exp_df <- result$exp_df
    enable_error_analysis <- result$enable_error_analysis
    
    # 更新参数值
    new_vals <- res$par
    if (is.null(names(new_vals)) && length(new_vals) == length(par_vec)) names(new_vals) <- names(par_vec)
    for(nm in names(new_vals)) {
      val <- new_vals[[nm]]
      if(grepl("Offset", nm)) updateSliderInput(session, "heat_offset", value=val)
      else if(nm=="fH") updateNumericInput(session, "factor_H", value=val)
      else if(nm=="fG") updateNumericInput(session, "factor_G", value=val)
      else if(nm=="V_init") updateNumericInput(session, "V_init_val", value=val)
      else updateSliderInput(session, nm, value=val)
    }
    
    # 误差分析
    if (enable_error_analysis) {
      # [修复] 使用翻译函数而不是硬编码中文
      progress$set(value = 0.97, detail = tr("fit_progress_error_analysis", lang()))
      tryCatch({
        valid_idx <- range_lim[1]:range_lim[2]
        max_idx <- min(nrow(exp_df), max(valid_idx, na.rm = TRUE))
        valid_idx <- valid_idx[valid_idx <= max_idx]
        n_data <- length(valid_idx)
        
        if (n_data <= length(new_vals)) {
          values$error_analysis <- NULL
          values$error_analysis_info <- NULL
          values$correlation_matrix <- NULL
          showNotification(tr("fit_error_insufficient_data", lang()), type = "warning", duration = 3)
        } else {
          final_rss <- res$value
          error_result <- calculate_hessian_ci_robust(
            obj_fun = obj_fun,
            par_opt = new_vals,
            n_data = n_data,
            rss = final_rss,
            conf_level = 0.95
          )
          
          if (!is.null(error_result) && nrow(error_result) > 0) {
            # 保存完整的误差分析结果（用于相关性矩阵计算）
            error_result_full <- error_result
            # [Updated] Use full error result in values$error_analysis for Report and Snapshots
            # Filtering for table display is moved to renderDT
            
            if (nrow(error_result) > 0) {
              values$error_analysis <- error_result
              dof <- n_data - length(new_vals)
              # 不存储翻译后的 reliability 文案，仅存 dof/color；渲染时按当前 lang() 用 tr() 取文，以便切换语言后正确更新
              reliability_color <- if (dof >= 10) "#27ae60" else if (dof >= 5) "#f39c12" else "#e74c3c"
              
              values$error_analysis_info <- list(
                n_data = n_data,
                n_params = length(new_vals),
                dof = dof,
                reliability_color = reliability_color
              )
              
              # [新增] 提取协方差矩阵并计算相关性矩阵
              # 使用完整的误差分析结果来获取协方差矩阵（包含所有拟合参数）
              cov_matrix <- attr(error_result_full, "cov_matrix")
              if (!is.null(cov_matrix) && is.matrix(cov_matrix)) {
                tryCatch({
                  # 使用所有拟合参数（new_vals 包含所有被拟合的参数）
                  param_names <- names(new_vals)
                  
                  # 如果协方差矩阵有行列名，使用它们（更可靠）
                  if (!is.null(rownames(cov_matrix)) && !is.null(colnames(cov_matrix))) {
                    # 确保行列名与参数名匹配
                    if (all(param_names %in% rownames(cov_matrix)) && 
                        all(param_names %in% colnames(cov_matrix))) {
                      # 按照参数顺序提取协方差矩阵
                      cov_matrix_ordered <- cov_matrix[param_names, param_names, drop = FALSE]
                    } else {
                      # 如果行列名不匹配，但维度匹配，直接使用
                      if (nrow(cov_matrix) == length(param_names) && 
                          ncol(cov_matrix) == length(param_names)) {
                        cov_matrix_ordered <- cov_matrix
                        rownames(cov_matrix_ordered) <- param_names
                        colnames(cov_matrix_ordered) <- param_names
                      } else {
                        cov_matrix_ordered <- NULL
                      }
                    }
                  } else {
                    # 如果协方差矩阵没有行列名，但维度匹配，直接使用并设置行列名
                    if (nrow(cov_matrix) == length(param_names) && 
                        ncol(cov_matrix) == length(param_names)) {
                      cov_matrix_ordered <- cov_matrix
                      rownames(cov_matrix_ordered) <- param_names
                      colnames(cov_matrix_ordered) <- param_names
                    } else {
                      cov_matrix_ordered <- NULL
                    }
                  }
                  
                  if (!is.null(cov_matrix_ordered)) {
                    # 检查协方差矩阵的对角线是否都是正数（方差必须为正）
                    diag_vals <- diag(cov_matrix_ordered)
                    valid_idx <- is.finite(diag_vals) & diag_vals > 0
                    
                    if (sum(valid_idx) > 1) {  # 至少需要2个有效参数才能计算相关性
                      # 只保留有效的参数
                      param_names_valid <- rownames(cov_matrix_ordered)[valid_idx]
                      cov_matrix_filtered <- cov_matrix_ordered[valid_idx, valid_idx, drop = FALSE]
                      
                      # 再次检查对角线（防止数值误差）
                      diag_vals_filtered <- diag(cov_matrix_filtered)
                      if (all(is.finite(diag_vals_filtered)) && all(diag_vals_filtered > 0)) {
                        # 计算相关性矩阵（使用 suppressWarnings 避免警告）
                        cor_matrix <- suppressWarnings(cov2cor(cov_matrix_filtered))
                        # 检查结果是否有效
                        if (all(is.finite(cor_matrix))) {
                          rownames(cor_matrix) <- param_names_valid
                          colnames(cor_matrix) <- param_names_valid
                          values$correlation_matrix <- cor_matrix
                        } else {
                          values$correlation_matrix <- NULL
                        }
                      } else {
                        values$correlation_matrix <- NULL
                      }
                    } else {
                      values$correlation_matrix <- NULL
                    }
                  } else {
                    values$correlation_matrix <- NULL
                  }
                }, error = function(e) {
                  values$correlation_matrix <- NULL
                })
              } else {
                values$correlation_matrix <- NULL
              }
            } else {
              values$error_analysis <- NULL
              values$error_analysis_info <- NULL
              values$correlation_matrix <- NULL
            }
          } else {
            values$error_analysis <- NULL
            values$error_analysis_info <- NULL
            values$correlation_matrix <- NULL
          }
        }
      }, error = function(e) {
        values$error_analysis <- NULL
        values$error_analysis_info <- NULL
        values$correlation_matrix <- NULL
        showNotification(paste(tr("fit_error_analysis_error", lang()), e$message), type = "error", duration = 5)
      })
    } else {
      values$error_analysis <- NULL
      values$error_analysis_info <- NULL
      values$correlation_matrix <- NULL
    }
    
    # [新增] 计算残差数据（仅在误差分析启用时计算）
    if (enable_error_analysis) {
      tryCatch({
        # 使用最优参数重新构建完整参数列表
        full_p_fitted <- c(p_curr, fixed_p)
        full_p_fitted[params_to_opt] <- new_vals
        if (!is.null(full_p_fitted$V_inj_vec) && "V_init" %in% names(full_p_fitted)) {
          full_p_fitted$V_inj_vec[1] <- full_p_fitted$V_init
        }
        
        # 计算拟合后的模拟结果
        sim_res_fitted <- calculate_simulation(full_p_fitted, input$active_paths)
        
        if (!is.null(sim_res_fitted)) {
          # 使用与拟合时相同的 range_lim 和 valid_idx
          valid_idx <- range_lim[1]:range_lim[2]
          max_idx <- min(nrow(sim_res_fitted), nrow(exp_df))
          valid_idx <- valid_idx[valid_idx <= max_idx]
          
          if (length(valid_idx) > 0) {
            # 构建残差数据框（与主图一致：热量用 kcal/mol，与轴标签「kcal/mol」一致）
            fit_cal <- sim_res_fitted$dQ_App[valid_idx]
            obs_cal <- exp_df$Heat_Raw[valid_idx]
            residuals_data <- data.frame(
              Inj = valid_idx,
              Fitted = fit_cal / 1000,
              Observed = obs_cal / 1000,
              Residual = (obs_cal - fit_cal) / 1000,
              Ratio_App = sim_res_fitted$Ratio_App[valid_idx],
              stringsAsFactors = FALSE
            )
            values$residuals_data <- residuals_data
          } else {
            values$residuals_data <- NULL
          }
        } else {
          values$residuals_data <- NULL
        }
      }, error = function(e) {
        values$residuals_data <- NULL
      })
    } else {
      values$residuals_data <- NULL
    }
    
    # 完成
    progress$set(value = 1, detail = tr("fit_progress_done", lang()))
    algo_name_en <- if(use_DE) "DE Global Fit" else "L-BFGS-B Fit"
    showNotification(paste(algo_name_en, "Complete"), type="message")
  }
  
  observeEvent(input$fit_1_step, { 
    if(lang_switching()) return()  # [修复] 防止在语言切换时触发
    perform_fitting(1) 
  }, ignoreInit = TRUE)
  observeEvent(input$fit_10_step, { 
    if(lang_switching()) return()
    perform_fitting(10) 
  }, ignoreInit = TRUE)
  observeEvent(input$fit_full, { 
    if(lang_switching()) return()
    perform_fitting(100) 
  }, ignoreInit = TRUE)
  observeEvent(input$fit_global, { 
    if(lang_switching()) return()
    perform_fitting(50, use_DE = TRUE) 
  }, ignoreInit = TRUE) # DE 50代通常能找到不错的区域
