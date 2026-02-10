# ==============================================================================
# 拟合问题诊断工具
# ==============================================================================
# 
# 这个工具帮助诊断"拟合无法启动"的问题
# 使用方法：在应用运行时，在RStudio控制台运行此脚本
# 或者在应用代码中调用诊断函数
# ==============================================================================

#' 诊断拟合启动失败的原因
#' 
#' @param p_curr 当前参数列表（从input获取）
#' @param fixed_p 固定参数列表
#' @param params_to_opt 要优化的参数名
#' @param range_lim 拟合区间 c(start_idx, end_idx)
#' @param active_paths 激活的反应路径
#' @param exp_df 实验数据
#' @return 诊断结果列表
diagnose_fitting_issue <- function(p_curr, fixed_p, params_to_opt, 
                                   range_lim, active_paths, exp_df) {
  
  cat("========================================\n")
  cat("拟合问题诊断\n")
  cat("========================================\n\n")
  
  # 准备完整参数
  par_vec <- unlist(p_curr[params_to_opt])
  full_p <- c(p_curr, fixed_p)
  
  # 诊断1: 检查参数值
  cat("步骤1：检查参数值\n")
  cat("----------------------------------------\n")
  cat("要优化的参数：\n")
  for(nm in names(par_vec)) {
    val <- par_vec[nm]
    cat(sprintf("  %s = %g", nm, val))
    
    # 检查范围
    if(grepl("logK", nm)) {
      if(val < 1 || val > 9) {
        cat(sprintf(" ⚠️  超出建议范围 (1-9)\n"))
      } else {
        cat(" ✅\n")
      }
    } else if(grepl("^H[0-9]$", nm)) {
      if(val < -15000 || val > 5000) {
        cat(sprintf(" ⚠️  超出建议范围 (-15000 到 5000)\n"))
      } else {
        cat(" ✅\n")
      }
    } else if(grepl("fH|fG", nm)) {
      if(val < 0.5 || val > 1.5) {
        cat(sprintf(" ⚠️  超出建议范围 (0.5-1.5)\n"))
      } else {
        cat(" ✅\n")
      }
    } else if(nm == "V_init") {
      v_inj_val <- if(fixed_p$V_inj > 0) fixed_p$V_inj else 1.5
      if(val < 0 || val > v_inj_val) {
        cat(sprintf(" ⚠️  超出建议范围 (0-%g)\n", v_inj_val))
      } else {
        cat(" ✅\n")
      }
    } else if(grepl("Offset", nm)) {
      if(val < -1500 || val > 1500) {
        cat(sprintf(" ⚠️  超出建议范围 (-1500 到 1500)\n"))
      } else {
        cat(" ✅\n")
      }
    } else {
      cat(" ✅\n")
    }
  }
  cat("\n")
  
  # 诊断2: 检查固定参数
  cat("步骤2：检查固定参数\n")
  cat("----------------------------------------\n")
  critical_params <- c("H_cell_0", "G_syringe", "V_cell", "V_inj", "n_inj")
  for(nm in critical_params) {
    if(nm %in% names(fixed_p)) {
      val <- fixed_p[[nm]]
      cat(sprintf("  %s = %g", nm, val))
      if(val <= 0) {
        cat(" ❌ 必须大于0\n")
      } else {
        cat(" ✅\n")
      }
    } else {
      cat(sprintf("  %s = ❌ 缺失\n", nm))
    }
  }
  cat("\n")
  
  # 诊断3: 尝试运行模拟
  cat("步骤3：测试模拟计算\n")
  cat("----------------------------------------\n")
  
  sim_res <- tryCatch({
    calculate_simulation(full_p, active_paths)
  }, error = function(e) {
    cat(sprintf("❌ 模拟计算出错: %s\n", e$message))
    return(NULL)
  })
  
  if(is.null(sim_res)) {
    cat("❌ 模拟计算返回NULL\n")
    cat("\n可能原因：\n")
    cat("  1. run_sim_modular 函数失败\n")
    cat("  2. 参数组合导致数值问题\n")
    cat("  3. 反应路径配置错误\n")
    return(list(success = FALSE, issue = "模拟计算失败"))
  }
  
  cat(sprintf("✅ 模拟计算成功，生成了 %d 针数据\n", nrow(sim_res)))
  
  # 诊断4: 检查模拟结果的有效性
  cat("\n步骤4：检查模拟结果有效性\n")
  cat("----------------------------------------\n")
  
  if(any(!is.finite(sim_res$dQ_App))) {
    n_inf <- sum(!is.finite(sim_res$dQ_App))
    n_nan <- sum(is.nan(sim_res$dQ_App))
    n_inf_val <- sum(is.infinite(sim_res$dQ_App))
    
    cat(sprintf("❌ 模拟结果包含无效值：\n"))
    cat(sprintf("  - NaN: %d 个\n", n_nan))
    cat(sprintf("  - Inf: %d 个\n", n_inf_val))
    cat(sprintf("  - 总计: %d 个\n", n_inf))
    
    # 找出第一个无效值的位置
    first_invalid <- which(!is.finite(sim_res$dQ_App))[1]
    if(!is.na(first_invalid)) {
      cat(sprintf("  - 第一个无效值在第 %d 针\n", first_invalid))
    }
    
    cat("\n建议：\n")
    cat("  1. 检查logK值是否合理（1-9）\n")
    cat("  2. 检查H值是否合理（-15000到5000）\n")
    cat("  3. 尝试调整参数范围\n")
    
    return(list(success = FALSE, issue = "模拟结果包含无效值", sim_res = sim_res))
  }
  
  cat("✅ 所有模拟值都是有效的\n")
  cat(sprintf("  - dQ_App 范围: [%g, %g]\n", 
              min(sim_res$dQ_App, na.rm=TRUE), 
              max(sim_res$dQ_App, na.rm=TRUE)))
  cat("\n")
  
  # 诊断5: 检查拟合区间
  cat("步骤5：检查拟合区间\n")
  cat("----------------------------------------\n")
  
  max_idx <- min(nrow(sim_res), nrow(exp_df))
  cat(sprintf("模拟数据针数: %d\n", nrow(sim_res)))
  cat(sprintf("实验数据针数: %d\n", nrow(exp_df)))
  cat(sprintf("最大可用针数: %d\n", max_idx))
  cat(sprintf("拟合区间设置: [%d, %d]\n", range_lim[1], range_lim[2]))
  
  valid_idx <- range_lim[1]:range_lim[2]
  valid_idx <- valid_idx[valid_idx <= max_idx]
  
  if(length(valid_idx) == 0) {
    cat("❌ 拟合区间无效：没有可用的数据点\n")
    cat("\n建议：\n")
    cat(sprintf("  1. 调整拟合区间，确保在 [1, %d] 范围内\n", max_idx))
    cat("  2. 检查实验数据是否正确导入\n")
    return(list(success = FALSE, issue = "拟合区间无效", 
                max_idx = max_idx, range_lim = range_lim))
  }
  
  cat(sprintf("✅ 有效数据点: %d 个\n", length(valid_idx)))
  cat("\n")
  
  # 诊断6: 检查数据匹配情况
  cat("步骤6：检查数据匹配情况\n")
  cat("----------------------------------------\n")
  
  sim_values <- sim_res$dQ_App[valid_idx]
  exp_values <- exp_df$Heat_Raw[valid_idx]
  
  cat(sprintf("模拟值范围: [%g, %g]\n", 
              min(sim_values, na.rm=TRUE), 
              max(sim_values, na.rm=TRUE)))
  cat(sprintf("实验值范围: [%g, %g]\n", 
              min(exp_values, na.rm=TRUE), 
              max(exp_values, na.rm=TRUE)))
  
  # 计算RSS
  rss <- sum((sim_values - exp_values)^2)
  cat(sprintf("当前RSS: %g\n", rss))
  
  if(rss >= 1e19) {
    cat("❌ RSS过大，说明模拟与实验数据完全不匹配\n")
    cat("\n建议：\n")
    cat("  1. 先手动调整参数，使曲线大致对齐\n")
    cat("  2. 检查实验数据是否正确导入\n")
    cat("  3. 检查模型路径是否匹配实验条件\n")
    cat("  4. 尝试调整Offset参数对齐基线\n")
    cat("  5. 尝试调整fH和fG参数对齐位置\n")
    
    return(list(success = FALSE, issue = "数据不匹配", rss = rss,
                sim_values = sim_values, exp_values = exp_values))
  }
  
  cat(sprintf("✅ RSS在合理范围内（%g < 1e19）\n", rss))
  cat("\n")
  
  # 诊断7: 检查反应路径
  cat("步骤7：检查反应路径配置\n")
  cat("----------------------------------------\n")
  cat(sprintf("激活的反应路径: %s\n", paste(active_paths, collapse=", ")))
  
  # 检查是否有必要的参数
  required_params <- c()
  if("rxn_M" %in% active_paths || any(grepl("rxn_", active_paths))) {
    required_params <- c(required_params, "logK1", "H1")
  }
  if("rxn_D" %in% active_paths) {
    required_params <- c(required_params, "logK2", "H2")
  }
  if("rxn_T" %in% active_paths) {
    required_params <- c(required_params, "logK3", "H3")
  }
  
  missing_params <- setdiff(required_params, names(p_curr))
  if(length(missing_params) > 0) {
    cat(sprintf("⚠️  可能缺少参数: %s\n", paste(missing_params, collapse=", ")))
  } else {
    cat("✅ 所有必要的参数都已设置\n")
  }
  cat("\n")
  
  # 总结
  cat("========================================\n")
  cat("诊断总结\n")
  cat("========================================\n\n")
  
  cat("✅ 所有检查通过！理论上拟合应该可以启动。\n")
  cat("\n如果仍然无法启动，请检查：\n")
  cat("  1. 是否选择了要优化的参数\n")
  cat("  2. 拟合区间是否在数据范围内\n")
  cat("  3. 实验数据格式是否正确\n")
  
  return(list(success = TRUE, rss = rss, sim_res = sim_res, 
              valid_idx = valid_idx))
}

# ==============================================================================
# 使用示例（在Shiny应用中）
# ==============================================================================
# 
# 在server.R的perform_fitting函数中，如果遇到错误，可以调用：
# 
# diagnose_result <- diagnose_fitting_issue(
#   p_curr = p_curr,
#   fixed_p = fixed_p,
#   params_to_opt = params_to_opt,
#   range_lim = range_lim,
#   active_paths = input$active_paths,
#   exp_df = exp_df
# )
# 
# 这将输出详细的诊断信息到控制台
# ==============================================================================
