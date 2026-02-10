# ==============================================================================
# 迁移示例：改进 server.R 中的 perform_fitting_sync 函数
# ==============================================================================
# 本文件展示如何使用新的常量和错误处理函数来改进现有代码
# 
# 说明：这是一个示例文件，展示改进前后的对比
# 实际应用时，应该直接修改 server.R 文件

# ==============================================================================
# 改进前的代码（第965-1005行）
# ==============================================================================

# 原始代码片段（仅供参考）
perform_fitting_sync_OLD <- function() {
  # ... 前面的代码省略 ...
  
  # ❌ 问题1：硬编码的参数边界
  lower_b <- par_vec; upper_b <- par_vec
  for(nm in names(par_vec)) {
    if(grepl("logK", nm)) { lower_b[nm] <- 1; upper_b[nm] <- 9 }
    if(grepl("H", nm))    { lower_b[nm] <- -15000; upper_b[nm] <- 5000 }
    if(grepl("fH|fG", nm)) { lower_b[nm] <- 0.5; upper_b[nm] <- 1.5 }
    if(nm == "V_init") {
      v_inj_val <- if(fixed_p$V_inj > 0) fixed_p$V_inj else 1.5
      lower_b[nm] <- 0
      upper_b[nm] <- 1 * v_inj_val
    }
    if(grepl("Offset", nm)) { lower_b[nm] <- -1500; upper_b[nm] <- 1500 }
  }
  
  # 更新进度：开始拟合
  if(!is.null(progress)) {
    algo_name <- if(use_DE) tr("fit_progress_de", lang_val) else tr("fit_progress_lbfgsb", lang_val)
    progress$set(value = 0.2, detail = algo_name)
  }
  
  res <- NULL
  
  if (use_DE) {
    # ❌ 问题2：简单的包依赖检查，没有统一错误处理
    if (!requireNamespace("DEoptim", quietly = TRUE)) {
      return(list(error = tr("fit_error_deoptim_required", lang_val)))
    }
    
    # 包装 obj_fun
    de_obj_fun <- function(p) {
      names(p) <- names(par_vec)
      obj_fun(p)
    }
    
    # ❌ 问题3：硬编码的优化参数
    pop_size <- max(10 * length(par_vec), 50)
    
    # ❌ 问题4：没有错误处理
    out <- DEoptim::DEoptim(fn = de_obj_fun, lower = lower_b, upper = upper_b,
                            control = list(itermax = max_iters, NP = pop_size, trace = FALSE))
    
    res <- list(
      par = out$optim$bestmem,
      value = out$optim$bestval
    )
  }
  
  # ... 后续代码省略 ...
}

# ==============================================================================
# 改进后的代码
# ==============================================================================

perform_fitting_sync_NEW <- function() {
  # ... 前面的代码保持不变 ...
  
  # ✅ 改进1：使用常量和辅助函数获取参数边界
  # 不再手动循环设置边界，而是使用 constants.R 中的辅助函数
  v_inj_val <- safe_numeric(fixed_p$V_inj, default = 1.5, min = 0)
  bounds <- get_parameter_bounds(names(par_vec), v_inj = v_inj_val)
  lower_b <- bounds$lower
  upper_b <- bounds$upper
  
  # 记录边界信息（用于调试）
  log_info(sprintf("参数边界设置完成，共 %d 个参数", length(par_vec)), 
          context = "perform_fitting_sync")
  
  # 更新进度：开始拟合
  if(!is.null(progress)) {
    algo_name <- if(use_DE) {
      tr("fit_progress_de", lang_val)
    } else {
      tr("fit_progress_lbfgsb", lang_val)
    }
    progress$set(value = 0.2, detail = algo_name)
  }
  
  res <- NULL
  
  if (use_DE) {
    # ✅ 改进2：统一的包依赖检查
    if (!requireNamespace("DEoptim", quietly = TRUE)) {
      handle_error(
        simpleError("DEoptim 包未安装"),
        context = "依赖检查",
        error_code = ERROR_CODES$PACKAGE_MISSING,
        lang_val = lang_val
      )
      return(list(error = tr("fit_error_deoptim_required", lang_val)))
    }
    
    # 包装 obj_fun 以便 DEoptim 调用
    de_obj_fun <- function(p) {
      names(p) <- names(par_vec) # 恢复参数名
      obj_fun(p)
    }
    
    # ✅ 改进3：使用常量定义优化参数
    pop_size <- max(
      DE_OPTIM$pop_size_multiplier * length(par_vec),
      DE_OPTIM$min_pop_size
    )
    # 限制最大种群大小（防止过大导致性能问题）
    pop_size <- min(pop_size, DE_OPTIM$max_pop_size)
    
    log_info(sprintf("DE优化参数：种群大小=%d, 最大迭代=%d", 
                    pop_size, max_iters),
            context = "perform_fitting_sync")
    
    # ✅ 改进4：使用统一错误处理包装优化调用
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
    error_code = ERROR_CODES$OPTIMIZATION_FAILED,
    lang_val = lang_val,
    default = NULL
    )
    
    # 检查优化是否成功
    if(is.null(out)) {
      log_warning("DE优化失败，返回错误", context = "perform_fitting_sync")
      return(list(error = tr("fit_error_optimization_failed", lang_val)))
    }
    
    # 提取结果
    res <- list(
      par = out$optim$bestmem,
      value = out$optim$bestval
    )
    
    log_info(sprintf("DE优化完成，目标函数值=%.4f", res$value),
            context = "perform_fitting_sync")
    
  } else {
    # ✅ L-BFGS-B 优化（也可以类似改进）
    out <- safe_execute({
      optim(
        par = par_vec,
        fn = obj_fun,
        method = "L-BFGS-B",
        lower = lower_b,
        upper = upper_b,
        control = list(
          maxit = LBFGS_OPTIM$max_iter,
          factr = LBFGS_OPTIM$factr,
          pgtol = LBFGS_OPTIM$pgtol
        )
      )
    },
    context = "L-BFGS-B局部优化",
    error_code = ERROR_CODES$OPTIMIZATION_FAILED,
    lang_val = lang_val,
    default = NULL
    )
    
    if(is.null(out)) {
      return(list(error = tr("fit_error_optimization_failed", lang_val)))
    }
    
    res <- out
  }
  
  # ... 后续代码保持不变 ...
  
  return(res)
}

# ==============================================================================
# 其他改进示例
# ==============================================================================

# 示例1：改进输入验证
# ------------------------------------------------------------------------------

# ❌ 改进前
get_inputs_OLD <- function() {
  n_inj <- input$n_inj
  if(is.null(n_inj)) n_inj <- 26
  
  conc_cell <- input$conc_cell
  if(is.null(conc_cell) || is.na(conc_cell)) conc_cell <- 0.1
  
  v_cell <- input$v_cell
  if(is.null(v_cell) || is.na(v_cell) || v_cell <= 0) v_cell <- 1.4
  
  return(list(n_inj = n_inj, conc_cell = conc_cell, v_cell = v_cell))
}

# ✅ 改进后
get_inputs_NEW <- function() {
  n_inj <- safe_input(input$n_inj, UI_DEFAULTS$n_inj_default)
  
  conc_cell <- safe_numeric(
    input$conc_cell,
    default = UI_DEFAULTS$conc_cell_default,
    min = UI_DEFAULTS$conc_cell_min,
    max = UI_DEFAULTS$conc_cell_max
  )
  
  v_cell <- safe_numeric(
    input$v_cell,
    default = UI_DEFAULTS$v_cell_default,
    min = UI_DEFAULTS$v_cell_min,
    max = UI_DEFAULTS$v_cell_max
  )
  
  log_info(sprintf("输入参数：n_inj=%d, conc_cell=%.3f, v_cell=%.3f",
                  n_inj, conc_cell, v_cell),
          context = "get_inputs")
  
  return(list(n_inj = n_inj, conc_cell = conc_cell, v_cell = v_cell))
}

# 示例2：改进数据加载
# ------------------------------------------------------------------------------

# ❌ 改进前
load_experimental_data_OLD <- function(file_path) {
  tryCatch({
    data <- read.csv(file_path)
    if(!"Q" %in% colnames(data)) {
      showNotification("缺少 Q 列", type = "error")
      return(NULL)
    }
    return(data)
  }, error = function(e) {
    showNotification(paste("读取失败:", e$message), type = "error")
    return(NULL)
  })
}

# ✅ 改进后
load_experimental_data_NEW <- function(file_path, lang_val = "zh") {
  # 使用 safe_execute 包装文件读取
  data <- safe_execute({
    read.csv(file_path, stringsAsFactors = FALSE)
  },
  context = sprintf("读取实验数据文件: %s", basename(file_path)),
  error_code = ERROR_CODES$FILE_READ_ERROR,
  lang_val = lang_val,
  default = NULL
  )
  
  if(is.null(data)) {
    return(NULL)
  }
  
  # 使用统一的数据验证
  if(!validate_dataframe(data, 
                        required_cols = c("Q", "inj_idx"),
                        min_rows = DATA_VALIDATION$min_data_points)) {
    handle_error(
      simpleError(sprintf("数据验证失败：文件必须包含 'Q' 和 'inj_idx' 列，且至少有 %d 行数据",
                         DATA_VALIDATION$min_data_points)),
      context = "数据验证",
      error_code = ERROR_CODES$DATA_INVALID,
      lang_val = lang_val
    )
    return(NULL)
  }
  
  log_info(sprintf("成功加载 %d 行实验数据", nrow(data)),
          context = "load_experimental_data")
  
  return(data)
}

# 示例3：改进数值计算
# ------------------------------------------------------------------------------

# ❌ 改进前（从 core_logic.R）
solve_equilibrium_OLD <- function(G_tot, H_tot, params) {
  # 硬编码的极小值检查
  if(G_tot <= 1e-20) return(c(0, H_tot, 0))
  if(H_tot <= 1e-20) return(c(G_tot, 0, 0))
  
  # 硬编码的安全对数
  Ks <- pmax(Ks, 1e-20)
  log_Ks <- log(Ks)
  
  # ... 其他计算 ...
}

# ✅ 改进后
solve_equilibrium_NEW <- function(G_tot, H_tot, params) {
  # 使用常量定义的极小值检查
  if(is_near_zero(G_tot, EPSILON)) {
    return(c(0, H_tot, 0))
  }
  if(is_near_zero(H_tot, EPSILON)) {
    return(c(G_tot, 0, 0))
  }
  
  # 使用安全函数
  Ks <- pmax(Ks, EPSILON)
  log_Ks <- safe_log(Ks, EPSILON_LOG)
  
  # ... 其他计算 ...
}

# ==============================================================================
# 迁移步骤建议
# ==============================================================================

# 步骤1：备份原文件
# cp server.R server.R.backup

# 步骤2：逐个函数迁移
# 1. perform_fitting_sync
# 2. 其他使用硬编码边界的函数
# 3. 输入验证相关代码
# 4. 数据加载相关代码
# 5. 数值计算相关代码

# 步骤3：测试每次改动
# - 运行应用
# - 测试基本功能
# - 触发错误场景，检查错误处理
# - 查看日志文件

# 步骤4：代码审查
# - 检查是否还有硬编码值
# - 确认错误处理一致
# - 验证日志记录正常

# ==============================================================================
# 测试建议
# ==============================================================================

# 测试用例1：测试参数边界函数
test_parameter_bounds <- function() {
  # 测试 logK 边界
  bounds <- get_param_bound("logK1")
  stopifnot(bounds["lower"] == 1)
  stopifnot(bounds["upper"] == 9)
  
  # 测试 H 边界
  bounds <- get_param_bound("H1")
  stopifnot(bounds["lower"] == -15000)
  stopifnot(bounds["upper"] == 5000)
  
  # 测试 V_init 边界
  bounds <- get_param_bound("V_init", v_inj = 0.01)
  stopifnot(bounds["lower"] == 0)
  stopifnot(bounds["upper"] == 0.01)
  
  cat("✓ 参数边界测试通过\n")
}

# 测试用例2：测试错误处理
test_error_handling <- function() {
  # 测试 safe_execute
  result <- safe_execute({
    stop("测试错误")
  }, context = "测试", show_error = FALSE, default = "默认值")
  
  stopifnot(result == "默认值")
  
  # 测试 safe_numeric
  val <- safe_numeric("abc", default = 1.0)
  stopifnot(val == 1.0)
  
  val <- safe_numeric(5.5, default = 1.0, min = 0, max = 10)
  stopifnot(val == 5.5)
  
  val <- safe_numeric(15, default = 1.0, min = 0, max = 10)
  stopifnot(val == 10)  # 超出上限，应返回上限
  
  cat("✓ 错误处理测试通过\n")
}

# 运行测试
if(FALSE) {  # 设置为 TRUE 以运行测试
  test_parameter_bounds()
  test_error_handling()
}
