# ==============================================================================
# test_server_improvements.R - 测试 server.R 的改进
# ==============================================================================
# 本文件测试 server.R 中应用的改进是否正常工作

cat("测试 server.R 改进...\n\n")

# 加载必要的模块
source("R/constants.R")
source("R/utils.R")

# 测试计数器
tests_passed <- 0
tests_failed <- 0

assert_true <- function(condition, test_name = "") {
  if(condition) {
    tests_passed <<- tests_passed + 1
    cat("✓", test_name, "\n")
    return(TRUE)
  }
  tests_failed <<- tests_failed + 1
  cat("✗", test_name, "\n")
  return(FALSE)
}

# ==============================================================================
# 测试1：参数边界函数在拟合场景中的使用
# ==============================================================================

cat("\n[测试] 参数边界获取（模拟拟合场景）\n")

# 模拟拟合场景的参数
par_vec <- c(logK1 = 5, H1 = -6000, logK2 = 6, H2 = -5000, 
            fH = 1, V_init = 1.4, Offset = 0)
v_inj_val <- 0.01

# 获取边界
bounds <- get_parameter_bounds(names(par_vec), v_inj = v_inj_val)

assert_true(!is.null(bounds), "边界获取成功")
assert_true(!is.null(bounds$lower), "下界存在")
assert_true(!is.null(bounds$upper), "上界存在")
assert_true(length(bounds$lower) == length(par_vec), "下界长度正确")
assert_true(length(bounds$upper) == length(par_vec), "上界长度正确")
assert_true(all(bounds$lower < bounds$upper), "所有参数下界 < 上界")

# 验证特定参数的边界
assert_true(bounds$lower["logK1"] == PARAM_BOUNDS$logK["lower"], "logK1 下界匹配常量")
assert_true(bounds$upper["logK1"] == 9, "logK1 上界 = 9")
assert_true(bounds$lower["H1"] == -15000, "H1 下界 = -15000")
assert_true(bounds$upper["H1"] == 5000, "H1 上界 = 5000")
assert_true(bounds$lower["V_init"] == 0, "V_init 下界 = 0")
assert_true(bounds$upper["V_init"] == v_inj_val, "V_init 上界 = v_inj")

# 运行时覆盖边界（用于会话级用户范围）
override_bounds <- list(
  logK1 = c(lower = 3, upper = 4),
  H1 = c(lower = -8000, upper = -2000),
  Offset = c(lower = -500, upper = 500)
)
bounds_override <- get_parameter_bounds(
  c("logK1", "H1", "Offset"),
  v_inj = v_inj_val,
  override_bounds = override_bounds
)
assert_true(bounds_override$lower["logK1"] == 3, "override logK1 下界生效")
assert_true(bounds_override$upper["logK1"] == 4, "override logK1 上界生效")
assert_true(bounds_override$lower["H1"] == -8000, "override H1 下界生效")
assert_true(bounds_override$upper["Offset"] == 500, "override Offset 上界生效")

# ==============================================================================
# 测试2：safe_numeric 在输入验证中的使用
# ==============================================================================

cat("\n[测试] 输入验证（safe_numeric）\n")

# 模拟各种输入情况
test_logK <- safe_numeric(5.5, 
                         default = DEFAULT_PARAMS$logK,
                         min = PARAM_BOUNDS$logK["lower"],
                         max = PARAM_BOUNDS$logK["upper"])
assert_true(test_logK == 5.5, "有效 logK 值通过")

test_logK_null <- safe_numeric(NULL,
                              default = DEFAULT_PARAMS$logK,
                              min = PARAM_BOUNDS$logK["lower"],
                              max = PARAM_BOUNDS$logK["upper"])
assert_true(test_logK_null == DEFAULT_PARAMS$logK, "NULL logK 使用默认值")

test_logK_out <- safe_numeric(15,
                             default = DEFAULT_PARAMS$logK,
                             min = PARAM_BOUNDS$logK["lower"],
                             max = PARAM_BOUNDS$logK["upper"])
assert_true(test_logK_out == PARAM_BOUNDS$logK["upper"], "超出上界的 logK 被限制")

test_H <- safe_numeric(-6000,
                      default = DEFAULT_PARAMS$H,
                      min = PARAM_BOUNDS$H["lower"],
                      max = PARAM_BOUNDS$H["upper"])
assert_true(test_H == -6000, "有效 H 值通过")

test_logK_override <- safe_numeric(5.5,
                                   default = DEFAULT_PARAMS$logK,
                                   min = bounds_override$lower["logK1"],
                                   max = bounds_override$upper["logK1"])
assert_true(test_logK_override == 4, "override 范围下 logK 会被限制到上界")

# ==============================================================================
# 测试3：DE 优化参数计算
# ==============================================================================

cat("\n[测试] DE 优化参数计算\n")

# 测试不同数量参数的种群大小计算
for(n_params in c(2, 5, 10, 20)) {
  pop_size <- max(
    DE_OPTIM$pop_size_multiplier * n_params,
    DE_OPTIM$min_pop_size
  )
  pop_size <- min(pop_size, DE_OPTIM$max_pop_size)
  
  assert_true(pop_size >= DE_OPTIM$min_pop_size, 
             sprintf("种群大小 >= 最小值（n_params=%d）", n_params))
  assert_true(pop_size <= DE_OPTIM$max_pop_size,
             sprintf("种群大小 <= 最大值（n_params=%d）", n_params))
}

# ==============================================================================
# 测试4：模拟 perform_fitting 中的参数准备
# ==============================================================================

cat("\n[测试] 模拟参数准备流程\n")

# 模拟 input 对象（使用 list）
mock_input <- list(
  logK1 = 5, H1 = -6000, logK2 = 6, H2 = -5000,
  logK3 = NULL, H3 = NA,  # 测试 NULL 和 NA
  logK4 = 15, H4 = -20000,  # 测试超出范围
  logK5 = 4, H5 = -5500,
  factor_H = 1.0, factor_G = 1.0,
  V_init_val = 1.4, heat_offset = 0,
  H_cell_0 = 0.1, G_syringe = 1.0,
  V_cell = 1.4, V_inj = 0.01,
  n_inj = 26, V_pre = 0
)

# 准备当前参数（模拟 server.R 中的代码）
p_curr <- list(
  logK1 = safe_numeric(mock_input$logK1, default = DEFAULT_PARAMS$logK, 
                      min = PARAM_BOUNDS$logK["lower"], max = PARAM_BOUNDS$logK["upper"]),
  H1 = safe_numeric(mock_input$H1, default = DEFAULT_PARAMS$H, 
                   min = PARAM_BOUNDS$H["lower"], max = PARAM_BOUNDS$H["upper"]),
  logK2 = safe_numeric(mock_input$logK2, default = DEFAULT_PARAMS$logK, 
                      min = PARAM_BOUNDS$logK["lower"], max = PARAM_BOUNDS$logK["upper"]),
  H2 = safe_numeric(mock_input$H2, default = DEFAULT_PARAMS$H, 
                   min = PARAM_BOUNDS$H["lower"], max = PARAM_BOUNDS$H["upper"]),
  logK3 = safe_numeric(mock_input$logK3, default = DEFAULT_PARAMS$logK, 
                      min = PARAM_BOUNDS$logK["lower"], max = PARAM_BOUNDS$logK["upper"]),
  H3 = safe_numeric(mock_input$H3, default = DEFAULT_PARAMS$H, 
                   min = PARAM_BOUNDS$H["lower"], max = PARAM_BOUNDS$H["upper"]),
  logK4 = safe_numeric(mock_input$logK4, default = DEFAULT_PARAMS$logK, 
                      min = PARAM_BOUNDS$logK["lower"], max = PARAM_BOUNDS$logK["upper"]),
  H4 = safe_numeric(mock_input$H4, default = DEFAULT_PARAMS$H, 
                   min = PARAM_BOUNDS$H["lower"], max = PARAM_BOUNDS$H["upper"]),
  fH = safe_numeric(mock_input$factor_H, default = DEFAULT_PARAMS$fH, 
                   min = PARAM_BOUNDS$fH_fG["lower"], max = PARAM_BOUNDS$fH_fG["upper"]),
  V_init = safe_numeric(mock_input$V_init_val, default = DEFAULT_PARAMS$V_init, min = 0),
  Offset = safe_numeric(mock_input$heat_offset, default = DEFAULT_PARAMS$Offset, 
                       min = PARAM_BOUNDS$Offset["lower"], max = PARAM_BOUNDS$Offset["upper"])
)

# 验证结果
assert_true(p_curr$logK1 == 5, "logK1 保持原值")
assert_true(p_curr$H1 == -6000, "H1 保持原值")
assert_true(p_curr$logK3 == DEFAULT_PARAMS$logK, "NULL logK3 使用默认值")
assert_true(p_curr$H3 == DEFAULT_PARAMS$H, "NA H3 使用默认值")
assert_true(p_curr$logK4 == PARAM_BOUNDS$logK["upper"], "超范围 logK4 被限制到上界")
assert_true(p_curr$H4 == PARAM_BOUNDS$H["lower"], "超范围 H4 被限制到下界")

# ==============================================================================
# 测试5：safe_execute 错误处理
# ==============================================================================

cat("\n[测试] 错误处理（safe_execute）\n")

# 测试成功执行
result_success <- safe_execute({
  2 + 2
}, context = "测试成功", show_error = FALSE)
assert_true(result_success == 4, "safe_execute 成功执行")

# 测试错误处理
result_error <- safe_execute({
  stop("测试错误")
}, context = "测试错误", default = "默认值", show_error = FALSE)
assert_true(result_error == "默认值", "safe_execute 错误时返回默认值")

# 测试 NULL 默认值
result_null <- safe_execute({
  stop("测试错误")
}, context = "测试错误", show_error = FALSE)
assert_true(is.null(result_null), "safe_execute 错误时默认返回 NULL")

# ==============================================================================
# 测试6：日志记录功能
# ==============================================================================

cat("\n[测试] 日志记录\n")

# 清理旧日志（如果存在）
if(file.exists("session.log")) {
  file.remove("session.log")
}

# 记录一些信息
log_info("测试信息1", context = "测试")
log_info("测试信息2", context = "测试")

# 检查日志文件是否创建
assert_true(file.exists("session.log"), "session.log 文件已创建")

# 读取并验证日志内容
if(file.exists("session.log")) {
  log_content <- readLines("session.log")
  assert_true(length(log_content) >= 2, "至少有2条日志记录")
  assert_true(any(grepl("测试信息1", log_content)), "日志包含测试信息1")
  assert_true(any(grepl("测试信息2", log_content)), "日志包含测试信息2")
}

# ==============================================================================
# 测试总结
# ==============================================================================

cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("测试总结\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat(sprintf("通过: %d\n", tests_passed))
cat(sprintf("失败: %d\n", tests_failed))
cat(sprintf("总计: %d\n", tests_passed + tests_failed))

if(tests_failed == 0) {
  cat("\n✓ 所有测试通过！\n")
  cat("server.R 的改进已准备好使用。\n")
} else {
  cat("\n✗ 有测试失败，请检查实现。\n")
}

cat(paste(rep("=", 70), collapse = ""), "\n")

# 清理测试日志
if(file.exists("session.log")) {
  file.remove("session.log")
}

# 返回测试结果
invisible(list(passed = tests_passed, failed = tests_failed))
