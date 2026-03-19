# ==============================================================================
# test_constants_utils.R - 测试常量定义和工具函数
# ==============================================================================
# 本文件用于测试新创建的 R/constants.R 和 R/utils.R 中的函数

# 加载必要的模块
cat("加载测试环境...\n")

resolve_repo_root <- function() {
  env_root <- Sys.getenv("ITCSUITE_REPO_ROOT", unset = "")
  if (nzchar(env_root)) {
    p <- normalizePath(env_root, winslash = "/", mustWork = FALSE)
    if (dir.exists(file.path(p, "ITCsimfit")) && dir.exists(file.path(p, "tests"))) {
      return(normalizePath(p, winslash = "/", mustWork = TRUE))
    }
  }

  cur <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  for (i in 0:8) {
    if (dir.exists(file.path(cur, "ITCsimfit")) && dir.exists(file.path(cur, "tests"))) {
      return(normalizePath(cur, winslash = "/", mustWork = TRUE))
    }
    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }
  stop("Cannot resolve repository root.")
}

repo_root <- resolve_repo_root()
itcsimfit_dir <- file.path(repo_root, "ITCsimfit")

source(file.path(itcsimfit_dir, "R", "constants.R"))
source(file.path(itcsimfit_dir, "R", "utils.R"))

# 测试计数器
tests_passed <- 0
tests_failed <- 0

# 测试辅助函数
assert_equal <- function(actual, expected, test_name = "", tolerance = 1e-10) {
  if(is.numeric(actual) && is.numeric(expected)) {
    if(abs(actual - expected) < tolerance) {
      tests_passed <<- tests_passed + 1
      cat("✓", test_name, "\n")
      return(TRUE)
    }
  } else if(identical(actual, expected)) {
    tests_passed <<- tests_passed + 1
    cat("✓", test_name, "\n")
    return(TRUE)
  }
  
  tests_failed <<- tests_failed + 1
  cat("✗", test_name, "\n")
  cat("  期望:", expected, "\n")
  cat("  实际:", actual, "\n")
  return(FALSE)
}

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
# 测试常量定义 (constants.R)
# ==============================================================================

cat("\n=== 测试常量定义 ===\n")

# 测试1：参数边界常量存在
cat("\n[测试组] 参数边界常量\n")
assert_true(exists("PARAM_BOUNDS"), "PARAM_BOUNDS 存在")
assert_true(exists("DEFAULT_PARAMS"), "DEFAULT_PARAMS 存在")
assert_true(!is.null(PARAM_BOUNDS$logK), "logK 边界已定义")
assert_true(!is.null(PARAM_BOUNDS$H), "H 边界已定义")

# 测试2：参数边界值正确
assert_equal(PARAM_BOUNDS$logK["lower"], 0, "logK 下界 = 0")
assert_equal(PARAM_BOUNDS$logK["upper"], 9, "logK 上界 = 9")
assert_equal(PARAM_BOUNDS$H["lower"], -15000, "H 下界 = -15000")
assert_equal(PARAM_BOUNDS$H["upper"], 5000, "H 上界 = 5000")

# 测试3：数值常量存在
cat("\n[测试组] 数值常量\n")
assert_true(exists("EPSILON"), "EPSILON 存在")
assert_true(exists("EPSILON_LOG"), "EPSILON_LOG 存在")
assert_equal(EPSILON, 1e-20, "EPSILON = 1e-20")
assert_equal(EPSILON_LOG, 1e-15, "EPSILON_LOG = 1e-15")

# 测试4：优化参数常量
cat("\n[测试组] 优化参数常量\n")
assert_true(exists("DE_OPTIM"), "DE_OPTIM 存在")
assert_equal(DE_OPTIM$pop_size_multiplier, 10, "DE 种群大小倍数 = 10")
assert_equal(DE_OPTIM$min_pop_size, 50, "DE 最小种群 = 50")

# 测试5：UI 默认值常量
cat("\n[测试组] UI 默认值\n")
assert_true(exists("UI_DEFAULTS"), "UI_DEFAULTS 存在")
assert_equal(UI_DEFAULTS$n_inj_default, 20, "默认注射次数 = 20")

# ==============================================================================
# 测试辅助函数 (constants.R)
# ==============================================================================

cat("\n=== 测试参数边界辅助函数 ===\n")

# 测试6：get_param_bound 函数
cat("\n[测试组] get_param_bound()\n")

# logK 边界
bounds <- get_param_bound("logK1")
assert_equal(bounds["lower"], 0, "get_param_bound('logK1') 下界")
assert_equal(bounds["upper"], 9, "get_param_bound('logK1') 上界")

# H 边界
bounds <- get_param_bound("H1")
assert_equal(bounds["lower"], -15000, "get_param_bound('H1') 下界")
assert_equal(bounds["upper"], 5000, "get_param_bound('H1') 上界")

# fH 边界
bounds <- get_param_bound("fH")
assert_equal(bounds["lower"], 0.5, "get_param_bound('fH') 下界")
assert_equal(bounds["upper"], 1.5, "get_param_bound('fH') 上界")

# Offset 边界
bounds <- get_param_bound("Offset")
assert_equal(bounds["lower"], -1500, "get_param_bound('Offset') 下界")
assert_equal(bounds["upper"], 1500, "get_param_bound('Offset') 上界")

# V_init 边界
bounds <- get_param_bound("V_init", v_inj = 0.01)
assert_equal(bounds["lower"], 0, "get_param_bound('V_init') 下界")
assert_equal(bounds["upper"], 0.01, "get_param_bound('V_init') 上界（v_inj=0.01）")

# 运行时覆盖边界
override_bounds <- list(
  logK1 = c(lower = 3, upper = 4),
  H1 = c(lower = -8000, upper = -2000),
  Offset = c(lower = -500, upper = 500)
)
bounds <- get_param_bound("logK1", override_bounds = override_bounds)
assert_equal(bounds["lower"], 3, "get_param_bound('logK1') 可使用 override 下界")
assert_equal(bounds["upper"], 4, "get_param_bound('logK1') 可使用 override 上界")
bounds <- get_param_bound("H1", override_bounds = override_bounds)
assert_equal(bounds["lower"], -8000, "get_param_bound('H1') 可使用 override 下界")
assert_equal(bounds["upper"], -2000, "get_param_bound('H1') 可使用 override 上界")

# 测试7：get_parameter_bounds 函数（多参数）
cat("\n[测试组] get_parameter_bounds()\n")

param_names <- c("logK1", "logK2", "H1", "H2", "fH", "V_init")
bounds <- get_parameter_bounds(param_names, v_inj = 0.01)

assert_true(!is.null(bounds$lower), "返回下界向量")
assert_true(!is.null(bounds$upper), "返回上界向量")
assert_equal(length(bounds$lower), 6, "下界向量长度 = 6")
assert_equal(length(bounds$upper), 6, "上界向量长度 = 6")
assert_equal(bounds$lower["logK1"], 0, "logK1 下界正确")
assert_equal(bounds$upper["H2"], 5000, "H2 上界正确")

bounds_override <- get_parameter_bounds(
  c("logK1", "H1", "Offset"),
  v_inj = 0.01,
  override_bounds = override_bounds
)
assert_equal(bounds_override$lower["logK1"], 3, "get_parameter_bounds override logK1 下界")
assert_equal(bounds_override$upper["logK1"], 4, "get_parameter_bounds override logK1 上界")
assert_equal(bounds_override$lower["H1"], -8000, "get_parameter_bounds override H1 下界")
assert_equal(bounds_override$upper["Offset"], 500, "get_parameter_bounds override Offset 上界")

# 测试8：validate_param_value 函数
cat("\n[测试组] validate_param_value()\n")

assert_true(validate_param_value("logK1", 5), "logK1=5 有效")
assert_true(!validate_param_value("logK1", 15), "logK1=15 无效（超出上界）")
assert_true(!validate_param_value("logK1", -1), "logK1=-1 无效（低于下界）")
assert_true(validate_param_value("H1", -6000), "H1=-6000 有效")
assert_true(!validate_param_value("H1", -20000), "H1=-20000 无效")

# ==============================================================================
# 测试工具函数 (utils.R)
# ==============================================================================

cat("\n=== 测试工具函数 ===\n")

# 测试9：safe_input 函数
cat("\n[测试组] safe_input()\n")

assert_equal(safe_input(NULL, 10), 10, "safe_input(NULL, 10) = 10")
assert_equal(safe_input(NA, 10), 10, "safe_input(NA, 10) = 10")
assert_equal(safe_input(c(), 10), 10, "safe_input(c(), 10) = 10")
assert_equal(safe_input(5, 10), 5, "safe_input(5, 10) = 5")
assert_equal(safe_input("hello", "default"), "hello", "safe_input('hello', 'default') = 'hello'")

# 测试10：safe_numeric 函数
cat("\n[测试组] safe_numeric()\n")

assert_equal(safe_numeric(5.5, 1.0), 5.5, "safe_numeric(5.5, 1.0) = 5.5")
assert_equal(safe_numeric(NULL, 1.0), 1.0, "safe_numeric(NULL, 1.0) = 1.0")
assert_equal(safe_numeric("abc", 1.0), 1.0, "safe_numeric('abc', 1.0) = 1.0")
assert_equal(safe_numeric(-5, 1.0, min = 0), 0, "safe_numeric(-5, 1.0, min=0) = 0（下限）")
assert_equal(safe_numeric(15, 1.0, max = 10), 10, "safe_numeric(15, 1.0, max=10) = 10（上限）")
assert_equal(safe_numeric(7, 1.0, min = 0, max = 10), 7, "safe_numeric(7, ...) = 7（范围内）")

# 测试11：validate_dataframe 函数
cat("\n[测试组] validate_dataframe()\n")

# 创建测试数据框
df_valid <- data.frame(Q = 1:10, inj_idx = 1:10)
df_empty <- data.frame()
df_missing_col <- data.frame(Q = 1:10)

assert_true(validate_dataframe(df_valid), "有效数据框通过验证")
assert_true(!validate_dataframe(df_empty), "空数据框验证失败")
assert_true(!validate_dataframe(NULL), "NULL 验证失败")
assert_true(!validate_dataframe(df_valid, min_rows = 20), "行数不足验证失败")
assert_true(validate_dataframe(df_valid, required_cols = c("Q")), "包含必需列通过验证")
assert_true(!validate_dataframe(df_missing_col, required_cols = c("Q", "inj_idx")), 
           "缺少必需列验证失败")

# 测试12：safe_log 函数
cat("\n[测试组] safe_log()\n")

assert_equal(safe_log(10), log(10), "safe_log(10) = log(10)", tolerance = 1e-10)
assert_equal(safe_log(0), log(EPSILON_LOG), "safe_log(0) = log(EPSILON_LOG)", tolerance = 1e-10)
assert_equal(safe_log(-5), log(EPSILON_LOG), "safe_log(-5) = log(EPSILON_LOG)", tolerance = 1e-10)

# 测试13：safe_divide 函数
cat("\n[测试组] safe_divide()\n")

assert_equal(safe_divide(10, 2), 5, "safe_divide(10, 2) = 5")
assert_equal(safe_divide(10, 0), 10 / EPSILON, "safe_divide(10, 0) 使用 EPSILON", tolerance = 1e-5)
assert_equal(safe_divide(10, -1e-30), 10 / EPSILON, "safe_divide(10, 极小负数)", tolerance = 1e-5)

# 测试14：is_near_zero 函数
cat("\n[测试组] is_near_zero()\n")

assert_true(is_near_zero(0), "is_near_zero(0) = TRUE")
assert_true(is_near_zero(1e-25), "is_near_zero(1e-25) = TRUE")
assert_true(!is_near_zero(0.01), "is_near_zero(0.01) = FALSE")
assert_true(!is_near_zero(1), "is_near_zero(1) = FALSE")

# 测试15：format_number 函数
cat("\n[测试组] format_number()\n")

formatted <- format_number(1234.5678, digits = 2)
assert_true(grepl("1234.57", formatted), "format_number(1234.5678, 2) 格式正确")

formatted <- format_number(1e-5, digits = 2, scientific = TRUE)
assert_true(grepl("e", formatted), "科学计数法包含 'e'")

formatted <- format_number(NULL)
assert_equal(formatted, "N/A", "format_number(NULL) = 'N/A'")

# 测试16：truncate_string 函数
cat("\n[测试组] truncate_string()\n")

long_str <- paste(rep("a", 100), collapse = "")
truncated <- truncate_string(long_str, max_length = 20)
assert_equal(nchar(truncated), 20, "截断后长度 = 20")
assert_true(grepl("\\.\\.\\.$", truncated), "截断后以 '...' 结尾")

short_str <- "short"
truncated <- truncate_string(short_str, max_length = 20)
assert_equal(truncated, short_str, "短字符串不截断")

# 测试17：cache_hit 函数
cat("\n[测试组] cache_hit()\n")

assert_true(!cache_hit(NULL, NULL, "key1"), "空缓存返回 FALSE")
assert_true(cache_hit("value", "key1", "key1"), "相同键返回 TRUE")
assert_true(!cache_hit("value", "key1", "key2"), "不同键返回 FALSE")

# 测试18：safe_execute 函数（错误处理）
cat("\n[测试组] safe_execute()\n")

# 测试成功执行
result <- safe_execute({
  2 + 2
}, context = "测试加法", show_error = FALSE)
assert_equal(result, 4, "safe_execute 成功执行返回结果")

# 测试错误处理
result <- safe_execute({
  stop("测试错误")
}, context = "测试错误", default = "默认值", show_error = FALSE)
assert_equal(result, "默认值", "safe_execute 错误时返回默认值")

# 测试 NULL 默认值
result <- safe_execute({
  stop("测试错误")
}, context = "测试错误", show_error = FALSE)
assert_true(is.null(result), "safe_execute 错误时默认返回 NULL")

# ==============================================================================
# 集成测试
# ==============================================================================

cat("\n=== 集成测试 ===\n")

# 测试19：完整的参数边界使用流程
cat("\n[测试组] 完整参数边界流程\n")

# 模拟拟合场景
param_names <- c("logK1", "logK2", "H1", "H2", "fH", "V_init", "Offset")
v_inj_value <- 0.015

# 获取边界
bounds <- get_parameter_bounds(param_names, v_inj = v_inj_value)

# 验证所有参数都有边界
assert_equal(length(bounds$lower), length(param_names), "所有参数都有下界")
assert_equal(length(bounds$upper), length(param_names), "所有参数都有上界")

# 验证边界合理性
for(i in seq_along(param_names)) {
  nm <- param_names[i]
  assert_true(bounds$lower[i] < bounds$upper[i], 
             sprintf("%s: 下界 < 上界", nm))
}

# 验证特定参数的边界
assert_equal(bounds$lower["V_init"], 0, "V_init 下界 = 0")
assert_equal(bounds$upper["V_init"], v_inj_value, 
            sprintf("V_init 上界 = %.3f", v_inj_value))

# 测试20：数据处理流程
cat("\n[测试组] 数据处理流程\n")

# 创建测试数据
test_data <- data.frame(
  inj_idx = 1:20,
  Q = rnorm(20, mean = -5000, sd = 500)
)

# 验证数据
is_valid <- validate_dataframe(test_data, 
                               required_cols = c("inj_idx", "Q"),
                               min_rows = 5)
assert_true(is_valid, "测试数据验证通过")

# 处理数据（安全操作）
processed_data <- safe_execute({
  # 模拟一些数据处理
  test_data$Q_norm <- test_data$Q / max(abs(test_data$Q))
  test_data
}, context = "数据处理", show_error = FALSE)

assert_true(!is.null(processed_data), "数据处理成功")
assert_true("Q_norm" %in% colnames(processed_data), "新列添加成功")

# ==============================================================================
# 性能测试
# ==============================================================================

cat("\n=== 性能测试 ===\n")

# 测试21：time_it 函数
cat("\n[测试组] time_it()\n")

# 测试耗时测量
timing_result <- time_it({
  Sys.sleep(0.1)
}, label = "睡眠测试", log_result = FALSE)

assert_true("result" %in% names(timing_result), "time_it 返回 result 字段")
assert_true(!is.null(timing_result$elapsed), "time_it 返回耗时")
assert_true(timing_result$elapsed >= 0.09, "耗时测量准确（>= 0.09秒）")

# 测试22：大量参数的边界获取性能
cat("\n[测试组] 性能测试\n")

large_param_names <- c(
  paste0("logK", 1:10),
  paste0("H", 1:10),
  paste0("fH", 1:5),
  "V_init", "Offset"
)

perf_result <- time_it({
  bounds <- get_parameter_bounds(large_param_names, v_inj = 0.01)
}, label = "大量参数边界获取", log_result = FALSE)

assert_true(perf_result$elapsed < 1.0, 
           sprintf("大量参数处理快速（%.3f秒 < 1秒）", perf_result$elapsed))

# ==============================================================================
# 测试总结
# ==============================================================================

cat("\n" , paste(rep("=", 70), collapse = ""), "\n")
cat("测试总结\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat(sprintf("通过: %d\n", tests_passed))
cat(sprintf("失败: %d\n", tests_failed))
cat(sprintf("总计: %d\n", tests_passed + tests_failed))

if(tests_failed == 0) {
  cat("\n✓ 所有测试通过！\n")
  cat("常量定义和工具函数已准备好使用。\n")
} else {
  cat("\n✗ 有测试失败，请检查实现。\n")
}

cat(paste(rep("=", 70), collapse = ""), "\n")

# 返回测试结果
invisible(list(passed = tests_passed, failed = tests_failed))
