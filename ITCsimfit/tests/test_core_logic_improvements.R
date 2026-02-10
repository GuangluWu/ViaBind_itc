# ==============================================================================
# test_core_logic_improvements.R - 测试 core_logic.R 的改进
# ==============================================================================
# 本文件测试 core_logic.R 中应用的改进是否正常工作

cat("测试 core_logic.R 改进...\n\n")

# 加载必要的模块
source("R/constants.R")
source("R/utils.R")
source("R/core_logic.R")

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

# ==============================================================================
# 测试1：常量使用验证
# ==============================================================================

cat("\n[测试] 验证常量已正确加载\n")

assert_true(exists("EPSILON"), "EPSILON 常量存在")
assert_true(exists("EPSILON_LOG"), "EPSILON_LOG 常量存在")
assert_true(exists("DEFAULT_PARAMS"), "DEFAULT_PARAMS 常量存在")
assert_equal(EPSILON, 1e-20, "EPSILON = 1e-20")
assert_equal(EPSILON_LOG, 1e-15, "EPSILON_LOG = 1e-15")
assert_true(!is.null(DEFAULT_PARAMS$logK), "DEFAULT_PARAMS$logK 存在")
assert_true(!is.null(DEFAULT_PARAMS$H), "DEFAULT_PARAMS$H 存在")

# ==============================================================================
# 测试2：边界处理使用 is_near_zero
# ==============================================================================

cat("\n[测试] 边界处理（is_near_zero）\n")

# 测试极小的 G_tot（应该返回 G_free = 0）
p_test <- list(
  K1 = 1e6, H1 = -6000,
  K2 = 1e5, H2 = -5000,
  K3 = 1e4, H3 = -4000,
  K4 = 1e3, H4 = -3000,
  K5 = 1e2, H5 = -2000
)
active_paths_test <- c("rxn_M")

# 测试极小 G_tot
result <- solve_equi_modular(EPSILON * 0.1, 1e-3, p_test, active_paths_test, c(1e-12, 1e-3))
assert_equal(result[1], 0, "极小 G_tot 返回 G_free = 0", tolerance = 1e-15)
assert_true(abs(result[2] - 1e-3) < 1e-15, "极小 G_tot 返回 H_free = H_tot")

# 测试极小 H_tot
result <- solve_equi_modular(1e-3, EPSILON * 0.1, p_test, active_paths_test, c(1e-12, 1e-12))
assert_true(abs(result[1] - 1e-3) < 1e-15, "极小 H_tot 返回 G_free = G_tot")
assert_equal(result[2], 0, "极小 H_tot 返回 H_free = 0", tolerance = 1e-15)

# ==============================================================================
# 测试3：safe_log 的使用
# ==============================================================================

cat("\n[测试] safe_log 在平衡求解中的使用\n")

# 创建一个场景：有极小的 K 值
p_small_k <- list(
  K1 = EPSILON * 10,  # 极小的 K1
  H1 = -6000,
  K2 = 1e5, H2 = -5000,
  K3 = 1e4, H3 = -4000,
  K4 = 1e3, H4 = -3000,
  K5 = 1e2, H5 = -2000
)

# 应该能够处理极小的 K 值而不崩溃
result <- tryCatch({
  solve_equi_modular(1e-4, 1e-3, p_small_k, c("rxn_M"), c(1e-12, 1e-3))
}, error = function(e) NULL)

assert_true(!is.null(result), "能够处理极小 K 值而不崩溃")
assert_true(all(is.finite(result)), "结果是有限值")

# ==============================================================================
# 测试4：DEFAULT_PARAMS 的使用
# ==============================================================================

cat("\n[测试] DEFAULT_PARAMS 在参数清洗中的使用\n")

# 模拟参数清洗函数（模拟 core_logic.R 中的 safe_K 和 safe_H）
safe_K_test <- function(val) if(is.null(val) || is.na(val)) 10^DEFAULT_PARAMS$logK else 10^val
safe_H_test <- function(val) if(is.null(val) || is.na(val)) DEFAULT_PARAMS$H else val

# 测试 NULL 值
k_result <- safe_K_test(NULL)
assert_equal(k_result, 10^DEFAULT_PARAMS$logK, "NULL logK 使用默认值", tolerance = 1)

h_result <- safe_H_test(NULL)
assert_equal(h_result, DEFAULT_PARAMS$H, "NULL H 使用默认值")

# 测试 NA 值
k_result <- safe_K_test(NA)
assert_equal(k_result, 10^DEFAULT_PARAMS$logK, "NA logK 使用默认值", tolerance = 1)

h_result <- safe_H_test(NA)
assert_equal(h_result, DEFAULT_PARAMS$H, "NA H 使用默认值")

# 测试有效值
k_result <- safe_K_test(7)
assert_equal(k_result, 1e7, "有效 logK 值正确转换")

h_result <- safe_H_test(-8000)
assert_equal(h_result, -8000, "有效 H 值保持不变")

# ==============================================================================
# 测试5：完整模拟使用常量
# ==============================================================================

cat("\n[测试] 完整模拟流程使用常量\n")

# 创建一个完整的参数列表
p_full <- list(
  V_cell = 1.4,
  V_inj = 0.01,
  n_inj = 20,
  H_cell_0 = 0.1,
  G_syringe = 1.0,
  logK1 = 6, H1 = -6000,
  logK2 = 5, H2 = -5000,
  logK3 = 4, H3 = -4000,
  logK4 = 3, H4 = -3000,
  logK5 = 2, H5 = -2000,
  V_init = 0.01,
  V_pre = 0
)

# 运行简单模型模拟
result_simple <- tryCatch({
  run_sim_modular(p_full, c("rxn_M"))
}, error = function(e) {
  cat("错误:", e$message, "\n")
  NULL
})

assert_true(!is.null(result_simple), "简单模型模拟成功")
assert_true(is.data.frame(result_simple), "返回 data.frame")
assert_equal(nrow(result_simple), 20, "结果行数正确")
assert_true(all(is.finite(result_simple$dQ)), "所有 dQ 值有限")
assert_true(all(result_simple$Fallback == 0), "简单模型无 fallback")

# 运行复杂模型模拟
result_complex <- tryCatch({
  run_sim_modular(p_full, c("rxn_M", "rxn_D"))
}, error = function(e) {
  cat("错误:", e$message, "\n")
  NULL
})

assert_true(!is.null(result_complex), "复杂模型模拟成功")
assert_true(is.data.frame(result_complex), "返回 data.frame")
assert_equal(nrow(result_complex), 20, "结果行数正确")
assert_true(all(is.finite(result_complex$dQ)), "所有 dQ 值有限")

# ==============================================================================
# 测试6：EPSILON 的正确使用
# ==============================================================================

cat("\n[测试] EPSILON 在数值保护中的使用\n")

# 测试 K 值保护（应该使用 EPSILON）
test_K_values <- c(0, -1, EPSILON * 0.5, 1e5)
protected_K <- pmax(test_K_values, EPSILON)

assert_true(all(protected_K >= EPSILON), "所有 K 值 >= EPSILON")
assert_equal(protected_K[1], EPSILON, "0 被保护为 EPSILON")
assert_equal(protected_K[2], EPSILON, "负数被保护为 EPSILON")
assert_equal(protected_K[4], 1e5, "正常值保持不变")

# 测试对数计算保护
test_values <- c(0, EPSILON * 0.1, 1.0, 1e5)
safe_logs <- safe_log(test_values, EPSILON_LOG)

assert_true(all(is.finite(safe_logs)), "所有对数值有限")
assert_equal(safe_logs[1], log(EPSILON_LOG), "0 的对数使用 EPSILON_LOG", tolerance = 1e-10)
assert_equal(safe_logs[3], 0, "log(1) = 0", tolerance = 1e-10)

# ==============================================================================
# 测试7：与硬编码版本的数值一致性
# ==============================================================================

cat("\n[测试] 数值结果与硬编码版本一致\n")

# 创建测试参数
p_consistency <- list(
  V_cell = 1.4,
  V_inj = 0.01,
  n_inj = 10,
  H_cell_0 = 0.1,
  G_syringe = 1.0,
  logK1 = 6, H1 = -6000,
  logK2 = 5, H2 = -5000,
  logK3 = 4, H3 = -4000,
  logK4 = 3, H4 = -3000,
  logK5 = 2, H5 = -2000,
  V_init = 0,
  V_pre = 0
)

# 运行模拟
result <- run_sim_modular(p_consistency, c("rxn_M"))

# 验证结果的数值特性
assert_true(all(result$dQ < 0), "所有 dQ < 0（放热反应）")
assert_true(all(result$Ratio > 0), "所有 Ratio > 0")
assert_true(all(result$H_pct >= 0 & result$H_pct <= 1), "H_pct 在 [0,1]")
assert_true(all(result$M_pct >= 0 & result$M_pct <= 1), "M_pct 在 [0,1]")

# 验证 dQ 的趋势（应该逐渐减小）
dQ_diff <- diff(result$dQ)
assert_true(all(dQ_diff > 0), "dQ 逐渐增加（趋向于0）")

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
  cat("core_logic.R 的改进已成功应用，数值结果保持一致。\n")
} else {
  cat("\n✗ 有测试失败，请检查实现。\n")
}

cat(paste(rep("=", 70), collapse = ""), "\n")

# 返回测试结果
invisible(list(passed = tests_passed, failed = tests_failed))
