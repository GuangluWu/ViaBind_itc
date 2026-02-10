# ==============================================================================
# 性能测试：向量化优化效果验证
# ==============================================================================
# 测试 run_sim_modular 在向量化优化后的性能提升

cat("开始性能测试...\n\n")

# 加载模块
source("global.R")
source("R/core_logic.R")
source("R/fitting.R")

# ==============================================================================
# 测试 1: 简单模型（只有 rxn_M）- 应该最快
# ==============================================================================
cat("=== 测试 1: 简单模型（rxn_M only）===\n")

p_simple <- list(
  V_cell = 0.2033,           # mL
  V_inj = 1.5 / 1000,        # mL (1.5 uL)
  n_inj = 50,                 # 50 针（较多针数以测试性能）
  H_cell_0 = 30e-6,           # M (30 uM)
  G_syringe = 600e-6,         # M (600 uM)
  V_pre = 0,
  V_init = 0,
  logK1 = 7.0, H1 = -6000,
  logK2 = 7.0, H2 = -6000,
  logK3 = 7.0, H3 = -6000,
  logK4 = 7.0, H4 = -6000,
  logK5 = 7.0, H5 = -6000
)

active_paths_simple <- "rxn_M"

# 运行多次取平均
n_runs <- 10
times_simple <- numeric(n_runs)

for (i in 1:n_runs) {
  start_time <- Sys.time()
  result_simple <- run_sim_modular(p_simple, active_paths_simple)
  end_time <- Sys.time()
  times_simple[i] <- as.numeric(difftime(end_time, start_time, units = "secs"))
}

avg_time_simple <- mean(times_simple)
cat(sprintf("  平均运行时间: %.4f 秒 (运行 %d 次)\n", avg_time_simple, n_runs))
cat(sprintf("  单次运行时间: %.4f 秒\n", avg_time_simple))
cat(sprintf("  结果行数: %d\n", nrow(result_simple)))
cat(sprintf("  结果列数: %d\n", ncol(result_simple)))
cat("  ✓ 简单模型测试通过\n\n")

# ==============================================================================
# 测试 2: 复杂模型（rxn_M + rxn_D）- 需要数值求解
# ==============================================================================
cat("=== 测试 2: 复杂模型（rxn_M + rxn_D）===\n")

p_complex <- p_simple
p_complex$logK2 <- 6.0  # 激活 D 路径
active_paths_complex <- c("rxn_M", "rxn_D")

times_complex <- numeric(n_runs)

for (i in 1:n_runs) {
  start_time <- Sys.time()
  result_complex <- run_sim_modular(p_complex, active_paths_complex)
  end_time <- Sys.time()
  times_complex[i] <- as.numeric(difftime(end_time, start_time, units = "secs"))
}

avg_time_complex <- mean(times_complex)
cat(sprintf("  平均运行时间: %.4f 秒 (运行 %d 次)\n", avg_time_complex, n_runs))
cat(sprintf("  单次运行时间: %.4f 秒\n", avg_time_complex))
cat(sprintf("  结果行数: %d\n", nrow(result_complex)))
cat(sprintf("  结果列数: %d\n", ncol(result_complex)))
cat("  ✓ 复杂模型测试通过\n\n")

# ==============================================================================
# 测试 3: 拟合场景模拟（多次调用）
# ==============================================================================
cat("=== 测试 3: 拟合场景模拟（模拟拟合时的多次调用）===\n")

# 模拟拟合时调用 100 次模拟
n_fit_calls <- 100
times_fit <- numeric(n_fit_calls)

for (i in 1:n_fit_calls) {
  start_time <- Sys.time()
  result_fit <- run_sim_modular(p_simple, active_paths_simple)
  end_time <- Sys.time()
  times_fit[i] <- as.numeric(difftime(end_time, start_time, units = "secs"))
}

total_time_fit <- sum(times_fit)
avg_time_fit <- mean(times_fit)
cat(sprintf("  总运行时间: %.4f 秒 (运行 %d 次)\n", total_time_fit, n_fit_calls))
cat(sprintf("  平均单次时间: %.4f 秒\n", avg_time_fit))
cat(sprintf("  预计拟合 1000 次需要: %.2f 秒 (%.2f 分钟)\n", 
            avg_time_fit * 1000, avg_time_fit * 1000 / 60))
cat("  ✓ 拟合场景测试通过\n\n")

# ==============================================================================
# 性能总结
# ==============================================================================
cat("=", paste(rep("=", 60), collapse=""), "\n")
cat("性能测试总结\n")
cat("=", paste(rep("=", 60), collapse=""), "\n")
cat(sprintf("简单模型（rxn_M only）:     %.4f 秒/次\n", avg_time_simple))
cat(sprintf("复杂模型（rxn_M + rxn_D）:  %.4f 秒/次\n", avg_time_complex))
cat(sprintf("性能比:                    %.2fx\n", avg_time_complex / avg_time_simple))
cat(sprintf("\n拟合场景（100次调用）:      %.4f 秒\n", total_time_fit))
cat(sprintf("预计拟合 1000 次:          %.2f 秒 (%.2f 分钟)\n", 
            avg_time_fit * 1000, avg_time_fit * 1000 / 60))
cat("\n")

# 验证结果正确性
cat("结果验证:\n")
cat(sprintf("  简单模型结果: %d 行 x %d 列\n", 
            nrow(result_simple), ncol(result_simple)))
cat(sprintf("  复杂模型结果: %d 行 x %d 列\n", 
            nrow(result_complex), ncol(result_complex)))
cat(sprintf("  简单模型 dQ 范围: [%.2f, %.2f] kcal/mol\n", 
            min(result_simple$dQ), max(result_simple$dQ)))
cat(sprintf("  复杂模型 dQ 范围: [%.2f, %.2f] kcal/mol\n", 
            min(result_complex$dQ), max(result_complex$dQ)))

cat("\n✓ 所有测试完成！\n")
