# ==============================================================================
# 验证模块加载脚本
# ==============================================================================
# 运行此脚本验证 Phase 1 重构后的模块是否正确加载

cat("开始验证模块加载...\n\n")

# 1. 加载全局配置
cat("1. 加载 global.R...\n")
tryCatch({
  source("global.R")
  cat("   ✓ global.R 加载成功\n")
}, error = function(e) {
  cat("   ✗ global.R 加载失败:", e$message, "\n")
  stop("模块加载失败")
})

# 2. 加载核心逻辑
cat("2. 加载 R/core_logic.R...\n")
tryCatch({
  source("R/core_logic.R")
  cat("   ✓ core_logic.R 加载成功\n")
}, error = function(e) {
  cat("   ✗ core_logic.R 加载失败:", e$message, "\n")
  stop("模块加载失败")
})

# 3. 加载误差分析
cat("3. 加载 R/error_analysis.R...\n")
tryCatch({
  source("R/error_analysis.R")
  cat("   ✓ error_analysis.R 加载成功\n")
}, error = function(e) {
  cat("   ✗ error_analysis.R 加载失败:", e$message, "\n")
  stop("模块加载失败")
})

# 4. 加载拟合函数
cat("4. 加载 R/fitting.R...\n")
tryCatch({
  source("R/fitting.R")
  cat("   ✓ fitting.R 加载成功\n")
}, error = function(e) {
  cat("   ✗ fitting.R 加载失败:", e$message, "\n")
  stop("模块加载失败")
})

# 5. 验证函数是否存在
cat("\n5. 验证函数是否存在...\n")
functions_to_check <- c(
  "solve_equi_modular",
  "run_sim_modular",
  "calculate_hessian_ci",
  "calculate_hessian_ci_robust",
  "calculate_parametric_bootstrap_ci",
  "calculate_bootstrap_ci_full",
  "calculate_simulation"
)

all_ok <- TRUE
for (fn in functions_to_check) {
  if (exists(fn)) {
    cat("   ✓", fn, "存在\n")
  } else {
    cat("   ✗", fn, "不存在\n")
    all_ok <- FALSE
  }
}

# 6. 总结
cat("\n", paste(rep("=", 50), collapse=""), "\n")
if (all_ok) {
  cat("✓ 所有模块加载成功！Phase 1 重构完成。\n")
} else {
  cat("✗ 部分函数缺失，请检查模块文件。\n")
}
cat(paste(rep("=", 50), collapse=""), "\n")
