# ==============================================================================
# global.R - 全局依赖和配置
# ==============================================================================
# 本文件在 app.R 之前加载，包含所有必需的库和全局配置

# 必需的 R 包
library(shiny)      # 用于创建 Shiny 应用程序
library(rootSolve)  # 用于求解非线性方程组 (multiroot)
library(tidyverse)  # 数据处理与可视化
library(DT)         # 交互式表格
library(DEoptim)    # 用于全局搜索拟合
library(readxl)     # 用于读取 xlsx 文件（与 ITCprocessor 数据对接）
library(writexl)    # 用于导出 xlsx 文件

# 注意：已移除异步拟合功能，使用同步模式以确保用户不会在拟合时进行其他操作

# 可选依赖（用于误差分析）
if (!requireNamespace("MASS", quietly = TRUE)) {
  # 如果 MASS 未安装，将在需要时提示
}

# 可选依赖（用于可视化）
if (!requireNamespace("gridExtra", quietly = TRUE)) {
  # 如果 gridExtra 未安装，将在需要时提示
  # 用于组合多个图形（残差诊断图）
}

# 加载核心函数模块
# 注意：Shiny 会自动加载 R/ 目录下的所有 .R 文件
# 但为了明确依赖关系，我们在这里显式 source（可选）

# 1. 加载常量定义（最先加载，供其他模块使用）
source("R/constants.R", local = FALSE)
if (!exists("PARAM_BOUNDS", envir = .GlobalEnv)) {
  stop("Failed to load constants from R/constants.R. Please check the file.")
}

# 2. 加载工具函数（包含错误处理）
source("R/infrastructure/logging.R", local = FALSE)
if (!exists("itc_log", envir = .GlobalEnv)) {
  stop("Failed to load logging interface from R/infrastructure/logging.R.")
}

source("R/infrastructure/errors.R", local = FALSE)
if (!exists("itc_error", envir = .GlobalEnv)) {
  stop("Failed to load error interface from R/infrastructure/errors.R.")
}

source("R/utils.R", local = FALSE)
if (!exists("handle_error", envir = .GlobalEnv)) {
  stop("Failed to load utility functions from R/utils.R. Please check the file.")
}

source("R/path_selection_helpers.R", local = FALSE)
if (!exists("normalize_active_paths_with_dependencies", envir = .GlobalEnv)) {
  stop("Failed to load path selection helpers from R/path_selection_helpers.R.")
}

# 3. 加载 i18n 翻译模块（需要在其他模块之前加载）
# 使用 local = FALSE 确保函数加载到全局环境
source("R/i18n.R", local = FALSE)

# 验证函数是否成功加载
if (!exists("tr", envir = .GlobalEnv)) {
  stop("Failed to load tr() function from R/i18n.R. Please check the file path and syntax.")
}
if (!exists("trf", envir = .GlobalEnv)) {
  stop("Failed to load trf() function from R/i18n.R. Please check the file path and syntax.")
}

# 3.1 加载物种分布导出/作图 helper（Step 2 Species Dist. 共享逻辑）
source("R/species_dist_helpers.R", local = FALSE)
if (!exists("build_species_dist_plot", envir = .GlobalEnv)) {
  stop("Failed to load species dist helpers from R/species_dist_helpers.R.")
}

# 4. 加载引导注释预埋模块（Phase 4，配置驱动，默认不启用 UI）
source("R/guide_annotations.R", local = FALSE)
if (!exists("load_guide_annotations", envir = .GlobalEnv)) {
  stop("Failed to load guide annotation module from R/guide_annotations.R.")
}

# 5. 加载性能监控模块
source("R/performance.R", local = FALSE)
if (!exists("perf_monitor", envir = .GlobalEnv)) {
  warning("Failed to load performance monitoring module from R/performance.R.")
}

# 全局配置
# 是否启用调试模式（在生产环境中设置为 FALSE）
DEBUG_MODE <- FALSE

# 是否启用性能监控（在生产环境中可以设置为 FALSE）
PERFORMANCE_MONITORING <- TRUE
