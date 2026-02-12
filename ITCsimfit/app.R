# ==============================================================================
# ITC模拟与拟合应用程序 (ITC Simulator & Fitter) - Phase 1 完整重构版本
# ==============================================================================
# 
# 功能说明：
# 本程序用于模拟和拟合等温滴定量热（ITC）实验数据。
# 
# Phase 1 重构说明：
# - 核心算法已提取到 R/core_logic.R
# - 误差分析函数已提取到 R/error_analysis.R
# - 拟合函数已提取到 R/fitting.R
# - UI 代码已拆分到 ui.R
# - Server 代码已拆分到 server.R
# - 本文件作为应用入口，负责加载所有模块并启动应用
#
# ==============================================================================

# 加载全局配置和依赖
source("global.R")

# 加载核心函数模块
source("R/core_logic.R")
source("R/error_analysis.R")
source("R/fitting.R")
source("R/weighting.R")
source("R/visualization.R")
source("R/bridge_step1_import.R")
source("R/export_bundle_helpers.R")

# 加载 UI 和 Server
source("ui.R")
source("server.R")

# 启动 Shiny 应用
shinyApp(ui = ui, server = server)
