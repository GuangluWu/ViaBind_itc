# ==============================================================================
# ITCgraph - ITC 数据可视化与出版级绘图工具
# ==============================================================================
#
# 功能说明：
# 读取 ITCsimfit 导出的拟合数据（xlsx），生成可发表质量的 ITC 图：
# - 上 Panel：Thermogram（扣完基线的滴定功率 vs 时间）
# - 下 Panel：Isotherm（积分热/拟合曲线 vs 摩尔比）
#
# ==============================================================================

# 加载全局配置和依赖
source("global.R")

# 加载核心函数模块
source("R/plotting.R")
source("R/bridge_plot_helpers.R")

# 加载 UI 和 Server
source("ui.R")
source("server.R")

# 启动 Shiny 应用
shinyApp(ui = ui, server = server)
