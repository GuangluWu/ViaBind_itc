# ==============================================================================
# ITCgraph - ITC 数据可视化与出版级绘图工具
# ==============================================================================
# [COMMENT_STD][MODULE_HEADER]
# 模块职责：作为 Step3 应用入口，加载绘图模块并启动 Shiny 应用。
# 依赖：global.R、R/plotting.R、R/bridge_plot_helpers.R、R/guide_annotations.R。
# 对外接口：ui、server、shinyApp(ui, server)。
# 副作用：启动时读取模块并建立全局函数符号。
# 变更历史：2026-02-12 - 增加 Phase 4 注释规范与 guide annotations 预埋加载。
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
source("R/i18n.R")
source("R/guide_annotations.R")

if (!exists("load_guide_annotations", mode = "function")) {
  stop("Failed to load guide annotation module from R/guide_annotations.R", call. = FALSE)
}
if (!exists("graph_tr", mode = "function")) {
  stop("Failed to load i18n module from R/i18n.R", call. = FALSE)
}

# [COMMENT_STD][ERROR_SEMANTICS]
# 错误码/类别：启动阶段模块加载失败属于初始化错误。
# 触发条件：guide_annotations 模块缺失或语法错误导致符号未导出。
# 用户可见性：启动即失败，错误直接可见。
# 日志级别：error。
# 恢复动作：中止启动，修复模块后重启应用。

# 加载 UI 和 Server
source("ui.R")
source("server.R")

# 启动 Shiny 应用
shinyApp(ui = ui, server = server)
