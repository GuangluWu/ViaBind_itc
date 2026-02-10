# ==============================================================================
# global.R - 全局依赖和配置
# ==============================================================================

# 必需的 R 包
library(shiny)
library(ggplot2)
library(patchwork)
library(readxl)
library(jsonlite)

# 可选依赖（颜色选择器）
if (!requireNamespace("colourpicker", quietly = TRUE)) {
  warning("Package 'colourpicker' is not installed. Color pickers will use text input instead.")
}

# 加载 i18n 翻译模块
source("R/i18n.R", local = FALSE)

# 加载能量单位标签模块（类似 i18n，用于 cal/J 切换）
source("R/energy_units.R", local = FALSE)

# 验证翻译函数
if (!exists("tr", envir = .GlobalEnv)) {
  stop("Failed to load tr() function from R/i18n.R.")
}

# 全局默认值
PLOT_DEFAULTS <- list(
  # 上 Panel (Thermogram)
  top_xlab       = "Time (min)",
  top_ylab        = "Delta Power (\u00B5cal/s)",
  top_line_color = "#D66666",
  top_line_width = 0.5,
  top_time_unit  = "min",   # "s" 或 "min"

  # 下 Panel (Isotherm)
  bot_xlab        = "Molar Ratio",
  bot_ylab        = "Heat of Injection (kcal/mol)",
  heat_offset     = 0,           # 基线热扣除 (cal/mol)，积分与拟合数据共用
  energy_unit     = "cal",       # "cal" 或 "J"；J 时上 panel µW、下 panel kJ/mol
  bot_point_color = "#000000",
  bot_point_shape = 21,
  bot_point_size  = 3,
  bot_point_fill  = "#D66666",
  bot_point_fill_alpha = 0.7,
  bot_layer_order = "points_over_line",
  bot_dim_first_point = TRUE,
  bot_line_color   = "#000000",
  bot_line_width   = 0.6,
  bot_line_linetype = "longdash",  # solid, dashed, dotted, dotdash, longdash
  
  # 全局
  base_size        = 12,
  height_ratio     = 1,
  border_linewidth = 0.3,
  export_width     = 4,
  export_height    = 6,
  export_dpi       = 300
)
