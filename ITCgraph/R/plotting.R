# ==============================================================================
# R/plotting.R - ITC 出版级绘图核心函数
# ==============================================================================
# 使用 ggplot2 + patchwork 生成上下双 panel ITC 图
# 参考经典 ITC 文献图风格：
#   - 两个 panel 上下紧贴（零间距）
#   - 上 panel X 轴（时间）标在顶部
#   - 两个 panel 都用 axis.line 绘制边框，确保视觉统一
#   - 时间轴默认使用 min


#' 创建 ITC 出版级图形（上下双 Panel）
#'
#' @param power_data data.frame 功率数据，包含 Time_s 和 Power_corrected_ucal_s
#' @param integration_data data.frame 积分数据，包含 Ratio_App 和 heat_cal_mol
#' @param simulation_data data.frame 模拟数据，包含 Ratio_App 和 dQ_App
#' @param params list 绘图参数
#' @return patchwork 组合图形对象
create_itc_figure <- function(power_data = NULL,
                              integration_data = NULL,
                              simulation_data = NULL,
                              params = list()) {
  
  # 合并默认参数
  p <- modifyList(PLOT_DEFAULTS, params)
  
  # ---- 构建上 Panel (Thermogram) ----
  p_top <- create_thermogram_panel(power_data, p)
  
  # ---- 构建下 Panel (Isotherm) ----
  p_bot <- create_isotherm_panel(integration_data, simulation_data, p)
  
  # ---- 使用 patchwork 组合 ----
  combined <- p_top / p_bot +
    plot_layout(heights = c(p$height_ratio, 1))
  
  return(combined)
}


#' 创建 Thermogram Panel（上 Panel）
#' X 轴在顶部，四面加框
#'
#' @param power_data data.frame 功率数据
#' @param p list 绘图参数
#' @return ggplot 对象
create_thermogram_panel <- function(power_data, p) {
  
  if (is.null(power_data) || nrow(power_data) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No thermogram data", size = 5, color = "grey50") +
        theme_void()
    )
  }
  
  # 时间单位转换；能量单位：cal -> ucal/s，J -> uW (1 ucal/s = 4.184 uW)
  time_unit <- if (!is.null(p$top_time_unit)) p$top_time_unit else "min"
  y_power <- power_data$Power_corrected_ucal_s
  if (identical(p$energy_unit, "J")) y_power <- y_power * 4.184
  if (time_unit == "min") {
    plot_data <- data.frame(x = power_data$Time_s / 60, y = y_power)
  } else {
    plot_data <- data.frame(x = power_data$Time_s, y = y_power)
  }
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line(color = p$top_line_color, linewidth = p$top_line_width) +
    labs(y = p$top_ylab) +
    # X 轴标签在顶部，不画底轴；右侧添加 dup 轴以画出右边框
    scale_x_continuous(
      position = "top",
      name = p$top_xlab,
      expand = expansion(mult = 0.02)
    ) +
    theme_bw(base_size = p$base_size) +
    theme(
      # 底部 margin 最小化，与下 panel 紧贴
      plot.margin = margin(t = 5, r = 8, b = 0, l = 8, unit = "pt"),
      # 轴文字
      axis.title.x.top = element_text(size = p$base_size, margin = margin(b = 4)),
      axis.title.y = element_text(size = p$base_size, margin = margin(r = 4)),
      axis.text = element_text(size = p$base_size - 2, color = "black"),
      # 统一用 axis.line 画边框（顶、左、右；不画底边，避免与下 panel 重叠）
      panel.border = element_blank(),
      axis.line.x.top = element_line(colour = "black", linewidth = p$border_linewidth %||% 0.5),
      axis.line.y.left = element_line(colour = "black", linewidth = p$border_linewidth %||% 0.5),
      axis.line.y.right = element_line(colour = "black", linewidth = p$border_linewidth %||% 0.5),
      axis.text.y.right = element_blank(),
      axis.ticks.y.right = element_blank(),
      axis.line.x.bottom = element_blank(),
      axis.ticks.x.bottom = element_blank(),
      axis.text.x.bottom = element_blank(),
      axis.ticks = element_line(color = "black", linewidth = p$border_linewidth %||% 0.5),
      axis.ticks.length = unit(3, "pt"),
      # 无网格
      panel.grid = element_blank()
    )
  
  # 自定义 X 轴范围
  if (!is.null(p$top_xmin) && !is.null(p$top_xmax)) {
    xmin <- p$top_xmin
    xmax <- p$top_xmax
    plot <- plot + scale_x_continuous(
      position = "top", name = p$top_xlab,
      limits = c(xmin, xmax), expand = expansion(mult = 0.02)
    )
  }
  
  # 自定义 Y 轴范围（保留 sec.axis 以维持右边框）
  dup_y <- dup_axis(name = NULL, labels = function(x) rep("", length(x)))
  if (!is.null(p$top_ymin) && !is.null(p$top_ymax)) {
    plot <- plot + scale_y_continuous(limits = c(p$top_ymin, p$top_ymax), sec.axis = dup_y, expand = expansion(mult = 0.05))
  } else {
    plot <- plot + scale_y_continuous(sec.axis = dup_y, expand = expansion(mult = 0.05))
  }
  
  return(plot)
}


#' 创建 Isotherm Panel（下 Panel）
#' 四面加框，与上 panel 紧贴
#'
#' @param integration_data data.frame 积分数据
#' @param simulation_data data.frame 模拟数据
#' @param p list 绘图参数
#' @return ggplot 对象
create_isotherm_panel <- function(integration_data, simulation_data, p) {
  
  has_int <- !is.null(integration_data) && nrow(integration_data) > 0
  has_sim <- !is.null(simulation_data) && nrow(simulation_data) > 0
  
  if (!has_int && !has_sim) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No isotherm data", size = 5, color = "grey50") +
        theme_void()
    )
  }
  
  # 基线热扣除（积分与拟合数据共用），再按单位转换
  offset <- as.numeric(p$heat_offset %||% 0)
  to_kcal <- function(v) (v - offset) / 1000
  if (p$energy_unit == "J") {
    # kcal/mol -> kJ/mol: 乘以 4.184
    to_display <- function(v) to_kcal(v) * 4.184
  } else {
    to_display <- to_kcal
  }
  if (has_int) {
    int_plot <- data.frame(
      x = integration_data$Ratio_App,
      y = to_display(integration_data$heat_cal_mol)
    )
    int_plot <- int_plot[is.finite(int_plot$x) & is.finite(int_plot$y), ]
    has_int <- nrow(int_plot) > 0
  }
  
  if (has_sim) {
    sim_plot <- data.frame(
      x = simulation_data$Ratio_App,
      y = to_display(simulation_data$dQ_App)
    )
    sim_plot <- sim_plot[is.finite(sim_plot$x) & is.finite(sim_plot$y), ]
    has_sim <- nrow(sim_plot) > 0
  }
  
  plot <- ggplot()
  
  fill_alpha <- as.numeric(p$bot_point_fill_alpha %||% 0.7)
  fill_alpha <- max(min(fill_alpha, 1), 0)
  point_fill <- grDevices::adjustcolor(p$bot_point_fill, alpha.f = fill_alpha)
  dim_first <- isTRUE(p$bot_dim_first_point)
  dim_color <- "#C8C8C8"
  dim_point_fill <- grDevices::adjustcolor(dim_color, alpha.f = fill_alpha)
  
  add_line <- function(g) {
    if (has_sim) {
      g + geom_line(
        data = sim_plot,
        aes(x = x, y = y),
        color = p$bot_line_color,
        linewidth = p$bot_line_width,
        linetype = p$bot_line_linetype %||% "solid"
      )
    } else {
      g
    }
  }
  
  add_points <- function(g) {
    if (!has_int) return(g)
    if (dim_first && nrow(int_plot) > 0) {
      first_point <- int_plot[1, , drop = FALSE]
      rest_points <- if (nrow(int_plot) > 1) int_plot[-1, , drop = FALSE] else NULL
      g <- g + geom_point(
        data = first_point,
        aes(x = x, y = y),
        shape = p$bot_point_shape,
        size = p$bot_point_size,
        color = dim_color,
        fill = dim_point_fill,
        stroke = 0.6
      )
      if (!is.null(rest_points) && nrow(rest_points) > 0) {
        g <- g + geom_point(
          data = rest_points,
          aes(x = x, y = y),
          shape = p$bot_point_shape,
          size = p$bot_point_size,
          color = p$bot_point_color,
          fill = point_fill,
          stroke = 0.6
        )
      }
      g
    } else {
      g + geom_point(
        data = int_plot,
        aes(x = x, y = y),
        shape = p$bot_point_shape,
        size = p$bot_point_size,
        color = p$bot_point_color,
        fill = point_fill,
        stroke = 0.6
      )
    }
  }
  
  layer_order <- p$bot_layer_order %||% "points_over_line"
  if (identical(layer_order, "line_over_points")) {
    plot <- add_points(plot)
    plot <- add_line(plot)
  } else {
    plot <- add_line(plot)
    plot <- add_points(plot)
  }
  
  # 添加轴标签和主题
  plot <- plot +
    labs(x = p$bot_xlab, y = p$bot_ylab) +
    theme_bw(base_size = p$base_size) +
    theme(
      # 顶部 margin 最小化，与上 panel 紧贴
      plot.margin = margin(t = 0, r = 8, b = 8, l = 8, unit = "pt"),
      # 轴文字
      axis.title.x = element_text(size = p$base_size, margin = margin(t = 4)),
      axis.title.y = element_text(size = p$base_size, margin = margin(r = 4)),
      axis.text = element_text(size = p$base_size - 2, color = "black"),
      # 统一用 axis.line 画边框（顶、左、右、底），与上 panel 视觉一致
      panel.border = element_blank(),
      axis.line.x.top = element_line(colour = "black", linewidth = p$border_linewidth %||% 0.5),
      axis.line.x.bottom = element_line(colour = "black", linewidth = p$border_linewidth %||% 0.5),
      axis.line.y.left = element_line(colour = "black", linewidth = p$border_linewidth %||% 0.5),
      axis.line.y.right = element_line(colour = "black", linewidth = p$border_linewidth %||% 0.5),
      axis.ticks = element_line(color = "black", linewidth = p$border_linewidth %||% 0.5),
      axis.ticks.length = unit(3, "pt"),
      # 顶部 sec.axis：负 length 使 ticks 朝下指入下 panel 内部
      axis.ticks.length.x.top = unit(-3, "pt"),
      # 顶部 sec.axis 只显示 ticks，隐藏标签
      axis.text.x.top = element_blank(),
      axis.title.x.top = element_blank(),
      # 右侧 sec.axis 只画线，隐藏标签和 ticks
      axis.text.y.right = element_blank(),
      axis.ticks.y.right = element_blank(),
      # 无网格
      panel.grid = element_blank()
    )
  
  # 自定义 X 轴范围，添加 dup_axis 使顶部也有 ticks
  dup_x <- dup_axis(name = NULL, labels = function(x) rep("", length(x)))
  dup_y <- dup_axis(name = NULL, labels = function(x) rep("", length(x)))
  if (!is.null(p$bot_xmin) && !is.null(p$bot_xmax)) {
    plot <- plot + scale_x_continuous(limits = c(p$bot_xmin, p$bot_xmax), sec.axis = dup_x, expand = expansion(mult = 0.02))
  } else {
    plot <- plot + scale_x_continuous(sec.axis = dup_x, expand = expansion(mult = 0.05))
  }
  
  # 自定义 Y 轴范围，添加 dup_axis 以画出右边框
  if (!is.null(p$bot_ymin) && !is.null(p$bot_ymax)) {
    plot <- plot + scale_y_continuous(limits = c(p$bot_ymin, p$bot_ymax), sec.axis = dup_y, expand = expansion(mult = 0.05))
  } else {
    plot <- plot + scale_y_continuous(sec.axis = dup_y, expand = expansion(mult = 0.05))
  }
  
  return(plot)
}
