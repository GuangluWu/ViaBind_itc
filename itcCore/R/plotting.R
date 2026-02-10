create_itc_figure <- function(power_data = NULL,
                              integration_data = NULL,
                              simulation_data = NULL,
                              params = list()) {
  p <- modifyList(PLOT_DEFAULTS, params)
  p_top <- create_thermogram_panel(power_data, p)
  p_bot <- create_isotherm_panel(integration_data, simulation_data, p)
  p_top / p_bot + patchwork::plot_layout(heights = c(p$height_ratio, 1))
}

create_thermogram_panel <- function(power_data, p) {
  if (is.null(power_data) || nrow(power_data) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No thermogram data", size = 5, color = "grey50") +
        ggplot2::theme_void()
    )
  }

  time_unit <- p$top_time_unit %||% "min"
  y_power <- power_data$Power_corrected_ucal_s
  if (identical(p$energy_unit, "J")) y_power <- y_power * 4.184

  plot_data <- if (time_unit == "min") {
    data.frame(x = power_data$Time_s / 60, y = y_power)
  } else {
    data.frame(x = power_data$Time_s, y = y_power)
  }

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(color = p$top_line_color, linewidth = p$top_line_width) +
    ggplot2::labs(y = p$top_ylab) +
    ggplot2::scale_x_continuous(position = "top", name = p$top_xlab, expand = ggplot2::expansion(mult = 0.02)) +
    ggplot2::theme_bw(base_size = p$base_size) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 5, r = 8, b = 0, l = 8, unit = "pt"),
      axis.title.x.top = ggplot2::element_text(size = p$base_size, margin = ggplot2::margin(b = 4)),
      axis.title.y = ggplot2::element_text(size = p$base_size, margin = ggplot2::margin(r = 4)),
      axis.text = ggplot2::element_text(size = p$base_size - 2, color = "black"),
      panel.border = ggplot2::element_blank(),
      axis.line.x.top = ggplot2::element_line(colour = "black", linewidth = p$border_linewidth %||% 0.5),
      axis.line.y.left = ggplot2::element_line(colour = "black", linewidth = p$border_linewidth %||% 0.5),
      axis.line.y.right = ggplot2::element_line(colour = "black", linewidth = p$border_linewidth %||% 0.5),
      axis.text.y.right = ggplot2::element_blank(),
      axis.ticks.y.right = ggplot2::element_blank(),
      axis.line.x.bottom = ggplot2::element_blank(),
      axis.ticks.x.bottom = ggplot2::element_blank(),
      axis.text.x.bottom = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(color = "black", linewidth = p$border_linewidth %||% 0.5),
      axis.ticks.length = grid::unit(3, "pt"),
      panel.grid = ggplot2::element_blank()
    )

  if (!is.null(p$top_xmin) && !is.null(p$top_xmax)) {
    plot <- plot + ggplot2::scale_x_continuous(
      position = "top",
      name = p$top_xlab,
      limits = c(p$top_xmin, p$top_xmax),
      expand = ggplot2::expansion(mult = 0.02)
    )
  }

  dup_y <- ggplot2::dup_axis(name = NULL, labels = function(x) rep("", length(x)))
  if (!is.null(p$top_ymin) && !is.null(p$top_ymax)) {
    plot <- plot + ggplot2::scale_y_continuous(limits = c(p$top_ymin, p$top_ymax), sec.axis = dup_y, expand = ggplot2::expansion(mult = 0.05))
  } else {
    plot <- plot + ggplot2::scale_y_continuous(sec.axis = dup_y, expand = ggplot2::expansion(mult = 0.05))
  }

  plot
}

create_isotherm_panel <- function(integration_data, simulation_data, p) {
  has_int <- !is.null(integration_data) && nrow(integration_data) > 0
  has_sim <- !is.null(simulation_data) && nrow(simulation_data) > 0

  if (!has_int && !has_sim) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No isotherm data", size = 5, color = "grey50") +
        ggplot2::theme_void()
    )
  }

  offset <- as.numeric(p$heat_offset %||% 0)
  to_kcal <- function(v) (v - offset) / 1000
  to_display <- if (identical(p$energy_unit, "J")) {
    function(v) to_kcal(v) * 4.184
  } else {
    to_kcal
  }

  if (has_int) {
    int_plot <- data.frame(x = integration_data$Ratio_App, y = to_display(integration_data$heat_cal_mol))
    int_plot <- int_plot[is.finite(int_plot$x) & is.finite(int_plot$y), , drop = FALSE]
    has_int <- nrow(int_plot) > 0
  }

  if (has_sim) {
    sim_plot <- data.frame(x = simulation_data$Ratio_App, y = to_display(simulation_data$dQ_App))
    sim_plot <- sim_plot[is.finite(sim_plot$x) & is.finite(sim_plot$y), , drop = FALSE]
    has_sim <- nrow(sim_plot) > 0
  }

  plot <- ggplot2::ggplot()

  fill_alpha <- as.numeric(p$bot_point_fill_alpha %||% 0.7)
  fill_alpha <- max(min(fill_alpha, 1), 0)
  point_fill <- grDevices::adjustcolor(p$bot_point_fill, alpha.f = fill_alpha)
  dim_first <- isTRUE(p$bot_dim_first_point)
  dim_color <- "#C8C8C8"
  dim_point_fill <- grDevices::adjustcolor(dim_color, alpha.f = fill_alpha)

  add_line <- function(g) {
    if (!has_sim) return(g)
    g + ggplot2::geom_line(
      data = sim_plot,
      ggplot2::aes(x = x, y = y),
      color = p$bot_line_color,
      linewidth = p$bot_line_width,
      linetype = p$bot_line_linetype %||% "solid"
    )
  }

  add_points <- function(g) {
    if (!has_int) return(g)
    if (dim_first && nrow(int_plot) > 0) {
      first_point <- int_plot[1, , drop = FALSE]
      rest_points <- if (nrow(int_plot) > 1) int_plot[-1, , drop = FALSE] else NULL
      g <- g + ggplot2::geom_point(
        data = first_point,
        ggplot2::aes(x = x, y = y),
        shape = p$bot_point_shape,
        size = p$bot_point_size,
        color = dim_color,
        fill = dim_point_fill,
        stroke = 0.6
      )
      if (!is.null(rest_points) && nrow(rest_points) > 0) {
        g <- g + ggplot2::geom_point(
          data = rest_points,
          ggplot2::aes(x = x, y = y),
          shape = p$bot_point_shape,
          size = p$bot_point_size,
          color = p$bot_point_color,
          fill = point_fill,
          stroke = 0.6
        )
      }
      return(g)
    }

    g + ggplot2::geom_point(
      data = int_plot,
      ggplot2::aes(x = x, y = y),
      shape = p$bot_point_shape,
      size = p$bot_point_size,
      color = p$bot_point_color,
      fill = point_fill,
      stroke = 0.6
    )
  }

  if (identical(p$bot_layer_order %||% "points_over_line", "line_over_points")) {
    plot <- add_points(plot)
    plot <- add_line(plot)
  } else {
    plot <- add_line(plot)
    plot <- add_points(plot)
  }

  plot <- plot +
    ggplot2::labs(x = p$bot_xlab, y = p$bot_ylab) +
    ggplot2::theme_bw(base_size = p$base_size) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 0, r = 8, b = 8, l = 8, unit = "pt"),
      axis.title.x = ggplot2::element_text(size = p$base_size, margin = ggplot2::margin(t = 4)),
      axis.title.y = ggplot2::element_text(size = p$base_size, margin = ggplot2::margin(r = 4)),
      axis.text = ggplot2::element_text(size = p$base_size - 2, color = "black"),
      panel.border = ggplot2::element_blank(),
      axis.line.x.top = ggplot2::element_line(colour = "black", linewidth = p$border_linewidth %||% 0.5),
      axis.line.x.bottom = ggplot2::element_line(colour = "black", linewidth = p$border_linewidth %||% 0.5),
      axis.line.y.left = ggplot2::element_line(colour = "black", linewidth = p$border_linewidth %||% 0.5),
      axis.line.y.right = ggplot2::element_line(colour = "black", linewidth = p$border_linewidth %||% 0.5),
      axis.ticks = ggplot2::element_line(color = "black", linewidth = p$border_linewidth %||% 0.5),
      axis.ticks.length = grid::unit(3, "pt"),
      axis.ticks.length.x.top = grid::unit(-3, "pt"),
      panel.grid = ggplot2::element_blank()
    )

  if (!is.null(p$bot_xmin) && !is.null(p$bot_xmax)) {
    plot <- plot + ggplot2::scale_x_continuous(limits = c(p$bot_xmin, p$bot_xmax), expand = ggplot2::expansion(mult = 0.05))
  } else {
    plot <- plot + ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.05))
  }

  if (!is.null(p$bot_ymin) && !is.null(p$bot_ymax)) {
    plot <- plot + ggplot2::scale_y_continuous(limits = c(p$bot_ymin, p$bot_ymax), expand = ggplot2::expansion(mult = 0.05))
  } else {
    plot <- plot + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.05))
  }

  plot
}
