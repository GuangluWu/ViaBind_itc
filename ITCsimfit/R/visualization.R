# ==============================================================================
# R/visualization.R - 可视化函数模块
# ==============================================================================
# 包含残差图和参数相关性矩阵的可视化函数

#' 绘制残差诊断图
#' 
#' @description
#' 生成包含4个子图的残差诊断图：
#' 1. 残差 vs 拟合值：检查异方差性
#' 2. 残差 vs 针数：检查系统性偏差
#' 3. 残差直方图：检查残差分布
#' 4. Q-Q 图：检查残差正态性
#' 
#' @param residuals_data data.frame 包含残差数据，必须包含以下列：
#'   - Inj: 针数索引
#'   - Fitted: 拟合值
#'   - Observed: 观测值
#'   - Residual: 残差
#'   - Ratio_App: 对应的 Ratio（可选，用于 x 轴）
#' @return ggplot 对象（组合图）或单个图
plot_residuals <- function(residuals_data) {
  if (is.null(residuals_data) || nrow(residuals_data) == 0) {
    return(ggplot() + 
           annotate("text", x = 0.5, y = 0.5, label = "暂无残差数据", size = 5) +
           theme_void())
  }
  
  # 确保必要的列存在
  required_cols <- c("Inj", "Fitted", "Observed", "Residual")
  if (!all(required_cols %in% colnames(residuals_data))) {
    return(ggplot() + 
           annotate("text", x = 0.5, y = 0.5, label = "残差数据格式不正确", size = 5) +
           theme_void())
  }
  
  # 子图1：残差 vs 拟合值
  p1 <- ggplot(residuals_data, aes(x = Fitted, y = Residual)) +
    geom_point(alpha = 0.6, color = "#3498db", size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5) +
    geom_smooth(method = "loess", formula = y ~ x, se = TRUE, color = "#e74c3c", linewidth = 0.8, alpha = 0.3) +
    labs(x = "拟合值 (kcal/mol)", y = "残差 (kcal/mol)", title = "残差 vs 拟合值") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
  
  # 子图2：残差 vs 针数
  p2 <- ggplot(residuals_data, aes(x = Inj, y = Residual)) +
    geom_point(alpha = 0.6, color = "#3498db", size = 1.5) +
    geom_line(alpha = 0.3, color = "#3498db", linewidth = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5) +
    geom_smooth(method = "loess", formula = y ~ x, se = TRUE, color = "#e74c3c", linewidth = 0.8, alpha = 0.3) +
    labs(x = "针数", y = "残差 (kcal/mol)", title = "残差 vs 针数") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
  
  # 子图3：残差直方图
  p3 <- ggplot(residuals_data, aes(x = Residual)) +
    geom_histogram(bins = 20, fill = "#3498db", alpha = 0.7, color = "white") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5) +
    labs(x = "残差 (kcal/mol)", y = "频数", title = "残差分布直方图") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
  
  # 子图4：Q-Q 图
  residuals_clean <- residuals_data$Residual[is.finite(residuals_data$Residual)]
  if (length(residuals_clean) > 0) {
    qq_data <- data.frame(
      Theoretical = qqnorm(residuals_clean, plot.it = FALSE)$x,
      Sample = qqnorm(residuals_clean, plot.it = FALSE)$y
    )
    
    p4 <- ggplot(qq_data, aes(x = Theoretical, y = Sample)) +
      geom_point(alpha = 0.6, color = "#3498db", size = 1.5) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 0.5) +
      labs(x = "理论分位数", y = "样本分位数", title = "Q-Q 图（正态性检验）") +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
  } else {
    p4 <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "无有效残差数据", size = 4) +
      theme_void()
  }
  
  # 组合图形（2x2 布局）
  if (requireNamespace("gridExtra", quietly = TRUE)) {
    combined_plot <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
    return(combined_plot)
  } else {
    # 如果没有 gridExtra，返回第一个图
    return(p1)
  }
}

#' 绘制参数相关性矩阵热图
#' 
#' @description
#' 使用 ggplot2 绘制参数相关性矩阵热图
#' 
#' @param cor_matrix matrix 或 data.frame 相关性矩阵
#' @param param_names character vector 参数名称（可选，如果 cor_matrix 有行名和列名则自动使用）
#' @param lang character 语言代码 ("zh" 或 "en")，默认为 "zh"
#' @return ggplot 对象
plot_correlation_matrix <- function(cor_matrix, param_names = NULL, lang = "zh") {
  if (is.null(cor_matrix)) {
    return(ggplot() + 
           annotate("text", x = 0.5, y = 0.5, label = "暂无相关性矩阵数据", size = 5) +
           theme_void())
  }
  
  # 转换为矩阵（如果还不是）
  if (!is.matrix(cor_matrix)) {
    cor_matrix <- as.matrix(cor_matrix)
  }
  
  # 获取参数名称
  if (is.null(param_names)) {
    if (!is.null(rownames(cor_matrix)) && !is.null(colnames(cor_matrix))) {
      param_names <- rownames(cor_matrix)
    } else {
      param_names <- paste0("Param", 1:nrow(cor_matrix))
    }
  }
  
  # 保留所有参数（不筛选，显示所有拟合参数）
  if (length(param_names) == 0) {
    return(ggplot() + 
           annotate("text", x = 0.5, y = 0.5, label = "未找到有效的拟合参数", size = 5) +
           theme_void())
  }
  
  # 如果矩阵维度与参数名不匹配，使用矩阵的行列名
  if (nrow(cor_matrix) != length(param_names) || ncol(cor_matrix) != length(param_names)) {
    if (!is.null(rownames(cor_matrix)) && !is.null(colnames(cor_matrix))) {
      param_names <- rownames(cor_matrix)
    }
  }
  
  # 转换为长格式数据框
  cor_df <- expand.grid(
    Var1 = param_names,
    Var2 = param_names,
    stringsAsFactors = FALSE
  )
  cor_df$value <- as.vector(cor_matrix)
  
  # 格式化参数名称（用于显示）
  format_param_name <- function(name) {
    if (grepl("logK", name)) {
      # logK1 -> log K₁, logK2 -> log K₂
      num <- gsub("logK", "", name)
      if (num == "") {
        return("log K")
      } else {
        return(paste0("log K", num))
      }
    } else if (grepl("^H[0-9]$", name)) {
      # H1 -> ΔH₁, H2 -> ΔH₂
      num <- gsub("H", "", name)
      if (num == "") {
        return("ΔH")
      } else {
        return(paste0("ΔH", num))
      }
    } else if (name == "fH") {
      return("fH")
    } else if (name == "fG") {
      return("fG")
    } else if (name == "V_init") {
      return("V_init")
    } else if (name == "Offset") {
      return("Offset")
    } else {
      return(name)
    }
  }
  
  cor_df$Var1_formatted <- sapply(cor_df$Var1, format_param_name)
  cor_df$Var2_formatted <- sapply(cor_df$Var2, format_param_name)
  
  # 确保 Var1 和 Var2 的顺序一致（用于对称矩阵）
  cor_df$Var1_formatted <- factor(cor_df$Var1_formatted, levels = unique(cor_df$Var1_formatted))
  cor_df$Var2_formatted <- factor(cor_df$Var2_formatted, levels = unique(cor_df$Var2_formatted))
  
  # 获取翻译文本
  legend_label <- tr("correlation_legend_label", lang)
  plot_title <- tr("correlation_plot_title", lang)
  
  # 绘制热图
  p <- ggplot(cor_df, aes(x = Var1_formatted, y = Var2_formatted, fill = value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_gradient2(
      low = "#3498db",      # 深蓝（负相关）
      mid = "white",        # 白色（无相关）
      high = "#e74c3c",     # 深红（正相关）
      midpoint = 0,
      limits = c(-1, 1),
      name = legend_label
    ) +
    geom_text(aes(label = sprintf("%.2f", value)), 
              color = "black", size = 3, fontface = "bold") +
    labs(x = "", y = "", title = plot_title) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(hjust = 1),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      panel.grid = element_blank(),
      legend.position = "right"
    )
  
  return(p)
}
