  
  # [新增] 显示误差分析可靠性评价
  output$error_analysis_reliability <- renderUI({
    info <- values$error_analysis_info
    if (is.null(info)) {
      return(NULL)
    }
    
    rel_key <- if (info$dof >= 10) "error_analysis_applicability_1" else if (info$dof >= 5) "error_analysis_applicability_2" else "error_analysis_applicability_3"
    div(style = "background-color: #f8f9fa; padding: 10px; border-left: 4px solid #3498db; margin-bottom: 8px; border-radius: 4px;",
        div(style = "display: flex; align-items: center; margin-bottom: 5px;",
            strong(tr("error_analysis_reliability", lang()), style = "margin-right: 10px;"),
            span(style = paste0("color: ", info$reliability_color, "; font-weight: bold; font-size: 1.1em;"),
                 tr(rel_key, lang()))
        ),
        div(style = "font-size: 0.85em; color: #555;",
            p(style = "margin: 2px 0;", 
              paste0(tr("error_analysis_data_points", lang()), info$n_data, " | ",
                     tr("error_analysis_fit_params", lang()), info$n_params, " | ",
                     tr("error_analysis_dof", lang()), info$dof)),
            p(style = "margin: 2px 0; font-style: italic;",
              if (info$dof >= 10) {
                tr("error_analysis_reliability_good", lang())
              } else if (info$dof >= 5) {
                tr("error_analysis_reliability_moderate", lang())
              } else {
                tr("error_analysis_reliability_low", lang())
              })
        )
    )
  })
  
  output$itcPlot <- renderPlot({
    # 初始化空图
    p <- ggplot() + theme_minimal(base_size=14) + theme(legend.position="bottom")
    tr_with_fallback <- function(key, zh_default, en_default) {
      val <- tryCatch(tr(key, lang()), error = function(e) NA_character_)
      if (is.null(val) || !nzchar(as.character(val)[1]) || identical(as.character(val)[1], key)) {
        return(if (identical(lang(), "zh")) zh_default else en_default)
      }
      as.character(val)[1]
    }
    sim_ratio_label <- tr_with_fallback("axis_simulated_ratio", "模拟 G/H 比", "Simulated G/H Ratio")
    exp_ratio_label <- tr_with_fallback("axis_experimental_ratio", "实验 G/H 比", "Experimental G/H Ratio")
    heat_per_inj_label <- tr_with_fallback("axis_heat_per_inj", "每针热量 (kcal/mol)", "Heat per Inj (kcal/mol)")
    legend_simulation <- tr_with_fallback("legend_simulation", "模拟", "Simulation")
    legend_experiment <- tr_with_fallback("legend_experiment", "实验", "Experiment")
    legend_excluded <- tr_with_fallback("legend_excluded", "排除点", "Excluded")
    legend_solver_fail <- tr_with_fallback("legend_solver_fail", "求解失败", "Solver Fail")
    
    sim <- sim_results()
    exp_df <- tryCatch(exp_data_processed(), error=function(e) NULL)
    
    # 准备 X 轴标签函数 (Bottom: Sim Ratio)
    # 使用函数闭包来处理可能的 NULL sim
    fmt_ratio <- function(x) formatC(x, format="f", digits=2)
    
    # 获取最大针数，用于设置 X 轴范围
    max_inj <- 0
    if(!is.null(sim)) max_inj <- max(max_inj, nrow(sim))
    if(!is.null(exp_df)) max_inj <- max(max_inj, nrow(exp_df))
    if(max_inj == 0) {
      # [修复] 优先使用用户输入的 n_inj 作为回退值，确保与设定一致
      max_inj <- if(!is.null(input$n_inj)) input$n_inj else UI_DEFAULTS$n_inj_default
    }
    
    # 构建 Bottom Axis (Sim Ratio) 的 Breaks 和 Labels
    # 我们每隔 5 针显示一个标签，避免拥挤
    breaks_seq <- seq(1, max_inj, by=5)
    
    labels_bottom <- function(breaks) {
      if(is.null(sim)) return(breaks)
      # 找到 breaks 对应的 Ratio_App
      ratios <- sim$Ratio_App[match(breaks, sim$Inj)]
      # 处理 NA (超出范围)
      ratios[is.na(ratios)] <- 0
      fmt_ratio(ratios)
    }
    
    # 构建 Top Axis (Exp Ratio) 的 Labels
    labels_top <- function(breaks) {
      if(is.null(exp_df)) return(rep("", length(breaks)))
      ratios <- exp_df$Ratio_Raw[match(breaks, exp_df$Inj)]
      ratios[is.na(ratios)] <- 0
      fmt_ratio(ratios)
    }
    
    # 设置 X 轴 (双轴系统)
    # 主轴: Injection Index -> 显示 Sim Ratio
    # 次轴: Injection Index -> 显示 Exp Ratio
    p <- p + scale_x_continuous(
      name = sim_ratio_label,
      breaks = breaks_seq,
      labels = labels_bottom,
      sec.axis = sec_axis(~., name = exp_ratio_label, breaks = breaks_seq, labels = labels_top)
    ) + labs(y = heat_per_inj_label)
    
    # 纵坐标统一为 kcal/mol：内部数据为 cal/mol，绘图时除以 1000（与 ITCprocessor 积分图一致）
    if(!is.null(sim) && nrow(sim) > 0) {
      sim_plot <- sim %>% mutate(dQ_kcal = dQ_App / 1000)
      p <- p + geom_line(data=sim_plot, aes(x=Inj, y=dQ_kcal, color=legend_simulation), linewidth=1)
      sim_ok <- sim_plot[sim_plot$Fallback == 0, ]
      if(nrow(sim_ok) > 0) {
        p <- p + geom_point(data=sim_ok, aes(x=Inj, y=dQ_kcal, color=legend_simulation), size=2)
      }
      sim_fb <- sim_plot[sim_plot$Fallback == 1, ]
      if(nrow(sim_fb) > 0) {
        p <- p + geom_point(data=sim_fb, aes(x=Inj, y=dQ_kcal, color=legend_solver_fail), size=3, shape=1, stroke=1.5)
      }
    }
    
    if(!is.null(exp_df)) {
      try({
        range_lim <- input$fit_data_range
        ed <- exp_df %>% mutate(Sel = Inj >= range_lim[1] & Inj <= range_lim[2], Heat_kcal = Heat_Raw / 1000)
        p <- p + geom_point(data=ed[!ed$Sel,], aes(x=Inj, y=Heat_kcal, color=legend_excluded), alpha=1, size=3, shape=1)
        p <- p + geom_point(data=ed[ed$Sel,], aes(x=Inj, y=Heat_kcal, color=legend_experiment), size=3, shape=18)
      })
    }
    
    p + scale_color_manual(
      name = NULL,
      values = setNames(c("#e74c3c", "#2980b9", "grey", "#8e44ad"),
                        c(legend_simulation, legend_experiment, legend_excluded, legend_solver_fail))
    )
  })
  
  output$distPlot <- renderPlot({
    sim <- sim_results()
    if(is.null(sim)) return(NULL)
    legend_species <- tryCatch(tr("legend_species", lang()), error = function(e) "")
    if (is.null(legend_species) || !nzchar(as.character(legend_species)[1]) || identical(as.character(legend_species)[1], "legend_species")) {
      legend_species <- if (identical(lang(), "zh")) "物种" else "Species"
    } else {
      legend_species <- as.character(legend_species)[1]
    }
    
    cols_to_plot <- c("H_pct", "M_pct")
    if("rxn_D" %in% input$active_paths) cols_to_plot <- c(cols_to_plot, "D_pct")
    if("rxn_T" %in% input$active_paths) cols_to_plot <- c(cols_to_plot, "T_pct")
    if("rxn_B" %in% input$active_paths) cols_to_plot <- c(cols_to_plot, "B_pct")
    if("rxn_F" %in% input$active_paths && "rxn_D" %in% input$active_paths) cols_to_plot <- c(cols_to_plot, "F_pct")
    if("rxn_U" %in% input$active_paths) cols_to_plot <- c(cols_to_plot, "U_pct")
    
    # [修改] 定义图例标签映射
    lbl_map <- c(
      "H_pct" = "H",
      "M_pct" = "H1G1",
      "D_pct" = "H1G2",
      "T_pct" = "H2G2",
      "B_pct" = "H2G1",
      "F_pct" = "H2G3",
      "U_pct" = "H1G1(U)"
    )

    sim %>% select(Ratio_App, all_of(cols_to_plot)) %>%
      pivot_longer(-Ratio_App, names_to="Species", values_to="Frac") %>%
      # 将 Species 转换为因子并重命名标签，确保顺序和显示正确
      mutate(Species = factor(Species, levels = names(lbl_map), labels = lbl_map)) %>%
      ggplot(aes(x=Ratio_App, y=Frac, fill=Species)) + 
      geom_area(alpha=0.8, color="white", linewidth=0.1) +
      scale_fill_brewer(palette="Set3", name = legend_species) + 
      labs(x=tr("axis_simulated_ratio", lang()), y=tr("axis_fraction_based_on_h", lang())) +
      theme_minimal(base_size=14) + theme(legend.position="bottom")
  })
  
  # [新增] 残差子标签页按钮事件处理
  observeEvent(input$res_subtab_1, { 
    if(lang_switching()) return()  # [修复] 防止在语言切换时触发
    values$residual_subtab <- "res1" 
  }, ignoreInit = TRUE)
  observeEvent(input$res_subtab_2, { 
    if(lang_switching()) return()
    values$residual_subtab <- "res2" 
  }, ignoreInit = TRUE)
  observeEvent(input$res_subtab_3, { 
    if(lang_switching()) return()
    values$residual_subtab <- "res3" 
  }, ignoreInit = TRUE)
  observeEvent(input$res_subtab_4, { 
    if(lang_switching()) return()
    values$residual_subtab <- "res4" 
  }, ignoreInit = TRUE)
  
  # [新增] 残差诊断按钮UI（动态高亮当前选中的按钮）
  output$residual_buttons <- renderUI({
    if (is.null(values$error_analysis)) return(NULL)
    current_subtab <- if(!is.null(values$residual_subtab)) values$residual_subtab else "res1"
    div(style = "margin-bottom: 10px; display: flex; gap: 5px; flex-wrap: wrap; align-items: center;",
        actionButton("res_subtab_1", tr("residual_tab_1", lang()), 
                   class = if(current_subtab == "res1") "btn btn-info btn-xs" else "btn btn-default btn-xs", 
                   style = "padding: 4px 8px; font-size: 11px; min-width: 100px;"),
        actionButton("res_subtab_2", tr("residual_tab_2", lang()), 
                   class = if(current_subtab == "res2") "btn btn-info btn-xs" else "btn btn-default btn-xs", 
                   style = "padding: 4px 8px; font-size: 11px; min-width: 100px;"),
        actionButton("res_subtab_3", tr("residual_tab_3", lang()), 
                   class = if(current_subtab == "res3") "btn btn-info btn-xs" else "btn btn-default btn-xs", 
                   style = "padding: 4px 8px; font-size: 11px; min-width: 100px;"),
        actionButton("res_subtab_4", tr("residual_tab_4", lang()), 
                   class = if(current_subtab == "res4") "btn btn-info btn-xs" else "btn btn-default btn-xs", 
                   style = "padding: 4px 8px; font-size: 11px; min-width: 100px;")
    )
  })
  
  # [新增] 残差诊断图输出 - 4个子图，根据子标签页选择显示
  output$residualPlot <- renderPlot({
    if (is.null(values$error_analysis) || is.null(values$residuals_data)) {
      return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, 
                     label = tr("residual_plot_required", lang()), size = 5) +
             theme_void())
    }
    
    residuals_data <- values$residuals_data
    selected_subtab <- values$residual_subtab
    
    tryCatch({
      # 根据选中的子标签页显示不同的图
      if (selected_subtab == "res1") {
        # 残差 vs 拟合值
        p <- ggplot(residuals_data, aes(x = Fitted, y = Residual)) +
          geom_point(alpha = 0.6, color = "#3498db", size = 1.5) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.6) +
          geom_smooth(method = "loess", formula = y ~ x, se = TRUE, color = "#e74c3c", linewidth = 0.8, alpha = 0.3) +
          labs(x = tr("residual_plot_x_1", lang()), y = tr("residual_plot_y_1", lang()), title = tr("residual_plot_title_1", lang())) +
          theme_minimal(base_size = 12) +
          theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                plot.margin = margin(5, 5, 5, 5, "pt"))
        print(p)
      } else if (selected_subtab == "res2") {
        # 残差 vs 针数
        p <- ggplot(residuals_data, aes(x = Inj, y = Residual)) +
          geom_point(alpha = 0.6, color = "#3498db", size = 1.5) +
          geom_line(alpha = 0.3, color = "#3498db", linewidth = 0.4) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.6) +
          geom_smooth(method = "loess", formula = y ~ x, se = TRUE, color = "#e74c3c", linewidth = 0.8, alpha = 0.3) +
          labs(x = tr("residual_plot_x_2", lang()), y = tr("residual_plot_y_2", lang()), title = tr("residual_plot_title_2", lang())) +
          theme_minimal(base_size = 12) +
          theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                plot.margin = margin(5, 5, 5, 5, "pt"))
        print(p)
      } else if (selected_subtab == "res3") {
        # 残差直方图
        p <- ggplot(residuals_data, aes(x = Residual)) +
          geom_histogram(bins = 20, fill = "#3498db", alpha = 0.7, color = "white") +
          geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.6) +
          labs(x = tr("residual_plot_x_3", lang()), y = tr("residual_plot_y_3", lang()), title = tr("residual_plot_title_3", lang())) +
          theme_minimal(base_size = 12) +
          theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                plot.margin = margin(5, 5, 5, 5, "pt"))
        print(p)
      } else if (selected_subtab == "res4") {
        # Q-Q 图
        residuals_clean <- residuals_data$Residual[is.finite(residuals_data$Residual)]
        if (length(residuals_clean) > 0) {
          qq_data <- data.frame(
            Theoretical = qqnorm(residuals_clean, plot.it = FALSE)$x,
            Sample = qqnorm(residuals_clean, plot.it = FALSE)$y
          )
          p <- ggplot(qq_data, aes(x = Theoretical, y = Sample)) +
            geom_point(alpha = 0.6, color = "#3498db", size = 1.5) +
            geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 0.6) +
            labs(x = tr("residual_plot_x_4", lang()), y = tr("residual_plot_y_4", lang()), title = tr("residual_plot_title_4", lang())) +
            theme_minimal(base_size = 12) +
            theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                  plot.margin = margin(5, 5, 5, 5, "pt"))
          print(p)
        } else {
          p <- ggplot() + annotate("text", x = 0.5, y = 0.5, label = tr("residual_no_data", lang()), size = 4) + theme_void()
          print(p)
        }
      } else {
        # 默认显示第一个图
        p <- ggplot(residuals_data, aes(x = Fitted, y = Residual)) +
          geom_point(alpha = 0.6, color = "#3498db", size = 1.5) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.6) +
          geom_smooth(method = "loess", formula = y ~ x, se = TRUE, color = "#e74c3c", linewidth = 0.8, alpha = 0.3) +
          labs(x = tr("residual_plot_x_1", lang()), y = tr("residual_plot_y_1", lang()), title = tr("residual_plot_title_1", lang())) +
          theme_minimal(base_size = 12) +
          theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                plot.margin = margin(5, 5, 5, 5, "pt"))
        print(p)
      }
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                label = paste0(tr("plot_error", lang()), e$message), size = 4) +
        theme_void()
    })
  })
  
  # [新增] 参数相关性矩阵图输出
  output$correlationPlot <- renderPlot({
    # 仅在误差分析完成时显示
    if (is.null(values$error_analysis) || is.null(values$correlation_matrix)) {
      return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, 
                     label = tr("residual_plot_required", lang()), size = 5) +
             theme_void())
    }
    
    tryCatch({
      plot_correlation_matrix(values$correlation_matrix, lang = lang())
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                label = paste0(tr("plot_error", lang()), e$message), size = 4) +
        theme_void()
    })
  })
  
  # [新增] 动态生成绘图标签页 - 所有标签页在一个tabsetPanel中，通过CSS分成两行显示
  output$plot_tabs <- renderUI({
    # 基础标签页（始终显示）
    tabs <- list(
      tabPanel(tr("plot_tab_itc", lang()), value = "tab_itc", br(), plotOutput("itcPlot", height = "460px")),
      tabPanel(tr("plot_tab_dist", lang()), value = "tab_dist", br(), plotOutput("distPlot", height = "460px"))
    )
    
    # 如果误差分析已完成，添加残差诊断和参数相关性标签页
    if (!is.null(values$error_analysis)) {
      # 残差诊断标签页：使用按钮切换不同的子图
      tabs <- c(tabs, list(
        tabPanel(tr("plot_tab_residuals", lang()), value = "tab_residuals", 
                 uiOutput("residual_buttons"),
                 # 共享的绘图区域（增加高度以确保有足够空间，减少边距）
                 plotOutput("residualPlot", height = "420px")
        ),
        tabPanel(tr("plot_tab_corr", lang()), value = "tab_corr", br(), plotOutput("correlationPlot", height = "460px"))
      ))
    }
    
    # 创建tabsetPanel并添加自定义CSS类
    tabset <- do.call(tabsetPanel, c(list(type = "tabs", id = "main_plot_tabs"), tabs))
    
    # 包装在div中，添加自定义CSS类用于控制标签页换行
    div(class = "two-row-tabs", tabset)
  })
  
