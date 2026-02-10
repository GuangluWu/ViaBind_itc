# ==============================================================================
# 性能监控应用示例
# ==============================================================================
# 本文件展示如何在实际代码中应用性能监控
# 
# 使用说明：
# 1. 这些示例可以直接复制到 server.R 或其他文件中
# 2. 根据实际需求调整标签和类别
# 3. 配置合适的性能阈值

# ==============================================================================
# 示例 1: 在拟合函数中应用性能监控
# ==============================================================================

# 改进前的代码
perform_fitting_sync_old <- function(data, initial_params, model_type, ...) {
  # 验证输入
  if (is.null(data) || nrow(data) == 0) {
    stop("无效的数据")
  }
  
  # 构建目标函数
  objective_fn <- function(par) {
    predicted <- model_predict(par, data)
    sum((data$dQ_obs - predicted)^2)
  }
  
  # 全局优化
  lower <- c(-5, -15000, 0.1, -100)
  upper <- c(12, 5000, 10, 100)
  
  de_result <- DEoptim(
    objective_fn,
    lower = lower,
    upper = upper,
    control = DEoptim.control(itermax = 100, NP = 50)
  )
  
  # 局部精化
  optim_result <- optim(
    par = de_result$optim$bestmem,
    fn = objective_fn,
    method = "L-BFGS-B",
    lower = lower,
    upper = upper
  )
  
  return(list(
    params = optim_result$par,
    value = optim_result$value,
    convergence = optim_result$convergence
  ))
}

# 改进后的代码（添加性能监控）
perform_fitting_sync_new <- function(data, initial_params, model_type, ...) {
  
  # 使用批量性能监控监控整个拟合流程
  fitting_result <- perf_monitor_batch(
    steps = list(
      # 步骤 1: 数据验证
      list(
        expr = quote({
          if (is.null(data) || nrow(data) == 0) {
            stop("无效的数据")
          }
          # 返回验证后的数据
          data
        }),
        label = "数据验证"
      ),
      
      # 步骤 2: 准备优化
      list(
        expr = quote({
          # 构建目标函数
          objective_fn <- function(par) {
            predicted <- model_predict(par, data)
            sum((data$dQ_obs - predicted)^2)
          }
          
          # 设置边界（使用 constants.R 中的定义）
          bounds <- get_parameter_bounds(
            c("logK", "H", "fH", "Offset"),
            v_inj = max(data$V_inj)
          )
          
          list(
            objective_fn = objective_fn,
            lower = bounds$lower,
            upper = bounds$upper
          )
        }),
        label = "准备优化"
      ),
      
      # 步骤 3: 全局优化（差分进化）
      list(
        expr = quote({
          de_control <- DEoptim.control(
            itermax = 100,
            NP = max(DE_OPTIM$pop_size_multiplier * length(prep$lower), 
                    DE_OPTIM$min_pop_size)
          )
          
          de_result <- DEoptim(
            prep$objective_fn,
            lower = prep$lower,
            upper = prep$upper,
            control = de_control
          )
          
          de_result
        }),
        label = "全局优化(DE)"
      ),
      
      # 步骤 4: 局部精化（L-BFGS-B）
      list(
        expr = quote({
          optim_result <- optim(
            par = de_result$optim$bestmem,
            fn = prep$objective_fn,
            method = "L-BFGS-B",
            lower = prep$lower,
            upper = prep$upper,
            control = list(maxit = LBFGS_OPTIM$max_iter)
          )
          
          optim_result
        }),
        label = "局部精化(L-BFGS-B)"
      )
    ),
    overall_label = sprintf("ITC拟合 (%s模型)", model_type),
    category = "fitting"
  )
  
  # 获取最终结果
  final_result <- fitting_result$results[[4]]  # L-BFGS-B 结果
  
  # 记录性能信息
  total_time <- fitting_result$summary$total_elapsed
  
  # 如果拟合很慢，记录警告
  if (total_time > 10.0) {
    log_warning(sprintf("拟合耗时较长: %.2f 秒 (%s模型, %d数据点)", 
                       total_time, model_type, nrow(data)))
  }
  
  # 返回结果（包含性能信息）
  return(list(
    params = final_result$par,
    value = final_result$value,
    convergence = final_result$convergence,
    performance = list(
      total_time = total_time,
      step_timings = fitting_result$summary$step_timings
    )
  ))
}

# ==============================================================================
# 示例 2: 在 Shiny observeEvent 中应用性能监控
# ==============================================================================

# 改进前的代码
observeEvent_old <- function(input, output, session) {
  observeEvent(input$run_fitting, {
    # 获取数据
    data <- experiment_data()
    if (is.null(data)) {
      showNotification("请先加载数据", type = "error")
      return(NULL)
    }
    
    # 获取参数
    params <- list(
      logK = input$logK,
      H = input$H,
      fH = input$fH
    )
    
    # 执行拟合
    result <- perform_fitting_sync(data, params, model_type = "1to1")
    
    # 更新结果
    fitted_params(result$params)
    
    showNotification("拟合完成", type = "message")
  })
}

# 改进后的代码（添加性能监控）
observeEvent_new <- function(input, output, session) {
  observeEvent(input$run_fitting, {
    
    # 整个操作使用性能监控
    result <- perf_monitor({
      
      # 1. 获取和验证数据
      data <- experiment_data()
      if (is.null(data)) {
        showNotification("请先加载数据", type = "error")
        return(NULL)
      }
      
      # 2. 获取参数
      params <- list(
        logK = safe_numeric(input$logK, default = 5, min = -5, max = 12),
        H = safe_numeric(input$H, default = -5000, min = -15000, max = 5000),
        fH = safe_numeric(input$fH, default = 1, min = 0.1, max = 10)
      )
      
      # 3. 执行拟合（内部已有性能监控）
      fit_result <- perform_fitting_sync_new(data, params, model_type = "1to1")
      
      # 4. 更新结果
      fitted_params(fit_result$params)
      
      fit_result
      
    }, 
    label = "拟合按钮点击处理",
    category = "user_interaction",
    details = list(
      model_type = "1to1",
      data_points = nrow(experiment_data())
    ))
    
    # 根据耗时显示不同的通知
    if (!is.null(result$result)) {
      elapsed <- result$elapsed
      
      if (elapsed < 2.0) {
        notification_type <- "message"
        notification_msg <- sprintf("拟合完成 (%.2f秒)", elapsed)
      } else if (elapsed < 5.0) {
        notification_type <- "warning"
        notification_msg <- sprintf("拟合完成，耗时较长 (%.2f秒)", elapsed)
      } else {
        notification_type <- "warning"
        notification_msg <- sprintf("拟合完成，耗时很长 (%.2f秒)，建议优化参数或减少数据点", elapsed)
      }
      
      showNotification(notification_msg, type = notification_type, duration = 5)
    }
  })
}

# ==============================================================================
# 示例 3: 在数据导入中应用性能监控
# ==============================================================================

# 改进前的代码
load_data_old <- function(file_path) {
  data <- read.csv(file_path)
  
  # 验证列
  required_cols <- c("V_inj", "dQ_obs")
  if (!all(required_cols %in% colnames(data))) {
    stop("缺少必需的列")
  }
  
  # 数据清理
  data <- data[complete.cases(data), ]
  
  # 排序
  data <- data[order(data$V_inj), ]
  
  return(data)
}

# 改进后的代码（添加性能监控）
load_data_new <- function(file_path) {
  
  result <- perf_monitor({
    
    # 1. 读取文件
    data <- tryCatch({
      read.csv(file_path, stringsAsFactors = FALSE)
    }, error = function(e) {
      handle_error(e, 
                  context = sprintf("读取文件: %s", basename(file_path)),
                  error_code = ERROR_CODES$FILE_READ_ERROR,
                  lang_val = "zh")
      return(NULL)
    })
    
    if (is.null(data)) return(NULL)
    
    # 2. 验证数据
    validated_data <- validate_dataframe(
      data,
      required_cols = c("V_inj", "dQ_obs"),
      context = sprintf("导入的CSV文件: %s", basename(file_path))
    )
    
    if (is.null(validated_data)) return(NULL)
    
    # 3. 数据清理
    # 移除 NA 行
    clean_data <- validated_data[complete.cases(validated_data), ]
    
    # 4. 排序
    clean_data <- clean_data[order(clean_data$V_inj), ]
    
    clean_data
    
  },
  label = sprintf("加载CSV文件: %s", basename(file_path)),
  category = "data_io",
  details = list(
    filename = basename(file_path),
    filesize = file.size(file_path)
  ))
  
  # 如果文件很大且加载很慢，记录警告
  if (!is.null(result$result) && result$elapsed > 2.0) {
    log_warning(sprintf("大文件加载耗时: %.2f 秒 (%s, %.2f MB)",
                       result$elapsed,
                       basename(file_path),
                       file.size(file_path) / 1024^2))
  }
  
  return(result$result)
}

# ==============================================================================
# 示例 4: 在模拟计算中应用性能监控
# ==============================================================================

# 改进前的代码
simulate_itc_old <- function(params, n_injections, v_total, c_m, c_s) {
  # 生成注射体积
  v_inj <- seq(0, v_total, length.out = n_injections)
  
  # 计算每次注射的热量
  dQ <- numeric(n_injections)
  for (i in seq_along(v_inj)) {
    dQ[i] <- calculate_heat(params, v_inj[i], c_m, c_s)
  }
  
  # 生成数据框
  result <- data.frame(
    injection = 1:n_injections,
    V_inj = v_inj,
    dQ = dQ
  )
  
  return(result)
}

# 改进后的代码（添加性能监控）
simulate_itc_new <- function(params, n_injections, v_total, c_m, c_s) {
  
  result <- perf_monitor_batch(
    steps = list(
      # 步骤 1: 验证参数
      list(
        expr = quote({
          # 验证参数范围
          validate_param_value("logK", params$logK)
          validate_param_value("H", params$H)
          validate_param_value("fH", params$fH)
          
          # 验证实验参数
          if (n_injections <= 0 || n_injections > 100) {
            stop("注射次数必须在 1-100 之间")
          }
          if (v_total <= 0) {
            stop("总体积必须大于 0")
          }
          
          TRUE
        }),
        label = "参数验证"
      ),
      
      # 步骤 2: 生成注射序列
      list(
        expr = quote({
          v_inj <- seq(0, v_total, length.out = n_injections)
          v_inj
        }),
        label = "生成注射序列"
      ),
      
      # 步骤 3: 计算热量（向量化）
      list(
        expr = quote({
          # 使用向量化计算提高性能
          dQ <- sapply(v_inj, function(v) {
            calculate_heat(params, v, c_m, c_s)
          })
          dQ
        }),
        label = "计算ITC热量"
      ),
      
      # 步骤 4: 构建结果数据框
      list(
        expr = quote({
          result_df <- data.frame(
            injection = 1:n_injections,
            V_inj = v_inj,
            dQ = dQ,
            stringsAsFactors = FALSE
          )
          result_df
        }),
        label = "构建结果"
      )
    ),
    overall_label = sprintf("ITC模拟 (%d注射)", n_injections),
    category = "simulation"
  )
  
  # 获取最终结果
  final_result <- result$results[[4]]
  
  # 记录性能（如果模拟很慢）
  if (result$summary$total_elapsed > 1.0) {
    log_info(sprintf("ITC模拟耗时: %.3f 秒 (%d注射, logK=%.2f, H=%.1f)",
                    result$summary$total_elapsed,
                    n_injections,
                    params$logK,
                    params$H))
  }
  
  return(final_result)
}

# ==============================================================================
# 示例 5: 创建性能监控的包装函数（函数装饰器）
# ==============================================================================

# 为现有函数添加性能监控（无需修改原函数）
wrap_with_performance_monitoring <- function() {
  
  # 包装数据加载函数
  read.csv.monitored <- with_perf_monitor(
    read.csv,
    label = "read.csv",
    category = "data_io"
  )
  
  # 包装优化函数
  DEoptim.monitored <- with_perf_monitor(
    DEoptim::DEoptim,
    label = "DEoptim",
    category = "optimization"
  )
  
  optim.monitored <- with_perf_monitor(
    optim,
    label = "optim",
    category = "optimization"
  )
  
  # 返回包装后的函数列表
  return(list(
    read.csv = read.csv.monitored,
    DEoptim = DEoptim.monitored,
    optim = optim.monitored
  ))
}

# 使用包装后的函数
use_monitored_functions <- function() {
  monitored <- wrap_with_performance_monitoring()
  
  # 使用包装后的函数（自动监控性能）
  data <- monitored$read.csv("data.csv")
  
  result <- monitored$DEoptim(
    objective_fn,
    lower = c(-5, -15000),
    upper = c(12, 5000)
  )
}

# ==============================================================================
# 示例 6: 在 Shiny 应用启动时配置性能监控
# ==============================================================================

configure_performance_monitoring_on_startup <- function() {
  
  # 根据环境配置性能监控
  is_production <- Sys.getenv("R_ENV") == "production"
  
  if (is_production) {
    # 生产环境：仅监控关键操作
    set_perf_config(
      enabled = TRUE,
      log_level = "summary",
      slow_threshold = 2.0,
      warning_threshold = 10.0,
      console_output = FALSE,
      auto_log_slow = TRUE,
      max_records = 500
    )
    
    message("性能监控已启用（生产模式）")
    
  } else {
    # 开发环境：详细监控
    set_perf_config(
      enabled = TRUE,
      log_level = "detailed",
      slow_threshold = 0.5,
      warning_threshold = 2.0,
      console_output = TRUE,
      auto_log_slow = TRUE,
      max_records = 1000
    )
    
    message("性能监控已启用（开发模式）")
  }
}

# 在 server.R 开头调用
# configure_performance_monitoring_on_startup()

# ==============================================================================
# 示例 7: 在 Shiny 会话结束时生成性能报告
# ==============================================================================

setup_performance_reporting_on_session_end <- function(session) {
  
  onSessionEnded(function() {
    
    # 生成会话性能报告
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    report_file <- sprintf("logs/perf_report_session_%s.txt", timestamp)
    
    # 确保目录存在
    if (!dir.exists("logs")) {
      dir.create("logs", recursive = TRUE)
    }
    
    # 生成报告
    tryCatch({
      generate_perf_report(output_file = report_file)
      message(sprintf("会话性能报告已保存: %s", report_file))
    }, error = function(e) {
      warning(sprintf("无法生成性能报告: %s", e$message))
    })
    
    # 分析性能瓶颈
    tryCatch({
      analysis_file <- sprintf("logs/perf_analysis_session_%s.txt", timestamp)
      sink(analysis_file)
      analyze_performance()
      sink()
      message(sprintf("性能分析已保存: %s", analysis_file))
    }, error = function(e) {
      sink()
      warning(sprintf("无法生成性能分析: %s", e$message))
    })
    
    # 清除记录
    clear_perf_records()
  })
}

# 在 server.R 中调用
# setup_performance_reporting_on_session_end(session)

# ==============================================================================
# 示例 8: 创建性能监控的 Shiny 管理面板
# ==============================================================================

# 在 ui.R 中添加（可以是一个隐藏的标签页或模态对话框）
create_performance_panel_ui <- function() {
  tabPanel(
    "性能监控",
    icon = icon("chart-line"),
    
    fluidRow(
      column(
        width = 12,
        h3("性能监控面板"),
        hr()
      )
    ),
    
    fluidRow(
      column(
        width = 4,
        actionButton("perf_refresh", "刷新数据", icon = icon("refresh")),
        actionButton("perf_clear", "清除记录", icon = icon("trash")),
        actionButton("perf_export", "导出报告", icon = icon("download"))
      ),
      column(
        width = 4,
        selectInput("perf_category_filter", "筛选类别",
                   choices = c("全部" = "all", 
                              "拟合" = "fitting",
                              "模拟" = "simulation",
                              "数据IO" = "data_io",
                              "优化" = "optimization"),
                   selected = "all")
      ),
      column(
        width = 4,
        numericInput("perf_min_elapsed", "最小耗时（秒）",
                    value = 0, min = 0, step = 0.1)
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        h4("性能统计"),
        verbatimTextOutput("perf_summary")
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        h4("性能记录"),
        DTOutput("perf_records_table")
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        h4("性能分析"),
        verbatimTextOutput("perf_analysis")
      )
    )
  )
}

# 在 server.R 中添加对应的逻辑
create_performance_panel_server <- function(input, output, session) {
  
  # 刷新数据
  perf_data <- reactiveVal(get_perf_records())
  
  observeEvent(input$perf_refresh, {
    perf_data(get_perf_records())
  })
  
  # 清除记录
  observeEvent(input$perf_clear, {
    clear_perf_records()
    perf_data(list())
    showNotification("性能记录已清除", type = "message")
  })
  
  # 导出报告
  observeEvent(input$perf_export, {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- sprintf("performance_report_%s.txt", timestamp)
    
    generate_perf_report(output_file = filename)
    
    showNotification(
      sprintf("性能报告已导出: %s", filename),
      type = "message",
      duration = 5
    )
  })
  
  # 筛选记录
  filtered_records <- reactive({
    records <- perf_data()
    
    # 筛选类别
    if (input$perf_category_filter != "all") {
      records <- Filter(function(r) r$category == input$perf_category_filter, records)
    }
    
    # 筛选耗时
    if (input$perf_min_elapsed > 0) {
      records <- Filter(function(r) r$elapsed >= input$perf_min_elapsed, records)
    }
    
    records
  })
  
  # 性能统计
  output$perf_summary <- renderPrint({
    records <- filtered_records()
    
    if (length(records) == 0) {
      cat("没有性能记录\n")
      return()
    }
    
    cat(sprintf("总记录数: %d\n", length(records)))
    cat(sprintf("慢操作数: %d\n", 
               sum(sapply(records, function(r) r$elapsed >= PERF_CONFIG$slow_threshold))))
    cat(sprintf("总耗时: %.2f 秒\n", 
               sum(sapply(records, function(r) r$elapsed))))
    cat(sprintf("平均耗时: %.3f 秒\n",
               mean(sapply(records, function(r) r$elapsed))))
  })
  
  # 性能记录表格
  output$perf_records_table <- renderDT({
    records <- filtered_records()
    
    if (length(records) == 0) {
      return(data.frame())
    }
    
    df <- do.call(rbind, lapply(records, function(r) {
      data.frame(
        时间戳 = format(r$timestamp, "%H:%M:%S"),
        类别 = r$category,
        标签 = r$label,
        耗时 = sprintf("%.3f秒", r$elapsed),
        内存 = ifelse(is.na(r$memory_used), "-", sprintf("%.1f MB", r$memory_used)),
        错误 = ifelse(r$error, "是", "否"),
        stringsAsFactors = FALSE
      )
    }))
    
    datatable(df, options = list(pageLength = 20, order = list(list(0, 'desc'))))
  })
  
  # 性能分析
  output$perf_analysis <- renderPrint({
    records <- filtered_records()
    
    if (length(records) == 0) {
      cat("没有性能记录可供分析\n")
      return()
    }
    
    # 简化的分析输出
    cat("=== 性能瓶颈分析 ===\n\n")
    
    # 最慢的操作
    elapsed_times <- sapply(records, function(r) r$elapsed)
    labels <- sapply(records, function(r) r$label)
    
    slow_idx <- order(elapsed_times, decreasing = TRUE)[1:min(5, length(elapsed_times))]
    
    cat("最慢的 5 个操作:\n")
    for (i in slow_idx) {
      cat(sprintf("%d. %s: %.3f秒\n", 
                 which(slow_idx == i), labels[i], elapsed_times[i]))
    }
  })
}

# ==============================================================================
# 使用说明
# ==============================================================================

# 1. 将需要的函数复制到对应的文件中（如 server.R, R/core_logic.R 等）
# 2. 根据实际需求调整性能监控的标签、类别和阈值
# 3. 在应用启动时配置性能监控参数
# 4. 定期查看性能报告和分析结果
# 5. 根据分析结果优化性能瓶颈

# 注意：
# - 性能监控会增加轻微开销，建议仅在开发和测试环境中启用详细监控
# - 生产环境可以使用较高的阈值和较低的日志级别
# - 定期清理性能记录以防止内存溢出
