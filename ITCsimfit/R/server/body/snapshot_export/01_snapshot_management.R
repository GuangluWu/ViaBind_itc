  # ============================================================================
  #  [MODIFIED] Section: 参数快照、RSS计算、导入导出
  # ============================================================================
  
  # --- [修复] 保存快照 (增加命名逻辑 + 高精度 RSS) ---
  update_v_pre_from_snapshot <- function(value) {
    if (exists("update_v_pre_programmatically", mode = "function", inherits = TRUE)) {
      return(isTRUE(update_v_pre_programmatically(value)))
    }
    target_num <- suppressWarnings(as.numeric(value)[1])
    if (!is.finite(target_num)) return(FALSE)
    current_num <- suppressWarnings(as.numeric(input$V_pre)[1])
    if (is.finite(current_num) && isTRUE(all.equal(current_num, target_num, tolerance = 1e-8))) {
      return(TRUE)
    }
    values$v_pre_programmatic_update <- TRUE
    updateNumericInput(session, "V_pre", value = target_num)
    TRUE
  }

  observeEvent(input$save_params, {
    # [修复] 防止在语言切换时触发
    if(lang_switching()) return()
    # 1. 直接获取当前计算好的 RSS (确保与 UI 显示一致)
    rss_info <- current_rss_val()
    current_rss <- rss_info$rss
    
    # 2. 【核心修改】构建名称 (Name)
    # 规则：
    # 1) xx + 文件名 + 时间戳
    # 2) 文件名 + 时间戳
    # 3) xx + sim + 时间戳
    # 4) sim + 时间戳
    time_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
    xx_raw <- trimws(input$snap_name)
    file_raw <- values$imported_xlsx_base_name
    has_base_name <- FALSE
    if (!is.null(file_raw) && length(file_raw) > 0) {
      base_candidate <- trimws(as.character(file_raw[[1]]))
      has_base_name <- nzchar(base_candidate)
    }
    if (!has_base_name) file_raw <- values$imported_xlsx_filename

    sanitize_snap_part <- function(x, drop_extension = FALSE) {
      if (is.null(x) || length(x) == 0) return("")
      out <- trimws(as.character(x[[1]]))
      if (!nzchar(out)) return("")
      if (drop_extension) out <- tools::file_path_sans_ext(out)
      out <- gsub("[[:space:]/\\\\]+", "_", out)
      out <- gsub("_+", "_", out)
      out <- gsub("^_+|_+$", "", out)
      out
    }

    xx_part <- sanitize_snap_part(xx_raw, drop_extension = FALSE)
    file_part <- sanitize_snap_part(file_raw, drop_extension = TRUE)
    has_xx <- nzchar(xx_part)
    has_file <- nzchar(file_part)

    final_name <- if (has_xx && has_file) {
      paste(xx_part, file_part, time_str, sep = "_")
    } else if (!has_xx && has_file) {
      paste(file_part, time_str, sep = "_")
    } else if (has_xx && !has_file) {
      paste(xx_part, "sim", time_str, sep = "_")
    } else {
      paste("sim", time_str, sep = "_")
    }
    
    # 3. 构造数据行 (用 Name 替代原来的 Time 列)
    # [新增] 包含实验条件，放在最后几列
    # [新增] 包含标准误差数据（如果有误差分析结果）
    
    # 获取误差分析结果（如果存在）
    error_df <- values$error_analysis
    se_values <- list()  # 存储标准误差值
    
    # 定义所有可能的参数名
    all_params <- c("logK1", "H1", "logK2", "H2", "logK3", "H3", 
                    "logK4", "H4", "logK5", "H5", "logK6", "H6",
                    "fH", "fG", "V_init", "Offset")
    
    # 为每个参数查找标准误差
    for (param in all_params) {
      if (!is.null(error_df) && param %in% error_df$Parameter) {
        se_val <- error_df$SE[error_df$Parameter == param]
        se_values[[param]] <- if(length(se_val) > 0 && is.finite(se_val)) se_val else NA
      } else {
        se_values[[param]] <- NA
      }
    }
    
    # [新增] 未激活的模型路径对应参数存 NA，其标准误差也置空
    active_paths_save <- if(is.null(input$active_paths) || length(input$active_paths) == 0) character(0) else input$active_paths
    if(!"rxn_D" %in% active_paths_save) { se_values[["logK2"]] <- NA; se_values[["H2"]] <- NA }
    if(!"rxn_T" %in% active_paths_save) { se_values[["logK3"]] <- NA; se_values[["H3"]] <- NA }
    if(!"rxn_B" %in% active_paths_save) { se_values[["logK4"]] <- NA; se_values[["H4"]] <- NA }
    if(!"rxn_F" %in% active_paths_save) { se_values[["logK5"]] <- NA; se_values[["H5"]] <- NA }
    if(!"rxn_U" %in% active_paths_save) { se_values[["logK6"]] <- NA; se_values[["H6"]] <- NA }
    
    # 格式化标准误差（logK保留3位小数，H保留1位小数，其他保留3位小数）
    format_se <- function(param, se) {
      if (is.na(se) || !is.finite(se)) return("")
      if (grepl("logK", param)) {
        return(sprintf("%.3f", se))
      } else if (grepl("^H[0-9]$", param)) {
        return(sprintf("%.1f", se))
      } else {
        return(sprintf("%.3f", se))
      }
    }
    
    # [新增] 未激活路径对应的参数存 NA
    logK2_save <- if("rxn_D" %in% active_paths_save) input$logK2 else NA
    H2_save    <- if("rxn_D" %in% active_paths_save) input$H2 else NA
    logK3_save <- if("rxn_T" %in% active_paths_save) input$logK3 else NA
    H3_save    <- if("rxn_T" %in% active_paths_save) input$H3 else NA
    logK4_save <- if("rxn_B" %in% active_paths_save) input$logK4 else NA
    H4_save    <- if("rxn_B" %in% active_paths_save) input$H4 else NA
    logK5_save <- if("rxn_F" %in% active_paths_save) input$logK5 else NA
    H5_save    <- if("rxn_F" %in% active_paths_save) input$H5 else NA
    logK6_save <- if("rxn_U" %in% active_paths_save) input$logK6 else NA
    H6_save    <- if("rxn_U" %in% active_paths_save) input$H6 else NA
    
    new_row <- data.frame(
      Name = final_name, # 第一列现在是组合名称
      RSS = if(is.na(current_rss)) "-" else formatC(current_rss, format="e", digits=3),
      # [修正] 强制包含 rxn_M，因为它总是激活的
      Model = paste(c("rxn_M", input$active_paths), collapse="+"),
      
      logK1=input$logK1, H1=input$H1, logK2=logK2_save, H2=H2_save,
      logK3=logK3_save, H3=H3_save, logK4=logK4_save, H4=H4_save,
      logK5=logK5_save, H5=H5_save, logK6=logK6_save, H6=H6_save,
      fH=input$factor_H, fG=input$factor_G, V_init=input$V_init_val,
      Offset=input$heat_offset,
      # [新增] 标准误差列
      logK1_SE = format_se("logK1", se_values[["logK1"]]),
      H1_SE = format_se("H1", se_values[["H1"]]),
      logK2_SE = format_se("logK2", se_values[["logK2"]]),
      H2_SE = format_se("H2", se_values[["H2"]]),
      logK3_SE = format_se("logK3", se_values[["logK3"]]),
      H3_SE = format_se("H3", se_values[["H3"]]),
      logK4_SE = format_se("logK4", se_values[["logK4"]]),
      H4_SE = format_se("H4", se_values[["H4"]]),
      logK5_SE = format_se("logK5", se_values[["logK5"]]),
      H5_SE = format_se("H5", se_values[["H5"]]),
      logK6_SE = format_se("logK6", se_values[["logK6"]]),
      H6_SE = format_se("H6", se_values[["H6"]]),
      fH_SE = format_se("fH", se_values[["fH"]]),
      fG_SE = format_se("fG", se_values[["fG"]]),
      V_init_SE = format_se("V_init", se_values[["V_init"]]),
      Offset_SE = format_se("Offset", se_values[["Offset"]]),
      # [新增] 实验条件列（放在最后）
      H_cell_0=input$H_cell_0, G_syringe=input$G_syringe, V_cell=input$V_cell, 
      V_inj=input$V_inj, n_inj=input$n_inj, V_pre=input$V_pre, Temp=input$Temp,
      stringsAsFactors=FALSE
    )
    
    values$param_list <- rbind(values$param_list, new_row)
    updateTextInput(session, "snap_name", value = "") 
  })

  # --- [新增] 清除所有快照 (Modal 确认) ---
  observeEvent(input$clear_params, {
    # [修复] 防止在语言切换时触发
    if(lang_switching()) return()
    showModal(modalDialog(
      title = tr("snapshot_clear_confirm_title", lang()),
      tr("snapshot_clear_confirm_msg", lang()),
      footer = tagList(
        modalButton(tr("snapshot_clear_confirm_cancel", lang())),
        actionButton("confirm_clear", tr("snapshot_clear_confirm_ok", lang()), class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_clear, {
    values$param_list <- data.frame()
    removeModal()
    showNotification(tr("snapshot_clear_success", lang()), type = "message")
  })

  # --- [新增] 恢复默认参数 (Model 确认) ---
  observeEvent(input$reset_defaults, {
    # [修复] 防止在语言切换时触发
    if(lang_switching()) return()
    showModal(modalDialog(
      title = tr("reset_confirm_title", lang()),
      tr("reset_confirm_msg", lang()),
      footer = tagList(
        modalButton(tr("reset_confirm_cancel", lang())),
        actionButton("confirm_reset", tr("reset_confirm_ok", lang()), class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_reset, {
    # [修复] 使用 DEFAULT_PARAMS 常量，确保与初始值一致
    updateSliderInput(session, "logK1", value=DEFAULT_PARAMS$logK)
    updateSliderInput(session, "H1", value=DEFAULT_PARAMS$H)
    updateSliderInput(session, "logK2", value=DEFAULT_PARAMS$logK)
    updateSliderInput(session, "H2", value=DEFAULT_PARAMS$H)
    updateSliderInput(session, "logK3", value=DEFAULT_PARAMS$logK)
    updateSliderInput(session, "H3", value=DEFAULT_PARAMS$H)
    updateSliderInput(session, "logK4", value=DEFAULT_PARAMS$logK)
    updateSliderInput(session, "H4", value=DEFAULT_PARAMS$H)
    updateSliderInput(session, "logK5", value=DEFAULT_PARAMS$logK)
    updateSliderInput(session, "H5", value=DEFAULT_PARAMS$H)
    updateSliderInput(session, "logK6", value=DEFAULT_PARAMS$logK)
    updateSliderInput(session, "H6", value=DEFAULT_PARAMS$H)
    
    updateNumericInput(session, "factor_H", value=DEFAULT_PARAMS$fH)
    updateNumericInput(session, "factor_G", value=DEFAULT_PARAMS$fG)
    updateSliderInput(session, "heat_offset", value=DEFAULT_PARAMS$Offset)
    
    # V_pre 和 V_init：有实验数据时恢复为第一针体积，否则用常量默认值
    exp_df <- tryCatch(exp_data_processed(), error = function(e) NULL)
    default_first <- if (!is.null(UI_DEFAULTS$v_pre_default)) UI_DEFAULTS$v_pre_default else DEFAULT_PARAMS$V_init
    first_v <- if (!is.null(exp_df) && "V_inj_uL" %in% names(exp_df)) {
      v <- exp_df$V_inj_uL[1]
      if (!is.na(v) && is.finite(v)) v else default_first
    } else {
      default_first
    }
    update_v_pre_from_snapshot(first_v)
    updateNumericInput(session, "V_init_val", value = first_v)
    
    removeModal()
    showNotification(tr("reset_success", lang()), type = "message")
  })

  # --- 2. 渲染表格 (修改：倒序显示，最新的在最上面) ---
  output$param_table <- renderDT({ 
    df <- values$param_list
    
    # 如果数据框为空，返回空表格
    if(is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(), 
                      options=list(dom='t'), 
                      rownames=FALSE))
    }
    
    # 确保 df 是数据框且列名有效
    # 移除任何意外的列名（比如翻译后的列名）
    # 只保留预期的列名
    expected_cols <- c("Name", "RSS", "Model", "logK1", "H1", "logK2", "H2", 
                       "logK3", "H3", "logK4", "H4", "logK5", "H5", "logK6", "H6",
                       "fH", "fG", "V_init", "Offset",
                       "logK1_SE", "H1_SE", "logK2_SE", "H2_SE",
                       "logK3_SE", "H3_SE", "logK4_SE", "H4_SE",
                       "logK5_SE", "H5_SE", "logK6_SE", "H6_SE",
                       "fH_SE", "fG_SE", 
                       "V_init_SE", "Offset_SE",
                       "H_cell_0", "G_syringe", "V_cell", "V_inj", 
                       "n_inj", "V_pre", "Temp")
    
    # 只保留存在的列（避免列名不匹配的错误）
    valid_cols <- intersect(colnames(df), expected_cols)
    if(length(valid_cols) == 0) {
      # 如果没有有效列，返回空表格
      return(datatable(data.frame(), 
                      options=list(dom='t'), 
                      rownames=FALSE))
    }
    
    # 只选择有效列
    df <- df[, valid_cols, drop = FALSE]
    
    # 如果有数据，进行倒序排列
    if(nrow(df) > 0) df <- df[nrow(df):1, ]
    
    datatable(df, 
              options=list(
                dom='tp', 
                pageLength=5, 
                scrollX=TRUE,
                scrollY = "120px",  # 固定表头，设置滚动高度（减小以让两行就能滚动）
                scrollCollapse = FALSE  # 改为 FALSE，确保即使内容少也显示滚动区域
              ), 
              selection='single',
              rownames=FALSE) 
  })
  
  # --- 3. 点击表格加载参数 (修改：适配倒序逻辑) ---
  # [辅助] 判断参数是否有效（NA、空、字符串"NA"视为无效，读取时用默认值）
  is_valid_snap_param <- function(x) {
    if (is.null(x)) return(FALSE)
    if (length(x) != 1) return(FALSE)
    if (is.na(x)) return(FALSE)
    if (is.character(x) && (nchar(trimws(as.character(x))) == 0 || trimws(as.character(x)) == "NA")) return(FALSE)
    if (is.character(x)) { n <- suppressWarnings(as.numeric(x)); if (is.na(n)) return(FALSE) }
    TRUE
  }
  snap_param_value <- function(x, default) {
    if (!is_valid_snap_param(x)) return(default)
    if (is.numeric(x)) return(x)
    suppressWarnings(as.numeric(x))
  }
  
  observeEvent(input$param_table_rows_selected, {
    # 1. 构建与显示一致的倒序数据框
    df_display <- values$param_list
    if(nrow(df_display) > 0) df_display <- df_display[nrow(df_display):1, ]
    
    # 2. 从倒序数据框中提取选中行
    target_p <- df_display[input$param_table_rows_selected, ]
    
    # 3. 批量更新 UI (兼容性处理：列不存在或为NA/空时使用默认值)
    # 基础参数 (logK1, H1)
    updateSliderInput(session, "logK1", value = snap_param_value(target_p$logK1, DEFAULT_PARAMS$logK))
    updateSliderInput(session, "H1", value = snap_param_value(target_p$H1, DEFAULT_PARAMS$H))
    
    # 可选参数 (logK2–H6)：读到 NA 则自动用默认值
    updateSliderInput(session, "logK2", value = snap_param_value(target_p$logK2, DEFAULT_PARAMS$logK))
    updateSliderInput(session, "H2", value = snap_param_value(target_p$H2, DEFAULT_PARAMS$H))
    updateSliderInput(session, "logK3", value = snap_param_value(target_p$logK3, DEFAULT_PARAMS$logK))
    updateSliderInput(session, "H3", value = snap_param_value(target_p$H3, DEFAULT_PARAMS$H))
    updateSliderInput(session, "logK4", value = snap_param_value(target_p$logK4, DEFAULT_PARAMS$logK))
    updateSliderInput(session, "H4", value = snap_param_value(target_p$H4, DEFAULT_PARAMS$H))
    updateSliderInput(session, "logK5", value = snap_param_value(target_p$logK5, DEFAULT_PARAMS$logK))
    updateSliderInput(session, "H5", value = snap_param_value(target_p$H5, DEFAULT_PARAMS$H))
    updateSliderInput(session, "logK6", value = snap_param_value(target_p$logK6, DEFAULT_PARAMS$logK))
    updateSliderInput(session, "H6", value = snap_param_value(target_p$H6, DEFAULT_PARAMS$H))
    
    updateNumericInput(session, "factor_H", value = snap_param_value(target_p$fH, DEFAULT_PARAMS$fH))
    updateNumericInput(session, "factor_G", value = snap_param_value(target_p$fG, DEFAULT_PARAMS$fG))
    updateNumericInput(session, "V_init_val", value = snap_param_value(target_p$V_init, DEFAULT_PARAMS$V_init))
    updateSliderInput(session, "heat_offset", value = snap_param_value(target_p$Offset, DEFAULT_PARAMS$Offset))
    
    # [新增] 加载实验条件（兼容性处理：如果列不存在，设为NA或默认值）
    if("H_cell_0" %in% names(target_p) && !is.na(target_p$H_cell_0)) {
      updateNumericInput(session, "H_cell_0", value=target_p$H_cell_0)
    }
    if("G_syringe" %in% names(target_p) && !is.na(target_p$G_syringe)) {
      updateNumericInput(session, "G_syringe", value=target_p$G_syringe)
    }
    if("V_cell" %in% names(target_p) && !is.na(target_p$V_cell)) {
      updateNumericInput(session, "V_cell", value=target_p$V_cell)
    }
    if("V_inj" %in% names(target_p) && !is.na(target_p$V_inj)) {
      updateNumericInput(session, "V_inj", value=target_p$V_inj)
    }
    if("n_inj" %in% names(target_p) && !is.na(target_p$n_inj)) {
      updateNumericInput(session, "n_inj", value=target_p$n_inj)
    }
    if("V_pre" %in% names(target_p) && !is.na(target_p$V_pre)) {
      update_v_pre_from_snapshot(target_p$V_pre)
    }
    if("Temp" %in% names(target_p) && !is.na(target_p$Temp)) {
      updateNumericInput(session, "Temp", value=target_p$Temp)
    } else {
      # [修复] 如果温度不存在，使用常量定义的默认值
      updateNumericInput(session, "Temp", value=UI_DEFAULTS$temp_default)
    }
    
    # 4. [新增] 自动解析并勾选模型 (Model)
    # Model 格式示例: "rxn_M+rxn_D+rxn_T"
    if ("Model" %in% names(target_p) && !is.na(target_p$Model) && nzchar(as.character(target_p$Model))) {
      tryCatch({
        saved_model_str <- as.character(target_p$Model)
        # 分割字符串 ("+" 分隔)
        saved_paths <- strsplit(saved_model_str, "\\+")[[1]]
        
        # 定义有效路径 (必须与 checkboxGroupInput 的 values 对应)
        valid_paths <- c("rxn_D", "rxn_T", "rxn_B", "rxn_F", "rxn_U")
        
        # 筛选出有效路径 (rxn_M 是基础反应，不在 checkbox 中，忽略即可)
        paths_to_select <- intersect(saved_paths, valid_paths)
        
        # 更新 CheckboxGroupInput
        updateCheckboxGroupInput(session, "active_paths", selected = paths_to_select)
        
      }, error = function(e) {
        showNotification(paste(tr("plot_error", lang()), e$message), type = "warning")
      })
    } else {
      # 如果 Model 列不存在或为空，提示用户
      showNotification(tr("import_params_warning_no_model", lang()), type = "warning")
    }
    
    showNotification(paste(tr("import_params_success", lang()), target_p$Name), type="message")
  })
  
  # --- 4. 导入参数文件 (xlsx 格式) ---
  observeEvent(input$import_params_file, {
    req(input$import_params_file)
    tryCatch({
      # 读取 xlsx（snapshots sheet）
      imported_df <- as.data.frame(readxl::read_excel(input$import_params_file$datapath, sheet = 1))
      
      # 将文件列名（带单位后缀）还原为内部列名
      file_to_internal <- c(
        "H_cell_0_mM" = "H_cell_0", "G_syringe_mM" = "G_syringe",
        "V_cell_mL" = "V_cell", "V_inj_uL" = "V_inj", "V_pre_uL" = "V_pre",
        "Temp_K" = "Temp",
        "H1_cal_mol" = "H1", "H2_cal_mol" = "H2", "H3_cal_mol" = "H3",
        "H4_cal_mol" = "H4", "H5_cal_mol" = "H5", "H6_cal_mol" = "H6",
        "V_init_uL" = "V_init", "Offset_cal" = "Offset"
      )
      for (file_name in names(file_to_internal)) {
        if (file_name %in% colnames(imported_df)) {
          colnames(imported_df)[colnames(imported_df) == file_name] <- file_to_internal[[file_name]]
        }
      }
      
      # 关键列检查
      required_cols <- c("logK1", "H1", "fH", "fG")
      if(!all(required_cols %in% colnames(imported_df))) {
        showNotification(tr("import_params_error_format", lang()), type = "error")
        return()
      }
      
      # 统一 RSS 列格式为科学计数法字符串
      if("RSS" %in% colnames(imported_df)) {
        rss_vals <- suppressWarnings(as.numeric(imported_df$RSS))
        imported_df$RSS <- ifelse(is.na(rss_vals), "-", formatC(rss_vals, format="e", digits=3))
      }
      
      # 为缺失的可选参数列添加列（用默认值）；若列存在但值为 NA 则保留 NA，加载快照时自动用默认值
      optional_param_cols <- list(
        "logK2" = 7.0, "H2" = -6000,
        "logK3" = 7.0, "H3" = -6000,
        "logK4" = 7.0, "H4" = -6000,
        "logK5" = 7.0, "H5" = -6000,
        "logK6" = 7.0, "H6" = -6000,
        "V_init" = 0, "Offset" = 0
      )
      for(col in names(optional_param_cols)) {
        if(!col %in% colnames(imported_df)) {
          imported_df[[col]] <- optional_param_cols[[col]]
        }
        # 列存在时不再把 NA 覆盖为默认值，读取快照时由 snap_param_value 自动用默认值
      }
      
      # 实验条件列（如果不存在，设为NA）
      exp_condition_cols <- c("H_cell_0", "G_syringe", "V_cell", "V_inj", "n_inj", "V_pre", "Temp")
      for(col in exp_condition_cols) {
        if(!col %in% colnames(imported_df)) {
          imported_df[[col]] <- NA
        }
      }
      if("Temp" %in% colnames(imported_df)) {
        imported_df$Temp[is.na(imported_df$Temp)] <- 298.15
      }
      
      # 内部预期列名
      expected_cols <- c("Name", "RSS", "Model", "logK1", "H1", "logK2", "H2", 
                         "logK3", "H3", "logK4", "H4", "logK5", "H5", 
                         "logK6", "H6",
                         "fH", "fG", "V_init", "Offset",
                         "logK1_SE", "H1_SE", "logK2_SE", "H2_SE",
                         "logK3_SE", "H3_SE", "logK4_SE", "H4_SE",
                         "logK5_SE", "H5_SE", "logK6_SE", "H6_SE",
                         "fH_SE", "fG_SE", 
                         "V_init_SE", "Offset_SE",
                         "H_cell_0", "G_syringe", "V_cell", "V_inj", 
                         "n_inj", "V_pre", "Temp")
      
      valid_cols <- intersect(colnames(imported_df), expected_cols)
      if(length(valid_cols) == 0) {
        showNotification(tr("import_params_error_format", lang()), type = "error")
        return()
      }
      
      imported_df <- imported_df[, valid_cols, drop = FALSE]
      values$param_list <- bind_rows(values$param_list, imported_df)
      
      showNotification(trf("import_params_success_count", lang(), count = nrow(imported_df)), type = "message")
      
    }, error = function(e) {
      showNotification(paste(tr("import_params_error", lang()), e$message), type = "error")
    })
  })
  
