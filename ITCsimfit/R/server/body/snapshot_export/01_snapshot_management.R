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

  snapshot_expected_cols <- c(
    "Name", "RSS", "Model", "logK1", "H1", "logK2", "H2",
    "logK3", "H3", "logK4", "H4", "logK5", "H5", "logK6", "H6", "logK7", "H7",
    "fH", "fG", "V_init", "Offset",
    "logK1_SE", "H1_SE", "logK2_SE", "H2_SE",
    "logK3_SE", "H3_SE", "logK4_SE", "H4_SE",
    "logK5_SE", "H5_SE", "logK6_SE", "H6_SE", "logK7_SE", "H7_SE",
    "fH_SE", "fG_SE",
    "V_init_SE", "Offset_SE",
    "H_cell_0", "G_syringe", "V_cell", "V_inj",
    "n_inj", "FitRangeStart", "FitRangeEnd", "V_pre", "Temp"
  )
  snapshot_rowid_col <- "row_id"
  snapshot_export_rowid_col <- "_snapshot_row_id"
  snapshot_fit_bounds_sheet_name <- "snapshot_fit_bounds"

  next_snapshot_row_ids <- function(n = 1L) {
    n_int <- suppressWarnings(as.integer(n)[1])
    if (!is.finite(n_int) || n_int <= 0L) return(character(0))
    seq_now <- suppressWarnings(as.integer(values$snapshot_row_seq)[1])
    if (!is.finite(seq_now)) seq_now <- 0L
    seq_vec <- seq.int(from = seq_now + 1L, length.out = n_int)
    values$snapshot_row_seq <- seq_vec[length(seq_vec)]
    paste0("snap_", seq_vec)
  }

  build_snapshot_fit_bounds_map_for_rows <- function(rows_df, bounds_map = values$snapshot_fit_bounds_by_row_id) {
    if (!is.data.frame(rows_df) || nrow(rows_df) == 0L) return(list())
    out <- list()
    source_map <- if (is.list(bounds_map)) bounds_map else list()
    for (idx in seq_len(nrow(rows_df))) {
      row_df <- rows_df[idx, , drop = FALSE]
      row_id <- trimws(as.character(row_df[[snapshot_rowid_col]][1] %||% ""))
      if (!nzchar(row_id)) next
      out[[row_id]] <- resolve_complete_fit_bounds(
        bounds = source_map[[row_id]],
        value_source = row_df
      )
    }
    out
  }

  reassign_snapshot_row_ids_against_existing <- function(df, existing_ids = character(0)) {
    if (!is.data.frame(df) || nrow(df) == 0L || !snapshot_rowid_col %in% names(df)) return(df)
    existing_ids <- trimws(as.character(existing_ids %||% character(0)))
    row_ids <- trimws(as.character(df[[snapshot_rowid_col]] %||% character(0)))
    conflict_idx <- which(
      !nzchar(row_ids) |
        row_ids %in% existing_ids |
        duplicated(row_ids)
    )
    if (length(conflict_idx) > 0L) {
      row_ids[conflict_idx] <- next_snapshot_row_ids(length(conflict_idx))
      df[[snapshot_rowid_col]] <- row_ids
    }
    df
  }

  remap_snapshot_fit_bounds_by_row_id <- function(bounds_map, old_ids, new_ids, rows_df = NULL) {
    if (!is.list(bounds_map)) bounds_map <- list()
    old_ids <- as.character(old_ids %||% character(0))
    new_ids <- as.character(new_ids %||% character(0))
    out <- list()
    n <- min(length(old_ids), length(new_ids))
    if (n < 1L) return(out)
    for (idx in seq_len(n)) {
      new_id <- trimws(new_ids[idx])
      if (!nzchar(new_id)) next
      row_df <- if (is.data.frame(rows_df) && nrow(rows_df) >= idx) rows_df[idx, , drop = FALSE] else NULL
      out[[new_id]] <- resolve_complete_fit_bounds(
        bounds = bounds_map[[old_ids[idx]]],
        value_source = row_df
      )
    }
    out
  }

  remember_snapshot_fit_bounds_for_row <- function(row_id, row_df = NULL, bounds = NULL) {
    row_id_chr <- trimws(as.character(row_id %||% "")[1])
    if (!nzchar(row_id_chr)) return(invisible(NULL))
    stored_bounds <- resolve_complete_fit_bounds(
      bounds = bounds %||% values$snapshot_fit_bounds_by_row_id[[row_id_chr]],
      value_source = row_df
    )
    current_map <- if (is.list(values$snapshot_fit_bounds_by_row_id)) values$snapshot_fit_bounds_by_row_id else list()
    current_map[[row_id_chr]] <- stored_bounds
    values$snapshot_fit_bounds_by_row_id <- current_map
    invisible(stored_bounds)
  }

  normalize_snapshot_table <- function(df, add_row_id = TRUE) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      return(data.frame(stringsAsFactors = FALSE))
    }

    out <- as.data.frame(df, stringsAsFactors = FALSE)
    valid_cols <- intersect(colnames(out), c(snapshot_expected_cols, snapshot_rowid_col))
    if (length(valid_cols) == 0) {
      return(normalize_snapshot_table(data.frame(), add_row_id = add_row_id))
    }
    out <- out[, valid_cols, drop = FALSE]

    missing_cols <- setdiff(snapshot_expected_cols, colnames(out))
    for (col in missing_cols) out[[col]] <- NA
    if (!snapshot_rowid_col %in% colnames(out)) out[[snapshot_rowid_col]] <- NA_character_

    out[[snapshot_rowid_col]] <- trimws(as.character(out[[snapshot_rowid_col]]))
    if (isTRUE(add_row_id) && nrow(out) > 0) {
      bad_id_idx <- which(is.na(out[[snapshot_rowid_col]]) | !nzchar(out[[snapshot_rowid_col]]))
      if (length(bad_id_idx) > 0) {
        out[[snapshot_rowid_col]][bad_id_idx] <- next_snapshot_row_ids(length(bad_id_idx))
      }
      dup_idx <- duplicated(out[[snapshot_rowid_col]]) | duplicated(out[[snapshot_rowid_col]], fromLast = TRUE)
      if (any(dup_idx)) {
        out[[snapshot_rowid_col]][dup_idx] <- next_snapshot_row_ids(sum(dup_idx))
      }
    }

    out <- out[, c(snapshot_expected_cols, snapshot_rowid_col), drop = FALSE]
    rownames(out) <- NULL
    out
  }

  observe({
    normalized_df <- normalize_snapshot_table(values$param_list, add_row_id = TRUE)
    checked_now <- as.character(values$param_checked_ids %||% character(0))
    valid_ids <- as.character(normalized_df[[snapshot_rowid_col]] %||% character(0))
    checked_keep <- checked_now[checked_now %in% valid_ids]
    active_now <- as.character(values$param_active_row_id %||% "")[1]
    active_keep <- if (nzchar(active_now) && active_now %in% valid_ids) active_now else NA_character_
    bounds_now <- normalize_snapshot_fit_bounds_by_row_id(
      values$snapshot_fit_bounds_by_row_id,
      rows_df = normalized_df,
      derive_missing = FALSE
    )
    max_id_seq <- suppressWarnings(as.integer(sub("^snap_", "", valid_ids)))
    max_id_seq <- max_id_seq[is.finite(max_id_seq)]
    seq_now <- suppressWarnings(as.integer(values$snapshot_row_seq)[1])
    if (!is.finite(seq_now) || seq_now < 0L) seq_now <- 0L
    seq_keep <- if (length(max_id_seq) > 0L) max(seq_now, max(max_id_seq)) else seq_now

    changed_df <- !isTRUE(all.equal(values$param_list, normalized_df, check.attributes = FALSE))
    changed_checked <- !isTRUE(identical(checked_now, checked_keep))
    changed_active <- !isTRUE(identical(active_now, active_keep))
    changed_bounds <- !isTRUE(all.equal(values$snapshot_fit_bounds_by_row_id, bounds_now, check.attributes = FALSE))
    changed_seq <- !isTRUE(identical(seq_now, seq_keep))

    if (changed_df) values$param_list <- normalized_df
    if (changed_checked) values$param_checked_ids <- checked_keep
    if (changed_active) values$param_active_row_id <- active_keep
    if (changed_bounds) values$snapshot_fit_bounds_by_row_id <- bounds_now
    if (changed_seq) values$snapshot_row_seq <- as.integer(seq_keep)
  })

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
                    "logK4", "H4", "logK5", "H5", "logK6", "H6", "logK7", "H7",
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
    active_paths_save <- if(is.null(input$active_paths) || length(input$active_paths) == 0) character(0) else as.character(input$active_paths)
    if (exists("normalize_active_paths_with_dependencies", mode = "function", inherits = TRUE)) {
      active_paths_save <- tryCatch(
        normalize_active_paths_with_dependencies(active_paths_save),
        error = function(e) active_paths_save
      )
    }
    if(!"rxn_D" %in% active_paths_save) { se_values[["logK2"]] <- NA; se_values[["H2"]] <- NA }
    if(!"rxn_T" %in% active_paths_save) { se_values[["logK3"]] <- NA; se_values[["H3"]] <- NA }
    if(!"rxn_B" %in% active_paths_save) { se_values[["logK4"]] <- NA; se_values[["H4"]] <- NA }
    if(!"rxn_F" %in% active_paths_save) { se_values[["logK5"]] <- NA; se_values[["H5"]] <- NA }
    if(!"rxn_U" %in% active_paths_save) { se_values[["logK6"]] <- NA; se_values[["H6"]] <- NA }
    if(!"rxn_E" %in% active_paths_save) { se_values[["logK7"]] <- NA; se_values[["H7"]] <- NA }
    
    # 格式化标准误差（logK保留3位小数，H保留1位小数，其他保留3位小数）
    format_se <- function(param, se) {
      if (is.na(se) || !is.finite(se)) return("")
      if (grepl("logK", param)) {
        return(sprintf("%.3f", se))
      } else if (grepl("^H[0-9]+$", param)) {
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
    logK7_save <- if("rxn_E" %in% active_paths_save) input$logK7 else NA
    H7_save    <- if("rxn_E" %in% active_paths_save) input$H7 else NA

    fit_range_now <- suppressWarnings(as.numeric(input$fit_data_range))
    fit_range_start_save <- if (length(fit_range_now) >= 1 && is.finite(fit_range_now[1])) {
      as.integer(round(fit_range_now[1]))
    } else {
      NA_integer_
    }
    fit_range_end_save <- if (length(fit_range_now) >= 2 && is.finite(fit_range_now[2])) {
      as.integer(round(fit_range_now[2]))
    } else {
      NA_integer_
    }
    
    new_row <- data.frame(
      Name = final_name, # 第一列现在是组合名称
      RSS = if(is.na(current_rss)) "-" else formatC(current_rss, format="e", digits=3),
      # [修正] 强制包含 rxn_M，因为它总是激活的
      Model = paste(c("rxn_M", active_paths_save), collapse="+"),
      
      logK1=input$logK1, H1=input$H1, logK2=logK2_save, H2=H2_save,
      logK3=logK3_save, H3=H3_save, logK4=logK4_save, H4=H4_save,
      logK5=logK5_save, H5=H5_save, logK6=logK6_save, H6=H6_save,
      logK7=logK7_save, H7=H7_save,
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
      logK7_SE = format_se("logK7", se_values[["logK7"]]),
      H7_SE = format_se("H7", se_values[["H7"]]),
      fH_SE = format_se("fH", se_values[["fH"]]),
      fG_SE = format_se("fG", se_values[["fG"]]),
      V_init_SE = format_se("V_init", se_values[["V_init"]]),
      Offset_SE = format_se("Offset", se_values[["Offset"]]),
      # [新增] 实验条件列（放在最后）
      H_cell_0=input$H_cell_0, G_syringe=input$G_syringe, V_cell=input$V_cell, 
      V_inj=input$V_inj, n_inj=input$n_inj,
      FitRangeStart=fit_range_start_save, FitRangeEnd=fit_range_end_save,
      V_pre=input$V_pre, Temp=input$Temp,
      stringsAsFactors=FALSE
    )

    new_row <- normalize_snapshot_table(new_row, add_row_id = TRUE)
    values$param_list <- bind_rows(
      new_row,
      normalize_snapshot_table(values$param_list, add_row_id = TRUE)
    )
    new_row_id <- as.character(new_row[[snapshot_rowid_col]][1] %||% "")
    if (nzchar(new_row_id)) {
      remember_snapshot_fit_bounds_for_row(
        row_id = new_row_id,
        row_df = new_row,
        bounds = fit_param_bounds()
      )
    }
    values$param_active_row_id <- as.character(new_row[[snapshot_rowid_col]][1] %||% NA_character_)
    updateTextInput(session, "snap_name", value = "") 
  })

  # --- [新增] 删除选中快照 (Modal 确认) ---
  observeEvent(input$delete_selected_params, {
    # [修复] 防止在语言切换时触发
    if(lang_switching()) return()
    checked_ids <- as.character(values$param_checked_ids %||% character(0))
    if (length(checked_ids) == 0) {
      showNotification(tr("snapshot_delete_none", lang()), type = "warning")
      return()
    }
    showModal(modalDialog(
      title = tr("snapshot_delete_confirm_title", lang()),
      tr("snapshot_delete_confirm_msg", lang()),
      footer = tagList(
        modalButton(tr("snapshot_delete_confirm_cancel", lang())),
        actionButton("confirm_delete_params", tr("snapshot_delete_confirm_ok", lang()), class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_delete_params, {
    df <- normalize_snapshot_table(values$param_list, add_row_id = TRUE)
    checked_ids <- as.character(values$param_checked_ids %||% character(0))
    if (nrow(df) > 0 && length(checked_ids) > 0) {
      df <- df[!df[[snapshot_rowid_col]] %in% checked_ids, , drop = FALSE]
    }
    values$param_list <- df
    current_bounds <- if (is.list(values$snapshot_fit_bounds_by_row_id)) values$snapshot_fit_bounds_by_row_id else list()
    if (length(checked_ids) > 0L && length(current_bounds) > 0L) {
      current_bounds[checked_ids] <- NULL
      values$snapshot_fit_bounds_by_row_id <- current_bounds
    }
    values$param_checked_ids <- character(0)
    if (nrow(df) == 0) values$param_active_row_id <- NA_character_
    tryCatch(DT::selectRows(DT::dataTableProxy("param_table"), NULL), error = function(e) NULL)
    removeModal()
    showNotification(tr("snapshot_delete_success", lang()), type = "message")
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
    for (idx in 1:7) {
      updateSliderInput(session, paste0("logK", idx), value = DEFAULT_PARAMS$logK)
      updateSliderInput(session, paste0("H", idx), value = DEFAULT_PARAMS$H)
    }
    
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

  # --- 2. 渲染表格（支持勾选、全选） ---
  output$param_table <- renderDT({
    df <- normalize_snapshot_table(values$param_list, add_row_id = FALSE)

    if (is.null(df) || nrow(df) == 0) {
      return(datatable(
        data.frame(),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }

    checked_ids <- as.character(values$param_checked_ids %||% character(0))
    active_id <- as.character(values$param_active_row_id %||% "")[1]
    if (is.na(active_id) || !nzchar(active_id)) active_id <- ""
    active_id_js <- gsub("\\\\", "\\\\\\\\", active_id)
    active_id_js <- gsub("'", "\\\\'", active_id_js, fixed = TRUE)
    select_col <- vapply(df[[snapshot_rowid_col]], function(id) {
      is_checked <- id %in% checked_ids
      sprintf(
        "<input type='checkbox' class='snap-select-row' data-row-id='%s' %s/>",
        htmltools::htmlEscape(id),
        if (is_checked) "checked='checked'" else ""
      )
    }, character(1))

    display_df <- df
    display_df[[".select"]] <- select_col
    display_df <- display_df[, c(".select", snapshot_expected_cols, snapshot_rowid_col), drop = FALSE]
    select_all_title <- htmltools::htmlEscape(tr("snapshot_select_all", lang()))
    colnames(display_df)[1] <- sprintf("<input type='checkbox' class='snap-select-all' title='%s'/>", select_all_title)

    # DataTables column index is 0-based.
    rowid_col_idx <- ncol(display_df) - 1L
    dt_callback <- JS(sprintf(
      "var rowIdCol = %d;
       var activeRowId = '%s';
       var nsSelect = '.snapSelect';
       var nsRow = '.snapRow';
       var nsHead = '.snapHead';
       var nsDraw = '.snapDraw';
       var $container = $(table.table().container());
       var $body = $(table.table().body());
       var $header = $(table.table().header());
       function syncSelectAll() {
         var allRows = table.$('input.snap-select-row');
         var checkedRows = table.$('input.snap-select-row:checked');
         var allBox = $header.find('input.snap-select-all');
         if (!allBox.length) return;
         if (allRows.length === 0) {
           allBox.prop('checked', false);
           allBox.prop('indeterminate', false);
           return;
         }
         allBox.prop('checked', checkedRows.length === allRows.length);
         allBox.prop('indeterminate', checkedRows.length > 0 && checkedRows.length < allRows.length);
       }
       function emitChecked() {
         var ids = [];
         table.$('input.snap-select-row:checked').each(function() {
           ids.push($(this).attr('data-row-id'));
         });
         Shiny.setInputValue(
           'param_table_checked_payload',
           {row_ids: ids, nonce: Date.now()},
           {priority: 'event'}
         );
         syncSelectAll();
       }
       function emitRowClick(trNode) {
         var rowApi = table.row(trNode);
         var rowIndex = rowApi.index();
         if (rowIndex === undefined || rowIndex === null) return;
         var rowData = rowApi.data();
         if (!rowData || rowData.length <= rowIdCol) return;
         var rowId = String(rowData[rowIdCol] || '');
         activeRowId = rowId;
         Shiny.setInputValue(
           'param_table_row_activate',
           {row_id: rowId, row_index: rowIndex + 1, nonce: Date.now()},
           {priority: 'event'}
         );
         markActiveById(activeRowId);
       }
       function markActiveById(id) {
         table.rows().every(function() {
           var rowData = this.data();
           var rowId = (rowData && rowData.length > rowIdCol) ? String(rowData[rowIdCol] || '') : '';
           $(this.node()).toggleClass('snap-row-active', !!id && rowId === id);
         });
       }
       $container.off(nsSelect);
       $body.off(nsRow);
       $header.off(nsHead);
       table.off('draw.dt' + nsDraw);
       $container.on('mousedown' + nsSelect, 'input.snap-select-row', function(e) {
         e.stopPropagation();
       });
       $container.on('click' + nsSelect, 'input.snap-select-row', function(e) {
         e.stopPropagation();
         e.stopImmediatePropagation();
       });
       $container.on('change' + nsSelect, 'input.snap-select-row', function(e) {
         e.stopPropagation();
         emitChecked();
       });
       $body.on('click' + nsRow, 'tr', function(e) {
         if ($(e.target).closest('input.snap-select-row').length) return;
         emitRowClick(this);
       });
       $header.on('click' + nsHead, 'input.snap-select-all', function(e) {
         e.stopPropagation();
         var checked = $(this).prop('checked');
         table.$('input.snap-select-row').prop('checked', checked);
         emitChecked();
       });
       table.on('draw.dt' + nsDraw, function() {
         syncSelectAll();
         markActiveById(activeRowId);
       });
       syncSelectAll();
       markActiveById(activeRowId);",
      rowid_col_idx,
      active_id_js
    ))

    datatable(
      display_df,
      escape = FALSE,
      rownames = FALSE,
      selection = "none",
      options = list(
        dom = "t",
        paging = FALSE,
        scrollX = TRUE,
        scrollY = "120px",
        scrollCollapse = FALSE,
        searching = FALSE,
        ordering = FALSE,
        columnDefs = list(
          list(orderable = FALSE, targets = c(0, rowid_col_idx)),
          list(className = "dt-center", targets = 0),
          list(visible = FALSE, targets = rowid_col_idx)
        )
      ),
      callback = dt_callback
    )
  }, server = FALSE)

  observeEvent(input$param_table_checked_payload, {
    payload <- input$param_table_checked_payload
    checked_in <- unique(as.character(payload$row_ids %||% character(0)))
    df <- normalize_snapshot_table(values$param_list, add_row_id = FALSE)
    valid_ids <- as.character(df[[snapshot_rowid_col]] %||% character(0))
    values$param_checked_ids <- checked_in[checked_in %in% valid_ids]
  }, ignoreInit = TRUE)

  # --- 3. 点击表格加载参数 ---
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
  
  observeEvent(input$param_table_row_activate, {
    clicked_id <- as.character(input$param_table_row_activate$row_id %||% "")[1]
    if (is.na(clicked_id) || !nzchar(clicked_id)) return()
    values$param_active_row_id <- clicked_id
    df_display <- normalize_snapshot_table(values$param_list, add_row_id = FALSE)
    if (nrow(df_display) == 0 || !snapshot_rowid_col %in% names(df_display)) return()
    selected_idx <- match(clicked_id, as.character(df_display[[snapshot_rowid_col]]))
    if (!is.finite(selected_idx) || is.na(selected_idx) || selected_idx < 1L) return()
    target_p <- df_display[selected_idx, , drop = FALSE]
    target_bounds <- remember_snapshot_fit_bounds_for_row(clicked_id, row_df = target_p)
    if (length(target_bounds) > 0L) {
      apply_fit_bounds_state(target_bounds, update_ui_inputs = TRUE)
    }
    
    # 3. 批量更新 UI (兼容性处理：列不存在或为NA/空时使用默认值)
    # 基础参数 (logK1, H1)
    updateSliderInput(session, "logK1", value = snap_param_value(target_p$logK1, DEFAULT_PARAMS$logK))
    updateSliderInput(session, "H1", value = snap_param_value(target_p$H1, DEFAULT_PARAMS$H))
    
    # 可选参数 (logK2–H7)：读到 NA 则自动用默认值
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
    updateSliderInput(session, "logK7", value = snap_param_value(target_p$logK7, DEFAULT_PARAMS$logK))
    updateSliderInput(session, "H7", value = snap_param_value(target_p$H7, DEFAULT_PARAMS$H))
    
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

    # [新增] 恢复拟合区间（先更新到 1~max，再恢复 start/end；非法则告警且保持当前值）
    fit_range_start_saved <- snap_param_value(target_p$FitRangeStart, NA_real_)
    fit_range_end_saved <- snap_param_value(target_p$FitRangeEnd, NA_real_)
    fit_range_n_inj_saved <- snap_param_value(target_p$n_inj, NA_real_)
    if (
      is.finite(fit_range_start_saved) &&
      is.finite(fit_range_end_saved) &&
      exists("apply_saved_fit_data_range", mode = "function", inherits = TRUE)
    ) {
      apply_saved_fit_data_range(
        saved_start = fit_range_start_saved,
        saved_end = fit_range_end_saved,
        saved_n_inj = fit_range_n_inj_saved,
        error_key = "snapshot_fit_range_restore_invalid",
        preferred_max = fit_range_n_inj_saved
      )
    }
    
    # 4. [新增] 自动解析并勾选模型 (Model)
    # Model 格式示例: "rxn_M+rxn_D+rxn_T"
    if ("Model" %in% names(target_p) && !is.na(target_p$Model) && nzchar(as.character(target_p$Model))) {
      tryCatch({
        saved_model_str <- as.character(target_p$Model)
        # 分割字符串 ("+" 分隔)
        saved_paths <- strsplit(saved_model_str, "\\+")[[1]]
        
        # 定义有效路径 (必须与 checkboxGroupInput 的 values 对应)
        valid_paths <- c("rxn_D", "rxn_T", "rxn_E", "rxn_B", "rxn_F", "rxn_U")
        
        # 筛选出有效路径 (rxn_M 是基础反应，不在 checkbox 中，忽略即可)
        paths_to_select <- intersect(saved_paths, valid_paths)
        if (exists("normalize_active_paths_with_dependencies", mode = "function", inherits = TRUE)) {
          paths_to_select <- normalize_active_paths_with_dependencies(paths_to_select, valid_paths = valid_paths)
        }
        
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
      import_obj <- input$import_params_file
      if (is.data.frame(import_obj)) {
        import_obj <- as.list(import_obj[1, , drop = FALSE])
      }
      datapath <- as.character(import_obj$datapath %||% "")[1]
      if (!nzchar(datapath) || !file.exists(datapath)) {
        showNotification(tr("import_params_error_format", lang()), type = "error")
        return()
      }
      sheets <- tryCatch(read_xlsx_sheets(datapath), error = function(e) NULL)
      if (!is.list(sheets) || length(sheets) < 1L) {
        showNotification(tr("import_params_error_format", lang()), type = "error")
        return()
      }
      imported_df <- if (is.data.frame(sheets[["snapshots"]])) {
        as.data.frame(sheets[["snapshots"]], stringsAsFactors = FALSE)
      } else {
        first_sheet <- sheets[[1]]
        if (!is.data.frame(first_sheet)) NULL else as.data.frame(first_sheet, stringsAsFactors = FALSE)
      }
      if (!is.data.frame(imported_df)) {
        showNotification(tr("import_params_error_format", lang()), type = "error")
        return()
      }
      imported_fit_bounds_raw <- extract_snapshot_fit_bounds_map(
        sheets[[snapshot_fit_bounds_sheet_name]],
        row_id_col = snapshot_export_rowid_col
      )
      
      # 将文件列名（带单位后缀）还原为内部列名
      file_to_internal <- c(
        "H_cell_0_mM" = "H_cell_0", "G_syringe_mM" = "G_syringe",
        "V_cell_mL" = "V_cell", "V_inj_uL" = "V_inj", "V_pre_uL" = "V_pre",
        "Temp_K" = "Temp",
        "H1_cal_mol" = "H1", "H2_cal_mol" = "H2", "H3_cal_mol" = "H3",
        "H4_cal_mol" = "H4", "H5_cal_mol" = "H5", "H6_cal_mol" = "H6", "H7_cal_mol" = "H7",
        "V_init_uL" = "V_init", "Offset_cal" = "Offset"
      )
      for (file_name in names(file_to_internal)) {
        if (file_name %in% colnames(imported_df)) {
          colnames(imported_df)[colnames(imported_df) == file_name] <- file_to_internal[[file_name]]
        }
      }
      if (snapshot_export_rowid_col %in% colnames(imported_df) && !snapshot_rowid_col %in% colnames(imported_df)) {
        colnames(imported_df)[colnames(imported_df) == snapshot_export_rowid_col] <- snapshot_rowid_col
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
        "logK7" = 7.0, "H7" = -6000,
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
      
      valid_cols <- intersect(colnames(imported_df), c(snapshot_expected_cols, snapshot_rowid_col))
      if(length(valid_cols) == 0) {
        showNotification(tr("import_params_error_format", lang()), type = "error")
        return()
      }

      imported_df <- imported_df[, valid_cols, drop = FALSE]
      imported_old_ids <- if (snapshot_rowid_col %in% names(imported_df)) {
        trimws(as.character(imported_df[[snapshot_rowid_col]]))
      } else {
        rep("", nrow(imported_df))
      }
      imported_df <- normalize_snapshot_table(imported_df, add_row_id = TRUE)
      existing_df <- normalize_snapshot_table(values$param_list, add_row_id = TRUE)
      existing_ids <- if (snapshot_rowid_col %in% names(existing_df)) as.character(existing_df[[snapshot_rowid_col]]) else character(0)
      imported_df <- reassign_snapshot_row_ids_against_existing(imported_df, existing_ids = existing_ids)
      imported_new_ids <- if (snapshot_rowid_col %in% names(imported_df)) {
        as.character(imported_df[[snapshot_rowid_col]])
      } else {
        character(0)
      }
      imported_fit_bounds <- remap_snapshot_fit_bounds_by_row_id(
        bounds_map = imported_fit_bounds_raw,
        old_ids = imported_old_ids,
        new_ids = imported_new_ids,
        rows_df = imported_df
      )
      values$param_list <- bind_rows(existing_df, imported_df)
      current_map <- normalize_snapshot_fit_bounds_by_row_id(
        values$snapshot_fit_bounds_by_row_id,
        rows_df = existing_df,
        derive_missing = FALSE
      )
      for (row_id in names(imported_fit_bounds)) {
        current_map[[row_id]] <- imported_fit_bounds[[row_id]]
      }
      values$snapshot_fit_bounds_by_row_id <- current_map
      values$param_checked_ids <- character(0)
      
      success_tpl <- tr("import_params_success_count", lang())
      success_msg <- gsub("\\{count\\}", as.character(nrow(imported_df)), success_tpl)
      showNotification(success_msg, type = "message")
      
    }, error = function(e) {
      showNotification(paste(tr("import_params_error", lang()), e$message), type = "error")
    })
  })
  
