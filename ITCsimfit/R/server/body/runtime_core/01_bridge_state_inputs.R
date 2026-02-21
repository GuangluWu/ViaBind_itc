  # ============================================================================
  # 原有代码继续
  # ============================================================================
  # [COMMENT_STD][MODULE_HEADER]
  # 模块职责：维护 Step2 运行态、桥接输入同步与拟合前状态归一。
  # 依赖：shiny reactive API、constants.R、bridge helper 函数族。
  # 对外接口：通过当前 server 环境暴露 reset/apply/publish 等内部函数。
  # 副作用：更新 UI 控件值、写入 reactiveValues、触发通知与桥接通道。
  # 变更历史：2026-02-12 - 增加 Phase 4 注释规范样板。
  
  # 使用同步拟合模式：拟合时界面会暂时无响应，确保用户不会在拟合时进行其他操作
  
  # [COMMENT_STD][IO_CONTRACT]
  # 输入来源：Shiny input/session、Step1 bridge payload、缓存 reactive 状态。
  # 字段/类型：input[[id]] 以 scalar/list 为主；payload 为 list 且含 schema/token/meta/integration。
  # 单位：体积统一按 uL，浓度按 mM，温度按 K（与 constants/UI 约定一致）。
  # 空值策略：safe_input_get/safe_rv_get 吞掉读取异常并返回 NULL/默认值。
  # 输出保证：状态更新函数返回 invisible(TRUE/FALSE) 或结构化 list，避免抛出未处理异常。
  values <- reactiveValues(is_fitting = FALSE, param_list = data.frame(), error_analysis = NULL, 
                           param_checked_ids = character(0), # snapshot table checked row ids
                           snapshot_row_seq = 0L,            # monotonic sequence for snapshot row_id
                           param_active_row_id = NA_character_, # highlighted snapshot row id
                           error_analysis_info = NULL,  # 存储误差分析可靠性信息
                           residuals_data = NULL,       # 存储残差数据（用于残差图）
                           correlation_matrix = NULL,   # 存储参数相关性矩阵
                           residual_subtab = "res1",    # 当前选中的残差子标签页
                           current_report = NULL,       # 当前报告文本（用于导出）
                           manual_exp_data = NULL,      # 手动导入的实验数据（模拟→实验功能）
                           manual_exp_source = NULL,    # 手动实验数据来源: sim_to_exp / step1_bridge
                           exp_data_disabled = FALSE,   # TRUE 时忽略所有实验数据输入，仅保留模拟模式
                           imported_xlsx_sheets = NULL, # 缓存 ITCprocessor 导出的原始 xlsx sheets
                           imported_xlsx_base_name = NULL,  # 导入文件的基名（去掉时间戳），用于导出命名
                           imported_xlsx_filename = NULL,  # 导入文件的显示用文件名（供界面展示）
                           v_pre_programmatic_update = FALSE)  # 程序更新 V_pre 时设为 TRUE，避免显示"第一针体积变更"提示
  
  # [缓存机制] 使用 reactiveVal 存储缓存的结果和键
  sim_cache_result <- reactiveVal(NULL)
  sim_cache_key <- reactiveVal(NULL)
  fit_slider_force_default <- reactiveVal(FALSE)
  path_selection_programmatic <- reactiveVal(FALSE)
  path_selection_last <- reactiveVal(character(0))

  normalize_active_paths_safe <- function(paths) {
    tryCatch(
      normalize_active_paths_with_dependencies(paths),
      error = function(e) {
        raw <- if (is.null(paths)) character(0) else as.character(paths)
        valid <- c("rxn_D", "rxn_T", "rxn_B", "rxn_F", "rxn_U")
        valid[valid %in% unique(raw)]
      }
    )
  }

  update_active_paths_normalized <- function(paths) {
    normalized <- normalize_active_paths_safe(paths)
    path_selection_last(normalized)
    current_raw <- safe_input_get("active_paths")
    current_raw <- if (is.null(current_raw)) character(0) else as.character(current_raw)
    current_raw <- trimws(current_raw)
    current_raw <- unique(current_raw[nzchar(current_raw)])
    if (setequal(current_raw, normalized) && length(current_raw) == length(normalized)) {
      return(invisible(FALSE))
    }
    path_selection_programmatic(TRUE)
    updateCheckboxGroupInput(session, "active_paths", selected = normalized)
    session$onFlushed(function() {
      path_selection_programmatic(FALSE)
    }, once = TRUE)
    invisible(TRUE)
  }

  # ============================================================================
  # 会话级拟合参数边界（仅本次会话生效）
  # ============================================================================
  FIT_BOUND_PARAM_IDS <- c(
    "logK1", "H1",
    "logK2", "H2",
    "logK3", "H3",
    "logK4", "H4",
    "logK5", "H5",
    "logK6", "H6",
    "Offset"
  )

  FIT_BOUND_STEPS <- c(
    logK1 = 0.001, H1 = 100,
    logK2 = 0.001, H2 = 100,
    logK3 = 0.001, H3 = 100,
    logK4 = 0.001, H4 = 100,
    logK5 = 0.001, H5 = 100,
    logK6 = 0.001, H6 = 100,
    Offset = 10
  )

  build_default_fit_param_bounds <- function() {
    out <- vector("list", length(FIT_BOUND_PARAM_IDS))
    names(out) <- FIT_BOUND_PARAM_IDS
    for (nm in FIT_BOUND_PARAM_IDS) {
      b <- get_param_bound(nm)
      out[[nm]] <- c(lower = as.numeric(b["lower"]), upper = as.numeric(b["upper"]))
    }
    out
  }

  fit_param_bounds <- reactiveVal(build_default_fit_param_bounds())

  get_fit_bound_param_ids_for_paths <- function(paths = NULL) {
    paths_safe <- if (is.null(paths) || length(paths) == 0) character(0) else as.character(paths)
    ids <- c("Offset", "logK1", "H1")
    if ("rxn_D" %in% paths_safe) ids <- c(ids, "logK2", "H2")
    if ("rxn_T" %in% paths_safe) ids <- c(ids, "logK3", "H3")
    if ("rxn_B" %in% paths_safe) ids <- c(ids, "logK4", "H4")
    if ("rxn_F" %in% paths_safe) ids <- c(ids, "logK5", "H5")
    if ("rxn_U" %in% paths_safe) ids <- c(ids, "logK6", "H6")
    unique(ids)
  }

  get_effective_param_bound <- function(param_name, v_inj = NULL) {
    get_param_bound(
      param_name = param_name,
      v_inj = v_inj,
      override_bounds = fit_param_bounds()
    )
  }

  get_effective_parameter_bounds <- function(param_names, v_inj = NULL) {
    get_parameter_bounds(
      param_names = param_names,
      v_inj = v_inj,
      override_bounds = fit_param_bounds()
    )
  }

  get_fit_bound_for_ui <- function(param_name) {
    b <- get_effective_param_bound(param_name)
    c(lower = as.numeric(b["lower"]), upper = as.numeric(b["upper"]))
  }

  sanitize_fit_bound_pair <- function(lower_in, upper_in, old_lower, old_upper, step = 1) {
    lower_num <- suppressWarnings(as.numeric(lower_in)[1])
    upper_num <- suppressWarnings(as.numeric(upper_in)[1])
    if (!is.finite(lower_num)) lower_num <- old_lower
    if (!is.finite(upper_num)) upper_num <- old_upper

    if (!is.finite(lower_num)) lower_num <- 0
    if (!is.finite(upper_num)) upper_num <- lower_num + step

    if (lower_num > upper_num) {
      tmp <- lower_num
      lower_num <- upper_num
      upper_num <- tmp
    }

    min_span <- suppressWarnings(as.numeric(step)[1])
    if (!is.finite(min_span) || min_span <= 0) min_span <- 1
    if (abs(upper_num - lower_num) < .Machine$double.eps^0.5) {
      upper_num <- lower_num + min_span
    }

    c(lower = lower_num, upper = upper_num)
  }

  clamp_to_bound <- function(value, bound, default) {
    safe_numeric(
      value,
      default = default,
      min = suppressWarnings(as.numeric(bound["lower"])[1]),
      max = suppressWarnings(as.numeric(bound["upper"])[1])
    )
  }

  get_slider_id_for_bound_param <- function(param_name) {
    if (identical(param_name, "Offset")) "heat_offset" else param_name
  }

  get_default_value_for_bound_param <- function(param_name) {
    if (grepl("^logK", param_name)) return(DEFAULT_PARAMS$logK)
    if (grepl("^H[0-9]+$", param_name)) return(DEFAULT_PARAMS$H)
    if (identical(param_name, "Offset")) return(DEFAULT_PARAMS$Offset)
    0
  }

  apply_bound_to_slider <- function(param_name, bound) {
    slider_id <- get_slider_id_for_bound_param(param_name)
    current_val <- safe_input_get(slider_id)
    default_val <- get_default_value_for_bound_param(param_name)
    clamped_val <- clamp_to_bound(current_val, bound, default = default_val)
    min_num <- suppressWarnings(as.numeric(bound["lower"])[1])
    max_num <- suppressWarnings(as.numeric(bound["upper"])[1])
    val_num <- suppressWarnings(as.numeric(clamped_val)[1])
    if (!is.finite(min_num)) min_num <- default_val
    if (!is.finite(max_num)) max_num <- min_num + 1
    if (!is.finite(val_num)) val_num <- default_val
    updateSliderInput(
      session,
      slider_id,
      min = min_num,
      max = max_num,
      value = val_num
    )
    invisible(TRUE)
  }

  apply_all_fit_bounds_to_sliders <- function() {
    bounds_now <- fit_param_bounds()
    for (nm in FIT_BOUND_PARAM_IDS) {
      b <- bounds_now[[nm]]
      if (is.null(b)) next
      apply_bound_to_slider(nm, b)
    }
    invisible(TRUE)
  }

  update_fit_param_bound <- function(param_name, lower_in, upper_in, update_ui_inputs = TRUE) {
    current_bounds <- fit_param_bounds()
    old_bound <- current_bounds[[param_name]]
    if (is.null(old_bound)) old_bound <- get_param_bound(param_name)

    step_val <- FIT_BOUND_STEPS[[param_name]]
    if (!is.finite(step_val)) step_val <- 1

    sanitized <- sanitize_fit_bound_pair(
      lower_in = lower_in,
      upper_in = upper_in,
      old_lower = as.numeric(old_bound["lower"]),
      old_upper = as.numeric(old_bound["upper"]),
      step = step_val
    )
    current_bounds[[param_name]] <- sanitized
    fit_param_bounds(current_bounds)

    if (isTRUE(update_ui_inputs)) {
      min_id <- paste0("bound_", param_name, "_min")
      max_id <- paste0("bound_", param_name, "_max")
      cur_min <- suppressWarnings(as.numeric(safe_input_get(min_id))[1])
      cur_max <- suppressWarnings(as.numeric(safe_input_get(max_id))[1])
      lower_num <- suppressWarnings(as.numeric(sanitized["lower"])[1])
      upper_num <- suppressWarnings(as.numeric(sanitized["upper"])[1])
      if (!is.finite(cur_min) || !isTRUE(all.equal(cur_min, lower_num, tolerance = 1e-10))) {
        updateNumericInput(session, min_id, value = lower_num)
      }
      if (!is.finite(cur_max) || !isTRUE(all.equal(cur_max, upper_num, tolerance = 1e-10))) {
        updateNumericInput(session, max_id, value = upper_num)
      }
    }

    apply_bound_to_slider(param_name, sanitized)
    invisible(TRUE)
  }

  session_home <- tryCatch({
    h <- session$userData$itcsuite_home
    if (is.null(h) || !is.list(h)) NULL else h
  }, error = function(e) NULL)

  home_add_recent <- function(record, payload = NULL) {
    fn <- if (!is.null(session_home)) session_home$add_recent_import else NULL
    if (is.function(fn)) fn(record, payload)
    invisible(NULL)
  }

  home_add_recent_export <- function(record, payload = NULL) {
    fn <- if (!is.null(session_home)) session_home$add_recent_export else NULL
    if (is.function(fn)) fn(record, payload)
    invisible(NULL)
  }

  home_register_restore <- function(step, handler) {
    fn <- if (!is.null(session_home)) session_home$register_restore_handler else NULL
    if (is.function(fn)) return(invisible(isTRUE(fn(step, handler))))
    invisible(FALSE)
  }

  is_finite_scalar <- function(x) length(x) == 1 && is.finite(x)

  payload_token <- function(payload) {
    token_vec <- suppressWarnings(as.numeric(payload$token))
    if (length(token_vec) >= 1) token_vec[1] else NA_real_
  }

  payload_signature <- function(payload) {
    build_step1_bridge_signature(payload)
  }

  default_step1_signature_state <- function() {
    list(consumed = NA_character_, reset = NA_character_, input = NA_character_)
  }

  get_step1_signature_state <- function() {
    st <- safe_rv_get(bridge_last_step1_signature, default = NULL)
    if (!is.list(st)) return(default_step1_signature_state())
    if (is.null(st$consumed)) st$consumed <- NA_character_
    if (is.null(st$reset)) st$reset <- NA_character_
    if (is.null(st$input)) st$input <- NA_character_
    st
  }

  set_step1_signature_state <- function(st) {
    if (!is.list(st)) st <- default_step1_signature_state()
    bridge_last_step1_signature(st)
    invisible(TRUE)
  }

  safe_input_get <- function(id) {
    tryCatch(shiny::isolate(input[[id]]), error = function(e) NULL)
  }

  safe_rv_get <- function(rv_fun, default = NULL) {
    tryCatch(shiny::isolate(rv_fun()), error = function(e) default)
  }

  # Programmatic V_pre update helper:
  # - only sets suppression flag when V_pre value actually changes
  # - returns TRUE when target value is already synced or updated successfully
  update_v_pre_programmatically <- function(value) {
    current_vpre <- safe_input_get("V_pre")
    if (is.null(current_vpre)) return(invisible(FALSE))

    target_num <- suppressWarnings(as.numeric(value))
    if (length(target_num) < 1 || !is.finite(target_num[1])) return(invisible(FALSE))
    target_num <- target_num[1]

    current_num <- suppressWarnings(as.numeric(current_vpre))
    if (length(current_num) >= 1 && is.finite(current_num[1])) {
      if (isTRUE(all.equal(current_num[1], target_num, tolerance = 1e-8))) {
        return(invisible(TRUE))
      }
    }

    values$v_pre_programmatic_update <- TRUE
    updateNumericInput(session, "V_pre", value = target_num)
    invisible(TRUE)
  }

  # Always apply first injection in deterministic order: V_pre first, then V_init <- V_pre.
  apply_first_injection_pair <- function(v_pre_target, retries = 3L, delay_sec = 0.05) {
    target_num <- suppressWarnings(as.numeric(v_pre_target))
    if (length(target_num) < 1 || !is.finite(target_num[1])) return(invisible(FALSE))
    target_num <- target_num[1]

    vpre_synced <- isTRUE(update_v_pre_programmatically(target_num))
    if (isTRUE(vpre_synced) && !is.null(safe_input_get("V_init_val"))) {
      updateNumericInput(session, "V_init_val", value = target_num)
      return(invisible(TRUE))
    }

    retries_n <- suppressWarnings(as.integer(retries)[1])
    if (!is.finite(retries_n) || retries_n <= 0L) return(invisible(FALSE))
    if (!requireNamespace("later", quietly = TRUE)) return(invisible(FALSE))

    later::later(function() {
      tryCatch(
        apply_first_injection_pair(target_num, retries = retries_n - 1L, delay_sec = delay_sec),
        error = function(e) NULL
      )
    }, delay = delay_sec)
    invisible(FALSE)
  }

  # Import flow helper:
  # - V_pre and V_init can be restored from different sources/values
  # - V_pre uses programmatic helper so manual warning is suppressed
  apply_import_first_injection_targets <- function(v_pre_target,
                                                   v_init_target,
                                                   retries = 3L,
                                                   delay_sec = 0.05) {
    pre_num <- suppressWarnings(as.numeric(v_pre_target))
    init_num <- suppressWarnings(as.numeric(v_init_target))
    if (length(pre_num) < 1 || !is.finite(pre_num[1])) return(invisible(FALSE))
    if (length(init_num) < 1 || !is.finite(init_num[1])) return(invisible(FALSE))
    pre_num <- pre_num[1]
    init_num <- init_num[1]

    vpre_synced <- isTRUE(update_v_pre_programmatically(pre_num))
    if (isTRUE(vpre_synced) && !is.null(safe_input_get("V_init_val"))) {
      updateNumericInput(session, "V_init_val", value = init_num)
      return(invisible(TRUE))
    }

    retries_n <- suppressWarnings(as.integer(retries)[1])
    if (!is.finite(retries_n) || retries_n <= 0L) return(invisible(FALSE))
    if (!requireNamespace("later", quietly = TRUE)) return(invisible(FALSE))

    later::later(function() {
      tryCatch(
        apply_import_first_injection_targets(
          pre_num,
          init_num,
          retries = retries_n - 1L,
          delay_sec = delay_sec
        ),
        error = function(e) NULL
      )
    }, delay = delay_sec)
    invisible(FALSE)
  }

  is_step2_tab_selected <- function(tab_value = safe_input_get("main_tabs")) {
    tab_chr <- as.character(tab_value %||% "")[1]
    tab_chr %in% c(
      "step2",
      "Step 2 Simulation & Fitting",
      "Step 2 模拟 & 拟合",
      "Step 2 模拟与拟合"
    )
  }

  update_numeric_if_present <- function(id, value) {
    if (is.null(safe_input_get(id))) return(invisible(FALSE))
    value_num <- suppressWarnings(as.numeric(value))
    if (length(value_num) < 1 || !is.finite(value_num[1])) return(invisible(FALSE))
    updateNumericInput(session, id, value = value_num[1])
    invisible(TRUE)
  }

  update_checkbox_if_present <- function(id, value) {
    if (is.null(safe_input_get(id))) return(invisible(FALSE))
    updateCheckboxInput(session, id, value = isTRUE(value))
    invisible(TRUE)
  }

  # 边界编辑器输入联动：用户修改 min/max 后立即生效到滑条与会话边界
  for (param_name in FIT_BOUND_PARAM_IDS) {
    local({
      nm <- param_name
      min_id <- paste0("bound_", nm, "_min")
      max_id <- paste0("bound_", nm, "_max")
      observeEvent(
        {
          input[[min_id]]
          input[[max_id]]
        },
        {
          update_fit_param_bound(
            param_name = nm,
            lower_in = input[[min_id]],
            upper_in = input[[max_id]],
            update_ui_inputs = TRUE
          )
        },
        ignoreInit = TRUE
      )
    })
  }

  observeEvent(input$reset_fit_bounds, {
    default_bounds <- build_default_fit_param_bounds()
    fit_param_bounds(default_bounds)

    for (nm in FIT_BOUND_PARAM_IDS) {
      b <- default_bounds[[nm]]
      updateNumericInput(session, paste0("bound_", nm, "_min"), value = as.numeric(b["lower"])[1])
      updateNumericInput(session, paste0("bound_", nm, "_max"), value = as.numeric(b["upper"])[1])
    }
    apply_all_fit_bounds_to_sliders()
    showNotification(tr("fit_bounds_reset_done", lang()), type = "message", duration = 3)
  }, ignoreInit = TRUE)

  reset_step2_ui_for_new_dataset <- function(default_n_inj = NULL, apply_first_injection_default = TRUE) {
    update_active_paths_normalized(character(0))
    updateCheckboxGroupInput(session, "fit_params", selected = c("logK1", "H1"))

    updateSliderInput(session, "logK1", value = DEFAULT_PARAMS$logK)
    updateSliderInput(session, "H1", value = DEFAULT_PARAMS$H)
    updateSliderInput(session, "logK2", value = DEFAULT_PARAMS$logK)
    updateSliderInput(session, "H2", value = DEFAULT_PARAMS$H)
    updateSliderInput(session, "logK3", value = DEFAULT_PARAMS$logK)
    updateSliderInput(session, "H3", value = DEFAULT_PARAMS$H)
    updateSliderInput(session, "logK4", value = DEFAULT_PARAMS$logK)
    updateSliderInput(session, "H4", value = DEFAULT_PARAMS$H)
    updateSliderInput(session, "logK5", value = DEFAULT_PARAMS$logK)
    updateSliderInput(session, "H5", value = DEFAULT_PARAMS$H)
    updateSliderInput(session, "logK6", value = DEFAULT_PARAMS$logK)
    updateSliderInput(session, "H6", value = DEFAULT_PARAMS$H)

    updateNumericInput(session, "factor_H", value = DEFAULT_PARAMS$fH)
    updateNumericInput(session, "factor_G", value = DEFAULT_PARAMS$fG)
    updateSliderInput(session, "heat_offset", value = DEFAULT_PARAMS$Offset)
    update_checkbox_if_present("enable_error_analysis", FALSE)
    update_checkbox_if_present("use_weighted_fitting", FALSE)
    update_checkbox_if_present("use_robust_fitting", FALSE)

    default_first <- if (!is.null(UI_DEFAULTS$v_pre_default)) UI_DEFAULTS$v_pre_default else DEFAULT_PARAMS$V_init
    if (isTRUE(apply_first_injection_default)) {
      apply_first_injection_pair(default_first)
    }

    n_inj_num <- suppressWarnings(as.integer(default_n_inj)[1])
    if (!is.finite(n_inj_num) || n_inj_num < 1L) {
      n_inj_num <- suppressWarnings(as.integer(input$n_inj)[1])
    }
    if (!is.finite(n_inj_num) || n_inj_num < 1L) n_inj_num <- UI_DEFAULTS$n_inj_default
    left_default <- if (n_inj_num >= 2L) 2L else 1L
    updateSliderInput(
      session, "fit_data_range",
      min = 1,
      max = n_inj_num,
      value = c(left_default, n_inj_num),
      step = 1
    )
    fit_slider_force_default(TRUE)

    values$error_analysis <- NULL
    values$error_analysis_info <- NULL
    values$residuals_data <- NULL
    values$correlation_matrix <- NULL
    values$residual_subtab <- "res1"
    values$current_report <- NULL
    values$param_active_row_id <- NA_character_
    # New dataset should not carry snapshot table row focus.
    tryCatch(DT::selectRows(DT::dataTableProxy("param_table"), NULL), error = function(e) NULL)
    invisible(TRUE)
  }

  observeEvent(input$path_graph_toggle, {
    payload <- input$path_graph_toggle
    path_id <- as.character(payload$path_id %||% "")[1]
    if (!nzchar(path_id)) return()

    current <- normalize_active_paths_safe(input$active_paths)
    toggled <- tryCatch(
      apply_path_graph_toggle_with_dependencies(current, path_id),
      error = function(e) current
    )
    update_active_paths_normalized(toggled)
  }, ignoreInit = TRUE)

  observeEvent(input$active_paths, {
    raw <- if (is.null(input$active_paths)) character(0) else as.character(input$active_paths)

    if (isTRUE(path_selection_programmatic())) {
      path_selection_last(normalize_active_paths_safe(raw))
      return()
    }

    prev <- normalize_active_paths_safe(path_selection_last())
    adjusted <- raw

    # Keep table-mode dependency behavior aligned with graph mode:
    # selecting F auto-enables D; deselecting D auto-disables F.
    if ("rxn_F" %in% adjusted && !"rxn_D" %in% adjusted) {
      if ("rxn_D" %in% prev && "rxn_F" %in% prev) {
        adjusted <- setdiff(adjusted, "rxn_F")
      } else {
        adjusted <- c(adjusted, "rxn_D")
      }
    }

    normalized <- normalize_active_paths_safe(adjusted)
    if (!identical(raw, normalized)) {
      update_active_paths_normalized(normalized)
    } else {
      path_selection_last(normalized)
    }
  }, ignoreInit = TRUE)
  
  apply_meta_to_exp_inputs <- function(meta_df) {
    if (is.null(meta_df) || !all(c("parameter", "value") %in% colnames(meta_df))) return(invisible(FALSE))
    updated_any <- FALSE
    param_vals <- extract_step1_meta_numeric_map(meta_df)
    
    if ("H_cell_0_mM" %in% names(param_vals) && !is.na(param_vals[["H_cell_0_mM"]])) {
      updated_any <- isTRUE(update_numeric_if_present("H_cell_0", param_vals[["H_cell_0_mM"]])) || updated_any
    }
    if ("G_syringe_mM" %in% names(param_vals) && !is.na(param_vals[["G_syringe_mM"]])) {
      updated_any <- isTRUE(update_numeric_if_present("G_syringe", param_vals[["G_syringe_mM"]])) || updated_any
    }
    if ("V_cell_mL" %in% names(param_vals) && !is.na(param_vals[["V_cell_mL"]])) {
      updated_any <- isTRUE(update_numeric_if_present("V_cell", param_vals[["V_cell_mL"]])) || updated_any
    }
    if ("V_inj_uL" %in% names(param_vals) && !is.na(param_vals[["V_inj_uL"]])) {
      updated_any <- isTRUE(update_numeric_if_present("V_inj", param_vals[["V_inj_uL"]])) || updated_any
    }
    if ("V_pre_uL" %in% names(param_vals) && !is.na(param_vals[["V_pre_uL"]])) {
      updated_any <- isTRUE(apply_first_injection_pair(param_vals[["V_pre_uL"]])) || updated_any
    }
    if ("n_inj" %in% names(param_vals) && !is.na(param_vals[["n_inj"]])) {
      updated_any <- isTRUE(update_numeric_if_present("n_inj", as.integer(param_vals[["n_inj"]]))) || updated_any
    }
    if ("Temp_K" %in% names(param_vals) && !is.na(param_vals[["Temp_K"]])) {
      updated_any <- isTRUE(update_numeric_if_present("Temp", param_vals[["Temp_K"]])) || updated_any
    }
    invisible(updated_any)
  }

  apply_step1_inputs_from_payload <- function(payload) {
    if (is.null(payload) || !is.list(payload)) return(list(updated_any = FALSE, vinit_synced = FALSE))

    meta_df <- get_step1_payload_meta_df(payload)
    int_df <- get_step1_payload_integration_df(payload)

    updated_any <- FALSE
    if (!is.null(meta_df)) {
      updated_any <- isTRUE(apply_meta_to_exp_inputs(meta_df)) || updated_any
    }

    # Ensure V_init sync is explicitly verified. If Step 2 has not mounted the
    # V_init control yet, do not mark payload as fully consumed.
    param_vals <- extract_step1_meta_numeric_map(meta_df)

    if (!is.null(int_df) && nrow(int_df) > 0) {
      vinj_vec <- if ("V_titrate_uL" %in% colnames(int_df)) suppressWarnings(as.numeric(int_df$V_titrate_uL)) else NULL
      if (!is.null(vinj_vec)) {
        vinj_vec <- vinj_vec[is.finite(vinj_vec)]
        if (length(vinj_vec) > 0) {
          updated_any <- isTRUE(update_numeric_if_present("V_inj", as.numeric(stats::median(vinj_vec)))) || updated_any
        }
      }

      updated_any <- isTRUE(update_numeric_if_present("n_inj", nrow(int_df))) || updated_any
    }

    default_first <- if (!is.null(UI_DEFAULTS$v_pre_default)) UI_DEFAULTS$v_pre_default else DEFAULT_PARAMS$V_init
    first_target <- resolve_first_injection_targets(
      mode = "step1",
      meta_vals = param_vals,
      int_df = int_df,
      default_v_pre = default_first
    )

    vinit_synced <- FALSE
    if (isTRUE(first_target$has_source) && is.finite(first_target$v_pre_target)) {
      pair_synced <- isTRUE(apply_first_injection_pair(first_target$v_pre_target))
      vinit_synced <- isTRUE(pair_synced)
      updated_any <- pair_synced || updated_any
    } else if (!is.null(safe_input_get("V_init_val"))) {
      # No V_pre source available, but input exists, so don't block consumption.
      vinit_synced <- TRUE
    }

    list(updated_any = isTRUE(updated_any), vinit_synced = isTRUE(vinit_synced))
  }
  
  consume_step1_payload <- function(payload) {
    if (is.null(payload) || !is.list(payload)) return(FALSE)

    token <- payload_token(payload)
    sig <- payload_signature(payload)
    sig_state <- get_step1_signature_state()
    if (!is.na(sig) && identical(sig, sig_state$consumed)) {
      return(FALSE)
    }
    
    meta_df <- get_step1_payload_meta_df(payload)
    int_df <- get_step1_payload_integration_df(payload)
    if (is.null(int_df) || nrow(int_df) == 0) return(FALSE)

    reset_step2_ui_for_new_dataset(
      default_n_inj = nrow(int_df),
      apply_first_injection_default = FALSE
    )
    apply_meta_to_exp_inputs(meta_df)
    
    vinj_default <- suppressWarnings(as.numeric(input$V_inj))
    if (length(vinj_default) >= 1) vinj_default <- vinj_default[1]
    if (!(length(vinj_default) == 1 && is.finite(vinj_default))) vinj_default <- UI_DEFAULTS$v_inj_default * 1000
    
    g_syringe <- suppressWarnings(as.numeric(input$G_syringe))
    if (length(g_syringe) >= 1) g_syringe <- g_syringe[1]
    if (!(length(g_syringe) == 1 && is.finite(g_syringe) && g_syringe > 0)) g_syringe <- UI_DEFAULTS$conc_syringe_default
    
    v_pre_default <- suppressWarnings(as.numeric(input$V_pre))
    if (length(v_pre_default) >= 1) v_pre_default <- v_pre_default[1]
    if (!(length(v_pre_default) == 1 && is.finite(v_pre_default))) v_pre_default <- vinj_default

    exp_df <- build_step1_bridge_exp_df(
      int_df = int_df,
      vinj_default = vinj_default,
      g_syringe = g_syringe,
      v_pre = v_pre_default
    )
    if (is.null(exp_df) || nrow(exp_df) == 0) return(FALSE)
    
    power_df <- get_step1_payload_power_corrected_df(payload)
    power_original_df <- get_step1_payload_power_original_df(payload)
    
    values$manual_exp_data <- exp_df
    values$manual_exp_source <- "step1_bridge"
    values$exp_data_disabled <- FALSE
    
    cached_sheets <- list(integration = int_df)
    if (!is.null(meta_df)) cached_sheets[["meta"]] <- meta_df
    if (!is.null(power_df)) cached_sheets[["power_corrected"]] <- power_df
    if (!is.null(power_original_df)) cached_sheets[["power_original"]] <- power_original_df
    values$imported_xlsx_sheets <- cached_sheets
    
    source_name <- resolve_step1_bridge_source_name(payload, default = "Step1")
    values$imported_xlsx_filename <- source_name
    base_name <- tools::file_path_sans_ext(basename(source_name))
    values$imported_xlsx_base_name <- if (nzchar(base_name)) base_name else "ITC_data"
    token_tag <- resolve_step1_bridge_token_tag(token)
    values$imported_xlsx_file_path <- paste0("bridge://step1/", token_tag)
    
    if (!is.na(sig)) {
      sig_state$consumed <- sig
      set_step1_signature_state(sig_state)
    }
    showNotification(tr("step1_loaded_to_step2", lang()), type = "message", duration = 2)
    TRUE
  }

  latest_step1_payload <- reactiveVal(NULL)
  get_latest_step1_payload <- function() {
    payload <- safe_rv_get(latest_step1_payload)
    if (!is.null(payload)) return(payload)
    bridge_step1_channel <- resolve_bridge_channel("step1_payload")
    payload <- if (is.function(bridge_step1_channel)) bridge_step1_channel() else NULL
    if (!is.null(payload) && is.list(payload)) latest_step1_payload(payload)
    payload
  }

  bridge_step1_channel <- resolve_bridge_channel("step1_payload")
  if (is.function(bridge_step1_channel)) {
    observeEvent(bridge_step1_channel(), {
      payload <- bridge_step1_channel()
      if (is.null(payload) || !is.list(payload)) return(FALSE)
      latest_step1_payload(payload)
      tryCatch(
        consume_step1_payload(payload),
        error = function(e) {
          showNotification(
            trf("step1_payload_skipped", lang(), conditionMessage(e)),
            type = "warning",
            duration = 5
          )
          FALSE
        }
      )
    }, ignoreNULL = TRUE)
  }

  ensure_step1_payload_inputs_synced <- function(payload = get_latest_step1_payload()) {
    current_tab <- as.character(safe_input_get("main_tabs") %||% "")
    if (!is_step2_tab_selected(current_tab)) return(invisible(FALSE))
    if (is.null(payload) || !is.list(payload)) return(invisible(FALSE))
    sig <- payload_signature(payload)
    sig_state <- get_step1_signature_state()

    if (!is.na(sig) && identical(sig, sig_state$input)) {
      return(invisible(FALSE))
    }

    need_reset <- is.na(sig) || !identical(sig, sig_state$reset)
    if (isTRUE(need_reset)) {
      int_df <- get_step1_payload_integration_df(payload)
      n_inj_default <- if (!is.null(int_df) && nrow(int_df) > 0) nrow(int_df) else NULL
      tryCatch(
        reset_step2_ui_for_new_dataset(
          default_n_inj = n_inj_default,
          apply_first_injection_default = FALSE
        ),
        error = function(e) NULL
      )
      if (!is.na(sig)) {
        sig_state$reset <- sig
      }
    }
    input_apply <- tryCatch(
      apply_step1_inputs_from_payload(payload),
      error = function(e) list(updated_any = FALSE, vinit_synced = FALSE)
    )
    if (isTRUE(input_apply$vinit_synced) && !is.na(sig)) {
      sig_state$input <- sig
      # Defensive cleanup: avoid stale suppression flag leaking into first manual edit.
      values$v_pre_programmatic_update <- FALSE
    }
    set_step1_signature_state(sig_state)
    invisible(TRUE)
  }

  is_step1_payload_sync_pending <- function(payload = get_latest_step1_payload()) {
    if (is.null(payload) || !is.list(payload)) return(FALSE)
    sig <- payload_signature(payload)
    if (is.na(sig)) return(FALSE)
    sig_state <- get_step1_signature_state()
    !identical(sig_state$input, sig)
  }

  replay_step1_sync_after_flush <- function(payload = get_latest_step1_payload(), passes = 2L) {
    pass_n <- suppressWarnings(as.integer(passes)[1])
    if (!is.finite(pass_n) || pass_n < 1L) return(invisible(FALSE))
    if (is.null(payload) || !is.list(payload)) return(invisible(FALSE))

    session$onFlushed(function() {
      current_tab <- as.character(safe_input_get("main_tabs") %||% "")
      if (is_step2_tab_selected(current_tab)) {
        tryCatch(ensure_step1_payload_inputs_synced(payload), error = function(e) NULL)
      }
      if (pass_n > 1L) {
        replay_step1_sync_after_flush(payload = payload, passes = pass_n - 1L)
      }
    }, once = TRUE)
    invisible(TRUE)
  }

  step1_sync_retry <- new.env(parent = emptyenv())
  step1_sync_retry$signature <- NA_character_
  step1_sync_retry$remaining <- 0L

  schedule_step1_sync_retry <- function(payload = get_latest_step1_payload(), attempts = 6L, delay_sec = 0.25) {
    if (is.null(payload) || !is.list(payload)) return(invisible(FALSE))
    if (!requireNamespace("later", quietly = TRUE)) return(invisible(FALSE))

    sig <- payload_signature(payload)
    if (is.na(sig)) sig <- paste0("na|", format(Sys.time(), "%Y%m%d%H%M%OS6"))

    attempts_n <- suppressWarnings(as.integer(attempts)[1])
    if (!is.finite(attempts_n) || attempts_n < 1L) return(invisible(FALSE))

    step1_sync_retry$signature <- sig
    step1_sync_retry$remaining <- attempts_n

    run_attempt <- function(expected_sig) {
      # Cancel stale retry chain if a newer payload token arrives.
      if (!identical(step1_sync_retry$signature, expected_sig)) return(invisible(FALSE))

      rem <- suppressWarnings(as.integer(step1_sync_retry$remaining)[1])
      if (!is.finite(rem) || rem <= 0L) return(invisible(FALSE))

      current_tab <- as.character(safe_input_get("main_tabs") %||% "")
      if (is_step2_tab_selected(current_tab)) {
        tryCatch(ensure_step1_payload_inputs_synced(payload), error = function(e) NULL)
        sig_state <- get_step1_signature_state()
        if (!is.na(sig_state$input) && identical(sig_state$input, expected_sig)) {
          step1_sync_retry$remaining <- 0L
          return(invisible(TRUE))
        }
      }

      step1_sync_retry$remaining <- rem - 1L
      if (step1_sync_retry$remaining > 0L) {
        later::later(function() run_attempt(expected_sig), delay = delay_sec)
      }
      invisible(FALSE)
    }

    later::later(function() run_attempt(sig), delay = delay_sec)
    invisible(TRUE)
  }

  # Ensure Step 1 parameters are applied after Step 2 controls are mounted.
  observeEvent(latest_step1_payload(), {
    payload <- latest_step1_payload()
    ensure_step1_payload_inputs_synced(payload)
    replay_step1_sync_after_flush(payload = payload, passes = 3L)
    schedule_step1_sync_retry(payload = payload, attempts = 8L, delay_sec = 0.25)
  }, ignoreNULL = TRUE)

  observeEvent(input$main_tabs, {
    if (!is_step2_tab_selected(input$main_tabs)) return()
    payload <- get_latest_step1_payload()
    ensure_step1_payload_inputs_synced(payload)
    replay_step1_sync_after_flush(payload = payload, passes = 2L)
    schedule_step1_sync_retry(payload = payload, attempts = 5L, delay_sec = 0.25)
  }, ignoreInit = TRUE)
  
  output$status_indicator <- renderUI({
    if(values$is_fitting) {
      tagList(div(class="status-light light-red"), 
              span(tr("fit_status_fitting", lang()), style="color:#e74c3c"))
    } else {
      tagList(div(class="status-light light-green"), 
              span(tr("fit_status_ready", lang()), style="color:#2ecc71"))
    }
  })

  # [新增] 统一的 RSS 计算逻辑 (Reactive)
  # 基于针数 (Injection Index) 对齐，而非 Ratio 插值
  current_rss_val <- reactive({
    sim_res <- sim_results()
    exp_df <- tryCatch(exp_data_processed(), error=function(e) NULL)
    range_lim <- input$fit_data_range
    
    if(is.null(sim_res) || is.null(exp_df)) return(list(rss = NA, method = ""))
    if(any(!is.finite(sim_res$dQ_App))) return(list(rss = NA, method = ""))
    
    # 确保模拟针数至少覆盖了拟合区间
    max_idx <- min(nrow(sim_res), nrow(exp_df))
    valid_idx <- range_lim[1]:range_lim[2]
    valid_idx <- valid_idx[valid_idx <= max_idx]
    
    if(length(valid_idx)==0) return(list(rss = NA, method = ""))
    
    # 计算残差
    residuals <- sim_res$dQ_App[valid_idx] - exp_df$Heat_Raw[valid_idx]
    
    # 检查是否启用了加权或鲁棒方法
    use_weighted <- isTRUE(input$use_weighted_fitting)
    use_robust <- isTRUE(input$use_robust_fitting)
    huber_delta_input <- if(!is.null(input$huber_delta) && !is.na(input$huber_delta) && input$huber_delta > 0) input$huber_delta else NULL
    
    # 构建方法标识
    method_parts <- c()
    if(isTRUE(use_weighted)) method_parts <- c(method_parts, tr("weighted_method", lang()))
    if(isTRUE(use_robust)) method_parts <- c(method_parts, tr("robust_method", lang()))
    method_str <- if(length(method_parts) > 0) paste0(" (", paste(method_parts, collapse="+"), ")") else ""
    
    # 计算损失（与拟合时使用相同的方法）
    loss <- calculate_weighted_robust_loss(
      residuals = residuals,
      weights = if(isTRUE(use_weighted)) calculate_weights_from_derivative(exp_df, valid_idx) else NULL,
      use_huber = isTRUE(use_robust),
      huber_delta = huber_delta_input
    )
    
    return(list(rss = loss, method = method_str))
  })

  output$fit_status <- renderText({
    rss_info <- current_rss_val()
    if(is.na(rss_info$rss)) {
      tr("rss_na", lang())
    } else {
      paste0(formatC(rss_info$rss, format="e", digits=3), rss_info$method)
    }
  })
  
  # [新增] 误差分析表格输出
  output$error_analysis_table <- renderDT({
    # 显式依赖 lang() 以确保语言切换时重新渲染
    current_lang_val <- lang()
    
    error_df <- values$error_analysis
    # Filter for display: only logK and H parameters
    if (!is.null(error_df)) {
      error_df <- error_df[grepl("logK|^H[0-9]$", error_df$Parameter), ]
    }
    
    if (is.null(error_df) || nrow(error_df) == 0) {
      msg_col <- tr("error_analysis_no_result", current_lang_val)
      return(datatable(data.frame(Message = msg_col), 
                       options = list(dom = 't', destroy = TRUE), rownames = FALSE))
    }
    
    # 确保 error_df 有正确的列名（原始列名，不依赖翻译）
    # 如果列名已经被重命名过，需要先恢复原始列名
    required_cols <- c("Parameter", "Value", "SE", "CI_Lower", "CI_Upper")
    if (!all(required_cols %in% colnames(error_df))) {
      # 如果列名不匹配，尝试从翻译后的列名恢复
      # 这种情况不应该发生，但为了安全起见
      msg_col <- tr("error_analysis_no_result", current_lang_val)
      return(datatable(data.frame(Message = msg_col), 
                       options = list(dom = 't', destroy = TRUE), rownames = FALSE))
    }
    
    # 格式化显示：只显示 logK 和 H 参数
    display_df <- error_df %>%
      mutate(
        # 格式化数值显示
        Value_Formatted = ifelse(
          grepl("logK", Parameter),
          sprintf("%.3f", Value),
          sprintf("%.1f", Value)
        ),
        SE_Formatted = ifelse(
          grepl("logK", Parameter),
          sprintf("±%.3f", SE),
          sprintf("±%.1f", SE)
        ),
        CI_Formatted = ifelse(
          grepl("logK", Parameter),
          sprintf("[%.3f, %.3f]", CI_Lower, CI_Upper),
          sprintf("[%.1f, %.1f]", CI_Lower, CI_Upper)
        ),
        # 参数名格式化（使用更安全的方法，避免 ifelse 的问题）
        Parameter_Formatted = sapply(Parameter, function(p) {
          if(is.null(p) || is.na(p) || p == "" || length(p) == 0) {
            return(p)
          }
          if(grepl("logK", p)) {
            result <- gsub("logK", "log K", p)
            if(is.null(result) || length(result) == 0 || result == "") p else result
          } else if(grepl("^H[0-9]$", p)) {
            result <- gsub("^H", "ΔH", p)
            if(is.null(result) || length(result) == 0 || result == "") p else result
          } else {
            p
          }
        })
      ) %>%
      select(Parameter_Formatted, Value_Formatted, SE_Formatted, CI_Formatted)
    
    # 获取翻译后的列名（确保不为空）
    param_name <- tr("error_analysis_table_param", current_lang_val)
    value_name <- tr("error_analysis_table_value", current_lang_val)
    se_name <- tr("error_analysis_table_se", current_lang_val)
    ci_name <- tr("error_analysis_table_ci", current_lang_val)
    
    # 如果翻译为空，使用默认值
    if(is.null(param_name) || length(param_name) == 0 || param_name == "") param_name <- "Parameter"
    if(is.null(value_name) || length(value_name) == 0 || value_name == "") value_name <- "Best Value"
    if(is.null(se_name) || length(se_name) == 0 || se_name == "") se_name <- "Standard Error"
    if(is.null(ci_name) || length(ci_name) == 0 || ci_name == "") ci_name <- "95% Confidence Interval"
    
    # 【关键修复】使用固定的内部列名，避免 DataTables 缓存问题
    # 内部列名永远使用英文，不随语言变化，这样 DataTables 就不会因为列名变化而报错
    internal_colnames <- c("Param", "Value", "SE", "CI")
    colnames(display_df) <- internal_colnames
    
    # 使用 initComplete 回调来在表格初始化完成后更新表头
    # 这样可以确保表头使用翻译后的名称，同时数据框列名保持固定
    init_complete_js <- JS(
      sprintf(
        "function(settings, json) {
          var api = this.api();
          $(api.table().header()).find('th').eq(0).text('%s');
          $(api.table().header()).find('th').eq(1).text('%s');
          $(api.table().header()).find('th').eq(2).text('%s');
          $(api.table().header()).find('th').eq(3).text('%s');
        }",
        gsub("'", "\\'", param_name),  # 转义单引号
        gsub("'", "\\'", value_name),
        gsub("'", "\\'", se_name),
        gsub("'", "\\'", ci_name)
      )
    )
    
    datatable(
      display_df,
      options = list(
        dom = 't',
        pageLength = 10,
        scrollY = "150px",
        scrollCollapse = TRUE,
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        # 添加 destroy: true 选项，确保在重新渲染时销毁旧表格
        destroy = TRUE,
        # 使用 initComplete 回调在表格初始化后更新表头
        initComplete = init_complete_js
      ),
      selection = 'none',  # 禁用行选择功能
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left; font-size: 0.9em; color: #666;',
        tr("error_analysis_note", current_lang_val)
      )
    ) %>%
      formatStyle(columns = 1:4, fontSize = '12px')
  })
  
  # [修改] 仅当用户手动调整第一针体积时提示同步 V_init；程序更新（导入、打开 app、加载快照等）不提示
  observeEvent(input$V_pre, {
    is_programmatic <- isTRUE(values$v_pre_programmatic_update)
    if (is_programmatic) {
      values$v_pre_programmatic_update <- FALSE
    }

    need_warn <- tryCatch(
      should_warn_v_pre_change(
        v_pre = input$V_pre,
        v_init = input$V_init_val,
        is_programmatic = is_programmatic,
        is_step1_sync_pending = isTRUE(is_step1_payload_sync_pending())
      ),
      error = function(e) FALSE
    )
    if (!isTRUE(need_warn)) return()

    showNotification(
      tr("v_pre_change_warning", lang()), 
      type = "message", 
      duration = 8
    )
  }, ignoreInit = TRUE)
  
  
  # 移除重复代码块占位符 (原有重复代码将被覆盖为空)
  
  output$dynamic_fit_params_ui <- renderUI({
    # [修复] active_paths 应该是 reactive 依赖，这样路径变化时会自动更新
    # 只有 current_selection 需要 isolate，避免循环依赖
    paths <- input$active_paths
    current_selection <- isolate({
      if(!is.null(input$fit_params)) input$fit_params else c("logK1", "H1")  # [修改] 默认只勾选基础参数
    })
    
    choices <- c("fH", "fG", "V_init", "Offset","logK1", "H1") # [修改] fpre -> V_init
    
    # [新增] 根据激活的路径自动添加对应的参数到选择中
    auto_selected <- c("logK1", "H1")  # 基础参数始终在自动选择列表中
    
    if("rxn_D" %in% paths) {
      choices <- c(choices, "logK2", "H2")
      auto_selected <- c(auto_selected, "logK2", "H2")
    }
    if("rxn_T" %in% paths) {
      choices <- c(choices, "logK3", "H3")
      auto_selected <- c(auto_selected, "logK3", "H3")
    }
    if("rxn_B" %in% paths) {
      choices <- c(choices, "logK4", "H4")
      auto_selected <- c(auto_selected, "logK4", "H4")
    }
    if("rxn_F" %in% paths) {
      choices <- c(choices, "logK5", "H5")
      auto_selected <- c(auto_selected, "logK5", "H5")
    }
    if("rxn_U" %in% paths) {
      choices <- c(choices, "logK6", "H6")
      auto_selected <- c(auto_selected, "logK6", "H6")
    }
    
    # [修改] 合并当前选择和自动选择，但只保留有效的选择项
    # 这样既保留了用户的手动选择（如 fH, fG 等），又自动添加了激活路径的参数
    valid_selection <- unique(c(
      intersect(current_selection, choices),  # 保留用户之前的选择（如果仍然有效）
      intersect(auto_selected, choices)       # 添加自动选择的参数
    ))
    
    # [新增] 添加全选/全不选按钮
    tagList(
      div(style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 5px;",
          tags$label(tr("fit_params_select", lang()), style="margin: 0; font-weight: bold;"),
          div(style="display: flex; gap: 5px;",
              actionButton("select_all_fit_params", tr("fit_params_select_all", lang()), class = "btn-default btn-xs", style="padding: 2px 8px; font-size: 11px;"),
              actionButton("deselect_all_fit_params", tr("fit_params_deselect_all", lang()), class = "btn-default btn-xs", style="padding: 2px 8px; font-size: 11px;")
          )
      ),
      checkboxGroupInput("fit_params", NULL, choices = choices, selected = valid_selection, inline = TRUE)
    )
  })
  
  # [新增] 全选拟合参数
  observeEvent(input$select_all_fit_params, {
    # [修复] 防止在语言切换时触发
    if(lang_switching()) return()
    
    paths <- input$active_paths
    choices <- c("fH", "fG", "V_init", "Offset","logK1", "H1")
    
    if("rxn_D" %in% paths) choices <- c(choices, "logK2", "H2")
    if("rxn_T" %in% paths) choices <- c(choices, "logK3", "H3")
    if("rxn_B" %in% paths) choices <- c(choices, "logK4", "H4")
    if("rxn_F" %in% paths) choices <- c(choices, "logK5", "H5")
    if("rxn_U" %in% paths) choices <- c(choices, "logK6", "H6")
    
    updateCheckboxGroupInput(session, "fit_params", selected = choices)
  }, ignoreInit = TRUE)
  
  # [新增] 全不选拟合参数
  observeEvent(input$deselect_all_fit_params, {
    # [修复] 防止在语言切换时触发
    if(lang_switching()) return()
    
    updateCheckboxGroupInput(session, "fit_params", selected = character(0))
  }, ignoreInit = TRUE)
  
  # [新增] 模拟→实验按钮点击事件处理器
  observeEvent(input$sim_to_exp, {
    sim <- sim_results()
    
    # 检查模拟数据是否有效
    if (is.null(sim)) {
      showNotification(
        tr("export_error_no_data", lang()), 
        type = "error", 
        duration = 5
      )
      return()
    }
    
    # 转换模拟数据为实验数据格式
    tryCatch({
      safe_num <- function(x, default) {
        v <- suppressWarnings(as.numeric(x)[1])
        if (is.finite(v)) v else default
      }
      vpre_target <- resolve_sim_to_exp_vpre_target(input$V_init_val)
      if (!isTRUE(vpre_target$ok)) {
        showNotification(tr(vpre_target$error_key, lang()), type = "error", duration = 5)
        return()
      }
      v_pre_now <- vpre_target$target_v_pre
      v_inj_now <- safe_num(input$V_inj, UI_DEFAULTS$v_inj_default * 1000)
      g_syringe_now <- safe_num(input$G_syringe, UI_DEFAULTS$conc_syringe_default)

      exp_data <- build_sim_to_exp_exp_df(
        sim_df = as.data.frame(sim),
        v_pre = v_pre_now,
        v_inj = v_inj_now,
        g_syringe = g_syringe_now
      )
      if (is.null(exp_data) || nrow(exp_data) == 0) {
        stop("Sim->Exp conversion produced empty data.")
      }
      
      # 存储到 manual_exp_data
      values$manual_exp_data <- exp_data
      values$manual_exp_source <- "sim_to_exp"
      values$exp_data_disabled <- FALSE

      # Sim->Exp: use current V_init as source and sync V_pre <- V_init.
      update_v_pre_programmatically(vpre_target$target_v_pre)
      
      # 清空导入文件信息，避免 UI 显示之前的导入文件名
      values$imported_xlsx_filename <- NULL
      values$imported_xlsx_base_name <- NULL
      values$imported_xlsx_file_path <- NULL
      values$imported_xlsx_sheets <- NULL
      
      # 显示成功通知
      showNotification(
        paste0(tr("btn_sim_to_exp", lang()), ": ", 
               nrow(exp_data), " ", 
               tr("error_analysis_data_points", lang())),
        type = "message",
        duration = 3
      )
    }, error = function(e) {
      showNotification(
        paste0(tr("error_occurred", lang()), ": ", e$message),
        type = "error",
        duration = 5
      )
    })
  })

  observeEvent(input$rm_exp, {
    values$manual_exp_data <- NULL
    values$manual_exp_source <- NULL
    values$imported_xlsx_sheets <- NULL
    values$imported_xlsx_file_path <- NULL
    values$imported_xlsx_base_name <- NULL
    values$imported_xlsx_filename <- NULL
    values$exp_data_disabled <- TRUE
    showNotification(tr("exp_data_removed_sim_only", lang()), type = "message", duration = 3)
  }, ignoreInit = TRUE)
  
  # 缓存当前导入的 xlsx 文件路径，用于与 reactive 共享已读取的 sheets，避免重复读取且保证参数更新与导入同步
  values$imported_xlsx_file_path <- NULL

  exp_data_processed <- reactive({
    if (isTRUE(values$exp_data_disabled)) return(NULL)

    safe_val <- function(x, default) {
      if (is.null(x) || length(x) == 0 || is.na(x)) default else x
    }

    current_exp_heat_context <- function() {
      list(
        v_pre = safe_val(input$V_pre, UI_DEFAULTS$v_pre_default),
        v_inj = safe_val(input$V_inj, UI_DEFAULTS$v_inj_default * 1000),
        g_syringe = safe_val(input$G_syringe, UI_DEFAULTS$conc_syringe_default)
      )
    }

    apply_updated_ratio <- function(d) {
      if (is.null(d) || nrow(d) == 0) return(d)
      V_inj_vec <- if ("V_inj_uL" %in% names(d)) {
        as.numeric(d$V_inj_uL)
      } else {
        rep(safe_val(input$V_inj, UI_DEFAULTS$v_inj_default * 1000), nrow(d))
      }
      if (any(is.na(V_inj_vec))) {
        V_inj_vec[is.na(V_inj_vec)] <- safe_val(input$V_inj, UI_DEFAULTS$v_inj_default * 1000)
      }
      p_ratio <- list(
        H_cell_0 = safe_val(input$H_cell_0, UI_DEFAULTS$conc_cell_default),
        G_syringe = safe_val(input$G_syringe, UI_DEFAULTS$conc_syringe_default),
        V_cell = safe_val(input$V_cell, UI_DEFAULTS$v_cell_default),
        V_inj_vec = V_inj_vec,
        n_inj = length(V_inj_vec),
        fH = safe_val(input$factor_H, DEFAULT_PARAMS$fH),
        fG = safe_val(input$factor_G, DEFAULT_PARAMS$fG)
      )
      ratio_app <- tryCatch(calculate_ratio_app(p_ratio), error = function(e) NULL)
      if (!is.null(ratio_app) && length(ratio_app) == nrow(d)) {
        d$Ratio_Raw <- ratio_app
      }
      d
    }

    apply_dynamic_heat_update <- function(d) {
      if (is.null(d) || nrow(d) == 0) return(d)
      if (!("Heat_ucal" %in% names(d))) return(d)

      ctx <- current_exp_heat_context()
      d$V_inj_uL <- build_current_vinj_ul(
        n = nrow(d),
        v_pre = ctx$v_pre,
        v_inj = ctx$v_inj
      )
      fallback_heat <- if ("Heat_Raw" %in% names(d)) as.numeric(d$Heat_Raw) else rep(NA_real_, nrow(d))
      d$Heat_Raw <- calc_heat_cal_mol_from_ucal(
        heat_ucal = as.numeric(d$Heat_ucal),
        vinj_ul = d$V_inj_uL,
        g_syringe = ctx$g_syringe,
        fallback_heat_cal_mol = fallback_heat
      )
      d <- d[is.finite(d$Heat_Raw), , drop = FALSE]
      if (nrow(d) == 0) return(NULL)
      d$Inj <- seq_len(nrow(d))
      d
    }

    build_exp_from_integration <- function(int_df) {
      if (is.null(int_df) || nrow(int_df) == 0) return(NULL)
      has_heat_cal_mol <- "heat_cal_mol" %in% colnames(int_df)
      has_heat_ucal <- "Heat_ucal" %in% colnames(int_df)
      if (!has_heat_cal_mol && !has_heat_ucal) {
        showNotification(tr("missing_heat_ucal_integration", lang()), type = "warning", duration = 5)
        return(NULL)
      }
      ctx <- current_exp_heat_context()
      d <- build_step2_exp_df_from_integration(
        int_df = int_df,
        v_pre = ctx$v_pre,
        v_inj = ctx$v_inj,
        g_syringe = ctx$g_syringe,
        prefer_heat_ucal = TRUE
      )
      if (is.null(d) || nrow(d) == 0) return(NULL)
      apply_updated_ratio(d)
    }

    # 优先使用手动导入的数据（模拟→实验功能）
    if (!is.null(values$manual_exp_data)) {
      d <- values$manual_exp_data
      d <- apply_dynamic_heat_update(d)
      if (is.null(d) || nrow(d) == 0) return(NULL)
      return(apply_updated_ratio(d))
    }
    
    filepath <- values$imported_xlsx_file_path
    if (is.null(filepath) || !nzchar(as.character(filepath)[1])) {
      req(input$exp_file)
      filepath <- input$exp_file$datapath
    }

    # 若已在 observeEvent(input$exp_file) 中读取过同一文件，直接使用缓存数据
    if (!is.null(values$imported_xlsx_sheets) && identical(filepath, values$imported_xlsx_file_path)) {
      # 优先：integration_rev > integration
      int_df <- get_preferred_integration_sheet(values$imported_xlsx_sheets)
      d <- build_exp_from_integration(int_df)
      if (!is.null(d)) return(d)
      # 备选：SimFit 导出的拟合数据 (simulation sheet)，作为实验数据导入
      sim_df <- values$imported_xlsx_sheets[["simulation"]]
      if (!is.null(sim_df) && "Ratio_App" %in% colnames(sim_df) && "dQ_App" %in% colnames(sim_df)) {
        d <- data.frame(
          Ratio_Raw = as.numeric(sim_df$Ratio_App),
          Heat_Raw  = as.numeric(sim_df$dQ_App),
          V_inj_uL  = if ("V_inj_uL" %in% colnames(sim_df)) as.numeric(sim_df$V_inj_uL) else NA_real_,
          Inj       = if ("Inj" %in% colnames(sim_df)) as.integer(sim_df$Inj) else seq_len(nrow(sim_df))
        )
        d <- d[!is.na(d$Ratio_Raw) & !is.na(d$Heat_Raw), ]
        d$Inj <- seq_len(nrow(d))
        return(apply_updated_ratio(d))
      }
    }
    
    df <- tryCatch({
      # 先读取并缓存所有 sheets
      sheet_names <- readxl::excel_sheets(filepath)
      cached_sheets <- list()
      for (sn in sheet_names) {
        cached_sheets[[sn]] <- as.data.frame(readxl::read_excel(filepath, sheet = sn))
      }
      values$imported_xlsx_sheets <- cached_sheets
      values$imported_xlsx_file_path <- filepath

      # --- 路径 1：integration_rev > integration ---
      int_df <- get_preferred_integration_sheet(cached_sheets)
      d <- build_exp_from_integration(int_df)
      if (!is.null(d)) return(d)

      # --- 路径 2：SimFit 导出的拟合数据 (simulation sheet)，作为实验数据导入 ---
      if ("simulation" %in% names(cached_sheets)) {
        sim_df <- cached_sheets[["simulation"]]
        if (!is.null(sim_df) && "Ratio_App" %in% colnames(sim_df) && "dQ_App" %in% colnames(sim_df)) {
          d <- data.frame(
            Ratio_Raw = as.numeric(sim_df$Ratio_App),
            Heat_Raw  = as.numeric(sim_df$dQ_App),
            V_inj_uL  = if ("V_inj_uL" %in% colnames(sim_df)) as.numeric(sim_df$V_inj_uL) else NA_real_,
            Inj       = if ("Inj" %in% colnames(sim_df)) as.integer(sim_df$Inj) else seq_len(nrow(sim_df))
          )
          d <- d[!is.na(d$Ratio_Raw) & !is.na(d$Heat_Raw), ]
          d$Inj <- seq_len(nrow(d))
          return(apply_updated_ratio(d))
        }
      }

      return(NULL)
    }, error = function(e) {
      values$imported_xlsx_sheets <- NULL
      values$imported_xlsx_file_path <- NULL
      return(NULL)
    })
    
    req(df)
    return(df)
  })
  
  read_xlsx_sheets <- function(filepath) {
    if (is.null(filepath) || !nzchar(as.character(filepath)[1])) return(NULL)
    tryCatch({
      snames <- readxl::excel_sheets(filepath)
      lst <- list()
      for (sn in snames) {
        lst[[sn]] <- as.data.frame(readxl::read_excel(filepath, sheet = sn))
      }
      lst
    }, error = function(e) NULL)
  }

  apply_step2_param_snapshot <- function(snapshot) {
    if (is.null(snapshot) || !is.list(snapshot)) return(invisible(FALSE))
    to_num1 <- function(x) {
      v <- suppressWarnings(as.numeric(x))
      if (length(v) < 1 || !is.finite(v[1])) return(NA_real_)
      v[1]
    }
    h0 <- to_num1(snapshot$H_cell_0)
    gs <- to_num1(snapshot$G_syringe)
    vc <- to_num1(snapshot$V_cell)
    vi <- to_num1(snapshot$V_inj)
    ni <- to_num1(snapshot$n_inj)
    vp <- to_num1(snapshot$V_pre)
    tp <- to_num1(snapshot$Temp)
    fh <- to_num1(snapshot$factor_H)
    fg <- to_num1(snapshot$factor_G)
    ho <- to_num1(snapshot$heat_offset)

    if (is.finite(h0)) updateNumericInput(session, "H_cell_0", value = h0)
    if (is.finite(gs)) updateNumericInput(session, "G_syringe", value = gs)
    if (is.finite(vc)) updateNumericInput(session, "V_cell", value = vc)
    if (is.finite(vi)) updateNumericInput(session, "V_inj", value = vi)
    if (is.finite(ni)) updateNumericInput(session, "n_inj", value = as.integer(ni))
    if (is.finite(vp)) updateNumericInput(session, "V_pre", value = vp)
    if (is.finite(tp)) updateNumericInput(session, "Temp", value = tp)
    if (is.finite(fh)) updateNumericInput(session, "factor_H", value = fh)
    if (is.finite(fg)) updateNumericInput(session, "factor_G", value = fg)
    if (is.finite(ho)) updateSliderInput(session, "heat_offset", value = ho)

    paths <- snapshot$active_paths
    if (is.character(paths) && length(paths) > 0) {
      update_active_paths_normalized(paths)
    }
    invisible(TRUE)
  }

  apply_imported_xlsx_state <- function(sheets,
                                        filepath,
                                        file_name = NULL,
                                        base_name_override = NULL,
                                        notify_messages = TRUE) {
    if (is.null(sheets) || !is.list(sheets) || length(sheets) < 1) return(FALSE)

    values$manual_exp_data <- NULL
    values$manual_exp_source <- NULL
    values$exp_data_disabled <- FALSE
    values$imported_xlsx_sheets <- NULL
    values$imported_xlsx_file_path <- NULL
    values$imported_xlsx_base_name <- NULL
    values$imported_xlsx_filename <- NULL

    file_display <- as.character(file_name %||% "")
    if (length(file_display) < 1 || !nzchar(trimws(file_display[1]))) {
      file_display <- "data.xlsx"
    } else {
      file_display <- trimws(file_display[1])
    }
    values$imported_xlsx_filename <- file_display

    base_raw <- tools::file_path_sans_ext(file_display)
    base_norm <- sub("_(processed|fitted)_\\d{8}_\\d{4}$", "", base_raw)
    base_override <- as.character(base_name_override %||% "")
    if (length(base_override) >= 1 && nzchar(trimws(base_override[1]))) {
      base_norm <- trimws(base_override[1])
    }
    values$imported_xlsx_base_name <- base_norm

    values$imported_xlsx_sheets <- sheets
    values$imported_xlsx_file_path <- as.character(filepath %||% "")[1]

    inferred_n_inj <- NULL
    preferred_int <- get_preferred_integration_sheet(sheets)
    if (!is.null(preferred_int)) {
      inferred_n_inj <- nrow(preferred_int)
    } else if ("simulation" %in% names(sheets) && is.data.frame(sheets[["simulation"]])) {
      inferred_n_inj <- nrow(as.data.frame(sheets[["simulation"]]))
    } else {
      meta_num <- extract_meta_numeric_map_with_rev_priority(
        meta_rev_df = sheets[["meta_rev"]],
        meta_df = sheets[["meta"]]
      )
      if ("n_inj" %in% names(meta_num) && !is.na(meta_num[["n_inj"]])) {
        inferred_n_inj <- suppressWarnings(as.integer(meta_num[["n_inj"]]))
      }
    }
    reset_step2_ui_for_new_dataset(default_n_inj = inferred_n_inj, apply_first_injection_default = FALSE)

    exp_key_cols <- c("H_cell_0_mM", "G_syringe_mM", "V_cell_mL", "V_inj_uL", "n_inj", "V_pre_uL", "Temp_K")
    has_exp_data <- FALSE
    if (!is.null(preferred_int)) {
      int_df <- preferred_int
      has_heat_src <- !is.null(int_df) && (("heat_cal_mol" %in% colnames(int_df)) || ("Heat_ucal" %in% colnames(int_df)))
      if (!is.null(int_df) && "Ratio_App" %in% colnames(int_df) && isTRUE(has_heat_src)) {
        has_exp_data <- TRUE
      }
    }
    if (!has_exp_data && "simulation" %in% names(sheets)) {
      sim_df <- sheets[["simulation"]]
      if (!is.null(sim_df) && "Ratio_App" %in% colnames(sim_df) && "dQ_App" %in% colnames(sim_df)) {
        has_exp_data <- TRUE
      }
    }

    if (has_exp_data) {
      meta_has_params <- FALSE
      meta_vals <- extract_meta_numeric_map_with_rev_priority(
        meta_rev_df = sheets[["meta_rev"]],
        meta_df = sheets[["meta"]]
      )
      meta_has_params <- any(vapply(exp_key_cols, function(k) {
        k %in% names(meta_vals) && !is.na(meta_vals[[k]])
      }, logical(1)))
      if (meta_has_params) {
        if ("H_cell_0_mM" %in% names(meta_vals) && !is.na(meta_vals[["H_cell_0_mM"]])) updateNumericInput(session, "H_cell_0", value = meta_vals[["H_cell_0_mM"]])
        if ("G_syringe_mM" %in% names(meta_vals) && !is.na(meta_vals[["G_syringe_mM"]])) updateNumericInput(session, "G_syringe", value = meta_vals[["G_syringe_mM"]])
        if ("V_cell_mL" %in% names(meta_vals) && !is.na(meta_vals[["V_cell_mL"]])) updateNumericInput(session, "V_cell", value = meta_vals[["V_cell_mL"]])
        if ("V_inj_uL" %in% names(meta_vals) && !is.na(meta_vals[["V_inj_uL"]])) updateNumericInput(session, "V_inj", value = meta_vals[["V_inj_uL"]])
        if ("n_inj" %in% names(meta_vals) && !is.na(meta_vals[["n_inj"]])) updateNumericInput(session, "n_inj", value = meta_vals[["n_inj"]])
        if ("Temp_K" %in% names(meta_vals) && !is.na(meta_vals[["Temp_K"]])) updateNumericInput(session, "Temp", value = meta_vals[["Temp_K"]])
        if (isTRUE(notify_messages)) {
          showNotification(tr("import_params_auto_filled", lang()), type = "message", duration = 3)
        }
      }

      fp_map <- extract_fit_params_map(sheets[["fit_params"]])
      fp_restore <- extract_simfit_restore_params(fp_map)
      fp_has_any <- any(vapply(fp_restore, function(v) is.finite(suppressWarnings(as.numeric(v))), logical(1)))
      if (fp_has_any) {
        restore_paths <- parse_active_paths_from_fit_params(fp_map)
        update_active_paths_normalized(restore_paths)

        if (is.finite(fp_restore$logK1)) updateSliderInput(session, "logK1", value = fp_restore$logK1)
        if (is.finite(fp_restore$H1)) updateSliderInput(session, "H1", value = fp_restore$H1)
        if (is.finite(fp_restore$logK2)) updateSliderInput(session, "logK2", value = fp_restore$logK2)
        if (is.finite(fp_restore$H2)) updateSliderInput(session, "H2", value = fp_restore$H2)
        if (is.finite(fp_restore$logK3)) updateSliderInput(session, "logK3", value = fp_restore$logK3)
        if (is.finite(fp_restore$H3)) updateSliderInput(session, "H3", value = fp_restore$H3)
        if (is.finite(fp_restore$logK4)) updateSliderInput(session, "logK4", value = fp_restore$logK4)
        if (is.finite(fp_restore$H4)) updateSliderInput(session, "H4", value = fp_restore$H4)
        if (is.finite(fp_restore$logK5)) updateSliderInput(session, "logK5", value = fp_restore$logK5)
        if (is.finite(fp_restore$H5)) updateSliderInput(session, "H5", value = fp_restore$H5)
        if (is.finite(fp_restore$logK6)) updateSliderInput(session, "logK6", value = fp_restore$logK6)
        if (is.finite(fp_restore$H6)) updateSliderInput(session, "H6", value = fp_restore$H6)

        if (is.finite(fp_restore$fH)) updateNumericInput(session, "factor_H", value = fp_restore$fH)
        if (is.finite(fp_restore$fG)) updateNumericInput(session, "factor_G", value = fp_restore$fG)
        if (is.finite(fp_restore$Offset_cal)) updateSliderInput(session, "heat_offset", value = fp_restore$Offset_cal)

        if (!meta_has_params) {
          if (is.finite(fp_restore$H_cell_0_mM)) updateNumericInput(session, "H_cell_0", value = fp_restore$H_cell_0_mM)
          if (is.finite(fp_restore$G_syringe_mM)) updateNumericInput(session, "G_syringe", value = fp_restore$G_syringe_mM)
          if (is.finite(fp_restore$V_cell_mL)) updateNumericInput(session, "V_cell", value = fp_restore$V_cell_mL)
          if (is.finite(fp_restore$V_inj_uL)) updateNumericInput(session, "V_inj", value = fp_restore$V_inj_uL)
          if (is.finite(fp_restore$n_inj)) updateNumericInput(session, "n_inj", value = as.integer(fp_restore$n_inj))
          if (is.finite(fp_restore$Temp_K)) updateNumericInput(session, "Temp", value = fp_restore$Temp_K)
        }
        if (isTRUE(notify_messages)) {
          showNotification(tr("import_params_from_fit_params", lang()), type = "message", duration = 4)
        }
      } else if (!meta_has_params && isTRUE(notify_messages)) {
        showNotification(tr("import_params_no_source", lang()), type = "warning", duration = 5)
      }

      default_first <- if (!is.null(UI_DEFAULTS$v_pre_default)) UI_DEFAULTS$v_pre_default else DEFAULT_PARAMS$V_init
      first_target <- resolve_first_injection_targets(
        mode = "import",
        meta_vals = meta_vals,
        fp_restore = if (isTRUE(fp_has_any)) fp_restore else NULL,
        int_df = preferred_int,
        default_v_pre = default_first
      )
      if (
        isTRUE(first_target$has_source) &&
        is.finite(first_target$v_pre_target) &&
        is.finite(first_target$v_init_target)
      ) {
        apply_import_first_injection_targets(
          v_pre_target = first_target$v_pre_target,
          v_init_target = first_target$v_init_target
        )
      }
    }

    if ("simulation" %in% names(sheets) && is.null(preferred_int) && isTRUE(notify_messages)) {
      sim_df <- sheets[["simulation"]]
      if (!is.null(sim_df) && "Ratio_App" %in% colnames(sim_df) && "dQ_App" %in% colnames(sim_df)) {
        showNotification(tr("import_fit_data_as_exp", lang()), type = "message", duration = 4)
      }
    }

    TRUE
  }

  normalize_step2_path <- function(path) {
    p <- as.character(path %||% "")[1]
    p <- trimws(p)
    if (!nzchar(p)) return("")
    tryCatch(normalizePath(p, winslash = "/", mustWork = FALSE), error = function(e) p)
  }

  restore_step2_home_record <- function(record) {
    if (is.null(record) || !is.list(record)) return(FALSE)
    filepath <- normalize_step2_path(record$source_path)
    if (!nzchar(filepath) || !file.exists(filepath)) return(FALSE)

    record_name <- as.character(record$display_name %||% "")[1]
    record_name <- trimws(record_name)
    file_name <- basename(filepath)
    if (nzchar(record_name) && grepl("\\.xlsx$", tolower(record_name))) {
      file_name <- record_name
    }
    sheets <- read_xlsx_sheets(filepath)
    if (is.null(sheets) || !is.list(sheets) || length(sheets) < 1) return(FALSE)

    isTRUE(apply_imported_xlsx_state(
      sheets = sheets,
      filepath = filepath,
      file_name = trimws(file_name),
      notify_messages = FALSE
    ))
  }

  home_register_restore("step2", restore_step2_home_record)

  import_step2_xlsx <- function(filepath, display_name = "data.xlsx", notify_messages = TRUE) {
    path_norm <- normalize_step2_path(filepath)
    if (!nzchar(path_norm) || !file.exists(path_norm)) return(FALSE)
    name_norm <- as.character(display_name %||% "")[1]
    name_norm <- trimws(name_norm)
    if (!nzchar(name_norm)) name_norm <- basename(path_norm)

    sheets <- read_xlsx_sheets(path_norm)
    if (is.null(sheets)) return(FALSE)

    ok <- isTRUE(apply_imported_xlsx_state(
      sheets = sheets,
      filepath = path_norm,
      file_name = name_norm,
      notify_messages = isTRUE(notify_messages)
    ))
    if (!isTRUE(ok)) return(FALSE)

    home_add_recent(
      list(
        display_name = values$imported_xlsx_filename %||% name_norm,
        file_name = values$imported_xlsx_filename %||% name_norm,
        source_step = "step2",
        target_step = "step2",
        source_path = path_norm,
        source_path_kind = "import"
      )
    )
    TRUE
  }

  # 监听文件上传：读取 xlsx、缓存 sheets，并立即根据 meta 更新实验参数（保证导入即更新）
  observeEvent(input$exp_file, {
    f <- input$exp_file
    if (is.null(f) || is.null(f$datapath)) return()
    filepath <- normalize_step2_path(f$datapath)
    display_name <- if (is.null(f$name) || f$name == "") "data.xlsx" else f$name
    import_step2_xlsx(filepath = filepath, display_name = display_name, notify_messages = TRUE)
  })

  observeEvent(input$desktop_pick_exp_file, {
    fn <- if (!is.null(session_desktop)) session_desktop$open_file else NULL
    if (!is.function(fn) || !isTRUE(desktop_open_file_enabled())) return()
    lang_now <- lang()

    fn(
      purpose = "step2_import",
      title = if (identical(lang_now, "zh")) "选择 Step2 数据文件" else "Select Step2 Data File",
      filters = list(list(name = "Excel Workbook", extensions = "xlsx")),
      on_selected = function(result) {
        ok <- isTRUE(import_step2_xlsx(
          filepath = result$file_path,
          display_name = result$file_name,
          notify_messages = TRUE
        ))
        if (!isTRUE(ok)) {
          showNotification(
            if (identical(lang_now, "zh")) "所选文件无效或读取失败。" else "Selected file is invalid or unreadable.",
            type = "error",
            duration = 4
          )
        }
      },
      on_cancel = function(...) invisible(NULL),
      on_error = function(message, ...) {
        msg <- as.character(message %||% "")[1]
        if (!nzchar(trimws(msg))) {
          msg <- if (identical(lang_now, "zh")) "桌面文件选择失败。" else "Desktop file picker failed."
        }
        showNotification(msg, type = "error", duration = 5)
      }
    )
  }, ignoreInit = TRUE)
  
  # 判断“当前无实验数据”：不依赖 exp_data_processed()，避免无文件时 req() 挂起导致滑条无法随 n_inj 更新
  has_exp_data <- reactive({
    if (!is.null(values$manual_exp_data) && nrow(values$manual_exp_data) > 0) return(TRUE)
    if (!is.null(values$imported_xlsx_sheets) && length(values$imported_xlsx_sheets) > 0) return(TRUE)
    if (is.null(input$exp_file) || length(input$exp_file) == 0) return(FALSE)
    exp_df <- tryCatch(exp_data_processed(), error = function(e) NULL)
    !is.null(exp_df) && nrow(exp_df) > 0
  })
  
  # 初始化：根据 n_inj 的初始值设置滑条默认值（只在应用启动时执行一次）
  fit_slider_initialized <- reactiveVal(FALSE)
  
  observe({
    if (!has_exp_data() && !fit_slider_initialized() && !is.null(input$n_inj) && is.numeric(input$n_inj) && input$n_inj > 0) {
      isolate({
        left_default <- if (as.integer(input$n_inj) >= 2L) 2L else 1L
        updateSliderInput(session, "fit_data_range", min = 1, max = input$n_inj, value = c(left_default, as.integer(input$n_inj)), step = 1)
        fit_slider_initialized(TRUE)
      })
    }
  }, priority = 10)
  
  # 有实验数据时：更新滑条 min/max；优先保留用户当前选择，仅在越界/无效时回退默认
  observeEvent(exp_data_processed(), {
    df <- exp_data_processed()
    if (is.null(df) || nrow(df) == 0) return()
    max_inj <- nrow(df)
    left_default <- if (max_inj >= 2L) 2L else 1L
    force_default <- isTRUE(fit_slider_force_default())
    cur <- isolate(input$fit_data_range)
    has_valid_cur <- !is.null(cur) &&
      length(cur) == 2 &&
      all(is.finite(cur))

    if (force_default) {
      new_value <- c(left_default, max_inj)
    } else if (has_valid_cur) {
      left <- as.integer(round(cur[1]))
      right <- as.integer(round(cur[2]))
      left <- max(1L, min(max_inj, left))
      right <- max(1L, min(max_inj, right))
      if (left > right) {
        left <- left_default
        right <- max_inj
      }
      new_value <- c(left, right)
    } else {
      new_value <- c(left_default, max_inj)
    }

    updateSliderInput(session, "fit_data_range", min = 1, max = max_inj, value = new_value, step = 1)
    if (force_default) fit_slider_force_default(FALSE)
  })
  
  # 无实验数据时：n_inj 变化时更新滑条 max 和 value（不依赖 exp_data_processed()，避免 req 挂起）
  observeEvent(input$n_inj, {
    if (!has_exp_data()) {
      if (!is.null(input$n_inj) && is.numeric(input$n_inj) && input$n_inj > 0) {
        n <- as.integer(input$n_inj)
        force_default <- isTRUE(fit_slider_force_default())
        cur <- isolate(input$fit_data_range)
        # 若当前范围仍在新 [1, n] 内则保留，否则设为全范围
        new_value <- if (force_default) {
          c(if (n >= 2L) 2L else 1L, n)
        } else if (!is.null(cur) && length(cur) == 2 && cur[1] >= 1 && cur[2] <= n && cur[1] <= cur[2]) {
          c(as.integer(cur[1]), as.integer(cur[2]))
        } else {
          c(if (n >= 2L) 2L else 1L, n)
        }
        updateSliderInput(session, "fit_data_range", min = 1, max = n, value = new_value, step = 1)
        if (force_default) fit_slider_force_default(FALSE)
      }
    }
  })
  
