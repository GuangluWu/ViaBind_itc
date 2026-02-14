fail_fast <- function(...) {
  stop(paste0(...), call. = FALSE)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# [COMMENT_STD][MODULE_HEADER]
# 模块职责：作为 ITCSuite 宿主应用，加载三步 legacy 子应用并提供跨步桥接总线。
# 依赖：shiny、bridge_contract.R、legacy 子应用 app.R、guide_annotations.R。
# 对外接口：ui、server、bridge_*_server() 模块函数。
# 副作用：启动时执行路径探测和子应用加载；运行时写入 session$userData。
# 变更历史：2026-02-12 - 增加 Phase 4 注释规范与 guide annotations 预埋加载。

library(shiny)
source("R/bridge_contract.R")
source("R/guide_annotations.R")
source("R/home_recent_helpers.R")
source("R/home_recent_store.R")

if (!exists("load_guide_annotations", mode = "function")) {
  fail_fast("Startup check failed: guide annotation loader is unavailable.")
}
if (!exists("home_detect_import_type", mode = "function")) {
  fail_fast("Startup check failed: home recent helper is unavailable.")
}
if (!exists("home_recent_store_load", mode = "function")) {
  fail_fast("Startup check failed: home recent store helper is unavailable.")
}

detect_repo_root <- function() {
  candidates <- unique(c(getwd(), dirname(getwd())))
  for (d in candidates) {
    ok <- dir.exists(file.path(d, "ITCprocessor")) &&
      dir.exists(file.path(d, "ITCsimfit")) &&
      dir.exists(file.path(d, "ITCgraph"))
    if (isTRUE(ok)) return(normalizePath(d, winslash = "/", mustWork = TRUE))
  }
  fail_fast(
    "Startup check failed: cannot locate repo root. ",
    "Please run from ITCSuiteWeb or repo root."
  )
}

load_legacy_app <- function(app_dir, label, required_symbols = character(0)) {
  app_file <- file.path(app_dir, "app.R")
  if (!dir.exists(app_dir)) {
    fail_fast("Startup check failed: missing legacy dir for ", label, ": ", app_dir)
  }
  if (!file.exists(app_file)) {
    fail_fast("Startup check failed: missing app.R for ", label, ": ", app_file)
  }

  env <- new.env(parent = globalenv())
  env$`%||%` <- `%||%`

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(app_dir)
  sys.source("app.R", envir = env, chdir = TRUE)

  # Legacy apps use source(..., local = FALSE) heavily. Snapshot global symbols
  # into module env so each legacy server has a stable dependency scope.
  for (nm in ls(envir = .GlobalEnv, all.names = TRUE)) {
    if (!exists(nm, envir = env, inherits = FALSE)) {
      try(assign(nm, get(nm, envir = .GlobalEnv, inherits = FALSE), envir = env), silent = TRUE)
    }
  }

  if (!exists("ui", envir = env, inherits = FALSE)) {
    fail_fast("Startup check failed: legacy ", label, " did not define `ui`.")
  }
  if (!exists("server", envir = env, inherits = FALSE)) {
    fail_fast("Startup check failed: legacy ", label, " did not define `server`.")
  }

  ui_obj <- get("ui", envir = env, inherits = FALSE)
  server_fun <- get("server", envir = env, inherits = FALSE)

  if (!is.function(server_fun)) {
    fail_fast("Startup check failed: legacy ", label, " `server` is not a function.")
  }
  # Keep legacy server lookups inside its own captured scope to avoid
  # cross-module symbol pollution when multiple legacy apps are hosted together.
  environment(server_fun) <- env

  if (length(required_symbols) > 0) {
    missing <- required_symbols[!vapply(
      required_symbols,
      function(sym) exists(sym, envir = env, inherits = TRUE),
      logical(1)
    )]
    if (length(missing) > 0) {
      fail_fast(
        "Startup check failed: missing required symbols for ",
        label, ": ", paste(missing, collapse = ", "), "."
      )
    }
  }

  list(ui = ui_obj, server = server_fun, env = env, label = label)
}

bridge_bus_server <- function(id) {
  # [COMMENT_STD][IO_CONTRACT]
  # 输入来源：moduleServer session 生命周期内的桥接读写调用。
  # 字段/类型：step1_payload/step2_plot_payload 为 list 载荷，内部含 schema_version/token 等字段。
  # 单位：token 采用 numeric 标量；时间戳由 payload 自身定义（ISO 字符串）。
  # 空值策略：validator 拒绝无效 payload；显式传入 NULL 时清空 channel。
  # 输出保证：返回包含两个 channel function 的 list，供步骤间共享。
  moduleServer(id, function(input, output, session) {
    step1_payload <- make_bridge_channel(sanitize_step1_payload, "step1_payload")
    step2_plot_payload <- make_bridge_channel(sanitize_step2_plot_payload, "step2_plot_payload")
    list(
      step1_payload = step1_payload,
      step2_plot_payload = step2_plot_payload
    )
  })
}

bridge_step1_to_step2_server <- function(id, bridge_bus) {
  moduleServer(id, function(input, output, session) {
    latest_token <- reactiveVal(NA_real_)
    observeEvent(bridge_bus$step1_payload(), {
      payload <- bridge_bus$step1_payload()
      if (is.null(payload) || !is.list(payload)) return()
      token <- suppressWarnings(as.numeric(payload$token)[1])
      if (is.finite(token)) latest_token(token)
    }, ignoreNULL = TRUE)
    list(latest_token = latest_token)
  })
}

bridge_step2_to_step3_server <- function(id, bridge_bus) {
  moduleServer(id, function(input, output, session) {
    latest_token <- reactiveVal(NA_real_)
    observeEvent(bridge_bus$step2_plot_payload(), {
      payload <- bridge_bus$step2_plot_payload()
      if (is.null(payload) || !is.list(payload)) return()
      token <- suppressWarnings(as.numeric(payload$token)[1])
      if (is.finite(token)) latest_token(token)
    }, ignoreNULL = TRUE)
    list(latest_token = latest_token)
  })
}

normalize_lang <- function(lang) {
  lang_chr <- tolower(as.character(lang %||% "en")[1])
  if (identical(lang_chr, "zh")) "zh" else "en"
}

host_tr <- function(key, lang) {
  dict <- list(
    en = list(
      home = "Home",
      step1 = "Step 1 Baseline & Integration",
      step2 = "Step 2 Simulation & Fitting",
      step3 = "Step 3 Plot & Export",
      home_welcome_title = "Welcome to ViaBind",
      home_welcome_desc = "Start from Step 1, or reopen one of your recently imported datasets.",
      home_start_step1 = "Enter Step 1",
      home_recent_title = "Recent Imports",
      home_recent_empty = "No recent imports in this session.",
      home_recent_export_title = "Recent Exports",
      home_recent_export_empty = "No recent exports in this session.",
      home_guide_button = "Beginner Guide",
      home_table_col_name = "Name",
      home_table_col_type = "Type",
      home_table_col_target = "Target",
      home_table_col_time = "Imported At",
      home_table_col_action = "Action",
      home_restore_action = "Restore & Open",
      home_import_type_itc = "ITC",
      home_import_type_processed_xlsx = "Processed XLSX",
      home_import_type_fitted_xlsx = "Fitted XLSX",
      home_export_type_xlsx = "XLSX",
      home_export_type_pdf = "PDF",
      home_export_type_png = "PNG",
      home_export_type_tiff = "TIFF",
      home_export_type_txt = "TXT",
      home_export_type_json = "JSON",
      home_target_step1 = "Step 1",
      home_target_step2 = "Step 2",
      home_target_step3 = "Step 3",
      home_restore_ok = "Restored %s and opened %s.",
      home_restore_failed_no_payload = "Source file is missing for %s.",
      home_restore_failed_path_missing = "Source path no longer exists for %s.",
      home_restore_failed_no_handler = "No restore handler registered for %s.",
      home_restore_failed_general = "Restore failed: %s",
      home_recent_path_label = "Path",
      home_recent_path_missing = "Path unavailable (file not found).",
      home_unknown_name = "Unnamed Import",
      home_guide_title = "Beginner Guide",
      home_guide_body_1 = "Home: enter Step 1, review recent imports, and quickly reopen prior work.",
      home_guide_body_2 = "Step 1: import .itc data, baseline correction, and integration.",
      home_guide_body_3 = "Step 2: simulation and fitting using imported or bridged data.",
      home_guide_body_4 = "Step 3: plotting and export for publication-ready output.",
      home_guide_placeholder = "A full interactive tour will be available in a later update.",
      close = "Close"
    ),
    zh = list(
      home = "首页",
      step1 = "Step 1 基线校正 & 积分",
      step2 = "Step 2 模拟 & 拟合",
      step3 = "Step 3 绘图 & 导出",
      home_welcome_title = "欢迎使用 ViaBind",
      home_welcome_desc = "你可以从 Step 1 开始，或从下方调用最近导入的数据。",
      home_start_step1 = "进入 Step 1",
      home_recent_title = "最近导入",
      home_recent_empty = "当前会话暂无最近导入记录。",
      home_recent_export_title = "最近导出",
      home_recent_export_empty = "当前会话暂无最近导出记录。",
      home_guide_button = "新手导引",
      home_table_col_name = "名称",
      home_table_col_type = "类型",
      home_table_col_target = "目标",
      home_table_col_time = "导入时间",
      home_table_col_action = "操作",
      home_restore_action = "恢复并进入",
      home_import_type_itc = "ITC",
      home_import_type_processed_xlsx = "Processed XLSX",
      home_import_type_fitted_xlsx = "Fitted XLSX",
      home_export_type_xlsx = "XLSX",
      home_export_type_pdf = "PDF",
      home_export_type_png = "PNG",
      home_export_type_tiff = "TIFF",
      home_export_type_txt = "TXT",
      home_export_type_json = "JSON",
      home_target_step1 = "Step 1",
      home_target_step2 = "Step 2",
      home_target_step3 = "Step 3",
      home_restore_ok = "已恢复 %s，并进入 %s。",
      home_restore_failed_no_payload = "缺少 %s 的源文件路径。",
      home_restore_failed_path_missing = "%s 的源路径不存在。",
      home_restore_failed_no_handler = "未找到 %s 的恢复处理器。",
      home_restore_failed_general = "恢复失败：%s",
      home_recent_path_label = "路径",
      home_recent_path_missing = "路径不可用（文件不存在）。",
      home_unknown_name = "未命名导入",
      home_guide_title = "新手导引",
      home_guide_body_1 = "首页：进入 Step 1、查看最近导入、快速恢复历史数据。",
      home_guide_body_2 = "Step 1：导入 .itc 数据并完成基线校正与积分。",
      home_guide_body_3 = "Step 2：基于导入或桥接数据进行模拟与拟合。",
      home_guide_body_4 = "Step 3：生成绘图并导出用于展示或发表的数据。",
      home_guide_placeholder = "完整的交互式导引将在后续版本上线。",
      close = "关闭"
    )
  )
  lang_norm <- normalize_lang(lang)
  val <- dict[[lang_norm]][[key]]
  if (is.null(val)) key else val
}

host_trf <- function(key, lang, ...) {
  template <- host_tr(key, lang)
  args <- list(...)
  if (length(args) < 1) return(template)
  tryCatch(
    do.call(sprintf, c(list(template), args)),
    error = function(e) template
  )
}

home_import_type_label <- function(type, lang) {
  type_norm <- tolower(normalize_home_scalar_chr(type, default = "processed_xlsx"))
  key_import <- paste0("home_import_type_", type_norm)
  val <- host_tr(key_import, lang)
  if (!identical(val, key_import)) return(val)

  key_export <- paste0("home_export_type_", type_norm)
  val2 <- host_tr(key_export, lang)
  if (!identical(val2, key_export)) return(val2)

  if (nzchar(type_norm)) toupper(type_norm) else host_tr("home_import_type_processed_xlsx", lang)
}

home_target_label <- function(step, lang) {
  step_norm <- normalize_home_scalar_chr(step, default = "step2")
  if (identical(step_norm, "step1")) return(host_tr("home_target_step1", lang))
  if (identical(step_norm, "step3")) return(host_tr("home_target_step3", lang))
  host_tr("home_target_step2", lang)
}

home_detect_export_type <- function(file_name = NULL, fallback = "xlsx") {
  file_norm <- tolower(normalize_home_scalar_chr(file_name, default = ""))
  if (grepl("\\.pdf$", file_norm)) return("pdf")
  if (grepl("\\.png$", file_norm)) return("png")
  if (grepl("\\.tiff?$", file_norm)) return("tiff")
  if (grepl("\\.txt$", file_norm)) return("txt")
  if (grepl("\\.json$", file_norm)) return("json")
  if (grepl("\\.xlsx$", file_norm)) return("xlsx")
  fallback_norm <- tolower(normalize_home_scalar_chr(fallback, default = "xlsx"))
  if (!nzchar(fallback_norm)) "xlsx" else fallback_norm
}

repo_root <- detect_repo_root()
icon_assets_dir <- file.path(repo_root, "icons")
if (dir.exists(icon_assets_dir)) {
  try(shiny::addResourcePath("itcsuite_assets", icon_assets_dir), silent = TRUE)
}
processor_legacy <- load_legacy_app(file.path(repo_root, "ITCprocessor"), "ITCprocessor")
simfit_legacy <- load_legacy_app(
  file.path(repo_root, "ITCsimfit"),
  "ITCsimfit",
  required_symbols = c(
    "calculate_weighted_robust_loss",
    "calculate_weights_from_derivative",
    "huber_loss",
    "calculate_huber_delta"
  )
)
graph_legacy <- load_legacy_app(file.path(repo_root, "ITCgraph"), "ITCgraph")

ui <- fluidPage(
  tags$head(
    tags$title("ViaBind: Your Path, Your Model"),
    tags$style(HTML("\
      .main-host-wrap { margin-top: 6px; }\
      .main-host-topbar { position: relative; }\
      .main-host-wrap .tab-content { padding-top: 8px; }\
      .main-host-wrap .nav-tabs { padding-right: 140px; }\
      .main-host-brand { color: #4b5563; font-size: 13px; margin: 6px 0 2px; }\
      .main-host-brand a { color: inherit; text-decoration: underline; }\
      .main-host-lang-switch { position: absolute; top: 5px; right: 0; z-index: 10; }\
      .main-host-lang-switch .btn { min-width: 92px; }\
      .home-tab-wrap { padding: 4px 2px 12px; }\
      .home-panel { background: #f8fafc; border: 1px solid #d8e2ef; border-radius: 10px; padding: 16px; margin-bottom: 12px; }\
      .home-title-row { display: flex; align-items: center; gap: 10px; margin-bottom: 8px; }\
      .home-title-icon { width: 44px; height: 44px; border-radius: 10px; object-fit: cover; box-shadow: 0 1px 4px rgba(0,0,0,0.15); }\
      .home-panel h3 { margin-top: 0; margin-bottom: 8px; }\
      .home-panel p { margin-bottom: 10px; color: #374151; }\
      .home-action-row { display: flex; gap: 8px; flex-wrap: wrap; margin-bottom: 14px; }\
      .home-recent-table-wrap { overflow-x: auto; overflow-y: auto; height: 454px; }\
      .home-recent-table-empty { display: flex; align-items: center; justify-content: center; background: #fff; border: 1px solid #e5e7eb; }\
      .home-recent-table { width: 100%; border-collapse: collapse; table-layout: fixed; background: #fff; }\
      .home-recent-table th, .home-recent-table td { border: 1px solid #e5e7eb; padding: 8px; font-size: 13px; vertical-align: middle; }\
      .home-recent-table th { background: #eff6ff; font-weight: 600; position: sticky; top: 0; z-index: 1; }\
      .home-recent-table td { word-break: break-word; }\
      .home-recent-name { font-weight: 600; color: #111827; line-height: 1.2; }\
      .home-recent-path { margin-top: 4px; font-size: 11px; color: #6b7280; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }\
      .home-recent-path-missing { color: #b91c1c; font-weight: 600; }\
      .home-empty-note { color: #6b7280; font-size: 13px; }\
    ")),
    tags$script(HTML("
      (function() {
        var disconnectTimer = null;
        var disconnectReloadKey = 'itcsuite.auto_reload_ts';
        var disconnectGraceMs = 8000;
        var disconnectReloadCooldownMs = 60000;

        function normalizeLang(value) {
          var v = (value || '').toLowerCase();
          return v === 'zh' ? 'zh' : 'en';
        }

        function detectInitialLang() {
          try {
            var saved = localStorage.getItem('itcsuite.lang');
            if (saved) return normalizeLang(saved);
          } catch (e) {}
          var navLang = ((navigator.language || navigator.userLanguage || '') + '').toLowerCase();
          if (navLang.indexOf('zh') === 0) return 'zh';
          return 'en';
        }

        function clearDisconnectTimer() {
          if (!disconnectTimer) return;
          clearTimeout(disconnectTimer);
          disconnectTimer = null;
        }

        function canAutoReload() {
          try {
            var raw = sessionStorage.getItem(disconnectReloadKey) || '0';
            var last = parseInt(raw, 10);
            if (!Number.isFinite(last)) return true;
            return (Date.now() - last) >= disconnectReloadCooldownMs;
          } catch (e) {
            return true;
          }
        }

        function markAutoReload() {
          try {
            sessionStorage.setItem(disconnectReloadKey, String(Date.now()));
          } catch (e) {}
        }

        function tryAutoReload() {
          if (!canAutoReload()) return;
          markAutoReload();
          window.location.reload();
        }

        Shiny.addCustomMessageHandler('itcsuite_i18n_set_lang', function(msg) {
          if (!msg || !msg.lang) return;
          try {
            localStorage.setItem('itcsuite.lang', normalizeLang(msg.lang));
          } catch (e) {}
        });

        Shiny.addCustomMessageHandler('itcsuite_i18n_tab_labels', function(msg) {
          if (!msg) return;
          if (msg.home) $('#main_tab_label_home').text(msg.home);
          if (msg.step1) $('#main_tab_label_step1').text(msg.step1);
          if (msg.step2) $('#main_tab_label_step2').text(msg.step2);
          if (msg.step3) $('#main_tab_label_step3').text(msg.step3);
        });

        $(document).on('shiny:connected', function() {
          Shiny.setInputValue('itcsuite_lang_init', detectInitialLang(), {priority: 'event'});
        });

        $(document).on('shiny:disconnected', function() {
          clearDisconnectTimer();
          disconnectTimer = setTimeout(function() {
            disconnectTimer = null;
            tryAutoReload();
          }, disconnectGraceMs);
        });

        $(document).on('shiny:reconnected', function() {
          clearDisconnectTimer();
        });

        window.addEventListener('beforeunload', clearDisconnectTimer);
      })();
    "))
  ),
  div(
    class = "main-host-wrap",
    div(
      class = "main-host-topbar",
      tabsetPanel(
        id = "main_tabs",
        selected = "home",
        tabPanel(tags$span(id = "main_tab_label_home", "Home"), value = "home", uiOutput("home_panel_ui")),
        tabPanel(tags$span(id = "main_tab_label_step1", "Step 1 Baseline & Integration"), value = "step1", uiOutput("legacy_processor_ui")),
        tabPanel(tags$span(id = "main_tab_label_step2", "Step 2 Simulation & Fitting"), value = "step2", uiOutput("legacy_simfit_ui")),
        tabPanel(tags$span(id = "main_tab_label_step3", "Step 3 Plot & Export"), value = "step3", uiOutput("legacy_graph_ui"))
      ),
      div(
        class = "main-host-lang-switch",
        actionButton("host_lang_toggle", "\U0001F1E8\U0001F1F3 中文", class = "btn btn-default btn-sm")
      )
    )
  )
)

server <- function(input, output, session) {
  # [COMMENT_STD][ERROR_SEMANTICS]
  # 错误码/类别：启动错误统一走 fail_fast；运行态桥接错误由 validator 拒绝并 warning。
  # 触发条件：缺失子应用符号、bridge payload 不合法、路径探测失败。
  # 用户可见性：启动失败为阻断错误；运行态桥接拒绝为非阻断告警。
  # 日志级别：启动阶段 error；运行阶段 warning。
  # 恢复动作：启动阶段停止应用；运行阶段忽略本次无效 payload 并保留既有状态。
  try(session$allowReconnect(TRUE), silent = TRUE)
  bridge_bus <- bridge_bus_server("bridge_bus")
  bridge_s1s2 <- bridge_step1_to_step2_server("bridge_s1s2", bridge_bus)
  bridge_s2s3 <- bridge_step2_to_step3_server("bridge_s2s3", bridge_bus)

  host_lang <- reactiveVal("en")
  host_lang_token <- reactiveVal(0)
  set_host_lang <- function(lang, persist = TRUE) {
    normalized <- normalize_lang(lang)
    if (!identical(host_lang(), normalized)) {
      host_lang(normalized)
      host_lang_token(host_lang_token() + 1)
    }
    if (isTRUE(persist)) {
      session$sendCustomMessage("itcsuite_i18n_set_lang", list(lang = normalized))
    }
    invisible(normalized)
  }

  home_recent_max_records <- home_recent_store_max_records_default()
  load_home_recent_state <- function() {
    home_recent_store_load(max_records = home_recent_max_records)
  }
  initial_home_recent_state <- load_home_recent_state()

  home_state <- reactiveValues(
    import_records = initial_home_recent_state$import_records %||% list(),
    next_seq = initial_home_recent_state$next_seq %||% 0L,
    import_observer_ids = character(0)
  )
  home_restore_handlers <- new.env(parent = emptyenv())

  normalize_recent_path <- function(path) {
    p <- normalize_home_scalar_chr(path, default = "")
    if (!nzchar(p)) return("")
    tryCatch(normalizePath(p, winslash = "/", mustWork = FALSE), error = function(e) p)
  }

  resolve_home_user_data_dir <- function() {
    base_dir <- normalize_home_scalar_chr(Sys.getenv("ITCSUITE_USER_DATA_DIR", unset = ""), default = "")
    if (!nzchar(base_dir)) {
      base_dir <- tools::R_user_dir("itcsuite", which = "data")
    }
    tryCatch(normalizePath(base_dir, winslash = "/", mustWork = FALSE), error = function(e) base_dir)
  }

  is_temporary_import_path <- function(path) {
    p <- normalize_recent_path(path)
    if (!nzchar(p)) return(FALSE)
    p_low <- tolower(p)
    if (startsWith(p_low, "/tmp/")) return(TRUE)
    if (startsWith(p_low, "/private/tmp/")) return(TRUE)
    if (startsWith(p_low, "/var/folders/")) return(TRUE)
    if (startsWith(p_low, "/private/var/folders/")) return(TRUE)
    grepl("/rtmp[^/]+/", p_low)
  }

  infer_recent_source_ext <- function(path, import_type = NULL) {
    ext <- tolower(tools::file_ext(path))
    if (nzchar(ext)) return(paste0(".", ext))
    import_type_norm <- normalize_home_scalar_chr(import_type, default = "")
    if (identical(import_type_norm, "itc")) return(".itc")
    ".xlsx"
  }

  persist_recent_import_source <- function(path, record_id, import_type, source_path_kind) {
    src <- normalize_recent_path(path)
    if (!nzchar(src)) return("")
    if (!identical(source_path_kind, "import")) return(src)
    if (!isTRUE(file.exists(src))) return(src)
    if (!is_temporary_import_path(src)) return(src)

    cache_dir <- file.path(resolve_home_user_data_dir(), "cache", "recent_imports")
    ok_dir <- tryCatch({
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
      TRUE
    }, error = function(e) FALSE)
    if (!isTRUE(ok_dir)) return(src)

    ext <- infer_recent_source_ext(src, import_type = import_type)
    cache_path <- file.path(cache_dir, paste0(record_id, ext))
    ok_copy <- tryCatch(
      isTRUE(file.copy(src, cache_path, overwrite = TRUE, copy.date = TRUE)),
      error = function(e) FALSE
    )
    if (!isTRUE(ok_copy)) return(src)
    normalize_recent_path(cache_path)
  }

  recent_path_exists <- function(path) {
    p <- normalize_recent_path(path)
    if (!nzchar(p)) return(FALSE)
    isTRUE(file.exists(p))
  }

  with_recent_path_status <- function(records) {
    if (!is.list(records)) return(list())
    lapply(records, function(rec) {
      if (!is.list(rec)) return(rec)
      rec$path_exists <- recent_path_exists(rec$source_path)
      rec
    })
  }

  persist_home_recent_state <- function() {
    state <- list(
      schema_version = home_recent_store_schema(),
      next_seq = suppressWarnings(as.integer(home_state$next_seq)[1]),
      import_records = home_state$import_records %||% list(),
      updated_at = paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%S", tz = "UTC"), "Z")
    )
    invisible(home_recent_store_save(state, max_records = home_recent_max_records))
  }

  next_home_record_id <- function() {
    seq_num <- suppressWarnings(as.integer(home_state$next_seq)[1])
    if (!is.finite(seq_num) || seq_num < 0L) seq_num <- 0L
    seq_num <- seq_num + 1L
    home_state$next_seq <- seq_num
    sprintf("home_rec_%06d", seq_num)
  }

  normalize_recent_display_name <- function(x) {
    out <- normalize_home_scalar_chr(x, default = "")
    if (!nzchar(out)) out <- host_tr("home_unknown_name", host_lang())
    out
  }

  valid_target_step <- function(step) {
    st <- normalize_home_scalar_chr(step, default = "")
    st %in% c("step1", "step2", "step3")
  }

  add_recent_record <- function(kind = c("import", "export"), record) {
    kind <- match.arg(kind)
    if (!identical(kind, "import")) return(invisible(NULL))
    rec <- if (is.list(record)) record else list()

    display_name <- normalize_recent_display_name(rec$display_name %||% rec$file_name)
    file_name <- normalize_home_scalar_chr(rec$file_name, default = display_name)
    entry_type <- ""
    target_step <- ""
    sheets <- if (is.list(rec$sheets)) rec$sheets else NULL
    fallback_type <- normalize_home_scalar_chr(rec$import_type, default = "processed_xlsx")
    entry_type <- home_detect_import_type(
      file_name = file_name,
      sheets = sheets,
      fallback = fallback_type
    )
    if (isTRUE(valid_target_step(rec$target_step))) {
      target_step <- normalize_home_scalar_chr(rec$target_step, default = "")
    } else {
      target_step <- home_target_step_from_import_type(entry_type)
    }

    source_step <- normalize_home_scalar_chr(rec$source_step, default = target_step)
    imported_at <- normalize_home_scalar_chr(
      rec$imported_at,
      default = paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%S", tz = "UTC"), "Z")
    )
    source_path_kind <- normalize_home_scalar_chr(rec$source_path_kind, default = "artifact")
    if (!source_path_kind %in% c("artifact", "import")) source_path_kind <- "artifact"
    record_id <- next_home_record_id()
    source_path <- persist_recent_import_source(
      path = rec$source_path,
      record_id = record_id,
      import_type = entry_type,
      source_path_kind = source_path_kind
    )
    artifact_path_input <- normalize_recent_path(rec$artifact_path)
    artifact_path <- if (nzchar(artifact_path_input)) artifact_path_input else source_path

    entry <- list(
      id = record_id,
      import_type = entry_type,
      display_name = display_name,
      target_step = target_step,
      imported_at = imported_at,
      source_step = source_step,
      source_path = source_path,
      artifact_path = artifact_path,
      source_path_kind = source_path_kind,
      restore_payload_key = ""
    )

    current_records <- home_state$import_records
    if (!is.list(current_records)) current_records <- list()
    merged <- c(list(entry), current_records)
    trimmed <- home_trim_recent_records(merged, max_records = home_recent_max_records)
    home_state$import_records <- trimmed$records
    persist_home_recent_state()

    invisible(entry)
  }

  add_recent_import <- function(record) {
    add_recent_record("import", record = record)
  }

  add_recent_export <- function(record) {
    invisible(NULL)
  }

  promote_recent_import_paths_to_cache <- function() {
    recs <- home_state$import_records
    if (!is.list(recs) || length(recs) < 1) return(invisible(FALSE))

    changed <- FALSE
    migrated <- lapply(recs, function(rec) {
      if (!is.list(rec)) return(rec)
      record_id <- normalize_home_scalar_chr(rec$id, default = "")
      if (!nzchar(record_id)) return(rec)
      source_kind <- normalize_home_scalar_chr(rec$source_path_kind, default = "artifact")
      if (!source_kind %in% c("artifact", "import")) source_kind <- "artifact"

      old_source <- normalize_recent_path(rec$source_path)
      new_source <- persist_recent_import_source(
        path = old_source,
        record_id = record_id,
        import_type = rec$import_type,
        source_path_kind = source_kind
      )
      if (!identical(new_source, old_source)) {
        rec$source_path <- new_source
        artifact_path <- normalize_recent_path(rec$artifact_path)
        if (!nzchar(artifact_path)) {
          rec$artifact_path <- new_source
        }
        changed <<- TRUE
      }
      rec
    })

    if (!isTRUE(changed)) return(invisible(FALSE))
    home_state$import_records <- migrated
    persist_home_recent_state()
    invisible(TRUE)
  }

  observeEvent(TRUE, {
    promote_recent_import_paths_to_cache()
  }, once = TRUE, ignoreInit = FALSE)

  register_restore_handler <- function(step, fn) {
    step_norm <- normalize_home_scalar_chr(step, default = "")
    if (!nzchar(step_norm) || !is.function(fn)) return(invisible(FALSE))
    assign(step_norm, fn, envir = home_restore_handlers)
    invisible(TRUE)
  }

  resolve_restore_handler <- function(step) {
    step_norm <- normalize_home_scalar_chr(step, default = "")
    if (!exists(step_norm, envir = home_restore_handlers, inherits = FALSE)) return(NULL)
    handler <- get(step_norm, envir = home_restore_handlers, inherits = FALSE)
    if (is.function(handler)) handler else NULL
  }

  find_recent_record <- function(record_id, kind = c("import", "export")) {
    kind <- match.arg(kind)
    if (!identical(kind, "import")) return(NULL)
    id <- normalize_home_scalar_chr(record_id, default = "")
    if (!nzchar(id)) return(NULL)
    recs <- home_state$import_records
    if (!is.list(recs) || length(recs) < 1) return(NULL)
    idx <- which(vapply(recs, function(rec) {
      is.list(rec) && identical(normalize_home_scalar_chr(rec$id, default = ""), id)
    }, logical(1)))
    if (length(idx) < 1) return(NULL)
    recs[[idx[1]]]
  }

  format_recent_imported_at <- function(value) {
    val <- normalize_home_scalar_chr(value, default = "")
    if (!nzchar(val)) return("")
    ts_num <- home_parse_imported_at(val)
    if (is.finite(ts_num)) {
      local_tz <- Sys.timezone()
      if (is.null(local_tz) || !nzchar(local_tz)) local_tz <- ""
      return(format(as.POSIXct(ts_num, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S", tz = local_tz))
    }
    val
  }

  format_recent_path_short <- function(path, max_chars = 72L) {
    p <- normalize_recent_path(path)
    if (!nzchar(p)) return("")
    max_n <- suppressWarnings(as.integer(max_chars)[1])
    if (!is.finite(max_n) || max_n < 14L) max_n <- 72L
    if (nchar(p, type = "width") <= max_n) return(p)
    left_n <- as.integer(floor((max_n - 3L) / 2L))
    right_n <- as.integer(max_n - left_n - 3L)
    paste0(substr(p, 1L, left_n), "...", substr(p, nchar(p) - right_n + 1L, nchar(p)))
  }

  restore_recent_record <- function(record_id, kind = c("import", "export")) {
    kind <- match.arg(kind)
    if (!identical(kind, "import")) return(invisible(FALSE))
    rec <- find_recent_record(record_id, kind = kind)
    if (is.null(rec) || !is.list(rec)) return(invisible(FALSE))

    display_name <- normalize_recent_display_name(rec$display_name)
    target_step <- normalize_home_scalar_chr(rec$target_step, default = "step2")
    target_label <- home_target_label(target_step, host_lang())
    source_path <- normalize_recent_path(rec$source_path)

    if (!nzchar(source_path)) {
      showNotification(
        host_trf("home_restore_failed_no_payload", host_lang(), display_name),
        type = "warning",
        duration = 4
      )
      return(invisible(FALSE))
    }
    if (!isTRUE(file.exists(source_path))) {
      showNotification(
        host_trf("home_restore_failed_path_missing", host_lang(), display_name),
        type = "warning",
        duration = 4
      )
      return(invisible(FALSE))
    }

    handler <- resolve_restore_handler(target_step)
    if (!is.function(handler)) {
      showNotification(
        host_trf("home_restore_failed_no_handler", host_lang(), target_label),
        type = "warning",
        duration = 4
      )
      return(invisible(FALSE))
    }

    restored_ok <- tryCatch(
      isTRUE(handler(rec)),
      error = function(e) {
        showNotification(
          host_trf("home_restore_failed_general", host_lang(), conditionMessage(e)),
          type = "error",
          duration = 5
        )
        FALSE
      }
    )
    if (!isTRUE(restored_ok)) return(invisible(FALSE))

    tryCatch(updateTabsetPanel(session, "main_tabs", selected = target_step), error = function(e) NULL)
    showNotification(
      host_trf("home_restore_ok", host_lang(), display_name, target_label),
      type = "message",
      duration = 3
    )
    invisible(TRUE)
  }

  session$userData$itcsuite_i18n <- list(
    get_lang = function() {
      host_lang()
    },
    set_lang = function(lang) {
      set_host_lang(lang, persist = TRUE)
    },
    lang_token = function() {
      host_lang_token()
    }
  )

  session$userData$itcsuite_home <- list(
    add_recent_import = function(record, payload = NULL) {
      add_recent_import(record)
    },
    add_recent_export = function(record, payload = NULL) {
      invisible(NULL)
    },
    register_restore_handler = function(step, fn) {
      register_restore_handler(step, fn)
    },
    restore_recent_record = function(record_id, kind = "import") {
      restore_recent_record(record_id, kind = kind)
    },
    get_recent_imports = function() {
      with_recent_path_status(home_state$import_records)
    },
    get_recent_exports = function() {
      list()
    }
  )

  observeEvent(input$itcsuite_lang_init, {
    set_host_lang(input$itcsuite_lang_init, persist = TRUE)
  }, ignoreInit = TRUE, once = TRUE)

  observeEvent(input$host_lang_toggle, {
    next_lang <- if (identical(host_lang(), "en")) "zh" else "en"
    set_host_lang(next_lang, persist = TRUE)
  }, ignoreInit = TRUE)

  observeEvent(host_lang(), {
    session$sendCustomMessage("itcsuite_i18n_tab_labels", list(
      home = host_tr("home", host_lang()),
      step1 = host_tr("step1", host_lang()),
      step2 = host_tr("step2", host_lang()),
      step3 = host_tr("step3", host_lang())
    ))
    updateActionButton(
      session,
      "host_lang_toggle",
      label = if (identical(host_lang(), "en")) "\U0001F1E8\U0001F1F3 中文" else "\U0001F1EC\U0001F1E7 English"
    )
  }, ignoreInit = FALSE)

  output$home_panel_ui <- renderUI({
    l <- host_lang()
    import_recs <- with_recent_path_status(home_state$import_records)
    if (!is.list(import_recs)) import_recs <- list()

    build_recent_table <- function(recs, empty_text) {
      if (!is.list(recs) || length(recs) < 1) {
        return(
          div(
            class = "home-recent-table-wrap home-recent-table-empty",
            div(class = "home-empty-note", empty_text)
          )
        )
      }
      header <- tags$tr(
        tags$th(host_tr("home_table_col_name", l)),
        tags$th(host_tr("home_table_col_type", l)),
        tags$th(host_tr("home_table_col_target", l)),
        tags$th(host_tr("home_table_col_time", l)),
        tags$th(host_tr("home_table_col_action", l))
      )
      colgroup <- tags$colgroup(
        tags$col(style = "width:30%;"),
        tags$col(style = "width:15%;"),
        tags$col(style = "width:15%;"),
        tags$col(style = "width:25%;"),
        tags$col(style = "width:15%;")
      )
      rows <- lapply(recs, function(rec) {
        if (!is.list(rec)) return(NULL)
        rec_id <- normalize_home_scalar_chr(rec$id, default = "")
        if (!nzchar(rec_id)) return(NULL)
        action_id <- paste0("home_restore_import_", rec_id)
        source_path <- normalize_recent_path(rec$source_path)
        path_exists <- isTRUE(rec$path_exists)
        path_body <- if (path_exists) {
          paste0(host_tr("home_recent_path_label", l), ": ", format_recent_path_short(source_path))
        } else {
          host_tr("home_recent_path_missing", l)
        }
        path_class <- if (path_exists) "home-recent-path" else "home-recent-path home-recent-path-missing"
        action_btn <- actionButton(action_id, host_tr("home_restore_action", l), class = "btn btn-primary btn-xs")
        if (!path_exists) {
          action_btn <- tagAppendAttributes(action_btn, disabled = "disabled")
        }
        tags$tr(
          tags$td(
            div(class = "home-recent-name", normalize_recent_display_name(rec$display_name)),
            div(class = path_class, title = source_path, path_body)
          ),
          tags$td(home_import_type_label(rec$import_type, l)),
          tags$td(home_target_label(rec$target_step, l)),
          tags$td(format_recent_imported_at(rec$imported_at)),
          tags$td(action_btn)
        )
      })
      rows <- Filter(Negate(is.null), rows)
      div(
        class = "home-recent-table-wrap",
        tags$table(
          class = "home-recent-table",
          colgroup,
          tags$thead(header),
          tags$tbody(rows)
        )
      )
    }

    home_icon <- file.path("/itcsuite_assets", "ViaBind_1024.png")

    div(
      class = "home-tab-wrap",
      div(
        class = "home-panel",
        div(
          class = "home-title-row",
          tags$img(class = "home-title-icon", src = home_icon, alt = "ViaBind"),
          tags$h3(host_tr("home_welcome_title", l))
        ),
        tags$p(host_tr("home_welcome_desc", l)),
        div(
          class = "home-action-row",
          actionButton("home_start_step1", host_tr("home_start_step1", l), class = "btn btn-primary"),
          actionButton("home_beginner_guide", host_tr("home_guide_button", l), class = "btn btn-info")
        ),
        tags$h4(host_tr("home_recent_title", l)),
        build_recent_table(import_recs, host_tr("home_recent_empty", l))
      )
    )
  })

  observe({
    recs <- home_state$import_records
    if (!is.list(recs) || length(recs) < 1) return()
    ids <- vapply(recs, function(rec) {
      if (!is.list(rec)) return("")
      normalize_home_scalar_chr(rec$id, default = "")
    }, character(1))
    ids <- ids[nzchar(ids)]
    known <- home_state$import_observer_ids %||% character(0)
    new_ids <- setdiff(ids, known)
    if (length(new_ids) < 1) return()

    for (rid in new_ids) {
      local({
        rec_id <- rid
        observeEvent(input[[paste0("home_restore_import_", rec_id)]], {
          restore_recent_record(rec_id, kind = "import")
        }, ignoreInit = TRUE)
      })
    }
    home_state$import_observer_ids <- unique(c(known, new_ids))
  })

  observeEvent(input$home_start_step1, {
    updateTabsetPanel(session, "main_tabs", selected = "step1")
  }, ignoreInit = TRUE)

  observeEvent(input$home_beginner_guide, {
    showModal(modalDialog(
      title = host_tr("home_guide_title", host_lang()),
      tags$p(host_tr("home_guide_body_1", host_lang())),
      tags$p(host_tr("home_guide_body_2", host_lang())),
      tags$p(host_tr("home_guide_body_3", host_lang())),
      tags$p(host_tr("home_guide_body_4", host_lang())),
      tags$p(tags$em(host_tr("home_guide_placeholder", host_lang()))),
      easyClose = TRUE,
      footer = modalButton(host_tr("close", host_lang()))
    ))
  }, ignoreInit = TRUE)

  session$userData$itcsuite_bridge <- list(
    step1_payload = bridge_bus$step1_payload,
    step2_plot_payload = bridge_bus$step2_plot_payload,
    bridge_s1s2_token = bridge_s1s2$latest_token,
    bridge_s2s3_token = bridge_s2s3$latest_token
  )

  output$legacy_processor_ui <- renderUI({
    processor_legacy$ui
  })

  output$legacy_simfit_ui <- renderUI({
    simfit_legacy$ui
  })

  output$legacy_graph_ui <- renderUI({
    graph_legacy$ui
  })

  processor_legacy$server(input, output, session)
  simfit_legacy$server(input, output, session)
  graph_legacy$server(input, output, session)
}

shinyApp(ui, server)
