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
source("R/home_sleep_restore_store.R")
source("R/home_desktop_helpers.R")
source("R/home_contact_helpers.R")
source("R/telemetry.R")

if (!exists("load_guide_annotations", mode = "function")) {
  fail_fast("Startup check failed: guide annotation loader is unavailable.")
}
if (!exists("home_detect_import_type", mode = "function")) {
  fail_fast("Startup check failed: home recent helper is unavailable.")
}
if (!exists("home_recent_store_load", mode = "function")) {
  fail_fast("Startup check failed: home recent store helper is unavailable.")
}
if (!exists("home_sleep_restore_store_load", mode = "function")) {
  fail_fast("Startup check failed: sleep restore store helper is unavailable.")
}
if (!exists("home_desktop_normalize_open_file_result", mode = "function")) {
  fail_fast("Startup check failed: home desktop helper is unavailable.")
}
if (!exists("home_contact_resolve_qr_src", mode = "function")) {
  fail_fast("Startup check failed: home contact helper is unavailable.")
}
if (!exists("telemetry_create_session", mode = "function")) {
  fail_fast("Startup check failed: telemetry helper is unavailable.")
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

register_home_icon_resource <- function(repo_root) {
  get_resource_path <- function(prefix) {
    paths <- shiny::resourcePaths()
    val <- unname(paths[prefix])
    out <- normalize_home_scalar_chr(val, default = "")
    if (is.na(out)) "" else out
  }

  prefix <- "itcsuite_home_assets"
  icon_candidates <- list(
    list(
      dir = file.path(repo_root, "icons"),
      file = "ViaBind_1024.png"
    ),
    list(
      dir = normalizePath(file.path(repo_root, ".."), winslash = "/", mustWork = FALSE),
      file = "icon.png"
    )
  )

  for (cand in icon_candidates) {
    dir_norm <- normalizePath(cand$dir, winslash = "/", mustWork = FALSE)
    icon_file <- file.path(dir_norm, cand$file)
    if (!dir.exists(dir_norm) || !file.exists(icon_file)) next

    existing_dir <- get_resource_path(prefix)
    if (is.character(existing_dir) && nzchar(existing_dir)) {
      existing_norm <- normalizePath(existing_dir, winslash = "/", mustWork = FALSE)
      if (!identical(existing_norm, dir_norm)) {
        try(shiny::removeResourcePath(prefix), silent = TRUE)
      }
    }

    registered <- FALSE
    tryCatch({
      shiny::addResourcePath(prefix, dir_norm)
      registered <- TRUE
    }, error = function(e) {
      mapped <- get_resource_path(prefix)
      mapped_norm <- normalizePath(mapped %||% "", winslash = "/", mustWork = FALSE)
      if (identical(mapped_norm, dir_norm)) {
        registered <<- TRUE
      }
    })

    if (isTRUE(registered)) {
      return(file.path("/", prefix, cand$file))
    }
  }

  NULL
}

resolve_home_icon_src <- function(repo_root) {
  bundled_icon <- file.path(repo_root, "ITCSuiteWeb", "www", "assets", "ViaBind_1024.png")
  if (file.exists(bundled_icon)) {
    return("/assets/ViaBind_1024.png")
  }
  register_home_icon_resource(repo_root)
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

bridge_bus_server <- function(id, on_reject = NULL) {
  # [COMMENT_STD][IO_CONTRACT]
  # 输入来源：moduleServer session 生命周期内的桥接读写调用。
  # 字段/类型：step1_payload/step2_plot_payload 为 list 载荷，内部含 schema_version/token 等字段。
  # 单位：token 采用 numeric 标量；时间戳由 payload 自身定义（ISO 字符串）。
  # 空值策略：validator 拒绝无效 payload；显式传入 NULL 时清空 channel。
  # 输出保证：返回包含两个 channel function 的 list，供步骤间共享。
  moduleServer(id, function(input, output, session) {
    step1_payload <- make_bridge_channel(
      sanitize_step1_payload,
      "step1_payload",
      reject_explainer = explain_step1_payload_rejection,
      on_reject = on_reject
    )
    step2_plot_payload <- make_bridge_channel(
      sanitize_step2_plot_payload,
      "step2_plot_payload",
      reject_explainer = explain_step2_plot_payload_rejection,
      on_reject = on_reject
    )
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
      home_contact_title = "Contact Developer",
      home_contact_dev_name_label = "Developer",
      home_contact_email_label = "Email",
      home_contact_website_label = "Website",
      home_contact_version_label = "Version",
      home_contact_citation_title = "Citation:",
      home_contact_citation_intro = "If you use this software in published work, please cite:",
      home_contact_citation_copy_btn = "Copy citation",
      home_contact_citation_copy_ok = "Citation copied.",
      home_contact_citation_copy_failed = "Copy failed. Please copy manually.",
      home_contact_donate_title = "Support ViaBind",
      home_contact_donate_link_label = "Buy Me a Coffee",
      home_contact_donate_note_line1 = "If this tool has made your work a little easier, you are welcome to support its continued development.",
      home_contact_donate_note_line2 = "All features will always remain freely available.",
      home_contact_donate_note_line3 = "",
      home_contact_qr_missing = "Donation QR code is not available yet.",
      close = "Close"
    ),
    zh = list(
      home = "首页",
      step1 = "Step 1 基线校正 & 积分",
      step2 = "Step 2 模拟 & 拟合",
      step3 = "Step 3 绘图 & 导出",
      home_welcome_title = "欢迎使用 ViaBind",
      home_welcome_desc = "您可以从 Step 1 开始，或从下方调用最近导入的数据。",
      home_start_step1 = "进入 Step 1",
      home_recent_title = "最近导入",
      home_recent_empty = "当前会话暂无最近导入记录。",
      home_recent_export_title = "最近导出",
      home_recent_export_empty = "当前会话暂无最近导出记录。",
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
      home_contact_title = "联系开发者",
      home_contact_dev_name_label = "开发者",
      home_contact_email_label = "邮箱",
      home_contact_website_label = "网址",
      home_contact_version_label = "版本",
      home_contact_citation_title = "引用：",
      home_contact_citation_intro = "如果您在发表工作中使用本软件，请引用：",
      home_contact_citation_copy_btn = "复制引用",
      home_contact_citation_copy_ok = "引用已复制。",
      home_contact_citation_copy_failed = "复制失败，请手动复制。",
      home_contact_donate_title = "支持 ViaBind",
      home_contact_donate_link_label = "Buy Me a Coffee",
      home_contact_donate_note_line1 = "功能永久免费。",
      home_contact_donate_note_line2 = "用得顺手？欢迎支持。",
      home_contact_donate_note_line3 = "我们负责把它做得更好。",
      home_contact_qr_missing = "捐赠二维码暂不可用。",
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
home_icon_src <- resolve_home_icon_src(repo_root)
home_contact_assets_dir <- file.path(repo_root, "ITCSuiteWeb", "www", "assets")
home_contact_profile <- list(
  name = "Guanglu Wu (吴光鹭)",
  email = "guanglu.wu@gmail.com",
  website = "https://guanglu.xyz"
)
home_contact_bmc_url <- home_contact_validate_https_url("https://buymeacoffee.com/guanglu")
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
      :root { --itcsuite-vh: 1vh; --itcsuite-host-chrome: 140px; --home-contact-qr-max-h: 132px; }\
      html, body { height: 100%; }\
      body > .container-fluid { height: 100%; }\
      .main-host-wrap { margin-top: 6px; height: calc(var(--itcsuite-vh, 1vh) * 100 - 12px); min-height: 0; overflow: hidden; }\
      .main-host-topbar { position: relative; height: 100%; display: flex; flex-direction: column; min-height: 0; }\
      .main-host-topbar > .tabbable { flex: 1 1 auto; min-height: 0; display: flex; flex-direction: column; }\
      .main-host-wrap .tab-content { padding-top: 8px; flex: 1 1 auto; min-height: 0; overflow-y: auto; overflow-x: hidden; }\
      .main-host-wrap .tab-content > .tab-pane { min-height: 100%; }\
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
      .home-contact-panel { margin-top: 14px; padding: 12px; border: 1px solid #d8e2ef; border-radius: 8px; background: #ffffff; }\
      .home-contact-grid { display: grid; grid-template-columns: minmax(0, 1fr) minmax(0, 1.1fr) minmax(260px, 0.9fr) auto; gap: 10px 8px; align-items: start; }\
      .home-contact-col-title { margin: 0 0 8px; font-size: 15px; font-weight: 600; color: #111827; }\
      .home-contact-lines p { margin: 0 0 6px; color: #1f2937; }\
      .home-contact-lines p:last-child { margin-bottom: 0; }\
      .home-contact-donate-note { margin: 2px 0 6px; color: #374151; }\
      .home-contact-col-dev { justify-self: start; text-align: left; }\
      .home-contact-col-citation { justify-self: stretch; text-align: left; }\
      .home-contact-lines p.home-contact-citation-line { font-size: 13px; line-height: 1.35; color: #1f2937; word-break: break-word; margin-left: 18px; }\
      .home-contact-citation-copy { margin-top: 4px; }\
      .home-contact-col-donate { justify-self: end; text-align: left; max-width: 360px; }\
      .home-contact-col-qr { justify-self: end; text-align: left; align-self: start; }\
      .home-contact-link { word-break: break-all; }\
      .home-contact-qr-wrap { margin-top: 0; }\
      .home-contact-qr { display: block; width: auto; height: auto; max-height: var(--home-contact-qr-max-h, 132px); max-width: 156px; object-fit: contain; border: 1px solid #e5e7eb; border-radius: 8px; background: #fff; padding: 4px; }\
      .home-contact-qr-missing { font-size: 12px; color: #6b7280; max-width: 220px; }\
      @media (max-width: 1200px) {\
        .home-contact-grid { grid-template-columns: 1fr 1fr; }\
        .home-contact-col-dev { grid-column: 1; grid-row: 1; }\
        .home-contact-col-citation { grid-column: 2; grid-row: 1; }\
        .home-contact-col-donate { grid-column: 1; grid-row: 2; justify-self: start; max-width: none; }\
        .home-contact-col-qr { grid-column: 2; grid-row: 2; justify-self: end; }\
      }\
      @media (max-width: 900px) { .home-contact-grid { grid-template-columns: 1fr; } .home-contact-col-donate, .home-contact-col-qr { justify-self: start; max-width: none; } .home-contact-col-dev, .home-contact-col-citation, .home-contact-col-donate, .home-contact-col-qr { grid-column: auto; grid-row: auto; } }\
    ")),
    tags$script(HTML("
      (function() {
        var disconnectTimer = null;
        var disconnectReloadKey = 'itcsuite.auto_reload_ts';
        var disconnectGraceMs = 8000;
        var disconnectReloadCooldownMs = 60000;
        var viewportRafId = null;

        function isWindowsHost() {
          var platform = (navigator.platform || '') + ' ' + (navigator.userAgent || '');
          return /Windows/i.test(platform);
        }

        function updateViewportCssVars() {
          var root = document.documentElement;
          var vh = Math.max(window.innerHeight || 0, 1) * 0.01;
          root.style.setProperty('--itcsuite-vh', vh + 'px');
          root.classList.toggle('itcsuite-win', isWindowsHost());

          var activePane = document.querySelector('.main-host-wrap .tab-content > .tab-pane.active');
          var paneTop = activePane ? activePane.getBoundingClientRect().top : 0;
          var hostChrome = Math.max(96, Math.round((paneTop || 0) + 18));
          root.style.setProperty('--itcsuite-host-chrome', hostChrome + 'px');

          // Keep QR visually close to donate content by capping image height to donate column height.
          root.style.setProperty('--home-contact-qr-max-h', '132px');
          var donateCol = document.querySelector('.home-contact-col-donate');
          if (donateCol) {
            var donateHeight = Math.round((donateCol.getBoundingClientRect() || {}).height || 0);
            if (Number.isFinite(donateHeight) && donateHeight > 0) {
              var qrMax = Math.max(82, Math.min(150, donateHeight - 6));
              root.style.setProperty('--home-contact-qr-max-h', qrMax + 'px');
            }
          }
        }

        function scheduleViewportCssVarRefresh() {
          if (viewportRafId) return;
          viewportRafId = window.requestAnimationFrame(function() {
            viewportRafId = null;
            updateViewportCssVars();
          });
        }

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

        function isDesktopRuntime() {
          return !!(window.itcsuiteDesktop && typeof window.itcsuiteDesktop.openFile === 'function');
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
          // Desktop shell already owns wake/reconnect recovery; avoid a second
          // renderer reload from web-side disconnect timer after long sleep.
          if (isDesktopRuntime()) return;
          if (!canAutoReload()) return;
          markAutoReload();
          window.location.reload();
        }

        function desktopOpenFileSupported() {
          return !!(window.itcsuiteDesktop && typeof window.itcsuiteDesktop.openFile === 'function');
        }

        function reportDesktopCapability() {
          if (!(window.Shiny && typeof window.Shiny.setInputValue === 'function')) return;
          window.Shiny.setInputValue(
            'itcsuite_desktop_capability',
            {
              open_file: desktopOpenFileSupported(),
              ts: Date.now()
            },
            { priority: 'event' }
          );
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

        Shiny.addCustomMessageHandler('itcsuite_desktop_open_file', async function(msg) {
          var payload = msg || {};
          var fallback = {
            request_id: (payload.request_id || '').toString(),
            purpose: (payload.purpose || '').toString(),
            canceled: true,
            file_path: '',
            file_name: '',
            error: 'Desktop open file API unavailable.'
          };

          if (!desktopOpenFileSupported()) {
            Shiny.setInputValue('itcsuite_desktop_open_file_result', fallback, { priority: 'event' });
            return;
          }

          try {
            var result = await window.itcsuiteDesktop.openFile(payload);
            if (!result || typeof result !== 'object') {
              Shiny.setInputValue('itcsuite_desktop_open_file_result', fallback, { priority: 'event' });
              return;
            }
            Shiny.setInputValue('itcsuite_desktop_open_file_result', result, { priority: 'event' });
          } catch (e) {
            fallback.error = (e && e.message) ? e.message : 'Desktop open file failed.';
            Shiny.setInputValue('itcsuite_desktop_open_file_result', fallback, { priority: 'event' });
          }
        });

        Shiny.addCustomMessageHandler('itcsuite_copy_to_clipboard', async function(msg) {
          var payload = msg || {};
          var text = (payload.text || '').toString();
          var resultInputId = (payload.result_input_id || 'itcsuite_copy_citation_result').toString();
          var result = { ok: false, ts: Date.now(), error: '' };

          if (!text) {
            result.error = 'No text provided.';
            Shiny.setInputValue(resultInputId, result, { priority: 'event' });
            return;
          }

          try {
            if (navigator.clipboard && typeof navigator.clipboard.writeText === 'function') {
              await navigator.clipboard.writeText(text);
              result.ok = true;
              Shiny.setInputValue(resultInputId, result, { priority: 'event' });
              return;
            }
          } catch (e) {
            result.error = (e && e.message) ? e.message : 'Clipboard API failed.';
          }

          var textarea = null;
          try {
            textarea = document.createElement('textarea');
            textarea.value = text;
            textarea.setAttribute('readonly', '');
            textarea.style.position = 'fixed';
            textarea.style.opacity = '0';
            textarea.style.left = '-9999px';
            textarea.style.top = '0';
            document.body.appendChild(textarea);
            textarea.focus();
            textarea.select();
            var copied = document.execCommand('copy');
            if (!copied) throw new Error('execCommand returned false');
            result.ok = true;
            result.error = '';
          } catch (e2) {
            if (!result.error) {
              result.error = (e2 && e2.message) ? e2.message : 'Fallback copy failed.';
            }
          } finally {
            if (textarea && textarea.parentNode) textarea.parentNode.removeChild(textarea);
          }

          Shiny.setInputValue(resultInputId, result, { priority: 'event' });
        });

        $(document).on('shiny:connected', function() {
          scheduleViewportCssVarRefresh();
          Shiny.setInputValue('itcsuite_lang_init', detectInitialLang(), {priority: 'event'});
          reportDesktopCapability();
        });

        $(document).on('shiny:disconnected', function() {
          if (isDesktopRuntime()) return;
          clearDisconnectTimer();
          disconnectTimer = setTimeout(function() {
            disconnectTimer = null;
            tryAutoReload();
          }, disconnectGraceMs);
        });

        $(document).on('shiny:reconnected', function() {
          clearDisconnectTimer();
          scheduleViewportCssVarRefresh();
          reportDesktopCapability();
        });

        $(window).on('resize', scheduleViewportCssVarRefresh);
        $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', scheduleViewportCssVarRefresh);
        $(document).on('shiny:value', function(event) {
          if (event && event.name === 'home_panel_ui') {
            scheduleViewportCssVarRefresh();
          }
        });
        setTimeout(scheduleViewportCssVarRefresh, 0);
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
  is_desktop_runtime <- identical(
    home_desktop_scalar_chr(Sys.getenv("ITCSUITE_DESKTOP", unset = ""), default = ""),
    "1"
  )
  host_app_version <- home_contact_read_viabind_version(repo_root = repo_root, default_version = "0.0.0-dev")
  telemetry <- telemetry_create_session(
    app_version = host_app_version,
    runtime = if (isTRUE(is_desktop_runtime)) "desktop" else "web",
    retention_days = 30L
  )
  telemetry_log_event <- function(event, level = "INFO", module = "host", payload = list(), err = NULL, op_id = NULL, lang = "en") {
    fn <- telemetry$log_event
    if (!is.function(fn)) return(invisible(NULL))
    tryCatch(
      fn(event = event, level = level, module = module, payload = payload, err = err, op_id = op_id, lang = lang),
      error = function(e) invisible(NULL)
    )
  }
  telemetry_start_op <- function(event, module = "host", payload = list(), lang = "en") {
    fn <- telemetry$start_op
    if (!is.function(fn)) return("")
    tryCatch(fn(event = event, module = module, payload = payload, lang = lang), error = function(e) "")
  }
  telemetry_finish_op <- function(op_id, outcome = "ok", payload = list(), err = NULL, module = NULL, lang = "en", level = NULL) {
    fn <- telemetry$finish_op
    if (!is.function(fn)) return(invisible(NULL))
    tryCatch(
      fn(op_id = op_id, outcome = outcome, payload = payload, err = err, lang = lang, level = level),
      error = function(e) invisible(NULL)
    )
  }
  session$userData$itcsuite_telemetry <- list(
    trace_id = telemetry$trace_id %||% "",
    session_id = telemetry$session_id %||% "",
    runtime = telemetry$runtime %||% "",
    app_version = telemetry$app_version %||% host_app_version,
    logs_dir = telemetry$logs_dir %||% "",
    log_event = telemetry_log_event,
    start_op = telemetry_start_op,
    finish_op = telemetry_finish_op
  )

  bridge_reject_logger <- function(label, reason, payload = NULL) {
    telemetry_log_event(
      event = "bridge.validation",
      level = "WARN",
      module = "bridge",
      payload = list(
        channel = label,
        reason = reason,
        payload = payload
      ),
      lang = "en"
    )
  }
  bridge_set_reject_logger(bridge_reject_logger)

  bridge_bus <- bridge_bus_server("bridge_bus", on_reject = bridge_reject_logger)
  bridge_s1s2 <- bridge_step1_to_step2_server("bridge_s1s2", bridge_bus)
  bridge_s2s3 <- bridge_step2_to_step3_server("bridge_s2s3", bridge_bus)

  host_lang <- reactiveVal("en")
  host_lang_token <- reactiveVal(0)
  host_lang_runtime <- new.env(parent = emptyenv())
  host_lang_runtime$current <- "en"
  host_lang_runtime$token <- 0L
  host_lang_runtime$skip_next_lang_init <- FALSE
  set_host_lang <- function(lang, persist = TRUE) {
    normalized <- normalize_lang(lang)
    if (!identical(host_lang_runtime$current, normalized)) {
      host_lang_runtime$current <- normalized
      host_lang_runtime$token <- as.integer(host_lang_runtime$token) + 1L
      host_lang(normalized)
      host_lang_token(host_lang_runtime$token)
    }
    if (isTRUE(persist)) {
      session$sendCustomMessage("itcsuite_i18n_set_lang", list(lang = normalized))
    }
    invisible(normalized)
  }

  desktop_runtime <- new.env(parent = emptyenv())
  desktop_runtime$open_file_capability <- FALSE
  desktop_open_file_seq <- reactiveVal(0L)
  desktop_open_file_pending <- new.env(parent = emptyenv())

  desktop_runtime_enabled <- function() {
    isTRUE(is_desktop_runtime)
  }

  desktop_enabled <- function() {
    isTRUE(desktop_runtime_enabled()) && isTRUE(desktop_runtime$open_file_capability)
  }

  desktop_default_filters <- function(purpose) {
    p <- home_desktop_scalar_chr(purpose, default = "step2_import")
    if (identical(p, "step1_import")) {
      return(home_desktop_sanitize_filters(
        list(list(name = "ITC Data", extensions = c("itc", "txt"))),
        fallback_name = "ITC Data",
        fallback_exts = c("itc", "txt")
      ))
    }
    home_desktop_sanitize_filters(
      list(list(name = "Excel Workbook", extensions = "xlsx")),
      fallback_name = "Excel Workbook",
      fallback_exts = "xlsx"
    )
  }

  invoke_desktop_callback <- function(fn, ...) {
    if (!is.function(fn)) return(invisible(FALSE))
    tryCatch({
      fn(...)
      TRUE
    }, error = function(e) FALSE)
  }

  observeEvent(input$itcsuite_desktop_capability, {
    capability <- input$itcsuite_desktop_capability
    desktop_runtime$open_file_capability <- isTRUE(home_desktop_capability_open_file(capability))
  }, ignoreInit = FALSE)

  observeEvent(input$itcsuite_desktop_open_file_result, {
    normalized <- home_desktop_normalize_open_file_result(input$itcsuite_desktop_open_file_result)
    pending <- home_desktop_pending_take(desktop_open_file_pending, normalized$request_id)
    if (is.null(pending) || !is.list(pending)) return(invisible(NULL))

    on_selected <- pending$on_selected
    on_cancel <- pending$on_cancel
    on_error <- pending$on_error

    if (isTRUE(normalized$canceled)) {
      invoke_desktop_callback(on_cancel, normalized)
      return(invisible(NULL))
    }
    if (nzchar(normalized$error)) {
      invoke_desktop_callback(on_error, normalized$error, normalized)
      return(invisible(NULL))
    }
    if (!nzchar(normalized$file_path)) {
      invoke_desktop_callback(on_error, "Desktop picker returned empty file path.", normalized)
      return(invisible(NULL))
    }
    invoke_desktop_callback(on_selected, normalized)
    invisible(NULL)
  }, ignoreInit = TRUE)

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
  sleep_restore_handlers <- new.env(parent = emptyenv())
  sleep_restore_runtime <- new.env(parent = emptyenv())
  sleep_restore_runtime$last_event_key <- ""
  sleep_restore_runtime$active_tab <- ""
  sleep_restore_runtime$session_started_at <- as.numeric(Sys.time())
  sleep_restore_runtime$autosave_bootstrap_done <- FALSE

  normalize_power_event <- function(payload) {
    src <- if (is.list(payload)) payload else list()
    type <- normalize_home_scalar_chr(src$type, default = "")
    if (!type %in% c("suspend", "resume", "unlock-screen")) type <- ""
    ts_num <- suppressWarnings(as.numeric(src$ts)[1])
    if (!is.finite(ts_num)) ts_num <- as.numeric(Sys.time()) * 1000
    list(
      type = type,
      ts = ts_num,
      source = normalize_home_scalar_chr(src$source, default = "unknown")
    )
  }

  build_power_event_key <- function(event_payload) {
    ev <- normalize_power_event(event_payload)
    if (!nzchar(ev$type)) return("")
    # De-duplicate direct + replay deliveries for the same power event.
    paste0(ev$type, "|", format(ev$ts, scientific = FALSE, trim = TRUE))
  }

  sleep_restore_last_event_key <- function(value) {
    if (missing(value)) {
      return(normalize_home_scalar_chr(sleep_restore_runtime$last_event_key, default = ""))
    }
    sleep_restore_runtime$last_event_key <- normalize_home_scalar_chr(value, default = "")
    invisible(sleep_restore_runtime$last_event_key)
  }

  set_sleep_restore_active_tab <- function(value) {
    tab_norm <- normalize_home_scalar_chr(value, default = "")
    if (!tab_norm %in% c("home", "step1", "step2", "step3")) tab_norm <- ""
    sleep_restore_runtime$active_tab <- tab_norm
    invisible(tab_norm)
  }

  observeEvent(input$main_tabs, {
    set_sleep_restore_active_tab(input$main_tabs)
  }, ignoreInit = FALSE)

  is_sleep_restore_step <- function(step) {
    step_norm <- normalize_home_scalar_chr(step, default = "")
    step_norm %in% c("step1", "step2", "step3")
  }

  register_sleep_restore_handler <- function(step, collect_fn, apply_fn) {
    step_norm <- normalize_home_scalar_chr(step, default = "")
    if (!is_sleep_restore_step(step_norm)) return(invisible(FALSE))
    if (!is.function(collect_fn) || !is.function(apply_fn)) return(invisible(FALSE))
    assign(step_norm, list(collect = collect_fn, apply = apply_fn), envir = sleep_restore_handlers)
    invisible(TRUE)
  }

  resolve_sleep_restore_handler <- function(step) {
    step_norm <- normalize_home_scalar_chr(step, default = "")
    if (!is_sleep_restore_step(step_norm)) return(NULL)
    if (!exists(step_norm, envir = sleep_restore_handlers, inherits = FALSE)) return(NULL)
    entry <- get(step_norm, envir = sleep_restore_handlers, inherits = FALSE)
    if (!is.list(entry)) return(NULL)
    collect_fn <- entry$collect
    apply_fn <- entry$apply
    if (!is.function(collect_fn) || !is.function(apply_fn)) return(NULL)
    entry
  }

  collect_sleep_restore_step <- function(step) {
    entry <- resolve_sleep_restore_handler(step)
    if (is.null(entry)) return(NULL)
    out <- tryCatch(entry$collect(), error = function(e) NULL)
    if (!is.list(out) || length(out) < 1L) return(NULL)
    out
  }

  apply_sleep_restore_step <- function(step, payload) {
    entry <- resolve_sleep_restore_handler(step)
    if (is.null(entry)) return(FALSE)
    if (!is.list(payload) || length(payload) < 1L) return(FALSE)
    isTRUE(tryCatch(entry$apply(payload), error = function(e) FALSE))
  }

  resolve_sleep_restore_active_tab <- function(tab_value = NULL) {
    if (is.null(tab_value)) {
      return(normalize_home_scalar_chr(sleep_restore_runtime$active_tab, default = ""))
    }
    tab_norm <- normalize_home_scalar_chr(tab_value, default = "")
    if (tab_norm %in% c("home", "step1", "step2", "step3")) tab_norm else ""
  }

  sleep_restore_lang <- function(default = "en") {
    out <- tryCatch(host_lang(), error = function(e) default)
    normalize_lang(out)
  }

  parse_sleep_restore_saved_at <- function(value) {
    saved_at <- normalize_home_scalar_chr(value, default = "")
    if (!nzchar(saved_at)) return(NA_real_)
    parsed <- tryCatch(
      as.POSIXct(saved_at, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OSZ"),
      error = function(e) NA
    )
    if (!is.finite(suppressWarnings(as.numeric(parsed)[1]))) {
      parsed <- tryCatch(
        as.POSIXct(saved_at, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
        error = function(e) NA
      )
    }
    parsed_num <- suppressWarnings(as.numeric(parsed)[1])
    if (is.finite(parsed_num)) parsed_num else NA_real_
  }

  should_startup_consume_pending_sleep_restore <- function(state, max_age_seconds = 6 * 3600) {
    if (!is.list(state)) return(FALSE)
    if (!isTRUE(state$pending_restore)) return(FALSE)
    source_event_norm <- normalize_home_scalar_chr(state$source_event, default = "")
    if (!identical(source_event_norm, "suspend")) return(FALSE)
    saved_at_num <- parse_sleep_restore_saved_at(state$saved_at)
    if (!is.finite(saved_at_num)) return(FALSE)
    age_sec <- as.numeric(Sys.time()) - saved_at_num
    is.finite(age_sec) && age_sec >= 0 && age_sec <= as.numeric(max_age_seconds)
  }

  sleep_restore_state_has_steps <- function(state) {
    if (!is.list(state)) return(FALSE)
    steps <- if (is.list(state$steps)) state$steps else list()
    if (length(steps) < 1L) return(FALSE)
    isTRUE(any(vapply(steps, function(x) is.list(x) && length(x) > 0L, logical(1))))
  }

  should_resume_consume_autosave_fallback <- function(state, max_age_seconds = 24 * 3600) {
    if (!is.list(state)) return(FALSE)
    if (isTRUE(state$pending_restore)) return(FALSE)
    source_event_norm <- normalize_home_scalar_chr(state$source_event, default = "")
    if (!identical(source_event_norm, "autosave")) return(FALSE)
    if (!isTRUE(sleep_restore_state_has_steps(state))) return(FALSE)
    active_tab_norm <- resolve_sleep_restore_active_tab(state$active_tab)
    if (!active_tab_norm %in% c("home", "step1", "step2", "step3")) return(FALSE)

    saved_at_num <- parse_sleep_restore_saved_at(state$saved_at)
    if (!is.finite(saved_at_num)) return(FALSE)
    age_sec <- as.numeric(Sys.time()) - saved_at_num
    if (!(is.finite(age_sec) && age_sec >= 0 && age_sec <= as.numeric(max_age_seconds))) return(FALSE)
    session_started_at_num <- suppressWarnings(as.numeric(sleep_restore_runtime$session_started_at)[1])
    if (is.finite(session_started_at_num) && saved_at_num >= (session_started_at_num - 1)) return(FALSE)

    restored_at_num <- parse_sleep_restore_saved_at(state$restored_at)
    if (is.finite(restored_at_num) && restored_at_num >= saved_at_num) return(FALSE)
    TRUE
  }

  resolve_sleep_restore_main_log_path <- function() {
    base_dir <- normalize_home_scalar_chr(Sys.getenv("ITCSUITE_USER_DATA_DIR", unset = ""), default = "")
    if (!nzchar(base_dir)) return("")
    file.path(base_dir, "logs", "main.log")
  }

  extract_main_log_ts <- function(line) {
    if (!is.character(line) || length(line) < 1L) return("")
    match <- regexpr("\"ts\":\"[^\"]+\"", line[[1]], perl = TRUE)
    if (identical(match, -1L)) return("")
    raw <- regmatches(line[[1]], match)
    sub("^\"ts\":\"([^\"]+)\"$", "\\1", raw, perl = TRUE)
  }

  find_recent_main_wake_event <- function(max_age_seconds = 20 * 60, max_lines = 1200L) {
    if (!isTRUE(desktop_runtime_enabled())) return(NULL)

    log_path <- resolve_sleep_restore_main_log_path()
    if (!nzchar(log_path) || !isTRUE(file.exists(log_path))) return(NULL)

    lines <- tryCatch(readLines(log_path, warn = FALSE), error = function(e) character(0))
    if (length(lines) < 1L) return(NULL)

    max_n <- suppressWarnings(as.integer(max_lines)[1])
    if (!is.finite(max_n) || max_n < 200L) max_n <- 1200L
    lines <- tail(lines, max_n)

    for (idx in rev(seq_along(lines))) {
      line <- lines[[idx]]
      event_type <- ""
      if (grepl("\"event\":\"power_resume\"", line, fixed = TRUE)) {
        event_type <- "resume"
      } else if (grepl("\"event\":\"unlock_screen\"", line, fixed = TRUE)) {
        event_type <- "unlock-screen"
      } else if (grepl("\"event\":\"power_suspend\"", line, fixed = TRUE)) {
        # Newest relevant event is suspend, no wake signal to consume.
        return(NULL)
      } else {
        next
      }

      ts_value <- extract_main_log_ts(line)
      ts_num <- parse_sleep_restore_saved_at(ts_value)
      if (!is.finite(ts_num)) next

      age_sec <- as.numeric(Sys.time()) - ts_num
      if (!(is.finite(age_sec) && age_sec >= 0 && age_sec <= as.numeric(max_age_seconds))) {
        return(NULL)
      }

      return(list(type = event_type, ts = ts_value, ts_num = ts_num))
    }

    NULL
  }

  should_startup_consume_autosave_fallback <- function(state) {
    if (!isTRUE(should_resume_consume_autosave_fallback(state))) return(FALSE)

    wake_event <- find_recent_main_wake_event()
    if (is.null(wake_event) || !is.list(wake_event)) return(FALSE)
    wake_ts_num <- suppressWarnings(as.numeric(wake_event$ts_num)[1])
    if (!is.finite(wake_ts_num)) return(FALSE)

    session_started_at_num <- suppressWarnings(as.numeric(sleep_restore_runtime$session_started_at)[1])
    if (is.finite(session_started_at_num)) {
      wake_to_session_sec <- session_started_at_num - wake_ts_num
      if (!(is.finite(wake_to_session_sec) && wake_to_session_sec >= 0 && wake_to_session_sec <= 180)) {
        return(FALSE)
      }
    }

    restored_at_num <- parse_sleep_restore_saved_at(state$restored_at)
    if (is.finite(restored_at_num) && wake_ts_num <= restored_at_num) return(FALSE)

    saved_at_num <- parse_sleep_restore_saved_at(state$saved_at)
    if (!is.finite(saved_at_num)) return(FALSE)
    if (wake_ts_num + 30 < saved_at_num) return(FALSE)

    TRUE
  }

  consume_resume_autosave_fallback <- function(event_type = "resume", trigger = "power_event") {
    if (!isTRUE(desktop_runtime_enabled())) return(invisible(FALSE))
    event_norm <- normalize_home_scalar_chr(event_type, default = "")
    if (!identical(event_norm, "resume")) return(invisible(FALSE))

    state <- home_sleep_restore_store_load(warn_fn = function(...) NULL)
    if (!isTRUE(should_resume_consume_autosave_fallback(state))) return(invisible(FALSE))

    state$pending_restore <- TRUE
    state$source_event <- "suspend"
    ok_save <- isTRUE(home_sleep_restore_store_save(state, warn_fn = function(...) NULL))
    if (!isTRUE(ok_save)) return(invisible(FALSE))

    isTRUE(consume_pending_sleep_restore(
      event_type = "resume",
      trigger = paste0(normalize_home_scalar_chr(trigger, default = "power_event"), "_autosave_fallback")
    ))
  }

  merge_step1_sleep_restore_payload <- function(payload, existing) {
    out <- if (is.list(payload)) payload else list()
    if (!is.list(existing) || length(existing) < 1L) {
      return(list(payload = out, reused_existing = FALSE))
    }

    reused <- FALSE
    out_path <- normalize_home_scalar_chr(out$source_path, default = "")
    if (!nzchar(out_path)) {
      existing_path <- normalize_home_scalar_chr(existing$source_path, default = "")
      if (nzchar(existing_path)) {
        out$source_path <- existing_path
        reused <- TRUE
      }
    }

    out_name <- normalize_home_scalar_chr(out$display_name, default = "")
    if (!nzchar(out_name)) {
      existing_name <- normalize_home_scalar_chr(existing$display_name, default = "")
      if (nzchar(existing_name)) {
        out$display_name <- existing_name
        reused <- TRUE
      }
    }

    out_params <- home_sleep_restore_normalize_scalar_map(out$params)
    existing_params <- home_sleep_restore_normalize_scalar_map(existing$params)
    if (length(existing_params) > 0L) {
      if (length(out_params) < 1L) {
        out$params <- existing_params
        reused <- TRUE
      } else {
        merged <- existing_params
        for (nm in names(out_params)) merged[[nm]] <- out_params[[nm]]
        out$params <- merged
      }
    } else {
      out$params <- out_params
    }

    list(payload = out, reused_existing = reused)
  }

  step2_sleep_restore_payload_is_weak <- function(payload) {
    if (!is.list(payload) || length(payload) < 1L) return(TRUE)

    source_path <- home_sleep_restore_scalar_chr(payload$source_path, default = "")
    file_name <- home_sleep_restore_scalar_chr(payload$file_name, default = "")
    source_kind <- home_sleep_restore_scalar_chr(payload$source_kind, default = "none")
    if (!source_kind %in% c("import", "step1_bridge", "sim_to_exp", "none")) source_kind <- "none"

    if (nzchar(source_path) || nzchar(file_name) || !identical(source_kind, "none")) return(FALSE)

    sheets <- home_sleep_restore_normalize_table_list(payload$sheets)
    if (length(sheets) > 0L) return(FALSE)

    manual_exp_data <- home_sleep_restore_normalize_table_like(payload$manual_exp_data)
    if (is.data.frame(manual_exp_data) && nrow(manual_exp_data) > 0L) return(FALSE)

    if (isTRUE(home_sleep_restore_scalar_lgl(payload$exp_data_disabled, default = FALSE))) return(FALSE)

    snapshot_table <- home_sleep_restore_normalize_step2_snapshot_table(payload$snapshot_table)
    if (is.data.frame(snapshot_table$rows) && nrow(snapshot_table$rows) > 0L) return(FALSE)
    if (length(snapshot_table$checked_ids %||% character(0)) > 0L) return(FALSE)
    if (nzchar(home_sleep_restore_scalar_chr(snapshot_table$active_row_id, default = ""))) return(FALSE)

    diagnostics <- home_sleep_restore_normalize_step2_diagnostics(payload$diagnostics)
    diag_names <- names(diagnostics)
    if (is.null(diag_names)) diag_names <- character(0)
    diag_effective <- setdiff(diag_names, "residual_subtab")
    if (length(diag_effective) > 0L) return(FALSE)

    params <- home_sleep_restore_normalize_step2_params(payload$params)
    param_names <- names(params)
    if (is.null(param_names)) param_names <- character(0)
    param_effective <- setdiff(param_names, "residual_subtab")
    if (length(param_effective) > 0L) return(FALSE)

    TRUE
  }

  step3_sleep_restore_settings_is_default_like <- function(settings) {
    settings_norm <- home_sleep_restore_normalize_scalar_map(settings)
    keys <- c(
      "top_xmin", "top_xmax", "top_ymin", "top_ymax",
      "bot_xmin", "bot_xmax", "bot_ymin", "bot_ymax",
      "bot_no_dim_start", "bot_no_dim_end"
    )
    if (!all(keys %in% names(settings_norm))) return(FALSE)

    targets <- c(
      top_xmin = 0,
      top_xmax = 100,
      top_ymin = -5,
      top_ymax = 5,
      bot_xmin = 0,
      bot_xmax = 3,
      bot_ymin = -20,
      bot_ymax = 5,
      bot_no_dim_start = 1,
      bot_no_dim_end = 1
    )
    as_num <- function(x) suppressWarnings(as.numeric(x)[1])
    for (nm in names(targets)) {
      value_num <- as_num(settings_norm[[nm]])
      target_num <- targets[[nm]]
      if (!is.finite(value_num) || abs(value_num - target_num) > 1e-9) return(FALSE)
    }
    TRUE
  }

  merge_step3_sleep_restore_payload <- function(payload, existing) {
    out <- if (is.list(payload)) payload else list()
    if (!is.list(existing) || length(existing) < 1L) {
      out$settings <- home_sleep_restore_normalize_scalar_map(out$settings)
      sheets_now <- home_sleep_restore_normalize_step3_sheets(out$sheets)
      if (length(sheets_now) > 0L) {
        out$sheets <- sheets_now
      } else {
        out$sheets <- NULL
      }
      has_payload <- nzchar(normalize_home_scalar_chr(out$source_path, default = "")) ||
        nzchar(normalize_home_scalar_chr(out$file_name, default = "")) ||
        length(out$settings) > 0L ||
        length(sheets_now) > 0L
      return(list(payload = if (isTRUE(has_payload)) out else NULL, reused_existing = FALSE))
    }

    reused <- FALSE

    out_path <- normalize_home_scalar_chr(out$source_path, default = "")
    existing_path <- normalize_home_scalar_chr(existing$source_path, default = "")
    if (!nzchar(out_path) && nzchar(existing_path)) {
      out$source_path <- existing_path
      reused <- TRUE
    }

    out_name <- normalize_home_scalar_chr(out$file_name, default = "")
    existing_name <- normalize_home_scalar_chr(existing$file_name, default = "")
    if (!nzchar(out_name) && nzchar(existing_name)) {
      out$file_name <- existing_name
      reused <- TRUE
    }

    out_settings <- home_sleep_restore_normalize_scalar_map(out$settings)
    existing_settings <- home_sleep_restore_normalize_scalar_map(existing$settings)
    if (length(existing_settings) > 0L) {
      if (length(out_settings) < 1L) {
        out$settings <- existing_settings
        reused <- TRUE
      } else {
        out_is_default <- isTRUE(step3_sleep_restore_settings_is_default_like(out_settings))
        existing_is_default <- isTRUE(step3_sleep_restore_settings_is_default_like(existing_settings))
        if (isTRUE(out_is_default) && !isTRUE(existing_is_default)) {
          out$settings <- existing_settings
          reused <- TRUE
        } else {
          merged <- existing_settings
          for (nm in names(out_settings)) merged[[nm]] <- out_settings[[nm]]
          out$settings <- merged
        }
      }
    } else {
      out$settings <- out_settings
    }

    out_sheets <- home_sleep_restore_normalize_step3_sheets(out$sheets)
    existing_sheets <- home_sleep_restore_normalize_step3_sheets(existing$sheets)
    if (length(existing_sheets) > 0L) {
      if (length(out_sheets) < 1L) {
        out$sheets <- existing_sheets
        reused <- TRUE
      } else {
        merged_sheets <- existing_sheets
        for (nm in names(out_sheets)) merged_sheets[[nm]] <- out_sheets[[nm]]
        out$sheets <- merged_sheets
      }
    } else if (length(out_sheets) > 0L) {
      out$sheets <- out_sheets
    } else {
      out$sheets <- NULL
    }

    has_payload <- nzchar(normalize_home_scalar_chr(out$source_path, default = "")) ||
      nzchar(normalize_home_scalar_chr(out$file_name, default = "")) ||
      length(home_sleep_restore_normalize_scalar_map(out$settings)) > 0L ||
      length(home_sleep_restore_normalize_step3_sheets(out$sheets)) > 0L
    list(payload = if (isTRUE(has_payload)) out else NULL, reused_existing = reused)
  }

  request_sleep_restore_snapshot <- function(reason = "manual", source_event = "manual") {
    if (!isTRUE(desktop_runtime_enabled())) return(invisible(FALSE))
    reason_norm <- normalize_home_scalar_chr(reason, default = "manual")
    source_event_norm <- normalize_home_scalar_chr(source_event, default = "manual")
    existing <- home_sleep_restore_store_load(warn_fn = function(...) NULL)
    allow_existing_reuse <- identical(source_event_norm, "autosave")
    existing_pending <- isTRUE(existing$pending_restore)
    existing_source_event <- normalize_home_scalar_chr(existing$source_event, default = "")
    existing_active_tab <- resolve_sleep_restore_active_tab(existing$active_tab)
    existing_steps <- if (is.list(existing$steps)) existing$steps else list()
    steps <- list()
    reused_existing <- list(step1 = FALSE, step2 = FALSE, step3 = FALSE)
    for (step in c("step1", "step2", "step3")) {
      payload <- collect_sleep_restore_step(step)
      existing_payload <- existing_steps[[step]]

      if (identical(step, "step1") && is.list(payload)) {
        merged <- merge_step1_sleep_restore_payload(payload, existing_payload)
        payload <- merged$payload
        if (isTRUE(merged$reused_existing)) reused_existing[[step]] <- TRUE
      }

      if (identical(step, "step2") && is.list(payload) && isTRUE(step2_sleep_restore_payload_is_weak(payload))) {
        if (is.list(existing_payload) && length(existing_payload) > 0L) {
          payload <- existing_payload
          reused_existing[[step]] <- TRUE
        } else {
          payload <- NULL
        }
      }

      if (identical(step, "step3") && is.list(payload)) {
        merged <- merge_step3_sleep_restore_payload(payload, existing_payload)
        payload <- merged$payload
        if (isTRUE(merged$reused_existing)) reused_existing[[step]] <- TRUE
      }

      allow_suspend_reuse <- identical(source_event_norm, "suspend")
      if ((isTRUE(allow_existing_reuse) || isTRUE(allow_suspend_reuse)) && is.null(payload)) {
        fallback <- existing_payload
        if (is.list(fallback) && length(fallback) > 0L) {
          payload <- fallback
          reused_existing[[step]] <- TRUE
        }
      }
      if (is.null(payload)) next
      steps[[step]] <- payload
    }
    has_steps <- length(steps) > 0L
    active_tab_now <- resolve_sleep_restore_active_tab(tryCatch(shiny::isolate(input$main_tabs), error = function(e) NULL))
    if (!nzchar(active_tab_now)) active_tab_now <- resolve_sleep_restore_active_tab()
    if (!nzchar(active_tab_now) && nzchar(existing_active_tab)) active_tab_now <- existing_active_tab

    pending_restore <- has_steps
    if (identical(source_event_norm, "autosave")) {
      # autosave 只作为兜底数据，不直接产生 pending；仅在复用 suspend pending 时沿用 pending。
      pending_from_existing <- isTRUE(
        has_steps &&
          existing_pending &&
          identical(existing_source_event, "suspend")
      )
      pending_restore <- isTRUE(pending_from_existing)
      if (isTRUE(pending_from_existing)) {
        source_event_norm <- "suspend"
      }
      if (isTRUE(pending_from_existing) && existing_active_tab %in% c("step1", "step2", "step3")) {
        active_tab_now <- existing_active_tab
      }
    }

    state <- list(
      schema_version = home_sleep_restore_store_schema(),
      pending_restore = pending_restore,
      saved_at = home_sleep_restore_store_now_utc(),
      source_event = source_event_norm,
      lang = sleep_restore_lang(),
      active_tab = active_tab_now,
      steps = steps,
      restored_at = normalize_home_scalar_chr(existing$restored_at, default = "")
    )
    ok <- isTRUE(home_sleep_restore_store_save(state, warn_fn = function(...) NULL))
    telemetry_log_event(
      event = "sleep_restore.snapshot",
      level = if (isTRUE(ok)) "INFO" else "WARN",
      module = "host",
      payload = list(
        reason = reason_norm,
        source_event = source_event_norm,
        has_step1 = "step1" %in% names(steps),
        has_step2 = "step2" %in% names(steps),
        has_step3 = "step3" %in% names(steps),
        reused_step1 = isTRUE(reused_existing$step1),
        reused_step2 = isTRUE(reused_existing$step2),
        reused_step3 = isTRUE(reused_existing$step3),
        pending_restore = pending_restore
      ),
      lang = sleep_restore_lang()
    )
    invisible(ok)
  }

  consume_pending_sleep_restore <- function(event_type = "resume", trigger = "power_event") {
    if (!isTRUE(desktop_runtime_enabled())) return(invisible(FALSE))
    event_norm <- normalize_home_scalar_chr(event_type, default = "")
    if (!event_norm %in% c("resume", "unlock-screen")) return(invisible(FALSE))

    state <- home_sleep_restore_store_load(warn_fn = function(...) NULL)
    if (!isTRUE(state$pending_restore)) return(invisible(FALSE))
    source_event_norm <- normalize_home_scalar_chr(state$source_event, default = "")
    if (!identical(source_event_norm, "suspend")) return(invisible(FALSE))
    host_lang_runtime$skip_next_lang_init <- TRUE
    set_host_lang(normalize_home_scalar_chr(state$lang, default = "en"), persist = TRUE)

    steps <- if (is.list(state$steps)) state$steps else list()
    applied_steps <- character(0)
    for (step in c("step1", "step2", "step3")) {
      payload <- steps[[step]]
      if (!is.list(payload) || length(payload) < 1L) next
      ok <- isTRUE(apply_sleep_restore_step(step, payload))
      if (isTRUE(ok)) applied_steps <- c(applied_steps, step)
    }

    if (length(applied_steps) < 1L) {
      telemetry_log_event(
        event = "sleep_restore.restore",
        level = "WARN",
        module = "host",
        payload = list(
          trigger = normalize_home_scalar_chr(trigger, default = "power_event"),
          event_type = event_norm,
          outcome = "deferred_no_steps_applied"
        ),
        lang = sleep_restore_lang()
      )
      session$onFlushed(function() {
        tryCatch(
          consume_pending_sleep_restore(
            event_type = "resume",
            trigger = paste0(normalize_home_scalar_chr(trigger, default = "power_event"), "_retry")
          ),
          error = function(e) NULL
        )
      }, once = TRUE)
      return(invisible(FALSE))
    }

    target_tab <- normalize_home_scalar_chr(state$active_tab, default = "")
    if (identical(target_tab, "home")) {
      tryCatch(updateTabsetPanel(session, "main_tabs", selected = "home"), error = function(e) NULL)
    } else if (target_tab %in% c("step1", "step2", "step3") && target_tab %in% applied_steps) {
      tryCatch(updateTabsetPanel(session, "main_tabs", selected = target_tab), error = function(e) NULL)
    }

    state$pending_restore <- FALSE
    state$restored_at <- home_sleep_restore_store_now_utc()
    home_sleep_restore_store_save(state, warn_fn = function(...) NULL)
    telemetry_log_event(
      event = "sleep_restore.restore",
      level = "INFO",
      module = "host",
      payload = list(
        trigger = normalize_home_scalar_chr(trigger, default = "power_event"),
        event_type = event_norm,
        restored_step1 = "step1" %in% applied_steps,
        restored_step2 = "step2" %in% applied_steps,
        restored_step3 = "step3" %in% applied_steps,
        target_tab = target_tab
      ),
      lang = sleep_restore_lang()
    )

    invisible(length(applied_steps) > 0L)
  }

  process_power_event <- function(payload, trigger = "input") {
    if (!isTRUE(desktop_runtime_enabled())) return(invisible(FALSE))
    event <- normalize_power_event(payload)
    if (!nzchar(event$type)) return(invisible(FALSE))
    event_key <- build_power_event_key(event)
    if (!nzchar(event_key)) return(invisible(FALSE))
    if (identical(event_key, sleep_restore_last_event_key())) return(invisible(FALSE))
    sleep_restore_last_event_key(event_key)

    if (identical(event$type, "suspend")) {
      request_sleep_restore_snapshot(reason = trigger, source_event = "suspend")
      return(invisible(TRUE))
    }
    if (event$type %in% c("resume", "unlock-screen")) {
      restored <- isTRUE(consume_pending_sleep_restore(event_type = event$type, trigger = trigger))
      if (!isTRUE(restored) && identical(event$type, "resume")) {
        restored <- isTRUE(consume_resume_autosave_fallback(event_type = "resume", trigger = trigger))
      }
      return(invisible(restored))
    }
    invisible(FALSE)
  }

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
      telemetry_log_event(
        event = "home.user_action",
        level = "WARN",
        module = "host",
        payload = list(action = "restore_recent", outcome = "no_payload", record_id = record_id, target_step = target_step),
        lang = host_lang()
      )
      return(invisible(FALSE))
    }
    if (!isTRUE(file.exists(source_path))) {
      showNotification(
        host_trf("home_restore_failed_path_missing", host_lang(), display_name),
        type = "warning",
        duration = 4
      )
      telemetry_log_event(
        event = "home.user_action",
        level = "WARN",
        module = "host",
        payload = list(action = "restore_recent", outcome = "path_missing", record_id = record_id, source_path = source_path),
        lang = host_lang()
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
      telemetry_log_event(
        event = "home.user_action",
        level = "WARN",
        module = "host",
        payload = list(action = "restore_recent", outcome = "missing_handler", record_id = record_id, target_step = target_step),
        lang = host_lang()
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
        telemetry_log_event(
          event = "home.user_action",
          level = "ERROR",
          module = "host",
          payload = list(action = "restore_recent", outcome = "error", record_id = record_id),
          err = e,
          lang = host_lang()
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
    telemetry_log_event(
      event = "home.user_action",
      level = "INFO",
      module = "host",
      payload = list(action = "restore_recent", outcome = "ok", record_id = record_id, target_step = target_step),
      lang = host_lang()
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

  session$userData$itcsuite_app_meta <- list(
    get_app_version = function() {
      version_chr <- home_desktop_scalar_chr(Sys.getenv("ITCSUITE_APP_VERSION", unset = ""), default = "")
      if (nzchar(version_chr)) version_chr else host_app_version
    },
    get_developer_profile = function() {
      list(
        name = home_contact_scalar_chr(home_contact_profile$name, default = ""),
        email = home_contact_scalar_chr(home_contact_profile$email, default = ""),
        website = home_contact_scalar_chr(home_contact_profile$website, default = "")
      )
    }
  )

  session$userData$itcsuite_desktop <- list(
    enabled = function() {
      desktop_enabled()
    },
    open_file = function(
      purpose,
      title,
      filters,
      on_selected,
      on_cancel = NULL,
      on_error = NULL
    ) {
      purpose_norm <- home_desktop_scalar_chr(purpose, default = "step2_import")
      if (!purpose_norm %in% c("step1_import", "step2_import", "step3_import")) {
        purpose_norm <- "step2_import"
      }
      if (!is.function(on_selected)) return(invisible(FALSE))

      if (!isTRUE(desktop_enabled())) {
        invoke_desktop_callback(on_error, "Desktop native file picker is unavailable.")
        return(invisible(FALSE))
      }

      request_meta <- home_desktop_next_request_id(desktop_open_file_seq(), purpose = purpose_norm)
      desktop_open_file_seq(request_meta$next_seq)
      request_id <- request_meta$request_id
      default_filters <- desktop_default_filters(purpose_norm)
      payload <- list(
        request_id = request_id,
        purpose = purpose_norm,
        title = home_desktop_scalar_chr(title, default = "Select File"),
        filters = home_desktop_sanitize_filters(
          filters,
          fallback_name = default_filters[[1]]$name,
          fallback_exts = default_filters[[1]]$extensions
        )
      )

      ok_pending <- home_desktop_pending_register(
        desktop_open_file_pending,
        request_id,
        callbacks = list(
          on_selected = on_selected,
          on_cancel = on_cancel,
          on_error = on_error
        )
      )
      if (!isTRUE(ok_pending)) {
        invoke_desktop_callback(on_error, "Failed to register desktop open file request.")
        return(invisible(FALSE))
      }

      session$sendCustomMessage("itcsuite_desktop_open_file", payload)
      invisible(TRUE)
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

  session$userData$itcsuite_sleep_restore <- list(
    register_handler = function(step, collect_fn, apply_fn) {
      register_sleep_restore_handler(step, collect_fn = collect_fn, apply_fn = apply_fn)
    },
    request_snapshot_now = function(reason = "manual") {
      request_sleep_restore_snapshot(
        reason = normalize_home_scalar_chr(reason, default = "manual"),
        source_event = "manual"
      )
    }
  )

  observe({
    if (!isTRUE(desktop_runtime_enabled())) return(invisible(NULL))
    invalidateLater(15000, session)
    if (!isTRUE(sleep_restore_runtime$autosave_bootstrap_done)) {
      sleep_restore_runtime$autosave_bootstrap_done <- TRUE
      return(invisible(NULL))
    }
    shiny::isolate(request_sleep_restore_snapshot(reason = "periodic_autosave", source_event = "autosave"))
  })

  observeEvent(input$itcsuite_power_event, {
    process_power_event(input$itcsuite_power_event, trigger = "power_event_input")
  }, ignoreInit = FALSE)

  session$onFlushed(function() {
    payload <- tryCatch(shiny::isolate(input$itcsuite_power_event), error = function(e) NULL)
    handled <- isTRUE(process_power_event(payload, trigger = "startup_flush"))
    if (isTRUE(handled)) return(invisible(NULL))

    state <- home_sleep_restore_store_load(warn_fn = function(...) NULL)
    if (isTRUE(should_startup_consume_pending_sleep_restore(state))) {
      consume_pending_sleep_restore(event_type = "resume", trigger = "startup_pending")
      return(invisible(NULL))
    }

    if (isTRUE(should_startup_consume_autosave_fallback(state))) {
      consume_resume_autosave_fallback(event_type = "resume", trigger = "startup_wake_detected")
    }
  }, once = TRUE)

  observeEvent(input$itcsuite_lang_init, {
    if (isTRUE(host_lang_runtime$skip_next_lang_init)) {
      host_lang_runtime$skip_next_lang_init <- FALSE
      session$sendCustomMessage("itcsuite_i18n_set_lang", list(lang = host_lang_runtime$current))
      telemetry_log_event(
        event = "home.user_action",
        level = "INFO",
        module = "host",
        payload = list(action = "lang_init_skipped_after_restore", lang = host_lang()),
        lang = host_lang()
      )
      return(invisible(NULL))
    }
    set_host_lang(input$itcsuite_lang_init, persist = TRUE)
    telemetry_log_event(
      event = "home.user_action",
      level = "INFO",
      module = "host",
      payload = list(action = "lang_init", lang = host_lang()),
      lang = host_lang()
    )
  }, ignoreInit = TRUE, once = TRUE)

  observeEvent(input$host_lang_toggle, {
    next_lang <- if (identical(host_lang(), "en")) "zh" else "en"
    set_host_lang(next_lang, persist = TRUE)
    telemetry_log_event(
      event = "home.user_action",
      level = "INFO",
      module = "host",
      payload = list(action = "lang_toggle", lang = next_lang),
      lang = next_lang
    )
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
    import_recs <- home_filter_existing_recent_records(import_recs)
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

    home_icon_tag <- NULL
    if (is.character(home_icon_src) && nzchar(home_icon_src)) {
      home_icon_tag <- tags$img(class = "home-title-icon", src = home_icon_src, alt = "ViaBind")
    }
    home_contact_email <- home_contact_scalar_chr(home_contact_profile$email, default = "")
    home_contact_email_href <- home_contact_mailto_href(home_contact_email)
    home_contact_site <- home_contact_scalar_chr(home_contact_profile$website, default = "")
    home_contact_version <- home_contact_build_viabind_signature(repo_root = repo_root)
    home_contact_citation <- home_contact_build_citation_info(repo_root = repo_root)
    home_contact_qr <- home_contact_resolve_qr_src(
      lang = l,
      assets_dir = home_contact_assets_dir,
      resource_prefix = "/assets"
    )
    home_contact_qr_node <- if (isTRUE(home_contact_qr$exists) && nzchar(home_contact_qr$src)) {
      tags$img(
        class = "home-contact-qr",
        src = home_contact_qr$src,
        alt = host_tr("home_contact_donate_link_label", l)
      )
    } else {
      div(class = "home-contact-qr-missing", host_tr("home_contact_qr_missing", l))
    }
    home_contact_donate_note_lines <- c(
      host_tr("home_contact_donate_note_line1", l),
      host_tr("home_contact_donate_note_line2", l),
      host_tr("home_contact_donate_note_line3", l)
    )
    home_contact_donate_note_lines <- home_contact_donate_note_lines[
      nzchar(trimws(home_contact_donate_note_lines))
    ]
    home_contact_donate_note <- tagList(lapply(home_contact_donate_note_lines, function(txt) {
      tags$p(class = "home-contact-donate-note", txt)
    }))
    home_contact_donate_link <- NULL
    if (identical(l, "en") && nzchar(home_contact_bmc_url)) {
      home_contact_donate_link <- tags$a(
        href = home_contact_bmc_url,
        target = "_blank",
        rel = "noopener noreferrer",
        class = "home-contact-link",
        host_tr("home_contact_donate_link_label", l)
      )
    }
    home_contact_donate_line <- NULL
    if (!is.null(home_contact_donate_link)) {
      home_contact_donate_line <- tags$p(home_contact_donate_link)
    }

    div(
      class = "home-tab-wrap",
      div(
        class = "home-panel",
        div(
          class = "home-title-row",
          home_icon_tag,
          tags$h3(host_tr("home_welcome_title", l))
        ),
        tags$p(host_tr("home_welcome_desc", l)),
        div(
          class = "home-action-row",
          actionButton("home_start_step1", host_tr("home_start_step1", l), class = "btn btn-primary")
        ),
        tags$h4(host_tr("home_recent_title", l)),
        build_recent_table(import_recs, host_tr("home_recent_empty", l)),
        div(
          class = "home-contact-panel",
          div(
            class = "home-contact-grid",
            div(
              class = "home-contact-lines home-contact-col home-contact-col-dev",
              tags$h5(class = "home-contact-col-title", host_tr("home_contact_title", l)),
              tags$p(
                tags$strong(paste0(host_tr("home_contact_dev_name_label", l), ": ")),
                home_contact_profile$name
              ),
              tags$p(
                tags$strong(paste0(host_tr("home_contact_email_label", l), ": ")),
                if (nzchar(home_contact_email_href)) {
                  tags$a(
                    href = home_contact_email_href,
                    class = "home-contact-link",
                    home_contact_email
                  )
                } else {
                  home_contact_email
                }
              ),
              tags$p(
                tags$strong(paste0(host_tr("home_contact_website_label", l), ": ")),
                tags$a(
                  href = home_contact_site,
                  target = "_blank",
                  rel = "noopener noreferrer",
                  class = "home-contact-link",
                  home_contact_site
                )
              ),
              tags$p(
                tags$strong(paste0(host_tr("home_contact_version_label", l), ": ")),
                home_contact_version
              )
            ),
            div(
              class = "home-contact-lines home-contact-col home-contact-col-citation",
              tags$h5(class = "home-contact-col-title", host_tr("home_contact_citation_title", l)),
              tags$p(host_tr("home_contact_citation_intro", l)),
              tags$p(class = "home-contact-citation-line", home_contact_citation$line1),
              tags$p(
                class = "home-contact-citation-line",
                home_contact_citation$line2_prefix,
                tags$a(
                  href = home_contact_citation$doi_href,
                  target = "_blank",
                  rel = "noopener noreferrer",
                  class = "home-contact-link",
                  home_contact_citation$doi_text
                )
              ),
              actionButton("home_copy_citation", host_tr("home_contact_citation_copy_btn", l), class = "btn btn-default btn-xs home-contact-citation-copy")
            ),
            div(
              class = "home-contact-lines home-contact-col home-contact-col-donate",
              tags$h5(class = "home-contact-col-title", host_tr("home_contact_donate_title", l)),
              home_contact_donate_note,
              home_contact_donate_line
            ),
            div(
              class = "home-contact-qr-wrap home-contact-col home-contact-col-qr",
              home_contact_qr_node
            )
          )
        )
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

  observeEvent(input$home_copy_citation, {
    citation_line <- home_contact_build_citation_info(repo_root = repo_root)$copy_text
    session$sendCustomMessage("itcsuite_copy_to_clipboard", list(
      text = citation_line,
      result_input_id = "itcsuite_copy_citation_result"
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$itcsuite_copy_citation_result, {
    result <- input$itcsuite_copy_citation_result
    ok <- isTRUE(result$ok)
    showNotification(
      if (ok) host_tr("home_contact_citation_copy_ok", host_lang()) else host_tr("home_contact_citation_copy_failed", host_lang()),
      type = if (ok) "message" else "warning",
      duration = if (ok) 2.5 else 4
    )
  }, ignoreInit = TRUE)

  telemetry_log_event(
    event = "app.lifecycle",
    level = "INFO",
    module = "host",
    payload = list(action = "session_started", session_id = telemetry$session_id %||% ""),
    lang = "en"
  )

  session$onSessionEnded(function() {
    telemetry_log_event(
      event = "app.lifecycle",
      level = "INFO",
      module = "host",
      payload = list(action = "session_ended", session_id = telemetry$session_id %||% ""),
      lang = "en"
    )
    bridge_set_reject_logger(NULL)
  })

  session$userData$itcsuite_bridge <- list(
    step1_payload = bridge_bus$step1_payload,
    step2_plot_payload = bridge_bus$step2_plot_payload,
    bridge_s1s2_token = bridge_s1s2$latest_token,
    bridge_s2s3_token = bridge_s2s3$latest_token
  )

  output$legacy_processor_ui <- renderUI({
    processor_legacy$ui
  })
  outputOptions(output, "legacy_processor_ui", suspendWhenHidden = FALSE)

  output$legacy_simfit_ui <- renderUI({
    simfit_legacy$ui
  })
  outputOptions(output, "legacy_simfit_ui", suspendWhenHidden = FALSE)

  output$legacy_graph_ui <- renderUI({
    graph_legacy$ui
  })
  outputOptions(output, "legacy_graph_ui", suspendWhenHidden = FALSE)

  processor_legacy$server(input, output, session)
  simfit_legacy$server(input, output, session)
  graph_legacy$server(input, output, session)
}

shinyApp(ui, server)
