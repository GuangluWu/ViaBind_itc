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

if (!exists("load_guide_annotations", mode = "function")) {
  fail_fast("Startup check failed: guide annotation loader is unavailable.")
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
      step1 = "Step 1 Baseline & Integration",
      step2 = "Step 2 Simulation & Fitting",
      step3 = "Step 3 Plot & Export"
    ),
    zh = list(
      step1 = "步骤 1 基线校正 & 积分",
      step2 = "步骤 2 模拟 & 拟合",
      step3 = "步骤 3 绘图 & 导出"
    )
  )
  lang_norm <- normalize_lang(lang)
  val <- dict[[lang_norm]][[key]]
  if (is.null(val)) key else val
}

repo_root <- detect_repo_root()
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
    ")),
    tags$script(HTML("
      (function() {
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

        Shiny.addCustomMessageHandler('itcsuite_i18n_set_lang', function(msg) {
          if (!msg || !msg.lang) return;
          try {
            localStorage.setItem('itcsuite.lang', normalizeLang(msg.lang));
          } catch (e) {}
        });

        Shiny.addCustomMessageHandler('itcsuite_i18n_tab_labels', function(msg) {
          if (!msg) return;
          if (msg.step1) $('#main_tab_label_step1').text(msg.step1);
          if (msg.step2) $('#main_tab_label_step2').text(msg.step2);
          if (msg.step3) $('#main_tab_label_step3').text(msg.step3);
        });

        $(document).on('shiny:connected', function() {
          Shiny.setInputValue('itcsuite_lang_init', detectInitialLang(), {priority: 'event'});
        });
      })();
    "))
  ),
  div(
    class = "main-host-wrap",
    div(
      class = "main-host-topbar",
      tabsetPanel(
        id = "main_tabs",
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

  observeEvent(input$itcsuite_lang_init, {
    set_host_lang(input$itcsuite_lang_init, persist = TRUE)
  }, ignoreInit = TRUE, once = TRUE)

  observeEvent(input$host_lang_toggle, {
    next_lang <- if (identical(host_lang(), "en")) "zh" else "en"
    set_host_lang(next_lang, persist = TRUE)
  }, ignoreInit = TRUE)

  observeEvent(host_lang(), {
    session$sendCustomMessage("itcsuite_i18n_tab_labels", list(
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
