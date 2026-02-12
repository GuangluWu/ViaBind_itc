  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  session_bridge <- tryCatch({
    b <- session$userData$itcsuite_bridge
    if (is.null(b) || !is.list(b)) NULL else b
  }, error = function(e) NULL)

  resolve_bridge_channel <- function(channel) {
    ch <- if (!is.null(session_bridge)) session_bridge[[channel]] else NULL
    if (is.function(ch)) return(ch)
    NULL
  }

  bridge_last_token <- reactiveVal(as.numeric(Sys.time()))
  next_bridge_token <- function() {
    now_token <- as.numeric(Sys.time())
    last_token <- suppressWarnings(as.numeric(bridge_last_token())[1])
    if (is.finite(last_token) && is.finite(now_token) && now_token <= last_token) {
      now_token <- last_token + 1e-6
    }
    bridge_last_token(now_token)
    now_token
  }

  bridge_last_step1_signature <- reactiveVal(list(
    consumed = NA_character_,
    reset = NA_character_,
    input = NA_character_
  ))
  
  bridge_set <- function(channel, payload) {
    ch <- resolve_bridge_channel(channel)
    if (is.function(ch)) ch(payload)
    invisible(NULL)
  }
  
  # ============================================================================
  # [i18n] 使用 SimFit 私有翻译函数，避免被其他 legacy 模块覆盖
  # ============================================================================
  load_simfit_i18n <- function() {
    empty_tbl <- data.frame(
      Key = character(0),
      Chinese = character(0),
      English = character(0),
      Category = character(0),
      Notes = character(0),
      stringsAsFactors = FALSE
    )
    csv_candidates <- unique(c(
      "i18n_translation_table.csv",
      file.path(getwd(), "i18n_translation_table.csv"),
      file.path(getwd(), "ITCsimfit", "i18n_translation_table.csv"),
      file.path(dirname(getwd()), "ITCsimfit", "i18n_translation_table.csv")
    ))
    csv_hits <- csv_candidates[file.exists(csv_candidates)]
    csv_path <- if (length(csv_hits) > 0) csv_hits[[1]] else NA_character_
    tbl <- if (!is.na(csv_path) && nzchar(csv_path)) {
      tryCatch(
        read.csv(csv_path, stringsAsFactors = FALSE, encoding = "UTF-8"),
        error = function(e) {
          warning("Failed to read SimFit i18n table: ", e$message)
          empty_tbl
        }
      )
    } else {
      warning("SimFit i18n table not found. Falling back to key names.")
      empty_tbl
    }

    tr_map <- new.env(parent = emptyenv())
    if (nrow(tbl) > 0 && all(c("Key", "Chinese", "English") %in% names(tbl))) {
      for (i in seq_len(nrow(tbl))) {
        key <- tbl$Key[[i]]
        if (is.null(key) || is.na(key) || key == "") next
        zh_val <- tbl$Chinese[[i]]
        en_val <- tbl$English[[i]]
        assign(
          key,
          list(
            zh = if (is.null(zh_val) || is.na(zh_val) || zh_val == "") key else as.character(zh_val),
            en = if (is.null(en_val) || is.na(en_val) || en_val == "") key else as.character(en_val)
          ),
          envir = tr_map
        )
      }
    }

    pretty_key_fallback <- function(key) {
      key_chr <- as.character(key)[1]
      if (is.na(key_chr) || key_chr == "") return("")
      txt <- gsub("_", " ", key_chr, fixed = TRUE)
      txt <- gsub("\\s+", " ", txt, perl = TRUE)
      txt <- trimws(txt)
      words <- strsplit(txt, " ", fixed = TRUE)[[1]]
      words <- vapply(words, function(w) {
        if (!nzchar(w)) return(w)
        paste0(toupper(substr(w, 1, 1)), substr(w, 2, nchar(w)))
      }, FUN.VALUE = character(1))
      paste(words, collapse = " ")
    }

    tr_local <- function(key, lang = "en") {
      if (is.null(key) || length(key) == 0 || is.na(key) || key == "") return("")
      lang_norm <- if (identical(as.character(lang)[1], "zh")) "zh" else "en"
      if (exists(key, envir = tr_map, inherits = FALSE)) {
        val <- get(key, envir = tr_map, inherits = FALSE)[[lang_norm]]
        if (!is.null(val) && !is.na(val) && val != "") return(val)
      }
      pretty_key_fallback(key)
    }
    trf_local <- function(key, lang = "en", ...) {
      template <- tr_local(key, lang)
      args <- list(...)
      if (length(args) == 0) return(template)
      tryCatch(do.call(sprintf, c(list(template), args)), error = function(e) template)
    }
    list(tr = tr_local, trf = trf_local)
  }
  simfit_i18n <- load_simfit_i18n()
  tr <- simfit_i18n$tr
  trf <- simfit_i18n$trf
  
  # ============================================================================
  # [i18n] English-first mode (language switch disabled for now)
  # ============================================================================
  current_lang <- reactiveVal("en")
  lang_switching <- reactiveVal(FALSE)

  lang <- reactive("en")

  get_lang_safe <- function() {
    "en"
  }

  output$app_title_dynamic <- renderUI(NULL)
  
  # 渲染语言切换按钮（仿照 processor：小国旗 + 目标语言名）
  output$lang_switch_button <- renderUI(NULL)
  
