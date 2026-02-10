# ==============================================================================
# i18n.R - 国际化翻译管理模块
# ==============================================================================
# 本模块负责管理应用的多语言支持（中文/英文）

# 加载翻译表
csv_paths <- c(
  "i18n_translation_table.csv",
  file.path(getwd(), "i18n_translation_table.csv"),
  file.path(dirname(getwd()), "i18n_translation_table.csv")
)
csv_path <- NULL
for (path in csv_paths) {
  if (file.exists(path)) {
    csv_path <- path
    break
  }
}
if (is.null(csv_path)) {
  warning("Cannot find i18n_translation_table.csv in standard locations.")
  translation_table <- data.frame(
    Key = character(0),
    Chinese = character(0),
    English = character(0),
    stringsAsFactors = FALSE
  )
} else {
  translation_table <- read.csv(csv_path, stringsAsFactors = FALSE, encoding = "UTF-8")
}

# 创建翻译查找表
translation_map <- list()
if (nrow(translation_table) > 0) {
  for (i in 1:nrow(translation_table)) {
    key <- translation_table$Key[i]
    if (!is.null(key) && !is.na(key) && key != "") {
      translation_map[[key]] <- list(
        zh = if (!is.null(translation_table$Chinese[i]) && !is.na(translation_table$Chinese[i])) translation_table$Chinese[i] else key,
        en = if (!is.null(translation_table$English[i]) && !is.na(translation_table$English[i])) translation_table$English[i] else key
      )
    }
  }
}

# 翻译函数
tr <- function(key, lang = "en") {
  if (is.null(key) || is.na(key) || key == "") return("")
  if (key %in% names(translation_map)) {
    translated <- translation_map[[key]][[lang]]
    if (!is.null(translated) && !is.na(translated) && translated != "") {
      return(translated)
    }
  }
  return(key)
}

# 参数化翻译函数
trf <- function(key, lang = "en", ...) {
  template <- tr(key, lang)
  if (length(list(...)) > 0) {
    tryCatch({
      return(sprintf(template, ...))
    }, error = function(e) {
      return(template)
    })
  }
  return(template)
}
