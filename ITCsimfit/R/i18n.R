# ==============================================================================
# i18n.R - 国际化翻译管理模块
# ==============================================================================
# 本模块负责管理应用的多语言支持（中文/英文）

# 加载翻译表
# 使用更简单可靠的方法：尝试多个可能的路径
# 在 Shiny 应用中，工作目录通常是 app.R 所在的目录
csv_paths <- c(
  "i18n_translation_table.csv",  # 当前工作目录（通常是项目根目录）
  file.path(getwd(), "i18n_translation_table.csv"),  # 明确的工作目录
  file.path(dirname(getwd()), "i18n_translation_table.csv")  # 父目录（备用）
)
csv_path <- NULL
for (path in csv_paths) {
  if (file.exists(path)) {
    csv_path <- path
    break
  }
}
if (is.null(csv_path)) {
  # 如果都找不到，尝试使用绝对路径（基于已知的项目结构）
  # 假设 CSV 文件在项目根目录
  warning("Cannot find i18n_translation_table.csv in standard locations. Trying to continue...")
  # 创建一个空的翻译表作为后备
  translation_table <- data.frame(
    Key = character(0),
    Chinese = character(0),
    English = character(0),
    Category = character(0),
    Notes = character(0),
    stringsAsFactors = FALSE
  )
} else {
  translation_table <- read.csv(csv_path, stringsAsFactors = FALSE, encoding = "UTF-8")
}

# 创建翻译查找表（提高查找效率）
translation_map <- list()
if (nrow(translation_table) > 0) {
  for (i in 1:nrow(translation_table)) {
    key <- translation_table$Key[i]
    if (!is.null(key) && !is.na(key) && key != "") {
      translation_map[[key]] <- list(
        zh = if(!is.null(translation_table$Chinese[i]) && !is.na(translation_table$Chinese[i])) translation_table$Chinese[i] else key,
        en = if(!is.null(translation_table$English[i]) && !is.na(translation_table$English[i])) translation_table$English[i] else key
      )
    }
  }
}

# 翻译函数
# @param key: 翻译键
# @param lang: 语言代码 ("zh" 或 "en")
# @return: 翻译后的文本，如果找不到则返回key本身
tr <- function(key, lang = "zh") {
  if (is.null(key) || is.na(key) || key == "") {
    return("")
  }
  
  # 如果key在翻译表中
  if (key %in% names(translation_map)) {
    translated <- translation_map[[key]][[lang]]
    if (!is.null(translated) && !is.na(translated) && translated != "") {
      return(translated)
    }
  }
  
  # 如果找不到翻译，返回key本身（便于调试）
  return(key)
}

# 参数化翻译函数（支持 sprintf 风格的格式化）
# @param key: 翻译键
# @param lang: 语言代码
# @param ...: 传递给 sprintf 的参数
trf <- function(key, lang = "zh", ...) {
  template <- tr(key, lang)
  if (length(list(...)) > 0) {
    tryCatch({
      return(sprintf(template, ...))
    }, error = function(e) {
      # 如果格式化失败，返回模板
      return(template)
    })
  }
  return(template)
}

# 注意：语言状态管理将在 server.R 中使用 reactiveVal 实现
# 因为需要访问 Shiny 的 session 对象
