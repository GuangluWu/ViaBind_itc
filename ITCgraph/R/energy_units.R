# ==============================================================================
# energy_units.R - 能量单位相关标签管理（类似 i18n）
# ==============================================================================
# 切换能量单位时，多处 UI 和绘图需使用对应单位的标签
# 本模块提供 unit_label(key, unit) 统一查找

unit_label_csv_paths <- c(
  "energy_unit_labels.csv",
  file.path(getwd(), "energy_unit_labels.csv"),
  file.path(dirname(getwd()), "energy_unit_labels.csv")
)
unit_csv_path <- NULL
for (path in unit_label_csv_paths) {
  if (file.exists(path)) {
    unit_csv_path <- path
    break
  }
}

if (is.null(unit_csv_path)) {
  # 内置默认
  unit_labels_table <- data.frame(
    Key = c("top_ylab", "bot_ylab"),
    cal = c("Delta Power (\u00B5cal/s)", "heat of injection (kcal/mol)"),
    J = c("Delta Power (\u00B5W)", "Heat of Injection (kJ/mol)"),
    stringsAsFactors = FALSE
  )
} else {
  unit_labels_table <- read.csv(unit_csv_path, stringsAsFactors = FALSE, encoding = "UTF-8")
}

# 构建查找表
unit_labels_map <- list()
if (nrow(unit_labels_table) > 0 && "Key" %in% colnames(unit_labels_table)) {
  valid_units <- setdiff(colnames(unit_labels_table), "Key")
  for (i in seq_len(nrow(unit_labels_table))) {
    key <- unit_labels_table$Key[i]
    if (!is.null(key) && !is.na(key) && key != "") {
      unit_labels_map[[key]] <- list()
      for (u in valid_units) {
        val <- unit_labels_table[[u]][i]
        if (!is.null(val) && !is.na(val) && val != "") {
          unit_labels_map[[key]][[u]] <- val
        }
      }
    }
  }
}

#' 根据能量单位获取标签
#' @param key 标签键名，如 "top_ylab", "bot_ylab"
#' @param unit 能量单位，"cal" 或 "J"
#' @param fallback 找不到时的回退值
#' @return 对应单位的标签字符串
unit_label <- function(key, unit = "cal", fallback = NULL) {
  if (is.null(key) || is.na(key) || key == "") return(fallback %||% "")
  u <- unit %||% "cal"
  if (!u %in% c("cal", "J")) u <- "cal"
  if (key %in% names(unit_labels_map) && u %in% names(unit_labels_map[[key]])) {
    return(unit_labels_map[[key]][[u]])
  }
  return(fallback %||% key)
}
