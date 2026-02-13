# ==============================================================================
# 测试文件：错误消息国际化
# ==============================================================================
# 测试 handle_error 和 get_error_message 函数的国际化功能

# 设置测试环境
# 不使用 testthat，使用基础 R 测试

# 加载必要的模块（加载到当前测试环境，避免依赖全局环境污染）
source("R/constants.R", local = TRUE)
source("R/utils.R", local = TRUE)

# 模拟翻译函数（如果不存在）
if(!exists("tr", inherits = FALSE)) {
  # 加载翻译表
  if(file.exists("i18n_translation_table.csv")) {
    i18n_table <- read.csv("i18n_translation_table.csv", stringsAsFactors = FALSE)
    
    tr <- function(key, lang = "zh") {
      if(lang == "zh") {
        result <- i18n_table$Chinese[i18n_table$Key == key]
      } else {
        result <- i18n_table$English[i18n_table$Key == key]
      }
      
      if(length(result) == 0 || is.na(result)) {
        return(key)  # 如果找不到翻译，返回键本身
      }
      return(result[1])
    }
  } else {
    # 如果翻译表不存在，创建一个简单的模拟函数
    tr <- function(key, lang = "zh") {
      return(key)
    }
  }
}

# ==============================================================================
# 测试套件
# ==============================================================================

test_results <- list()
test_count <- 0
pass_count <- 0

run_test <- function(test_name, test_expr) {
  test_count <<- test_count + 1
  cat(sprintf("\n[测试 %d] %s\n", test_count, test_name))
  
  result <- tryCatch({
    test_expr
    cat("  ✓ 通过\n")
    pass_count <<- pass_count + 1
    TRUE
  }, error = function(e) {
    cat(sprintf("  ✗ 失败: %s\n", e$message))
    FALSE
  })
  
  test_results[[test_name]] <<- result
  return(result)
}

cat("==================================================\n")
cat("测试：错误消息国际化\n")
cat("==================================================\n")

# ==============================================================================
# 1. 测试 get_error_message 函数
# ==============================================================================

cat("\n### 1. 测试 get_error_message 函数\n")

# 测试 1.1: 数据错误代码 - 中文
run_test("获取 E001 错误消息（中文）", {
  msg <- get_error_message("E001", "zh")
  stopifnot(is.list(msg))
  stopifnot("title" %in% names(msg))
  stopifnot("desc" %in% names(msg))
  stopifnot(nchar(msg$title) > 0)
  stopifnot(nchar(msg$desc) > 0)
  cat(sprintf("    标题: %s\n", msg$title))
  cat(sprintf("    描述: %s\n", msg$desc))
})

# 测试 1.2: 数据错误代码 - 英文
run_test("获取 E001 错误消息（英文）", {
  msg <- get_error_message("E001", "en")
  stopifnot(is.list(msg))
  stopifnot("title" %in% names(msg))
  stopifnot("desc" %in% names(msg))
  stopifnot(nchar(msg$title) > 0)
  stopifnot(nchar(msg$desc) > 0)
  cat(sprintf("    Title: %s\n", msg$title))
  cat(sprintf("    Desc: %s\n", msg$desc))
})

# 测试 1.3: 所有错误代码 - 中文
run_test("获取所有错误代码的消息（中文）", {
  for(code_name in names(ERROR_CODES)) {
    code <- ERROR_CODES[[code_name]]
    msg <- get_error_message(code, "zh")
    stopifnot(is.list(msg))
    stopifnot(nchar(msg$title) > 0)
    cat(sprintf("    [%s] %s\n", code, msg$title))
  }
})

# 测试 1.4: 所有错误代码 - 英文
run_test("获取所有错误代码的消息（英文）", {
  for(code_name in names(ERROR_CODES)) {
    code <- ERROR_CODES[[code_name]]
    msg <- get_error_message(code, "en")
    stopifnot(is.list(msg))
    stopifnot(nchar(msg$title) > 0)
    cat(sprintf("    [%s] %s\n", code, msg$title))
  }
})

# 测试 1.5: 不存在的错误代码
run_test("获取不存在的错误代码消息", {
  msg <- get_error_message("E999999", "zh")
  stopifnot(is.list(msg))
  # 应该返回翻译键或错误代码作为标题（取决于tr函数的实现）
  stopifnot(nchar(msg$title) > 0)
  cat(sprintf("    不存在的代码返回: %s\n", msg$title))
})

# ==============================================================================
# 2. 测试 handle_error 函数的国际化
# ==============================================================================

cat("\n### 2. 测试 handle_error 函数的国际化\n")

# 创建临时日志文件
temp_log <- tempfile(fileext = ".log")
old_file_paths <- if(exists("FILE_PATHS", envir = .GlobalEnv, inherits = FALSE)) {
  get("FILE_PATHS", envir = .GlobalEnv, inherits = FALSE)
} else {
  NULL
}
assign("FILE_PATHS", list(error_log = temp_log, session_log = temp_log), envir = .GlobalEnv)

# 测试 2.1: 带错误代码的错误处理 - 中文
run_test("handle_error 带错误代码（中文）", {
  e <- simpleError("测试错误消息")
  # 捕获输出
  output <- capture.output({
    result <- handle_error(e, 
                          context = "测试上下文", 
                          show_to_user = FALSE,  # 不显示通知（避免需要 Shiny 环境）
                          log_to_file = TRUE,
                          lang_val = "zh",
                          error_code = ERROR_CODES$DATA_INVALID)
  }, type = "message")
  
  # 检查日志文件
  stopifnot(file.exists(temp_log))
  log_content <- readLines(temp_log)
  stopifnot(length(log_content) > 0)
  
  # 检查日志中包含错误代码
  stopifnot(any(grepl("E001", log_content)))
  cat(sprintf("    日志内容: %s\n", tail(log_content, 1)))
})

# 测试 2.2: 带错误代码的错误处理 - 英文
run_test("handle_error 带错误代码（英文）", {
  e <- simpleError("Test error message")
  # 捕获输出
  output <- capture.output({
    result <- handle_error(e, 
                          context = "Test context", 
                          show_to_user = FALSE,
                          log_to_file = TRUE,
                          lang_val = "en",
                          error_code = ERROR_CODES$PARAM_INVALID)
  }, type = "message")
  
  # 检查日志文件
  log_content <- readLines(temp_log)
  stopifnot(length(log_content) > 0)
  
  # 检查日志中包含错误代码
  stopifnot(any(grepl("E101", log_content)))
  cat(sprintf("    Log content: %s\n", tail(log_content, 1)))
})

# 测试 2.3: 不同类型的错误代码
run_test("handle_error 处理不同类型的错误", {
  test_cases <- list(
    list(code = ERROR_CODES$DATA_MISSING, msg = "数据缺失测试"),
    list(code = ERROR_CODES$SOLVER_FAILED, msg = "求解器失败测试"),
    list(code = ERROR_CODES$FILE_NOT_FOUND, msg = "文件未找到测试")
  )
  
  for(tc in test_cases) {
    e <- simpleError(tc$msg)
    output <- capture.output({
      handle_error(e, 
                  show_to_user = FALSE,
                  log_to_file = TRUE,
                  lang_val = "zh",
                  error_code = tc$code)
    }, type = "message")
    
    log_content <- readLines(temp_log)
    stopifnot(any(grepl(tc$code, log_content)))
    cat(sprintf("    [%s] 已记录\n", tc$code))
  }
})

# 测试 2.4: 无错误代码的错误处理（向后兼容）
run_test("handle_error 无错误代码（向后兼容）", {
  e <- simpleError("普通错误消息")
  output <- capture.output({
    result <- handle_error(e, 
                          context = "测试", 
                          show_to_user = FALSE,
                          log_to_file = TRUE,
                          lang_val = "zh",
                          error_code = NULL)  # 无错误代码
  }, type = "message")
  
  # 应该仍然能正常工作
  log_content <- readLines(temp_log)
  stopifnot(length(log_content) > 0)
  stopifnot(any(grepl("普通错误消息", log_content)))
})

# ==============================================================================
# 3. 测试与 safe_execute 的集成
# ==============================================================================

cat("\n### 3. 测试与 safe_execute 的集成\n")

# 测试 3.1: safe_execute 使用错误代码
run_test("safe_execute 集成错误代码", {
  # 清空日志
  writeLines("", temp_log)
  
  result <- safe_execute({
    stop("数据验证失败")
  }, context = "数据加载", 
     default = NULL,
     show_error = FALSE,
     lang_val = "zh")
  
  # safe_execute 目前不直接支持 error_code 参数
  # 但可以通过扩展来支持
  stopifnot(is.null(result))
})

# ==============================================================================
# 清理
# ==============================================================================

# 恢复原始 FILE_PATHS
if(!is.null(old_file_paths)) {
  assign("FILE_PATHS", old_file_paths, envir = .GlobalEnv)
} else {
  if (exists("FILE_PATHS", envir = .GlobalEnv, inherits = FALSE)) {
    rm("FILE_PATHS", envir = .GlobalEnv)
  }
}

# 删除临时日志文件
if(file.exists(temp_log)) {
  unlink(temp_log)
}

# ==============================================================================
# 测试总结
# ==============================================================================

cat("\n==================================================\n")
cat("测试总结\n")
cat("==================================================\n")
cat(sprintf("通过: %d\n", pass_count))
cat(sprintf("失败: %d\n", test_count - pass_count))
cat(sprintf("总计: %d\n", test_count))
cat(sprintf("通过率: %.1f%%\n", 100 * pass_count / test_count))

if(pass_count == test_count) {
  cat("\n✅ 所有测试通过！\n")
} else {
  cat("\n❌ 部分测试失败\n")
  cat("\n失败的测试:\n")
  for(name in names(test_results)) {
    if(!test_results[[name]]) {
      cat(sprintf("  - %s\n", name))
    }
  }
}

cat("==================================================\n")
