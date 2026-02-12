resolve_repo_root <- function() {
  env_root <- Sys.getenv("ITCSUITE_REPO_ROOT", unset = "")
  if (nzchar(env_root)) {
    p <- normalizePath(env_root, winslash = "/", mustWork = FALSE)
    if (dir.exists(file.path(p, "ITCSuiteWeb")) && dir.exists(file.path(p, "tests"))) {
      return(normalizePath(p, winslash = "/", mustWork = TRUE))
    }
  }

  cur <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  for (i in 0:8) {
    if (dir.exists(file.path(cur, "ITCSuiteWeb")) && dir.exists(file.path(cur, "tests"))) {
      return(normalizePath(cur, winslash = "/", mustWork = TRUE))
    }
    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }
  stop("Cannot resolve repository root.")
}

repo_root <- resolve_repo_root()
web_dir <- file.path(repo_root, "ITCSuiteWeb")

target_files <- c(
  file.path(web_dir, "app.R"),
  file.path(web_dir, "R", "bridge_contract.R")
)

testthat::test_that("comment standard target files exist", {
  testthat::expect_true(all(file.exists(target_files)))
})

testthat::test_that("module header markers and fields exist", {
  for (path in target_files) {
    txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    testthat::expect_true(grepl("\\[COMMENT_STD\\]\\[MODULE_HEADER\\]", txt), info = basename(path))
    testthat::expect_true(grepl("模块职责：", txt), info = basename(path))
    testthat::expect_true(grepl("依赖：", txt), info = basename(path))
    testthat::expect_true(grepl("对外接口：", txt), info = basename(path))
    testthat::expect_true(grepl("副作用：", txt), info = basename(path))
    testthat::expect_true(grepl("变更历史：", txt), info = basename(path))
  }
})

testthat::test_that("io contract and error semantics markers exist in app/bridge contract", {
  txt_all <- paste(
    unlist(lapply(target_files, function(path) readLines(path, warn = FALSE, encoding = "UTF-8"))),
    collapse = "\n"
  )

  testthat::expect_true(grepl("\\[COMMENT_STD\\]\\[IO_CONTRACT\\]", txt_all))
  testthat::expect_true(grepl("输入来源：", txt_all))
  testthat::expect_true(grepl("字段/类型：", txt_all))
  testthat::expect_true(grepl("单位：", txt_all))
  testthat::expect_true(grepl("空值策略：", txt_all))
  testthat::expect_true(grepl("输出保证：", txt_all))

  testthat::expect_true(grepl("\\[COMMENT_STD\\]\\[ERROR_SEMANTICS\\]", txt_all))
  testthat::expect_true(grepl("错误码/类别：", txt_all))
  testthat::expect_true(grepl("触发条件：", txt_all))
  testthat::expect_true(grepl("用户可见性：", txt_all))
  testthat::expect_true(grepl("日志级别：", txt_all))
  testthat::expect_true(grepl("恢复动作：", txt_all))
})
