resolve_repo_root <- function() {
  env_root <- Sys.getenv("ITCSUITE_REPO_ROOT", unset = "")
  candidates <- unique(c(
    env_root,
    getwd(),
    file.path(getwd(), ".."),
    file.path(getwd(), "..", "..")
  ))
  for (cand in candidates) {
    if (!nzchar(cand)) next
    normalized <- normalizePath(cand, winslash = "/", mustWork = FALSE)
    if (!dir.exists(normalized)) next
    if (dir.exists(file.path(normalized, "ITCsimfit")) && dir.exists(file.path(normalized, "tests"))) {
      return(normalizePath(normalized, winslash = "/", mustWork = TRUE))
    }
  }
  stop("Cannot resolve repository root for commenting standard test.")
}

repo_root <- resolve_repo_root()
simfit_dir <- file.path(repo_root, "ITCsimfit")

target_files <- c(
  file.path(simfit_dir, "R", "core_logic.R"),
  file.path(simfit_dir, "R", "fitting.R"),
  file.path(simfit_dir, "R", "server", "body", "runtime_core", "01_bridge_state_inputs.R"),
  file.path(simfit_dir, "R", "server", "body", "snapshot_export", "02_export_bridge.R"),
  file.path(simfit_dir, "R", "infrastructure", "errors.R")
)

testthat::test_that("target files exist for comment standard baseline", {
  testthat::expect_true(all(file.exists(target_files)))
})

testthat::test_that("all target files have module header markers", {
  for (path in target_files) {
    txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    testthat::expect_true(
      grepl("\\[COMMENT_STD\\]\\[MODULE_HEADER\\]", txt),
      info = basename(path)
    )
    testthat::expect_true(grepl("模块职责：", txt), info = basename(path))
    testthat::expect_true(grepl("依赖：", txt), info = basename(path))
    testthat::expect_true(grepl("对外接口：", txt), info = basename(path))
    testthat::expect_true(grepl("副作用：", txt), info = basename(path))
    testthat::expect_true(grepl("变更历史：", txt), info = basename(path))
  }
})

testthat::test_that("comment categories and required fields are present", {
  txt_all <- paste(
    unlist(lapply(target_files, function(path) readLines(path, warn = FALSE, encoding = "UTF-8"))),
    collapse = "\n"
  )

  testthat::expect_true(grepl("\\[COMMENT_STD\\]\\[KEY_ALGO\\]", txt_all))
  testthat::expect_true(grepl("算法目标：", txt_all))
  testthat::expect_true(grepl("输入约束：", txt_all))
  testthat::expect_true(grepl("数值稳定策略：", txt_all))
  testthat::expect_true(grepl("失败回退：", txt_all))
  testthat::expect_true(grepl("复杂度：", txt_all))

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
