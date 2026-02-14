repo_root <- itcsuite_repo_root()

read_src <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

testthat::test_that("host app defines home tab and itcsuite_home interface", {
  src <- read_src(file.path(repo_root, "ITCSuiteWeb", "app.R"))
  testthat::expect_true(grepl("selected\\s*=\\s*\"home\"", src, perl = TRUE))
  testthat::expect_true(grepl("value\\s*=\\s*\"home\"", src, perl = TRUE))
  testthat::expect_true(grepl("session\\$userData\\$itcsuite_home\\s*<-\\s*list", src, perl = TRUE))
  testthat::expect_true(grepl("register_restore_handler", src, perl = TRUE))
  testthat::expect_true(grepl("source\\(\"R/home_recent_store\\.R\"\\)", src, perl = TRUE))
  testthat::expect_true(grepl("home_recent_store_load\\(", src, perl = TRUE))
  testthat::expect_true(grepl("home_recent_store_save\\(", src, perl = TRUE))
})

testthat::test_that("step1 registers restore handler and reports recent import", {
  src <- read_src(file.path(repo_root, "ITCprocessor", "app.R"))
  testthat::expect_true(grepl("home_register_restore\\(\"step1\"", src, perl = TRUE))
  testthat::expect_true(grepl("home_add_recent\\(", src, perl = TRUE))
  testthat::expect_true(grepl("home_add_recent_export\\(", src, perl = TRUE))
  testthat::expect_true(grepl("import_type\\s*=\\s*\"itc\"", src, perl = TRUE))
})

testthat::test_that("step2 registers restore handler and reuses import state function", {
  src <- read_src(file.path(
    repo_root,
    "ITCsimfit",
    "R",
    "server",
    "body",
    "runtime_core",
    "01_bridge_state_inputs.R"
  ))
  testthat::expect_true(grepl("home_register_restore\\(\"step2\"", src, perl = TRUE))
  testthat::expect_true(grepl("apply_imported_xlsx_state\\s*<-\\s*function", src, perl = TRUE))
  testthat::expect_true(grepl("home_add_recent\\(", src, perl = TRUE))
  testthat::expect_true(grepl("home_add_recent_export\\s*<-\\s*function", src, perl = TRUE))
})

testthat::test_that("step3 registers restore handler and reports import/export to home", {
  src <- read_src(file.path(repo_root, "ITCgraph", "server.R"))
  testthat::expect_true(grepl("home_register_restore\\(\"step3\"", src, perl = TRUE))
  testthat::expect_true(grepl("record_step3_recent_import", src, perl = TRUE))
  testthat::expect_true(grepl("record_step3_recent_export", src, perl = TRUE))
})
