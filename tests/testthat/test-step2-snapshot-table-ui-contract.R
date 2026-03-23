repo_root <- itcsuite_repo_root()
snapshot_src <- paste(
  readLines(
    file.path(
      repo_root,
      "ITCsimfit",
      "R",
      "server",
      "body",
      "snapshot_export",
      "01_snapshot_management.R"
    ),
    warn = FALSE
  ),
  collapse = "\n"
)

testthat::test_that("step2 snapshot table disables pagination and keeps internal scrolling", {
  required_patterns <- c(
    "dom\\s*=\\s*\"t\"",
    "paging\\s*=\\s*FALSE",
    "scrollX\\s*=\\s*TRUE",
    "scrollY\\s*=\\s*\"120px\""
  )

  for (pattern in required_patterns) {
    testthat::expect_true(grepl(pattern, snapshot_src, perl = TRUE), info = pattern)
  }

  testthat::expect_false(
    grepl("pageLength\\s*=", snapshot_src, perl = TRUE),
    info = "Snapshot table should not reintroduce pageLength-based pagination."
  )
})

testthat::test_that("step2 snapshot table active-row styling no longer depends on current-page paging", {
  testthat::expect_true(
    grepl("function\\s+markActiveById\\(id\\)\\s*\\{[\\s\\S]*?table\\.rows\\(\\)\\.every\\(function\\(\\)", snapshot_src, perl = TRUE)
  )

  testthat::expect_false(
    grepl("table\\.rows\\(\\{\\s*page\\s*:\\s*'current'\\s*\\}\\)\\.every", snapshot_src, perl = TRUE),
    info = "Active-row highlighting should work across the full scrollable table, not a paged subset."
  )
})
