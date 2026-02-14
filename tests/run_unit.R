#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
script_arg <- commandArgs()
script_file <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)][1])
script_dir <- dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE))
source(file.path(script_dir, "lib", "runner_utils.R"))
strict_mode <- parse_strict_flag(args)

setwd(normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE))
repo_root <- detect_repo_root()
setwd(repo_root)
Sys.setenv(ITCSUITE_REPO_ROOT = repo_root)
on.exit(Sys.unsetenv("ITCSUITE_REPO_ROOT"), add = TRUE)

cat(sprintf("Running unit tests (strict=%s)\n", tolower(as.character(strict_mode))))

results <- list()
required_keys <- character(0)

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Unit runner requires package `testthat`.")
}

results[["ITCSuiteWeb bridge contract"]] <- run_testthat_file(
  file.path(repo_root, "ITCSuiteWeb", "tests", "testthat", "test-bridge-contract.R"),
  label = "ITCSuiteWeb/test-bridge-contract.R"
)
required_keys <- c(required_keys, "ITCSuiteWeb bridge contract")

results[["ITCSuiteWeb guide annotation schema"]] <- run_testthat_file(
  file.path(repo_root, "ITCSuiteWeb", "tests", "testthat", "test-guide-annotations-schema.R"),
  label = "ITCSuiteWeb/test-guide-annotations-schema.R"
)
required_keys <- c(required_keys, "ITCSuiteWeb guide annotation schema")

results[["ITCSuiteWeb guide annotation resolver"]] <- run_testthat_file(
  file.path(repo_root, "ITCSuiteWeb", "tests", "testthat", "test-guide-annotations-resolver.R"),
  label = "ITCSuiteWeb/test-guide-annotations-resolver.R"
)
required_keys <- c(required_keys, "ITCSuiteWeb guide annotation resolver")

results[["ITCSuiteWeb home recent helpers"]] <- run_testthat_file(
  file.path(repo_root, "ITCSuiteWeb", "tests", "testthat", "test-home-recent-helpers.R"),
  label = "ITCSuiteWeb/test-home-recent-helpers.R"
)
required_keys <- c(required_keys, "ITCSuiteWeb home recent helpers")

results[["ITCSuiteWeb home recent store"]] <- run_testthat_file(
  file.path(repo_root, "ITCSuiteWeb", "tests", "testthat", "test-home-recent-store.R"),
  label = "ITCSuiteWeb/test-home-recent-store.R"
)
required_keys <- c(required_keys, "ITCSuiteWeb home recent store")

results[["ITCSuiteWeb home desktop helpers"]] <- run_testthat_file(
  file.path(repo_root, "ITCSuiteWeb", "tests", "testthat", "test-home-desktop-helpers.R"),
  label = "ITCSuiteWeb/test-home-desktop-helpers.R"
)
required_keys <- c(required_keys, "ITCSuiteWeb home desktop helpers")

results[["ITCSuiteWeb comment standard minimal"]] <- run_testthat_file(
  file.path(repo_root, "ITCSuiteWeb", "tests", "testthat", "test-commenting-standard-minimal.R"),
  label = "ITCSuiteWeb/test-commenting-standard-minimal.R"
)
required_keys <- c(required_keys, "ITCSuiteWeb comment standard minimal")

results[["ITCgraph guide annotation schema"]] <- run_testthat_file(
  file.path(repo_root, "ITCgraph", "tests", "testthat", "test-guide-annotations-schema.R"),
  label = "ITCgraph/test-guide-annotations-schema.R"
)
required_keys <- c(required_keys, "ITCgraph guide annotation schema")

results[["ITCgraph guide annotation resolver"]] <- run_testthat_file(
  file.path(repo_root, "ITCgraph", "tests", "testthat", "test-guide-annotations-resolver.R"),
  label = "ITCgraph/test-guide-annotations-resolver.R"
)
required_keys <- c(required_keys, "ITCgraph guide annotation resolver")

results[["ITCgraph comment standard minimal"]] <- run_testthat_file(
  file.path(repo_root, "ITCgraph", "tests", "testthat", "test-commenting-standard-minimal.R"),
  label = "ITCgraph/test-commenting-standard-minimal.R"
)
required_keys <- c(required_keys, "ITCgraph comment standard minimal")

results[["ITCprocessor guide annotation schema"]] <- run_testthat_file(
  file.path(repo_root, "ITCprocessor", "tests", "testthat", "test-guide-annotations-schema.R"),
  label = "ITCprocessor/test-guide-annotations-schema.R"
)
required_keys <- c(required_keys, "ITCprocessor guide annotation schema")

results[["ITCprocessor guide annotation resolver"]] <- run_testthat_file(
  file.path(repo_root, "ITCprocessor", "tests", "testthat", "test-guide-annotations-resolver.R"),
  label = "ITCprocessor/test-guide-annotations-resolver.R"
)
required_keys <- c(required_keys, "ITCprocessor guide annotation resolver")

results[["ITCprocessor comment standard minimal"]] <- run_testthat_file(
  file.path(repo_root, "ITCprocessor", "tests", "testthat", "test-commenting-standard-minimal.R"),
  label = "ITCprocessor/test-commenting-standard-minimal.R"
)
required_keys <- c(required_keys, "ITCprocessor comment standard minimal")

root_testthat_dir <- file.path(repo_root, "tests", "testthat")
if (dir.exists(root_testthat_dir)) {
  results[["Root testthat suite"]] <- run_testthat_dir(
    root_testthat_dir,
    label = "tests/testthat"
  )
  required_keys <- c(required_keys, "Root testthat suite")
}

results[["ITCsimfit guide annotation schema"]] <- run_testthat_file(
  file.path(repo_root, "ITCsimfit", "tests", "test_guide_annotations_schema.R"),
  label = "ITCsimfit/tests/test_guide_annotations_schema.R"
)
required_keys <- c(required_keys, "ITCsimfit guide annotation schema")

results[["ITCsimfit guide annotation resolver"]] <- run_testthat_file(
  file.path(repo_root, "ITCsimfit", "tests", "test_guide_annotations_resolver.R"),
  label = "ITCsimfit/tests/test_guide_annotations_resolver.R"
)
required_keys <- c(required_keys, "ITCsimfit guide annotation resolver")

results[["ITCsimfit comment standard minimal"]] <- run_testthat_file(
  file.path(repo_root, "ITCsimfit", "tests", "test_commenting_standard_minimal.R"),
  label = "ITCsimfit/tests/test_commenting_standard_minimal.R"
)
required_keys <- c(required_keys, "ITCsimfit comment standard minimal")

required_legacy_scripts <- c(
  "tests/test_core_logic.R",
  "tests/test_fitting.R",
  "tests/test_server_improvements.R"
)

optional_legacy_scripts <- c(
  "tests/test_constants_utils.R",
  "tests/test_core_logic_improvements.R",
  "tests/test_error_i18n.R",
  "tests/test_performance.R"
)

for (rel in required_legacy_scripts) {
  key <- paste0("ITCsimfit/", rel)
  results[[key]] <- run_legacy_script(
    script_path = file.path(repo_root, "ITCsimfit", rel),
    working_dir = file.path(repo_root, "ITCsimfit"),
    label = key
  )
  required_keys <- c(required_keys, key)
}

for (rel in optional_legacy_scripts) {
  key <- paste0("ITCsimfit/", rel, " (optional)")
  results[[key]] <- run_legacy_script(
    script_path = file.path(repo_root, "ITCsimfit", rel),
    working_dir = file.path(repo_root, "ITCsimfit"),
    label = key
  )
}

print_runner_summary(results)

ok <- all(vapply(required_keys, function(k) isTRUE(results[[k]]$ok), logical(1)))
quit(save = "no", status = if (ok) 0 else 1)
