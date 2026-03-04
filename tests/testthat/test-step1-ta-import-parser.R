repo_root <- itcsuite_repo_root()
proc_dir <- file.path(repo_root, "ITCprocessor")

source(file.path(proc_dir, "R", "data_parser.R"), local = FALSE)
source(file.path(proc_dir, "R", "baseline.R"), local = FALSE)
source(file.path(proc_dir, "R", "integration.R"), local = FALSE)

testthat::test_that("detect_step1_source_type supports TA extensions and display-name override", {
  testthat::expect_identical(detect_step1_source_type("a.itc"), "itc")
  testthat::expect_identical(detect_step1_source_type("a.txt"), "itc")
  testthat::expect_identical(detect_step1_source_type("a.nitc"), "ta_nitc")
  testthat::expect_identical(detect_step1_source_type("a.csc"), "ta_csc")
  testthat::expect_identical(detect_step1_source_type("a.xml"), "ta_xml")

  tmp_no_ext <- file.path(tempdir(), "ta_source_no_ext")
  testthat::expect_identical(detect_step1_source_type(tmp_no_ext, display_name = "sample.nitc"), "ta_nitc")
  testthat::expect_identical(detect_step1_source_type(tmp_no_ext, display_name = "sample.xlsx"), "xlsx")
})

testthat::test_that("convert_ta_to_xlsx handles source path with spaces", {
  src <- file.path(repo_root, "Examples", "os-cb8-e7-2 2024-11-11_22.41 E7.nitc")
  testthat::expect_true(file.exists(src))

  out <- convert_ta_to_xlsx(
    source_path = src,
    source_type = "ta_nitc",
    app_dir = proc_dir,
    overwrite = TRUE
  )

  testthat::expect_true(file.exists(out))
  testthat::expect_true(grepl("_nitc_extract\\.xlsx$", out))
})

testthat::test_that("read_step1_input parses NITC via xlsx and keeps injection compatibility", {
  testthat::skip_if_not_installed("readxl")

  src <- file.path(repo_root, "Examples", "os-cb8-e7-2 2024-11-11_22.41 E7.nitc")
  testthat::expect_true(file.exists(src))

  td <- file.path(tempdir(), "itcsuite-ta-step1")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  copied <- file.path(td, "sample_no_ext")
  ok <- file.copy(src, copied, overwrite = TRUE)
  testthat::expect_true(isTRUE(ok))

  rd <- read_step1_input(
    file_path = copied,
    display_name = basename(src),
    app_dir = proc_dir,
    overwrite_ta_xlsx = TRUE
  )

  out_xlsx <- file.path(td, "sample_no_ext_nitc_extract.xlsx")
  testthat::expect_true(file.exists(out_xlsx))
  testthat::expect_true(nrow(rd$data) > 100)
  testthat::expect_equal(rd$injections[[1]], 1L)
  testthat::expect_equal(rd$injection_times[[1]], rd$data$Time[[1]])
  testthat::expect_equal(length(rd$injections), rd$params$n_injections + 1L)

  testthat::expect_equal(rd$params$cell_volume_mL, 0.17, tolerance = 1e-8)
  testthat::expect_equal(rd$params$titration_interval_s, 200, tolerance = 1e-8)
  testthat::expect_equal(rd$params$V_pre_ul, 2.5, tolerance = 1e-8)
  testthat::expect_equal(rd$params$V_inj_ul, 2.5, tolerance = 1e-8)

  base <- SegmentedBaseline(
    rd$data$Time,
    rd$data$Power,
    rd$injections,
    injection_times = rd$injection_times,
    baseline_duration = 20,
    baseline_offset = 5,
    spar = 0.1
  )
  int_res <- integrate_peaks(
    rd$data$Time,
    rd$data$Power - base,
    rd$injections,
    integration_window = NULL,
    start_offset = -3
  )
  int_res <- int_res[int_res$Injection > 0, ]
  testthat::expect_equal(nrow(int_res), rd$params$n_injections)
})

testthat::test_that("read_step1_input supports CSC and XML TA sources", {
  testthat::skip_if_not_installed("readxl")

  cases <- list(
    list(file = file.path(repo_root, "Examples", "aatoCB8-g1.csc"), type = "ta_csc"),
    list(file = file.path(repo_root, "Examples", "2-1bd.xml"), type = "ta_xml")
  )

  td <- file.path(tempdir(), "itcsuite-ta-step1-multi")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)

  for (case in cases) {
    src <- case$file
    testthat::expect_true(file.exists(src))
    copied <- file.path(td, basename(src))
    ok <- file.copy(src, copied, overwrite = TRUE)
    testthat::expect_true(isTRUE(ok))

    rd <- read_step1_input(
      file_path = copied,
      display_name = basename(src),
      app_dir = proc_dir,
      overwrite_ta_xlsx = TRUE
    )

    testthat::expect_true(nrow(rd$data) > 0)
    testthat::expect_true(length(rd$injections) >= 2)
    testthat::expect_true(is.finite(rd$params$temperature_C))
    testthat::expect_true(is.finite(rd$params$syringe_conc_mM))
    testthat::expect_true(is.finite(rd$params$cell_conc_mM))
    testthat::expect_true(is.finite(rd$params$cell_volume_mL))
    testthat::expect_equal(rd$params$n_injections, 20L)
  }
})

testthat::test_that("first real injection timing is aligned across NITC/CSC/XML examples", {
  testthat::skip_if_not_installed("readxl")

  files <- c(
    file.path(repo_root, "Examples", "os-cb8-e7-2 2024-11-11_22.41 E7.nitc"),
    file.path(repo_root, "Examples", "aatoCB8-g1.csc"),
    file.path(repo_root, "Examples", "2-1bd.xml")
  )
  testthat::expect_true(all(file.exists(files)))

  first_real <- numeric(length(files))
  for (i in seq_along(files)) {
    src <- files[[i]]
    rd <- read_step1_input(
      file_path = src,
      display_name = basename(src),
      app_dir = proc_dir,
      overwrite_ta_xlsx = TRUE
    )
    testthat::expect_true(length(rd$injection_times) >= 2)
    first_real[[i]] <- suppressWarnings(as.numeric(rd$injection_times[[2]]))
  }

  testthat::expect_true(all(is.finite(first_real)))
  testthat::expect_equal(first_real[[1]], first_real[[2]], tolerance = 1e-8)
  testthat::expect_equal(first_real[[1]], first_real[[3]], tolerance = 1e-8)
  testthat::expect_true(first_real[[1]] > 0)
})

testthat::test_that("nitc selector does not hardcode first injection to 300", {
  nitc_script <- file.path(proc_dir, "R", "nitc_to_xlsx.R")
  testthat::expect_true(file.exists(nitc_script))

  lines <- readLines(nitc_script, warn = FALSE)
  lines <- lines[trimws(lines) != "main()"]
  env <- new.env(parent = baseenv())
  eval(parse(text = lines), envir = env)

  mock_start <- seq(50, by = 200, length.out = 8)
  mock_orig_stop <- mock_start + 200
  step200 <- list(
    list(values = mock_start),
    list(values = mock_orig_stop)
  )

  sel <- env$select_start_stop(step200 = step200, mono_int = NULL, arith = NULL)
  testthat::expect_equal(sel$start, mock_start, tolerance = 1e-8)
  testthat::expect_equal(sel$original_stop, mock_orig_stop, tolerance = 1e-8)
})
