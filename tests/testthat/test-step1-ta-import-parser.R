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

testthat::test_that("convert_ta_to_xlsx prefers ITCSUITE_RSCRIPT when valid", {
  rscript_bin <- unname(Sys.which("Rscript"))[[1L]]
  testthat::skip_if(!nzchar(rscript_bin), "Rscript executable not found in PATH.")
  rscript_bin <- normalizePath(rscript_bin, winslash = "/", mustWork = FALSE)
  testthat::expect_true(file.exists(rscript_bin))

  old_val <- Sys.getenv("ITCSUITE_RSCRIPT", unset = NA_character_)
  on.exit({
    if (is.na(old_val)) {
      Sys.unsetenv("ITCSUITE_RSCRIPT")
    } else {
      Sys.setenv(ITCSUITE_RSCRIPT = old_val)
    }
  }, add = TRUE)

  Sys.setenv(ITCSUITE_RSCRIPT = rscript_bin)
  resolved <- resolve_ta_rscript_bin()
  testthat::expect_identical(resolved, rscript_bin)

  src <- file.path(repo_root, "Examples", "2-1bd.xml")
  testthat::expect_true(file.exists(src))
  out <- convert_ta_to_xlsx(
    source_path = src,
    source_type = "ta_xml",
    app_dir = proc_dir,
    overwrite = TRUE
  )
  testthat::expect_true(file.exists(out))
  testthat::expect_true(grepl("_xml_extract\\.xlsx$", out))
})

testthat::test_that("convert_ta_to_xlsx falls back when ITCSUITE_RSCRIPT is invalid", {
  missing_bin <- file.path(tempdir(), sprintf("missing-rscript-%s", as.integer(Sys.time())))
  old_val <- Sys.getenv("ITCSUITE_RSCRIPT", unset = NA_character_)
  on.exit({
    if (is.na(old_val)) {
      Sys.unsetenv("ITCSUITE_RSCRIPT")
    } else {
      Sys.setenv(ITCSUITE_RSCRIPT = old_val)
    }
  }, add = TRUE)

  Sys.setenv(ITCSUITE_RSCRIPT = missing_bin)
  resolved <- resolve_ta_rscript_bin()
  testthat::expect_false(identical(resolved, missing_bin))
  testthat::expect_true(nzchar(resolved))
  testthat::expect_true(file.exists(resolved))

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
    if (identical(case$type, "ta_xml")) {
      testthat::expect_true(is.finite(rd$params$temperature_C))
    }
    testthat::expect_true(is.finite(rd$params$syringe_conc_mM))
    testthat::expect_true(is.finite(rd$params$cell_conc_mM))
    testthat::expect_true(is.finite(rd$params$cell_volume_mL))
    testthat::expect_equal(rd$params$n_injections, length(rd$injections) - 1L)
  }
})

testthat::test_that("read_step1_input parses bdtocb.csc with >20 injections and finite core params", {
  testthat::skip_if_not_installed("readxl")

  src <- file.path(repo_root, "Examples", "bdtocb.csc")
  testthat::expect_true(file.exists(src))

  rd <- read_step1_input(
    file_path = src,
    display_name = basename(src),
    app_dir = proc_dir,
    overwrite_ta_xlsx = TRUE
  )

  testthat::expect_true(nrow(rd$data) >= 8000)
  testthat::expect_true(rd$params$n_injections > 20L)
  testthat::expect_equal(rd$params$n_injections, length(rd$injections) - 1L)

  testthat::expect_true(is.finite(rd$params$syringe_conc_mM))
  testthat::expect_true(is.finite(rd$params$cell_conc_mM))
  testthat::expect_true(is.finite(rd$params$cell_volume_mL))
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

testthat::test_that("xml converter keeps all titration rows when row count is not 20", {
  testthat::skip_if_not_installed("readxl")

  td <- file.path(tempdir(), "itcsuite-ta-xml-non20")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  src <- file.path(td, "synthetic_non20.xml")

  n_rows <- 25L
  kv <- c(
    "<MockKVP><Key>irTable</Key><Value>IR</Value></MockKVP>",
    "<MockKVP><Key>injVolTable</Key><Value>INJ</Value></MockKVP>",
    "<MockKVP><Key>areaTable</Key><Value>AREA</Value></MockKVP>",
    "<MockKVP><Key>originalIRTable</Key><Value>OIR</Value></MockKVP>",
    "<MockKVP><Key>baselineTable</Key><Value>BASE</Value></MockKVP>",
    "<MockKVP><Key>rgraphTable</Key><Value>RGRAPH</Value></MockKVP>",
    "<MockKVP><Key>Titrant</Key><Value>0.5</Value></MockKVP>",
    "<MockKVP><Key>Titrate</Key><Value>0.0375</Value></MockKVP>",
    "<MockKVP><Key>InitalTitrateVolume</Key><Value>170</Value></MockKVP>",
    "<MockKVP><Key>Temperature</Key><Value>25</Value></MockKVP>"
  )
  ir <- vapply(
    seq_len(n_rows),
    function(i) sprintf("<IR><Start>%d</Start><Stop>%d</Stop></IR>", 100 + (i - 1L) * 200L, 100 + i * 200L),
    character(1)
  )
  oir <- vapply(
    seq_len(n_rows),
    function(i) sprintf("<OIR><Start>%d</Start><Stop>%d</Stop></OIR>", 100 + (i - 1L) * 200L, 100 + i * 200L + 9L),
    character(1)
  )
  inj <- vapply(
    seq_len(n_rows),
    function(i) sprintf("<INJ><InjNum>%d</InjNum><InjVolume>2.5</InjVolume></INJ>", i),
    character(1)
  )
  area <- vapply(
    seq_len(n_rows),
    function(i) sprintf(
      paste0(
        "<AREA>",
        "<Region>%d</Region>",
        "<inj_x0020_volume_x0020__x0028__x00B5_L_x0029_>2.5</inj_x0020_volume_x0020__x0028__x00B5_L_x0029_>",
        "<Q_x0020__x0028__x00B5_J_x0029_>%0.6f</Q_x0020__x0028__x00B5_J_x0029_>",
        "<Corrected_x0020_Q_x0020__x0028__x00B5_J_x0029_>%0.6f</Corrected_x0020_Q_x0020__x0028__x00B5_J_x0029_>",
        "<moles_x0020_titrant_x0020__x002F__x0020_moles_x0020_titrate>%0.6f</moles_x0020_titrant_x0020__x002F__x0020_moles_x0020_titrate>",
        "<moles_x0020_titrant_x0020__x0028_moles_x0029_>%0.6f</moles_x0020_titrant_x0020__x0028_moles_x0029_>",
        "<moles_x0020_titrate_x0020__x0028_moles_x0029_>%0.6f</moles_x0020_titrate_x0020__x0028_moles_x0029_>",
        "<total_x0020_volume_x0020__x0028__x00B5_L_x0029_>%0.6f</total_x0020_volume_x0020__x0028__x00B5_L_x0029_>",
        "</AREA>"
      ),
      i,
      as.numeric(i) * 10,
      as.numeric(i) * 9.5,
      as.numeric(i) * 0.05,
      as.numeric(i) * 0.001,
      as.numeric(i) * 0.001,
      170 + as.numeric(i) * 2.5
    ),
    character(1)
  )

  xml_lines <- c(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
    "<root>",
    kv,
    ir,
    oir,
    inj,
    area,
    "</root>"
  )
  writeLines(xml_lines, src, useBytes = TRUE)

  out <- convert_ta_to_xlsx(
    source_path = src,
    source_type = "ta_xml",
    app_dir = proc_dir,
    overwrite = TRUE
  )
  testthat::expect_true(file.exists(out))

  tp <- as.data.frame(readxl::read_excel(out, sheet = "titration_points"), stringsAsFactors = FALSE)
  testthat::expect_equal(nrow(tp), n_rows)
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
