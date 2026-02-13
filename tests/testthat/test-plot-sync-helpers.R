repo_root <- itcsuite_repo_root()
source(file.path(repo_root, "ITCgraph", "R", "bridge_plot_helpers.R"))

testthat::test_that("sync_from_fit_params maps Offset_cal, fH, fG", {
  fit_params <- data.frame(
    parameter = c("Offset_cal", "fH", "fG"),
    value = c("12.5", "0.8", "1.2"),
    stringsAsFactors = FALSE
  )
  sync <- bridge_plot_sync_from_fit_params(fit_params, default_heat_offset = 0)
  testthat::expect_equal(sync$heat_offset, 12.5)
  testthat::expect_equal(sync$ratio_fh, 0.8)
  testthat::expect_equal(sync$ratio_fg, 1.2)
})

testthat::test_that("sync_from_fit_params falls back to defaults when map missing", {
  sync <- bridge_plot_sync_from_fit_params(NULL, default_heat_offset = 5)
  testthat::expect_equal(sync$heat_offset, 5)
  testthat::expect_equal(sync$ratio_fh, 1)
  testthat::expect_equal(sync$ratio_fg, 1)
})

testthat::test_that("ratio multiplier follows correction switch", {
  testthat::expect_equal(
    bridge_plot_ratio_multiplier(ratio_fh = 0.8, ratio_fg = 1.2, apply_ratio = TRUE),
    1.5
  )
  testthat::expect_equal(
    bridge_plot_ratio_multiplier(ratio_fh = 0.8, ratio_fg = 1.2, apply_ratio = FALSE),
    1
  )
})

testthat::test_that("heat correction applies offset-aware inverse fG when enabled", {
  y_raw <- c(1000, 2500, -300)
  heat_offset <- 100
  corrected <- bridge_plot_apply_heat_correction(
    y_raw = y_raw,
    ratio_fg = 2,
    heat_offset = heat_offset,
    apply_ratio = TRUE
  )
  expected <- ((y_raw - heat_offset) / 2) + heat_offset
  testthat::expect_equal(corrected, expected)

  to_kcal <- function(v) (v - heat_offset) / 1000
  testthat::expect_equal(to_kcal(corrected), ((y_raw - heat_offset) / 2) / 1000)
})

testthat::test_that("heat correction keeps raw value when disabled", {
  y_raw <- c(1000, 2500, -300)
  corrected <- bridge_plot_apply_heat_correction(
    y_raw = y_raw,
    ratio_fg = 2,
    heat_offset = 100,
    apply_ratio = FALSE
  )
  testthat::expect_equal(corrected, y_raw)
})

testthat::test_that("heat correction falls back to fG=1 for invalid factor", {
  y_raw <- c(1000, 2500, -300)
  corrected <- bridge_plot_apply_heat_correction(
    y_raw = y_raw,
    ratio_fg = 0,
    heat_offset = 100,
    apply_ratio = TRUE
  )
  testthat::expect_equal(corrected, y_raw)
})

testthat::test_that("heat correction remains unchanged when fG equals 1", {
  y_raw <- c(1000, 2500, -300)
  corrected_on <- bridge_plot_apply_heat_correction(
    y_raw = y_raw,
    ratio_fg = 1,
    heat_offset = 100,
    apply_ratio = TRUE
  )
  corrected_off <- bridge_plot_apply_heat_correction(
    y_raw = y_raw,
    ratio_fg = 1,
    heat_offset = 100,
    apply_ratio = FALSE
  )
  testthat::expect_equal(corrected_on, y_raw)
  testthat::expect_equal(corrected_off, y_raw)
})

testthat::test_that("resolve_step2_payload_source keeps source label rules", {
  source_file <- bridge_plot_resolve_step2_payload_source(
    list(source = "file", source_label = "fit.xlsx"),
    token = 3
  )
  testthat::expect_equal(source_file, "fit.xlsx")

  source_sim <- bridge_plot_resolve_step2_payload_source(
    list(source = "sim_to_exp", source_label = ""),
    token = 3
  )
  testthat::expect_equal(source_sim, "Simulation to experiment")

  source_bridge <- bridge_plot_resolve_step2_payload_source(
    list(source = "bridge", source_label = ""),
    token = 3
  )
  testthat::expect_equal(source_bridge, "Step2 bridge @ 3")
})

testthat::test_that("extract_step2_payload_frames prefers integration_rev and meta_rev", {
  payload <- list(
    sheets = list(
      integration = data.frame(Injection = 1, Ratio_App = 0.1),
      integration_rev = data.frame(Injection = 1, Ratio_App = 0.2),
      meta = data.frame(parameter = "A", value = "1"),
      meta_rev = data.frame(parameter = "A", value = "2")
    )
  )
  parsed <- bridge_plot_extract_step2_payload_frames(payload)
  testthat::expect_true(is.data.frame(parsed$integration))
  testthat::expect_equal(parsed$integration$Ratio_App[1], 0.2)
  testthat::expect_equal(parsed$meta$value[1], "2")
})
