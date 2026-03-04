#!/usr/bin/env Rscript

require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      sprintf("Missing package '%s'. Install with: install.packages('%s')", pkg, pkg),
      call. = FALSE
    )
  }
}

require_pkg("writexl")

suppressPackageStartupMessages({
  library(writexl)
})

TOKEN_DATE_RE <- "\\d{4}/\\d{2}/\\d{2}.*[A-Z]{8,}"

parse_cli <- function(argv) {
  if (length(argv) == 0) {
    stop("Usage: Rscript csc_to_xlsx.R <input.csc> [-o <out.xlsx>]", call. = FALSE)
  }
  input <- NULL
  out_xlsx <- NULL
  i <- 1L
  while (i <= length(argv)) {
    a <- argv[[i]]
    if (a %in% c("-o", "--out")) {
      if (i == length(argv)) {
        stop("Missing value after -o/--out", call. = FALSE)
      }
      out_xlsx <- argv[[i + 1L]]
      i <- i + 2L
      next
    }
    if (startsWith(a, "-")) {
      stop(sprintf("Unknown option: %s", a), call. = FALSE)
    }
    if (!is.null(input)) {
      stop("Only one input CSC file is supported", call. = FALSE)
    }
    input <- a
    i <- i + 1L
  }
  if (is.null(input)) {
    stop("Input CSC path is required", call. = FALSE)
  }
  list(input = input, out_xlsx = out_xlsx)
}

read_blob <- function(path) {
  size <- file.info(path)$size
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  readBin(con, what = "raw", n = size)
}

load_payload <- function(csc_path) {
  raw <- read_blob(csc_path)
  dec <- tryCatch(memDecompress(raw, type = "gzip"), error = function(e) NULL)
  if (is.null(dec)) raw else dec
}

read_doubles_by_phase <- function(payload, phase) {
  start <- phase + 1L
  available <- length(payload) - start + 1L
  count <- available %/% 8L
  if (count <= 0L) {
    return(numeric())
  }
  bytes <- payload[start:(start + count * 8L - 1L)]
  con <- rawConnection(bytes, open = "rb")
  on.exit(close(con), add = TRUE)
  readBin(con, what = "double", n = count, size = 8L, endian = "little")
}

is_intish <- function(v, tol = 1e-9) {
  is.finite(v) && abs(v - round(v)) < tol
}

scan_numeric_series <- function(payload, min_len = 500L) {
  series <- list()
  for (phase in 0:7) {
    values <- read_doubles_by_phase(payload, phase)
    if (length(values) == 0) {
      next
    }
    run_start <- NA_integer_
    flush_run <- function(end_idx) {
      if (is.na(run_start) || end_idx < run_start) {
        return()
      }
      run_vals <- values[run_start:end_idx]
      if (length(run_vals) < min_len) {
        return()
      }
      head_vals <- run_vals[seq_len(min(2000L, length(run_vals)))]
      uniq <- length(unique(round(head_vals, 6)))
      if (uniq <= 10) {
        return()
      }
      series[[length(series) + 1L]] <<- list(
        phase = phase,
        start = phase + (run_start - 1L) * 8L,
        len = length(run_vals),
        values = run_vals
      )
    }

    for (i in seq_along(values)) {
      v <- values[[i]]
      plausible <- is.finite(v) && v >= -1e5 && v <= 1e5 && (v == 0 || abs(v) >= 1e-6)
      if (plausible) {
        if (is.na(run_start)) {
          run_start <- i
        }
      } else {
        flush_run(i - 1L)
        run_start <- NA_integer_
      }
    }
    if (!is.na(run_start)) {
      flush_run(length(values))
    }
  }
  series
}

is_index_axis <- function(values) {
  if (length(values) < 100L) {
    return(FALSE)
  }
  check <- values[seq_len(min(length(values), 4500L))]
  diffs <- diff(check)
  finite <- is.finite(check[-1]) & is.finite(check[-length(check)])
  if (!any(finite)) {
    return(FALSE)
  }
  good <- sum(abs(diffs[finite] - 1.0) < 1e-9)
  total <- sum(finite)
  starts_near_zero <- abs(check[[1]]) < 2.0
  starts_near_zero && (good / total) > 0.995
}

extract_heatflow_df <- function(payload) {
  series <- scan_numeric_series(payload, min_len = 500L)
  if (length(series) == 0) {
    return(data.frame(
      "Time (seconds)" = numeric(),
      "Raw Heat Rate (µJ / s)" = numeric(),
      "Raw Heat Rate (µcal / s)" = numeric(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  axis_idx <- which(vapply(series, function(s) is_index_axis(s$values), logical(1)))
  if (length(axis_idx) == 0) {
    return(data.frame(
      "Time (seconds)" = numeric(),
      "Raw Heat Rate (µJ / s)" = numeric(),
      "Raw Heat Rate (µcal / s)" = numeric(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }
  axis <- series[[axis_idx[[1]]]]
  peers_idx <- which(vapply(series, function(s) s$len == axis$len, logical(1)))
  peers_idx <- setdiff(peers_idx, axis_idx[[1]])
  if (length(peers_idx) == 0) {
    return(data.frame(
      "Time (seconds)" = axis$values,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  peers <- lapply(peers_idx, function(i) series[[i]])
  # Stable ordering: higher variance first is usually µJ/s, lower is µcal/s.
  ord <- order(vapply(peers, function(s) sd(s$values, na.rm = TRUE), numeric(1)), decreasing = TRUE)
  peers <- peers[ord]

  if (length(peers) >= 2L) {
    a <- peers[[1]]$values
    b <- peers[[2]]$values
    ok <- is.finite(a) & is.finite(b) & abs(b) > 1e-12
    if (any(ok)) {
      ratio <- median(a[ok] / b[ok], na.rm = TRUE)
      if (is.finite(ratio) && abs(ratio) < 1) {
        tmp <- peers[[1]]
        peers[[1]] <- peers[[2]]
        peers[[2]] <- tmp
      }
    }
  }

  out <- data.frame(
    "Time (seconds)" = axis$values,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out[["Raw Heat Rate (µJ / s)"]] <- peers[[1]]$values
  if (length(peers) >= 2L) {
    out[["Raw Heat Rate (µcal / s)"]] <- peers[[2]]$values
  } else {
    out[["Raw Heat Rate (µcal / s)"]] <- NA_real_
  }
  out
}

dedup_seq_by_key <- function(seq_list, key_fn) {
  if (length(seq_list) == 0) {
    return(seq_list)
  }
  ord <- order(vapply(seq_list, function(x) x$start, numeric(1)))
  seq_list <- seq_list[ord]
  seen <- character()
  out <- list()
  for (item in seq_list) {
    key <- key_fn(item)
    if (key %in% seen) {
      next
    }
    seen <- c(seen, key)
    out[[length(out) + 1L]] <- item
  }
  out
}

find_step200_sequences <- function(data) {
  out <- list()
  n <- 20L
  for (phase in 0:7) {
    vals <- read_doubles_by_phase(data, phase)
    if (length(vals) < n) {
      next
    }
    for (start_idx in seq_len(length(vals) - n + 1L)) {
      s <- vals[start_idx:(start_idx + n - 1L)]
      if (!all(is.finite(s))) {
        next
      }
      if (!all(vapply(s, is_intish, logical(1)))) {
        next
      }
      d <- diff(s)
      if (!all(abs(d - 200.0) < 1e-9)) {
        next
      }
      if (s[[1]] < 0 || s[[n]] > 10000) {
        next
      }
      out[[length(out) + 1L]] <- list(
        phase = phase,
        start = phase + (start_idx - 1L) * 8L,
        values = s
      )
    }
  }
  dedup_seq_by_key(out, function(x) paste(format(round(x$values, 9), scientific = FALSE, trim = TRUE), collapse = ","))
}

find_monotonic_int_sequences <- function(data, n = 20L) {
  out <- list()
  for (phase in 0:7) {
    vals <- read_doubles_by_phase(data, phase)
    if (length(vals) < n) {
      next
    }
    for (start_idx in seq_len(length(vals) - n + 1L)) {
      s <- vals[start_idx:(start_idx + n - 1L)]
      if (!all(is.finite(s))) {
        next
      }
      if (!all(vapply(s, is_intish, logical(1)))) {
        next
      }
      iv <- as.numeric(round(s))
      if (iv[[1]] < 0 || iv[[n]] > 10000) {
        next
      }
      d <- diff(iv)
      if (min(d) < 0) {
        next
      }
      if (sum(d > 0) < (n - 2L)) {
        next
      }
      out[[length(out) + 1L]] <- list(
        phase = phase,
        start = phase + (start_idx - 1L) * 8L,
        values = iv
      )
    }
  }
  dedup_seq_by_key(out, function(x) paste(x$values, collapse = ","))
}

find_constant_sequences <- function(data, target, n = 20L) {
  out <- list()
  for (phase in 0:7) {
    vals <- read_doubles_by_phase(data, phase)
    if (length(vals) < n) {
      next
    }
    for (start_idx in seq_len(length(vals) - n + 1L)) {
      s <- vals[start_idx:(start_idx + n - 1L)]
      if (!all(is.finite(s))) {
        next
      }
      if (all(abs(s - target) < 1e-12)) {
        out[[length(out) + 1L]] <- list(
          phase = phase,
          start = phase + (start_idx - 1L) * 8L,
          values = s
        )
      }
    }
  }
  dedup_seq_by_key(out, function(x) paste(format(round(x$values, 12), scientific = FALSE, trim = TRUE), collapse = ","))
}

derive_step_sequence_pair <- function(step_sequences) {
  if (is.null(step_sequences) || length(step_sequences) == 0) {
    return(list(start = NULL, original_stop = NULL, source = "none"))
  }

  seq_vals <- lapply(step_sequences, function(x) as.numeric(x$values))
  seq_vals <- seq_vals[vapply(seq_vals, function(v) length(v) > 0L && all(is.finite(v)), logical(1))]
  if (length(seq_vals) == 0) {
    return(list(start = NULL, original_stop = NULL, source = "none"))
  }

  best <- NULL
  best_score <- -Inf
  for (i in seq_along(seq_vals)) {
    for (j in seq_along(seq_vals)) {
      if (j <= i) next
      a <- seq_vals[[i]]
      b <- seq_vals[[j]]
      if (length(a) != length(b)) next
      if (all(a > b)) {
        tmp <- a
        a <- b
        b <- tmp
      }
      if (!all(b > a)) next

      widths <- b - a
      in_range <- widths >= 20 & widths <= 400
      score <- sum(in_range)
      if (score < ceiling(length(widths) * 0.6)) next
      if (stats::sd(widths) < 1e-9) score <- score + 4
      score <- score + length(a) * 0.5

      better <- FALSE
      if (score > best_score) {
        better <- TRUE
      } else if (!is.null(best) && abs(score - best_score) < 1e-9) {
        if (a[[1]] < best$start[[1]]) {
          better <- TRUE
        } else if (a[[1]] == best$start[[1]] && length(a) > length(best$start)) {
          better <- TRUE
        }
      }
      if (better) {
        best_score <- score
        best <- list(start = a, original_stop = b, source = "step_pair")
      }
    }
  }

  if (!is.null(best)) return(best)

  firsts <- vapply(seq_vals, function(v) v[[1]], numeric(1))
  lens <- vapply(seq_vals, length, integer(1))
  ord <- order(firsts, -lens)
  start <- seq_vals[[ord[[1]]]]
  step_guess <- suppressWarnings(stats::median(diff(start)))
  if (!is.finite(step_guess) || step_guess <= 0) step_guess <- 200
  list(
    start = start,
    original_stop = start + step_guess,
    source = "step_derived"
  )
}

select_start_stop <- function(step200, mono_int) {
  step_pick <- derive_step_sequence_pair(step200)
  start <- step_pick$start
  original_stop <- step_pick$original_stop
  if (is.null(start)) {
    return(list(start = NULL, stop = NULL, original_stop = NULL, source = "none"))
  }

  best_stop <- NULL
  best_score <- -1L
  for (cand_item in mono_int) {
    cand <- cand_item$values
    if (length(cand) != length(start)) {
      next
    }
    if (all(cand == start)) {
      next
    }
    widths <- cand - start
    ok <- widths[widths >= 20 & widths <= 400]
    score <- length(ok)
    if (length(unique(widths)) > 5) {
      score <- score + 2L
    }
    if (score > best_score) {
      best_score <- score
      best_stop <- cand
    }
  }
  if (!is.null(best_stop) && best_score >= 12) {
    orig <- if (!is.null(original_stop)) original_stop else (start + 200.0)
    return(list(start = start, stop = best_stop, original_stop = orig, source = "monotonic_int_match"))
  }

  if (!is.null(original_stop)) {
    return(list(start = start, stop = original_stop, original_stop = original_stop, source = "step200_fallback"))
  }
  derived <- start + 200.0
  list(start = start, stop = derived, original_stop = derived, source = "derived_plus_200")
}

extract_ascii_tokens <- function(data, min_len = 3L) {
  ints <- as.integer(data)
  tokens <- character()
  run_start <- NA_integer_
  flush_run <- function(end_idx) {
    if (is.na(run_start)) {
      return()
    }
    run_len <- end_idx - run_start + 1L
    if (run_len >= min_len) {
      txt <- rawToChar(as.raw(ints[run_start:end_idx]))
      txt <- trimws(txt)
      if (!identical(txt, "")) {
        tokens <<- c(tokens, txt)
      }
    }
  }
  for (i in seq_along(ints)) {
    ok <- ints[[i]] >= 32L && ints[[i]] <= 126L
    if (ok) {
      if (is.na(run_start)) {
        run_start <- i
      }
    } else {
      flush_run(i - 1L)
      run_start <- NA_integer_
    }
  }
  if (!is.na(run_start)) {
    flush_run(length(ints))
  }
  tokens
}

extract_ascii_runs <- function(data, min_len = 3L) {
  ints <- as.integer(data)
  out <- list()
  run_start <- NA_integer_
  flush_run <- function(end_idx) {
    if (is.na(run_start)) return()
    run_len <- end_idx - run_start + 1L
    if (run_len >= min_len) {
      txt <- rawToChar(as.raw(ints[run_start:end_idx]))
      txt <- trimws(txt)
      if (!identical(txt, "")) {
        out[[length(out) + 1L]] <<- list(start = run_start, end = end_idx, text = txt)
      }
    }
  }
  for (i in seq_along(ints)) {
    ok <- ints[[i]] >= 32L && ints[[i]] <= 126L
    if (ok) {
      if (is.na(run_start)) run_start <- i
    } else {
      flush_run(i - 1L)
      run_start <- NA_integer_
    }
  }
  if (!is.na(run_start)) flush_run(length(ints))
  out
}

extract_parameter_tokens_dense <- function(payload) {
  coarse <- extract_ascii_runs(payload, min_len = 3L)
  if (length(coarse) == 0) return(character())
  coarse_text <- vapply(coarse, function(x) x$text, character(1))
  root_candidates <- which(coarse_text == "root")
  idx_root <- NA_integer_
  if (length(root_candidates) > 0) {
    for (i in root_candidates) {
      j1 <- i + 1L
      j2 <- min(i + 25L, length(coarse_text))
      if (j1 <= j2 && any(coarse_text[j1:j2] == "units")) {
        idx_root <- i
        break
      }
    }
    if (is.na(idx_root)) idx_root <- root_candidates[[1]]
  }
  idx_end_candidates <- which(coarse_text == "2024/10/87:46:01ZOHTRCGVDC" & seq_along(coarse_text) > idx_root)
  idx_end <- if (length(idx_end_candidates) > 0) idx_end_candidates[[1]] else NA_integer_
  if (is.na(idx_root) || is.na(idx_end)) return(character())

  start <- max(1L, coarse[[idx_root]]$start - 40L)
  end <- min(length(payload), coarse[[idx_end]]$end + 40L)
  seg <- payload[start:end]
  dense <- extract_ascii_runs(seg, min_len = 1L)
  vapply(dense, function(x) x$text, character(1))
}

extract_experiment_parameters <- function(payload) {
  runs <- extract_parameter_tokens_dense(payload)
  if (length(runs) == 0) return(data.frame())
  i_report <- match("ReportViews", runs)
  if (is.na(i_report)) return(data.frame())

  values <- runs[(i_report + 1L):length(runs)]
  # Trim framing noise from serialized stream.
  while (length(values) > 0 && values[[1]] %in% c("\"", "0")) {
    values <- values[-1L]
  }
  h4_idx <- which(startsWith(values, "H4sIA"))
  if (length(h4_idx) > 0) {
    values <- values[seq_len(h4_idx[[1]] - 1L)]
  }

  rows <- list()
  add_row <- function(key, value, confidence = "high", note = "") {
    rows[[length(rows) + 1L]] <<- list(
      key = key,
      value = value,
      confidence = confidence,
      note = note
    )
  }

  ptr <- 1L
  if (ptr <= length(values)) {
    add_row("units", values[[ptr]], "high")
    ptr <- ptr + 1L
  }
  if (ptr <= length(values) && grepl("^-?\\d+$", values[[ptr]], perl = TRUE)) {
    add_row("calUnitsColumnIndex", values[[ptr]], "high")
    ptr <- ptr + 1L
  }
  if (ptr <= length(values)) {
    add_row("XColumnName", values[[ptr]], "high")
    ptr <- ptr + 1L
  }
  if ((ptr + 1L) <= length(values)) {
    add_row("YColumnName", paste0(values[[ptr]], values[[ptr + 1L]]), "high", "Reconstructed from split tokens")
    ptr <- ptr + 2L
  }

  table_keys <- c("areaTable", "baselineTable", "irTable", "originalIRTable", "injVolTable")
  for (k in table_keys) {
    if (ptr <= length(values) && grepl(TOKEN_DATE_RE, values[[ptr]], perl = TRUE)) {
      add_row(k, values[[ptr]], "high")
      ptr <- ptr + 1L
    } else {
      add_row(k, "", "low", "Not found in expected order")
    }
  }

  ordered_keys <- c(
    "IsUnfilteredData",
    "Titrant",
    "Titrate",
    "InitalTitrateVolume",
    "Temperature",
    "HeatRateOffset",
    "starttime",
    "equilseconds",
    "integrationRegionStart",
    "integrationRegionWidth",
    "integrationRegionInterval",
    "numRegions",
    "AutoCreateBaseline",
    "UserName",
    "StirRate",
    "Comments",
    "ExothermUp",
    "ContinousTitration",
    "FirstInjStartTime",
    "analysisType",
    "PartiallyFilledCell",
    "DefaultInjVolume",
    "TotalContInjVolume",
    "Version",
    "ConstantSubtraction",
    "BlankSubtractType"
  )

  validators <- list(
    IsUnfilteredData = "^(true|false)$",
    Titrant = "^-?\\d+(?:\\.\\d+)?$",
    Titrate = "^-?\\d+(?:\\.\\d+)?$",
    InitalTitrateVolume = "^-?\\d+(?:\\.\\d+)?$",
    Temperature = "^-?\\d+(?:\\.\\d+)?$",
    HeatRateOffset = "^-?\\d+(?:\\.\\d+)?$",
    starttime = "^\\d{10,20}$",
    equilseconds = "^-?\\d+$",
    integrationRegionStart = "^-?\\d+$",
    integrationRegionWidth = "^-?\\d+$",
    integrationRegionInterval = "^-?\\d+$",
    numRegions = "^-?\\d+$",
    AutoCreateBaseline = "^(true|false)$",
    UserName = "^[A-Za-z0-9_.-]{2,}$",
    StirRate = "^-?\\d+(?:\\.\\d+)?$",
    Comments = ".*",
    ExothermUp = "^(true|false)$",
    ContinousTitration = "^(true|false)$",
    FirstInjStartTime = "^-?\\d+(?:\\.\\d+)?$",
    analysisType = "^[A-Za-z][A-Za-z0-9_-]*$",
    PartiallyFilledCell = "^(true|false)$",
    DefaultInjVolume = "^-?\\d+(?:\\.\\d+)?$",
    TotalContInjVolume = "^-?\\d+(?:\\.\\d+)?$",
    Version = "^-?\\d+(?:\\.\\d+)?$",
    ConstantSubtraction = "^-?\\d+(?:\\.\\d+)?$",
    BlankSubtractType = "^[A-Za-z][A-Za-z0-9_-]*$"
  )

  for (k in ordered_keys) {
    if (ptr > length(values)) {
      add_row(k, "", "low", "No remaining token")
      next
    }
    tok <- values[[ptr]]
    if (k == "Comments" &&
      (grepl("^(true|false)$", tok, ignore.case = TRUE, perl = TRUE) ||
        grepl("^-?\\d+(?:\\.\\d+)?$", tok, perl = TRUE))) {
      add_row(k, "", "medium", "Likely empty in source")
      next
    }
    if (k == "Temperature") {
      if (!(grepl("^-?\\d+(?:\\.\\d+)?$", tok, perl = TRUE) && as.numeric(tok) >= -5 && as.numeric(tok) <= 120)) {
        add_row(k, "", "low", "Missing or not in expected range")
        next
      }
    }
    if (k == "numRegions" && !grepl("^-?\\d+$", tok, perl = TRUE)) {
      add_row(k, "", "low", "Missing integer token")
      next
    }
    pat <- validators[[k]]
    if (grepl(pat, tok, perl = TRUE, ignore.case = grepl("true|false", pat))) {
      add_row(k, tok, "high")
      ptr <- ptr + 1L
    } else {
      add_row(k, "", "low", "Token did not match expected type")
    }
  }

  if (ptr <= length(values) && grepl("^(true|false)$", values[[ptr]], ignore.case = TRUE, perl = TRUE)) {
    add_row("bUseDefaultInjVolume", values[[ptr]], "high")
  } else {
    add_row("bUseDefaultInjVolume", "", "low", "Not available in parsed value stream")
  }

  tail_date <- ""
  for (v in values) {
    if (grepl(TOKEN_DATE_RE, v, perl = TRUE) && grepl("ZOHTRCGVDC$", v, perl = TRUE)) {
      tail_date <- v
      break
    }
  }
  add_row("rgraphTable", tail_date, if (nchar(tail_date) > 0) "medium" else "low")

  data.frame(
    key = vapply(rows, function(x) x$key, character(1)),
    value = vapply(rows, function(x) x$value, character(1)),
    confidence = vapply(rows, function(x) x$confidence, character(1)),
    note = vapply(rows, function(x) x$note, character(1)),
    stringsAsFactors = FALSE
  )
}

add_input_filename_row <- function(params_df, input_filename) {
  if (is.null(params_df) || nrow(params_df) == 0) {
    params_df <- data.frame(
      key = character(),
      value = character(),
      confidence = character(),
      note = character(),
      stringsAsFactors = FALSE
    )
  }
  row <- data.frame(
    key = "InputFileName",
    value = input_filename,
    confidence = "high",
    note = "Source file name",
    stringsAsFactors = FALSE
  )
  rbind(row, params_df)
}

infer_conditions <- function(tokens) {
  i0 <- match("IsUnfilteredData", tokens)
  if (is.na(i0)) {
    i0 <- 1L
  }
  i1 <- length(tokens)
  for (i in seq(i0, length(tokens))) {
    if (startsWith(tokens[[i]], "H4sIA")) {
      i1 <- i - 1L
      break
    }
  }
  block <- if (i0 <= i1) tokens[i0:i1] else character()

  value_tokens <- character()
  date_count <- 0L
  in_values <- FALSE
  for (t in block) {
    if (grepl(TOKEN_DATE_RE, t, perl = TRUE)) {
      date_count <- date_count + 1L
      next
    }
    if (!in_values && date_count >= 5L) {
      if (
        grepl("^(true|false)$", t, ignore.case = TRUE, perl = TRUE) ||
          grepl("^-?\\d+(?:\\.\\d+)?$", t, perl = TRUE) ||
          t %in% c("Administrator", "Titration", "InjectionByInjection")
      ) {
        in_values <- TRUE
      }
    }
    if (in_values) {
      value_tokens <- c(value_tokens, t)
    }
  }

  inferred <- list()
  first_match <- function(pred) {
    for (t in value_tokens) {
      if (pred(t)) return(t)
    }
    NULL
  }

  heat_offset <- first_match(function(t) grepl("^-\\d+\\.\\d+$", t, perl = TRUE))
  if (!is.null(heat_offset)) inferred$HeatRateOffset <- heat_offset

  ticks <- first_match(function(t) grepl("^\\d{15,20}$", t, perl = TRUE))
  if (!is.null(ticks)) inferred$FirstInjStartTimeTicks <- ticks

  for (k in c("1080", "300", "120", "180", "125", "2.5", "0.0", "0.5", "0.0375", "170")) {
    if (k %in% value_tokens) {
      inferred[[sprintf("value_%s", k)]] <- k
    }
  }
  if ("Administrator" %in% value_tokens) inferred$UserName <- "Administrator"
  if ("Titration" %in% value_tokens) inferred$analysisType <- "Titration"
  if ("InjectionByInjection" %in% value_tokens) inferred$ReportView <- "InjectionByInjection"

  list(
    token_block_start_key = "IsUnfilteredData",
    token_block_size = length(block),
    value_tokens = value_tokens,
    inferred = inferred,
    notes = list("Conditions are inferred from serialized token blocks; short tokens may be missing.")
  )
}

build_injection_df <- function(starts, stops, original_stops, volumes = NULL) {
  n <- min(length(starts), length(stops), length(original_stops), if (is.null(volumes)) .Machine$integer.max else length(volumes))
  if (n <= 0) {
    return(data.frame(
      inj_num = integer(),
      start_s = numeric(),
      stop_s = numeric(),
      width_s = numeric(),
      original_start_s = numeric(),
      original_stop_s = numeric(),
      original_width_s = numeric(),
      inj_volume_uL = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    inj_num = seq_len(n),
    start_s = starts[seq_len(n)],
    stop_s = stops[seq_len(n)],
    width_s = stops[seq_len(n)] - starts[seq_len(n)],
    original_start_s = starts[seq_len(n)],
    original_stop_s = original_stops[seq_len(n)],
    original_width_s = original_stops[seq_len(n)] - starts[seq_len(n)],
    inj_volume_uL = if (is.null(volumes)) NA_real_ else volumes[seq_len(n)],
    stringsAsFactors = FALSE
  )
}

build_conditions_df <- function(cond) {
  rows <- list()
  add_row <- function(section, item, value = NA_character_, index = NA_integer_) {
    rows[[length(rows) + 1L]] <<- list(
      section = section,
      item = as.character(item),
      value = as.character(value),
      index = index
    )
  }

  add_row("meta", "token_block_start_key", cond$token_block_start_key)
  add_row("meta", "token_block_size", cond$token_block_size)

  if (!is.null(cond$value_tokens) && length(cond$value_tokens) > 0) {
    for (i in seq_along(cond$value_tokens)) {
      add_row("value_tokens", "token", cond$value_tokens[[i]], as.integer(i))
    }
  }

  if (!is.null(cond$inferred) && length(cond$inferred) > 0) {
    inf_names <- names(cond$inferred)
    if (is.null(inf_names)) inf_names <- rep("inferred", length(cond$inferred))
    for (i in seq_along(cond$inferred)) {
      add_row("inferred", inf_names[[i]], cond$inferred[[i]])
    }
  }

  if (!is.null(cond$notes) && length(cond$notes) > 0) {
    for (i in seq_along(cond$notes)) {
      add_row("notes", "note", cond$notes[[i]], as.integer(i))
    }
  }

  data.frame(
    section = vapply(rows, function(x) x$section, character(1)),
    item = vapply(rows, function(x) x$item, character(1)),
    value = vapply(rows, function(x) x$value, character(1)),
    index = vapply(rows, function(x) if (is.na(x$index)) NA_integer_ else as.integer(x$index), integer(1)),
    stringsAsFactors = FALSE
  )
}

main <- function() {
  cli <- parse_cli(commandArgs(trailingOnly = TRUE))
  csc <- path.expand(cli$input)
  if (!file.exists(csc)) {
    stop(sprintf("Input not found: %s", csc), call. = FALSE)
  }
  if (is.null(cli$out_xlsx)) {
    out_xlsx <- file.path(dirname(csc), paste0(tools::file_path_sans_ext(basename(csc)), "_csc_extract.xlsx"))
  } else {
    out_xlsx <- path.expand(cli$out_xlsx)
  }
  dir.create(dirname(out_xlsx), recursive = TRUE, showWarnings = FALSE)

  payload <- load_payload(csc)
  step200 <- find_step200_sequences(payload)
  mono_int <- find_monotonic_int_sequences(payload, n = 20L)
  selected <- select_start_stop(step200, mono_int)
  const_25 <- find_constant_sequences(payload, 2.5, n = 20L)
  volumes <- if (length(const_25) > 0) const_25[[1]]$values else NULL

  injections_df <- data.frame(
    inj_num = integer(),
    start_s = numeric(),
    stop_s = numeric(),
    width_s = numeric(),
    original_start_s = numeric(),
    original_stop_s = numeric(),
    original_width_s = numeric(),
    inj_volume_uL = numeric(),
    stringsAsFactors = FALSE
  )
  if (!is.null(selected$start) && !is.null(selected$stop)) {
    injections_df <- build_injection_df(selected$start, selected$stop, selected$original_stop, volumes)
  }

  heatflow_df <- extract_heatflow_df(payload)

  exp_params <- extract_experiment_parameters(payload)
  if (nrow(exp_params) == 0) {
    exp_params <- data.frame(
      key = character(),
      value = character(),
      confidence = character(),
      note = character(),
      stringsAsFactors = FALSE
    )
  }
  exp_params <- add_input_filename_row(exp_params, basename(csc))

  sheets <- list(
    experiment_parameters = exp_params,
    titration_points = injections_df,
    heatflow = heatflow_df
  )
  write_xlsx(sheets, path = out_xlsx)

  cat(normalizePath(out_xlsx, winslash = "/", mustWork = FALSE), "\n", sep = "")
}

main()
