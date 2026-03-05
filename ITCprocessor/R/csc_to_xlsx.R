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
  unit_like <- abs(diffs[finite] - 1.0) < 1e-6
  good_ratio <- mean(unit_like)
  mono_ratio <- mean(diffs[finite] > 0)
  starts_on_integer_grid <- is.finite(check[[1]]) && abs(check[[1]] - round(check[[1]])) < 1e-3
  starts_on_integer_grid && good_ratio > 0.98 && mono_ratio > 0.995
}

pick_best_axis_idx <- function(series, axis_idx) {
  if (length(axis_idx) == 0L) {
    return(NA_integer_)
  }
  if (length(axis_idx) == 1L) {
    return(axis_idx[[1]])
  }
  lens <- vapply(series, function(s) s$len, numeric(1))
  starts <- vapply(series, function(s) s$start, numeric(1))
  peer_cnt <- vapply(axis_idx, function(i) as.integer(sum(lens == lens[[i]]) - 1L), integer(1))
  ord <- order(-peer_cnt, -lens[axis_idx], starts[axis_idx])
  axis_idx[[ord[[1]]]]
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
  axis_pick <- pick_best_axis_idx(series, axis_idx)
  axis <- series[[axis_pick]]
  peers_idx <- which(vapply(series, function(s) s$len == axis$len, logical(1)))
  peers_idx <- setdiff(peers_idx, axis_pick)
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

find_step200_sequences <- function(data, min_len = 6L) {
  out <- list()
  tol <- 1e-9
  for (phase in 0:7) {
    vals <- read_doubles_by_phase(data, phase)
    if (length(vals) < min_len) {
      next
    }

    i <- 1L
    while (i < length(vals)) {
      a <- vals[[i]]
      b <- vals[[i + 1L]]
      if (!(is_intish(a, tol) && is_intish(b, tol))) {
        i <- i + 1L
        next
      }
      d <- b - a
      if (!is.finite(d) || abs(d - 200.0) > tol) {
        i <- i + 1L
        next
      }

      j <- i + 1L
      while (j < length(vals)) {
        x1 <- vals[[j]]
        x2 <- vals[[j + 1L]]
        if (!(is_intish(x1, tol) && is_intish(x2, tol))) {
          break
        }
        dd <- x2 - x1
        if (!is.finite(dd) || abs(dd - 200.0) > tol) {
          break
        }
        j <- j + 1L
      }

      run_vals <- as.numeric(round(vals[i:j]))
      run_len <- length(run_vals)
      if (!(run_len >= min_len && run_vals[[1]] >= 0 && run_vals[[run_len]] >= run_vals[[1]])) {
        i <- j + 1L
        next
      }

      out[[length(out) + 1L]] <- list(
        phase = phase,
        start = phase + (i - 1L) * 8L,
        len = run_len,
        step = 200.0,
        values = run_vals
      )

      i <- j + 1L
    }
  }
  dedup_seq_by_key(out, function(x) paste(format(round(x$values, 9), scientific = FALSE, trim = TRUE), collapse = ","))
}

find_arithmetic_int_sequences <- function(data, min_len = 8L, step_min = 1L, step_max = 10000L) {
  out <- list()
  tol <- 1e-9

  for (phase in 0:7) {
    vals <- read_doubles_by_phase(data, phase)
    if (length(vals) < min_len) {
      next
    }

    i <- 1L
    while (i < length(vals)) {
      v1 <- vals[[i]]
      v2 <- vals[[i + 1L]]
      if (!(is_intish(v1, tol) && is_intish(v2, tol))) {
        i <- i + 1L
        next
      }

      d_raw <- v2 - v1
      d <- round(d_raw)
      if (!is.finite(d) || abs(d) > 1e6) {
        i <- i + 1L
        next
      }
      if (d < step_min || d > step_max || abs(d_raw - d) > tol) {
        i <- i + 1L
        next
      }

      j <- i + 1L
      while (j < length(vals)) {
        a <- vals[[j]]
        b <- vals[[j + 1L]]
        if (!(is_intish(a, tol) && is_intish(b, tol))) {
          break
        }
        dd <- b - a
        if (!is.finite(dd)) {
          break
        }
        if (abs(dd - d) > tol) {
          break
        }
        j <- j + 1L
      }

      run_vals <- as.numeric(round(vals[i:j]))
      run_len <- length(run_vals)
      if (run_len >= min_len && run_vals[[1]] >= 0 && run_vals[[run_len]] >= run_vals[[1]]) {
        out[[length(out) + 1L]] <- list(
          phase = phase,
          start = phase + (i - 1L) * 8L,
          step = d,
          len = run_len,
          values = run_vals
        )
      }

      i <- j + 1L
    }
  }

  dedup_seq_by_key(out, function(x) paste(c(x$step, x$values), collapse = ","))
}

find_monotonic_int_sequences <- function(data, n = 8L) {
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

find_constant_sequences <- function(data, target, n = 6L) {
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

find_constant_run <- function(data, target, tol = 1e-12, min_len = 6L) {
  best <- NULL
  for (phase in 0:7) {
    vals <- read_doubles_by_phase(data, phase)
    if (length(vals) == 0) {
      next
    }
    i <- 1L
    while (i <= length(vals)) {
      if (!(is.finite(vals[[i]]) && abs(vals[[i]] - target) < tol)) {
        i <- i + 1L
        next
      }
      j <- i
      while (j < length(vals) && is.finite(vals[[j + 1L]]) && abs(vals[[j + 1L]] - target) < tol) {
        j <- j + 1L
      }
      run_len <- j - i + 1L
      if (run_len >= min_len) {
        run_vals <- vals[i:j]
        if (is.null(best) || run_len > best$len) {
          best <- list(
            phase = phase,
            start = phase + (i - 1L) * 8L,
            len = run_len,
            values = run_vals
          )
        }
      }
      i <- j + 1L
    }
  }
  best
}

find_best_volume_run <- function(
  data,
  expected_len = NA_integer_,
  preferred_targets = numeric(),
  tol = 1e-12,
  min_len = 3L,
  value_min = 0.01,
  value_max = 100.0
) {
  min_len <- max(1L, as.integer(min_len))
  expected_len <- suppressWarnings(as.integer(expected_len))
  has_expected <- is.finite(expected_len) && expected_len > 0L

  candidates <- list()
  add_candidate <- function(item, preferred = FALSE) {
    if (is.null(item) || is.null(item$values) || length(item$values) < min_len) {
      return()
    }
    v <- suppressWarnings(as.numeric(item$values[[1]]))
    if (!is.finite(v) || v <= 0 || v < value_min || v > value_max) {
      return()
    }
    candidates[[length(candidates) + 1L]] <<- list(
      phase = item$phase,
      start = item$start,
      len = as.integer(length(item$values)),
      value = v,
      values = as.numeric(item$values),
      preferred = isTRUE(preferred)
    )
  }

  pref <- suppressWarnings(as.numeric(preferred_targets))
  pref <- unique(pref[is.finite(pref) & pref > 0 & pref >= value_min & pref <= value_max])
  if (length(pref) > 0L) {
    for (target in pref) {
      run <- find_constant_run(data, target = target, tol = tol, min_len = min_len)
      if (!is.null(run)) {
        add_candidate(run, preferred = TRUE)
      }
    }
  }

  for (phase in 0:7) {
    vals <- read_doubles_by_phase(data, phase)
    if (length(vals) == 0L) {
      next
    }
    i <- 1L
    while (i <= length(vals)) {
      v <- vals[[i]]
      if (!(is.finite(v) && v > 0 && v >= value_min && v <= value_max)) {
        i <- i + 1L
        next
      }
      j <- i
      while (j < length(vals) && is.finite(vals[[j + 1L]]) && abs(vals[[j + 1L]] - v) < tol) {
        j <- j + 1L
      }
      run_len <- j - i + 1L
      if (run_len >= min_len) {
        add_candidate(
          list(
            phase = phase,
            start = phase + (i - 1L) * 8L,
            values = vals[i:j]
          ),
          preferred = FALSE
        )
      }
      i <- j + 1L
    }
  }

  if (length(candidates) == 0L) {
    return(NULL)
  }

  lens <- vapply(candidates, function(x) as.numeric(x$len), numeric(1))
  starts <- vapply(candidates, function(x) as.numeric(x$start), numeric(1))
  preferred_flag <- vapply(candidates, function(x) isTRUE(x$preferred), logical(1))
  len_delta <- if (has_expected) abs(lens - expected_len) else rep(0, length(lens))
  score <- lens - 2 * len_delta + ifelse(preferred_flag, 3, 0)
  ord <- order(-score, -as.integer(preferred_flag), len_delta, -lens, starts)
  candidates[[ord[[1]]]]
}

score_widths <- function(widths, min_coverage = 0.6) {
  valid <- is.finite(widths) & widths > 0
  n_valid <- sum(valid)
  needed <- max(1L, ceiling(length(widths) * min_coverage))
  if (n_valid < needed) {
    return(list(score = -Inf, n_valid = n_valid, cv = Inf))
  }
  w <- widths[valid]
  med <- stats::median(w)
  cv <- if (length(w) >= 2L && is.finite(med) && med > 0) stats::sd(w) / med else 0
  score <- n_valid - min(cv, 5)
  list(score = score, n_valid = n_valid, cv = cv)
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
      ws <- score_widths(widths, min_coverage = 0.6)
      if (!is.finite(ws$score)) next
      score <- ws$score
      if (is.finite(ws$cv) && ws$cv < 1e-9) score <- score + 4
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

is_sequence_extension <- function(candidate, base, tol = 1e-9) {
  if (is.null(base)) {
    return(TRUE)
  }
  if (length(candidate) < length(base)) {
    return(FALSE)
  }
  all(abs(candidate[seq_len(length(base))] - base) <= tol)
}

select_start_stop <- function(step200, mono_int = NULL, arith = NULL) {
  step_pick <- derive_step_sequence_pair(step200)
  preferred_start <- step_pick$start
  preferred_original_stop <- step_pick$original_stop

  if (!is.null(arith) && length(arith) > 0L) {
    cand_idx <- seq_along(arith)
    if (!is.null(preferred_start)) {
      compat <- which(vapply(
        arith,
        function(x) is_sequence_extension(as.numeric(x$values), preferred_start),
        logical(1)
      ))
      cand_idx <- compat
    }
    if (length(cand_idx) > 0L) {
      lens <- vapply(arith[cand_idx], function(x) x$len, numeric(1))
      starts <- vapply(arith[cand_idx], function(x) x$values[[1]], numeric(1))
      steps <- vapply(arith[cand_idx], function(x) x$step, numeric(1))
      pref_step <- if (!is.null(preferred_start) && length(preferred_start) >= 2L) {
        suppressWarnings(stats::median(diff(preferred_start)))
      } else {
        NA_real_
      }
      step_delta <- if (is.finite(pref_step)) abs(steps - pref_step) else abs(steps - 200.0)
      ord <- order(-lens, step_delta, starts)
      best_arith <- arith[[cand_idx[[ord[[1]]]]]]

      start <- as.numeric(best_arith$values)
      stop <- start + as.numeric(best_arith$step)
      source <- "arithmetic_derived"

      same_shape_idx <- which(vapply(
        arith,
        function(x) x$len == best_arith$len && x$step == best_arith$step,
        logical(1)
      ))
      if (length(same_shape_idx) > 0L) {
        best_pair <- NULL
        best_pair_score <- -Inf
        for (i in same_shape_idx) {
          cand <- as.numeric(arith[[i]]$values)
          if (all(cand == start) || !all(cand > start)) {
            next
          }
          widths <- cand - start
          ws <- score_widths(widths, min_coverage = 0.6)
          if (!is.finite(ws$score)) {
            next
          }
          score <- ws$score + length(start) * 0.25
          if (is.finite(ws$cv) && ws$cv < 1e-9) {
            score <- score + 1
          }
          if (score > best_pair_score) {
            best_pair_score <- score
            best_pair <- cand
          }
        }
        if (!is.null(best_pair) && best_pair_score > 0) {
          stop <- best_pair
          source <- "arithmetic_pair"
        }
      }

      return(list(
        start = start,
        stop = stop,
        original_stop = stop,
        source = source
      ))
    }
  }

  if (is.null(preferred_start)) {
    return(list(start = NULL, stop = NULL, original_stop = NULL, source = "none"))
  }
  start <- preferred_start

  best_stop <- NULL
  best_score <- -Inf
  best_support <- 0L
  if (!is.null(mono_int)) {
    for (cand_item in mono_int) {
      cand <- cand_item$values
      if (length(cand) != length(start)) {
        next
      }
      if (all(cand == start)) {
        next
      }
      widths <- cand - start
      ws <- score_widths(widths, min_coverage = 0.5)
      if (!is.finite(ws$score)) {
        next
      }
      score <- ws$score
      if (length(unique(round(widths[is.finite(widths)], 6))) > 5) {
        score <- score + 2L
      }
      support <- as.integer(ws$n_valid)
      if (support > best_support || (support == best_support && score > best_score)) {
        best_support <- support
        best_score <- score
        best_stop <- cand
      }
    }
  }
  if (!is.null(best_stop) && best_support >= max(3L, ceiling(length(start) * 0.35))) {
    orig <- if (!is.null(preferred_original_stop)) preferred_original_stop else (start + 200.0)
    return(list(start = start, stop = best_stop, original_stop = orig, source = "monotonic_int_match"))
  }

  if (!is.null(preferred_original_stop)) {
    return(list(start = start, stop = preferred_original_stop, original_stop = preferred_original_stop, source = "step200_fallback"))
  }
  derived <- start + 200.0
  list(start = start, stop = derived, original_stop = derived, source = "derived_plus_200")
}

select_from_monotonic_sequences <- function(mono_int) {
  if (is.null(mono_int) || length(mono_int) == 0L) {
    return(list(start = NULL, stop = NULL, original_stop = NULL, source = "none"))
  }
  lens <- vapply(mono_int, function(x) length(x$values), integer(1))
  starts <- vapply(mono_int, function(x) x$values[[1]], numeric(1))
  ord <- order(-lens, starts)
  best <- mono_int[[ord[[1]]]]
  start <- as.numeric(best$values)
  ds <- diff(start)
  ds <- ds[is.finite(ds) & ds > 0]
  step <- if (length(ds) > 0L) stats::median(ds) else 200.0
  stop <- start + step
  list(start = start, stop = stop, original_stop = stop, source = "monotonic_derived")
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
  if (is.na(idx_root)) return(character())

  idx_end <- length(coarse)
  h4_idx <- which(startsWith(coarse_text, "H4sIA") & seq_along(coarse_text) > idx_root)
  if (length(h4_idx) > 0L) {
    idx_end <- max(idx_root, h4_idx[[1]] - 1L)
  } else {
    serialized_idx <- which(
      grepl("CSCAnalysis, Version=|^System\\.Collections\\.", coarse_text, perl = TRUE) &
        seq_along(coarse_text) > idx_root
    )
    if (length(serialized_idx) > 0L) {
      idx_end <- max(idx_root, serialized_idx[[1]] - 1L)
    }
  }

  start <- max(1L, coarse[[idx_root]]$start - 40L)
  end <- min(length(payload), coarse[[idx_end]]$end + 40L)
  seg <- payload[start:end]
  dense <- extract_ascii_runs(seg, min_len = 3L)
  vapply(dense, function(x) x$text, character(1))
}

extract_parameter_key_value_tokens <- function(runs) {
  if (length(runs) == 0L) {
    return(list(keys = character(), values = character()))
  }

  known_keys <- c(
    "root",
    "units",
    "calUnitsColumnIndex",
    "XColumnName",
    "YColumnName",
    "areaTable",
    "baselineTable",
    "irTable",
    "originalIRTable",
    "injVolTable",
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
    "BlankSubtractType",
    "bUseDefaultInjVolume",
    "rgraphTable",
    "ModelFitSettings",
    "bBaselineExists",
    "bValidAnalysisForBatch",
    "bValidExperimentParams",
    "bFitClosenessCriteriaMet",
    "FitModels",
    "ConfidenceSettings",
    "NumModels",
    "ITCResultsLog",
    "ReportViews"
  )

  i_root <- match("root", runs)
  if (is.na(i_root)) {
    return(list(keys = character(), values = character()))
  }

  is_key_like <- function(tok) {
    tok %in% known_keys || grepl("^[A-Za-z][A-Za-z0-9_.-]{1,80}$", tok, perl = TRUE)
  }
  is_value_like <- function(tok) {
    startsWith(tok, "H4sIA") ||
      grepl(TOKEN_DATE_RE, tok, perl = TRUE) ||
      grepl("^(true|false)$", tok, ignore.case = TRUE, perl = TRUE) ||
      grepl("^-?\\d+(?:\\.\\d+)?$", tok, perl = TRUE)
  }

  i <- i_root
  keys <- c("root")
  i <- i + 1L
  while (i <= length(runs)) {
    tok <- runs[[i]]
    if (!is_key_like(tok)) {
      break
    }
    # After enough keys are seen, a value-like unknown token likely marks value stream start.
    if (length(keys) >= 8L && !(tok %in% known_keys) && is_value_like(tok)) {
      break
    }
    keys <- c(keys, tok)
    i <- i + 1L
  }
  values <- if (i <= length(runs)) runs[i:length(runs)] else character()
  list(keys = keys, values = values)
}

extract_experiment_parameters <- function(payload) {
  runs <- extract_parameter_tokens_dense(payload)
  if (length(runs) == 0) return(data.frame())
  key_value <- extract_parameter_key_value_tokens(runs)
  if (length(key_value$values) == 0L || length(key_value$keys) <= 1L) return(data.frame())
  values <- key_value$values
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

  key_order <- key_value$keys[key_value$keys != "root"]
  key_order <- key_order[!duplicated(key_order)]

  infer_type <- function(key) {
    k <- tolower(key)
    if (grepl("table$", k, perl = TRUE)) return("date")
    if (k %in% c("units", "xcolumnname", "ycolumnname", "comments", "username", "analysistype", "blanksubtracttype", "reportviews")) return("text")
    if (k == "starttime") return("ticks")
    if (k %in% c("calunitscolumnindex", "equilseconds", "integrationregionstart", "integrationregionwidth", "integrationregioninterval", "numregions")) return("int")
    if (grepl("^(is|auto|b|exotherm|continous|partially)", k, perl = TRUE)) return("bool")
    if (grepl("volume|rate|temp|offset|titrant|titrate|subtraction|firstinjstarttime|version", k, perl = TRUE)) return("num")
    "text"
  }

  token_matches <- function(tok, type, key = "") {
    if (is.null(tok) || is.na(tok) || identical(tok, "") || startsWith(tok, "H4sIA")) {
      return(FALSE)
    }
    if (identical(type, "date")) return(grepl(TOKEN_DATE_RE, tok, perl = TRUE))
    if (identical(type, "bool")) return(grepl("^(true|false)$", tok, ignore.case = TRUE, perl = TRUE))
    if (identical(type, "ticks")) return(grepl("^\\d{10,20}$", tok, perl = TRUE))
    if (identical(type, "int")) return(grepl("^-?\\d+$", tok, perl = TRUE))
    if (identical(type, "num")) {
      ok <- grepl("^-?\\d+(?:\\.\\d+)?$", tok, perl = TRUE)
      if (!ok) return(FALSE)
      if (identical(key, "Temperature")) {
        v <- suppressWarnings(as.numeric(tok))
        return(is.finite(v) && v >= -20 && v <= 80)
      }
      return(TRUE)
    }
    TRUE
  }

  ptr <- 1L
  pull_value <- function(key, ptr0) {
    if (ptr0 > length(values)) {
      return(list(value = "", ptr = ptr0, confidence = "low", note = "No remaining token"))
    }
    type <- infer_type(key)
    tok <- values[[ptr0]]

    if (identical(key, "YColumnName") && (ptr0 + 1L) <= length(values)) {
      joined <- paste0(values[[ptr0]], values[[ptr0 + 1L]])
      if (grepl("Heat Rate", joined, ignore.case = TRUE, perl = TRUE) || grepl("\\(.*\\)", joined, perl = TRUE)) {
        return(list(value = joined, ptr = ptr0 + 2L, confidence = "high", note = "Reconstructed from split tokens"))
      }
    }

    if (token_matches(tok, type, key = key)) {
      return(list(value = tok, ptr = ptr0 + 1L, confidence = "high", note = ""))
    }

    lookahead <- if (ptr0 < length(values)) {
      seq.int(ptr0 + 1L, min(length(values), ptr0 + 6L))
    } else {
      integer()
    }
    hit <- NA_integer_
    if (length(lookahead) > 0L) {
      for (j in lookahead) {
        if (token_matches(values[[j]], type, key = key)) {
          hit <- j
          break
        }
      }
    }
    if (!is.na(hit)) {
      return(list(
        value = values[[hit]],
        ptr = hit + 1L,
        confidence = "medium",
        note = sprintf("Matched with lookahead offset %d", hit - ptr0)
      ))
    }

    if (identical(tolower(key), "comments")) {
      return(list(value = "", ptr = ptr0, confidence = "medium", note = "Likely empty in source"))
    }

    if (identical(type, "text") && !grepl("^(true|false)$", tok, ignore.case = TRUE, perl = TRUE)) {
      return(list(value = tok, ptr = ptr0 + 1L, confidence = "medium", note = "Accepted as free text"))
    }

    list(value = "", ptr = ptr0, confidence = "low", note = "Token did not match expected type")
  }

  for (k in key_order) {
    taken <- pull_value(k, ptr)
    add_row(k, taken$value, taken$confidence, taken$note)
    ptr <- taken$ptr
  }

  if (!("rgraphTable" %in% key_order)) {
    tail_date <- ""
    for (v in values) {
      if (grepl(TOKEN_DATE_RE, v, perl = TRUE)) {
        tail_date <- v
        break
      }
    }
    add_row("rgraphTable", tail_date, if (nchar(tail_date) > 0) "medium" else "low")
  }

  data.frame(
    key = vapply(rows, function(x) x$key, character(1)),
    value = vapply(rows, function(x) x$value, character(1)),
    confidence = vapply(rows, function(x) x$confidence, character(1)),
    note = vapply(rows, function(x) x$note, character(1)),
    stringsAsFactors = FALSE
  )
}

collect_numeric_param_values <- function(params_df, keys) {
  if (is.null(params_df) || nrow(params_df) == 0L || length(keys) == 0L) {
    return(numeric())
  }
  out <- numeric()
  key_lc <- tolower(as.character(params_df$key))
  for (k in keys) {
    idx <- which(key_lc == tolower(k))
    if (length(idx) == 0L) {
      next
    }
    vals <- suppressWarnings(as.numeric(params_df$value[idx]))
    vals <- vals[is.finite(vals)]
    if (length(vals) > 0L) {
      out <- c(out, vals)
    }
  }
  unique(out)
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
  n <- min(length(starts), length(stops), length(original_stops))
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
  inj_vol <- rep(NA_real_, n)
  if (!is.null(volumes) && length(volumes) > 0L) {
    m <- min(n, length(volumes))
    inj_vol[seq_len(m)] <- as.numeric(volumes[seq_len(m)])
  }
  data.frame(
    inj_num = seq_len(n),
    start_s = starts[seq_len(n)],
    stop_s = stops[seq_len(n)],
    width_s = stops[seq_len(n)] - starts[seq_len(n)],
    original_start_s = starts[seq_len(n)],
    original_stop_s = original_stops[seq_len(n)],
    original_width_s = original_stops[seq_len(n)] - starts[seq_len(n)],
    inj_volume_uL = inj_vol,
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
  step200 <- find_step200_sequences(payload, min_len = 6L)
  arith <- find_arithmetic_int_sequences(payload, min_len = 6L)
  selected <- select_start_stop(step200 = step200, arith = arith)
  mono_int <- NULL
  if (identical(selected$source, "none")) {
    mono_int <- find_monotonic_int_sequences(payload, n = 8L)
    selected <- select_start_stop(step200 = step200, mono_int = mono_int)
  }
  if (identical(selected$source, "none")) {
    selected <- select_from_monotonic_sequences(mono_int)
  }
  n_inj <- if (!is.null(selected$start)) length(selected$start) else 0L
  volume_min_len <- if (n_inj > 0L) max(3L, min(12L, as.integer(n_inj))) else 6L
  volume_targets <- collect_numeric_param_values(
    exp_params,
    keys = c("DefaultInjVolume", "raw::injvol", "raw::injvolume", "raw::injectionvolume", "raw::defaultinjvolume")
  )
  volume_run <- find_best_volume_run(
    payload,
    expected_len = n_inj,
    preferred_targets = volume_targets,
    min_len = volume_min_len
  )
  volumes <- if (!is.null(volume_run)) volume_run$values else NULL

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
