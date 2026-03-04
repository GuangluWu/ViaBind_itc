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

parse_cli <- function(argv) {
  if (length(argv) == 0) {
    stop("Usage: Rscript nitc_to_xlsx.R <input.nitc> [-o <out.xlsx>]", call. = FALSE)
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
      stop("Only one input NITC file is supported", call. = FALSE)
    }
    input <- a
    i <- i + 1L
  }
  if (is.null(input)) {
    stop("Input NITC path is required", call. = FALSE)
  }
  list(input = input, out_xlsx = out_xlsx)
}

read_blob <- function(path) {
  size <- file.info(path)$size
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  readBin(con, what = "raw", n = size)
}

load_nitc_payload <- function(nitc_path) {
  raw <- read_blob(nitc_path)
  dec <- tryCatch(
    memDecompress(raw, type = "gzip"),
    error = function(e) e
  )
  if (inherits(dec, "error")) {
    stop(
      sprintf("Input is not a valid gzip-compressed NITC file: %s", nitc_path),
      call. = FALSE
    )
  }
  dec
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

scan_numeric_series <- function(payload, min_len = 500L, abs_min = 1e-9) {
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
      uniq <- length(unique(round(head_vals, 7)))
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
      plausible <- is.finite(v) && v >= -1e5 && v <= 1e5 && (v == 0 || abs(v) >= abs_min)
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

find_arithmetic_int_sequences <- function(data, min_len = 8L, step_min = 20L, step_max = 400L) {
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
      if (run_len >= min_len && run_vals[[1]] >= 0 && run_vals[[run_len]] <= 20000) {
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

select_start_stop <- function(step200, mono_int = NULL, arith = NULL) {
  step_pick <- derive_step_sequence_pair(step200)
  preferred_start <- step_pick$start
  preferred_original_stop <- step_pick$original_stop

  if (!is.null(arith) && length(arith) > 0) {
    best <- NULL
    best_score <- -Inf

    for (i in seq_along(arith)) {
      for (j in seq_along(arith)) {
        if (j <= i) {
          next
        }
        a <- arith[[i]]
        b <- arith[[j]]
        if (a$len != b$len || a$step != b$step) {
          next
        }

        av <- a$values
        bv <- b$values
        if (all(av > bv)) {
          tmp <- av
          av <- bv
          bv <- tmp
        }
        if (!all(bv > av)) {
          next
        }

        # If a preferred start sequence is available from step sequences,
        # do not allow arithmetic branch to shift and skip early injections.
        if (!is.null(preferred_start)) {
          if (length(av) != length(preferred_start) || any(abs(av - preferred_start) > 1e-9)) {
            next
          }
        }

        widths <- bv - av
        in_range <- widths >= 20 & widths <= 400
        score <- sum(in_range)
        if (score < ceiling(length(widths) * 0.6)) {
          next
        }
        if (sd(widths) < 1e-9) {
          score <- score + 5
        }
        med_w <- median(widths)
        if (is.finite(med_w) && med_w >= 20 && med_w <= 400) {
          score <- score + 2
        }
        score <- score + length(av) * 0.5

        better <- FALSE
        if (score > best_score) {
          better <- TRUE
        } else if (!is.null(best) && abs(score - best_score) < 1e-9) {
          if (length(av) > length(best$start)) {
            better <- TRUE
          } else if (length(av) == length(best$start) && av[[1]] < best$start[[1]]) {
            better <- TRUE
          }
        }

        if (better) {
          best_score <- score
          best <- list(start = av, stop = bv, original_stop = bv, source = "arithmetic_pair")
        }
      }
    }

    if (!is.null(best)) {
      best_step <- if (length(best$start) >= 2L) median(diff(best$start)) else NA_real_
      len_vec <- vapply(arith, function(x) x$len, numeric(1))
      longest <- arith[[which.max(len_vec)]]
      if (
        is.finite(best_step) &&
        longest$len > length(best$start) &&
        abs(longest$values[[1]] - best$start[[1]]) < 1e-9 &&
        abs(longest$step - best_step) < 1e-9
      ) {
        if (is.null(preferred_start) ||
            (length(longest$values) == length(preferred_start) && all(abs(longest$values - preferred_start) < 1e-9))) {
          return(list(
            start = longest$values,
            stop = longest$values + longest$step,
            original_stop = longest$values + longest$step,
            source = "arithmetic_derived_from_longest"
          ))
        }
      }
      return(best)
    }

    if (is.null(preferred_start)) {
      ord <- order(
        vapply(arith, function(x) x$len, numeric(1)),
        decreasing = TRUE
      )
      fallback <- arith[[ord[[1]]]]
      return(list(
        start = fallback$values,
        stop = fallback$values + fallback$step,
        original_stop = fallback$values + fallback$step,
        source = "arithmetic_derived"
      ))
    }
  }

  if (is.null(preferred_start)) {
    return(list(start = NULL, stop = NULL, original_stop = NULL, source = "none"))
  }
  start <- preferred_start

  best_stop <- NULL
  best_score <- -1L
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
  }

  if (!is.null(best_stop) && best_score >= 12) {
    orig <- if (!is.null(preferred_original_stop)) preferred_original_stop else (start + 200.0)
    return(list(start = start, stop = best_stop, original_stop = orig, source = "monotonic_int_match"))
  }

  if (!is.null(preferred_original_stop)) {
    return(list(start = start, stop = preferred_original_stop, original_stop = preferred_original_stop, source = "step200_fallback"))
  }
  derived <- start + 200.0
  list(start = start, stop = derived, original_stop = derived, source = "derived_plus_200")
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

extract_ascii_tokens <- function(
  data,
  min_len = 1L,
  max_len = 2000L,
  start_idx = 1L,
  end_idx = length(data)
) {
  ints <- as.integer(data)
  if (length(ints) == 0L) {
    return(character())
  }

  start_idx <- max(1L, as.integer(start_idx))
  end_idx <- min(length(ints), as.integer(end_idx))
  if (start_idx > end_idx) {
    return(character())
  }

  # Use a growable list to avoid repeated vector copies on large payloads.
  token_buf <- vector("list", 1024L)
  token_count <- 0L
  append_token <- function(txt) {
    token_count <<- token_count + 1L
    if (token_count > length(token_buf)) {
      length(token_buf) <<- length(token_buf) * 2L
    }
    token_buf[[token_count]] <<- txt
  }

  run_start <- NA_integer_
  flush_run <- function(end_run_idx) {
    if (is.na(run_start)) {
      return()
    }
    run_len <- end_run_idx - run_start + 1L
    if (run_len < min_len || run_len > max_len) {
      return()
    }
    txt <- rawToChar(as.raw(ints[run_start:end_run_idx]))
    txt <- trimws(txt)
    if (!identical(txt, "")) {
      append_token(txt)
    }
  }

  for (i in seq.int(start_idx, end_idx)) {
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
    flush_run(end_idx)
  }

  if (token_count == 0L) {
    return(character())
  }
  unlist(token_buf[seq_len(token_count)], use.names = FALSE)
}

locate_ascii_anchor_window <- function(payload, anchor = "stirrate", pre = 512L, post = 220000L) {
  if (length(payload) == 0L) {
    return(c(1L, 0L))
  }
  hits <- tryCatch(
    grepRaw(charToRaw(anchor), payload, fixed = TRUE, all = TRUE),
    error = function(e) integer()
  )
  if (length(hits) == 0L) {
    return(c(1L, length(payload)))
  }
  start <- max(1L, as.integer(hits[[1]]) - as.integer(pre))
  end <- min(length(payload), as.integer(hits[[1]]) + as.integer(post))
  c(start, end)
}

find_anchor_index <- function(tokens, anchor) {
  if (length(tokens) < length(anchor)) {
    return(NA_integer_)
  }
  limit <- length(tokens) - length(anchor) + 1L
  for (i in seq_len(limit)) {
    if (all(tokens[i:(i + length(anchor) - 1L)] == anchor)) {
      return(i)
    }
  }
  NA_integer_
}

extract_raw_kv_block <- function(payload) {
  anchor <- c("stirrate", "syringesize", "injrate", "tempsetpoint")

  # Parse around an anchor window first; full-payload min_len=1 scans can be slow.
  win <- locate_ascii_anchor_window(payload, anchor = "stirrate", pre = 512L, post = 220000L)
  tokens <- extract_ascii_tokens(
    payload,
    min_len = 1L,
    max_len = 2000L,
    start_idx = win[[1]],
    end_idx = win[[2]]
  )
  i_start <- find_anchor_index(tokens, anchor)
  if (is.na(i_start)) {
    # Fallback: broader scan with a safer minimum token length.
    tokens <- extract_ascii_tokens(payload, min_len = 3L, max_len = 2000L)
    i_start <- find_anchor_index(tokens, anchor)
    if (is.na(i_start)) {
      return(data.frame(raw_key = character(), raw_value = character(), stringsAsFactors = FALSE))
    }
  }

  i_sha_rel <- match("SHA1", tokens[i_start:length(tokens)])
  if (is.na(i_sha_rel)) {
    return(data.frame(raw_key = character(), raw_value = character(), stringsAsFactors = FALSE))
  }
  i_end <- i_start + i_sha_rel - 1L

  keys <- tokens[i_start:i_end]
  n_keys <- length(keys)
  values_start <- i_end + 1L
  values_end <- min(length(tokens), values_start + n_keys - 1L)
  values <- if (values_start <= values_end) tokens[values_start:values_end] else character()

  if (length(values) < n_keys) {
    values <- c(values, rep("", n_keys - length(values)))
  }
  if (length(values) > n_keys) {
    values <- values[seq_len(n_keys)]
  }

  data.frame(
    raw_key = keys,
    raw_value = values,
    stringsAsFactors = FALSE
  )
}

build_experiment_parameters <- function(payload, input_filename = "") {
  raw_kv <- extract_raw_kv_block(payload)
  rows <- list()

  add_row <- function(key, value, confidence = "high", note = "") {
    rows[[length(rows) + 1L]] <<- list(
      key = key,
      value = value,
      confidence = confidence,
      note = note
    )
  }

  if (!is.null(input_filename) && nzchar(input_filename)) {
    add_row("InputFileName", input_filename, "high", "Source .nitc filename")
  }

  if (nrow(raw_kv) > 0) {
    for (i in seq_len(nrow(raw_kv))) {
      add_row(
        key = paste0("raw::", raw_kv$raw_key[[i]]),
        value = raw_kv$raw_value[[i]],
        confidence = "high"
      )
    }
  }

  lookup_value <- function(src_key) {
    if (nrow(raw_kv) == 0) {
      return(list(value = "", found = FALSE, exact = FALSE))
    }

    exact <- which(raw_kv$raw_key == src_key)
    if (length(exact) > 0) {
      return(list(value = raw_kv$raw_value[[exact[[1]]]], found = TRUE, exact = TRUE))
    }

    idx <- which(tolower(raw_kv$raw_key) == tolower(src_key))
    if (length(idx) > 0) {
      return(list(value = raw_kv$raw_value[[idx[[1]]]], found = TRUE, exact = FALSE))
    }

    list(value = "", found = FALSE, exact = FALSE)
  }

  std_map <- list(
    StirRate = "stirrate",
    Temperature = "tempsetpoint",
    Titrant = "syringeconcentration",
    Titrate = "cellconcentration",
    HeatRateOffset = "heatRateOffset",
    starttime = "starttime",
    equilseconds = "equilseconds",
    UserName = "user",
    Comments = "comments",
    ExothermUp = "ExothermUp",
    Version = "FileVersion"
  )

  for (std_key in names(std_map)) {
    src_key <- std_map[[std_key]]
    got <- lookup_value(src_key)
    if (got$found) {
      note <- if (got$exact) "" else sprintf("Mapped from key '%s' (case-insensitive)", src_key)
      conf <- if (got$exact) "high" else "medium"
      add_row(std_key, got$value, conf, note)
    } else {
      if (nrow(raw_kv) == 0) {
        note <- sprintf("KV block not found; missing source key: %s", src_key)
        add_row(std_key, "", "low", note)
      } else {
        note <- sprintf("Missing source key: %s", src_key)
        add_row(std_key, "", "medium", note)
      }
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      key = character(),
      value = character(),
      confidence = character(),
      note = character(),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    key = vapply(rows, function(x) x$key, character(1)),
    value = vapply(rows, function(x) x$value, character(1)),
    confidence = vapply(rows, function(x) x$confidence, character(1)),
    note = vapply(rows, function(x) x$note, character(1)),
    stringsAsFactors = FALSE
  )
}

get_param_numeric <- function(exp_params, key) {
  if (nrow(exp_params) == 0) {
    return(NA_real_)
  }
  idx <- which(exp_params$key == key)
  if (length(idx) == 0) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(exp_params$value[[idx[[1]]]]))
}

empty_heatflow_df <- function() {
  data.frame(
    "Time (seconds)" = numeric(),
    "Raw Heat Rate (µJ / s)" = numeric(),
    "Raw Heat Rate (µcal / s)" = numeric(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

pick_heat_series <- function(peers, heat_rate_offset = NA_real_) {
  means <- vapply(peers, function(s) mean(s$values, na.rm = TRUE), numeric(1))
  sds <- vapply(peers, function(s) sd(s$values, na.rm = TRUE), numeric(1))
  if (is.finite(heat_rate_offset)) {
    which.min(abs(means - heat_rate_offset))
  } else {
    which.max(sds)
  }
}

build_heatflow_df_from_series <- function(series, heat_rate_offset = NA_real_, allow_synthetic_axis = FALSE) {
  if (length(series) == 0) {
    return(NULL)
  }

  axis_idx <- which(vapply(series, function(s) is_index_axis(s$values), logical(1)))
  axis_values <- NULL
  peers_idx <- integer()

  if (length(axis_idx) > 0) {
    axis <- series[[axis_idx[[1]]]]
    axis_values <- axis$values
    peers_idx <- which(vapply(series, function(s) s$len == axis$len, logical(1)))
    peers_idx <- setdiff(peers_idx, axis_idx[[1]])
  } else if (allow_synthetic_axis) {
    lens <- vapply(series, function(s) s$len, numeric(1))
    uniq <- sort(unique(lens), decreasing = TRUE)
    if (length(uniq) == 0) {
      return(NULL)
    }
    best_len <- uniq[[1]]
    best_cnt <- sum(lens == best_len)
    for (k in uniq) {
      cnt <- sum(lens == k)
      if (cnt > best_cnt || (cnt == best_cnt && k > best_len)) {
        best_len <- k
        best_cnt <- cnt
      }
    }
    peers_idx <- which(lens == best_len)
    axis_values <- seq(0, best_len - 1)
  } else {
    return(NULL)
  }

  out <- data.frame(
    "Time (seconds)" = axis_values,
    "Raw Heat Rate (µJ / s)" = NA_real_,
    "Raw Heat Rate (µcal / s)" = NA_real_,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (length(peers_idx) == 0) {
    return(out)
  }

  peers <- lapply(peers_idx, function(i) series[[i]])
  uj_idx <- pick_heat_series(peers, heat_rate_offset = heat_rate_offset)

  uj <- peers[[uj_idx]]$values
  out[["Raw Heat Rate (µJ / s)"]] <- uj

  uc_idx <- NA_integer_
  best_delta <- Inf
  for (i in seq_along(peers)) {
    if (i == uj_idx) {
      next
    }
    cand <- peers[[i]]$values
    ok <- is.finite(uj) & is.finite(cand) & abs(cand) > 1e-12
    if (!any(ok)) {
      next
    }
    ratio <- median(uj[ok] / cand[ok], na.rm = TRUE)
    if (!is.finite(ratio)) {
      next
    }
    if (ratio >= 3.5 && ratio <= 4.8) {
      delta <- abs(ratio - 4.184)
      if (delta < best_delta) {
        best_delta <- delta
        uc_idx <- i
      }
    }
  }

  if (!is.na(uc_idx)) {
    out[["Raw Heat Rate (µcal / s)"]] <- peers[[uc_idx]]$values
  } else {
    out[["Raw Heat Rate (µcal / s)"]] <- uj / 4.184
  }

  out
}

extract_heatflow_df <- function(payload, heat_rate_offset = NA_real_) {
  primary <- scan_numeric_series(payload, min_len = 500L, abs_min = 1e-9)
  out <- build_heatflow_df_from_series(primary, heat_rate_offset = heat_rate_offset, allow_synthetic_axis = FALSE)
  if (!is.null(out)) {
    return(out)
  }

  # Fallback for short/partial records: use lower threshold and synthetic axis if needed.
  fallback <- scan_numeric_series(payload, min_len = 100L, abs_min = 1e-12)
  out2 <- build_heatflow_df_from_series(fallback, heat_rate_offset = heat_rate_offset, allow_synthetic_axis = TRUE)
  if (!is.null(out2)) {
    return(out2)
  }

  empty_heatflow_df()
}

main <- function() {
  cli <- parse_cli(commandArgs(trailingOnly = TRUE))
  nitc <- path.expand(cli$input)
  if (!file.exists(nitc)) {
    stop(sprintf("Input not found: %s", nitc), call. = FALSE)
  }

  if (is.null(cli$out_xlsx)) {
    out_xlsx <- file.path(dirname(nitc), paste0(tools::file_path_sans_ext(basename(nitc)), "_nitc_extract.xlsx"))
  } else {
    out_xlsx <- path.expand(cli$out_xlsx)
  }
  dir.create(dirname(out_xlsx), recursive = TRUE, showWarnings = FALSE)

  payload <- load_nitc_payload(nitc)
  exp_params <- build_experiment_parameters(payload, input_filename = basename(nitc))

  step200 <- find_step200_sequences(payload)
  arith <- find_arithmetic_int_sequences(payload, min_len = 6L, step_min = 20L, step_max = 400L)
  selected <- select_start_stop(step200 = step200, arith = arith)
  if (identical(selected$source, "none")) {
    mono_int <- find_monotonic_int_sequences(payload, n = 20L)
    selected <- select_start_stop(step200 = step200, mono_int = mono_int)
  }

  n_inj <- if (!is.null(selected$start)) length(selected$start) else 20L
  const_25 <- find_constant_run(payload, 2.5, min_len = max(6L, min(20L, n_inj)))
  volumes <- if (!is.null(const_25)) const_25$values else NULL

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

  heat_offset <- get_param_numeric(exp_params, "HeatRateOffset")
  heatflow_df <- extract_heatflow_df(payload, heat_rate_offset = heat_offset)

  sheets <- list(
    experiment_parameters = exp_params,
    titration_points = injections_df,
    heatflow = heatflow_df
  )
  write_xlsx(sheets, path = out_xlsx)

  cat(normalizePath(out_xlsx, winslash = "/", mustWork = FALSE), "\n", sep = "")
}

main()
