#!/usr/bin/env Rscript

require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      sprintf("Missing package '%s'. Install with: install.packages('%s')", pkg, pkg),
      call. = FALSE
    )
  }
}

require_pkg("xml2")
require_pkg("writexl")

suppressPackageStartupMessages({
  library(xml2)
  library(writexl)
})

decode_encoded_name_scalar <- function(s) {
  m <- gregexpr("_x[0-9A-Fa-f]{4}_", s, perl = TRUE)[[1]]
  if (length(m) == 1 && m[1] == -1) {
    return(s)
  }
  parts <- regmatches(s, list(m))[[1]]
  repl <- vapply(
    parts,
    function(tok) {
      cp <- strtoi(substr(tok, 3, 6), base = 16L)
      intToUtf8(cp)
    },
    character(1)
  )
  regmatches(s, list(m)) <- list(repl)
  s
}

decode_encoded_name <- function(x) {
  vapply(x, decode_encoded_name_scalar, character(1))
}

parse_cli <- function(argv) {
  if (length(argv) == 0) {
    stop("Usage: Rscript xml_to_xlsx.R <input.xml> [-o <out.xlsx>]", call. = FALSE)
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
      stop("Only one input XML file is supported", call. = FALSE)
    }
    input <- a
    i <- i + 1L
  }
  if (is.null(input)) {
    stop("Input XML path is required", call. = FALSE)
  }
  list(input = input, out_xlsx = out_xlsx)
}

build_rows_by_tag <- function(data_children) {
  tags <- xml_name(data_children)
  idx <- split(seq_along(tags), tags)
  lapply(idx, function(i) data_children[i])
}

row_to_named <- function(row, decode_names = TRUE) {
  kids <- xml_children(row)
  if (length(kids) == 0) {
    return(setNames(character(), character()))
  }
  names_raw <- xml_name(kids)
  vals <- trimws(xml_text(kids))
  if (decode_names) {
    names_raw <- decode_encoded_name(names_raw)
  }
  vals <- as.character(vals)
  names(vals) <- names_raw
  vals
}

is_kv_row <- function(row) {
  kids <- xml_children(row)
  if (length(kids) < 2) {
    return(FALSE)
  }
  identical(xml_name(kids[[1]]), "Key") && identical(xml_name(kids[[2]]), "Value")
}

kv_table_to_named <- function(rows) {
  out <- character()
  if (length(rows) == 0) {
    return(out)
  }
  for (i in seq_along(rows)) {
    row <- rows[[i]]
    if (!is_kv_row(row)) {
      next
    }
    kids <- xml_children(row)
    key <- trimws(xml_text(kids[[1]]))
    val <- trimws(xml_text(kids[[2]]))
    if (!identical(key, "")) {
      out[[key]] <- val
    }
  }
  out
}

find_experiment_kvp_table <- function(rows_by_tag) {
  required <- c("irTable", "injVolTable", "areaTable")
  best_tag <- NULL
  best_data <- character()
  best_score <- -1L
  for (tag in names(rows_by_tag)) {
    rows <- rows_by_tag[[tag]]
    if (length(rows) == 0 || !is_kv_row(rows[[1]])) {
      next
    }
    data <- kv_table_to_named(rows)
    if (!all(required %in% names(data))) {
      next
    }
    score <- length(data)
    if (score > best_score) {
      best_score <- score
      best_tag <- tag
      best_data <- data
    }
  }
  list(tag = best_tag, data = best_data)
}

resolve_table_name <- function(name, rows_by_tag, decoded_to_raw) {
  if (is.null(name) || is.na(name) || identical(name, "")) {
    return(NULL)
  }
  if (name %in% names(rows_by_tag)) {
    return(name)
  }
  if (name %in% names(decoded_to_raw)) {
    return(decoded_to_raw[[name]])
  }
  NULL
}

parse_num <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || trimws(x) == "") {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(x))
}

normalize_lookup_name <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(character())
  }
  out <- enc2utf8(as.character(x))
  out <- iconv(out, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  out[is.na(out)] <- ""
  out <- tolower(trimws(out))
  gsub("[[:space:]]+", " ", out, perl = TRUE)
}

named_value_or_default <- function(named_values, keys, default = "") {
  if (length(named_values) == 0) {
    return(default)
  }
  nms <- names(named_values)
  if (is.null(nms) || length(nms) == 0) {
    return(default)
  }
  nms_norm <- normalize_lookup_name(nms)
  key_norm <- normalize_lookup_name(keys)
  idx <- match(key_norm, nms_norm, nomatch = 0L)
  idx <- idx[idx > 0L]
  if (length(idx) == 0) {
    return(default)
  }
  val <- named_values[[idx[[1]]]]
  if (is.null(val)) {
    return(default)
  }
  val
}

is_valid_xml_codepoint <- function(cp) {
  cp %in% c(9L, 10L, 13L) ||
    (cp >= 32L && cp <= 55295L) ||
    (cp >= 57344L && cp <= 65533L) ||
    (cp >= 65536L && cp <= 1114111L)
}

replace_charrefs <- function(text, pattern, base) {
  m <- gregexpr(pattern, text, perl = TRUE)[[1]]
  if (length(m) == 1 && m[1] == -1) {
    return(list(text = text, removed = 0L))
  }
  tokens <- regmatches(text, list(m))[[1]]
  repl <- character(length(tokens))
  removed <- 0L
  for (i in seq_along(tokens)) {
    num_text <- sub(pattern, "\\1", tokens[[i]], perl = TRUE)
    cp <- suppressWarnings(strtoi(num_text, base = base))
    if (is.na(cp) || !is_valid_xml_codepoint(cp)) {
      repl[[i]] <- ""
      removed <- removed + 1L
    } else {
      repl[[i]] <- tokens[[i]]
    }
  }
  regmatches(text, list(m)) <- list(repl)
  list(text = text, removed = removed)
}

clean_xml_text <- function(path) {
  size <- file.info(path)$size
  if (is.na(size) || !is.finite(size) || size < 0) {
    stop(sprintf("Unable to read XML size for: %s", path), call. = FALSE)
  }
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  payload <- readBin(con, what = "raw", n = size)
  text <- rawToChar(payload)
  Encoding(text) <- "bytes"
  hex_res <- replace_charrefs(text, "&#x([0-9A-Fa-f]+);", base = 16L)
  dec_res <- replace_charrefs(hex_res$text, "&#([0-9]+);", base = 10L)
  ctrl_res <- gsub("[\001-\010\013\014\016-\037]", "", dec_res$text, perl = TRUE)
  Encoding(ctrl_res) <- "bytes"
  list(
    cleaned = ctrl_res,
    stats = list(
      removed_invalid_hex_refs = hex_res$removed,
      removed_invalid_dec_refs = dec_res$removed,
      parse_mode = "cleaned_text_then_read_xml_raw_utf8"
    )
  )
}

kv_to_df <- function(named_values, source) {
  if (length(named_values) == 0) {
    return(data.frame(source = character(), key = character(), value = character(), stringsAsFactors = FALSE))
  }
  data.frame(
    source = source,
    key = names(named_values),
    value = unname(named_values),
    stringsAsFactors = FALSE
  )
}

build_titration_df <- function(rows_by_tag, ir_table, orig_ir_table, inj_table, area_table) {
  ir_rows <- rows_by_tag[[ir_table]]
  orig_ir_rows <- rows_by_tag[[orig_ir_table]]
  inj_rows <- rows_by_tag[[inj_table]]
  area_rows <- rows_by_tag[[area_table]]
  if (is.null(ir_rows)) ir_rows <- list()
  if (is.null(orig_ir_rows)) orig_ir_rows <- list()
  if (is.null(inj_rows)) inj_rows <- list()
  if (is.null(area_rows)) area_rows <- list()

  ir <- lapply(ir_rows, row_to_named, decode_names = TRUE)
  oir <- lapply(orig_ir_rows, row_to_named, decode_names = TRUE)
  inj <- lapply(inj_rows, row_to_named, decode_names = TRUE)
  area <- lapply(area_rows, row_to_named, decode_names = TRUE)

  n <- max(length(ir), length(oir), length(inj), length(area), 0L)
  if (n == 0) {
    return(data.frame(
      index = integer(),
      inj_num = numeric(),
      region = numeric(),
      start_s = numeric(),
      stop_s = numeric(),
      width_s = numeric(),
      orig_start_s = numeric(),
      orig_stop_s = numeric(),
      inj_volume_uL = numeric(),
      Q_uJ = numeric(),
      corrected_Q_uJ = numeric(),
      mole_ratio = numeric(),
      moles_titrant = numeric(),
      moles_titrate = numeric(),
      total_volume_uL = numeric()
    ))
  }

  rows <- vector("list", n)
  for (i in seq_len(n)) {
    ir_i <- if (i <= length(ir)) ir[[i]] else character()
    oir_i <- if (i <= length(oir)) oir[[i]] else character()
    inj_i <- if (i <= length(inj)) inj[[i]] else character()
    area_i <- if (i <= length(area)) area[[i]] else character()

    start_s <- parse_num(named_value_or_default(ir_i, "Start"))
    stop_s <- parse_num(named_value_or_default(ir_i, "Stop"))
    width_s <- if (!is.na(start_s) && !is.na(stop_s)) stop_s - start_s else NA_real_
    inj_vol <- parse_num(named_value_or_default(inj_i, "InjVolume"))
    if (is.na(inj_vol)) {
      inj_vol <- parse_num(named_value_or_default(area_i, "inj volume (uL)"))
    }

    rows[[i]] <- data.frame(
      index = i,
      inj_num = parse_num(named_value_or_default(inj_i, "InjNum")),
      region = parse_num(named_value_or_default(area_i, "Region")),
      start_s = start_s,
      stop_s = stop_s,
      width_s = width_s,
      orig_start_s = parse_num(named_value_or_default(oir_i, "Start")),
      orig_stop_s = parse_num(named_value_or_default(oir_i, "Stop")),
      inj_volume_uL = inj_vol,
      Q_uJ = parse_num(named_value_or_default(area_i, "Q (uJ)")),
      corrected_Q_uJ = parse_num(named_value_or_default(area_i, "Corrected Q (uJ)")),
      mole_ratio = parse_num(named_value_or_default(area_i, "moles titrant / moles titrate")),
      moles_titrant = parse_num(named_value_or_default(area_i, "moles titrant (moles)")),
      moles_titrate = parse_num(named_value_or_default(area_i, "moles titrate (moles)")),
      total_volume_uL = parse_num(named_value_or_default(area_i, "total volume (uL)")),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

choose_heatflow_table <- function(exp_kvp_tag, rows_by_tag, schema_defs = list(), related_tables = character()) {
  score_tag <- function(tag) {
    rows <- rows_by_tag[[tag]]
    if (is.null(rows) || length(rows) == 0L) {
      return(-Inf)
    }
    score <- as.numeric(length(rows))
    low_tag <- tolower(tag)
    if (grepl("(kvp|const)$", low_tag, perl = TRUE)) {
      score <- score - 500
    }
    if (tag %in% related_tables) {
      score <- score - 250
    }

    raw_cols <- schema_defs[[tag]]
    if (is.null(raw_cols) || length(raw_cols) == 0L) {
      raw_cols <- xml_name(xml_children(rows[[1]]))
    }
    dec_cols <- tolower(decode_encoded_name(raw_cols))
    if (length(dec_cols) > 0L) {
      if (any(grepl("time|second|sec", dec_cols, perl = TRUE))) {
        score <- score + 400
      }
      if (any(grepl("heat|power|rate|q\\s*\\(|u?j|u?cal", dec_cols, perl = TRUE))) {
        score <- score + 400
      }
      if (any(grepl("inj|region|moles|volume", dec_cols, perl = TRUE))) {
        score <- score - 150
      }
    }
    score
  }

  if (!is.null(exp_kvp_tag) && endsWith(exp_kvp_tag, "KVP")) {
    candidate <- sub("KVP$", "", exp_kvp_tag)
    if (candidate %in% names(rows_by_tag)) {
      return(candidate)
    }
  }
  if (length(rows_by_tag) == 0) {
    return(NULL)
  }
  tags <- names(rows_by_tag)
  scores <- vapply(tags, score_tag, numeric(1))
  tags[[which.max(scores)]]
}

build_heatflow_df <- function(rows_by_tag, table_tag, schema_defs) {
  rows <- rows_by_tag[[table_tag]]
  if (is.null(rows) || length(rows) == 0) {
    return(data.frame(note = "No heatflow table found", stringsAsFactors = FALSE))
  }
  raw_cols <- schema_defs[[table_tag]]
  if (is.null(raw_cols) || length(raw_cols) == 0) {
    raw_cols <- xml_name(xml_children(rows[[1]]))
  }
  dec_cols <- decode_encoded_name(raw_cols)

  records <- vector("list", length(rows))
  for (i in seq_along(rows)) {
    kids <- xml_children(rows[[i]])
    vals <- setNames(trimws(xml_text(kids)), xml_name(kids))
    rec <- vector("list", length(raw_cols))
    names(rec) <- dec_cols
    for (j in seq_along(raw_cols)) {
      txt <- vals[[raw_cols[[j]]]]
      if (is.null(txt)) {
        txt <- ""
      }
      num <- parse_num(txt)
      rec[[j]] <- if (!is.na(num)) num else txt
    }
    records[[i]] <- as.data.frame(rec, stringsAsFactors = FALSE, check.names = FALSE)
  }

  df <- do.call(rbind, records)
  rownames(df) <- NULL
  for (nm in names(df)) {
    txt <- as.character(df[[nm]])
    num <- suppressWarnings(as.numeric(txt))
    blanks <- trimws(txt) == ""
    if (sum(!is.na(num)) > 0 && all((!is.na(num)) | blanks)) {
      df[[nm]] <- num
    }
  }
  df
}

get_schema_defs <- function(schema_node) {
  defs <- list()
  if (length(schema_node) == 0) {
    return(defs)
  }
  ns <- c(xs = "http://www.w3.org/2001/XMLSchema")
  table_nodes <- xml_find_all(schema_node, ".//xs:choice/xs:element", ns = ns)
  for (t in table_nodes) {
    tname <- xml_attr(t, "name")
    if (is.na(tname) || identical(tname, "")) {
      next
    }
    col_nodes <- xml_find_all(t, ".//xs:sequence/xs:element", ns = ns)
    cols <- xml_attr(col_nodes, "name")
    cols <- cols[!is.na(cols) & cols != ""]
    defs[[tname]] <- as.character(cols)
  }
  defs
}

main <- function() {
  cli <- parse_cli(commandArgs(trailingOnly = TRUE))
  input <- path.expand(cli$input)
  if (!file.exists(input)) {
    stop(sprintf("Input XML not found: %s", input), call. = FALSE)
  }

  if (is.null(cli$out_xlsx)) {
    out_xlsx <- file.path(dirname(input), paste0(tools::file_path_sans_ext(basename(input)), "_xml_extract.xlsx"))
  } else {
    out_xlsx <- path.expand(cli$out_xlsx)
  }
  dir.create(dirname(out_xlsx), recursive = TRUE, showWarnings = FALSE)

  cleaned <- clean_xml_text(input)
  doc <- tryCatch(
    read_xml(
      charToRaw(cleaned$cleaned),
      encoding = "UTF-8",
      options = c("RECOVER", "NOERROR", "NOWARNING")
    ),
    error = function(e) {
      locale_ctype <- tryCatch(Sys.getlocale("LC_CTYPE"), error = function(...) "unknown")
      stop(
        sprintf(
          paste0(
            "Failed to parse XML after cleaning for %s. ",
            "This may be caused by locale/encoding mismatch (LC_CTYPE=%s). ",
            "Original parser error: %s"
          ),
          basename(input),
          locale_ctype,
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
  root <- xml_root(doc)
  root_name <- xml_name(root)

  children <- xml_children(root)
  child_names <- xml_name(children)
  schema_node <- children[child_names == "schema"]
  schema_defs <- get_schema_defs(schema_node)

  data_children <- children[child_names != "schema"]
  rows_by_tag <- build_rows_by_tag(data_children)
  decoded_to_raw <- setNames(names(rows_by_tag), decode_encoded_name(names(rows_by_tag)))

  exp_kvp_info <- find_experiment_kvp_table(rows_by_tag)
  exp_kvp_tag <- exp_kvp_info$tag
  exp_kvp <- exp_kvp_info$data
  global_kvp <- if ("KVP" %in% names(rows_by_tag)) kv_table_to_named(rows_by_tag[["KVP"]]) else character()

  related_decoded <- c(
    areaTable = exp_kvp[["areaTable"]],
    baselineTable = exp_kvp[["baselineTable"]],
    irTable = exp_kvp[["irTable"]],
    originalIRTable = exp_kvp[["originalIRTable"]],
    injVolTable = exp_kvp[["injVolTable"]],
    rgraphTable = exp_kvp[["rgraphTable"]]
  )
  related_raw <- lapply(related_decoded, resolve_table_name, rows_by_tag = rows_by_tag, decoded_to_raw = decoded_to_raw)

  const_tag <- NULL
  const_params <- character()
  if (!is.null(exp_kvp_tag) && endsWith(exp_kvp_tag, "KVP")) {
    cand <- sub("KVP$", "CONST", exp_kvp_tag)
    if (cand %in% names(rows_by_tag)) {
      const_tag <- cand
      const_params <- kv_table_to_named(rows_by_tag[[cand]])
    }
  }

  related_tables <- unique(unlist(related_raw, use.names = FALSE))
  related_tables <- related_tables[!is.na(related_tables) & nzchar(related_tables)]
  heatflow_table <- choose_heatflow_table(
    exp_kvp_tag,
    rows_by_tag,
    schema_defs = schema_defs,
    related_tables = related_tables
  )
  heatflow_df <- build_heatflow_df(rows_by_tag, heatflow_table, schema_defs)

  titration_df <- build_titration_df(
    rows_by_tag = rows_by_tag,
    ir_table = related_raw$irTable,
    orig_ir_table = related_raw$originalIRTable,
    inj_table = related_raw$injVolTable,
    area_table = related_raw$areaTable
  )

  params_df <- rbind(
    data.frame(
      source = "meta",
      key = "InputFileName",
      value = basename(input),
      stringsAsFactors = FALSE
    ),
    kv_to_df(exp_kvp, "experiment_kvp"),
    kv_to_df(const_params, "const"),
    kv_to_df(global_kvp, "global_kvp")
  )

  write_xlsx(
    list(
      experiment_parameters = params_df,
      titration_points = titration_df,
      heatflow = heatflow_df
    ),
    path = out_xlsx
  )

  cat(normalizePath(out_xlsx, winslash = "/", mustWork = FALSE), "\n", sep = "")
}

main()
