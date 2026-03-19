#!/usr/bin/env Rscript

encode_error_payload <- function(message) {
  payload <- list(
    message = as.character(message)[1],
    ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    return(jsonlite::toJSON(payload, auto_unbox = TRUE))
  }
  escaped <- gsub("\"", "\\\\\"", payload$message, fixed = TRUE)
  sprintf("{\"message\":\"%s\",\"ts\":\"%s\"}", escaped, payload$ts)
}

fail <- function(message, code = 1L) {
  cat(sprintf("ITCSUITE_ERROR %s\n", encode_error_payload(message)))
  flush.console()
  quit(save = "no", status = as.integer(code))
}

parse_args <- function(args) {
  out <- list(
    repo_root = NULL,
    app_dir = "ITCSuiteWeb",
    host = "127.0.0.1",
    port = 0L,
    log_dir = NULL
  )

  i <- 1L
  while (i <= length(args)) {
    key <- args[[i]]
    if (!startsWith(key, "--")) {
      fail(sprintf("Unknown argument: %s", key))
    }

    if (i == length(args)) {
      fail(sprintf("Missing value for argument: %s", key))
    }

    val <- args[[i + 1L]]
    i <- i + 2L

    if (key == "--repo-root") {
      out$repo_root <- val
    } else if (key == "--app-dir") {
      out$app_dir <- val
    } else if (key == "--host") {
      out$host <- val
    } else if (key == "--port") {
      out$port <- suppressWarnings(as.integer(val))
    } else if (key == "--log-dir") {
      out$log_dir <- val
    } else {
      fail(sprintf("Unknown argument: %s", key))
    }
  }

  out
}

write_log <- function(log_file, ...) {
  if (is.null(log_file)) return(invisible(NULL))
  line <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste0(..., collapse = ""), "\n")
  cat(line, file = log_file, append = TRUE)
  invisible(NULL)
}

ensure_dependency <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    fail(sprintf("Required package missing: %s", pkg))
  }
}

choose_port <- function(requested, host = "127.0.0.1") {
  if (is.null(requested) || !is.finite(requested) || requested < 0L) {
    fail("Invalid --port value")
  }
  if (requested > 0L) return(as.integer(requested))

  ensure_dependency("httpuv")

  for (i in 1:20) {
    p <- as.integer(sample.int(20000L, size = 1L) + 30000L)
    srv <- tryCatch({
      httpuv::startServer(host, p, list())
    }, error = function(e) NULL)

    if (!is.null(srv)) {
      httpuv::stopServer(srv)
      return(p)
    }
  }

  fail("Could not find an available port after 20 attempts")
}

ensure_utf8_locale <- function(log_file = NULL) {
  options(encoding = "UTF-8")

  current <- tryCatch(Sys.getlocale("LC_CTYPE"), error = function(e) "")
  if (is.character(current) && grepl("UTF-?8", current, ignore.case = TRUE)) {
    write_log(log_file, "locale LC_CTYPE already UTF-8: ", current)
    return(invisible(TRUE))
  }

  candidates <- unique(c(
    Sys.getenv("LC_CTYPE", unset = ""),
    Sys.getenv("LANG", unset = ""),
    "en_US.UTF-8",
    "zh_CN.UTF-8",
    "C.UTF-8",
    "UTF-8"
  ))
  candidates <- candidates[nzchar(candidates)]

  for (loc in candidates) {
    ok <- tryCatch({
      out <- suppressWarnings(Sys.setlocale("LC_CTYPE", loc))
      is.character(out) && nzchar(out) && !is.na(out) && grepl("UTF-?8", out, ignore.case = TRUE)
    }, error = function(e) FALSE)
    if (isTRUE(ok)) {
      write_log(log_file, "locale LC_CTYPE set to UTF-8 via: ", loc)
      return(invisible(TRUE))
    }
  }

  write_log(log_file, "warning: failed to switch LC_CTYPE to UTF-8; current=", tryCatch(Sys.getlocale("LC_CTYPE"), error = function(e) "unknown"))
  invisible(FALSE)
}

main <- function() {
  ensure_dependency("jsonlite")
  ensure_dependency("shiny")

  args <- parse_args(commandArgs(trailingOnly = TRUE))

  if (is.null(args$repo_root)) {
    fail("Missing required argument: --repo-root")
  }

  repo_root <- normalizePath(args$repo_root, winslash = "/", mustWork = FALSE)
  if (!dir.exists(repo_root)) {
    fail(sprintf("Repo root does not exist: %s", repo_root))
  }

  app_path <- file.path(repo_root, args$app_dir)
  app_path <- normalizePath(app_path, winslash = "/", mustWork = FALSE)
  if (!dir.exists(app_path)) {
    fail(sprintf("App directory does not exist: %s", app_path))
  }

  for (required_dir in c("ITCprocessor", "ITCsimfit", "ITCgraph", "ITCSuiteWeb")) {
    if (!dir.exists(file.path(repo_root, required_dir))) {
      fail(sprintf("Missing suite component under repo root: %s", required_dir))
    }
  }

  log_file <- NULL
  if (!is.null(args$log_dir) && nzchar(args$log_dir)) {
    dir.create(args$log_dir, recursive = TRUE, showWarnings = FALSE)
    log_file <- file.path(args$log_dir, "launch_shiny.log")
  }

  write_log(log_file, "repo_root=", repo_root)
  write_log(log_file, "app_path=", app_path)
  ensure_utf8_locale(log_file = log_file)

  port <- choose_port(args$port, as.character(args$host)[1])
  ready_payload <- list(
    port = as.integer(port),
    host = as.character(args$host)[1],
    ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  setwd(repo_root)

  disable_ragg <- identical(Sys.getenv("ITCSUITE_DISABLE_RAGG", unset = ""), "1")
  if (isTRUE(disable_ragg)) {
    options(shiny.useragg = FALSE)
    write_log(log_file, "graphics fallback enabled: shiny.useragg=FALSE")
  }

  options(shiny.host = as.character(args$host)[1])
  options(shiny.port = as.integer(port))

  # Emit the READY signal via shiny.launch.browser option so it fires AFTER
  # httpuv has successfully bound to the port, avoiding the race condition.
  ready_emitted <- FALSE
  on_start_fn <- function(url) {
    if (isTRUE(ready_emitted)) return(invisible(NULL))
    ready_emitted <<- TRUE
    cat(sprintf("ITCSUITE_READY %s\n", jsonlite::toJSON(ready_payload, auto_unbox = TRUE)))
    flush.console()
    write_log(log_file, "ready host=", args$host, " port=", port, " url=", url, " (emitted after server bind)")
  }
  options(shiny.launch.browser = on_start_fn)

  tryCatch({
    shiny::runApp(
      appDir = app_path,
      host = as.character(args$host)[1],
      port = as.integer(port)
    )
  }, error = function(e) {
    write_log(log_file, "runApp error: ", conditionMessage(e))
    fail(conditionMessage(e), code = 2L)
  })
}

main()
