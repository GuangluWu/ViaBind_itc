itcsuite_repo_root <- function() {
  env_root <- Sys.getenv("ITCSUITE_REPO_ROOT", unset = "")
  if (nzchar(env_root) &&
      dir.exists(file.path(env_root, "ITCprocessor")) &&
      dir.exists(file.path(env_root, "ITCSuiteWeb"))) {
    return(normalizePath(env_root, winslash = "/", mustWork = TRUE))
  }

  candidates <- unique(c(
    normalizePath(getwd(), winslash = "/", mustWork = TRUE),
    normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(getwd(), "..", ".."), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(getwd(), "..", "..", ".."), winslash = "/", mustWork = FALSE)
  ))
  for (d in candidates) {
    if (!dir.exists(d)) next
    if (dir.exists(file.path(d, "ITCprocessor")) &&
        dir.exists(file.path(d, "ITCSuiteWeb")) &&
        dir.exists(file.path(d, "ITCsimfit")) &&
        dir.exists(file.path(d, "ITCgraph"))) {
      return(normalizePath(d, winslash = "/", mustWork = TRUE))
    }
  }
  stop("Unable to detect ITCSuite repo root for tests.")
}

