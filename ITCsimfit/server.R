# ==============================================================================
# server.R - Thin server entrypoint
# ==============================================================================
# Keep this file lightweight. The full server implementation lives in
# R/server/server_main.R.

server_impl_candidates <- unique(c(
  file.path("R", "server", "server_main.R"),
  file.path(getwd(), "R", "server", "server_main.R"),
  file.path(getwd(), "ITCsimfit", "R", "server", "server_main.R")
))

server_impl_hits <- server_impl_candidates[file.exists(server_impl_candidates)]
if (length(server_impl_hits) < 1) {
  stop("Cannot find server implementation file: R/server/server_main.R")
}

itcsimfit_server_impl_path <- normalizePath(server_impl_hits[[1]], winslash = "/", mustWork = TRUE)
itcsimfit_server_dir <- dirname(itcsimfit_server_impl_path)
itcsimfit_app_dir <- normalizePath(dirname(dirname(itcsimfit_server_dir)), winslash = "/", mustWork = TRUE)

assign("ITCSIMFIT_SERVER_IMPL_PATH", itcsimfit_server_impl_path, envir = .GlobalEnv)
assign("ITCSIMFIT_SERVER_DIR", itcsimfit_server_dir, envir = .GlobalEnv)
assign("ITCSIMFIT_APP_DIR", itcsimfit_app_dir, envir = .GlobalEnv)

source(server_impl_hits[[1]], local = FALSE)

if (!exists("server", mode = "function", inherits = FALSE)) {
  stop("Server implementation did not define `server`.")
}
