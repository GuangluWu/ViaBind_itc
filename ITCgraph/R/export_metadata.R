# ==============================================================================
# export_metadata.R - Step3 export metadata helpers
# ==============================================================================

graph_meta_scalar_chr <- function(x, default = "") {
  out <- as.character(x)[1]
  out <- trimws(out)
  if (!is.na(out) && nzchar(out)) out else default
}

graph_meta_get_api <- function(session = NULL) {
  api <- tryCatch({
    if (is.null(session) || is.null(session$userData)) return(NULL)
    session$userData$itcsuite_app_meta
  }, error = function(e) NULL)
  if (is.list(api)) api else NULL
}

graph_meta_resolve_app_version <- function(session = NULL, fallback = "0.0.0-dev") {
  fallback_chr <- graph_meta_scalar_chr(fallback, default = "0.0.0-dev")
  if (!nzchar(fallback_chr)) fallback_chr <- "0.0.0-dev"

  api <- graph_meta_get_api(session)
  from_api <- ""
  if (!is.null(api) && is.function(api$get_app_version)) {
    from_api <- tryCatch(graph_meta_scalar_chr(api$get_app_version(), default = ""), error = function(e) "")
  }
  if (nzchar(from_api)) return(from_api)

  from_env <- graph_meta_scalar_chr(Sys.getenv("ITCSUITE_APP_VERSION", unset = ""), default = "")
  if (nzchar(from_env)) return(from_env)

  fallback_chr
}

graph_meta_resolve_developer_profile <- function(
  session = NULL,
  fallback = list(name = "", email = "", website = "")
) {
  out <- list(name = "", email = "", website = "")
  for (nm in names(out)) {
    out[[nm]] <- graph_meta_scalar_chr(fallback[[nm]], default = "")
  }

  api <- graph_meta_get_api(session)
  if (is.null(api) || !is.function(api$get_developer_profile)) return(out)

  from_api <- tryCatch(api$get_developer_profile(), error = function(e) NULL)
  if (!is.list(from_api)) return(out)

  for (nm in names(out)) {
    val <- graph_meta_scalar_chr(from_api[[nm]], default = "")
    if (nzchar(val)) out[[nm]] <- val
  }
  out
}

graph_meta_build_signature <- function(module_name = "ITCgraph", version = "0.0.0-dev") {
  module_chr <- graph_meta_scalar_chr(module_name, default = "ITCgraph")
  version_chr <- graph_meta_scalar_chr(version, default = "0.0.0-dev")
  sprintf("ViaBind v%s: %s", version_chr, module_chr)
}

graph_meta_build_pdf_author <- function(developer_profile = list(), fallback_author = "ViaBind") {
  name <- graph_meta_scalar_chr(developer_profile$name, default = "")
  email <- graph_meta_scalar_chr(developer_profile$email, default = "")
  website <- graph_meta_scalar_chr(developer_profile$website, default = "")

  primary <- ""
  if (nzchar(name) && nzchar(email)) {
    primary <- sprintf("%s <%s>", name, email)
  } else if (nzchar(name)) {
    primary <- name
  } else if (nzchar(email)) {
    primary <- sprintf("<%s>", email)
  }

  author <- primary
  if (nzchar(website)) {
    author <- if (nzchar(author)) paste0(author, " | ", website) else website
  }
  if (!nzchar(author)) author <- graph_meta_scalar_chr(fallback_author, default = "ViaBind")
  author
}

graph_meta_resolve_pdf_metadata <- function(
  session = NULL,
  module_name = "ITCgraph",
  fallback_version = "0.0.0-dev",
  fallback_profile = list(name = "", email = "", website = ""),
  fallback_author = "ViaBind"
) {
  version <- graph_meta_resolve_app_version(session = session, fallback = fallback_version)
  profile <- graph_meta_resolve_developer_profile(session = session, fallback = fallback_profile)
  list(
    title = graph_meta_build_signature(module_name = module_name, version = version),
    author = graph_meta_build_pdf_author(developer_profile = profile, fallback_author = fallback_author),
    version = version,
    developer_profile = profile
  )
}
