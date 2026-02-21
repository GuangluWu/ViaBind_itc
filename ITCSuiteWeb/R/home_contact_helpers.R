`%||%` <- function(x, y) if (is.null(x)) y else x

# [COMMENT_STD][MODULE_HEADER]
# Module role: helpers for Home contact section (developer links, QR selection, donation URL).
# Dependencies: base R only.
# Public API: home_contact_* functions.
# Side effects: reads local filesystem when resolving QR assets.
# Change log: 2026-02-21 - add Home contact and donate helper utilities.

home_contact_scalar_chr <- function(x, default = "") {
  out <- as.character(x %||% "")[1]
  out <- trimws(out)
  if (nzchar(out)) out else default
}

home_contact_normalize_lang <- function(lang) {
  out <- tolower(home_contact_scalar_chr(lang, default = "en"))
  if (identical(out, "zh")) "zh" else "en"
}

home_contact_qr_filename <- function(lang = "en") {
  if (identical(home_contact_normalize_lang(lang), "zh")) "bmc_qr_zh.png" else "bmc_qr_en.png"
}

home_contact_join_web_path <- function(prefix = "/assets", file_name = "") {
  clean_prefix <- home_contact_scalar_chr(prefix, default = "/assets")
  clean_prefix <- sub("/+$", "", clean_prefix)
  clean_file <- home_contact_scalar_chr(file_name, default = "")
  if (!nzchar(clean_file)) return("")
  paste0(clean_prefix, "/", clean_file)
}

home_contact_validate_https_url <- function(url = "") {
  out <- home_contact_scalar_chr(url, default = "")
  if (!nzchar(out)) return("")
  if (!grepl("^https://", tolower(out))) return("")
  out
}

home_contact_mailto_href <- function(email = "") {
  clean <- home_contact_scalar_chr(email, default = "")
  if (!nzchar(clean)) return("")
  paste0("mailto:", clean)
}

home_contact_resolve_qr_src <- function(
  lang = "en",
  assets_dir = file.path(getwd(), "www", "assets"),
  resource_prefix = "/assets"
) {
  lang_norm <- home_contact_normalize_lang(lang)
  primary_name <- home_contact_qr_filename(lang_norm)
  fallback_name <- home_contact_qr_filename("en")

  primary_path <- file.path(assets_dir, primary_name)
  fallback_path <- file.path(assets_dir, fallback_name)

  if (file.exists(primary_path)) {
    return(list(
      src = home_contact_join_web_path(resource_prefix, primary_name),
      file_name = primary_name,
      used_lang = lang_norm,
      fallback_to_en = FALSE,
      exists = TRUE
    ))
  }

  if (!identical(primary_name, fallback_name) && file.exists(fallback_path)) {
    return(list(
      src = home_contact_join_web_path(resource_prefix, fallback_name),
      file_name = fallback_name,
      used_lang = "en",
      fallback_to_en = TRUE,
      exists = TRUE
    ))
  }

  list(
    src = "",
    file_name = "",
    used_lang = lang_norm,
    fallback_to_en = !identical(lang_norm, "en"),
    exists = FALSE
  )
}
