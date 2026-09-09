# On-disk cache for resolved PDF links -------------------------------
# Keyed by record id (doi:/pmid:/pmcid:/title:). Only hits are cached, so a
# record that is not open access yet is re-tried on the next run; a record
# that resolved once is never re-resolved.

mr_cache_dir <- function() {
  getOption("medrev.cache_dir", tools::R_user_dir("medrev", "cache"))
}

mr_cache_file <- function() file.path(mr_cache_dir(), "resolve.rds")

mr_cache_empty <- function() {
  data.frame(key = character(), pdf_url = character(), pdf_source = character(),
             oa_status = character(), stringsAsFactors = FALSE)
}

mr_cache_load <- function() {
  f <- mr_cache_file()
  if (!file.exists(f)) return(mr_cache_empty())
  out <- tryCatch(readRDS(f), error = function(e) NULL)
  if (!is.data.frame(out) || !all(c("key", "pdf_url") %in% names(out))) mr_cache_empty() else out
}

mr_cache_save <- function(new_rows) {
  if (!nrow(new_rows)) return(invisible(FALSE))
  cc <- rbind(mr_cache_load(), new_rows[, c("key", "pdf_url", "pdf_source", "oa_status")])
  cc <- cc[!duplicated(cc$key, fromLast = TRUE), , drop = FALSE]
  d <- mr_cache_dir()
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  tryCatch({ saveRDS(cc, mr_cache_file()); invisible(TRUE) },
           error = function(e) { cli::cli_warn("Could not write resolve cache: {conditionMessage(e)}"); invisible(FALSE) })
}

#' The resolve cache
#'
#' [resolve()] caches every PDF link it finds to disk, keyed by record id, so
#' re-running a review does not re-query Europe PMC / Unpaywall for links it
#' already has. `medrev_cache_path()` returns the cache file location;
#' `medrev_cache_clear()` deletes it.
#'
#' The location is `getOption("medrev.cache_dir")` or, by default,
#' `tools::R_user_dir("medrev", "cache")`.
#'
#' @return `medrev_cache_path()`: a path string. `medrev_cache_clear()`:
#'   `TRUE` if a cache file was removed, invisibly.
#' @export
#' @name medrev_cache
#'
#' @examples
#' medrev_cache_path()
medrev_cache_path <- function() mr_cache_file()

#' @rdname medrev_cache
#' @export
medrev_cache_clear <- function() {
  f <- mr_cache_file()
  if (file.exists(f)) {
    unlink(f)
    cli::cli_alert_success("Removed resolve cache at {.path {f}}.")
    invisible(TRUE)
  } else {
    cli::cli_alert_info("No resolve cache to remove.")
    invisible(FALSE)
  }
}
