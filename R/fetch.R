# fetch(): download the open-access PDFs -------------------------------

#' Download the open-access PDFs for a review
#'
#' Resolves PDF links first if none are present (via [resolve()]), then
#' downloads one file per selected record. A response that is not actually a
#' PDF (an HTML paywall or error page) is rejected and logged as a failure,
#' not saved.
#'
#' @param x A `medrev` object.
#' @param dir Directory to write PDFs into; created if needed.
#' @param which Which records to fetch: `"included"` (screened-in, or all when
#'   nothing has been screened), `"oa"` (those plus a resolved OA link), or
#'   `"all"`.
#' @param name File-name template; `{field}` placeholders are filled from the
#'   record (`{pmid}`, `{doi}`, `{year}`, `{id}`, ...). `.pdf` is appended.
#' @param overwrite Re-download files that already exist.
#' @param limit Optional cap on the number of records to download; the first
#'   `limit` selected records (those with a resolved link first) are kept.
#' @param delay Seconds to pause between downloads.
#' @param errors Optional path to write a TSV of records that could not be
#'   fetched (no link or download failed).
#' @param sources,email Passed to [resolve()] when links must be resolved.
#'
#' @return A `basetable` download log with one row per selected record:
#'   `id`, `pmid`, `doi`, `pdf_source`, `oa_status`, `pdf_url`, `file`,
#'   `status` (`"downloaded"` / `"exists"` / `"no_link"` / `"failed"`),
#'   `bytes`. The resolved review is attached as `attr(log, "review")`.
#' @export
#'
#' @examples
#' \dontrun{
#' r <- pubmed("crispr off-target", n = 40, years = 2019)
#' log <- fetch(r, dir = "crispr_pdfs", name = "{year}_{pmid}")
#' log
#' }
fetch <- function(x, dir = "medrev_pdfs", which = c("included", "oa", "all"),
                  name = "{pmid}", overwrite = FALSE, limit = NULL, delay = 0.5,
                  errors = NULL,
                  sources = c("europepmc", "unpaywall", "citation_meta"),
                  email = NULL) {
  mr_assert(x)
  which <- match.arg(which)
  if (!is.null(limit)) limit <- mr_check_count(limit, "limit", min = 0L)
  df <- mr_df(x)

  if (!any(!is.na(df$pdf_url) & nzchar(df$pdf_url))) {
    x <- resolve(x, sources = sources, email = email)
    df <- mr_df(x)
  }

  sel <- which(mr_which_rows(df, which))
  if (!length(sel)) {
    cli::cli_alert_warning("No records match {.arg which = \"{which}\"}.")
    return(mr_download_log(df[0, ]))
  }
  if (!is.null(limit) && length(sel) > limit) {
    has_link <- !is.na(df$pdf_url[sel]) & nzchar(df$pdf_url[sel])
    sel <- utils::head(c(sel[has_link], sel[!has_link]), limit)
    cli::cli_alert_info("Limiting to {limit} of the selected records.")
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  rows <- vector("list", length(sel))
  cli::cli_progress_bar("Downloading", total = length(sel))
  for (k in seq_along(sel)) {
    i <- sel[[k]]
    cli::cli_progress_update()
    rec <- as.list(df[i, ])
    url <- rec$pdf_url
    file <- file.path(dir, paste0(mr_render_name(name, rec), ".pdf"))
    status <- NA_character_; bytes <- NA_integer_

    if (is.na(url) || !nzchar(url)) {
      status <- "no_link"
    } else if (file.exists(file) && !overwrite) {
      status <- "exists"; bytes <- as.integer(file.info(file)$size)
    } else {
      ok <- tryCatch({
        req <- httr2::request(url) |>
          httr2::req_user_agent(mr_user_agent(email)) |>
          httr2::req_timeout(120) |>
          httr2::req_retry(max_tries = 3,
                           is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)) |>
          httr2::req_error(is_error = function(resp) FALSE)
        resp <- httr2::req_perform(req)
        if (httr2::resp_status(resp) >= 400L || !mr_is_pdf(resp)) FALSE
        else {
          writeBin(httr2::resp_body_raw(resp), file)
          TRUE
        }
      }, error = function(e) FALSE)
      if (ok) {
        status <- "downloaded"; bytes <- as.integer(file.info(file)$size)
      } else {
        status <- "failed"
        if (file.exists(file) && is.na(bytes)) unlink(file)
      }
    }
    rows[[k]] <- data.frame(
      id = rec$id, pmid = rec$pmid, doi = rec$doi, pdf_source = rec$pdf_source,
      oa_status = rec$oa_status, pdf_url = url,
      file = if (identical(status, "downloaded") || identical(status, "exists")) file else NA_character_,
      status = status, bytes = bytes, stringsAsFactors = FALSE
    )
    if (identical(status, "downloaded") && delay > 0) Sys.sleep(delay)
  }
  cli::cli_progress_done()

  log <- do.call(rbind, rows)
  n_ok <- sum(log$status %in% c("downloaded", "exists"))
  cli::cli_alert_success("{n_ok} of {nrow(log)} PDF{?s} available in {.path {dir}}.")

  if (!is.null(errors)) {
    bad <- log[log$status %in% c("no_link", "failed"), c("id", "pmid", "doi", "status", "pdf_url")]
    utils::write.table(bad, errors, sep = "\t", row.names = FALSE, quote = FALSE)
    if (nrow(bad)) cli::cli_alert_info("{nrow(bad)} unfetched record{?s} written to {.path {errors}}.")
  }

  x <- mr_stage(x, "fetched", sprintf("%d/%d pdf downloaded", n_ok, nrow(log)))
  out <- mr_download_log(log)
  attr(out, "review") <- x
  out
}

mr_download_log <- function(log) {
  cols <- c("id", "pmid", "doi", "pdf_source", "oa_status", "pdf_url",
            "file", "status", "bytes")
  if (!nrow(log)) {
    log <- data.frame(
      id = character(), pmid = character(), doi = character(),
      pdf_source = character(), oa_status = character(), pdf_url = character(),
      file = character(), status = character(), bytes = integer(),
      stringsAsFactors = FALSE
    )
  }
  basetable::as_basetable(log[, cols, drop = FALSE])
}
