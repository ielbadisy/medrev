# resolve(): attach open-access PDF URLs --------------------------------

#' Resolve records to open-access PDF links
#'
#' For each record without a PDF link yet, tries the requested `sources` in
#' order and stops at the first hit. Only open-access locations are used;
#' closed-access records keep an empty `pdf_url`. This is called automatically
#' by [fetch()]; use it directly to inspect coverage first.
#'
#' Sources:
#' * `"europepmc"` - the Europe PMC full-text PDF for open-access PMC articles.
#' * `"unpaywall"` - the best open-access PDF Unpaywall knows for the DOI,
#'   anywhere on the web. Requires a contact e-mail (see [medrev_email()]).
#' * `"citation_meta"` - the `citation_pdf_url` tag on the article's landing
#'   page, used only when the record is already known to be open access.
#'
#' @param x A `medrev` object.
#' @param sources Character vector of sources to try, in order.
#' @param email Contact e-mail for Unpaywall.
#'
#' @return The `medrev` object with `pdf_url`, `pdf_source`, and `oa_status`
#'   filled where a link was found, and a `"resolved"` stage logged.
#' @export
#'
#' @examples
#' \dontrun{
#' r <- pubmed("diabetic retinopathy screening", n = 50) |> resolve()
#' table(as.data.frame(r)$pdf_source, useNA = "ifany")
#' }
resolve <- function(x, sources = c("europepmc", "unpaywall", "citation_meta"),
                    email = NULL) {
  mr_assert(x)
  sources <- match.arg(sources, several.ok = TRUE)
  df <- mr_df(x)
  n <- nrow(df)
  if (!n) return(x)

  need_email <- "unpaywall" %in% sources
  email <- if (need_email) tryCatch(mr_require_email(email, "Unpaywall"),
                                    error = function(e) { cli::cli_warn(conditionMessage(e)); NULL }) else NULL
  use_unpaywall <- "unpaywall" %in% sources && !is.null(email)

  cli::cli_progress_bar("Resolving PDFs", total = n)
  for (i in seq_len(n)) {
    cli::cli_progress_update()
    if (!is.na(df$pdf_url[[i]]) && nzchar(df$pdf_url[[i]])) next
    for (src in sources) {
      hit <- switch(src,
        europepmc = mr_res_epmc(df[i, ]),
        unpaywall = if (use_unpaywall) mr_res_unpaywall(df$doi[[i]], email) else NULL,
        citation_meta = if (isTRUE(df$is_oa[[i]]) || identical(df$oa_status[[i]], "oa"))
          mr_res_citation(df$url[[i]]) else NULL
      )
      if (!is.null(hit)) {
        df$pdf_url[[i]] <- hit$url
        df$pdf_source[[i]] <- src
        df$oa_status[[i]] <- hit$oa_status %||% "oa"
        break
      }
    }
  }
  cli::cli_progress_done()

  got <- sum(!is.na(df$pdf_url) & nzchar(df$pdf_url))
  cli::cli_alert_info("PDF link for {got} of {n} record{?s}.")
  x <- mr_set_df(x, df)
  mr_stage(x, "resolved", sprintf("%d/%d with pdf link", got, n))
}

# Europe PMC render endpoint for an open-access PMC article.
mr_res_epmc <- function(row) {
  pmcid <- row$pmcid
  if (is.na(pmcid) || !nzchar(pmcid)) return(NULL)
  if (!isTRUE(row$is_oa)) return(NULL)
  pmcid <- toupper(pmcid)
  if (!grepl("^PMC", pmcid)) pmcid <- paste0("PMC", pmcid)
  list(url = sprintf("https://europepmc.org/articles/%s?pdf=render", pmcid), oa_status = "oa")
}

mr_res_unpaywall <- function(doi, email) {
  if (is.na(doi) || !nzchar(doi)) return(NULL)
  out <- tryCatch({
    req <- mr_req(.mr_unpaywall, email = email, rate = 8) |>
      httr2::req_url_path_append(doi) |>
      httr2::req_url_query(email = email) |>
      httr2::req_error(is_error = function(resp) FALSE)
    resp <- httr2::req_perform(req)
    if (httr2::resp_status(resp) != 200L) return(NULL)
    httr2::resp_body_json(resp, simplifyVector = TRUE)
  }, error = function(e) NULL)
  if (is.null(out) || !isTRUE(out$is_oa)) return(NULL)
  loc <- out$best_oa_location
  url <- loc$url_for_pdf %||% NULL
  if (is.null(url) || is.na(url) || !nzchar(url)) return(NULL)
  list(url = url, oa_status = out$oa_status %||% "oa")
}

# Parse <meta name="citation_pdf_url" content="..."> from a landing page.
mr_res_citation <- function(url) {
  if (is.na(url) || !nzchar(url) || !grepl("^https?://", url)) return(NULL)
  html <- tryCatch({
    req <- httr2::request(url) |>
      httr2::req_user_agent(mr_user_agent()) |>
      httr2::req_timeout(30) |>
      httr2::req_error(is_error = function(resp) FALSE)
    resp <- httr2::req_perform(req)
    if (httr2::resp_status(resp) >= 400L) return(NULL)
    httr2::resp_body_string(resp)
  }, error = function(e) NULL)
  if (is.null(html)) return(NULL)
  m <- regmatches(html, regexpr(
    "<meta[^>]+name=[\"']citation_pdf_url[\"'][^>]*>", html, ignore.case = TRUE))
  if (!length(m)) return(NULL)
  cu <- regmatches(m, regexpr("content=[\"'][^\"']+[\"']", m, ignore.case = TRUE))
  if (!length(cu)) return(NULL)
  list(url = gsub("^content=[\"']|[\"']$", "", cu), oa_status = "oa")
}
