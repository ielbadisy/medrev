# resolve(): attach open-access PDF URLs --------------------------------

#' Resolve records to open-access PDF links
#'
#' For each record without a PDF link yet, tries the requested `sources` and
#' keeps the first hit. Only open-access locations are used; closed-access
#' records keep an empty `pdf_url`. Called automatically by [fetch()]; use it
#' directly to inspect coverage first.
#'
#' Sources:
#' * `"europepmc"` - the Europe PMC full-text PDF for open-access PMC articles
#'   (no network call - the URL is constructed).
#' * `"unpaywall"` - the best open-access PDF Unpaywall knows for the DOI.
#'   Requires a contact e-mail (see [medrev_email()]). Queried for all
#'   candidates at once, `max_active` requests in flight.
#' * `"citation_meta"` - the `citation_pdf_url` tag on the landing page, only
#'   for records already known to be open access.
#'
#' Every link found is written to an on-disk cache (see [medrev_cache_path()]),
#' so re-running `resolve()` on an overlapping review is nearly free.
#'
#' @param x A `medrev` object.
#' @param sources Character vector of sources to try, in order.
#' @param email Contact e-mail for Unpaywall.
#' @param cache Use (and update) the on-disk link cache.
#' @param max_active Maximum concurrent HTTP requests for the Unpaywall and
#'   citation-metadata passes.
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
                    email = NULL, cache = TRUE, max_active = 5L) {
  mr_assert(x)
  sources <- match.arg(sources, several.ok = TRUE)
  max_active <- mr_check_count(max_active, "max_active")
  df <- mr_df(x)
  n <- nrow(df)
  if (!n) return(x)

  unresolved <- function() is.na(df$pdf_url) | !nzchar(df$pdf_url)

  # 1. cache -------------------------------------------------------------
  from_cache <- 0L
  if (isTRUE(cache)) {
    cc <- mr_cache_load()
    if (nrow(cc)) {
      idx <- which(unresolved())
      m <- match(df$id[idx], cc$key)
      hit <- idx[!is.na(m)]
      mm <- m[!is.na(m)]
      df$pdf_url[hit] <- cc$pdf_url[mm]
      df$pdf_source[hit] <- cc$pdf_source[mm]
      df$oa_status[hit] <- cc$oa_status[mm]
      from_cache <- length(hit)
    }
  }

  # 2. europepmc (no HTTP) ---------------------------------------------
  if ("europepmc" %in% sources) {
    for (i in which(unresolved())) {
      h <- mr_res_epmc(df[i, ])
      if (!is.null(h)) {
        df$pdf_url[i] <- h$url; df$pdf_source[i] <- "europepmc"; df$oa_status[i] <- "oa"
      }
    }
  }

  # 3. unpaywall (parallel) ------------------------------------------
  if ("unpaywall" %in% sources) {
    email <- tryCatch(mr_require_email(email, "Unpaywall"),
                      error = function(e) { cli::cli_warn(conditionMessage(e)); NULL })
    cand <- which(unresolved() & !is.na(df$doi) & nzchar(df$doi))
    if (!is.null(email) && length(cand)) {
      cli::cli_progress_step("Unpaywall: {length(cand)} DOI{?s}")
      reqs <- lapply(df$doi[cand], function(d) {
        mr_req(.mr_unpaywall, email = email, rate = 8) |>
          httr2::req_url_path_append(d) |>
          httr2::req_url_query(email = email) |>
          httr2::req_error(is_error = function(resp) FALSE)
      })
      resps <- httr2::req_perform_parallel(reqs, on_error = "continue",
                                           max_active = max_active, progress = FALSE)
      for (k in seq_along(cand)) {
        h <- mr_unpaywall_pick(resps[[k]])
        if (!is.null(h)) {
          i <- cand[[k]]
          df$pdf_url[i] <- h$url; df$pdf_source[i] <- "unpaywall"; df$oa_status[i] <- h$oa_status
        }
      }
      cli::cli_progress_done()
    }
  }

  # 4. citation_meta (parallel HTML) -------------------------------
  if ("citation_meta" %in% sources) {
    cand <- which(unresolved() & (df$is_oa %in% TRUE | df$oa_status %in% "oa") &
                    !is.na(df$url) & grepl("^https?://", df$url))
    if (length(cand)) {
      cli::cli_progress_step("citation_pdf_url: {length(cand)} page{?s}")
      reqs <- lapply(df$url[cand], function(u) {
        httr2::request(u) |>
          httr2::req_user_agent(mr_user_agent(email)) |>
          httr2::req_timeout(30) |>
          httr2::req_error(is_error = function(resp) FALSE)
      })
      resps <- httr2::req_perform_parallel(reqs, on_error = "continue",
                                           max_active = max_active, progress = FALSE)
      for (k in seq_along(cand)) {
        h <- mr_citation_pick(resps[[k]])
        if (!is.null(h)) {
          i <- cand[[k]]
          df$pdf_url[i] <- h$url; df$pdf_source[i] <- "citation_meta"; df$oa_status[i] <- "oa"
        }
      }
      cli::cli_progress_done()
    }
  }

  got <- !is.na(df$pdf_url) & nzchar(df$pdf_url)
  if (isTRUE(cache) && any(got)) {
    mr_cache_save(data.frame(
      key = df$id[got], pdf_url = df$pdf_url[got],
      pdf_source = df$pdf_source[got], oa_status = df$oa_status[got],
      stringsAsFactors = FALSE
    ))
  }
  n_got <- sum(got)
  extra <- if (from_cache) sprintf(" (%d from cache)", from_cache) else ""
  cli::cli_alert_info("PDF link for {n_got} of {n} record{?s}{extra}.")
  x <- mr_set_df(x, df)
  mr_stage(x, "resolved", sprintf("%d/%d with pdf link", n_got, n))
}

# Europe PMC render endpoint for an open-access PMC article.
mr_res_epmc <- function(row) {
  pmcid <- row$pmcid
  if (is.na(pmcid) || !nzchar(pmcid) || !isTRUE(row$is_oa)) return(NULL)
  pmcid <- toupper(pmcid)
  if (!grepl("^PMC", pmcid)) pmcid <- paste0("PMC", pmcid)
  list(url = sprintf("https://europepmc.org/articles/%s?pdf=render", pmcid), oa_status = "oa")
}

# From an Unpaywall response (or a failed-request condition): a PDF link or NULL.
mr_unpaywall_pick <- function(resp) {
  if (!inherits(resp, "httr2_response")) return(NULL)
  if (httr2::resp_status(resp) != 200L) return(NULL)
  out <- tryCatch(httr2::resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(out) || !isTRUE(out$is_oa)) return(NULL)
  url <- out$best_oa_location$url_for_pdf %||% NULL
  if (is.null(url) || is.na(url) || !nzchar(url)) return(NULL)
  list(url = url, oa_status = out$oa_status %||% "oa")
}

# From a landing-page response: the citation_pdf_url meta tag or NULL.
mr_citation_pick <- function(resp) {
  if (!inherits(resp, "httr2_response")) return(NULL)
  if (httr2::resp_status(resp) >= 400L) return(NULL)
  html <- tryCatch(httr2::resp_body_string(resp), error = function(e) NULL)
  if (is.null(html)) return(NULL)
  m <- regmatches(html, regexpr(
    "<meta[^>]+name=[\"']citation_pdf_url[\"'][^>]*>", html, ignore.case = TRUE))
  if (!length(m)) return(NULL)
  cu <- regmatches(m, regexpr("content=[\"'][^\"']+[\"']", m, ignore.case = TRUE))
  if (!length(cu)) return(NULL)
  list(url = gsub("^content=[\"']|[\"']$", "", cu), oa_status = "oa")
}
