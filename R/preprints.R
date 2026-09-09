# preprints(): a medRxiv / bioRxiv backend ---------------------------------
# The bioRxiv/medRxiv API (api.biorxiv.org) has no text search: it serves
# papers by date window only. preprints() scans a window and keeps the
# records whose title/abstract/category match the query words client-side.

.mr_biorxiv <- "https://api.biorxiv.org/details"

#' Search medRxiv / bioRxiv for a topic
#'
#' Europe PMC already indexes many preprints (they arrive as
#' `source = "preprint"` from [pubmed()]); use `preprints()` when you need the
#' most recent ones, before Europe PMC has picked them up. Because the API has
#' no text search, this scans a publication-date window and keeps records whose
#' title, abstract, or category contain the query words.
#'
#' @param query A search string. It is split into words (3+ letters); a record
#'   matches when `match` of them appear in its text.
#' @param server `"medrxiv"` or `"biorxiv"`.
#' @param from,to Publication-date bounds, `"YYYY-MM-DD"`. Default: the last
#'   year.
#' @param n Maximum number of matching records to keep.
#' @param match `"all"` (every query word must appear) or `"any"`.
#' @param max_scan Stop after scanning this many records even if fewer than `n`
#'   matched.
#' @param email Optional contact e-mail (see [medrev_email()]).
#'
#' @return A `medrev` object, ready to combine with a [pubmed()] review via
#'   [c()].
#' @export
#'
#' @examples
#' \dontrun{
#' pp <- preprints("sepsis machine learning", "medrxiv", from = "2025-01-01")
#' r  <- c(pubmed("sepsis machine learning", n = 100), pp)
#' }
preprints <- function(query, server = c("medrxiv", "biorxiv"),
                      from = NULL, to = NULL, n = 200L,
                      match = c("all", "any"), max_scan = 5000L, email = NULL) {
  mr_check_string(query, "query")
  server <- match.arg(server)
  match <- match.arg(match)
  n <- mr_check_count(n, "n")
  max_scan <- mr_check_count(max_scan, "max_scan")
  from <- from %||% format(Sys.Date() - 365, "%Y-%m-%d")
  to <- to %||% format(Sys.Date(), "%Y-%m-%d")
  for (d in c(from, to)) {
    if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", d)) mr_stop("Dates must be {.val YYYY-MM-DD}; got {.val {d}}.")
  }
  tokens <- mr_query_tokens(query)

  cli::cli_progress_step("Scanning {server} {from} to {to}")
  kept <- list()
  scanned <- 0L
  cursor <- 0L
  total <- NA_integer_
  repeat {
    pg <- mr_preprint_page(server, from, to, cursor, email)
    if (is.na(total)) total <- pg$total
    cx <- pg$collection
    if (!length(cx) || !nrow(cx)) break
    scanned <- scanned + nrow(cx)
    hit <- mr_preprint_match(cx, tokens, match)
    if (any(hit)) kept[[length(kept) + 1L]] <- mr_biorxiv_rows(cx[hit, , drop = FALSE], server)
    got <- sum(vapply(kept, nrow, integer(1)))
    cursor <- cursor + nrow(cx)
    if (got >= n || scanned >= max_scan || (!is.na(total) && cursor >= total)) break
  }
  cli::cli_progress_done()

  df <- if (length(kept)) do.call(rbind, kept) else mr_empty_records()
  if (nrow(df)) {
    ord <- order(df$doi, -df$.v)
    df <- df[ord, , drop = FALSE]
    df <- df[!duplicated(df$doi), , drop = FALSE]
    df$.v <- NULL
    if (nrow(df) > n) df <- df[seq_len(n), , drop = FALSE]
    rownames(df) <- NULL
  } else {
    df$.v <- NULL
  }
  cli::cli_alert_info("Kept {nrow(df)} of {scanned} scanned {server} record{?s}.")

  x <- new_medrev(df[, .mr_cols, drop = FALSE])
  x <- mr_log_search(x, sprintf("%s: %s", server, query), server, scanned, nrow(df))
  x <- mr_stage(x, "identified", sprintf("%s %s..%s (%d scanned)", server, from, to, scanned))
  x
}

mr_preprint_page <- function(server, from, to, cursor, email) {
  req <- mr_req(.mr_biorxiv, email = email, rate = 4) |>
    httr2::req_url_path_append(server, from, to, cursor) |>
    httr2::req_error(is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  if (httr2::resp_status(resp) != 200L) {
    return(list(collection = data.frame(), total = 0L))
  }
  j <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  total <- suppressWarnings(as.integer(j$messages$total[[1]] %||% NA))
  list(collection = j$collection %||% data.frame(), total = total)
}

# Words of 3+ letters, lower-cased.
mr_query_tokens <- function(query) {
  w <- tolower(unlist(strsplit(query, "[^A-Za-z0-9]+")))
  unique(w[nchar(w) >= 3])
}

# Logical vector: which rows of `cx` match the query tokens.
mr_preprint_match <- function(cx, tokens, mode) {
  if (!length(tokens)) return(rep(TRUE, nrow(cx)))
  hay <- tolower(paste(cx$title %||% "", cx$abstract %||% "", cx$category %||% ""))
  hits <- vapply(tokens, function(t) grepl(t, hay, fixed = TRUE), logical(nrow(cx)))
  if (is.null(dim(hits))) hits <- matrix(hits, nrow = nrow(cx))
  if (mode == "all") rowSums(hits) == length(tokens) else rowSums(hits) > 0
}

# Map the API collection data.frame to medrev's schema (+ a private .v version).
mr_biorxiv_rows <- function(cx, server) {
  doi <- tolower(cx$doi)
  srv <- tolower(server)
  ver <- suppressWarnings(as.integer(cx$version))
  ver[is.na(ver)] <- 1L
  out <- data.frame(
    id = paste0("doi:", doi),
    source = "preprint",
    pmid = NA_character_,
    pmcid = NA_character_,
    doi = doi,
    title = cx$title,
    abstract = cx$abstract,
    authors = cx$authors,
    journal = paste0(tools::toTitleCase(srv), " (preprint)"),
    year = suppressWarnings(as.integer(substr(cx$date, 1, 4))),
    type = "Preprint",
    keywords = cx$category,
    mesh = NA_character_,
    language = "eng",
    is_oa = TRUE,
    license = cx$license,
    url = paste0("https://doi.org/", doi),
    pdf_url = sprintf("https://www.%s.org/content/%sv%d.full.pdf", srv, doi, ver),
    pdf_source = srv,
    oa_status = "oa",
    decision = NA_character_,
    reason = NA_character_,
    screened_at = NA_character_,
    retrieved_at = as.character(Sys.Date()),
    .v = ver,
    stringsAsFactors = FALSE
  )
  out
}

#' Combine reviews
#'
#' Merges the records, search logs, and stage logs of two or more `medrev`
#' objects, dropping records whose `id` is already present. Use it to bring a
#' [preprints()] search together with a [pubmed()] one.
#'
#' @param ... `medrev` objects.
#'
#' @return A single `medrev` object with a `"combined"` stage logged.
#' @export
#'
#' @examples
#' \dontrun{
#' r <- c(pubmed("crispr base editing", n = 100),
#'        preprints("crispr base editing", "biorxiv"))
#' }
c.medrev <- function(...) {
  xs <- Filter(is_medrev, list(...))
  if (!length(xs)) mr_stop("{.fn c} needs at least one {.cls medrev} object.")
  if (length(xs) == 1L) return(xs[[1]])

  recs <- do.call(rbind, lapply(xs, mr_df))
  recs <- recs[!duplicated(recs$id), , drop = FALSE]
  rownames(recs) <- NULL
  out <- new_medrev(recs)
  out$searches <- do.call(rbind, lapply(xs, function(z) as.data.frame(z$searches)))
  out$stages <- do.call(rbind, lapply(xs, function(z) as.data.frame(z$stages)))
  mr_stage(out, "combined",
           sprintf("%d reviews merged, %d unique records", length(xs), nrow(recs)))
}
