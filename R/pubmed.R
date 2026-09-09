# pubmed(): build a review from a query or a list of identifiers -----------

#' Start a review from a research question or a list of identifiers
#'
#' Searches the biomedical literature through Europe PubMed Central, which
#' indexes all of PubMed and MEDLINE plus extra open-access content, and
#' returns a `medrev` object holding a tidy record table.
#'
#' Supply either `query` (a topic, question, or Europe PMC field expression) or
#' `ids` (PubMed IDs / DOIs), not both.
#'
#' @param query A search string. Plain text works; Europe PMC syntax is also
#'   accepted, e.g. `"(diabetic retinopathy) AND (screening) AND SRC:MED"`.
#' @param ids A character vector of PMIDs or DOIs, or a path to a file with one
#'   identifier per line. Used instead of `query`.
#' @param n Maximum number of records to retrieve.
#' @param years Optional publication-year filter: a single lower bound, or a
#'   length-2 `c(from, to)`.
#' @param sort `"relevance"` (default) or `"date"` (newest first).
#'
#' @return A `medrev` object. Its record table (`as.data.frame(x)`) has one
#'   row per hit with columns `id`, `source`, `pmid`, `pmcid`, `doi`, `title`,
#'   `abstract`, `authors`, `journal`, `year`, `type`, `keywords`, `mesh`,
#'   `language`, `is_oa`, `license`, `url`, and empty `pdf_url` / `oa_status` /
#'   `decision` columns filled by later steps.
#' @export
#'
#' @examples
#' \dontrun{
#' r <- pubmed("machine learning for sepsis prediction", n = 200, years = 2018)
#' r
#' r <- pubmed(ids = c("31452104", "10.1038/s41586-020-2649-2"))
#' }
pubmed <- function(query = NULL, ids = NULL, n = 100L, years = NULL,
                   sort = c("relevance", "date")) {
  sort <- match.arg(sort)
  n <- mr_check_count(n, "n")
  if (is.null(query) && is.null(ids)) mr_stop("Supply either {.arg query} or {.arg ids}.")
  if (!is.null(query) && !is.null(ids)) mr_stop("Supply {.arg query} or {.arg ids}, not both.")

  if (!is.null(ids)) return(mr_from_ids(ids, n = n))

  mr_check_string(query, "query")
  full <- query
  if (!is.null(years)) {
    yr <- as.integer(years)
    lo <- yr[[1]]
    hi <- if (length(yr) >= 2L) yr[[2]] else as.integer(format(Sys.Date(), "%Y"))
    full <- sprintf("(%s) AND (PUB_YEAR:[%d TO %d])", query, lo, hi)
  }
  sort_clause <- if (sort == "date") "P_PDATE_D desc" else ""

  cli::cli_progress_step("Searching Europe PMC")
  df <- mr_epmc_search(full, n = n, sort = sort_clause)
  hits <- attr(df, "hits")
  cli::cli_progress_done()
  cli::cli_alert_info("Retrieved {nrow(df)} of {hits %||% NA} matching record{?s}.")

  x <- new_medrev(df)
  x <- mr_log_search(x, query, "europepmc", hits %||% NA_integer_, nrow(df))
  x <- mr_stage(x, "identified", sprintf("Europe PMC: %s", query))
  x
}

mr_from_ids <- function(ids, n = 100L) {
  ids <- mr_as_ids(ids)
  if (!length(ids)) mr_stop("No identifiers supplied.")
  is_doi <- grepl("^10\\.", ids)
  terms <- ifelse(is_doi, sprintf('DOI:"%s"', tolower(ids)), sprintf("EXT_ID:%s", ids))

  cli::cli_progress_step("Looking up {length(ids)} identifier{?s} in Europe PMC")
  chunks <- split(terms, ceiling(seq_along(terms) / 40L))
  parts <- lapply(chunks, function(tt) {
    mr_epmc_search(paste(tt, collapse = " OR "), n = max(length(tt) * 2L, n))
  })
  df <- do.call(rbind, parts)
  df <- df[!duplicated(df$id), , drop = FALSE]
  rownames(df) <- NULL
  cli::cli_progress_done()

  found <- c(df$pmid[!is.na(df$pmid)], tolower(df$doi[!is.na(df$doi)]))
  missing <- ids[!(ids %in% found | tolower(ids) %in% found)]
  if (length(missing)) cli::cli_alert_warning("{length(missing)} identifier{?s} not found in Europe PMC.")

  x <- new_medrev(df)
  x <- mr_log_search(x, sprintf("%d supplied ids", length(ids)), "europepmc",
                     nrow(df), nrow(df))
  x <- mr_stage(x, "identified", sprintf("id lookup: %d supplied, %d resolved", length(ids), nrow(df)))
  attr(x, "unresolved") <- missing
  x
}
