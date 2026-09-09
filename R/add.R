# add(): append a second search to a review ---------------------------

#' Add another search to a review
#'
#' Runs a further query (or identifier lookup) and merges the new records into
#' `x`. Records whose `id` is already present are not added again; run
#' [dedup()] afterwards for title-level de-duplication across the combined set.
#' Each call is appended to the review's search log (see [search_log()]).
#'
#' @param x A `medrev` object.
#' @param query,ids,n,years,sort As in [pubmed()].
#'
#' @return `x` with the new records merged in and an `"added"` stage logged.
#' @export
#'
#' @examples
#' \dontrun{
#' r <- pubmed("sepsis prognosis machine learning", n = 100)
#' r <- add(r, "sepsis mortality prediction deep learning", n = 100)
#' search_log(r)
#' }
add <- function(x, query = NULL, ids = NULL, n = 100L, years = NULL,
                sort = c("relevance", "date")) {
  mr_assert(x)
  sort <- match.arg(sort)
  res <- mr_run_search(query, ids, n, years, sort)

  old <- mr_df(x)
  fresh <- res$df[!res$df$id %in% old$id, , drop = FALSE]
  merged <- rbind(old, fresh[, names(old), drop = FALSE])

  x <- mr_set_df(x, merged)
  x <- mr_log_search(x, res$label, res$source, res$hits, nrow(res$df))
  x <- mr_stage(x, "added",
                sprintf("+%d new, %d already present", nrow(fresh), nrow(res$df) - nrow(fresh)))
  if (length(res$unresolved)) attr(x, "unresolved") <- res$unresolved
  cli::cli_alert_info("Added {nrow(fresh)} new record{?s}; {nrow(res$df) - nrow(fresh)} already in the review.")
  x
}

#' The search log of a review
#'
#' One row per [pubmed()] / [add()] call: the query text, source, total hit
#' count, number of records retrieved, and time.
#'
#' @param x A `medrev` object.
#' @return A `basetable`.
#' @export
#'
#' @examples
#' \dontrun{
#' search_log(pubmed("crispr screen", n = 20))
#' }
search_log <- function(x) {
  mr_assert(x)
  basetable::as_basetable(as.data.frame(x$searches))
}
