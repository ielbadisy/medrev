# prisma(): PRISMA-style flow counts ----------------------------------

#' PRISMA-style flow counts for a review
#'
#' Derives the identification / de-duplication / screening / inclusion counts
#' from the review's stage log and current screening decisions.
#'
#' @param x A `medrev` object.
#'
#' @return A `medrev_prisma` object: a named integer vector of counts with a
#'   print method that lays out the flow.
#' @export
#'
#' @examples
#' \dontrun{
#' r <- pubmed("sepsis biomarkers", n = 200) |> dedup(fuzzy = TRUE)
#' r <- screen(r, exclude = year < 2010, reason = "pre-2010")
#' prisma(r)
#' }
prisma <- function(x) {
  mr_assert(x)
  df <- mr_df(x)
  st <- as.data.frame(x$stages)

  stage_n <- function(name, default) {
    hit <- which(st$stage == name)
    if (length(hit)) st$n[[hit[[length(hit)]]]] else default
  }
  identified <- stage_n("identified", nrow(df))
  after_dedup <- stage_n("deduplicated", identified)
  dup_removed <- max(identified - after_dedup, 0L)
  flagged <- nrow(attr(x, "review_candidates") %||% data.frame())

  excluded <- sum(df$decision %in% "exclude")
  maybe <- sum(df$decision %in% "maybe")
  included <- nrow(df) - excluded - maybe
  with_pdf <- sum(mr_which_rows(df, "included") &
                    !is.na(df$pdf_url) & nzchar(df$pdf_url))

  counts <- c(
    identified = as.integer(identified),
    duplicates_removed = as.integer(dup_removed),
    flagged_for_review = as.integer(flagged),
    after_deduplication = as.integer(after_dedup),
    screened = as.integer(nrow(df)),
    excluded = as.integer(excluded),
    maybe = as.integer(maybe),
    included = as.integer(included),
    included_with_pdf = as.integer(with_pdf)
  )
  structure(counts, class = "medrev_prisma")
}

#' @export
print.medrev_prisma <- function(x, ...) {
  row <- function(label, n) cat(format(label, width = 32), format(n, big.mark = ","), "\n")
  cat("PRISMA flow\n")
  row("records identified", x[["identified"]])
  row("  duplicates removed", -x[["duplicates_removed"]])
  if (x[["flagged_for_review"]] > 0)
    row("  borderline duplicates flagged", x[["flagged_for_review"]])
  row("records after de-duplication", x[["after_deduplication"]])
  row("records screened", x[["screened"]])
  row("  excluded", -x[["excluded"]])
  if (x[["maybe"]] > 0)
    row("  undecided (maybe)", x[["maybe"]])
  row("studies included", x[["included"]])
  row("  with an open-access PDF", x[["included_with_pdf"]])
  invisible(x)
}

#' @export
as.data.frame.medrev_prisma <- function(x, ...) {
  data.frame(step = names(x), n = as.integer(x), row.names = NULL)
}
