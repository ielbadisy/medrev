# Descriptive counts over a review's records ---------------------------

#' Publications per year
#'
#' @param x A `medrev` object.
#' @param cumulative Return a running total instead of per-year counts.
#' @param which Which records to count: `"all"` or `"included"` (see [fetch()]).
#'
#' @return A `basetable` with columns `year` and `n`, covering every year from
#'   the earliest to the latest record (gap years included with `n = 0`).
#' @export
#'
#' @examples
#' \dontrun{
#' year_counts(pubmed("crispr screening", n = 200))
#' }
year_counts <- function(x, cumulative = FALSE, which = c("all", "included")) {
  mr_assert(x)
  which <- match.arg(which)
  df <- mr_df(x)
  df <- df[mr_which_rows(df, which), , drop = FALSE]
  yr <- suppressWarnings(as.integer(df$year))
  yr <- yr[!is.na(yr)]
  if (!length(yr)) {
    return(basetable::as_basetable(data.frame(year = integer(), n = integer())))
  }
  rng <- seq.int(min(yr), max(yr))
  tab <- tabulate(match(yr, rng), nbins = length(rng))
  if (cumulative) tab <- cumsum(tab)
  basetable::as_basetable(data.frame(year = rng, n = as.integer(tab)))
}

#' Keyword or MeSH term frequencies
#'
#' Splits the semicolon-separated `keywords` or `mesh` column and counts terms.
#'
#' @param x A `medrev` object.
#' @param field `"keywords"` or `"mesh"`.
#' @param min_n Drop terms occurring fewer than `min_n` times.
#' @param top Keep only the `top` most frequent terms (after `min_n`).
#' @param which Which records to count: `"all"` or `"included"`.
#'
#' @return A `basetable` with columns `term` and `n`, most frequent first.
#' @export
#'
#' @examples
#' \dontrun{
#' term_counts(pubmed("sepsis biomarkers", n = 200), "mesh", top = 20)
#' }
term_counts <- function(x, field = c("keywords", "mesh"), min_n = 1L,
                        top = NULL, which = c("all", "included")) {
  mr_assert(x)
  field <- match.arg(field)
  which <- match.arg(which)
  df <- mr_df(x)
  df <- df[mr_which_rows(df, which), , drop = FALSE]
  vals <- df[[field]]
  vals <- vals[!is.na(vals) & nzchar(vals)]
  terms <- trimws(unlist(strsplit(vals, "\\s*;\\s*")))
  terms <- terms[nzchar(terms)]
  if (!length(terms)) {
    return(basetable::as_basetable(data.frame(term = character(), n = integer())))
  }
  tt <- sort(table(terms), decreasing = TRUE)
  out <- data.frame(term = names(tt), n = as.integer(tt), stringsAsFactors = FALSE)
  out <- out[out$n >= min_n, , drop = FALSE]
  if (!is.null(top)) out <- utils::head(out, mr_check_count(top, "top"))
  rownames(out) <- NULL
  basetable::as_basetable(out)
}
