# The medrev object --------------------------------------------------------
#
# A single S3 carrier for a review. `$records` is the record table (a
# basetable); `$searches` logs each query; `$stages` records the row count
# after each pipeline step and drives `prisma()`.

new_medrev <- function(records, searches = NULL, stages = NULL) {
  if (is.data.frame(records)) records <- as.data.frame(records)
  records <- records[, .mr_cols, drop = FALSE]
  structure(
    list(
      records = basetable::as_basetable(records),
      searches = searches %||% mr_empty_searches(),
      stages = stages %||% mr_empty_stages(),
      meta = list(created = mr_now(), version = as.character(utils::packageVersion("medrev")))
    ),
    class = "medrev"
  )
}

mr_empty_searches <- function() {
  data.frame(query = character(), source = character(), hits = integer(),
             retrieved = integer(), time = character(), stringsAsFactors = FALSE)
}

mr_empty_stages <- function() {
  data.frame(stage = character(), n = integer(), detail = character(),
             time = character(), stringsAsFactors = FALSE)
}

# Record a pipeline stage (row = current record count).
mr_stage <- function(x, stage, detail = NA_character_) {
  x$stages <- rbind(as.data.frame(x$stages), data.frame(
    stage = stage, n = nrow(x$records), detail = detail, time = mr_now(),
    stringsAsFactors = FALSE
  ))
  x
}

mr_log_search <- function(x, query, source, hits, retrieved) {
  x$searches <- rbind(as.data.frame(x$searches), data.frame(
    query = query, source = source, hits = as.integer(hits),
    retrieved = as.integer(retrieved), time = mr_now(), stringsAsFactors = FALSE
  ))
  x
}

# The record table as a plain data.frame, for internal row-wise work.
mr_df <- function(x) as.data.frame(x$records)

# Store a data.frame back as the record table, keeping schema + class.
mr_set_df <- function(x, df) {
  x$records <- basetable::as_basetable(df[, .mr_cols, drop = FALSE])
  x
}

#' Is `x` a medrev object?
#'
#' @param x An object.
#' @return A logical scalar.
#' @export
is_medrev <- function(x) inherits(x, "medrev")

mr_assert <- function(x) {
  if (!is_medrev(x)) mr_stop("{.arg x} must be a {.cls medrev} object (from {.fn pubmed}).")
  invisible(x)
}

# Row mask for a `which` selector used by fetch()/bibliography().
# "included" = every record unless screening has explicitly excluded it (or
# marked it "maybe"); with no screening yet, that is all records.
mr_which_rows <- function(df, which = c("all", "included", "oa")) {
  which <- match.arg(which)
  screened <- any(!is.na(df$decision))
  inc <- if (screened) !(df$decision %in% c("exclude", "maybe")) else rep(TRUE, nrow(df))
  switch(which,
    all = rep(TRUE, nrow(df)),
    included = inc,
    oa = inc & (df$oa_status %in% "oa" | (!is.na(df$pdf_url) & nzchar(df$pdf_url)))
  )
}

#' @export
print.medrev <- function(x, ...) {
  df <- mr_df(x)
  cli::cli_h1("medrev review")
  if (nrow(x$searches)) {
    q <- x$searches$query
    cli::cli_text("{.strong Searches:} {length(q)}  ({paste(utils::head(unique(q), 2), collapse = '; ')}{if (length(unique(q)) > 2) ', ...' else ''})")
  }
  cli::cli_text("{.strong Records:} {nrow(df)}")
  if (nrow(df)) {
    n_oa <- sum(df$oa_status %in% "oa" | df$is_oa %in% TRUE, na.rm = TRUE)
    n_pdf <- sum(!is.na(df$pdf_url) & nzchar(df$pdf_url))
    cli::cli_text("{.strong Open access:} {n_oa}  |  {.strong PDF link resolved:} {n_pdf}")
    if (any(!is.na(df$decision))) {
      d <- factor(df$decision, levels = c("include", "exclude", "maybe"))
      cli::cli_text("{.strong Screened:} include {sum(d == 'include', na.rm = TRUE)}  exclude {sum(d == 'exclude', na.rm = TRUE)}  maybe {sum(d == 'maybe', na.rm = TRUE)}  undecided {sum(is.na(df$decision))}")
    }
  }
  if (nrow(x$stages)) {
    cli::cli_text("{.strong Stages:} {paste(sprintf('%s (%d)', x$stages$stage, x$stages$n), collapse = ' -> ')}")
  }
  invisible(x)
}

#' @export
summary.medrev <- function(object, ...) {
  out <- as.data.frame(object$stages)
  cat(sprintf("medrev review: %d records, %d search(es)\n",
              nrow(object$records), nrow(object$searches)))
  if (nrow(out)) print(out, row.names = FALSE)
  invisible(out)
}

#' @export
as.data.frame.medrev <- function(x, ...) as.data.frame(x$records, ...)

#' @export
`[.medrev` <- function(x, i, ...) {
  x <- mr_set_df(x, mr_df(x)[i, , drop = FALSE])
  x
}
