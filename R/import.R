# medrev(): build a review from records found elsewhere -----------------

#' Create a review from a table of records
#'
#' Builds a `medrev` object from records you already have: an export from
#' Embase, the Cochrane Library, Web of Science or a reference manager, or a
#' hand-made list of included studies. The result works with every medrev
#' verb (`dedup()`, `screen()`, `chunk()`, `pdftext()`, `extract()`, ...) and
#' can be merged with Europe PMC searches using `c()`.
#'
#' @param records A `data.frame` with at least a `title` column. Any of the
#'   medrev record columns (`pmid`, `pmcid`, `doi`, `abstract`, `authors`,
#'   `journal`, `year`, ...) are kept; other columns are dropped.
#' @param source Label stored in the `source` column and the search log,
#'   e.g. `"embase"`.
#'
#' @return A `medrev` object. Record ids are `pmid:<pmid>`, else
#'   `doi:<doi>`, else `<source>:<row number>`.
#' @export
#'
#' @examples
#' r <- medrev(data.frame(
#'   title = c("Drug X in sepsis: a randomised trial", "Drug X and mortality"),
#'   doi = c("10.1000/a", NA), year = c(2021, 2023)
#' ), source = "embase")
#' r
medrev <- function(records, source = "import") {
  if (!is.data.frame(records)) mr_stop("{.arg records} must be a data.frame.")
  if (!is.character(source) || length(source) != 1L || !nzchar(source)) {
    mr_stop("{.arg source} must be a single non-empty string.")
  }
  df <- as.data.frame(records, stringsAsFactors = FALSE)
  names(df) <- tolower(names(df))
  if (!"title" %in% names(df)) mr_stop("{.arg records} needs a {.val title} column.")
  n <- nrow(df)
  chr <- setdiff(.mr_cols, c("year", "is_oa"))
  for (cl in chr) {
    df[[cl]] <- if (cl %in% names(df)) as.character(df[[cl]]) else rep(NA_character_, n)
  }
  df$year <- if ("year" %in% names(df)) suppressWarnings(as.integer(df$year)) else rep(NA_integer_, n)
  df$is_oa <- if ("is_oa" %in% names(df)) as.logical(df$is_oa) else rep(NA, n)
  for (cl in c("pmid", "pmcid", "doi")) df[[cl]][!is.na(df[[cl]]) & !nzchar(trimws(df[[cl]]))] <- NA_character_
  df$doi <- sub("^https?://(dx\\.)?doi\\.org/", "", df$doi)
  df$source <- ifelse(is.na(df$source), source, df$source)
  df$id <- ifelse(!is.na(df$id), df$id,
               ifelse(!is.na(df$pmid), paste0("pmid:", df$pmid),
                      ifelse(!is.na(df$doi), paste0("doi:", tolower(df$doi)),
                             paste0(source, ":", seq_len(n)))))
  df$retrieved_at <- ifelse(is.na(df$retrieved_at), as.character(Sys.Date()), df$retrieved_at)
  x <- new_medrev(df)
  x <- mr_log_search(x, sprintf("imported (%s)", source), source, n, n)
  x <- mr_stage(x, "identified", sprintf("%d imported from %s", n, source))
  cli::cli_alert_success("{n} record{?s} imported from {.val {source}}.")
  x
}
