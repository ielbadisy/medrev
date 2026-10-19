# chunk(): split record text into overlapping windows -----------------
# The first, deterministic step of the enrichment layer. Everything
# downstream (summarize/extract/synthesize) works from chunks, so the model
# never sees more than what is here and provenance stays a plain join on
# record_id.

#' Split records into overlapping text chunks
#'
#' Breaks each selected record's text into word windows for downstream
#' model-assisted steps. By default the text is the abstract; pass a
#' [fulltext()] or [pdftext()] result to use full text where it is available.
#'
#' @param x A `medrev` object.
#' @param size Window length in words.
#' @param overlap Words shared between consecutive windows (`< size`).
#' @param which Which records to chunk: `"included"`, `"oa"`, or `"all"`.
#' @param fulltext Optional `basetable` from [fulltext()] or [pdftext()], or
#'   a list of them; where one has an `"available"` row for a record, that
#'   text is chunked instead of the abstract (earlier list elements win).
#'
#' @return A `medrev_chunks` object: a `basetable` with columns `record_id`,
#'   `chunk_id`, `seq`, `text`, `n_words`, `tokens_est`, `source`.
#' @export
#'
#' @examples
#' \dontrun{
#' r <- pubmed("sepsis prognosis", n = 30)
#' ch <- chunk(r, size = 200)
#' ch
#' }
chunk <- function(x, size = 1200L, overlap = 100L,
                  which = c("included", "oa", "all"), fulltext = NULL) {
  mr_assert(x)
  which <- match.arg(which)
  size <- mr_check_count(size, "size")
  overlap <- mr_check_count(overlap, "overlap", min = 0L)
  if (overlap >= size) mr_stop("{.arg overlap} must be smaller than {.arg size}.")

  df <- mr_df(x)
  df <- df[mr_which_rows(df, which), , drop = FALSE]
  ft <- mr_fulltext_lookup(fulltext)

  rows <- list()
  n_empty <- 0L
  for (i in seq_len(nrow(df))) {
    rid <- df$id[[i]]
    text <- ft[[rid]] %||% df$abstract[[i]]
    if (is.na(text) || nchar(text) < 20L) {
      n_empty <- n_empty + 1L
      next
    }
    w <- mr_windows(text, size, overlap)
    if (!length(w)) {
      n_empty <- n_empty + 1L
      next
    }
    rows[[length(rows) + 1L]] <- data.frame(
      record_id = rid,
      chunk_id = sprintf("%s#%02d", rid, seq_along(w)),
      seq = seq_along(w),
      text = vapply(w, function(z) z$text, character(1)),
      n_words = vapply(w, function(z) z$n, integer(1)),
      tokens_est = vapply(w, function(z) as.integer(ceiling(z$n * 4 / 3)), integer(1)),
      source = df$source[[i]],
      stringsAsFactors = FALSE
    )
  }

  out <- if (length(rows)) do.call(rbind, rows) else mr_empty_chunks()
  rownames(out) <- NULL
  if (n_empty) {
    cli::cli_alert_info("{n_empty} record{?s} had no usable text and produced no chunks.")
  }
  cli::cli_alert_success("{nrow(out)} chunk{?s} from {length(rows)} record{?s}.")
  structure(basetable::as_basetable(out), class = c("medrev_chunks", "basetable", "data.frame"))
}

# text -> list of {text, n} word windows.
mr_windows <- function(text, size, overlap) {
  words <- strsplit(trimws(text), "\\s+")[[1]]
  words <- words[nzchar(words)]
  n <- length(words)
  if (!n) return(list())
  if (n <= size) return(list(list(text = paste(words, collapse = " "), n = n)))
  step <- size - overlap
  starts <- seq.int(1L, n, by = step)
  out <- list()
  for (s in starts) {
    e <- min(s + size - 1L, n)
    out[[length(out) + 1L]] <- list(text = paste(words[s:e], collapse = " "), n = e - s + 1L)
    if (e == n) break
  }
  out
}

# Named list: record id -> full-text string, from a fulltext() / pdftext()
# result or a list of them (the first available text per record wins).
mr_fulltext_lookup <- function(fulltext) {
  if (is.null(fulltext)) return(list())
  if (!is.data.frame(fulltext) && is.list(fulltext)) {
    out <- list()
    for (ft in rev(fulltext)) {
      l <- mr_fulltext_lookup(ft)
      out[names(l)] <- l
    }
    return(out)
  }
  d <- as.data.frame(fulltext)
  tcol <- if ("text" %in% names(d)) "text" else if ("xml" %in% names(d)) "xml" else NULL
  if (is.null(tcol) || !"id" %in% names(d)) return(list())
  ok <- d$status %in% "available" & !is.na(d[[tcol]]) & nzchar(d[[tcol]])
  l <- as.list(d[[tcol]][ok])
  names(l) <- d$id[ok]
  l
}

mr_empty_chunks <- function() {
  data.frame(record_id = character(), chunk_id = character(), seq = integer(),
             text = character(), n_words = integer(), tokens_est = integer(),
             source = character(), stringsAsFactors = FALSE)
}

#' @export
print.medrev_chunks <- function(x, ...) {
  d <- as.data.frame(x)
  nrec <- length(unique(d$record_id))
  mw <- if (nrow(d)) as.integer(round(mean(d$n_words))) else 0L
  cat(sprintf("<medrev_chunks> %d chunk%s from %d record%s (mean %d words/chunk)\n",
              nrow(d), if (nrow(d) == 1) "" else "s",
              nrec, if (nrec == 1) "" else "s", mw))
  NextMethod()
}

#' @export
as.data.frame.medrev_chunks <- function(x, ...) {
  class(x) <- "data.frame"
  x
}
