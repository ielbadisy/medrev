# pdftext(): text from local PDFs ------------------------------------
# Meta-analyses need every included study, open access or not. pdftext()
# reads PDFs the reviewer already has (downloaded by fetch() or obtained by
# other means) into the same shape as fulltext(), so chunk() can use them.

#' Extract text from local PDFs
#'
#' Reads the PDFs you hold for a review's records, e.g. those saved by
#' [fetch()] or closed-access articles obtained through a library, and
#' returns their text in the same shape as [fulltext()], ready for
#' `chunk(fulltext = )`. Uses the 'pdftools' package when installed, else the
#' `pdftotext` command line tool (poppler-utils).
#'
#' @param x A `medrev` object.
#' @param dir Directory holding the PDFs.
#' @param name File name template used to find each record's PDF, as in
#'   [fetch()] (`{pmid}`, `{doi}`, `{year}`, `{id}`, ...); `.pdf` is
#'   appended. Records missing a template field are looked up by record id.
#' @param files Optional named character vector `c(record_id = "path.pdf")`
#'   that overrides `dir` / `name`, for PDFs with arbitrary file names.
#' @param which Which records to read: `"included"`, `"oa"`, or `"all"`.
#' @param engine `"auto"`, `"pdftools"` or `"pdftotext"`.
#'
#' @return A `basetable` with one row per selected record: `id`, `file`,
#'   `status` (`"available"`, `"missing"` when no file was found, or
#'   `"unreadable"` when it holds no text, e.g. a scanned image), `chars`,
#'   and `text`.
#' @export
#'
#' @examples
#' \dontrun{
#' fetch(r, dir = "pdfs")                      # open access
#' # ... add closed-access PDFs to pdfs/ as <pmid>.pdf ...
#' pt <- pdftext(r, dir = "pdfs")
#' ch <- chunk(r, fulltext = list(fulltext(r), pt))
#' }
pdftext <- function(x, dir = "medrev_pdfs", name = "{pmid}", files = NULL,
                    which = c("included", "oa", "all"),
                    engine = c("auto", "pdftools", "pdftotext")) {
  mr_assert(x)
  which <- match.arg(which)
  engine <- mr_pdf_engine(match.arg(engine))
  df <- mr_df(x)
  df <- df[mr_which_rows(df, which), , drop = FALSE]
  if (!is.null(files) && (!is.character(files) || is.null(names(files)))) {
    mr_stop("{.arg files} must be a named character vector {.code c(record_id = \"path.pdf\")}.")
  }

  rows <- vector("list", nrow(df))
  cli::cli_progress_bar("Reading PDFs", total = nrow(df))
  for (i in seq_len(nrow(df))) {
    cli::cli_progress_update()
    rec <- as.list(df[i, ])
    file <- if (!is.null(files)) unname(files[rec$id]) else mr_pdf_file(dir, name, rec)
    text <- NA_character_
    status <- "missing"
    if (!is.na(file) && file.exists(file)) {
      text <- mr_read_pdf(file, engine)
      status <- if (!is.na(text) && nchar(text) >= 200L) "available" else "unreadable"
    } else {
      file <- NA_character_
    }
    rows[[i]] <- data.frame(id = rec$id, file = file, status = status,
                            chars = if (is.na(text)) 0L else nchar(text),
                            text = text, stringsAsFactors = FALSE)
  }
  cli::cli_progress_done()
  out <- if (length(rows)) do.call(rbind, rows) else
    data.frame(id = character(), file = character(), status = character(),
               chars = integer(), text = character(), stringsAsFactors = FALSE)
  n_ok <- sum(out$status == "available")
  cli::cli_alert_success("Text from {n_ok} of {nrow(out)} record{?s}.")
  if (any(out$status == "unreadable")) {
    cli::cli_alert_warning("{sum(out$status == 'unreadable')} PDF{?s} had no extractable text (scanned images need OCR).")
  }
  basetable::as_basetable(out)
}

mr_pdf_engine <- function(engine) {
  has_pkg <- requireNamespace("pdftools", quietly = TRUE)
  has_bin <- nzchar(Sys.which("pdftotext"))
  if (engine == "auto") engine <- if (has_pkg) "pdftools" else if (has_bin) "pdftotext" else "none"
  if (engine == "none" || (engine == "pdftools" && !has_pkg) || (engine == "pdftotext" && !has_bin)) {
    mr_stop(c("No PDF text engine available.",
              "i" = "Install the {.pkg pdftools} package, or the {.code pdftotext} tool (poppler-utils)."))
  }
  engine
}

# One PDF -> cleaned text (NA on failure).
mr_read_pdf <- function(file, engine) {
  txt <- tryCatch({
    if (engine == "pdftools") {
      paste(pdftools::pdf_text(file), collapse = "\n")
    } else {
      out <- tempfile(fileext = ".txt")
      on.exit(unlink(out), add = TRUE)
      system2(Sys.which("pdftotext"), c("-enc", "UTF-8", shQuote(file), shQuote(out)),
              stdout = FALSE, stderr = FALSE)
      if (!file.exists(out)) NA_character_ else paste(readLines(out, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    }
  }, error = function(e) NA_character_)
  if (is.na(txt)) return(NA_character_)
  mr_clean_pdf_text(txt)
}

# Join words hyphenated across lines, drop form feeds, squeeze spaces.
mr_clean_pdf_text <- function(txt) {
  txt <- gsub("\f", "\n", txt, fixed = TRUE)
  txt <- gsub("([a-z])-\\s*\n\\s*([a-z])", "\\1\\2", txt, perl = TRUE)
  txt <- gsub("[ \t]+", " ", txt)
  txt <- gsub("\\s*\n\\s*", "\n", txt)
  trimws(txt)
}
