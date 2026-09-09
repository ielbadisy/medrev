# bibliography(): export records as references ------------------------

#' Export a review's records to a reference file
#'
#' Writes the record table to `file`. The format is taken from the extension:
#' `.ris`, `.bib` / `.bibtex`, or `.csv`. With no `file`, returns a character
#' vector of formatted references.
#'
#' @param x A `medrev` object.
#' @param file Output path, or `NULL` to return formatted reference strings.
#' @param which Which records to export (see [fetch()]); default `"all"`.
#'
#' @return `file` invisibly, or a character vector when `file` is `NULL`.
#' @export
#'
#' @examples
#' \dontrun{
#' r <- pubmed("insulin resistance review", n = 30)
#' bibliography(r, "insulin.ris")
#' cat(bibliography(r), sep = "\n")
#' }
bibliography <- function(x, file = NULL, which = c("all", "included", "oa")) {
  mr_assert(x)
  which <- match.arg(which)
  df <- mr_df(x)
  df <- df[mr_which_rows(df, which), , drop = FALSE]

  if (is.null(file)) {
    return(vapply(seq_len(nrow(df)), function(i) mr_cite(df[i, ]), character(1)))
  }
  mr_check_string(file, "file")
  ext <- tolower(tools::file_ext(file))
  txt <- switch(ext,
    ris = vapply(seq_len(nrow(df)), function(i) mr_ris(df[i, ]), character(1)),
    bib = ,
    bibtex = vapply(seq_len(nrow(df)), function(i) mr_bibtex(df[i, ]), character(1)),
    csv = { utils::write.csv(df, file, row.names = FALSE, na = ""); return(invisible(file)) },
    mr_stop("Unsupported extension {.val {ext}}; use .ris, .bib, or .csv.")
  )
  writeLines(txt, file)
  invisible(file)
}

# --- formatters -----------------------------------------------------------

mr_authors_etal <- function(x, max_authors = 3) {
  one <- as.character(x)[[1]]
  if (is.na(one) || !nzchar(one)) return("Unknown")
  parts <- trimws(unlist(strsplit(one, "\\s*,\\s*(?=[A-Z])|\\s*;\\s*", perl = TRUE)))
  parts <- parts[nzchar(parts)]
  if (length(parts) > max_authors + 1L) parts <- c(parts[seq_len(max_authors)], "et al.")
  paste(parts, collapse = ", ")
}

mr_cite <- function(row) {
  yr <- if (is.na(row$year)) "n.d." else as.character(row$year)
  doi <- if (is.na(row$doi) || !nzchar(row$doi)) "" else paste0(" doi:", row$doi)
  paste0(mr_authors_etal(row$authors), " (", yr, "). ",
         row$title %||% "", ". ", row$journal %||% "", ".", doi)
}

mr_cite_key <- function(row) {
  a <- trimws(unlist(strsplit(row$authors %||% "anon", "[,;]"))[[1]])
  a <- gsub("[^A-Za-z0-9]", "", a)
  if (!nzchar(a)) a <- "ref"
  paste0(a, if (is.na(row$year)) "" else row$year, "_",
         gsub("[^A-Za-z0-9]", "", substr(row$id %||% "id", 1, 16)))
}

mr_bibtex <- function(row) {
  fields <- c(title = row$title, author = row$authors, journal = row$journal,
              year = if (is.na(row$year)) NA_character_ else as.character(row$year),
              doi = row$doi, url = row$url)
  fields <- fields[!is.na(fields) & nzchar(fields)]
  fields <- gsub("([{}])", "\\\\\\1", fields)
  body <- paste(sprintf("  %s = {%s}", names(fields), fields), collapse = ",\n")
  paste0("@article{", mr_cite_key(row), ",\n", body, "\n}\n")
}

mr_ris <- function(row) {
  au <- trimws(unlist(strsplit(row$authors %||% "", "\\s*;\\s*|\\s*,\\s*(?=[A-Z])", perl = TRUE)))
  au <- au[nzchar(au)]
  lines <- c("TY  - JOUR", paste0("AU  - ", au))
  add <- function(tag, v) if (!is.na(v) && nzchar(as.character(v))) paste0(tag, "  - ", v)
  lines <- c(lines,
             add("TI", row$title), add("JO", row$journal), add("PY", row$year),
             add("DO", row$doi), add("UR", row$url), add("AB", row$abstract))
  paste(c(lines[!vapply(lines, is.null, logical(1))], "ER  - ", ""), collapse = "\n")
}
