# fulltext(): retrieve JATS full text for open-access records ----------

#' Retrieve full text for open-access records
#'
#' Downloads the JATS full-text XML that Europe PMC serves for open-access PMC
#' articles, and optionally flattens it to plain text. Records without a PMC id
#' are skipped (Europe PMC full text is PMC-backed).
#'
#' @param x A `medrev` object.
#' @param which Which records to try: `"included"`, `"oa"`, or `"all"` (see
#'   [fetch()]).
#' @param format `"text"` (JATS `<body>` flattened to text) or `"xml"` (the raw
#'   document).
#' @param email Optional contact e-mail (see [medrev_email()]).
#'
#' @return A `basetable` with one row per attempted record: `id`, `pmcid`,
#'   `status` (`"available"` / `"unavailable"`), `chars`, and a `text` or `xml`
#'   column holding the content.
#' @export
#'
#' @examples
#' \dontrun{
#' r <- pubmed("open access sepsis machine learning", n = 20)
#' ft <- fulltext(r, which = "oa")
#' }
fulltext <- function(x, which = c("included", "oa", "all"),
                     format = c("text", "xml"), email = NULL) {
  mr_assert(x)
  which <- match.arg(which)
  format <- match.arg(format)
  df <- mr_df(x)

  sel <- which(mr_which_rows(df, which) & !is.na(df$pmcid) & nzchar(df$pmcid))
  if (!length(sel)) {
    cli::cli_alert_warning("No records with a PMC id to fetch full text for.")
    return(mr_empty_fulltext(format))
  }

  rows <- vector("list", length(sel))
  cli::cli_progress_bar("Fetching full text", total = length(sel))
  for (k in seq_along(sel)) {
    i <- sel[[k]]
    cli::cli_progress_update()
    pmcid <- toupper(df$pmcid[[i]])
    if (!grepl("^PMC", pmcid)) pmcid <- paste0("PMC", pmcid)
    xml <- tryCatch(epmc_fulltext_xml(pmcid, email), error = function(e) NA_character_)
    ok <- !is.na(xml) && nzchar(xml)
    content <- if (!ok) NA_character_ else if (format == "xml") xml else mr_jats_text(xml)
    rows[[k]] <- data.frame(
      id = df$id[[i]], pmcid = pmcid,
      status = if (ok && !is.na(content)) "available" else "unavailable",
      chars = if (is.na(content)) 0L else nchar(content),
      content = content, stringsAsFactors = FALSE
    )
  }
  cli::cli_progress_done()

  log <- do.call(rbind, rows)
  names(log)[names(log) == "content"] <- format
  got <- sum(log$status == "available")
  cli::cli_alert_success("Full text for {got} of {nrow(log)} record{?s}.")
  basetable::as_basetable(log)
}

mr_empty_fulltext <- function(format) {
  out <- data.frame(id = character(), pmcid = character(), status = character(),
                    chars = integer(), stringsAsFactors = FALSE)
  out[[format]] <- character()
  basetable::as_basetable(out)
}

# Flatten JATS XML to text: the <body> if present, else the whole document.
mr_jats_text <- function(xml) {
  doc <- tryCatch(xml2::read_xml(xml), error = function(e) NULL)
  if (is.null(doc)) return(NA_character_)
  xml2::xml_ns_strip(doc)
  body <- xml2::xml_find_first(doc, "//body")
  node <- if (inherits(body, "xml_missing")) doc else body
  txt <- xml2::xml_text(node)
  txt <- gsub("[ \t\r\f]+", " ", txt)
  txt <- gsub(" *\n[ \n]*", "\n", txt)
  trimws(txt)
}
