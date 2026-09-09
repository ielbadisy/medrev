# Europe PMC search + record normalisation ----------------------------------
# Docs: https://europepmc.org/RestfulWebService

# Map one Europe PMC "core" result to a one-row data.frame in medrev's schema.
mr_epmc_row <- function(rec) {
  `%|%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
  ptypes <- unlist(rec$pubTypeList$pubType %|% list())
  kw <- unlist(rec$keywordList$keyword %|% list())
  mesh <- vapply(rec$meshHeadingList$meshHeading %|% list(),
                 function(m) mr_chr1(m$descriptorName), character(1))
  journal <- rec$journalInfo$journal$title %|% rec$journalTitle %|% NA_character_
  native <- mr_chr1(rec$source)             # e.g. "MED", "PMC", "PPR"
  eid <- mr_chr1(rec$id)
  urls <- rec$fullTextUrlList$fullTextUrl %|% list()
  pdf_url <- NA_character_
  if (length(urls)) {
    style <- tolower(vapply(urls, function(u) mr_chr1(u$documentStyle), character(1)))
    avail <- tolower(vapply(urls, function(u) mr_chr1(u$availability), character(1)))
    pick <- which(style == "pdf" & avail %in% c("open access", "free"))
    if (!length(pick)) pick <- which(style == "pdf")
    if (length(pick)) pdf_url <- mr_chr1(urls[[pick[[1]]]]$url)
  }
  doi <- tolower(mr_chr1(rec$doi))
  data.frame(
    id = NA_character_,
    source = "europepmc",
    pmid = if (identical(native, "MED")) eid else mr_chr1(rec$pmid),
    pmcid = mr_chr1(rec$pmcid),
    doi = doi,
    title = mr_chr1(rec$title),
    abstract = mr_chr1(rec$abstractText),
    authors = mr_chr1(rec$authorString),
    journal = mr_chr1(journal),
    year = suppressWarnings(as.integer(mr_chr1(rec$pubYear))),
    type = if (length(ptypes)) paste(ptypes, collapse = "; ") else NA_character_,
    keywords = if (length(kw)) paste(kw, collapse = "; ") else NA_character_,
    mesh = if (length(mesh)) paste(mesh[!is.na(mesh)], collapse = "; ") else NA_character_,
    language = mr_chr1(rec$language),
    is_oa = mr_yn(rec$isOpenAccess) || mr_yn(rec$inEPMC),
    license = mr_chr1(rec$license),
    url = if (!is.na(doi)) paste0("https://doi.org/", doi)
          else paste0("https://europepmc.org/article/", native %|% "MED", "/", eid),
    pdf_url = pdf_url,
    pdf_source = if (!is.na(pdf_url)) "europepmc" else NA_character_,
    oa_status = NA_character_,
    decision = NA_character_,
    reason = NA_character_,
    screened_at = NA_character_,
    retrieved_at = as.character(Sys.Date()),
    stringsAsFactors = FALSE
  )
}

# An empty record data.frame with the full schema and correct column types.
mr_empty_records <- function() {
  df <- mr_epmc_row(list())
  df[0, , drop = FALSE]
}

# Paged Europe PMC search. `sort` is "" (relevance) or an EPMC sort clause.
mr_epmc_search <- function(query, n = 100L, sort = "", email = NULL) {
  page_size <- min(n, 1000L)
  cursor <- "*"
  rows <- list()
  got <- 0L
  hits <- NA_integer_
  repeat {
    q <- list(query = query, format = "json", resultType = "core",
              pageSize = page_size, cursorMark = cursor)
    if (nzchar(sort)) q$sort <- sort
    res <- epmc_json("search", q, email = email)
    if (is.na(hits)) hits <- suppressWarnings(as.integer(res$hitCount %||% 0L))
    recs <- res$resultList$result %||% list()
    if (!length(recs)) break
    rows <- c(rows, lapply(recs, mr_epmc_row))
    got <- got + length(recs)
    nxt <- res$nextCursorMark %||% cursor
    if (identical(nxt, cursor) || got >= n || got >= hits) break
    cursor <- nxt
  }
  df <- if (length(rows)) do.call(rbind, rows) else mr_empty_records()
  if (nrow(df) > n) df <- df[seq_len(n), , drop = FALSE]
  df$id <- mr_make_id(df$doi, df$pmid, df$pmcid, df$title)
  df <- df[, .mr_cols, drop = FALSE]
  attr(df, "hits") <- hits
  df
}
