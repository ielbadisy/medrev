# Small internal utilities ---------------------------------------------------

mr_stop <- function(..., .envir = parent.frame()) {
  cli::cli_abort(c(...), call = .envir, .envir = .envir)
}

mr_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

mr_check_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    mr_stop("{.arg {name}} must be a non-empty string.")
  }
  invisible(x)
}

mr_check_count <- function(x, name, min = 1L) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < min) {
    mr_stop("{.arg {name}} must be a single number >= {min}.")
  }
  as.integer(x)
}

# "" -> NA, keeping type character.
mr_blank_na <- function(x) {
  x <- as.character(x)
  x[!nzchar(x)] <- NA_character_
  x
}

mr_chr1 <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) NA_character_ else as.character(x[[1]])
}

mr_yn <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA)
  tolower(as.character(x)[[1]]) %in% c("y", "true", "1")
}

# Lower-case, strip markup and punctuation, collapse whitespace: a title key
# for duplicate detection.
mr_norm_title <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("<[^>]+>", " ", x)
  x <- gsub("[^a-z0-9 ]+", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  x[is.na(x)] <- ""
  x
}

# Sanitise a string for use as a file-name component.
mr_slug <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x[!nzchar(x)] <- "NA"
  x
}

# Expand a "{field}" template against one record row (a named list).
mr_render_name <- function(template, row) {
  fields <- regmatches(template, gregexpr("\\{[a-zA-Z0-9_]+\\}", template))[[1]]
  out <- template
  for (f in fields) {
    key <- gsub("[{}]", "", f)
    val <- if (key %in% names(row)) mr_slug(row[[key]]) else "NA"
    out <- gsub(f, val, out, fixed = TRUE)
  }
  out
}

# PDF path for one record. When a template field is missing for the record
# (e.g. no PMID), the record id is used instead so files never collide.
mr_pdf_file <- function(dir, template, row) {
  fields <- gsub("[{}]", "", regmatches(template, gregexpr("\\{[a-zA-Z0-9_]+\\}", template))[[1]])
  missing <- vapply(fields, function(k) {
    v <- if (k %in% names(row)) row[[k]] else NA
    length(v) == 0L || is.na(v[[1]]) || !nzchar(as.character(v[[1]]))
  }, logical(1))
  stem <- if (any(missing) && !is.null(row$id) && !is.na(row$id)) mr_slug(row$id) else mr_render_name(template, row)
  file.path(dir, paste0(stem, ".pdf"))
}

# Coerce a scalar, vector, or file path into a character vector of ids.
mr_as_ids <- function(x) {
  if (length(x) == 1L && is.character(x) && file.exists(x)) {
    lines <- readLines(x, warn = FALSE)
    x <- trimws(sub("[\t ].*$", "", lines))
  }
  x <- trimws(as.character(x))
  unique(x[nzchar(x)])
}

# A stable, human-readable record id: DOI > PMID > PMCID > normalised title.
mr_make_id <- function(doi, pmid, pmcid, title) {
  ok <- function(v) !is.na(v) & nzchar(v)
  ifelse(ok(doi), paste0("doi:", tolower(doi)),
  ifelse(ok(pmid), paste0("pmid:", pmid),
  ifelse(ok(pmcid), paste0("pmcid:", toupper(pmcid)),
         paste0("title:", mr_norm_title(title)))))
}
