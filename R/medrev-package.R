#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang %||%
## usethis namespace: end
NULL

# The stable column order of a record table. Every table stored in a
# `medrev` object's `$records` has exactly these columns, in this order.
.mr_cols <- c(
  "id", "source", "pmid", "pmcid", "doi", "title", "abstract", "authors",
  "journal", "year", "type", "keywords", "mesh", "language",
  "is_oa", "license", "url",
  "pdf_url", "pdf_source", "oa_status",
  "decision", "reason", "screened_at", "retrieved_at"
)
