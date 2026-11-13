#' Deprecated functions
#'
#' These functions were renamed in medrev 0.8.2 and will be removed in a future
#' release. Use the new names.
#'
#' @param ... Arguments passed to the new function.
#' @name medrev-deprecated
#' @keywords internal
NULL

#' @rdname medrev-deprecated
#' @export
fuzzy_match <- function(...) {
  .Deprecated("fuzzymatch")
  fuzzymatch(...)
}

#' @rdname medrev-deprecated
#' @export
is_medrev <- function(...) {
  .Deprecated("ismedrev")
  ismedrev(...)
}

#' @rdname medrev-deprecated
#' @export
llm_echo <- function(...) {
  .Deprecated("llmecho")
  llmecho(...)
}

#' @rdname medrev-deprecated
#' @export
medrev_cache_clear <- function(...) {
  .Deprecated("medrevcacheclear")
  medrevcacheclear(...)
}

#' @rdname medrev-deprecated
#' @export
medrev_cache_path <- function(...) {
  .Deprecated("medrevcachepath")
  medrevcachepath(...)
}

#' @rdname medrev-deprecated
#' @export
medrev_email <- function(...) {
  .Deprecated("medrevemail")
  medrevemail(...)
}

#' @rdname medrev-deprecated
#' @export
review_brief <- function(...) {
  .Deprecated("reviewbrief")
  reviewbrief(...)
}

#' @rdname medrev-deprecated
#' @export
search_log <- function(...) {
  .Deprecated("searchlog")
  searchlog(...)
}

#' @rdname medrev-deprecated
#' @export
term_counts <- function(...) {
  .Deprecated("termcounts")
  termcounts(...)
}

#' @rdname medrev-deprecated
#' @export
year_counts <- function(...) {
  .Deprecated("yearcounts")
  yearcounts(...)
}

