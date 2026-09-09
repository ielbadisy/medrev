# HTTP layer: polite, retrying clients for the external services -------------

.mr_epmc <- "https://www.ebi.ac.uk/europepmc/webservices/rest"
.mr_unpaywall <- "https://api.unpaywall.org/v2"

#' Contact e-mail for polite API access
#'
#' Europe PMC does not require an e-mail, but Unpaywall does and general API
#' etiquette asks for one. Looked up, in order, from the `email` argument, the
#' `medrev.email` option, and the `MEDREV_EMAIL` environment variable.
#'
#' @param email Optional explicit e-mail; returned as-is when non-empty.
#'
#' @return A single string, or `NULL` when nothing is configured.
#' @export
#'
#' @examples
#' withr::with_options(list(medrev.email = "me@example.org"), medrev_email())
medrev_email <- function(email = NULL) {
  email <- email %||% getOption("medrev.email") %||% Sys.getenv("MEDREV_EMAIL", "")
  if (is.null(email) || !nzchar(email)) NULL else email
}

mr_require_email <- function(email, what) {
  email <- medrev_email(email)
  if (is.null(email)) {
    mr_stop(
      "{what} needs a contact e-mail.",
      i = "Set {.code options(medrev.email = \"you@example.org\")} or the {.envvar MEDREV_EMAIL} variable."
    )
  }
  email
}

mr_user_agent <- function(email = NULL) {
  email <- medrev_email(email)
  v <- utils::packageVersion("medrev")
  base <- paste0("medrev/", v, " (https://github.com/ielbadisy/medrev)")
  if (is.null(email)) base else paste0(base, "; mailto:", email)
}

# Build a request with a user agent, timeout, throttle and transient-only retry.
mr_req <- function(base_url, email = NULL, rate = 3, timeout = 60) {
  httr2::request(base_url) |>
    httr2::req_user_agent(mr_user_agent(email)) |>
    httr2::req_timeout(timeout) |>
    httr2::req_throttle(rate = rate) |>
    httr2::req_retry(
      max_tries = 3,
      backoff = function(attempt) 2^attempt,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
    )
}

# Perform a request, rethrowing any failure as one readable error naming the
# remote service.
mr_perform <- function(req, source) {
  tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      mr_stop("Request to {source} failed: {conditionMessage(e)}", parent = e)
    }
  )
}

# TRUE when a response body starts with the PDF magic number or is typed as PDF.
mr_is_pdf <- function(resp) {
  ctype <- tryCatch(httr2::resp_content_type(resp), error = function(e) "")
  if (isTRUE(grepl("application/pdf", ctype, fixed = TRUE))) return(TRUE)
  body <- tryCatch(httr2::resp_body_raw(resp), error = function(e) raw())
  length(body) >= 5L && identical(rawToChar(body[1:5]), "%PDF-")
}

epmc_json <- function(path, query = list(), email = NULL) {
  req <- mr_req(.mr_epmc, email = email, rate = 8) |>
    httr2::req_url_path_append(path)
  req <- do.call(httr2::req_url_query, c(list(req), query))
  resp <- mr_perform(req, "Europe PMC")
  httr2::resp_check_status(resp)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}
