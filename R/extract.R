# extract(): model-assisted structured extraction ------------------

#' Extract structured fields from evidence with a model
#'
#' Asks the model to return a JSON object with a fixed set of keys, using only
#' the chunk text. Parsed results are stored as `type == "extraction"` insights
#' (the JSON in the `data` column); [extractions()] flattens them to one row
#' per record with a column per field.
#'
#' @param chunks A `medrev_chunks` object from [chunk()].
#' @param llm A model callback: `function(prompt, ...)` returning a string.
#' @param fields A named character vector: names are the JSON keys to return,
#'   values are short descriptions of what each key means, e.g.
#'   `c(design = "study design", n = "total sample size")`.
#' @param prompt Optional prompt template; must contain `{evidence}` and
#'   `{schema}` placeholders.
#' @param per `"record"` (one object per record) or `"all"`.
#' @param model A short label recorded in the `model` column.
#'
#' @return A `medrev_insights` object. Rows whose response could not be parsed
#'   as JSON keep the raw text in `text` and `NA` in `data`.
#' @export
#'
#' @examples
#' \dontrun{
#' ins <- extract(chunk(r), llm = my_model,
#'                fields = c(design = "study design", n = "sample size",
#'                           outcome = "primary outcome"))
#' extractions(ins)
#' }
extract <- function(chunks, llm, fields, prompt = NULL,
                    per = c("record", "all"), model = "user-callback") {
  if (!inherits(chunks, "medrev_chunks")) {
    mr_stop("{.arg chunks} must be a {.cls medrev_chunks} object (from {.fn chunk}).")
  }
  mr_check_llm(llm)
  per <- match.arg(per)
  if (is.null(names(fields)) || !is.character(fields) || any(!nzchar(names(fields)))) {
    mr_stop("{.arg fields} must be a named character vector, e.g. {.code c(n = \"sample size\")}.")
  }
  template <- prompt %||% .mr_extract_prompt
  for (ph in c("{evidence}", "{schema}")) {
    if (!grepl(ph, template, fixed = TRUE)) mr_stop("{.arg prompt} must contain the {ph} placeholder.")
  }
  schema <- paste(sprintf("  \"%s\": string  // %s", names(fields), fields), collapse = ",\n")
  schema <- paste0("{\n", schema, "\n}")

  d <- as.data.frame(chunks)
  if (!nrow(d)) {
    cli::cli_alert_warning("No chunks to extract from.")
    return(mr_empty_insights())
  }
  groups <- if (per == "record") split(d, d$record_id) else list(all = d)

  rows <- vector("list", length(groups))
  n_bad <- 0L
  cli::cli_progress_bar("Extracting", total = length(groups))
  for (k in seq_along(groups)) {
    cli::cli_progress_update()
    g <- groups[[k]][order(groups[[k]]$seq), , drop = FALSE]
    evidence <- paste(sprintf("[%s] %s", g$chunk_id, g$text), collapse = "\n\n")
    p <- gsub("{schema}", schema, mr_render_prompt(template, evidence), fixed = TRUE)
    resp <- mr_call_llm(llm, p)
    parsed <- mr_parse_json(resp, names(fields))
    ok <- !is.null(parsed)
    if (!ok) n_bad <- n_bad + 1L
    rows[[k]] <- data.frame(
      insight_id = sprintf("ext-%04d", k),
      type = "extraction",
      record_id = if (per == "record") g$record_id[[1]] else NA_character_,
      chunk_ids = paste(g$chunk_id, collapse = ";"),
      n_chunks = nrow(g),
      text = if (ok) NA_character_ else resp,
      data = if (ok) jsonlite::toJSON(parsed, auto_unbox = TRUE, null = "null") else NA_character_,
      model = model,
      at = mr_now(),
      stringsAsFactors = FALSE
    )
  }
  cli::cli_progress_done()
  if (n_bad) cli::cli_alert_warning("{n_bad} response{?s} could not be parsed as JSON (raw text kept).")
  cli::cli_alert_success("{length(groups) - n_bad} of {length(groups)} extraction{?s} parsed.")
  new_medrev_insights(do.call(rbind, rows))
}

.mr_extract_prompt <- paste0(
  "Extract the following fields from the excerpts below. Use ONLY information ",
  "in the excerpts; if a field is not reported, use null. Reply with a single ",
  "JSON object and nothing else, matching this shape:\n\n{schema}\n\n",
  "EXCERPTS:\n{evidence}"
)

#' Flatten extraction insights to a table
#'
#' @param insights A `medrev_insights` object from [extract()].
#'
#' @return A `basetable` with `record_id`, `insight_id`, and one column per
#'   extracted field. Unparsed rows have `NA` fields.
#' @export
extractions <- function(insights) {
  if (!inherits(insights, "medrev_insights")) {
    mr_stop("{.arg insights} must be a {.cls medrev_insights} object.")
  }
  d <- as.data.frame(insights)
  d <- d[d$type == "extraction", , drop = FALSE]
  if (!nrow(d)) return(basetable::as_basetable(data.frame(record_id = character(),
                                                          insight_id = character())))
  rows <- lapply(seq_len(nrow(d)), function(i) {
    base <- data.frame(record_id = d$record_id[[i]], insight_id = d$insight_id[[i]],
                       stringsAsFactors = FALSE)
    if (is.na(d$data[[i]])) return(base)
    obj <- tryCatch(jsonlite::fromJSON(d$data[[i]]), error = function(e) list())
    if (!length(obj)) return(base)
    flat <- lapply(obj, function(v) if (is.null(v) || all(is.na(v))) NA_character_ else paste(as.character(v), collapse = "; "))
    cbind(base, as.data.frame(flat, stringsAsFactors = FALSE), stringsAsFactors = FALSE)
  })
  do.call(basetable::rbindfill, rows)
}

# Best-effort parse of a model response into a named list of the requested
# keys. Returns NULL when no JSON object can be recovered.
mr_parse_json <- function(txt, keys) {
  s <- gsub("```[a-zA-Z]*", "", txt)
  m <- regmatches(s, regexpr("\\{.*\\}", s, perl = TRUE))
  if (!length(m)) return(NULL)
  obj <- tryCatch(jsonlite::fromJSON(m, simplifyVector = TRUE), error = function(e) NULL)
  if (!is.list(obj)) return(NULL)
  out <- lapply(keys, function(k) {
    v <- obj[[k]]
    if (is.null(v) || length(v) == 0L || all(is.na(v))) NA_character_
    else paste(as.character(v), collapse = "; ")
  })
  names(out) <- keys
  out
}
