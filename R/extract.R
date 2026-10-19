# extract(): model-assisted structured extraction ------------------

#' Extract structured data from evidence with a model
#'
#' Fills in a data extraction form for each record: study characteristics
#' for a systematic review (`form("sr")`), effect data for a meta-analysis
#' (`form("ma")`), or any custom set of fields. The model must use only the
#' chunk text and return, for every field, a value and the verbatim quote
#' that supports it. Values are coerced to the field kind (number, integer,
#' choice, text) and stored as JSON in a `medrev_insights` table:
#' `type == "extraction"` for one-row-per-study forms and `type == "effect"`
#' for many-rows-per-study forms. [extractions()] and [effectsizes()] turn
#' them into tables, [verify()] checks them against the source text.
#'
#' @param chunks A `medrev_chunks` object from [chunk()].
#' @param llm A model callback: `function(prompt, ...)` returning a string.
#' @param form A [form()], or a named character vector of free-text fields
#'   (names are the keys, values describe each key), e.g.
#'   `c(design = "study design", n = "total sample size")`.
#' @param prompt Optional prompt template; must contain `{evidence}` and
#'   `{schema}` placeholders.
#' @param per `"record"` (one call per record) or `"all"` (one call over all
#'   chunks).
#' @param model A short label recorded in the `model` column.
#' @param fields Deprecated alias for `form` (a named character vector).
#'
#' @return A `medrev_insights` object. Rows whose response could not be parsed
#'   keep the raw text in `text` and `NA` in `data`.
#' @export
#'
#' @examples
#' \dontrun{
#' ch <- chunk(r, fulltext = fulltext(r))
#' sr <- extract(ch, llm = my_model, form = form("sr"))
#' extractions(sr)
#' ma <- extract(ch, llm = my_model, form = form("ma", measure = "binary"))
#' effectsizes(ma)
#' }
extract <- function(chunks, llm, form = NULL, prompt = NULL,
                    per = c("record", "all"), model = "user-callback",
                    fields = NULL) {
  if (!inherits(chunks, "medrev_chunks")) {
    mr_stop("{.arg chunks} must be a {.cls medrev_chunks} object (from {.fn chunk}).")
  }
  mr_check_llm(llm)
  per <- match.arg(per)
  form <- form %||% fields
  if (is.null(form)) mr_stop("Supply a {.arg form}, e.g. {.code form(\"sr\")}.")
  f <- mr_as_form(form)
  template <- prompt %||% .mr_extract_prompt
  for (ph in c("{evidence}", "{schema}")) {
    if (!grepl(ph, template, fixed = TRUE)) mr_stop("{.arg prompt} must contain the {ph} placeholder.")
  }
  schema <- mr_form_prompt_schema(f)
  rows_form <- identical(attr(f, "unit"), "row")
  itype <- if (rows_form) "effect" else "extraction"

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
    parsed <- mr_parse_form(resp, f)
    ok <- !is.null(parsed)
    if (!ok) n_bad <- n_bad + 1L
    payload <- if (!ok) NA_character_
      else if (rows_form) mr_to_json(list(rows = parsed))
      else mr_to_json(parsed[[1]])
    rows[[k]] <- data.frame(
      insight_id = sprintf("%s-%04d", if (rows_form) "eff" else "ext", k),
      type = itype,
      record_id = if (per == "record") g$record_id[[1]] else NA_character_,
      chunk_ids = paste(g$chunk_id, collapse = ";"),
      n_chunks = nrow(g),
      text = if (ok) NA_character_ else resp,
      data = payload,
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
  "You are extracting data for a systematic review. Fill in the form below ",
  "using ONLY the excerpts. For every field give the value and a short ",
  "verbatim quote copied exactly from the excerpts that supports it. If a ",
  "field is not reported, use null for both. Do not compute or convert ",
  "values; report them as written. Reply with a single JSON object and ",
  "nothing else, matching this shape:\n\n{schema}\n\n",
  "EXCERPTS:\n{evidence}"
)

mr_to_json <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA))
}

# Parse a model response against a form. Returns a list of rows, each a named
# list field -> list(value, quote), or NULL when nothing can be recovered.
mr_parse_form <- function(txt, f) {
  m <- mr_json_block(txt)
  if (is.null(m)) return(NULL)
  obj <- tryCatch(jsonlite::fromJSON(m, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(obj)) return(NULL)
  rows_form <- identical(attr(f, "unit"), "row")
  objs <- if (rows_form) {
    if (!is.null(names(obj)) && "rows" %in% names(obj)) obj$rows
    else if (is.null(names(obj))) obj
    else list(obj)
  } else {
    list(if (is.null(names(obj)) && length(obj)) obj[[1]] else obj)
  }
  objs <- Filter(function(o) is.list(o) && !is.null(names(o)), objs)
  if (!length(objs)) {
    return(if (rows_form && (identical(obj, list()) || identical(obj$rows, list()))) list() else NULL)
  }
  lapply(objs, function(o) mr_parse_fields(o, f))
}

# The first balanced JSON object or array in a response, ignoring code fences
# and any text before or after it (JSON may span several lines). Arrays count
# only when they hold objects, so a "[1]" in prose is skipped. NULL if none.
mr_json_block <- function(txt) {
  if (is.null(txt) || is.na(txt)) return(NULL)
  s <- gsub("```[a-zA-Z]*", "", txt)
  ch <- strsplit(s, "", fixed = TRUE)[[1]]
  start <- which(ch %in% c("{", "["))
  for (i in start) {
    depth <- 0L
    in_str <- FALSE
    esc <- FALSE
    for (j in i:length(ch)) {
      c1 <- ch[[j]]
      if (in_str) {
        if (esc) esc <- FALSE
        else if (c1 == "\\") esc <- TRUE
        else if (c1 == "\"") in_str <- FALSE
        next
      }
      if (c1 == "\"") in_str <- TRUE
      else if (c1 %in% c("{", "[")) depth <- depth + 1L
      else if (c1 %in% c("}", "]")) {
        depth <- depth - 1L
        if (depth == 0L) {
          block <- paste(ch[i:j], collapse = "")
          is_obj <- ch[[i]] == "{" || grepl("{", block, fixed = TRUE)
          if (is_obj && jsonlite::validate(block)) return(block)
          break
        }
      }
    }
  }
  NULL
}

mr_parse_fields <- function(o, f) {
  out <- lapply(seq_len(nrow(f)), function(i) {
    raw <- o[[f$field[[i]]]]
    if (is.list(raw) && !is.null(names(raw)) && any(c("value", "quote") %in% names(raw))) {
      v <- raw$value
      q <- raw$quote
    } else {
      v <- raw
      q <- NULL
    }
    q <- if (is.null(q) || !length(q) || all(is.na(unlist(q)))) NA_character_ else paste(unlist(q), collapse = " ")
    list(value = mr_coerce_value(v, f$kind[[i]], f$choices[[i]]), quote = q)
  })
  names(out) <- f$field
  out
}

#' Flatten extraction insights to a table
#'
#' @param insights A `medrev_insights` object from [extract()].
#' @param type `"extraction"` (one row per study, from one-row forms such as
#'   `form("sr")`) or `"effect"` (one row per extracted effect row, from
#'   `form("ma")`).
#' @param quotes If `TRUE`, add a `<field>_quote` column next to each field
#'   with the supporting quote.
#'
#' @return A `basetable` with `record_id`, `insight_id` (and `row` for
#'   effects), and one column per field. Number and integer fields are
#'   numeric. Unparsed rows have `NA` fields.
#' @export
extractions <- function(insights, type = c("extraction", "effect"), quotes = FALSE) {
  if (!inherits(insights, "medrev_insights")) {
    mr_stop("{.arg insights} must be a {.cls medrev_insights} object.")
  }
  type <- match.arg(type)
  d <- as.data.frame(insights)
  d <- d[d$type == type, , drop = FALSE]
  empty <- data.frame(record_id = character(), insight_id = character())
  if (type == "effect") empty$row <- integer()
  if (!nrow(d)) return(basetable::as_basetable(empty))
  rows <- lapply(seq_len(nrow(d)), function(i) {
    base <- data.frame(record_id = d$record_id[[i]], insight_id = d$insight_id[[i]],
                       stringsAsFactors = FALSE)
    if (type == "effect") base$row <- NA_integer_
    objs <- mr_insight_rows(d$data[[i]], type)
    if (!length(objs)) return(base)
    parts <- lapply(seq_along(objs), function(j) {
      b <- base
      if (type == "effect") b$row <- j
      cbind(b, mr_flatten_obj(objs[[j]], quotes), stringsAsFactors = FALSE)
    })
    as.data.frame(do.call(basetable::rbindfill, c(parts, typeconflict = "coerce")))
  })
  out <- do.call(basetable::rbindfill, c(rows, typeconflict = "coerce"))
  basetable::as_basetable(out)
}

# Decode one insight's JSON into a list of field objects.
mr_insight_rows <- function(json, type) {
  if (is.na(json)) return(list())
  obj <- tryCatch(jsonlite::fromJSON(json, simplifyVector = FALSE), error = function(e) NULL)
  if (!length(obj)) return(list())
  if (type == "effect") obj$rows %||% list() else list(obj)
}

# A field object -> one-row data.frame (value, plus quote if asked).
mr_flatten_obj <- function(o, quotes = FALSE) {
  cols <- list()
  for (nm in names(o)) {
    v <- o[[nm]]
    q <- NA_character_
    if (is.list(v) && !is.null(names(v)) && "value" %in% names(v)) {
      q <- if (is.null(v$quote)) NA_character_ else as.character(v$quote)
      v <- v$value
    }
    cols[[nm]] <- if (is.null(v) || !length(v)) NA
      else if (is.numeric(v) && length(v) == 1L) v
      else if (all(is.na(unlist(v)))) NA
      else paste(as.character(unlist(v)), collapse = "; ")
    if (quotes) cols[[paste0(nm, "_quote")]] <- q
  }
  as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
}
