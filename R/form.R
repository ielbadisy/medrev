# form(): typed data extraction forms --------------------------------
# A form says what extract() asks the model for: one field per row, with a
# kind (text, number, integer, choice), a description, and the allowed
# choices. A "study" form returns one object per record (study
# characteristics); a "row" form returns any number of rows per record
# (one per outcome x comparison x timepoint, for meta-analysis).

.mr_form_kinds <- c("text", "number", "integer", "choice")

.mr_designs <- c("RCT", "cluster RCT", "crossover RCT", "non-randomised trial",
                 "cohort", "case-control", "cross-sectional", "case series",
                 "other")

.mr_measures <- c("OR", "RR", "RD", "HR", "MD", "SMD")

#' Data extraction forms for systematic reviews and meta-analysis
#'
#' Builds the typed form that [extract()] fills in. `"sr"` is a study
#' characteristics form (one row per study); `"ma"` is an effect data form
#' (one row per outcome, comparison and timepoint, as many as the study
#' reports). Every field is returned with a verbatim supporting quote, so
#' each value can be checked against the source with [verify()].
#'
#' @param type `"sr"` (study characteristics) or `"ma"` (effect data).
#' @param measure For `type = "ma"`: which arm-level data to ask for.
#'   `"binary"` (events and totals), `"continuous"` (means, SDs, totals),
#'   `"hr"` (time-to-event: hazard ratio, events, totals) or `"any"` (all of
#'   them). Reported effect estimates with their CI are always requested.
#' @param add Extra fields: a named character vector (names are field names,
#'   values are descriptions; all are text fields), or a `data.frame` with
#'   columns `field`, `kind`, `description` and optionally `choices`
#'   (choices separated by `";"`).
#' @param drop Names of default fields to leave out.
#'
#' @return A `medrev_form`: a `data.frame` with columns `field`, `kind`,
#'   `description`, `choices`, and an attribute `unit` (`"study"` or
#'   `"row"`).
#' @export
#'
#' @examples
#' form("sr")
#' form("ma", measure = "binary")
#' form("sr", add = c(dose = "drug dose and schedule"), drop = "funding")
form <- function(type = c("sr", "ma"),
                 measure = c("any", "binary", "continuous", "hr"),
                 add = NULL, drop = NULL) {
  type <- match.arg(type)
  measure <- match.arg(measure)
  f <- if (type == "sr") mr_form_sr() else mr_form_ma(measure)
  if (!is.null(add)) f <- rbind(f, mr_form_rows(add))
  if (!is.null(drop)) {
    bad <- setdiff(drop, f$field)
    if (length(bad)) mr_stop("Unknown field{?s} in {.arg drop}: {.val {bad}}.")
    f <- f[!f$field %in% drop, , drop = FALSE]
  }
  new_medrev_form(f, unit = if (type == "sr") "study" else "row", type = type)
}

mr_form_sr <- function() {
  mr_fdf(
    c("design", "choice", "study design", paste(.mr_designs, collapse = ";")),
    c("registration", "text", "trial registration number (e.g. NCT, ISRCTN), if any", ""),
    c("country", "text", "country or countries where the study was done", ""),
    c("setting", "text", "setting (e.g. hospital, ICU, primary care) and number of centres", ""),
    c("period", "text", "recruitment or study period", ""),
    c("population", "text", "who was studied: main inclusion criteria", ""),
    c("n_total", "integer", "total number of participants randomised or enrolled", ""),
    c("age", "text", "age of participants as reported (mean/median and spread)", ""),
    c("female_pct", "number", "percentage of female participants", ""),
    c("intervention", "text", "intervention or exposure", ""),
    c("comparator", "text", "comparator or control", ""),
    c("outcomes", "text", "outcomes reported, primary first", ""),
    c("follow_up", "text", "length of follow-up", ""),
    c("funding", "text", "funding source", "")
  )
}

mr_form_ma <- function(measure) {
  common <- mr_fdf(
    c("outcome", "text", "outcome name", ""),
    c("timepoint", "text", "timepoint of the outcome (e.g. 28 days, 1 year)", ""),
    c("group_e", "text", "intervention / exposed group label", ""),
    c("group_c", "text", "comparator / control group label", ""),
    c("analysis", "text", "analysis population and adjustment (e.g. ITT unadjusted, adjusted for age and sex)", "")
  )
  binary <- mr_fdf(
    c("events_e", "integer", "number of participants with the event, intervention group", ""),
    c("events_c", "integer", "number of participants with the event, comparator group", "")
  )
  continuous <- mr_fdf(
    c("mean_e", "number", "mean outcome, intervention group", ""),
    c("sd_e", "number", "standard deviation, intervention group", ""),
    c("mean_c", "number", "mean outcome, comparator group", ""),
    c("sd_c", "number", "standard deviation, comparator group", "")
  )
  ns <- mr_fdf(
    c("n_e", "integer", "number analysed, intervention group", ""),
    c("n_c", "integer", "number analysed, comparator group", "")
  )
  est <- mr_fdf(
    c("measure", "choice", "effect measure as reported", paste(.mr_measures, collapse = ";")),
    c("estimate", "number", "reported effect estimate (point value, not log)", ""),
    c("ci_low", "number", "lower bound of the 95% confidence interval", ""),
    c("ci_high", "number", "upper bound of the 95% confidence interval", ""),
    c("p_value", "number", "p-value for the comparison", "")
  )
  arms <- switch(measure,
    binary = rbind(ns, binary),
    continuous = rbind(ns, continuous),
    hr = rbind(ns, binary),
    any = rbind(ns, binary, continuous)
  )
  rbind(common, arms, est)
}

mr_fdf <- function(...) {
  m <- do.call(rbind, list(...))
  data.frame(field = m[, 1], kind = m[, 2], description = m[, 3], choices = m[, 4],
             stringsAsFactors = FALSE)
}

# Normalise user-supplied fields (named character vector or data.frame).
mr_form_rows <- function(add) {
  if (is.character(add)) {
    if (is.null(names(add)) || any(!nzchar(names(add)))) {
      mr_stop("Fields must be a named character vector, e.g. {.code c(n = \"sample size\")}.")
    }
    return(data.frame(field = names(add), kind = "text", description = unname(add),
                      choices = "", stringsAsFactors = FALSE))
  }
  if (is.data.frame(add)) {
    need <- c("field", "kind", "description")
    miss <- setdiff(need, names(add))
    if (length(miss)) mr_stop("Field table is missing column{?s} {.val {miss}}.")
    if (!"choices" %in% names(add)) add$choices <- ""
    add <- data.frame(field = as.character(add$field), kind = as.character(add$kind),
                      description = as.character(add$description),
                      choices = ifelse(is.na(add$choices), "", as.character(add$choices)),
                      stringsAsFactors = FALSE)
    bad <- setdiff(add$kind, .mr_form_kinds)
    if (length(bad)) mr_stop("Unknown field kind {.val {bad}}; use one of {.val {(.mr_form_kinds)}}.")
    return(add)
  }
  mr_stop("Fields must be a named character vector or a data.frame.")
}

new_medrev_form <- function(f, unit, type = "custom") {
  rownames(f) <- NULL
  if (anyDuplicated(f$field)) mr_stop("Duplicated field name{?s}: {.val {unique(f$field[duplicated(f$field)])}}.")
  if (any(!grepl("^[A-Za-z][A-Za-z0-9_]*$", f$field))) {
    mr_stop("Field names must start with a letter and use only letters, digits and {.code _}.")
  }
  structure(f, class = c("medrev_form", "data.frame"), unit = unit, type = type)
}

# Coerce extract()'s `form` argument to a medrev_form.
mr_as_form <- function(form) {
  if (inherits(form, "medrev_form")) return(form)
  new_medrev_form(mr_form_rows(form), unit = "study", type = "custom")
}

#' @export
print.medrev_form <- function(x, ...) {
  unit <- attr(x, "unit")
  cat(sprintf("<medrev_form> %s form, %d field%s, %s\n", attr(x, "type"), nrow(x),
              if (nrow(x) == 1) "" else "s",
              if (identical(unit, "row")) "many rows per study" else "one row per study"))
  d <- as.data.frame(x)
  d$description <- ifelse(nchar(d$description) > 48,
                          paste0(substr(d$description, 1, 45), "..."), d$description)
  d$kind <- ifelse(d$kind == "choice", sprintf("choice(%d)", lengths(strsplit(d$choices, ";"))), d$kind)
  d$choices <- NULL
  print.data.frame(d, row.names = FALSE, right = FALSE)
  invisible(x)
}

#' @export
as.data.frame.medrev_form <- function(x, ...) {
  attr(x, "unit") <- NULL
  attr(x, "type") <- NULL
  class(x) <- "data.frame"
  x
}

# The JSON shape shown to the model.
mr_form_prompt_schema <- function(f) {
  d <- as.data.frame(f)
  lines <- vapply(seq_len(nrow(d)), function(i) {
    val <- switch(d$kind[[i]],
      number = "number",
      integer = "integer",
      choice = paste0("one of ", paste(sprintf("\"%s\"", strsplit(d$choices[[i]], ";")[[1]]), collapse = ", ")),
      "string"
    )
    sprintf("  \"%s\": {\"value\": %s or null, \"quote\": \"verbatim text supporting the value\"}  // %s",
            d$field[[i]], val, d$description[[i]])
  }, character(1))
  obj <- paste0("{\n", paste(lines, collapse = ",\n"), "\n}")
  if (identical(attr(f, "unit"), "row")) {
    paste0("{\"rows\": [\n", obj, ",\n  ... one object per outcome, comparison and timepoint\n]}")
  } else {
    obj
  }
}

# Coerce one raw value to a field's kind. Returns NA when not coercible.
mr_coerce_value <- function(v, kind, choices = "") {
  if (is.null(v) || length(v) == 0L || all(is.na(v))) {
    return(switch(kind, number = NA_real_, integer = NA_integer_, NA_character_))
  }
  if (is.list(v)) v <- unlist(v)
  s <- trimws(paste(as.character(v), collapse = "; "))
  if (!nzchar(s) || tolower(s) %in% c("null", "na", "n/a", "nr", "not reported")) {
    return(switch(kind, number = NA_real_, integer = NA_integer_, NA_character_))
  }
  switch(kind,
    number = mr_first_number(s),
    integer = {
      x <- mr_first_number(s)
      if (is.na(x)) NA_integer_ else as.integer(round(x))
    },
    choice = {
      ch <- strsplit(choices, ";")[[1]]
      hit <- ch[tolower(ch) == tolower(s)]
      if (length(hit)) hit[[1]] else if ("other" %in% ch) "other" else s
    },
    s
  )
}

# First number in a string ("1,204 (52%)" -> 1204; "<0.001" -> 0.001).
mr_first_number <- function(s) {
  s <- gsub("(?<=\\d),(?=\\d{3})", "", s, perl = TRUE)
  s <- gsub("\u2212", "-", s)
  s <- gsub("(?<=\\d)\u00b7(?=\\d)", ".", s, perl = TRUE)
  m <- regmatches(s, regexpr("-?\\d*\\.?\\d+(?:[eE][-+]?\\d+)?", s, perl = TRUE))
  if (!length(m)) return(NA_real_)
  suppressWarnings(as.numeric(m))
}
