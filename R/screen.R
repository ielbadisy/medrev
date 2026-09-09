# screen(): record inclusion / exclusion decisions -----------------------

#' Screen records for inclusion
#'
#' Three ways to use it:
#'
#' * `screen(x)` with no other arguments returns an editable **screening
#'   sheet**: one row per record with empty `decision` / `exclude_reason` /
#'   `reviewer` / `notes` columns.
#' * `screen(x, include = <condition>)` / `screen(x, exclude = <condition>,
#'   reason = "...")` evaluate the conditions against the record table (as in
#'   [subset()]) and set `decision` for the matching rows.
#' * `screen(x, sheet = <edited sheet>)` reads decisions back from a sheet
#'   returned by the first form.
#'
#' @param x A `medrev` object.
#' @param include,exclude Unquoted conditions evaluated in the record table
#'   (columns `year`, `journal`, `title`, `abstract`, ... are in scope).
#'   `include` marks matches `"include"`; `exclude` marks matches `"exclude"`.
#' @param reason Short text stored in `reason` for rows matched by `exclude`.
#' @param sheet A data frame returned by `screen(x)` with its `decision`
#'   column filled in.
#'
#' @return With `sheet` or a condition: the `medrev` object with `decision`,
#'   `reason`, `screened_at` updated and a `"screened"` stage logged. With no
#'   arguments: a `basetable` screening sheet.
#' @export
#'
#' @examples
#' \dontrun{
#' r <- pubmed("statins primary prevention", n = 100)
#' r <- screen(r, exclude = year < 2005, reason = "pre-2005")
#' sheet <- screen(r)                 # to edit by hand
#' r <- screen(r, sheet = sheet)
#' }
screen <- function(x, include, exclude, reason = NA_character_, sheet = NULL) {
  mr_assert(x)
  df <- mr_df(x)

  if (!is.null(sheet)) {
    return(mr_apply_sheet(x, sheet))
  }
  has_inc <- !missing(include)
  has_exc <- !missing(exclude)
  if (!has_inc && !has_exc) {
    return(mr_screen_sheet(df))
  }

  now <- mr_now()
  if (has_inc) {
    hit <- mr_eval_cond(substitute(include), df, parent.frame())
    df$decision[hit] <- "include"
    df$screened_at[hit] <- now
  }
  if (has_exc) {
    hit <- mr_eval_cond(substitute(exclude), df, parent.frame())
    df$decision[hit] <- "exclude"
    df$reason[hit] <- if (is.na(reason)) "excluded by rule" else reason
    df$screened_at[hit] <- now
  }
  x <- mr_set_df(x, df)
  n_inc <- sum(df$decision %in% "include")
  n_exc <- sum(df$decision %in% "exclude")
  cli::cli_alert_info("Screened: {n_inc} include, {n_exc} exclude, {sum(is.na(df$decision))} undecided.")
  mr_stage(x, "screened", sprintf("include %d / exclude %d", n_inc, n_exc))
}

# Evaluate an unquoted condition in the record data frame; returns a logical
# vector the length of nrow(df) (NA -> FALSE).
mr_eval_cond <- function(expr, df, env) {
  val <- tryCatch(eval(expr, df, env),
                  error = function(e) mr_stop("Could not evaluate screening condition: {conditionMessage(e)}"))
  if (is.numeric(val)) {                 # row indices
    out <- rep(FALSE, nrow(df)); out[val] <- TRUE; return(out)
  }
  if (!is.logical(val) || length(val) != nrow(df)) {
    mr_stop("A screening condition must give a logical vector with one value per record.")
  }
  val & !is.na(val)
}

mr_screen_sheet <- function(df) {
  basetable::as_basetable(data.frame(
    id = df$id, pmid = df$pmid, doi = df$doi, year = df$year,
    journal = df$journal, title = df$title, abstract = df$abstract,
    decision = df$decision, exclude_reason = df$reason,
    reviewer = NA_character_, notes = NA_character_,
    stringsAsFactors = FALSE
  ))
}

mr_apply_sheet <- function(x, sheet) {
  sheet <- as.data.frame(sheet)
  if (!all(c("id", "decision") %in% names(sheet))) {
    mr_stop("{.arg sheet} must have at least {.field id} and {.field decision} columns.")
  }
  df <- mr_df(x)
  if (anyDuplicated(df$id)) {
    cli::cli_warn("Records have duplicate {.field id}s; run {.fn dedup} before screening for a reliable match.")
  }
  m <- match(df$id, sheet$id)
  dec <- tolower(trimws(as.character(sheet$decision[m])))
  dec[!dec %in% c("include", "exclude", "maybe")] <- NA_character_
  decided <- !is.na(dec)
  df$decision[decided] <- dec[decided]
  if ("exclude_reason" %in% names(sheet)) {
    df$reason[decided] <- as.character(sheet$exclude_reason[m])[decided]
  }
  df$screened_at[decided] <- mr_now()
  x <- mr_set_df(x, df)
  cli::cli_alert_info("Applied {sum(decided)} decision{?s} from sheet.")
  mr_stage(x, "screened", sprintf("sheet: %d decided", sum(decided)))
}
