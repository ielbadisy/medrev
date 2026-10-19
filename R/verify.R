# verify(): deterministic checks on extracted data -------------------
# Nothing here calls a model. Each check is a rule on the extracted values
# and, when chunks are given, on the text the model saw.

#' Check extracted data against the source text and basic rules
#'
#' Runs deterministic checks on the output of [extract()]:
#'
#' * `quote_not_found` (error): the supporting quote does not occur in the
#'   chunks the model was given (case, spacing and punctuation ignored), a
#'   sign the value may be invented;
#' * `number_not_in_quote` (warning): a numeric value does not appear in its
#'   own quote;
#' * `quote_missing` (note): a value was returned without a quote;
#' * `ci_order` (error): the estimate lies outside its confidence interval,
#'   or the bounds are reversed;
#' * `ratio_nonpositive` (error): an OR, RR or HR, or its bounds, is not
#'   positive;
#' * `events_exceed_n` (error): more events than participants in an arm;
#' * `negative_value` (error): a negative count or SD;
#' * `p_range` (error): a p-value outside 0 to 1;
#' * `arms_exceed_total` (warning): `n_e + n_c` is larger than the study's
#'   `n_total` (when both forms were extracted for the record);
#' * `shared_registration` (warning): two records report the same trial
#'   registration, so they may describe one study (see `study =` in
#'   [effectsizes()]).
#'
#' @param insights A `medrev_insights` object from [extract()] (extraction
#'   and/or effect rows).
#' @param chunks The `medrev_chunks` given to [extract()]. Needed for the
#'   quote checks; without it they are skipped.
#'
#' @return A `basetable` with one row per issue: `insight_id`, `record_id`,
#'   `row`, `field`, `check`, `severity`, `value`, `quote`, `message`. Zero
#'   rows means every check passed.
#' @export
#'
#' @examples
#' \dontrun{
#' ma <- extract(ch, my_model, form = form("ma"))
#' issues <- verify(ma, ch)
#' issues[issues$severity == "error", ]
#' }
verify <- function(insights, chunks = NULL) {
  if (!inherits(insights, "medrev_insights")) {
    mr_stop("{.arg insights} must be a {.cls medrev_insights} object.")
  }
  if (!is.null(chunks) && !inherits(chunks, "medrev_chunks")) {
    mr_stop("{.arg chunks} must be a {.cls medrev_chunks} object (from {.fn chunk}).")
  }
  d <- as.data.frame(insights)
  d <- d[d$type %in% c("extraction", "effect") & !is.na(d$data), , drop = FALSE]
  chunk_text <- if (is.null(chunks)) NULL else {
    cd <- as.data.frame(chunks)
    stats::setNames(cd$text, cd$chunk_id)
  }

  issues <- list()
  add <- function(...) issues[[length(issues) + 1L]] <<- mr_issue(...)
  n_total <- list()
  arms <- list()
  regs <- list()

  for (i in seq_len(nrow(d))) {
    ins <- d[i, ]
    objs <- mr_insight_rows(ins$data, ins$type)
    evidence <- if (is.null(chunk_text)) NULL else {
      ids <- strsplit(ins$chunk_ids, ";", fixed = TRUE)[[1]]
      mr_norm_text(paste(chunk_text[ids[ids %in% names(chunk_text)]], collapse = " "))
    }
    for (j in seq_along(objs)) {
      o <- objs[[j]]
      row <- if (ins$type == "effect") j else NA_integer_
      vals <- list()
      for (fld in names(o)) {
        v <- o[[fld]]
        q <- NA_character_
        if (is.list(v) && "value" %in% names(v)) {
          q <- if (is.null(v$quote)) NA_character_ else as.character(v$quote)
          v <- v$value
        }
        if (is.null(v) || !length(v) || all(is.na(unlist(v)))) next
        v <- unlist(v)[[1]]
        vals[[fld]] <- v
        base <- list(ins = ins, row = row, field = fld, value = v, quote = q)
        if (is.na(q) || !nzchar(trimws(q))) {
          do.call(add, c(base, check = "quote_missing", severity = "note",
                         message = "value has no supporting quote"))
          next
        }
        if (!is.null(evidence) && !grepl(mr_norm_text(q), evidence, fixed = TRUE)) {
          do.call(add, c(base, check = "quote_not_found", severity = "error",
                         message = "quote does not occur in the source chunks"))
        }
        if (is.numeric(v) && !mr_number_in(v, q)) {
          do.call(add, c(base, check = "number_not_in_quote", severity = "warning",
                         message = sprintf("%s does not appear in its quote", format(v))))
        }
      }
      for (p in mr_rule_checks(vals)) {
        do.call(add, c(list(ins = ins, row = row, quote = NA_character_), p))
      }
      if (!is.na(ins$record_id)) {
        if (!is.null(vals$n_total)) n_total[[ins$record_id]] <- vals$n_total
        if (!is.null(vals$registration)) regs[[ins$record_id]] <- toupper(gsub("\\s", "", vals$registration))
        if (is.numeric(vals$n_e) && is.numeric(vals$n_c)) {
          arms[[length(arms) + 1L]] <- list(record_id = ins$record_id, insight_id = ins$insight_id,
                                            row = row, n = vals$n_e + vals$n_c)
        }
      }
    }
  }

  # Cross-form check: arm totals vs the study total.
  for (a in arms) {
    tot <- n_total[[a$record_id]]
    if (is.numeric(tot) && a$n > tot) {
      issues[[length(issues) + 1L]] <- mr_issue(
        ins = a, row = a$row, field = "n_e+n_c", value = a$n, quote = NA_character_,
        check = "arms_exceed_total", severity = "warning",
        message = sprintf("arms total %s exceeds the study n_total %s", a$n, tot))
    }
  }
  # Several records, one registration.
  if (length(regs)) {
    r <- unlist(regs)
    for (reg in unique(r[duplicated(r)])) {
      for (rid in names(r)[r == reg]) {
        issues[[length(issues) + 1L]] <- mr_issue(
          ins = list(insight_id = NA_character_, record_id = rid), row = NA_integer_,
          field = "registration", value = reg, quote = NA_character_,
          check = "shared_registration", severity = "warning",
          message = sprintf("registration %s is shared by %s", reg,
                            paste(names(r)[r == reg], collapse = ", ")))
      }
    }
  }

  out <- if (length(issues)) do.call(rbind, issues) else mr_issue_empty()
  rownames(out) <- NULL
  if (!nrow(out)) {
    cli::cli_alert_success("All checks passed.")
  } else {
    tab <- table(factor(out$severity, levels = c("error", "warning", "note")))
    cli::cli_alert_info("{tab[['error']]} error{?s}, {tab[['warning']]} warning{?s}, {tab[['note']]} note{?s}.")
  }
  basetable::as_basetable(out)
}

# Row-level rules on one object's values. Returns a list of issue specs.
mr_rule_checks <- function(v) {
  out <- list()
  iss <- function(field, value, check, severity, message) {
    out[[length(out) + 1L]] <<- list(field = field, value = value, check = check,
                                      severity = severity, message = message)
  }
  num <- function(k) if (is.numeric(v[[k]])) v[[k]] else NULL
  est <- num("estimate"); lo <- num("ci_low"); hi <- num("ci_high")
  m <- toupper(v$measure %||% "")
  if (!is.null(lo) && !is.null(hi) && lo > hi) {
    iss("ci_low", lo, "ci_order", "error", "lower CI bound is above the upper bound")
  } else if (!is.null(est) && !is.null(lo) && !is.null(hi) && (est < lo || est > hi)) {
    iss("estimate", est, "ci_order", "error", sprintf("estimate %s lies outside [%s, %s]", est, lo, hi))
  }
  if (m %in% .mr_ratio) {
    for (k in c("estimate", "ci_low", "ci_high")) {
      x <- num(k)
      if (!is.null(x) && x <= 0) iss(k, x, "ratio_nonpositive", "error", sprintf("%s must be positive", m))
    }
  }
  for (arm in c("e", "c")) {
    ev <- num(paste0("events_", arm)); n <- num(paste0("n_", arm))
    if (!is.null(ev) && !is.null(n) && ev > n) {
      iss(paste0("events_", arm), ev, "events_exceed_n", "error", sprintf("%s events but %s participants", ev, n))
    }
  }
  for (k in intersect(names(v), c("n_total", "n_e", "n_c", "events_e", "events_c", "sd_e", "sd_c"))) {
    x <- num(k)
    if (!is.null(x) && x < 0) iss(k, x, "negative_value", "error", "must not be negative")
  }
  p <- num("p_value")
  if (!is.null(p) && (p < 0 || p > 1)) iss("p_value", p, "p_range", "error", "p-value outside 0 to 1")
  out
}

mr_issue <- function(ins, row, field, value, quote, check, severity, message) {
  data.frame(insight_id = ins$insight_id %||% NA_character_, record_id = ins$record_id %||% NA_character_,
             row = as.integer(row), field = field, check = check, severity = severity,
             value = as.character(value), quote = as.character(quote), message = message,
             stringsAsFactors = FALSE)
}

mr_issue_empty <- function() {
  data.frame(insight_id = character(), record_id = character(), row = integer(),
             field = character(), check = character(), severity = character(),
             value = character(), quote = character(), message = character(),
             stringsAsFactors = FALSE)
}

# Lowercase, unify dashes/minus, keep letters, digits and dots, single spaces.
mr_norm_text <- function(x) {
  x <- tolower(x)
  x <- gsub("[\u2010-\u2015\u2212]", "-", x)
  x <- gsub("(?<=\\d)\u00b7(?=\\d)", ".", x, perl = TRUE)
  x <- gsub("(?<=\\d),(?=\\d{3})", "", x, perl = TRUE)
  x <- gsub("[^a-z0-9.%-]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

# Does the number x appear in string s?
mr_number_in <- function(x, s) {
  s <- gsub("(?<=\\d),(?=\\d{3})", "", s, perl = TRUE)
  s <- gsub("\u2212", "-", s)
  s <- gsub("(?<=\\d)\u00b7(?=\\d)", ".", s, perl = TRUE)
  nums <- suppressWarnings(as.numeric(regmatches(s, gregexpr("\\d*\\.?\\d+(?:[eE][-+]?\\d+)?", s, perl = TRUE))[[1]]))
  nums <- nums[!is.na(nums)]
  any(abs(nums - abs(x)) <= 1e-9 * max(1, abs(x)))
}
