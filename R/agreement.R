# agreement(): double extraction ------------------------------------

#' Compare two independent extractions
#'
#' Systematic reviews extract data twice, independently, and resolve the
#' disagreements. `agreement()` compares two [extract()] results built with
#' the same form (for example two models, or one model run twice) field by
#' field. Study rows are matched by record; effect rows are matched within a
#' record by outcome, timepoint and measure (in order of appearance when
#' those repeat).
#'
#' @param a,b `medrev_insights` objects from [extract()].
#' @param type `"extraction"` (study rows) or `"effect"` (effect rows).
#' @param tol Relative tolerance for numbers: values agree when
#'   `|a - b| <= tol * max(1, |a|)`.
#'
#' @return A `basetable` with one row per record, row and field: `record_id`,
#'   `key` (the matched effect row, `NA` for study rows), `field`, `value_a`,
#'   `value_b` and `status` (`"agree"`, `"disagree"`, `"only_a"`,
#'   `"only_b"`). Filter on `status != "agree"` to get the list to resolve.
#'   The overall and per-field agreement are printed.
#' @export
#'
#' @examples
#' \dontrun{
#' a <- extract(ch, model_1, form = form("ma"))
#' b <- extract(ch, model_2, form = form("ma"))
#' ag <- agreement(a, b, type = "effect")
#' ag[ag$status != "agree", ]
#' }
agreement <- function(a, b, type = c("extraction", "effect"), tol = 1e-6) {
  type <- match.arg(type)
  if (!is.numeric(tol) || length(tol) != 1L || tol < 0) mr_stop("{.arg tol} must be a single non-negative number.")
  ta <- mr_agreement_rows(a, type, "a")
  tb <- mr_agreement_rows(b, type, "b")
  keys <- union(names(ta), names(tb))
  rows <- lapply(keys, function(k) {
    ra <- ta[[k]]; rb <- tb[[k]]
    src <- if (is.null(ra)) rb else ra
    rid <- attr(src, "record_id")
    rkey <- if (type == "effect") attr(src, "key") else NA_character_
    flds <- union(names(ra), names(rb))
    if (!length(flds)) return(NULL)
    va <- vapply(flds, function(f) mr_agree_chr(ra[[f]]), "")
    vb <- vapply(flds, function(f) mr_agree_chr(rb[[f]]), "")
    status <- if (is.null(ra)) rep("only_b", length(flds))
      else if (is.null(rb)) rep("only_a", length(flds))
      else ifelse(mapply(mr_values_agree, ra[flds], rb[flds], MoreArgs = list(tol = tol)), "agree", "disagree")
    data.frame(record_id = rid, key = rkey, field = flds, value_a = unname(va),
               value_b = unname(vb), status = unname(status), stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  out <- if (length(rows)) do.call(rbind, rows) else
    data.frame(record_id = character(), key = character(), field = character(),
               value_a = character(), value_b = character(), status = character())
  rownames(out) <- NULL
  if (nrow(out)) {
    rate <- tapply(out$status == "agree", out$field, mean)
    worst <- utils::head(sort(rate), 3L)
    cli::cli_alert_info(sprintf("Agreement %.0f%% over %d value%s; lowest: %s.",
                                100 * mean(out$status == "agree"), nrow(out), if (nrow(out) == 1) "" else "s",
                                paste(sprintf("%s %.0f%%", names(worst), 100 * worst), collapse = ", ")))
  } else {
    cli::cli_alert_warning("Nothing to compare.")
  }
  basetable::as_basetable(out)
}

# insights -> named list of field lists; names are match keys.
mr_agreement_rows <- function(x, type, which) {
  if (!inherits(x, "medrev_insights")) {
    mr_stop("{.arg {which}} must be a {.cls medrev_insights} object.")
  }
  d <- as.data.frame(x)
  d <- d[d$type == type & !is.na(d$data) & !is.na(d$record_id), , drop = FALSE]
  out <- list()
  for (i in seq_len(nrow(d))) {
    objs <- mr_insight_rows(d$data[[i]], type)
    seen <- character()
    for (o in objs) {
      vals <- lapply(o, function(v) if (is.list(v) && "value" %in% names(v)) v$value else v)
      vals <- lapply(vals, function(v) if (is.null(v) || !length(v)) NA else unlist(v)[[1]])
      rk <- NA_character_
      if (type == "effect") {
        rk <- paste(vapply(c("outcome", "timepoint", "measure"), function(f) {
          v <- vals[[f]]
          if (is.null(v) || is.na(v)) "" else mr_norm_text(as.character(v))
        }, ""), collapse = " | ")
        seen <- c(seen, rk)
        rk <- sprintf("%s #%d", rk, sum(seen == rk))
      }
      k <- paste(d$record_id[[i]], rk, sep = "::")
      out[[k]] <- structure(vals, record_id = d$record_id[[i]], key = rk)
    }
  }
  out
}

mr_values_agree <- function(x, y, tol) {
  nx <- is.null(x) || is.na(x); ny <- is.null(y) || is.na(y)
  if (nx || ny) return(nx && ny)
  if (is.numeric(x) && is.numeric(y)) return(abs(x - y) <= tol * max(1, abs(x)))
  identical(mr_norm_text(as.character(x)), mr_norm_text(as.character(y)))
}

mr_agree_chr <- function(v) if (is.null(v) || is.na(v)) NA_character_ else as.character(v)
