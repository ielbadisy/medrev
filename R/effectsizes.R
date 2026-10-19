# effectsizes(): meta-analysis-ready effect table ---------------------
# Turns extracted effect rows into one effect size per row on the analysis
# scale (log for ratio measures), with its standard error and a record of
# how it was obtained. Pooling is left to meta / metafor.

.mr_ratio <- c("OR", "RR", "HR")

#' Effect sizes from extracted effect data
#'
#' Computes one effect size per extracted effect row (from
#' `extract(form = form("ma"))`), on the scale used for pooling: log for
#' ratio measures (OR, RR, HR), raw for differences (RD, MD, SMD). The
#' estimate is taken from what the study reports, in this order of
#' preference (reversed with `prefer = "computed"`):
#'
#' * **ci**: reported estimate and confidence interval;
#' * **p**: reported estimate and p-value (SE back-calculated from the
#'   normal approximation);
#' * **counts**: events and totals per arm (OR by default, or RR / RD when
#'   that is the reported measure); a continuity correction `cc` is added to
#'   all cells when one is zero, and rows with no events or all events in
#'   both arms are left `NA`;
#' * **means**: means, SDs and totals per arm (MD by default, or Hedges' g (exact
#'   small-sample correction) when the reported measure is SMD).
#'
#' The result plugs straight into `meta::metagen(TE = yi, seTE = sei)` or
#' `metafor::rma(yi, vi)`.
#'
#' @param insights A `medrev_insights` object with `type == "effect"` rows.
#' @param study How records map to studies, for studies reported in several
#'   records: `NULL` (each record is a study), a named character vector
#'   `c(record_id = "study label")`, or a `data.frame` with columns
#'   `record_id` and `study`.
#' @param prefer `"reported"` (default) or `"computed"`.
#' @param level Confidence level of the reported intervals.
#' @param cc Continuity correction for zero cells.
#'
#' @return A `basetable`, one row per effect row, with `study`,
#'   `record_id`, `insight_id`, `row`, `outcome`, `timepoint`, `group_e`,
#'   `group_c`, `analysis`, `measure`, `n_e`, `n_c`, `estimate`, `ci_low`,
#'   `ci_high`, `yi`, `sei`, `vi`, `scale` (`"log"` or `"raw"`), `derived`
#'   (`"ci"`, `"p"`, `"counts"`, `"means"`, or `NA` when no effect could be
#'   obtained) and `note`.
#' @export
#'
#' @examples
#' \dontrun{
#' es <- effectsizes(extract(ch, my_model, form = form("ma", "binary")))
#' m <- metafor::rma(yi, vi, data = es[es$outcome == "mortality", ])
#' }
effectsizes <- function(insights, study = NULL, prefer = c("reported", "computed"),
                        level = 0.95, cc = 0.5) {
  prefer <- match.arg(prefer)
  if (!is.numeric(level) || length(level) != 1L || level <= 0 || level >= 1) {
    mr_stop("{.arg level} must be a single number between 0 and 1.")
  }
  tab <- as.data.frame(extractions(insights, type = "effect"))
  cols <- c("outcome", "timepoint", "group_e", "group_c", "analysis", "measure",
            "n_e", "n_c", "events_e", "events_c", "mean_e", "sd_e", "mean_c",
            "sd_c", "estimate", "ci_low", "ci_high", "p_value")
  num <- setdiff(cols, c("outcome", "timepoint", "group_e", "group_c", "analysis", "measure"))
  for (cl in setdiff(cols, names(tab))) tab[[cl]] <- rep(NA, nrow(tab))
  for (cl in num) tab[[cl]] <- suppressWarnings(as.numeric(tab[[cl]]))
  for (cl in setdiff(cols, num)) tab[[cl]] <- as.character(tab[[cl]])
  tab <- tab[!is.na(tab$row), , drop = FALSE]

  z <- stats::qnorm(1 - (1 - level) / 2)
  res <- lapply(seq_len(nrow(tab)), function(i) mr_effect_row(tab[i, ], prefer, z, cc))
  res <- if (length(res)) do.call(rbind, res) else
    data.frame(measure = character(), yi = numeric(), sei = numeric(),
               derived = character(), note = character())

  out <- data.frame(
    study = mr_study_labels(tab$record_id, study),
    record_id = tab$record_id, insight_id = tab$insight_id, row = tab$row,
    outcome = tab$outcome, timepoint = tab$timepoint,
    group_e = tab$group_e, group_c = tab$group_c, analysis = tab$analysis,
    measure = res$measure, n_e = tab$n_e, n_c = tab$n_c,
    estimate = tab$estimate, ci_low = tab$ci_low, ci_high = tab$ci_high,
    yi = res$yi, sei = res$sei, vi = res$sei^2,
    scale = ifelse(res$measure %in% .mr_ratio, "log", ifelse(is.na(res$measure), NA, "raw")),
    derived = res$derived, note = res$note,
    stringsAsFactors = FALSE
  )
  n_na <- sum(is.na(out$yi))
  if (nrow(out)) {
    cli::cli_alert_success("{nrow(out) - n_na} of {nrow(out)} effect row{?s} with an effect size.")
  } else {
    cli::cli_alert_warning("No effect rows to compute.")
  }
  basetable::as_basetable(out)
}

# One row -> list(measure, yi, sei, derived, note).
mr_effect_row <- function(r, prefer, z, cc) {
  m <- toupper(r$measure)
  if (!m %in% .mr_measures) m <- NA_character_
  order <- c("ci", "p", "counts", "means")
  if (prefer == "computed") order <- c("counts", "means", "ci", "p")
  notes <- character()
  for (how in order) {
    got <- switch(how,
      ci = mr_es_ci(r, m, z),
      p = mr_es_p(r, m),
      counts = mr_es_counts(r, m, cc),
      means = mr_es_means(r, m)
    )
    if (!is.null(got$note)) notes <- c(notes, got$note)
    if (!is.null(got$yi) && is.finite(got$yi) && is.finite(got$sei) && got$sei > 0) {
      return(data.frame(measure = got$measure, yi = got$yi, sei = got$sei, derived = how,
                        note = if (length(notes)) paste(unique(notes), collapse = "; ") else NA_character_,
                        stringsAsFactors = FALSE))
    }
  }
  data.frame(measure = m, yi = NA_real_, sei = NA_real_, derived = NA_character_,
             note = paste(unique(c(notes, "not enough data for an effect size")), collapse = "; "),
             stringsAsFactors = FALSE)
}

mr_es_ci <- function(r, m, z) {
  est <- r$estimate; lo <- r$ci_low; hi <- r$ci_high
  if (is.na(m) || any(is.na(c(est, lo, hi)))) return(NULL)
  if (lo > hi) { tmp <- lo; lo <- hi; hi <- tmp }
  if (m %in% .mr_ratio) {
    if (any(c(est, lo, hi) <= 0)) return(list(note = "non-positive ratio or CI bound"))
    return(list(measure = m, yi = log(est), sei = (log(hi) - log(lo)) / (2 * z)))
  }
  list(measure = m, yi = est, sei = (hi - lo) / (2 * z))
}

mr_es_p <- function(r, m) {
  est <- r$estimate; p <- r$p_value
  if (is.na(m) || is.na(est) || is.na(p) || p <= 0 || p >= 1) return(NULL)
  yi <- if (m %in% .mr_ratio) { if (est <= 0) return(NULL); log(est) } else est
  if (yi == 0) return(NULL)
  list(measure = m, yi = yi, sei = abs(yi) / stats::qnorm(1 - p / 2),
       note = "SE from p-value")
}

mr_es_counts <- function(r, m, cc) {
  a <- r$events_e; n1 <- r$n_e; c <- r$events_c; n2 <- r$n_c
  if (any(is.na(c(a, n1, c, n2)))) return(NULL)
  if (n1 <= 0 || n2 <= 0 || a < 0 || c < 0 || a > n1 || c > n2) {
    return(list(note = "impossible counts"))
  }
  note <- if (!is.na(m) && !m %in% c("OR", "RR", "RD")) sprintf("OR from counts (reported measure %s)", m)
  m <- if (m %in% c("OR", "RR", "RD")) m else "OR"
  b <- n1 - a; d <- n2 - c
  if (m == "RD") {
    p1 <- a / n1; p2 <- c / n2
    return(list(measure = m, yi = p1 - p2, sei = sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2), note = note))
  }
  if ((a == 0 && c == 0) || (b == 0 && d == 0)) {
    return(list(note = "no events (or all events) in both arms"))
  }
  if (any(c(a, b, c, d) == 0)) {
    a <- a + cc; b <- b + cc; c <- c + cc; d <- d + cc
    n1 <- a + b; n2 <- c + d
    note <- c(note, sprintf("continuity correction %s", cc))
  }
  if (m == "OR") {
    return(list(measure = m, yi = log(a * d / (b * c)), sei = sqrt(1 / a + 1 / b + 1 / c + 1 / d), note = note))
  }
  list(measure = m, yi = log((a / n1) / (c / n2)), sei = sqrt(1 / a - 1 / n1 + 1 / c - 1 / n2), note = note)
}

mr_es_means <- function(r, m) {
  m1 <- r$mean_e; s1 <- r$sd_e; n1 <- r$n_e; m2 <- r$mean_c; s2 <- r$sd_c; n2 <- r$n_c
  if (any(is.na(c(m1, s1, n1, m2, s2, n2)))) return(NULL)
  if (n1 < 2 || n2 < 2 || s1 < 0 || s2 < 0) return(list(note = "impossible SD or n"))
  if (identical(m, "SMD")) {
    sp <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
    if (sp == 0) return(NULL)
    df <- n1 + n2 - 2
    g <- exp(lgamma(df / 2) - log(sqrt(df / 2)) - lgamma((df - 1) / 2)) * (m1 - m2) / sp
    return(list(measure = "SMD", yi = g, sei = sqrt(1 / n1 + 1 / n2 + g^2 / (2 * (n1 + n2)))))
  }
  list(measure = "MD", yi = m1 - m2, sei = sqrt(s1^2 / n1 + s2^2 / n2))
}

# record ids -> study labels.
mr_study_labels <- function(ids, study) {
  if (is.null(study)) return(ids)
  if (is.data.frame(study)) {
    if (!all(c("record_id", "study") %in% names(study))) {
      mr_stop("{.arg study} must have columns {.val record_id} and {.val study}.")
    }
    study <- stats::setNames(as.character(study$study), study$record_id)
  }
  if (!is.character(study) || is.null(names(study))) {
    mr_stop("{.arg study} must be a named character vector or a data.frame.")
  }
  out <- unname(study[ids])
  ifelse(is.na(out), ids, out)
}
