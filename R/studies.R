# studies(): link records that report the same study ------------------

#' Group records into studies by trial registration
#'
#' One trial is often reported in several articles (main results, secondary
#' outcomes, long-term follow-up). Counting each as a study double-counts
#' its participants in a meta-analysis. `studies()` groups records whose
#' extracted `registration` matches (ignoring case and spaces), so the result
#' can be passed to `effectsizes(study = )`. Check the grouping by hand:
#' records without a registration stay on their own.
#'
#' @param insights A `medrev_insights` object holding study extractions
#'   from `extract(form = form("sr"))` (any form with a `registration`
#'   field).
#' @param label `"registration"` to label grouped studies by their
#'   registration number, or `"first"` to use the first record id.
#'
#' @return A `data.frame` with `record_id`, `study` and `n_records` (records
#'   in that study).
#' @export
#'
#' @examples
#' \dontrun{
#' sr <- extract(ch, my_model, form = form("sr"))
#' st <- studies(sr)
#' es <- effectsizes(ma, study = st)
#' }
studies <- function(insights, label = c("registration", "first")) {
  label <- match.arg(label)
  tab <- as.data.frame(extractions(insights, type = "extraction"))
  tab <- tab[!is.na(tab$record_id), , drop = FALSE]
  tab <- tab[!duplicated(tab$record_id), , drop = FALSE]
  if (!nrow(tab)) {
    return(data.frame(record_id = character(), study = character(), n_records = integer()))
  }
  reg <- if ("registration" %in% names(tab)) toupper(gsub("\\s", "", as.character(tab$registration))) else rep(NA_character_, nrow(tab))
  reg[!is.na(reg) & !nzchar(reg)] <- NA_character_
  key <- ifelse(is.na(reg), tab$record_id, reg)
  study <- if (label == "registration") key else tab$record_id[match(key, key)]
  n <- as.integer(table(key)[key])
  out <- data.frame(record_id = tab$record_id, study = study, n_records = n, stringsAsFactors = FALSE)
  n_multi <- length(unique(key[n > 1L]))
  if (n_multi) cli::cli_alert_info("{n_multi} stud{?y/ies} reported in more than one record.")
  out
}
