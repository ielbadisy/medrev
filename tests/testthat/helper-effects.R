# Effect insights built from a list of row lists (one element per record).
effect_insights <- function(rows_by_record) {
  ids <- names(rows_by_record) %||% paste0("pmid:", seq_along(rows_by_record))
  df <- data.frame(
    insight_id = sprintf("eff-%04d", seq_along(ids)), type = "effect",
    record_id = ids, chunk_ids = paste0(ids, "#01"), n_chunks = 1L,
    text = NA_character_,
    data = vapply(rows_by_record, function(r) medrev:::mr_to_json(list(rows = r)), ""),
    model = "test", at = "2026-10-19", stringsAsFactors = FALSE
  )
  medrev:::new_medrev_insights(df)
}
`%||%` <- function(a, b) if (is.null(a)) b else a
