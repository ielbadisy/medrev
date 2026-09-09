# synthesize(): a cross-study synthesis from insights --------------

.mr_synth_prompt <- paste0(
  "You are synthesising a body of biomedical evidence from the per-study ",
  "notes below. Write 2-3 short paragraphs covering: the main findings ",
  "across studies, where they agree and where they disagree, and the key ",
  "gaps or limitations. Use ONLY the notes; do not introduce outside ",
  "information.\n\nNOTES:\n{evidence}"
)

#' Synthesise insights across studies
#'
#' Feeds the per-study summaries and/or extractions from [summarize()] /
#' [extract()] to the model and asks for a cross-study synthesis.
#'
#' @param insights A `medrev_insights` object.
#' @param llm A model callback: `function(prompt, ...)` returning a string.
#' @param prompt Optional prompt template containing `{evidence}`.
#' @param model A short label recorded in the `model` column.
#'
#' @return A `medrev_insights` object with a single `type == "synthesis"` row.
#' @export
#'
#' @examples
#' \dontrun{
#' ins  <- summarize(chunk(r), llm = my_model)
#' syn  <- synthesize(ins, llm = my_model)
#' }
synthesize <- function(insights, llm, prompt = NULL, model = "user-callback") {
  if (!inherits(insights, "medrev_insights")) {
    mr_stop("{.arg insights} must be a {.cls medrev_insights} object.")
  }
  mr_check_llm(llm)
  template <- prompt %||% .mr_synth_prompt
  if (!grepl("{evidence}", template, fixed = TRUE)) {
    mr_stop("{.arg prompt} must contain the literal {{evidence}} placeholder.")
  }

  d <- as.data.frame(insights)
  d <- d[d$type %in% c("summary", "extraction"), , drop = FALSE]
  if (!nrow(d)) {
    cli::cli_alert_warning("No summary or extraction insights to synthesise.")
    return(mr_empty_insights())
  }

  notes <- vapply(seq_len(nrow(d)), function(i) {
    body <- if (!is.na(d$text[[i]])) d$text[[i]] else d$data[[i]]
    rid <- d$record_id[[i]]
    sprintf("- (%s) %s", if (is.na(rid)) "study" else rid, body)
  }, character(1))

  cli::cli_progress_step("Synthesising {nrow(d)} note{?s}")
  resp <- mr_call_llm(llm, mr_render_prompt(template, paste(notes, collapse = "\n")))
  cli::cli_progress_done()

  new_medrev_insights(data.frame(
    insight_id = "syn-0001", type = "synthesis", record_id = NA_character_,
    chunk_ids = NA_character_, n_chunks = nrow(d), text = resp,
    data = NA_character_, model = model, at = mr_now(),
    stringsAsFactors = FALSE
  ))
}
