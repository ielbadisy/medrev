# summarize(): model-assisted grounded summaries ---------------------

.mr_summary_prompt <- paste0(
  "You are summarising biomedical evidence. Using ONLY the excerpts below, ",
  "write a concise summary (3-5 sentences) covering the study's design, ",
  "population or setting, main quantitative result, and any stated ",
  "limitations. Do not add anything that is not supported by the excerpts.\n\n",
  "EXCERPTS:\n{evidence}"
)

#' Summarise evidence with a model
#'
#' Groups chunks (by record, or all together), builds a grounded prompt from
#' their text, and calls the supplied model. Output is stored as a
#' `medrev_insights` table and never written back into the review's records.
#'
#' @param chunks A `medrev_chunks` object from [chunk()].
#' @param llm A model callback: `function(prompt, ...)` returning a string. Use
#'   [llm_echo()] to try the plumbing without a real model.
#' @param prompt Optional prompt template; must contain the literal `{evidence}`
#'   placeholder, which is replaced with the joined chunk text.
#' @param per `"record"` for one summary per record, `"all"` for a single
#'   summary over every chunk.
#' @param model A short label recorded in the `model` column.
#'
#' @return A `medrev_insights` object with `type == "summary"`.
#' @export
#'
#' @examples
#' \dontrun{
#' r  <- pubmed("sepsis prognosis machine learning", n = 20)
#' ins <- summarize(chunk(r), llm = my_model)
#' }
summarize <- function(chunks, llm, prompt = NULL, per = c("record", "all"),
                      model = "user-callback") {
  if (!inherits(chunks, "medrev_chunks")) {
    mr_stop("{.arg chunks} must be a {.cls medrev_chunks} object (from {.fn chunk}).")
  }
  mr_check_llm(llm)
  per <- match.arg(per)
  template <- prompt %||% .mr_summary_prompt
  if (!grepl("{evidence}", template, fixed = TRUE)) {
    mr_stop("{.arg prompt} must contain the literal {{evidence}} placeholder.")
  }

  d <- as.data.frame(chunks)
  if (!nrow(d)) {
    cli::cli_alert_warning("No chunks to summarise.")
    return(mr_empty_insights())
  }

  groups <- if (per == "record") split(d, d$record_id) else list(all = d)
  rows <- vector("list", length(groups))
  cli::cli_progress_bar("Summarising", total = length(groups))
  for (k in seq_along(groups)) {
    cli::cli_progress_update()
    g <- groups[[k]]
    g <- g[order(g$seq), , drop = FALSE]
    evidence <- paste(sprintf("[%s] %s", g$chunk_id, g$text), collapse = "\n\n")
    resp <- mr_call_llm(llm, mr_render_prompt(template, evidence))
    rows[[k]] <- data.frame(
      insight_id = sprintf("sum-%04d", k),
      type = "summary",
      record_id = if (per == "record") g$record_id[[1]] else NA_character_,
      chunk_ids = paste(g$chunk_id, collapse = ";"),
      n_chunks = nrow(g),
      text = resp,
      data = NA_character_,
      model = model,
      at = mr_now(),
      stringsAsFactors = FALSE
    )
  }
  cli::cli_progress_done()
  cli::cli_alert_success("{length(groups)} summary/summaries generated.")
  new_medrev_insights(do.call(rbind, rows))
}
