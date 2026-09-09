# The enrichment layer: model-generated content -----------------------
#
# Generated content is kept in its own `medrev_insights` table and never
# written back into a review's records. A "model" is any function
#   function(prompt, ...) -> character(1)
# that takes a prompt string and returns the model's text response. medrev
# builds the prompts and parses the responses; the caller supplies the call.

#' A deterministic stub model
#'
#' A stand-in "model" for examples and tests: it returns a fixed string that
#' encodes the prompt size instead of calling any service. Use it anywhere a
#' `llm` argument is expected.
#'
#' @param prompt A prompt string.
#' @param ... Ignored.
#'
#' @return A single string.
#' @export
#'
#' @examples
#' llm_echo("summarise this evidence")
llm_echo <- function(prompt, ...) {
  nw <- length(strsplit(trimws(prompt), "\\s+")[[1]])
  sprintf("[echo] response to a %d-word prompt.", nw)
}

.mr_insight_cols <- c("insight_id", "type", "record_id", "chunk_ids",
                      "n_chunks", "text", "data", "model", "at")

new_medrev_insights <- function(df) {
  for (col in setdiff(.mr_insight_cols, names(df))) df[[col]] <- NA
  df <- df[, .mr_insight_cols, drop = FALSE]
  rownames(df) <- NULL
  structure(basetable::as_basetable(df),
            class = c("medrev_insights", "basetable", "data.frame"))
}

mr_empty_insights <- function() {
  new_medrev_insights(data.frame(
    insight_id = character(), type = character(), record_id = character(),
    chunk_ids = character(), n_chunks = integer(), text = character(),
    data = character(), model = character(), at = character(),
    stringsAsFactors = FALSE
  ))
}

#' @export
print.medrev_insights <- function(x, ...) {
  d <- as.data.frame(x)
  tab <- table(factor(d$type))
  parts <- if (length(tab)) paste(sprintf("%s %d", names(tab), as.integer(tab)), collapse = ", ") else "none"
  cat(sprintf("<medrev_insights> %d insight%s (%s)\n",
              nrow(d), if (nrow(d) == 1) "" else "s", parts))
  NextMethod()
}

#' @export
as.data.frame.medrev_insights <- function(x, ...) {
  class(x) <- "data.frame"
  x
}

# Validate a model callback.
mr_check_llm <- function(llm) {
  if (!is.function(llm)) {
    mr_stop("{.arg llm} must be a function {.code function(prompt, ...)} returning a string.")
  }
  invisible(llm)
}

# Call the model, coercing / guarding the response to a single string.
mr_call_llm <- function(llm, prompt) {
  out <- tryCatch(llm(prompt), error = function(e) paste0("<model error: ", conditionMessage(e), ">"))
  out <- paste(as.character(out), collapse = "\n")
  if (!nzchar(trimws(out))) out <- "<empty model response>"
  out
}

# Render a "{evidence}" template (string) against joined chunk text.
mr_render_prompt <- function(template, evidence) {
  gsub("{evidence}", evidence, template, fixed = TRUE)
}
