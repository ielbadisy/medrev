# review_brief(): assemble a Markdown evidence brief -----------------

#' Assemble a rapid evidence brief
#'
#' Builds a Markdown brief from a review: the search log, the PRISMA-style
#' flow, an optional synthesis and per-study notes carried in `insights`, and
#' a numbered reference list of the included studies. No model call is made
#' here; pass the output of [summarize()] / [extract()] / [synthesize()] via
#' `insights`.
#'
#' @param x A `medrev` object.
#' @param insights Optional `medrev_insights` from the enrichment verbs.
#' @param title Brief title.
#' @param file Optional path to write the Markdown to. When given, the path is
#'   returned invisibly; otherwise a `medrev_brief` object is returned.
#'
#' @return A `medrev_brief` object (or `file`, invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' r   <- pubmed("sepsis prognosis machine learning", n = 30) |> dedup()
#' ins <- summarize(chunk(r), llm = my_model)
#' syn <- synthesize(ins, llm = my_model)
#' review_brief(r, insights = c(ins, syn), title = "Sepsis prognosis: rapid brief",
#'              file = "brief.md")
#' }
review_brief <- function(x, insights = NULL, title = "Rapid evidence brief",
                         file = NULL) {
  mr_assert(x)
  mr_check_string(title, "title")
  ins <- if (is.null(insights)) as.data.frame(mr_empty_insights()) else as.data.frame(insights)

  df <- mr_df(x)
  inc <- df[mr_which_rows(df, "included"), , drop = FALSE]

  sections <- list()
  sections[["Search"]] <- mr_md_list(sprintf(
    "**%s** (%s) - %d retrieved of %s",
    x$searches$query, x$searches$source, x$searches$retrieved,
    ifelse(is.na(x$searches$hits), "?", format(x$searches$hits, big.mark = ","))
  ))

  p <- prisma(x)
  sections[["Study flow"]] <- mr_md_list(c(
    sprintf("records identified: %d", p[["identified"]]),
    sprintf("duplicates removed: %d", p[["duplicates_removed"]]),
    sprintf("records screened: %d", p[["screened"]]),
    sprintf("excluded: %d", p[["excluded"]]),
    sprintf("studies included: %d", p[["included"]]),
    sprintf("included with an open-access PDF: %d", p[["included_with_pdf"]])
  ))

  syn <- ins$text[ins$type == "synthesis"]
  if (length(syn) && !is.na(syn[[1]])) sections[["Synthesis"]] <- syn[[1]]

  sections[["Included studies"]] <- mr_brief_studies(inc, ins)

  refs <- bibliography(x, which = "included")
  sections[["References"]] <- if (length(refs)) {
    paste(sprintf("%d. %s", seq_along(refs), refs), collapse = "\n")
  } else "_none_"

  brief <- structure(
    list(title = title, sections = sections, at = mr_now(),
         n_included = nrow(inc)),
    class = "medrev_brief"
  )
  if (is.null(file)) return(brief)
  mr_check_string(file, "file")
  writeLines(format(brief), file)
  cli::cli_alert_success("Brief written to {.path {file}}.")
  invisible(file)
}

# Per-study block: citation + summary text and/or extracted fields.
mr_brief_studies <- function(inc, ins) {
  if (!nrow(inc)) return("_none_")
  sm <- ins[ins$type == "summary", c("record_id", "text")]
  ex <- ins[ins$type == "extraction", c("record_id", "data")]
  blocks <- vapply(seq_len(nrow(inc)), function(i) {
    rid <- inc$id[[i]]
    cite <- mr_cite(inc[i, ])
    out <- paste0("**", cite, "**")
    s <- sm$text[sm$record_id == rid]
    if (length(s) && !is.na(s[[1]])) out <- paste0(out, "\n\n", s[[1]])
    e <- ex$data[ex$record_id == rid]
    if (length(e) && !is.na(e[[1]])) {
      obj <- mr_insight_rows(e[[1]], "extraction")
      if (length(obj)) {
        flat <- mr_flatten_obj(obj[[1]])
        kv <- paste(sprintf("- **%s:** %s", names(flat),
                            vapply(flat, function(v) if (is.na(v)) "n/r" else as.character(v),
                                   character(1))),
                    collapse = "\n")
        out <- paste0(out, "\n\n", kv)
      }
    }
    out
  }, character(1))
  paste(blocks, collapse = "\n\n---\n\n")
}

mr_md_list <- function(items) paste(sprintf("- %s", items), collapse = "\n")

#' @export
format.medrev_brief <- function(x, ...) {
  parts <- c(
    sprintf("# %s", x$title),
    sprintf("_Generated %s_", x$at),
    unlist(lapply(names(x$sections), function(nm) {
      c(sprintf("## %s", nm), x$sections[[nm]])
    }))
  )
  paste(parts, collapse = "\n\n")
}

#' @export
print.medrev_brief <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}

#' @export
as.character.medrev_brief <- function(x, ...) format(x)
