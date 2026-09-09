# dedup(): remove duplicate records ---------------------------------------

#' Remove duplicate records from a review
#'
#' Records are matched on any of the keys in `by`: DOI (case-insensitive),
#' PMID, PMCID, or normalised title (lower-cased, markup and punctuation
#' stripped, whitespace collapsed; year appended when known). The first
#' occurrence is kept.
#'
#' With `fuzzy = TRUE`, titles that are near-identical after normalisation are
#' also collapsed. A near-zero edit distance is a confident duplicate and is
#' removed; a borderline match (distance between `cutoff / 2` and `cutoff`) is
#' *kept* but flagged in `attr(x, "review_candidates")` for a manual look,
#' because one differing word in an otherwise identical title can be a
#' different study.
#'
#' @param x A `medrev` object.
#' @param by Keys to match on: any of `"doi"`, `"pmid"`, `"pmcid"`, `"title"`.
#' @param fuzzy Also collapse near-identical titles.
#' @param cutoff Maximum normalised title edit distance treated as a fuzzy
#'   match (0-1).
#'
#' @return The `medrev` object with duplicates removed, a `"deduplicated"`
#'   stage logged, and a `duplicate_audit` attribute listing what was dropped.
#' @export
#'
#' @examples
#' \dontrun{
#' r <- pubmed("crispr gene therapy", n = 300) |> dedup(fuzzy = TRUE)
#' attr(r, "duplicate_audit")
#' }
dedup <- function(x, by = c("doi", "pmid", "pmcid", "title"),
                  fuzzy = FALSE, cutoff = 0.08) {
  mr_assert(x)
  by <- match.arg(by, several.ok = TRUE)
  df <- mr_df(x)
  n <- nrow(df)
  if (n <= 1L) return(x)

  key_cols <- list(
    doi = ifelse(is.na(df$doi) | !nzchar(df$doi), NA_character_, paste0("doi:", tolower(df$doi))),
    pmid = ifelse(is.na(df$pmid) | !nzchar(df$pmid), NA_character_, paste0("pmid:", df$pmid)),
    pmcid = ifelse(is.na(df$pmcid) | !nzchar(df$pmcid), NA_character_, paste0("pmcid:", toupper(df$pmcid))),
    title = {
      t <- mr_norm_title(df$title)
      t[!is.na(df$year)] <- paste(t[!is.na(df$year)], df$year[!is.na(df$year)])
      ifelse(nzchar(t), paste0("title:", t), NA_character_)
    }
  )[by]

  keep <- rep(TRUE, n)
  dup_of <- rep(NA_integer_, n)
  dup_key <- rep(NA_character_, n)
  review <- list()
  seen <- integer(0)           # named: key -> first kept row index
  kept <- integer(0)

  review_cut <- cutoff / 2

  for (i in seq_len(n)) {
    keys <- unlist(lapply(key_cols, `[[`, i), use.names = FALSE)
    keys <- keys[!is.na(keys)]
    hit <- keys[keys %in% names(seen)]
    if (length(hit)) {
      keep[i] <- FALSE
      dup_of[i] <- seen[[hit[[1]]]]
      dup_key[i] <- hit[[1]]
      next
    }
    if (fuzzy && "title" %in% by && length(kept) && nzchar(df$title[[i]] %||% "")) {
      d <- mr_fuzzy_nearest(df$title[[i]], df$title[kept])
      if (!is.na(d$dist) && d$dist <= cutoff) {
        j <- kept[[d$which]]
        same_year <- is.na(df$year[[i]]) || is.na(df$year[[j]]) || df$year[[i]] == df$year[[j]]
        if (same_year && d$dist <= review_cut) {
          keep[i] <- FALSE
          dup_of[i] <- j
          dup_key[i] <- sprintf("fuzzy_title(%.3f)", d$dist)
          next
        }
        if (same_year) {
          review[[length(review) + 1L]] <- data.frame(
            id = df$id[[i]], title = df$title[[i]],
            candidate_id = df$id[[j]], candidate_title = df$title[[j]],
            dist = d$dist, stringsAsFactors = FALSE
          )
        }
      }
    }
    for (k in keys) seen[k] <- i
    kept <- c(kept, i)
  }

  removed <- sum(!keep)
  cli::cli_alert_info("Removed {removed} duplicate record{?s}; {sum(keep)} remain.")

  audit <- data.frame(
    id = df$id[!keep], title = df$title[!keep], doi = df$doi[!keep],
    matched_key = dup_key[!keep], kept_id = df$id[dup_of[!keep]],
    stringsAsFactors = FALSE
  )
  x <- mr_set_df(x, df[keep, , drop = FALSE])
  x <- mr_stage(x, "deduplicated",
                sprintf("removed %d by %s%s", removed, paste(by, collapse = "+"),
                        if (fuzzy) " + fuzzy" else ""))
  attr(x, "duplicate_audit") <- audit
  attr(x, "review_candidates") <- if (length(review)) do.call(rbind, review) else
    data.frame(id = character(), title = character(), candidate_id = character(),
               candidate_title = character(), dist = numeric(), stringsAsFactors = FALSE)
  x
}

# Nearest normalised-title match of `one` within vector `pool`: list(which, dist).
mr_fuzzy_nearest <- function(one, pool) {
  a <- mr_norm_title(one)
  b <- mr_norm_title(pool)
  if (!nzchar(a) || !length(b)) return(list(which = NA_integer_, dist = NA_real_))
  d <- as.integer(utils::adist(a, b, ignore.case = TRUE))
  nd <- d / max(nchar(a), 1L)
  w <- which.min(nd)
  list(which = w, dist = nd[[w]])
}

#' Fuzzy-match one title against a set of candidates
#'
#' Exposes the normalised edit-distance matcher [dedup()] uses.
#'
#' @param x A single title string.
#' @param choices A character vector of candidate titles.
#' @param cutoff Maximum normalised distance counted as a match.
#'
#' @return A data frame with `choice`, `dist`, and `match` columns, ordered by
#'   distance.
#' @export
#'
#' @examples
#' fuzzy_match(
#'   "Interpretable machine learning for cancer prognosis",
#'   c("Interpretable ML for cancer prognosis", "An unrelated title")
#' )
fuzzy_match <- function(x, choices, cutoff = 0.1) {
  mr_check_string(x, "x")
  a <- mr_norm_title(x)
  b <- mr_norm_title(choices)
  d <- as.integer(utils::adist(a, b, ignore.case = TRUE))
  nd <- d / max(nchar(a), 1L)
  ord <- order(nd)
  data.frame(choice = choices[ord], dist = nd[ord], match = nd[ord] <= cutoff,
             stringsAsFactors = FALSE)
}
