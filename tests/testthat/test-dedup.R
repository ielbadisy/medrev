test_that("exact dedup removes the repeated DOI", {
  r <- dedup(fake_review(), fuzzy = FALSE)
  df <- as.data.frame(r)
  expect_equal(nrow(df), 4L)                       # 5 -> 4 (one dup DOI)
  aud <- attr(r, "duplicate_audit")
  expect_equal(nrow(aud), 1L)
  expect_equal(aud$kept_id, "doi:10.1/a")
})

test_that("fuzzy dedup is a no-op here (titles already exact-match via DOI)", {
  r <- dedup(fake_review(), fuzzy = TRUE)
  expect_equal(nrow(as.data.frame(r)), 4L)
})

test_that("fuzzy dedup collapses near-identical titles without a shared id", {
  df <- data.frame(
    id = c("pmid:1", "pmid:2"), source = "europepmc",
    pmid = c("1", "2"), pmcid = NA_character_, doi = NA_character_,
    title = c("Deep learning for early sepsis prediction",
              "Deep learning for early sepsis prediction."),
    abstract = NA_character_, authors = "A", journal = "J", year = c(2021L, 2021L),
    type = NA_character_, keywords = NA_character_, mesh = NA_character_,
    language = "eng", is_oa = FALSE, license = NA_character_,
    url = NA_character_, pdf_url = NA_character_, pdf_source = NA_character_,
    oa_status = NA_character_, decision = NA_character_, reason = NA_character_,
    screened_at = NA_character_, retrieved_at = "2026-01-01",
    stringsAsFactors = FALSE
  )
  r <- medrev:::new_medrev(df)
  r <- medrev:::mr_stage(r, "identified", "t")
  out <- dedup(r, fuzzy = TRUE)
  expect_equal(nrow(as.data.frame(out)), 1L)
})

test_that("stage log records the deduplication step", {
  r <- dedup(fake_review())
  st <- as.data.frame(r$stages)
  expect_true("deduplicated" %in% st$stage)
})

test_that("fuzzymatch ranks candidates by normalised distance", {
  res <- fuzzymatch("Interpretable machine learning for cancer prognosis",
                     c("An unrelated title",
                       "Interpretable ML for cancer prognosis"))
  expect_equal(res$choice[1], "Interpretable ML for cancer prognosis")
  expect_true(res$dist[1] < res$dist[2])
})

test_that("fuzzymatch distance is symmetric and bounded by 1", {
  a <- "Sepsis"
  b <- "Deep learning for early sepsis prediction in the ICU"
  d_ab <- fuzzymatch(a, b)$dist
  d_ba <- fuzzymatch(b, a)$dist
  expect_equal(d_ab, d_ba)
  expect_true(d_ab >= 0 && d_ab <= 1)
  expect_equal(fuzzymatch(a, a)$dist, 0)
  expect_lte(max(fuzzymatch("ab", c("xyzxyzxyz", "a long other title"))$dist), 1)
})

test_that("fuzzy dedup drops at cutoff / 2 and flags up to cutoff", {
  mk <- function(titles) {
    n <- length(titles)
    df <- data.frame(
      id = paste0("pmid:", seq_len(n)), source = "europepmc",
      pmid = as.character(seq_len(n)), pmcid = NA_character_, doi = NA_character_,
      title = titles, abstract = NA_character_, authors = "A", journal = "J",
      year = 2021L, type = NA_character_, keywords = NA_character_,
      mesh = NA_character_, language = "eng", is_oa = FALSE,
      license = NA_character_, url = NA_character_, pdf_url = NA_character_,
      pdf_source = NA_character_, oa_status = NA_character_,
      decision = NA_character_, reason = NA_character_,
      screened_at = NA_character_, retrieved_at = "2026-01-01",
      stringsAsFactors = FALSE
    )
    medrev:::mr_stage(medrev:::new_medrev(df), "identified", "t")
  }
  base <- "abcdefghijklmnopqrst"           # 20 characters
  one_off <- "abcdefghijklmnopqrsX"        # d = 1/20 = 0.05
  two_off <- "abcdefghijklmnopqXYt"        # d = 2/20 = 0.10
  # cutoff 0.12: 0.05 <= 0.06 is dropped
  out <- suppressMessages(dedup(mk(c(base, one_off)), fuzzy = TRUE, cutoff = 0.12))
  expect_equal(nrow(as.data.frame(out)), 1L)
  # cutoff 0.12: 0.10 is in (0.06, 0.12], kept and flagged
  out <- suppressMessages(dedup(mk(c(base, two_off)), fuzzy = TRUE, cutoff = 0.12))
  expect_equal(nrow(as.data.frame(out)), 2L)
  expect_equal(nrow(attr(out, "review_candidates")), 1L)
  expect_equal(attr(out, "review_candidates")$dist, 0.10)
  # cutoff 0.08: 0.10 is a different record, neither dropped nor flagged
  out <- suppressMessages(dedup(mk(c(base, two_off)), fuzzy = TRUE, cutoff = 0.08))
  expect_equal(nrow(as.data.frame(out)), 2L)
  expect_equal(nrow(attr(out, "review_candidates")), 0L)
})
