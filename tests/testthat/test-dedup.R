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

test_that("fuzzy_match ranks candidates by normalised distance", {
  res <- fuzzy_match("Interpretable machine learning for cancer prognosis",
                     c("An unrelated title",
                       "Interpretable ML for cancer prognosis"))
  expect_equal(res$choice[1], "Interpretable ML for cancer prognosis")
  expect_true(res$dist[1] < res$dist[2])
})
