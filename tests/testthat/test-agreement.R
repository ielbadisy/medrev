q <- function(v) list(value = v, quote = NA)

test_that("effect rows are matched by outcome/timepoint/measure, not position", {
  a <- effect_insights(list("pmid:1" = list(
    list(outcome = q("Death"), timepoint = q("28 days"), measure = q("OR"), estimate = q(0.71)),
    list(outcome = q("ICU stay"), measure = q("MD"), estimate = q(-1.2)))))
  b <- effect_insights(list("pmid:1" = list(
    list(outcome = q("ICU stay"), measure = q("MD"), estimate = q(-1.5)),
    list(outcome = q("death"), timepoint = q("28 days"), measure = q("OR"), estimate = q(0.7100000001)),
    list(outcome = q("stroke"), measure = q("OR"), estimate = q(2)))))
  ag <- as.data.frame(agreement(a, b, type = "effect"))
  est <- ag[ag$field == "estimate", ]
  expect_equal(est$status[grepl("^death", est$key)], "agree")
  expect_equal(est$status[grepl("^icu", est$key)], "disagree")
  expect_true(all(ag$status[grepl("^stroke", ag$key)] == "only_b"))
})

test_that("study rows compare text loosely and NA with NA as agreement", {
  mk <- function(design, n, country) medrev:::new_medrev_insights(data.frame(
    insight_id = "ext-0001", type = "extraction", record_id = "pmid:1", chunk_ids = "pmid:1#01",
    n_chunks = 1L, text = NA_character_,
    data = medrev:::mr_to_json(list(design = q(design), n_total = q(n), country = q(country))),
    model = "t", at = "x", stringsAsFactors = FALSE))
  ag <- as.data.frame(agreement(mk("RCT", 120L, NA), mk("rct", 121L, NA)))
  expect_equal(ag$status[ag$field == "design"], "agree")
  expect_equal(ag$status[ag$field == "n_total"], "disagree")
  expect_equal(ag$status[ag$field == "country"], "agree")
  ag2 <- as.data.frame(agreement(mk("RCT", 120L, NA), mk("RCT", 121L, NA), tol = 0.01))
  expect_equal(ag2$status[ag2$field == "n_total"], "agree")
})

test_that("agreement() validates inputs and handles empty input", {
  expect_error(agreement(data.frame(), medrev:::mr_empty_insights()), "medrev_insights")
  expect_error(agreement(medrev:::mr_empty_insights(), medrev:::mr_empty_insights(), tol = -1), "tol")
  expect_equal(nrow(as.data.frame(agreement(medrev:::mr_empty_insights(), medrev:::mr_empty_insights()))), 0L)
})
