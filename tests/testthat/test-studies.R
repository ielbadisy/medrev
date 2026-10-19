sr_ins <- function(ids, regs) {
  medrev:::new_medrev_insights(data.frame(
    insight_id = sprintf("ext-%04d", seq_along(ids)), type = "extraction", record_id = ids,
    chunk_ids = paste0(ids, "#01"), n_chunks = 1L, text = NA_character_,
    data = vapply(regs, function(r) medrev:::mr_to_json(list(registration = list(value = r, quote = NA))), "", USE.NAMES = FALSE),
    model = "t", at = "x", stringsAsFactors = FALSE))
}

test_that("records sharing a registration form one study", {
  st <- studies(sr_ins(c("pmid:1", "pmid:2", "pmid:3", "pmid:4"), c("NCT01", "nct 01", NA, "NCT02")))
  expect_equal(st$study, c("NCT01", "NCT01", "pmid:3", "NCT02"))
  expect_equal(st$n_records, c(2L, 2L, 1L, 1L))
  st2 <- studies(sr_ins(c("pmid:1", "pmid:2"), c("NCT01", "NCT01")), label = "first")
  expect_equal(st2$study, c("pmid:1", "pmid:1"))
})

test_that("studies() output feeds effectsizes(study =)", {
  st <- studies(sr_ins(c("pmid:1", "pmid:2"), c("NCT01", "NCT01")))
  q <- function(v) list(value = v, quote = NA)
  ma <- effect_insights(list("pmid:1" = list(list(measure = q("MD"), estimate = q(1), ci_low = q(0), ci_high = q(2))),
                             "pmid:2" = list(list(measure = q("MD"), estimate = q(2), ci_low = q(1), ci_high = q(3)))))
  expect_equal(as.data.frame(effectsizes(ma, study = st))$study, c("NCT01", "NCT01"))
  expect_equal(nrow(studies(medrev:::mr_empty_insights())), 0L)
})
