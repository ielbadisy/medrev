row <- function(...) {
  x <- list(...)
  lapply(x, function(v) list(value = v, quote = NA))
}

test_that("ratio estimates with a CI become log effects with SE from the CI", {
  ins <- effect_insights(list("pmid:1" = list(row(outcome = "death", measure = "HR",
                                                  estimate = 0.75, ci_low = 0.6, ci_high = 0.94))))
  es <- as.data.frame(effectsizes(ins))
  expect_equal(es$yi, log(0.75))
  expect_equal(es$sei, (log(0.94) - log(0.6)) / (2 * qnorm(0.975)))
  expect_equal(es$vi, es$sei^2)
  expect_equal(es$scale, "log")
  expect_equal(es$derived, "ci")
})

test_that("difference measures stay on the raw scale", {
  ins <- effect_insights(list(list(row(measure = "MD", estimate = -2, ci_low = -3.5, ci_high = -0.5))))
  es <- as.data.frame(effectsizes(ins))
  expect_equal(es$yi, -2)
  expect_equal(es$sei, 3 / (2 * qnorm(0.975)))
  expect_equal(es$scale, "raw")
})

test_that("counts reproduce metafor::escalc for OR, RR and RD", {
  skip_if_not_installed("metafor")
  for (m in c("OR", "RR", "RD")) {
    ins <- effect_insights(list(list(row(measure = m, events_e = 12, n_e = 100,
                                         events_c = 20, n_c = 98))))
    es <- as.data.frame(effectsizes(ins))
    ref <- metafor::escalc(m, ai = 12, n1i = 100, ci = 20, n2i = 98)
    expect_equal(es$yi, as.numeric(ref$yi), tolerance = 1e-10)
    expect_equal(es$vi, as.numeric(ref$vi), tolerance = 1e-10)
    expect_equal(es$derived, "counts")
  }
})

test_that("zero cells get a continuity correction like metafor", {
  skip_if_not_installed("metafor")
  ins <- effect_insights(list(list(row(events_e = 0, n_e = 50, events_c = 4, n_c = 50))))
  es <- as.data.frame(effectsizes(ins))
  ref <- metafor::escalc("OR", ai = 0, n1i = 50, ci = 4, n2i = 50)
  expect_equal(es$yi, as.numeric(ref$yi), tolerance = 1e-10)
  expect_match(es$note, "continuity correction")
  both0 <- effect_insights(list(list(row(events_e = 0, n_e = 50, events_c = 0, n_c = 50))))
  expect_true(is.na(as.data.frame(effectsizes(both0))$yi))
})

test_that("means reproduce metafor::escalc for MD and SMD (Hedges g)", {
  skip_if_not_installed("metafor")
  for (m in c("MD", "SMD")) {
    ins <- effect_insights(list(list(row(measure = m, mean_e = 10.2, sd_e = 3.1, n_e = 40,
                                         mean_c = 12.0, sd_c = 2.8, n_c = 42))))
    es <- as.data.frame(effectsizes(ins))
    ref <- metafor::escalc(m, m1i = 10.2, sd1i = 3.1, n1i = 40, m2i = 12, sd2i = 2.8, n2i = 42)
    expect_equal(es$yi, as.numeric(ref$yi), tolerance = 1e-10)
    expect_equal(es$vi, as.numeric(ref$vi), tolerance = 1e-10)
    expect_equal(es$derived, "means")
  }
})

test_that("reported is preferred over computed unless prefer = 'computed'", {
  r <- row(measure = "OR", estimate = 0.5, ci_low = 0.3, ci_high = 0.8,
           events_e = 12, n_e = 100, events_c = 20, n_c = 98)
  ins <- effect_insights(list(list(r)))
  expect_equal(as.data.frame(effectsizes(ins))$derived, "ci")
  expect_equal(as.data.frame(effectsizes(ins, prefer = "computed"))$derived, "counts")
})

test_that("p-value fallback and the not-enough-data case", {
  ins <- effect_insights(list(list(row(measure = "HR", estimate = 0.8, p_value = 0.04),
                                   row(outcome = "pain", measure = "HR"))))
  es <- as.data.frame(effectsizes(ins))
  expect_equal(es$derived[1], "p")
  expect_equal(es$sei[1], abs(log(0.8)) / qnorm(0.98))
  expect_true(is.na(es$yi[2]) && is.na(es$derived[2]))
  expect_match(es$note[2], "not enough data")
})

test_that("counts with a non-count measure are flagged; bad CIs are rejected", {
  ins <- effect_insights(list(list(row(measure = "HR", events_e = 5, n_e = 50, events_c = 9, n_c = 50),
                                   row(measure = "OR", estimate = 0, ci_low = -1, ci_high = 2))))
  es <- as.data.frame(effectsizes(ins))
  expect_equal(es$measure[1], "OR")
  expect_match(es$note[1], "reported measure HR")
  expect_true(is.na(es$yi[2]))
})

test_that("study = maps several records to one study", {
  ins <- effect_insights(list("pmid:1" = list(row(measure = "MD", estimate = 1, ci_low = 0, ci_high = 2)),
                              "pmid:2" = list(row(measure = "MD", estimate = 1, ci_low = 0, ci_high = 2))))
  es <- as.data.frame(effectsizes(ins, study = c("pmid:1" = "TRIAL-A", "pmid:2" = "TRIAL-A")))
  expect_equal(es$study, c("TRIAL-A", "TRIAL-A"))
  es2 <- as.data.frame(effectsizes(ins, study = data.frame(record_id = "pmid:2", study = "B")))
  expect_equal(es2$study, c("pmid:1", "B"))
  expect_error(effectsizes(ins, study = "x"), "named character")
  expect_error(effectsizes(ins, level = 2), "level")
})

test_that("effectsizes() on insights without effects returns an empty table", {
  es <- effectsizes(medrev:::mr_empty_insights())
  expect_equal(nrow(as.data.frame(es)), 0L)
})
