test_that("yearcounts covers the full range with zero-filled gaps", {
  d <- as.data.frame(yearcounts(fake_review()))
  expect_s3_class(yearcounts(fake_review()), "basetable")
  expect_equal(range(d$year), c(2004, 2021))
  expect_equal(nrow(d), 18L)
  expect_equal(d$n[d$year == 2021], 2L)     # duplicate 2021 row counted
  expect_equal(d$n[d$year == 2004], 1L)
  expect_equal(d$n[d$year == 2010], 0L)     # gap year present
  expect_equal(sum(d$n), 5L)
})

test_that("yearcounts cumulative is a running total", {
  d <- as.data.frame(yearcounts(fake_review(), cumulative = TRUE))
  expect_equal(d$n[nrow(d)], 5L)
  expect_true(all(diff(d$n) >= 0))
})

test_that("termcounts splits and ranks keywords", {
  d <- as.data.frame(termcounts(fake_review(), "keywords"))
  expect_equal(d$term[1:2], c("deep learning", "sepsis"))  # ties: alphabetical from table()
  expect_equal(d$n[d$term == "sepsis"], 2L)
  expect_true(all(c("icu", "ml") %in% d$term))
})

test_that("termcounts honours min_n and top", {
  expect_equal(nrow(as.data.frame(termcounts(fake_review(), "keywords", min_n = 2))), 2L)
  expect_equal(nrow(as.data.frame(termcounts(fake_review(), "keywords", top = 1))), 1L)
  expect_equal(nrow(as.data.frame(termcounts(fake_review(), "mesh"))), 0L)  # all NA
})

test_that("which = 'included' drops excluded records", {
  r <- screen(fake_review(), exclude = year < 2020, reason = "old")
  d <- as.data.frame(yearcounts(r, which = "included"))
  expect_equal(min(d$year), 2020)
})
