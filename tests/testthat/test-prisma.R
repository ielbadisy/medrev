test_that("prisma() counts identification and de-duplication", {
  r <- dedup(fake_review())
  p <- prisma(r)
  expect_equal(p[["identified"]], 5L)
  expect_equal(p[["duplicates_removed"]], 1L)
  expect_equal(p[["after_deduplication"]], 4L)
  expect_equal(p[["screened"]], 4L)
  expect_equal(p[["included"]], 4L)
})

test_that("prisma() reflects screening exclusions", {
  r <- dedup(fake_review())
  r <- screen(r, exclude = year < 2010, reason = "old")
  p <- prisma(r)
  expect_equal(p[["excluded"]], 1L)
  expect_equal(p[["included"]], 3L)
})

test_that("prisma print and as.data.frame work", {
  p <- prisma(fake_review())
  expect_output(print(p), "PRISMA flow")
  d <- as.data.frame(p)
  expect_true(all(c("step", "n") %in% names(d)))
})
