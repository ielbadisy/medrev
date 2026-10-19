test_that("medrev() builds a review with ids, typed columns and logs", {
  r <- medrev(data.frame(Title = c("A", "B", "C"), PMID = c("11", NA, NA),
                         doi = c(NA, "https://doi.org/10.1/X", ""), year = c("2020", "2021", "x"),
                         extra = 1:3), source = "embase")
  expect_true(is_medrev(r))
  d <- medrev:::mr_df(r)
  expect_equal(d$id, c("pmid:11", "doi:10.1/x", "embase:3"))
  expect_equal(d$doi, c(NA, "10.1/X", NA))
  expect_identical(d$year, c(2020L, 2021L, NA))
  expect_equal(d$source, rep("embase", 3))
  expect_false("extra" %in% names(d))
  expect_equal(nrow(as.data.frame(search_log(r))), 1L)
  expect_equal(as.data.frame(prisma(r))$n[1], 3L)
})

test_that("imported records merge with other reviews and flow through dedup", {
  a <- medrev(data.frame(title = "Same", pmid = "1"), source = "embase")
  b <- medrev(data.frame(title = c("Same", "Other"), pmid = c("1", "2")), source = "cochrane")
  expect_equal(nrow(medrev:::mr_df(c(a, b))), 2L)
})

test_that("medrev() validates input", {
  expect_error(medrev(list(title = "a")), "data.frame")
  expect_error(medrev(data.frame(name = "a")), "title")
  expect_error(medrev(data.frame(title = "a"), source = ""), "source")
})
