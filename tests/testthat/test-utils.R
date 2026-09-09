test_that("mr_slug makes safe file-name parts", {
  expect_equal(medrev:::mr_slug("10.1038/s41586-020-2649-2"), "10.1038_s41586-020-2649-2")
  expect_equal(medrev:::mr_slug(NA), "NA")
  expect_equal(medrev:::mr_slug(""), "NA")
})

test_that("mr_norm_title normalises for matching", {
  a <- medrev:::mr_norm_title("Deep Learning for Sepsis: A Review!")
  b <- medrev:::mr_norm_title("<i>deep</i>  learning   for sepsis  a review")
  expect_equal(a, b)
})

test_that("mr_render_name fills templates from a record row", {
  row <- list(pmid = "123", year = 2020, doi = "10.1/x")
  expect_equal(medrev:::mr_render_name("{year}_{pmid}", row), "2020_123")
  expect_equal(medrev:::mr_render_name("{missing}", row), "NA")
})

test_that("mr_make_id prefers doi then pmid then pmcid then title", {
  expect_equal(medrev:::mr_make_id("10.1/X", "9", "PMC9", "t"), "doi:10.1/x")
  expect_equal(medrev:::mr_make_id(NA, "9", "PMC9", "t"), "pmid:9")
  expect_equal(medrev:::mr_make_id(NA, NA, "pmc9", "t"), "pmcid:PMC9")
  expect_match(medrev:::mr_make_id(NA, NA, NA, "Hello World"), "^title:hello world$")
})

test_that("mr_as_ids reads vectors and files", {
  expect_equal(medrev:::mr_as_ids(c(" 1 ", "2", "", "2")), c("1", "2"))
  f <- withr::local_tempfile()
  writeLines(c("111\tName A", "222\tName B"), f)
  expect_equal(medrev:::mr_as_ids(f), c("111", "222"))
})
