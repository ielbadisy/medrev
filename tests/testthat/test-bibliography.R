test_that("bibliography() returns formatted strings with no file", {
  refs <- bibliography(fake_review())
  expect_length(refs, 5L)
  expect_match(refs[1], "Smith J.*\\(2021\\).*Deep learning for sepsis")
})

test_that("citations do not double the sentence period", {
  r <- fake_review()
  df <- medrev:::mr_df(r)
  df$title[1] <- "A title that already ends with a period."
  df$journal[1] <- "Some Journal."
  r <- medrev:::mr_set_df(r, df)
  ref <- bibliography(r)[1]
  expect_false(grepl("\\.\\.", ref))
  expect_match(ref, "period\\. Some Journal\\.")
})

test_that("RIS export is well formed", {
  f <- withr::local_tempfile(fileext = ".ris")
  bibliography(fake_review(), f)
  txt <- readLines(f)
  expect_true(any(txt == "TY  - JOUR"))
  expect_true(any(grepl("^AU  - ", txt)))
  expect_true(any(grepl("^DO  - 10.1/a", txt)))
  expect_true(any(txt == "ER  - "))
})

test_that("BibTeX export is well formed", {
  f <- withr::local_tempfile(fileext = ".bib")
  bibliography(fake_review(), f)
  txt <- paste(readLines(f), collapse = "\n")
  expect_match(txt, "@article\\{")
  expect_match(txt, "title = \\{Deep learning for sepsis\\}")
})

test_that("CSV export writes the record table", {
  f <- withr::local_tempfile(fileext = ".csv")
  bibliography(fake_review(), f)
  back <- utils::read.csv(f)
  expect_equal(nrow(back), 5L)
  expect_true("doi" %in% names(back))
})

test_that("unknown extension errors", {
  expect_error(bibliography(fake_review(), tempfile(fileext = ".xml")), "Unsupported")
})
