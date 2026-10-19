# Offline fetch() behaviour. Real downloads are covered in test-live.R.

# A review whose pdf_url is already populated, so fetch() does not call
# resolve() (no network). The URLs are unreachable on purpose.
stub_resolved <- function() {
  r <- fake_review()
  df <- medrev:::mr_df(r)
  df$pdf_url <- paste0("http://127.0.0.1:9/", df$pmid, ".pdf")
  df$pdf_source <- "europepmc"
  df$oa_status <- ifelse(df$is_oa, "oa", NA_character_)
  medrev:::mr_set_df(r, df)
}

test_that("limit caps the number of records processed", {
  r <- stub_resolved()
  dir <- withr::local_tempdir()
  log <- fetch(r, dir = dir, which = "all", limit = 2, delay = 0)
  expect_equal(nrow(as.data.frame(log)), 2L)
})

test_that("limit above the selection size is a no-op", {
  r <- stub_resolved()
  dir <- withr::local_tempdir()
  log <- fetch(r, dir = dir, which = "all", limit = 99, delay = 0)
  expect_equal(nrow(as.data.frame(log)), 5L)
})

test_that("records with a link are preferred when limiting", {
  r <- stub_resolved()
  df <- medrev:::mr_df(r)
  df$pdf_url[c(1, 2)] <- NA_character_          # rows 3,4,5 have links
  r <- medrev:::mr_set_df(r, df)
  dir <- withr::local_tempdir()
  log <- as.data.frame(fetch(r, dir = dir, which = "all", limit = 2, delay = 0))
  expect_equal(nrow(log), 2L)
  expect_true(all(log$status == "failed"))     # linked rows attempted, not skipped
})

test_that("unreachable URLs are logged as failures, nothing is written", {
  r <- stub_resolved()
  dir <- withr::local_tempdir()
  log <- as.data.frame(fetch(r, dir = dir, which = "all", delay = 0))
  expect_true(all(log$status == "failed"))
  expect_true(all(is.na(log$file)))
  expect_equal(length(list.files(dir)), 0L)
})

test_that("errors file lists unfetched records", {
  r <- stub_resolved()
  dir <- withr::local_tempdir()
  ef <- withr::local_tempfile(fileext = ".tsv")
  fetch(r, dir = dir, which = "all", delay = 0, errors = ef)
  bad <- read.delim(ef)
  expect_equal(nrow(bad), 5L)
  expect_true(all(bad$status == "failed"))
})

test_that("the PMC bucket is only tried for Europe PMC links", {
  expect_null(medrev:::mr_pmc_cloud_pdf(list(pdf_url = "https://x.org/a.pdf", pmcid = "PMC1"), NULL))
  expect_null(medrev:::mr_pmc_cloud_pdf(list(pdf_url = NA_character_, pmcid = "PMC1"), NULL))
})

test_that("Europe PMC links resolve to the PMC bucket", {
  skip_on_cran()
  skip_if_offline("pmc-oa-opendata.s3.amazonaws.com")
  u <- medrev:::mr_pmc_cloud_pdf(list(pdf_url = "https://europepmc.org/articles/PMC12796369?pdf=render"), NULL)
  expect_match(u, "^https://pmc-oa-opendata\\.s3\\.amazonaws\\.com/PMC12796369\\.[0-9]+/PMC12796369\\.[0-9]+\\.pdf$")
})
