# Network tests: skipped on CRAN and offline.

test_that("pubmed() searches Europe PMC end to end", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline("www.ebi.ac.uk")
  r <- pubmed("crispr cas9 off-target", n = 12)
  expect_s3_class(r, "medrev")
  df <- as.data.frame(r)
  expect_gt(nrow(df), 0L)
  expect_equal(names(df), medrev:::.mr_cols)
  expect_true(any(!is.na(df$doi)))
})

test_that("pubmed(ids=) resolves identifiers", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline("www.ebi.ac.uk")
  r <- pubmed(ids = c("28495875", "10.1038/nbt.3820"))
  expect_gte(nrow(as.data.frame(r)), 1L)
})

test_that("resolve() + fetch() downloads at least one OA PDF", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline("www.ebi.ac.uk")
  dir <- withr::local_tempdir()
  r <- pubmed("open access machine learning radiology", n = 15)
  r <- resolve(r, sources = "europepmc")
  log <- fetch(r, dir = dir, which = "oa", name = "{pmid}")
  expect_s3_class(log, "basetable")
  ld <- as.data.frame(log)
  if (any(ld$status == "downloaded")) {
    f <- ld$file[ld$status == "downloaded"][1]
    expect_true(file.exists(f))
    expect_identical(readBin(f, "raw", 5L), charToRaw("%PDF-"))
  } else {
    skip("no OA PDF available for this query right now")
  }
})
