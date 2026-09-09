test_that("mr_epmc_row maps a core result to the schema", {
  rec <- list(
    id = "38000000", source = "MED", pmid = "38000000",
    pmcid = "PMC10000000", doi = "10.1000/XYZ",
    title = "A study", abstractText = "abstract here",
    authorString = "Doe J, Roe A", pubYear = "2023",
    journalInfo = list(journal = list(title = "Test J")),
    pubTypeList = list(pubType = list("Journal Article", "Review")),
    keywordList = list(keyword = list("alpha", "beta")),
    isOpenAccess = "Y", inEPMC = "Y",
    fullTextUrlList = list(fullTextUrl = list(
      list(documentStyle = "pdf", availability = "Open access",
           url = "https://example.org/x.pdf"),
      list(documentStyle = "html", availability = "Open access",
           url = "https://example.org/x")
    ))
  )
  row <- medrev:::mr_epmc_row(rec)
  expect_equal(row$pmid, "38000000")
  expect_equal(row$doi, "10.1000/xyz")
  expect_equal(row$journal, "Test J")
  expect_equal(row$year, 2023L)
  expect_true(row$is_oa)
  expect_equal(row$type, "Journal Article; Review")
  expect_equal(row$keywords, "alpha; beta")
  expect_equal(row$pdf_url, "https://example.org/x.pdf")
  expect_equal(row$url, "https://doi.org/10.1000/xyz")
})

test_that("mr_epmc_row tolerates an empty record and yields the schema", {
  empty <- medrev:::mr_empty_records()
  expect_equal(nrow(empty), 0L)
  expect_setequal(names(empty), names(medrev:::mr_epmc_row(list())))
})
