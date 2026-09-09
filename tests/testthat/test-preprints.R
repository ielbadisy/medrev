test_that("mr_query_tokens keeps 3+ letter words, lower-cased", {
  tok <- medrev:::mr_query_tokens("Deep-Learning for SEPSIS a")
  expect_true(all(c("deep", "learning", "sepsis") %in% tok))
  expect_false("a" %in% tok)
})

test_that("mr_biorxiv_rows maps the API shape to the schema", {
  cx <- data.frame(
    doi = "10.1101/2025.01.01.X", title = "T", abstract = "A",
    authors = "Doe, J.; Roe, B.", date = "2025-01-02", version = "2",
    category = "epidemiology", license = "cc_by", stringsAsFactors = FALSE
  )
  row <- medrev:::mr_biorxiv_rows(cx, "medrxiv")
  expect_equal(row$source, "preprint")
  expect_equal(row$doi, "10.1101/2025.01.01.x")
  expect_equal(row$year, 2025L)
  expect_true(row$is_oa)
  expect_equal(row$pdf_url,
               "https://www.medrxiv.org/content/10.1101/2025.01.01.xv2.full.pdf")
  expect_equal(row$.v, 2L)
})

test_that("mr_preprint_match applies all/any over title+abstract+category", {
  cx <- data.frame(
    title = c("sepsis prediction", "cancer trial"),
    abstract = c("machine learning model", "chemotherapy"),
    category = c("infectious diseases", "oncology"), stringsAsFactors = FALSE
  )
  expect_equal(medrev:::mr_preprint_match(cx, c("sepsis", "machine"), "all"), c(TRUE, FALSE))
  expect_equal(medrev:::mr_preprint_match(cx, c("sepsis", "chemotherapy"), "any"), c(TRUE, TRUE))
  expect_equal(medrev:::mr_preprint_match(cx, c("cancer"), "all"), c(FALSE, TRUE))
})

test_that("c() merges reviews, logs a combined stage, feeds prisma", {
  r1 <- dedup(fake_review())                       # 4 unique records
  r2 <- fake_review()
  d2 <- medrev:::mr_df(r2)
  d2$id <- paste0(d2$id, "_x"); d2$doi <- NA_character_; d2$pmcid <- NA_character_
  d2$pmid <- paste0(d2$pmid, "z")
  r2 <- medrev:::mr_set_df(r2, d2)

  m <- c(r1, r2)
  expect_s3_class(m, "medrev")
  # 4 from r1 + 4 unique from r2 (fake_review has one repeated id within it)
  expect_equal(nrow(as.data.frame(m)), 8L)
  expect_equal(nrow(as.data.frame(search_log(m))), 2L)
  expect_true("combined" %in% as.data.frame(m$stages)$stage)
  expect_equal(prisma(m)[["identified"]], 8L)
})

test_that("c() with one review returns it unchanged", {
  r <- fake_review()
  expect_identical(c(r), r)
})

test_that("preprints() searches medRxiv end to end", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline("api.biorxiv.org")
  from <- format(Sys.Date() - 120, "%Y-%m-%d")
  pp <- preprints("machine learning", "medrxiv", from = from, n = 5, max_scan = 2000)
  d <- as.data.frame(pp)
  skip_if(nrow(d) == 0, "no matches in the scanned window")
  expect_true(all(d$source == "preprint"))
  expect_true(all(d$is_oa))
  expect_true(all(grepl("\\.full\\.pdf$", d$pdf_url)))
  expect_equal(nrow(as.data.frame(search_log(pp))), 1L)
})
