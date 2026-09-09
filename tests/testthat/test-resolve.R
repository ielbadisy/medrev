fake_resp <- function(status, body, ctype = "application/json") {
  httr2::response(status_code = status,
                  headers = list(`content-type` = ctype),
                  body = charToRaw(body))
}

test_that("mr_res_epmc builds a render URL only for OA PMC rows", {
  row <- as.data.frame(fake_review())[1, ]           # PMC1, is_oa TRUE
  expect_match(medrev:::mr_res_epmc(row)$url, "europepmc.org/articles/PMC1\\?pdf=render")
  row$is_oa <- FALSE
  expect_null(medrev:::mr_res_epmc(row))
  row$is_oa <- TRUE; row$pmcid <- NA_character_
  expect_null(medrev:::mr_res_epmc(row))
})

test_that("mr_unpaywall_pick reads the best OA PDF location", {
  j <- '{"is_oa":true,"oa_status":"gold","best_oa_location":{"url_for_pdf":"https://x.org/a.pdf"}}'
  h <- medrev:::mr_unpaywall_pick(fake_resp(200, j))
  expect_equal(h$url, "https://x.org/a.pdf")
  expect_equal(h$oa_status, "gold")
  expect_null(medrev:::mr_unpaywall_pick(fake_resp(200, '{"is_oa":false}')))
  expect_null(medrev:::mr_unpaywall_pick(fake_resp(404, '{}')))
  expect_null(medrev:::mr_unpaywall_pick(simpleError("request failed")))
})

test_that("mr_citation_pick extracts citation_pdf_url", {
  html <- '<head><meta name="citation_pdf_url" content="https://p.org/f.pdf"></head>'
  expect_equal(medrev:::mr_citation_pick(fake_resp(200, html, "text/html"))$url,
               "https://p.org/f.pdf")
  expect_null(medrev:::mr_citation_pick(fake_resp(200, "<head></head>", "text/html")))
  expect_null(medrev:::mr_citation_pick(fake_resp(500, "err", "text/html")))
})

test_that("the resolve cache round-trips, upserts, and clears", {
  withr::local_options(medrev.cache_dir = withr::local_tempdir())
  expect_equal(nrow(medrev:::mr_cache_load()), 0L)
  medrev:::mr_cache_save(data.frame(key = "doi:x", pdf_url = "u1",
    pdf_source = "unpaywall", oa_status = "oa", stringsAsFactors = FALSE))
  expect_equal(medrev:::mr_cache_load()$pdf_url, "u1")
  medrev:::mr_cache_save(data.frame(key = "doi:x", pdf_url = "u2",
    pdf_source = "unpaywall", oa_status = "oa", stringsAsFactors = FALSE))
  expect_equal(medrev:::mr_cache_load()$pdf_url, "u2")     # upsert, not append
  expect_true(medrev_cache_clear())
  expect_equal(nrow(medrev:::mr_cache_load()), 0L)
})

test_that("resolve() fills europepmc links offline and caches them", {
  withr::local_options(medrev.cache_dir = withr::local_tempdir())
  r <- resolve(fake_review(), sources = "europepmc")
  d <- as.data.frame(r)
  expect_true(all(!is.na(d$pdf_url[d$pmcid %in% c("PMC1", "PMC3")])))
  expect_true(all(d$pdf_source[!is.na(d$pdf_url)] == "europepmc"))
  expect_gt(nrow(medrev:::mr_cache_load()), 0L)
  expect_true("resolved" %in% as.data.frame(r$stages)$stage)

  d2 <- d; d2$pdf_url <- NA_character_; d2$pdf_source <- NA_character_
  r2 <- suppressWarnings(resolve(medrev:::mr_set_df(r, d2), sources = "unpaywall"))
  d2 <- as.data.frame(r2)
  expect_true(all(!is.na(d2$pdf_url[d2$pmcid %in% c("PMC1", "PMC3")])))   # from cache
  expect_true(all(d2$pdf_source[!is.na(d2$pdf_url)] == "europepmc"))
})

test_that("resolve(cache = FALSE) never writes the cache", {
  withr::local_options(medrev.cache_dir = withr::local_tempdir())
  resolve(fake_review(), sources = "europepmc", cache = FALSE)
  expect_false(file.exists(medrev:::mr_cache_file()))
})
