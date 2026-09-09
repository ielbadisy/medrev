test_that("search_log returns one row per logged search", {
  sl <- search_log(fake_review())
  expect_s3_class(sl, "basetable")
  d <- as.data.frame(sl)
  expect_equal(nrow(d), 1L)
  expect_true(all(c("query", "source", "hits", "retrieved", "time") %in% names(d)))
})

test_that("prisma counts an added stage in the identified total", {
  r <- fake_review()
  r <- medrev:::mr_stage(r, "added", "test")
  st <- as.data.frame(r$stages)
  st$n[st$stage == "added"] <- 9L
  r$stages <- st
  expect_equal(prisma(r)[["identified"]], 9L)
})

test_that("add() merges a live second search", {
  skip_on_cran()
  skip_if_offline("www.ebi.ac.uk")
  r <- pubmed("sepsis prognosis machine learning", n = 15)
  n1 <- nrow(as.data.frame(r))
  r <- add(r, "sepsis mortality prediction deep learning", n = 15)
  d <- as.data.frame(r)
  expect_gte(nrow(d), n1)
  expect_equal(anyDuplicated(d$id), 0L)
  expect_equal(nrow(as.data.frame(search_log(r))), 2L)
  expect_true("added" %in% as.data.frame(r$stages)$stage)
})
