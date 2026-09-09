test_that("screen() with no args returns an editable sheet", {
  sheet <- screen(fake_review())
  expect_s3_class(sheet, "basetable")
  expect_true(all(c("id", "decision", "exclude_reason") %in% names(as.data.frame(sheet))))
  expect_equal(nrow(as.data.frame(sheet)), 5L)
})

test_that("exclude/include conditions set decisions", {
  r <- screen(fake_review(), exclude = year < 2010, reason = "pre-2010")
  df <- as.data.frame(r)
  expect_equal(df$decision[df$year < 2010], "exclude")
  expect_equal(unique(df$reason[df$decision %in% "exclude"]), "pre-2010")
  expect_true(all(is.na(df$decision[df$year >= 2010])))

  r2 <- screen(r, include = year >= 2020)
  df2 <- as.data.frame(r2)
  expect_true(all(df2$decision[df2$year >= 2020] == "include"))
})

test_that("row-index conditions work", {
  r <- screen(fake_review(), include = c(1, 3))
  df <- as.data.frame(r)
  expect_equal(which(df$decision %in% "include"), c(1L, 3L))
})

test_that("a filled sheet round-trips through screen(sheet=)", {
  r <- dedup(fake_review())                 # unique ids before screening
  sheet <- as.data.frame(screen(r))
  expect_equal(nrow(sheet), 4L)
  sheet$decision <- c("include", "exclude", "maybe", "include")
  sheet$exclude_reason[2] <- "wrong population"
  r2 <- screen(r, sheet = sheet)
  df <- as.data.frame(r2)
  expect_equal(df$decision, c("include", "exclude", "maybe", "include"))
  expect_equal(df$reason[2], "wrong population")
  expect_false(any(is.na(df$screened_at)))
})

test_that("screening is visible to prisma()", {
  r <- screen(fake_review(), exclude = year < 2010, reason = "old")
  p <- prisma(r)
  expect_s3_class(p, "medrev_prisma")
  expect_equal(p[["excluded"]], 1L)
  expect_equal(p[["included"]], 4L)
})
