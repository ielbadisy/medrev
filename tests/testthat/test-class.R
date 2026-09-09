test_that("new_medrev builds the schema and a basetable record slot", {
  r <- fake_review()
  expect_s3_class(r, "medrev")
  expect_true(medrev::is_medrev(r))
  expect_s3_class(r$records, "basetable")
  expect_equal(names(as.data.frame(r)), medrev:::.mr_cols)
})

test_that("print and summary run without error", {
  r <- fake_review()
  expect_no_error(print(r))
  expect_invisible(summary(r))
})

test_that("mr_which_rows respects screening state", {
  r <- fake_review()
  df <- medrev:::mr_df(r)
  expect_true(all(medrev:::mr_which_rows(df, "all")))
  # nothing screened -> included == all
  expect_true(all(medrev:::mr_which_rows(df, "included")))
  df$decision <- c("include", "exclude", "include", "exclude", NA)
  expect_equal(which(medrev:::mr_which_rows(df, "included")), c(1L, 3L, 5L))
})

test_that("[.medrev subsets the record table", {
  r <- fake_review()
  r2 <- r[1:2, ]
  expect_equal(nrow(as.data.frame(r2)), 2L)
  expect_s3_class(r2, "medrev")
})
