test_that("underscore aliases are deprecated wrappers of the new names", {
  expect_warning(res <- fuzzy_match("a title", c("a title", "other")), "deprecated")
  expect_equal(res, fuzzymatch("a title", c("a title", "other")))
  expect_warning(expect_true(is_medrev(fake_review())), "deprecated")
})

test_that("vignettes and README use only the new names", {
  old <- c("fuzzy_match", "is_medrev", "llm_echo", "medrev_cache_clear",
           "medrev_cache_path", "medrev_email", "review_brief", "search_log",
           "term_counts", "year_counts")
  root <- testthat::test_path("..", "..")
  files <- c(list.files(file.path(root, "vignettes"), "\\.Rmd$", full.names = TRUE),
             file.path(root, "README.Rmd"))
  files <- files[file.exists(files)]
  skip_if(!length(files), "sources not available")
  txt <- unlist(lapply(files, readLines, warn = FALSE))
  hits <- old[vapply(old, function(o) any(grepl(paste0("\\b", o, "\\("), txt)), logical(1))]
  expect_equal(hits, character(0))
})
