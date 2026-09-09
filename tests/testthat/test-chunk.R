test_that("mr_windows makes overlapping windows that cover all words", {
  w <- medrev:::mr_windows("a b c d e f", size = 3, overlap = 1)
  expect_equal(length(w), 3L)
  expect_equal(w[[1]]$text, "a b c")
  expect_equal(w[[2]]$text, "c d e")     # 1-word overlap
  expect_equal(w[[3]]$text, "e f")
  expect_equal(medrev:::mr_windows("only three words", size = 10, overlap = 2)[[1]]$n, 3L)
})

test_that("chunk() splits abstracts and estimates tokens", {
  ch <- chunk(chunkable_review(), size = 100, overlap = 20, which = "all")
  expect_s3_class(ch, "medrev_chunks")
  expect_s3_class(ch, "basetable")
  d <- as.data.frame(ch)
  # 300-word abstract -> ceil((300-100)/80)+1 windows; 40-word -> 1; 5-word -> skipped (<20 chars? no: 29)
  expect_true(all(c("record_id", "chunk_id", "seq", "text", "n_words", "tokens_est") %in% names(d)))
  expect_true(all(d$tokens_est == ceiling(d$n_words * 4 / 3)))
  expect_equal(unique(d$record_id[d$record_id == "pmid:1"])[1], "pmid:1")
  expect_gt(sum(d$record_id == "pmid:1"), 1L)          # long abstract -> many chunks
  expect_equal(sum(d$record_id == "pmid:2"), 1L)       # 40 words -> one chunk
})

test_that("chunk() rejects overlap >= size", {
  expect_error(chunk(chunkable_review(), size = 50, overlap = 50), "smaller")
})

test_that("chunk() uses fulltext where available", {
  r <- chunkable_review()
  ft <- basetable::as_basetable(data.frame(
    id = "pmid:2", pmcid = "PMC2", status = "available",
    chars = 6L, text = paste(rep("word", 250), collapse = " "),
    stringsAsFactors = FALSE
  ))
  ch <- as.data.frame(chunk(r, size = 100, overlap = 0, which = "all", fulltext = ft))
  # pmid:2 now has 250 words of full text -> more than the 1 abstract chunk
  expect_gt(sum(ch$record_id == "pmid:2"), 1L)
})

test_that("chunk() with no usable text returns an empty typed object", {
  r <- chunkable_review()
  d <- medrev:::mr_df(r); d$abstract <- NA_character_
  r <- medrev:::mr_set_df(r, d)
  ch <- chunk(r, which = "all")
  expect_s3_class(ch, "medrev_chunks")
  expect_equal(nrow(as.data.frame(ch)), 0L)
})
