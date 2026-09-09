test_that("llm_echo reports the prompt size", {
  expect_equal(llm_echo("a b c"), "[echo] response to a 3-word prompt.")
})

test_that("mr_check_llm / mr_call_llm guard the callback and its response", {
  expect_error(medrev:::mr_check_llm(42), "must be a function")
  expect_invisible(medrev:::mr_check_llm(function(p) p))
  expect_match(medrev:::mr_call_llm(function(p) stop("boom"), "x"), "<model error: boom>")
  expect_equal(medrev:::mr_call_llm(function(p) "", "x"), "<empty model response>")
  expect_equal(medrev:::mr_call_llm(function(p) c("a", "b"), "x"), "a\nb")
})

test_that("mr_render_prompt substitutes the evidence placeholder", {
  expect_equal(medrev:::mr_render_prompt("see {evidence} end", "TEXT"), "see TEXT end")
})

test_that("summarize(per = 'record') makes one grounded summary per record", {
  ch <- chunk(chunkable_review(), size = 100, overlap = 10, which = "all")
  captured <- NULL
  spy <- function(prompt, ...) { captured <<- prompt; "a summary" }
  ins <- summarize(ch, llm = spy, per = "record")
  expect_s3_class(ins, "medrev_insights")
  d <- as.data.frame(ins)
  expect_equal(sort(unique(d$type)), "summary")
  expect_setequal(d$record_id, unique(as.data.frame(ch)$record_id))
  expect_true(all(d$text == "a summary"))
  expect_match(captured, "EXCERPTS:")            # default template used
  expect_true(all(d$n_chunks >= 1))
})

test_that("summarize(per = 'all') makes a single cross-record summary", {
  ch <- chunk(chunkable_review(), size = 100, overlap = 10, which = "all")
  ins <- as.data.frame(summarize(ch, llm = llm_echo, per = "all"))
  expect_equal(nrow(ins), 1L)
  expect_true(is.na(ins$record_id))
  expect_gt(ins$n_chunks, 1L)
})

test_that("summarize accepts a custom prompt and rejects one without {evidence}", {
  ch <- chunk(chunkable_review(), size = 100, overlap = 10, which = "all")
  seen <- NULL
  ins <- summarize(ch, llm = function(p, ...) { seen <<- p; "ok" },
                   prompt = "TL;DR of: {evidence}", per = "all")
  expect_match(seen, "^TL;DR of: ")
  expect_error(summarize(ch, llm = llm_echo, prompt = "no placeholder"), "\\{evidence\\}")
})

test_that("summarize on empty chunks returns an empty insights table", {
  r <- chunkable_review()
  d <- medrev:::mr_df(r); d$abstract <- NA_character_
  ch <- chunk(medrev:::mr_set_df(r, d), which = "all")
  ins <- summarize(ch, llm = llm_echo)
  expect_s3_class(ins, "medrev_insights")
  expect_equal(nrow(as.data.frame(ins)), 0L)
})
