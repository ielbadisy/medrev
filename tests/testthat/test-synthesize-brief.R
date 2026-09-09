mk_insights <- function() {
  ch <- chunk(chunkable_review(), size = 120, overlap = 10, which = "all")
  summarize(ch, llm = function(p, ...) "study summary text", per = "record")
}

test_that("synthesize() makes one synthesis row from summaries", {
  ins <- mk_insights()
  seen <- NULL
  syn <- synthesize(ins, llm = function(p, ...) { seen <<- p; "the synthesis" })
  d <- as.data.frame(syn)
  expect_equal(nrow(d), 1L)
  expect_equal(d$type, "synthesis")
  expect_true(is.na(d$record_id))
  expect_equal(d$n_chunks, nrow(as.data.frame(ins)))
  expect_match(seen, "NOTES:")
  expect_equal(d$text, "the synthesis")
})

test_that("synthesize() handles no usable insights and a bad prompt", {
  expect_equal(nrow(as.data.frame(synthesize(medrev:::mr_empty_insights(), llm_echo))), 0L)
  expect_error(synthesize(mk_insights(), llm_echo, prompt = "no placeholder"), "\\{evidence\\}")
})

test_that("c() merges insight tables", {
  ins <- mk_insights()
  syn <- synthesize(ins, llm = llm_echo)
  both <- c(ins, syn)
  expect_s3_class(both, "medrev_insights")
  expect_equal(nrow(as.data.frame(both)), nrow(as.data.frame(ins)) + 1L)
  expect_true(all(c("summary", "synthesis") %in% as.data.frame(both)$type))
})

test_that("review_brief() assembles the expected Markdown sections", {
  r <- dedup(fake_review())
  ins <- mk_insights()
  syn <- synthesize(ins, llm = function(p, ...) "SYNTH BODY")
  b <- review_brief(r, insights = c(ins, syn), title = "Test Brief")
  expect_s3_class(b, "medrev_brief")
  md <- format(b)
  expect_match(md, "^# Test Brief")
  for (h in c("## Search", "## Study flow", "## Synthesis", "## Included studies", "## References")) {
    expect_true(grepl(h, md, fixed = TRUE))
  }
  expect_match(md, "SYNTH BODY")
})

test_that("review_brief() works with no insights and can write to a file", {
  r <- dedup(fake_review())
  b <- review_brief(r, title = "Bare")
  expect_false(grepl("## Synthesis", format(b), fixed = TRUE))

  f <- withr::local_tempfile(fileext = ".md")
  out <- review_brief(r, title = "Bare", file = f)
  expect_equal(out, f)
  expect_match(paste(readLines(f), collapse = "\n"), "# Bare")
})
