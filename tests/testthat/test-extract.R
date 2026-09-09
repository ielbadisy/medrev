fields3 <- c(design = "study design", n = "total sample size",
            outcome = "primary outcome")

test_that("mr_parse_json recovers a JSON object, code fences and prose notwithstanding", {
  txt <- "Sure!\n```json\n{\"design\": \"RCT\", \"n\": 240, \"outcome\": \"mortality\"}\n```\n"
  got <- medrev:::mr_parse_json(txt, names(fields3))
  expect_equal(got$design, "RCT")
  expect_equal(got$n, "240")
  expect_equal(got$outcome, "mortality")
})

test_that("mr_parse_json returns NA per missing key and NULL when no object", {
  got <- medrev:::mr_parse_json('{"design": "cohort"}', names(fields3))
  expect_equal(got$design, "cohort")
  expect_true(is.na(got$n))
  expect_null(medrev:::mr_parse_json("no json here", names(fields3)))
})

test_that("extract() stores parsed JSON and flags unparseable responses", {
  ch <- chunk(chunkable_review(), size = 120, overlap = 10, which = "all")
  model <- function(prompt, ...) {
    if (grepl("pmid:1", prompt)) '{"design":"RCT","n":100,"outcome":"death"}'
    else "I could not find these details."
  }
  ins <- extract(ch, llm = model, fields = fields3, per = "record")
  d <- as.data.frame(ins)
  expect_true(all(d$type == "extraction"))
  r1 <- d[d$record_id == "pmid:1", ]
  expect_true(is.na(r1$text) && !is.na(r1$data))
  r2 <- d[d$record_id == "pmid:2", ]
  expect_true(!is.na(r2$text) && is.na(r2$data))
})

test_that("extractions() flattens to one row per record with a column per field", {
  ch <- chunk(chunkable_review(), size = 120, overlap = 10, which = "all")
  model <- function(prompt, ...) '{"design":"RCT","n":50,"outcome":"LOS"}'
  tab <- as.data.frame(extractions(extract(ch, llm = model, fields = fields3, per = "record")))
  expect_true(all(c("record_id", "design", "n", "outcome") %in% names(tab)))
  expect_true(all(tab$design == "RCT"))
  expect_equal(nrow(tab), length(unique(as.data.frame(ch)$record_id)))
})

test_that("extract() validates fields and prompt placeholders", {
  ch <- chunk(chunkable_review(), size = 120, overlap = 10, which = "all")
  expect_error(extract(ch, llm_echo, fields = c("no name")), "named character vector")
  expect_error(extract(ch, llm_echo, fields = fields3, prompt = "only {evidence}"),
               "\\{schema\\}")
})

test_that("extract() on empty chunks returns empty insights", {
  r <- chunkable_review()
  d <- medrev:::mr_df(r); d$abstract <- NA_character_
  ch <- chunk(medrev:::mr_set_df(r, d), which = "all")
  expect_equal(nrow(as.data.frame(extract(ch, llm_echo, fields = fields3))), 0L)
})
