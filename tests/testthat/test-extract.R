fields3 <- c(design = "study design", n = "total sample size",
            outcome = "primary outcome")

test_that("mr_parse_form recovers a JSON object, code fences and prose notwithstanding", {
  txt <- "Sure!\n```json\n{\"design\": \"RCT\", \"n\": 240, \"outcome\": \"mortality\"}\n```\n"
  got <- medrev:::mr_parse_form(txt, medrev:::mr_as_form(fields3))[[1]]
  expect_equal(got$design$value, "RCT")
  expect_equal(got$n$value, "240")
  expect_true(is.na(got$n$quote))
})

test_that("mr_parse_form returns NA per missing key and NULL when no object", {
  f <- medrev:::mr_as_form(fields3)
  got <- medrev:::mr_parse_form('{"design": "cohort"}', f)[[1]]
  expect_equal(got$design$value, "cohort")
  expect_true(is.na(got$n$value))
  expect_null(medrev:::mr_parse_form("no json here", f))
})

test_that("typed forms coerce values and keep quotes", {
  f <- form("ma", measure = "binary")
  txt <- paste0('{"rows": [',
    '{"outcome": {"value": "mortality", "quote": "28-day mortality"},',
    ' "n_e": {"value": "1,204 patients", "quote": "1,204 patients"},',
    ' "measure": {"value": "or", "quote": "odds ratio"},',
    ' "estimate": {"value": "0.71", "quote": "OR 0.71"}},',
    '{"outcome": "ICU stay", "measure": "weird"}]}')
  got <- medrev:::mr_parse_form(txt, f)
  expect_length(got, 2L)
  expect_identical(got[[1]]$n_e$value, 1204L)
  expect_identical(got[[1]]$measure$value, "OR")
  expect_equal(got[[1]]$estimate$value, 0.71)
  expect_equal(got[[1]]$estimate$quote, "OR 0.71")
  expect_equal(got[[2]]$outcome$value, "ICU stay")
  expect_equal(got[[2]]$measure$value, "weird")
  expect_true(is.na(got[[2]]$n_e$value))
})

test_that("mr_first_number handles separators, signs and p-value notation", {
  expect_equal(medrev:::mr_first_number("1,204 (52%)"), 1204)
  expect_equal(medrev:::mr_first_number("<0.001"), 0.001)
  expect_equal(medrev:::mr_first_number("−0.35"), -0.35)
  expect_true(is.na(medrev:::mr_first_number("none")))
})

test_that("form() builds sr and ma forms, adds and drops fields", {
  sr <- form("sr")
  expect_s3_class(sr, "medrev_form")
  expect_identical(attr(sr, "unit"), "study")
  expect_true(all(c("design", "n_total", "registration") %in% sr$field))
  ma <- form("ma", measure = "continuous")
  expect_identical(attr(ma, "unit"), "row")
  expect_true(all(c("mean_e", "sd_c", "estimate") %in% ma$field))
  expect_false("events_e" %in% ma$field)
  f <- form("sr", add = c(dose = "dose"), drop = "funding")
  expect_true("dose" %in% f$field)
  expect_false("funding" %in% f$field)
  expect_error(form("sr", drop = "nope"), "Unknown field")
  expect_error(form("sr", add = c(design = "again")), "Duplicated")
  expect_error(form("sr", add = data.frame(field = "x", kind = "date", description = "d")),
               "Unknown field kind")
  expect_output(print(sr), "one row per study")
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
  model <- function(prompt, ...) '{"design":{"value":"RCT","quote":"randomised"},"n":50,"outcome":"LOS"}'
  ins <- extract(ch, llm = model, form = fields3, per = "record")
  tab <- as.data.frame(extractions(ins))
  expect_true(all(c("record_id", "design", "n", "outcome") %in% names(tab)))
  expect_true(all(tab$design == "RCT"))
  expect_equal(nrow(tab), length(unique(as.data.frame(ch)$record_id)))
  tq <- as.data.frame(extractions(ins, quotes = TRUE))
  expect_true(all(tq$design_quote == "randomised"))
})

test_that("extract() with an ma form stores effect rows, flattened per row", {
  ch <- chunk(chunkable_review(), size = 120, overlap = 10, which = "all")
  model <- function(prompt, ...) {
    expect_match(prompt, '"rows"', fixed = TRUE)
    '{"rows":[{"outcome":"death","n_e":50,"events_e":5,"estimate":0.8},{"outcome":"LOS","estimate":-1.2}]}'
  }
  ins <- extract(ch, llm = model, form = form("ma", "binary"))
  expect_true(all(as.data.frame(ins)$type == "effect"))
  tab <- as.data.frame(extractions(ins, type = "effect"))
  nrec <- length(unique(as.data.frame(ch)$record_id))
  expect_equal(nrow(tab), 2L * nrec)
  expect_equal(tab$row, rep(1:2, nrec))
  expect_true(is.numeric(tab$estimate))
  expect_equal(nrow(as.data.frame(extractions(ins))), 0L)
})

test_that("an empty rows array is a valid (zero-row) extraction", {
  ch <- chunk(chunkable_review(), size = 120, overlap = 10, which = "all")
  ins <- extract(ch, llm = function(p, ...) '{"rows": []}', form = form("ma"))
  expect_false(anyNA(as.data.frame(ins)$data))
})

test_that("extract() validates fields and prompt placeholders", {
  ch <- chunk(chunkable_review(), size = 120, overlap = 10, which = "all")
  expect_error(extract(ch, llm_echo, fields = c("no name")), "named character vector")
  expect_error(extract(ch, llm_echo), "Supply a")
  expect_error(extract(ch, llm_echo, fields = fields3, prompt = "only {evidence}"),
               "\\{schema\\}")
})

test_that("extract() on empty chunks returns empty insights", {
  r <- chunkable_review()
  d <- medrev:::mr_df(r); d$abstract <- NA_character_
  ch <- chunk(medrev:::mr_set_df(r, d), which = "all")
  expect_equal(nrow(as.data.frame(extract(ch, llm_echo, fields = fields3))), 0L)
})
