q <- function(v, quote = NA) list(value = v, quote = quote)

src_chunks <- function(text, id = "pmid:1") {
  df <- data.frame(record_id = id, chunk_id = paste0(id, "#01"), seq = 1L, text = text,
                   n_words = 10L, tokens_est = 13L, source = "europepmc", stringsAsFactors = FALSE)
  structure(basetable::as_basetable(df), class = c("medrev_chunks", "basetable", "data.frame"))
}

test_that("a fully supported extraction passes every check", {
  ch <- src_chunks("Mortality at 28 days: odds ratio 0.71 (95% CI 0.52–0.97); 1,204 patients were randomised.")
  ins <- effect_insights(list("pmid:1" = list(list(
    outcome = q("mortality", "Mortality at 28 days"),
    measure = q("OR", "odds ratio"),
    estimate = q(0.71, "odds ratio 0.71"),
    ci_low = q(0.52, "95% CI 0.52-0.97"),
    ci_high = q(0.97, "95% CI 0.52-0.97"),
    n_e = q(1204, "1204 patients")))))
  expect_equal(nrow(as.data.frame(verify(ins, ch))), 0L)
})

test_that("invented quotes and numbers absent from their quote are flagged", {
  ch <- src_chunks("Mortality was lower with the drug, odds ratio 0.71.")
  ins <- effect_insights(list("pmid:1" = list(list(
    estimate = q(0.71, "odds ratio 0.71"),
    ci_low = q(0.5, "95% CI 0.50 to 0.98"),
    ci_high = q(0.98, "odds ratio 0.71")))))
  v <- as.data.frame(verify(ins, ch))
  expect_true(any(v$check == "quote_not_found" & v$field == "ci_low"))
  expect_true(any(v$check == "number_not_in_quote" & v$field == "ci_high"))
  expect_false(any(v$field == "estimate"))
  expect_false(any(as.data.frame(verify(ins))$check == "quote_not_found"))
})

test_that("rule checks catch impossible values", {
  ins <- effect_insights(list(list(
    list(measure = q("HR"), estimate = q(1.5), ci_low = q(0.6), ci_high = q(0.9)),
    list(measure = q("RR"), estimate = q(-0.2)),
    list(events_e = q(60), n_e = q(50), sd_c = q(-1), p_value = q(1.4)),
    list(ci_low = q(2), ci_high = q(1)))))
  v <- as.data.frame(verify(ins))
  chk <- function(row) v$check[v$row == row & v$severity == "error"]
  expect_true("ci_order" %in% chk(1))
  expect_true("ratio_nonpositive" %in% chk(2))
  expect_true(all(c("events_exceed_n", "negative_value", "p_range") %in% chk(3)))
  expect_true("ci_order" %in% chk(4))
  expect_true(all(v$check[v$severity == "note"] == "quote_missing"))
})

test_that("arms larger than the study total and shared registrations are flagged", {
  sr <- function(id, n, reg) {
    medrev:::new_medrev_insights(data.frame(
      insight_id = paste0("ext-", id), type = "extraction", record_id = id,
      chunk_ids = paste0(id, "#01"), n_chunks = 1L, text = NA_character_,
      data = medrev:::mr_to_json(list(n_total = q(n), registration = q(reg))),
      model = "t", at = "x", stringsAsFactors = FALSE))
  }
  ins <- c(sr("pmid:1", 100L, "NCT0001"), sr("pmid:2", 300L, "nct 0001"),
           effect_insights(list("pmid:1" = list(list(n_e = q(80L), n_c = q(70L))))))
  v <- as.data.frame(verify(ins))
  expect_equal(sum(v$check == "arms_exceed_total"), 1L)
  expect_equal(sort(v$record_id[v$check == "shared_registration"]), c("pmid:1", "pmid:2"))
})

test_that("verify() validates its inputs", {
  expect_error(verify(data.frame()), "medrev_insights")
  expect_error(verify(medrev:::mr_empty_insights(), chunks = data.frame()), "medrev_chunks")
  expect_equal(nrow(as.data.frame(verify(medrev:::mr_empty_insights()))), 0L)
})

test_that("mr_norm_text ignores case, dashes, spacing and thousands separators", {
  expect_equal(medrev:::mr_norm_text("OR 0.71 (95% CI 0.52–0.97); n = 1,204"),
               "or 0.71 95% ci 0.52-0.97 n 1204")
  expect_true(medrev:::mr_number_in(3.53, "OR 3\u00b753 (95% CI 1\u00b703-12\u00b703)"))
  expect_equal(medrev:::mr_norm_text("p=0\u00b7044"), "p 0.044")
})
