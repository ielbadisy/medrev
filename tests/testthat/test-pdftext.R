# Write a small text PDF with base graphics.
make_pdf <- function(file, lines) {
  grDevices::pdf(file, width = 8, height = 11)
  graphics::plot.new()
  for (k in seq_along(lines)) graphics::text(0, 1 - k * 0.05, lines[[k]], adj = 0, cex = 0.7)
  grDevices::dev.off()
}

have_engine <- function() requireNamespace("pdftools", quietly = TRUE) || nzchar(Sys.which("pdftotext"))

test_that("pdftext() reads local PDFs by name template and by explicit files", {
  skip_if_not(have_engine())
  dir <- withr::local_tempdir()
  body <- c("A randomised trial of drug X versus placebo in adults with sepsis.",
            "Mortality at 28 days was 12/100 with drug X and 20/98 with placebo.",
            "The odds ratio was 0.71 (95% CI 0.52 to 0.97) in the intention to treat",
            "population. Adverse events were similar between groups. Funding was",
            "provided by a public research grant. The trial was registered as NCT0001.")
  make_pdf(file.path(dir, "1.pdf"), body)
  r <- chunkable_review()
  pt <- as.data.frame(pdftext(r, dir = dir, which = "all"))
  expect_equal(pt$status, c("available", "missing", "missing"))
  expect_match(pt$text[1], "odds ratio was 0.71")
  ch <- as.data.frame(chunk(r, fulltext = pt, which = "all", size = 500))
  expect_match(paste(ch$text[ch$record_id == "pmid:1"], collapse = " "), "NCT0001")

  file.copy(file.path(dir, "1.pdf"), file.path(dir, "any name.pdf"))
  pt2 <- as.data.frame(pdftext(r, files = c("pmid:2" = file.path(dir, "any name.pdf")), which = "all"))
  expect_equal(pt2$status, c("missing", "available", "missing"))
  expect_error(pdftext(r, files = "x.pdf"), "named character")
})

test_that("chunk() takes a list of text sources, earlier ones winning", {
  r <- chunkable_review()
  long <- function(w) paste(rep(w, 300), collapse = " ")
  a <- data.frame(id = "pmid:1", status = "available", text = long("first"))
  b <- data.frame(id = c("pmid:1", "pmid:2"), status = "available", text = c(long("second"), long("other")))
  ch <- as.data.frame(chunk(r, fulltext = list(a, b), which = "all", size = 400))
  expect_match(ch$text[ch$record_id == "pmid:1"][1], "^first")
  expect_match(ch$text[ch$record_id == "pmid:2"][1], "^other")
})

test_that("mr_clean_pdf_text joins hyphenated line breaks", {
  expect_equal(medrev:::mr_clean_pdf_text("rando-\n  mised  trial\f end"), "randomised trial\nend")
})
