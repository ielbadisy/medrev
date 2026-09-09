test_that("mr_jats_text flattens the body and collapses whitespace", {
  xml <- paste0(
    "<article><front><article-title>T</article-title></front>",
    "<body><sec><title>Intro</title><p>Hello   world.</p>",
    "<p>Second\n\n  line.</p></sec></body></article>"
  )
  txt <- medrev:::mr_jats_text(xml)
  expect_match(txt, "Hello world\\.")
  expect_match(txt, "Second\nline\\.")
  expect_false(grepl("  ", txt))
})

test_that("mr_jats_text returns NA on unparseable input", {
  expect_true(is.na(medrev:::mr_jats_text("<not xml")))
})

test_that("fulltext() with no PMC ids returns an empty typed log", {
  r <- fake_review()
  df <- medrev:::mr_df(r)
  df$pmcid <- NA_character_
  r <- medrev:::mr_set_df(r, df)
  ft <- fulltext(r, which = "all")
  expect_s3_class(ft, "basetable")
  d <- as.data.frame(ft)
  expect_equal(nrow(d), 0L)
  expect_true("text" %in% names(d))
})

test_that("fulltext() retrieves real JATS text for an OA article", {
  skip_on_cran()
  skip_if_offline("www.ebi.ac.uk")
  r <- pubmed(ids = "PMC7327471")            # an OA PMC article
  ft <- as.data.frame(fulltext(r, which = "all"))
  skip_if(nrow(ft) == 0, "record has no PMC id")
  if (ft$status[1] == "available") {
    expect_gt(ft$chars[1], 1000)
    expect_type(ft$text, "character")
  } else {
    skip("full text not available for this article right now")
  }
})
