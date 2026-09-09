# A synthetic medrev object for offline tests.
fake_review <- function() {
  df <- data.frame(
    id = c("doi:10.1/a", "doi:10.1/b", "pmid:3", "doi:10.1/a", "pmid:5"),
    source = "europepmc",
    pmid = c("1", "2", "3", "1b", "5"),
    pmcid = c("PMC1", NA, "PMC3", NA, NA),
    doi = c("10.1/a", "10.1/b", NA, "10.1/a", NA),
    title = c("Deep learning for sepsis", "A trial of statins",
              "Machine learning in the ICU", "Deep learning for sepsis",
              "Unrelated cardiology paper"),
    abstract = c("we trained a model", "rct", "icu study", "we trained a model", "hearts"),
    authors = c("Smith J; Doe A", "Roe B", "Lee C; Park D; Kim E; Cho F; Han G",
                "Smith J; Doe A", "Ng H"),
    journal = c("J ML Med", "Lancet", "Crit Care", "J ML Med", "Circulation"),
    year = c(2021L, 2004L, 2020L, 2021L, 2019L),
    type = "journal-article",
    keywords = c("sepsis; deep learning", NA, "icu; ml", "sepsis; deep learning", NA),
    mesh = NA_character_, language = "eng",
    is_oa = c(TRUE, FALSE, TRUE, TRUE, FALSE),
    license = NA_character_,
    url = paste0("https://doi.org/", c("10.1/a", "10.1/b", "x", "10.1/a", "y")),
    pdf_url = NA_character_, pdf_source = NA_character_, oa_status = NA_character_,
    decision = NA_character_, reason = NA_character_, screened_at = NA_character_,
    retrieved_at = as.character(Sys.Date()),
    stringsAsFactors = FALSE
  )
  r <- medrev:::new_medrev(df)
  r <- medrev:::mr_log_search(r, "fake query", "europepmc", 5L, 5L)
  r <- medrev:::mr_stage(r, "identified", "test")
  r
}
