
<!-- README.md is generated from README.Rmd. Please edit that file. -->

# medrev

<!-- badges: start -->
[![R-CMD-check](https://github.com/ielbadisy/medrev/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ielbadisy/medrev/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`medrev` turns a research question into a folder of open-access PDFs
plus the audit tables a literature review needs. It searches the
literature through [Europe PubMed
Central](https://europepmc.org/RestfulWebService) (which indexes all of
PubMed and MEDLINE), de-duplicates, tracks screening decisions, resolves
open-access full text, and downloads it. Record tables are
[`basetable`](https://github.com/ielbadisy/basetable) objects. Only
open-access content is fetched; closed-access hits are reported, never
scraped.

## Installation

``` r
# install.packages("pak")
pak::pak("ielbadisy/medrev")
```

## A full workflow

The chunks below are executed when this README is knit, so the output is
real (counts move as Europe PMC is updated).

``` r
library(medrev)

r <- pubmed("interpretable machine learning for sepsis prognosis",
            n = 40, years = c(2018, 2025))
r
#> <medrev review>
#>   searches    : 1  (interpretable machine learning for sepsis prognosis)
#>   records     : 40
#>   open access : 38   pdf link resolved: 38
#>   stages      : identified(40)
```

`r` is a `medrev` object: a record table plus a log of every search and
every pipeline stage. Remove duplicates (exact identifiers, then
near-identical titles):

``` r
r <- dedup(r, fuzzy = TRUE)
```

Record screening decisions with conditions evaluated against the record
table (as in `subset()`):

``` r
r <- screen(r, exclude = year < 2019, reason = "pre-2019 protocol")
r <- screen(r, exclude = !grepl("machine learning|deep learning|neural",
                                tolower(paste(title, abstract))),
            reason = "not an ML study")
r
#> <medrev review>
#>   searches    : 1  (interpretable machine learning for sepsis prognosis)
#>   records     : 40
#>   open access : 38   pdf link resolved: 38
#>   screened    : include 0 / exclude 4 / maybe 0 / undecided 36
#>   stages      : identified(40) -> deduplicated(40) -> screened(40) -> screened(40)
```

The record table is a plain data frame under `as.data.frame()`:

``` r
d <- as.data.frame(r)
d$title <- substr(d$title, 1, 60)
keep <- !d$decision %in% "exclude"
head(d[keep, c("year", "is_oa", "title")], 5)
#>   year is_oa                                                        title
#> 1 2025 FALSE Interpretable machine learning based comorbidity specific mo
#> 3 2025  TRUE Reforming disease prognosis and treatment prediction for pal
#> 4 2025  TRUE Revolutionizing sepsis diagnosis using machine learning and 
#> 5 2025  TRUE Prognostic model and U-shaped SBP-risk correlation in unstab
#> 6 2025  TRUE Under the background of the new global definition of ARDS: a
```

Resolve open-access PDF links (Europe PMC full text here; add
`"unpaywall"` with a contact e-mail for wider coverage):

``` r
r <- resolve(r, sources = "europepmc")
table(as.data.frame(r)$pdf_source, useNA = "ifany")
#> 
#> europepmc      <NA> 
#>        38         2
```

PRISMA-style flow counts fall out of the stage log:

``` r
prisma(r)
#> PRISMA flow
#> records identified               40 
#>   duplicates removed             0 
#> records after de-duplication     40 
#> records screened                 40 
#>   excluded                       -4 
#> studies included                 36 
#>   with an open-access PDF        34
```

Download the open-access PDFs of the screened-in records (`limit` keeps
this README’s knit quick; drop it to fetch them all):

``` r
dl <- fetch(r, dir = tempfile("medrev_pdfs"), which = "oa",
            name = "{year}_{pmid}", limit = 4)
as.data.frame(dl)[, c("pmid", "pdf_source", "status", "bytes")]
#>       pmid pdf_source     status    bytes
#> 1 41339488  europepmc downloaded  3988934
#> 2 41131449  europepmc downloaded  3319762
#> 3 41402560  europepmc downloaded  2006381
#> 4 41049485  europepmc downloaded 60355877
```

Export the references:

``` r
cat(bibliography(r, which = "included")[1:3], sep = "\n\n")
#> Cui C, Zhao J, Mu F, et al. (2025). Interpretable machine learning based comorbidity specific mortality risk score for bloodstream infections. The Journal of infection. doi:10.1016/j.jinf.2025.106656
#> 
#> Kavitha MS, Sreeja GG, Karthik S, Sabitha R. (2025). Reforming disease prognosis and treatment prediction for palliative care with hybrid metaheuristic deep neural architectures in IoT healthcare ecosystems. Scientific reports. doi:10.1038/s41598-025-31037-7
#> 
#> Zubair M, Din I, Sarwar N, et al. (2025). Revolutionizing sepsis diagnosis using machine learning and deep learning models: a systematic literature review. BMC infectious diseases. doi:10.1186/s12879-025-11423-2
```

## Starting from a reference list

``` r
r2 <- pubmed(ids = c("10.1038/s41586-020-2649-2", "10.1371/journal.pmed.1000097"))
d2 <- as.data.frame(r2)
d2$title <- substr(d2$title, 1, 55)
d2[, c("pmid", "year", "title")]
#>       pmid year                                                   title
#> 1 32939066 2020                           Array programming with NumPy.
#> 2 19621072 2009 Preferred reporting items for systematic reviews and me
```

## Adding preprints

Europe PMC already indexes many preprints (they come back as
`source = "preprint"`). For the most recent ones, scan medRxiv / bioRxiv
directly and combine the two searches with `c()`:

``` r
pp <- preprints("sepsis machine learning", "medrxiv", from = "2025-06-01")
r  <- c(pubmed("sepsis machine learning", n = 200), pp)   # merged, deduped by id
```

## Model-assisted synthesis (optional)

The enrichment verbs need a **model callback** you supply: any
`function(prompt, ...)` that returns a string. medrev builds the grounded
prompts and parses the responses; nothing is written back into the record
table. `llm_echo()` is a stub for trying the plumbing.

``` r
model <- function(prompt, ...) {
  # call your LLM of choice here and return its text response
}

ch  <- chunk(r, which = "included")                 # abstracts (or fulltext(r))
sm  <- summarize(ch, llm = model)                   # one grounded summary per study
ex  <- extract(ch, llm = model,                     # structured fields as JSON
               fields = c(design = "study design",
                          n = "sample size",
                          outcome = "primary outcome"))
extractions(ex)                                     # -> one row per study, a column per field
syn <- synthesize(c(sm, ex), llm = model)           # cross-study synthesis

review_brief(r, insights = c(sm, ex, syn),
             title = "Sepsis prognosis: rapid brief",
             file = "brief.md")                     # Markdown: flow + synthesis + studies + refs
```

## Hand-screening in a spreadsheet

``` r
sheet <- screen(r)                 # a basetable with an empty `decision` column
# write.csv(as.data.frame(sheet), "screen.csv"); edit; read back
r <- screen(r, sheet = sheet)
```

## Configuration

Unpaywall (one of the PDF resolvers) needs a contact e-mail:

``` r
options(medrev.email = "you@example.org")   # or set MEDREV_EMAIL
```

## The `medrev` object

A single carrier: `$records` (the record table), `$searches` (query
log), and `$stages` (row count after each step, which drives
`prisma()`). `as.data.frame(r)` returns the record table; `r[i, ]`
subsets it.
