# medrev 0.4.0

* `year_counts()` - publications per year over the full observed range (gap
  years zero-filled), optionally cumulative.
* `term_counts()` - keyword / MeSH term frequencies, with `min_n` and `top`.
* `fulltext()` - download the JATS full text Europe PMC serves for
  open-access PMC articles, optionally flattened to plain text.
* `pubmed(ids = )` now also accepts PMC identifiers (`PMCxxxxxxx`).
* Records from medRxiv / bioRxiv and other preprints are tagged
  `source = "preprint"`.
* Added an R-CMD-check GitHub Action.

# medrev 0.3.0

* `add()` appends a further query or identifier lookup to an existing review,
  merging new records and skipping ids already present.
* `search_log()` returns the per-search audit log (one row per `pubmed()` /
  `add()` call).
* `prisma()` counts `add()` stages in the "records identified" total.

# medrev 0.2.0

* `fetch()` gains `limit` to cap the number of downloads; records with a
  resolved link are taken first.
* `print.medrev`, `print.medrev_prisma`, and `summary.medrev` now write to
  stdout with `cat()` instead of `cli`, so their output is visible in
  scripts, reports, and knitr (previously silent there).
* `bibliography()` no longer doubles the sentence period when a title or
  journal name already ends with one.
* README now knits an executed end-to-end workflow against Europe PMC.

# medrev 0.1.0

First release. A base-R-style pipeline for topic-driven biomedical literature
reviews, built on Europe PubMed Central and the `basetable` engine.

* `pubmed()` - start a review from a research question or a list of PMIDs /
  DOIs; returns a `medrev` object with a stable record schema.
* `dedup()` - remove duplicate records on DOI / PMID / PMCID / normalised
  title, with an optional fuzzy-title pass and a borderline-match audit.
* `screen()` - record inclusion / exclusion decisions by condition or from an
  editable sheet.
* `resolve()` - attach open-access PDF links from Europe PMC full text,
  Unpaywall, and publisher `citation_pdf_url` metadata.
* `fetch()` - download the open-access PDFs, rejecting non-PDF responses;
  returns a download log.
* `prisma()` - PRISMA-style flow counts derived from the stage log.
* `bibliography()` - export records to RIS, BibTeX, or CSV.
* `fuzzy_match()` - normalised edit-distance title matcher.

Only open-access content is downloaded; closed-access hits are reported,
never scraped.
