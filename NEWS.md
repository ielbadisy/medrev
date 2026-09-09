# medrev 0.7.0

* `screen()` gains `file =` to write the blank screening sheet to CSV, and
  `sheet =` now also accepts a path to such a CSV - a spreadsheet round trip
  without hand-rolling `write.csv()` / `read.csv()`.
* `resolve()` runs the Unpaywall and citation-metadata lookups as a
  concurrent batch (`max_active`, default 5) instead of one blocking request
  per DOI, and caches every link it finds on disk (keyed by record id) so
  re-resolving an overlapping review skips the network. `cache = FALSE` opts
  out; `medrev_cache_path()` / `medrev_cache_clear()` manage the cache.
* New getting-started vignette, including a concrete Claude API callback for
  the enrichment layer.

# medrev 0.6.0

The optional model-assisted enrichment layer. A "model" is any function
`function(prompt, ...)` returning a string; medrev builds the grounded
prompts and parses the responses, and generated content is kept in a
`medrev_insights` table, never written back into the records.

* `chunk()` - split each record's abstract (or `fulltext()` output) into
  overlapping word windows (`medrev_chunks`).
* `summarize()` - one grounded summary per record, or one over all chunks.
* `extract()` / `extractions()` - pull a fixed set of JSON fields from the
  evidence and flatten them to one row per study.
* `synthesize()` - a cross-study synthesis from the summaries / extractions.
* `review_brief()` - assemble a Markdown brief (search log, PRISMA flow,
  synthesis, per-study notes, references); `file =` writes it out.
* `llm_echo()` - a deterministic stub model for trying the plumbing.
* `c()` also merges `medrev_insights` tables.

# medrev 0.5.0

* `preprints()` - a medRxiv / bioRxiv backend. The API has no text search, so
  it scans a publication-date window and keeps records whose
  title/abstract/category contain the query words.
* `c()` (`c.medrev`) merges the records, search logs, and stage logs of
  several `medrev` objects, deduping by `id` - e.g.
  `c(pubmed(...), preprints(...))`.
* `prisma()` counts `c()` merges in the "records identified" total.

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
