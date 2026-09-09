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
