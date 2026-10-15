# cvpa 0.3.0

After upgrading, run `update_cvpa_data()` to refresh your local cache: the
cached question catalog from earlier versions is out of date.

## Breaking changes

* `update_cvpa_data(questions = "all")` has been removed. Name the question
  codes you need (see `question_catalog()`); it now errors with a pointer to
  the full `dataset.parquet` in the
  [cvpa_dataset repository](https://github.com/lucasjacklucas/cvpa_dataset)
  for anyone who wants everything.

* `wtd_vote()` and `gap_analysis()` now default to `years = NULL`, meaning
  every year in `data`, instead of `1945:2023` and `1945:2022`. The old
  defaults silently dropped any year after 2023 (and, for `gap_analysis()`,
  2023 itself). Results on the bundled `vote_data` are unchanged for
  `wtd_vote()`; `gap_analysis()` now includes 2023. Pass `years` explicitly
  to restore the old behaviour.

## Data

* The default `cvpa.data_url` is now `https://policyattitudes.ca/cp3_parquet/`,
  hosted with the project site, instead of the copy on quantoid.ca, which
  was out of date and is no longer read by the package. The new data
  includes the merge of `ces_soim_009` into `bri_soim_014` (same-sex
  marriage; `bri_soim_014` now covers the CES 1993, 1997, 2000, 2004, 2006
  and 2008 surveys and has the wording "Support or oppose allowing same-sex
  couples to marry"), and respondent ages recovered for five Environics
  surveys.

* `question_catalog()` now lists only questions and surveys that have
  respondent-level data, so `n_waves` counts only surveys that actually
  have data, and questions with no data (such as `env_intd_045`) are no
  longer listed. Surveys with no region variable cannot be weighted and
  are not included.

## Bug fixes

* `update_cvpa_data()` no longer leaves a corrupt file in the cache when a
  download fails. Downloads are written to a temporary file and checked to
  be valid Parquet before they replace anything, so a failed, truncated, or
  error-page download can't overwrite a good cached copy. A failure to
  download the catalog or code labels stops with an error and leaves the
  cache untouched.

* `update_cvpa_data()` skips a question whose file can't be downloaded,
  with a warning naming it, instead of stopping at the first missing file.
  The return value gains a `"skipped"` attribute listing those questions.

* A local `file://` mirror set through `options(cvpa.data_url = ...)` now
  works; a cache-busting query string was being appended to every URL.

* Two question wordings (`ces_dpin_016`, `env_intd_012`) no longer show
  their quotation marks repeated several times over.
