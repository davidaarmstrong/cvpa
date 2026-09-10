## -----------------------------------------------------------------------
## Data access layer: cvpa no longer ships policy-attitude data bundled in
## the package. Instead it reads the same Parquet files the cp3 Shiny/JS
## apps are built from (https://www.quantoid.ca/files/app-js-remote/),
## hosted at https://www.quantoid.ca/files/cp3_parquet/ -- vbl_data.parquet
## (the question catalog), code_labels.parquet (demographic + party value
## labels), and one resp/question=<code>/data_0.parquet file per question.
##
## Design: local-cache-first, not query-the-network-on-every-call. Nothing
## in this package touches the network except update_cvpa_data() itself --
## every other function reads only from the local cache and errors with a
## pointer to update_cvpa_data() if the file it needs isn't there yet. This
## mirrors cp3/populate_db.R's Parquet export (same file layout) and
## app-js-remote/app.js's DATA_BASE_URL/respUrlFor() (same URL scheme), so
## keep those three in sync if the hosted layout ever changes.
## -----------------------------------------------------------------------

#' Base URL for the hosted cp3 policy-attitude Parquet files
#'
#' Override with `options(cvpa.data_url = ...)` to point the package at a
#' different mirror (e.g. a local copy of cp3/parquet_out/ during
#' development).
#' @keywords internal
.cvpa_data_base_url <- function() {
  getOption("cvpa.data_url", "https://www.quantoid.ca/files/cp3_parquet/")
}

#' Local cache directory used by cvpa
#'
#' Where [update_cvpa_data()] stores downloaded Parquet files and every
#' other cvpa function reads them from. Defaults to a per-user cache
#' directory managed by R (see [tools::R_user_dir()]); override with
#' `options(cvpa.cache_dir = ...)`.
#'
#' @return The cache directory path (created if it doesn't exist yet).
#' @export
cvpa_cache_dir <- function() {
  dir <- getOption("cvpa.cache_dir", NULL)
  if (is.null(dir)) dir <- tools::R_user_dir("cvpa", "cache")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

.cvpa_path_catalog <- function() file.path(cvpa_cache_dir(), "vbl_data.parquet")
.cvpa_path_code_labels <- function() file.path(cvpa_cache_dir(), "code_labels.parquet")
.cvpa_path_resp <- function(question) {
  file.path(cvpa_cache_dir(), "resp", paste0("question=", question), "data_0.parquet")
}

# Cache-busted single-file download. The query-string cache-buster mirrors
# app.js's DATA_VERSION fix (see cp3's populate_db.R comment above its
# Parquet-export step) -- cheap insurance against a CDN edge cache serving
# a stale copy of a just-updated file right after a data refresh.
.cvpa_download_one <- function(url, dest, quiet = TRUE) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  sep <- if (grepl("?", url, fixed = TRUE)) "&" else "?"
  bust_url <- paste0(url, sep, "t=", as.integer(Sys.time()))
  utils::download.file(bust_url, dest, mode = "wb", quiet = quiet)
  invisible(dest)
}

#' Download or refresh cvpa's local cache of the cp3 policy-attitude data
#'
#' cvpa reads policy-attitude and vote data from Parquet files hosted
#' alongside the cp3 web app, not from data bundled in the package. Call
#' this first (and again whenever you want the latest data) before using
#' [question_catalog()], [question_data()], [wtd_response()], or
#' [response_gap_analysis()].
#'
#' The question catalog (`vbl_data.parquet`) and the demographic/party
#' value-label table (`code_labels.parquet`) are small and are always
#' refreshed. Per-question respondent-level data
#' (`resp/question=<code>/data_0.parquet`) is only fetched for the
#' questions you name in `questions` -- there are several hundred of these
#' across all domains, so nothing downloads them until you ask.
#'
#' @param questions Which questions' respondent-level data to fetch, in
#'   addition to the catalog: `NULL` (default) fetches none, `"all"`
#'   fetches every question in the catalog (~630 files, roughly 90MB as of
#'   this writing), or a character vector of specific question codes (see
#'   [question_catalog()]).
#' @param force If `FALSE` (default), question files already in the cache
#'   are left alone. If `TRUE`, every requested file is re-downloaded.
#' @param quiet Suppress progress messages.
#' @return Invisibly, `TRUE`.
#' @examples
#' \dontrun{
#' update_cvpa_data()                                  # just the catalog
#' update_cvpa_data(questions = "bri_econ_013")         # + one question
#' update_cvpa_data(questions = c("vote_choice", "vote_intention"))
#' update_cvpa_data(questions = "all")                  # everything
#' }
#' @export
update_cvpa_data <- function(questions = NULL, force = FALSE, quiet = FALSE) {
  base <- .cvpa_data_base_url()

  if (!quiet) message("Fetching question catalog (vbl_data.parquet)...")
  .cvpa_download_one(paste0(base, "vbl_data.parquet"), .cvpa_path_catalog())

  if (!quiet) message("Fetching code labels (code_labels.parquet)...")
  .cvpa_download_one(paste0(base, "code_labels.parquet"), .cvpa_path_code_labels())

  if (!is.null(questions)) {
    cat_df <- .cvpa_catalog_raw()
    if (identical(questions, "all")) {
      questions <- unique(cat_df$question)
    } else {
      bad <- setdiff(questions, unique(cat_df$question))
      if (length(bad) > 0) {
        stop("Unknown question code(s): ", paste(bad, collapse = ", "),
             "\n  See question_catalog() for valid codes.", call. = FALSE)
      }
    }
    have <- vapply(questions, function(q) file.exists(.cvpa_path_resp(q)), logical(1))
    todo <- if (force) questions else questions[!have]
    n <- length(todo)
    if (!quiet && n > 0) message(sprintf("Fetching %d question file(s)...", n))
    for (i in seq_along(todo)) {
      q <- todo[i]
      if (!quiet) message(sprintf("  [%d/%d] %s", i, n, q))
      url <- paste0(base, "resp/question=", utils::URLencode(q, reserved = TRUE), "/data_0.parquet")
      .cvpa_download_one(url, .cvpa_path_resp(q))
    }
    if (!quiet && n == 0) message("All requested question files already cached (use force = TRUE to re-fetch).")
  }
  invisible(TRUE)
}

.cvpa_require_cache <- function(path, what, hint) {
  if (!file.exists(path)) {
    stop(what, " is not cached yet. Run ", hint, " first (see ?update_cvpa_data).",
         call. = FALSE)
  }
}

# Full (un-deduplicated, one row per question x survey_id x type) catalog,
# read fresh from the local cache every call -- it's small (a few thousand
# rows) so there's no need for an in-memory cache on top of the file one.
.cvpa_catalog_raw <- function() {
  path <- .cvpa_path_catalog()
  .cvpa_require_cache(path, "The question catalog", "update_cvpa_data()")
  nanoparquet::read_parquet(path)
}

.cvpa_code_labels <- function() {
  path <- .cvpa_path_code_labels()
  .cvpa_require_cache(path, "The code-labels table", "update_cvpa_data()")
  nanoparquet::read_parquet(path)
}
