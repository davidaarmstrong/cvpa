## -----------------------------------------------------------------------
## Data access layer: cvpa no longer ships policy-attitude data bundled in
## the package. Instead it reads the same Parquet files the policy-attitudes
## web app is built from (https://policyattitudes.ca/app/), hosted by the
## Canadian Voting and Policy Attitudes Project site at
## https://policyattitudes.ca/cp3_parquet/ -- vbl_data.parquet
## (the question catalog), code_labels.parquet (demographic + party value
## labels), and one resp/question=<code>/data_0.parquet file per question.
##
## Design: local-cache-first, not query-the-network-on-every-call. Nothing
## in this package touches the network except update_cvpa_data() itself --
## every other function reads only from the local cache and errors with a
## pointer to update_cvpa_data() if the file it needs isn't there yet. This
## mirrors the cvpa_dataset repo's code/rebuild/populate_db.R Parquet export
## (same file layout, written to site/static/cp3_parquet/) and
## site/static/app/app.js's DATA_BASE_URL/respUrlFor() (same URL scheme), so
## keep those three in sync if the hosted layout ever changes.
## -----------------------------------------------------------------------

#' Base URL for the hosted cp3 policy-attitude Parquet files
#'
#' Override with `options(cvpa.data_url = ...)` to point the package at a
#' different mirror (e.g. a local copy of the cvpa_dataset repo's
#' `site/static/cp3_parquet/` during development).
#' @keywords internal
.cvpa_data_base_url <- function() {
  getOption("cvpa.data_url", "https://policyattitudes.ca/cp3_parquet/")
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

# A real Parquet file starts and ends with the 4-byte magic "PAR1". Checking
# both ends catches the two ways a download goes wrong without raising an
# error: an HTML error page served with a 200, and a truncated transfer.
.cvpa_is_parquet <- function(path) {
  size <- file.size(path)
  if (is.na(size) || size < 12) return(FALSE)
  magic <- charToRaw("PAR1")
  con <- file(path, "rb")
  on.exit(close(con))
  head <- readBin(con, "raw", 4)
  seek(con, size - 4)
  tail <- readBin(con, "raw", 4)
  identical(head, magic) && identical(tail, magic)
}

# Cache-busted single-file download. The query-string cache-buster mirrors
# app.js's DATA_VERSION fix (see the cvpa_dataset repo's
# code/rebuild/populate_db.R comment above its Parquet-export step) -- cheap
# insurance against a CDN edge cache serving a stale copy of a just-updated
# file right after a data refresh.
#
# Writes to a temporary file next to `dest` and only moves it into place
# once it has downloaded cleanly and checks out as Parquet, so a failed or
# partial download can never overwrite a good cached copy (it used to
# download straight onto `dest`, leaving a corrupt file behind on failure).
# Returns TRUE on success and FALSE on any failure, leaving `dest` untouched;
# callers decide whether a failure is fatal.
.cvpa_download_one <- function(url, dest, quiet = TRUE) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  # Only web URLs get the cache-buster: for a file:// URL (a local mirror via
  # options(cvpa.data_url = ...)) the query string would become part of the
  # file name and the file would never be found.
  sep <- if (grepl("?", url, fixed = TRUE)) "&" else "?"
  bust_url <- if (grepl("^https?://", url)) paste0(url, sep, "t=", as.integer(Sys.time())) else url
  tmp <- tempfile(pattern = ".download-", tmpdir = dirname(dest), fileext = ".part")
  on.exit(unlink(tmp), add = TRUE)
  ok <- tryCatch(
    identical(
      suppressWarnings(utils::download.file(bust_url, tmp, mode = "wb", quiet = quiet)),
      0L
    ),
    error = function(e) FALSE
  )
  if (!ok || !.cvpa_is_parquet(tmp)) return(FALSE)
  file.rename(tmp, dest) || file.copy(tmp, dest, overwrite = TRUE)
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
#'   addition to the catalog: `NULL` (default) fetches none, or a character
#'   vector of specific question codes (see [question_catalog()]). There is
#'   deliberately no "fetch everything" option -- if you want the full
#'   dataset, download `dataset.parquet` from
#'   <https://github.com/lucasjacklucas/cvpa_dataset> instead.
#' @param force If `FALSE` (default), question files already in the cache
#'   are left alone. If `TRUE`, every requested file is re-downloaded.
#' @param quiet Suppress progress messages.
#' @details Downloads are written to a temporary file and checked to be
#'   valid Parquet before they replace anything in the cache, so a failed or
#'   interrupted download never leaves a corrupt file behind or overwrites a
#'   good one. If the catalog or the code labels can't be downloaded, this
#'   stops with an error (and your existing cached copies are untouched). If
#'   an individual *question* file can't be downloaded -- e.g. a question
#'   that is listed in the catalog but has no respondent-level data
#'   available -- it is skipped with a warning naming the question(s), and
#'   the rest are still fetched.
#' @return Invisibly, `TRUE`, with an attribute `"skipped"` holding the
#'   question codes (if any) that could not be downloaded.
#' @examples
#' \dontrun{
#' update_cvpa_data()                                  # just the catalog
#' update_cvpa_data(questions = "bri_econ_013")         # + one question
#' update_cvpa_data(questions = c("vote_choice", "vote_intention"))
#' }
#' @export
update_cvpa_data <- function(questions = NULL, force = FALSE, quiet = FALSE) {
  base <- .cvpa_data_base_url()

  # The catalog and code labels are required for everything else, so a
  # failure here is fatal -- but because downloads go through a temp file,
  # it leaves any previously cached copy exactly as it was.
  if (!quiet) message("Fetching question catalog (vbl_data.parquet)...")
  if (!.cvpa_download_one(paste0(base, "vbl_data.parquet"), .cvpa_path_catalog())) {
    stop("Could not download the question catalog from ", base,
         "\n  Your cached data (if any) has been left unchanged. ",
         "Check your connection, or options(cvpa.data_url).", call. = FALSE)
  }

  if (!quiet) message("Fetching code labels (code_labels.parquet)...")
  if (!.cvpa_download_one(paste0(base, "code_labels.parquet"), .cvpa_path_code_labels())) {
    stop("Could not download the code labels from ", base,
         "\n  Your cached data (if any) has been left unchanged. ",
         "Check your connection, or options(cvpa.data_url).", call. = FALSE)
  }

  skipped <- character(0)
  if (!is.null(questions)) {
    if (identical(questions, "all")) {
      stop("questions = \"all\" is no longer supported; name the question codes you ",
           "need (see question_catalog()).\n  To get the full dataset, download ",
           "dataset.parquet from https://github.com/lucasjacklucas/cvpa_dataset",
           call. = FALSE)
    }
    cat_df <- .cvpa_catalog_raw()
    bad <- setdiff(questions, unique(cat_df$question))
    if (length(bad) > 0) {
      stop("Unknown question code(s): ", paste(bad, collapse = ", "),
           "\n  See question_catalog() for valid codes.", call. = FALSE)
    }
    have <- vapply(questions, function(q) file.exists(.cvpa_path_resp(q)), logical(1))
    todo <- if (force) questions else questions[!have]
    n <- length(todo)
    if (!quiet && n > 0) message(sprintf("Fetching %d question file(s)...", n))
    for (i in seq_along(todo)) {
      q <- todo[i]
      if (!quiet) message(sprintf("  [%d/%d] %s", i, n, q))
      url <- paste0(base, "resp/question=", utils::URLencode(q, reserved = TRUE), "/data_0.parquet")
      if (!.cvpa_download_one(url, .cvpa_path_resp(q))) skipped <- c(skipped, q)
    }
    if (!quiet && n == 0) message("All requested question files already cached (use force = TRUE to re-fetch).")
    if (length(skipped) > 0) {
      warning(sprintf(
        "Could not download data for %d question(s), which were skipped: %s\n  They are listed in the catalog but may have no respondent-level data available.",
        length(skipped), paste(skipped, collapse = ", ")), call. = FALSE)
    }
  }
  res <- TRUE
  attr(res, "skipped") <- skipped
  invisible(res)
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
  cat_df <- nanoparquet::read_parquet(path)
  # Two wordings in the published catalog (ces_dpin_016, env_intd_012) have
  # their quotation marks escaped repeatedly upstream, so each quote arrives
  # as a run of four quote characters instead of one -- an artifact of the
  # source CSV. Collapse any run of 2+ quotes back to one, here where every
  # function reads the catalog, so it can't leak into question_catalog()
  # output. Checked against the whole catalog: those two are the only
  # wordings containing any quote character at all, so nothing legitimate is
  # affected, and this is a harmless no-op once the source is fixed.
  cat_df$question_wording <- gsub('"{2,}', '"', cat_df$question_wording)
  cat_df
}

.cvpa_code_labels <- function() {
  path <- .cvpa_path_code_labels()
  .cvpa_require_cache(path, "The code-labels table", "update_cvpa_data()")
  nanoparquet::read_parquet(path)
}
