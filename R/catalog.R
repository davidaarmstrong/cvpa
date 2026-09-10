## -----------------------------------------------------------------------
## Question catalog, response-label lookup, and the general per-question
## data fetcher. See R/data_access.R for the caching/download layer these
## build on, and R/demographics.R for the demographic side of decoding.
## -----------------------------------------------------------------------

#' Grouping variables available for any question
#'
#' Every table returned by [question_data()] carries these columns in
#' addition to `response`, `weight`, and `year` -- eleven demographic
#' variables (see [demographic_vars] for what each one measures) plus
#' `vote_intention` and `vote_choice`, which travel with every question's
#' data and can be used as grouping variables just like a demographic (this
#' is what lets you, say, break a policy question down by how people voted).
#' Used to validate the `grouping_vars`/`grouping_var` argument in
#' [wtd_response()] and [response_gap_analysis()].
#'
#' @seealso [demographic_vars], [question_data()]
#' @export
cvpa_grouping_vars <- c(
  "vote_intention", "vote_choice", "region", "province",
  "com_500", "com_100", "gender", "age_cats", "religion",
  "degree", "language", "union_household", "occupation"
)

# Raw `response` codes for each non-party question type, ascending -- mirrors
# TYPE_CONFIG in cp3/app-js-remote/app.js exactly. Both this package and the
# JS app decode the same vbl_data.value_labels string against these codes,
# positionally, in ascending order; keep the two in sync if that convention
# ever changes.
.cvpa_type_codes <- list(b = c(0, 1), nsa = c(0, 1, 2), lsm = c(-1, 0, 1))

# A question's dominant type (by number of survey waves using it) and, for
# non-party types, that type's ascending response labels split out of
# vbl_data.value_labels. Mirrors app.js's getQuestionMeta(): a handful of
# questions have a few waves coded with a different type than the rest (see
# cp3/docs/data_quality_check.md item 3); both this package and the app
# resolve that by majority vote rather than erroring.
.cvpa_question_meta <- function(cat_df, question) {
  sub <- cat_df[cat_df$question == question, ]
  if (nrow(sub) == 0) {
    stop("Unknown question code: '", question, "'. See question_catalog().", call. = FALSE)
  }
  tt <- table(sub$type)
  type <- names(tt)[which.max(tt)]
  value_labels <- if (type == "party") {
    NULL
  } else {
    vl_raw <- sub$value_labels[sub$type == type][1]
    trimws(strsplit(vl_raw, ",")[[1]])
  }
  list(type = type, value_labels = value_labels, n_waves = nrow(sub))
}

#' List available policy and vote questions
#'
#' One row per question, deduplicated across the several hundred
#' survey-wave rows each question has in the underlying catalog (see
#' [update_cvpa_data()]). Covers every domain in the cp3 project, including
#' `"vote"` (`vote_choice`/`vote_intention`) -- from cvpa's point of view
#' vote is just one more question, decoded and analyzed the same way as any
#' policy question (see [question_data()], [wtd_response()]).
#'
#' @param domain Optional character vector of domain codes to restrict to
#'   (see the `domain` column of this function's own output for valid
#'   values, e.g. `"econ"`, `"soim"`, `"vote"`).
#' @param type Optional character vector of question types to restrict to:
#'   `"b"` (binary), `"lsm"` (3-point less/same/more-style), `"nsa"`
#'   (3-point never/sometimes/always-style), or `"party"` (vote_choice,
#'   vote_intention).
#' @return A data frame with one row per question: `question`, `domain`,
#'   `domain_label`, `issue_label`, `question_wording`, `value_labels`
#'   (comma-separated, ascending -- see [question_response_labels()] for a
#'   decoded, one-row-per-category version), `type`, and `n_waves` (how
#'   many survey waves asked this question).
#' @examples
#' \dontrun{
#' update_cvpa_data()
#' question_catalog(domain = "econ")
#' }
#' @export
question_catalog <- function(domain = NULL, type = NULL) {
  cat_df <- .cvpa_catalog_raw()

  dominant <- cat_df %>%
    dplyr::count(question, type, name = "n") %>%
    dplyr::group_by(question) %>%
    dplyr::slice_max(n, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(question, type)

  meta <- cat_df %>%
    dplyr::group_by(question) %>%
    dplyr::summarise(
      domain = dplyr::first(domain),
      domain_label = dplyr::first(domain_label),
      issue_label = dplyr::first(issue_label),
      question_wording = dplyr::first(question_wording),
      n_waves = dplyr::n(),
      .groups = "drop"
    )

  value_labels <- cat_df %>%
    dplyr::inner_join(dominant, by = c("question", "type")) %>%
    dplyr::group_by(question) %>%
    dplyr::summarise(value_labels = dplyr::first(value_labels), .groups = "drop")

  out <- meta %>%
    dplyr::left_join(dominant, by = "question") %>%
    dplyr::left_join(value_labels, by = "question")
  out <- as.data.frame(out[order(out$domain, out$issue_label), ])
  rownames(out) <- NULL

  if (!is.null(domain)) {
    bad <- setdiff(domain, unique(out$domain))
    if (length(bad) > 0) stop("Unknown domain(s): ", paste(bad, collapse = ", "), call. = FALSE)
    out <- out[out$domain %in% domain, ]
  }
  if (!is.null(type)) {
    bad <- setdiff(type, unique(out$type))
    if (length(bad) > 0) stop("Unknown type(s): ", paste(bad, collapse = ", "), call. = FALSE)
    out <- out[out$type %in% type, ]
  }
  rownames(out) <- NULL
  out
}

#' Response category labels for one question
#'
#' A decoded, one-row-per-category view of what [question_catalog()]
#' reports as a single comma-separated `value_labels` string (or, for
#' `vote_choice`/`vote_intention`, of the relevant rows of
#' `code_labels.parquet`) -- the same table [question_data()] uses
#' internally to turn a question's raw `response` codes into a labeled
#' factor.
#'
#' @param question A question code, e.g. `"bri_econ_013"` or `"vote_choice"`.
#' @return A data frame with `code`, `label`, and `sort_order`, ordered by
#'   `sort_order`.
#' @examples
#' \dontrun{
#' update_cvpa_data()
#' question_response_labels("bri_econ_013")
#' question_response_labels("vote_choice")
#' }
#' @export
question_response_labels <- function(question) {
  cat_df <- .cvpa_catalog_raw()
  meta <- .cvpa_question_meta(cat_df, question)
  if (meta$type == "party") {
    cl <- .cvpa_code_labels()
    lut <- cl[cl$variable == question, c("code", "label", "sort_order")]
    lut <- lut[order(lut$sort_order), ]
    rownames(lut) <- NULL
    return(lut)
  }
  codes <- .cvpa_type_codes[[meta$type]]
  data.frame(code = codes, label = meta$value_labels, sort_order = seq_along(codes))
}

# Decodes one question's raw resp data.frame in place: every demographic +
# vote_intention/vote_choice column present becomes a factor via
# code_labels, and `response` becomes a factor via question_response_labels()
# (party questions) or vbl_data.value_labels applied positionally against
# .cvpa_type_codes (b/lsm/nsa questions) -- exactly how app.js's
# fetchChartData() labels the same columns, just done once in R instead of
# per-query in SQL.
.cvpa_decode_question <- function(df, question) {
  cl <- .cvpa_code_labels()
  demo_cols <- intersect(cvpa_grouping_vars, names(df))
  for (v in demo_cols) {
    lut <- cl[cl$variable == v, ]
    lut <- lut[order(lut$sort_order), ]
    df[[v]] <- factor(df[[v]], levels = lut$code, labels = lut$label)
  }
  labels <- question_response_labels(question)
  df$response <- factor(df$response, levels = labels$code, labels = labels$label)
  df
}

#' Fetch and decode one question's respondent-level data
#'
#' Reads the locally cached `resp/question=<question>/data_0.parquet` file
#' (see [update_cvpa_data()]) and, by default, decodes `response` and every
#' demographic/vote-grouping column (see [cvpa_grouping_vars]) into labeled
#' factors. Works identically for a policy question and for
#' `"vote_choice"`/`"vote_intention"` -- vote is just one more question.
#'
#' @param question A question code from [question_catalog()], e.g.
#'   `"bri_econ_013"` or `"vote_choice"`.
#' @param years Optional integer vector to filter `year` to.
#' @param decode If `TRUE` (default), decode `response` and the
#'   demographic/vote columns into labeled factors; if `FALSE`, return the
#'   raw numeric codes as stored in the Parquet file.
#' @return A data frame with one row per respondent-question-wave: `response`,
#'   `weight`, `year`, `response_id`, `survey_id`, and the columns in
#'   [cvpa_grouping_vars].
#' @examples
#' \dontrun{
#' update_cvpa_data(questions = "bri_econ_013")
#' d <- question_data("bri_econ_013")
#' wtd_response(d, grouping_vars = "gender")
#' }
#' @export
question_data <- function(question, years = NULL, decode = TRUE) {
  path <- .cvpa_path_resp(question)
  .cvpa_require_cache(
    path, sprintf("Question '%s'", question),
    sprintf('update_cvpa_data(questions = "%s")', question)
  )
  df <- nanoparquet::read_parquet(path)
  if (!is.null(years)) df <- df[df$year %in% years, , drop = FALSE]
  if (decode) df <- .cvpa_decode_question(df, question)
  df
}
