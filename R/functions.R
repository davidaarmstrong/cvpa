utils::globalVariables(c("int", "count", "cnt0", "cnt1", "n0", "n1",
                         "year", "estimate1", "estimate2", "statistic",
                         "parameter", "method", "vote_intention",
                         "vote_choice", "alternative", "weight",
                         "party_support", "val", "type", "vote", "gender",
                         "response", "response_share"))

# Legacy grouping-variable names accepted by wtd_vote()/gap_analysis(),
# matched to the bundled `vote_data` dataset's own column names (it predates
# the cp3 Parquet pipeline and, notably, has a single `community_size`
# column rather than cp3's com_100/com_500 split -- see [demographic_vars]).
# Kept exactly as it was before this file started delegating to
# wtd_response(), so nothing about wtd_vote()'s validation behavior changes.
.cvpa_legacy_vote_grps <- c("age_cats", "religion", "degree", "gender", "province",
                            "region", "language", "union_household",
                            "community_size", "occupation")

#' Weighted proportion of each response category, by year and group
#'
#' The general engine behind [wtd_vote()]: given a data frame with a
#' `response` column (any set of categories -- vote choice, a policy
#' question's answer categories, whatever `response_var` points at),
#' `weight`, `year`, and zero or more grouping columns, compute each
#' response category's weighted share of the total for every
#' year/grouping-variable combination. [question_data()] produces data in
#' exactly this shape (its `response` column decoded to a factor by
#' question type), so this works identically for a policy question or for
#' vote choice/intention -- see [response_gap_analysis()] for the
#' significance-test analog.
#'
#' @param data A data frame as returned by [question_data()] (or any data
#'   frame with the same shape: `response_var`, `weight`, `year`, plus
#'   whichever of `avail_grps` you want to group by).
#' @param years Select desired years. `NULL` (default) uses every year in
#'   `data`.
#' @param grouping_vars For which groups do you want separate estimates;
#'   must be a subset of `avail_grps`.
#' @param avail_grps Grouping variables considered valid; defaults to
#'   [cvpa_grouping_vars].
#' @param response_var Name of the column in `data` holding the response
#'   category. Defaults to `"response"`, the column [question_data()]
#'   produces.
#' @return A data frame with `year`, `grouping_vars`, the response
#'   category (named `response_var`), `count` (summed weight), and
#'   `response_share` (that category's share of the year/group total).
#' @examples
#' \dontrun{
#' update_cvpa_data(questions = "bri_econ_013")
#' d <- question_data("bri_econ_013")
#' wtd_response(d, grouping_vars = "gender")
#' }
#' @importFrom dplyr `%>%` filter select mutate summarise group_by all_of rowwise across contains rename ungroup
#' @importFrom stats na.omit
#' @export
wtd_response <- function(data, years = NULL, grouping_vars = NULL,
                          avail_grps = cvpa_grouping_vars, response_var = "response") {
  if (any(!grouping_vars %in% avail_grps)) {
    nogrp <- setdiff(grouping_vars, avail_grps)
    stop(paste0("The following grouping variables are not present in the data: ",
                paste(nogrp, collapse = ", "), "\n"))
  }
  if (!identical(response_var, "response")) {
    data <- data %>% dplyr::rename(response = dplyr::all_of(response_var))
  }
  data <- data %>% dplyr::filter(!is.na(response))
  if (!is.null(years)) {
    avail_yrs <- sort(unique(data$year))
    if (any(!years %in% avail_yrs)) {
      yrs_out <- setdiff(years, avail_yrs)
      years <- intersect(years, avail_yrs)
      message(paste0("The following years are not available: ", paste(yrs_out, collapse = ", ")))
    }
    data <- data %>% dplyr::filter(year %in% years)
  }
  gv2 <- c("year", grouping_vars)
  all_grp <- c("year", "response", grouping_vars)
  res <- data %>%
    dplyr::select(dplyr::all_of(c(all_grp, "weight"))) %>%
    stats::na.omit() %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(all_grp))) %>%
    dplyr::summarise(count = sum(weight, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(gv2))) %>%
    dplyr::mutate(response_share = count / sum(count)) %>%
    dplyr::ungroup()
  if (!identical(response_var, "response")) {
    res <- res %>% dplyr::rename(!!response_var := response)
  }
  res
}

#' Make weighted mean of vote intention/choice
#'
#' @param data Data from inst/extdata/integrated_with_weights.RDS.
#' @param vote_type Use vote intention or choice
#' @param incl_undecided If using vote intention, should undecided voters be included?
#' @param years Select desired years
#' @param grouping_vars For which groups do you want separate estimates (region or province, gender, age_cats, degree, religion, language, union_household, community_size)
#' @param ... Other arguments to be passed down, not implemented
#' @details Since 0.2.0, this is a thin wrapper around the general
#'   [wtd_response()] engine that also powers policy-question analysis (see
#'   [question_data()]) -- its inputs, outputs, and validation behavior are
#'   unchanged from earlier versions.
#' @examples
#' data(vote_data)
#' wtd_vote(vote_data, "intention", years=2000:2022)
#'
#' @importFrom dplyr `%>%` filter select mutate summarise group_by all_of rowwise across contains rename
#' @importFrom stats na.omit prop.test setNames
#' @importFrom rio factorize
#' @importFrom rlang sym
#' @export
#'
wtd_vote <- function(
    data,
  vote_type = c("intention", "choice"),
  incl_undecided = FALSE,
  years = 1945:2023,
  grouping_vars = NULL,
  ...){
  v <- match.arg(vote_type)
  data <- data %>% filter(type == {{v}} & !is.na(vote))
  if(!incl_undecided) data <- data %>% filter(vote != "Undecided")
  if("region" %in% grouping_vars & "province" %in% grouping_vars){
    message("You can only choose one of region or province; region has been selected.\n")
    grouping_vars <- grouping_vars[-which(grouping_vars == "province")]
  }
  res <- wtd_response(data, years = years, grouping_vars = grouping_vars,
                       avail_grps = .cvpa_legacy_vote_grps, response_var = "vote")
  res %>% dplyr::rename(party_support = response_share)
}

# Shared post-aggregation step for gap_analysis()/response_gap_analysis():
# takes wtd_vote()'s or wtd_response()'s output (year, a category column,
# one grouping column, count, share), reshapes the two grouping-variable
# levels into columns, and runs a difference-of-proportions test for every
# (year, category) cell. `category_col`/`share_col` are just the column
# names to operate on -- the statistical logic is identical either way.
#' @importFrom tidyr pivot_wider unnest drop_na
#' @importFrom dplyr if_any starts_with if_all
#' @importFrom broom tidy
.cvpa_gap_stats <- function(res, category_col, share_col, group_col) {
  counts <- res %>%
    select(-dplyr::all_of(share_col)) %>%
    pivot_wider(names_from = dplyr::all_of(group_col), values_from = count, names_prefix="cnt") %>%
    filter(if_all(starts_with("cnt"), ~.x > 5)) %>%
    drop_na() %>%
    filter(!if_any(starts_with("cnt"), ~.x == 0))
  levs <- gsub("^cnt(.*)", "\\1", names(counts)[3:4])
  counts <- counts %>% setNames(c("year", category_col, "cnt0", "cnt1"))
  counts <- counts %>%
    group_by(year) %>%
    mutate(n0 = sum(cnt0),
           n1 = sum(cnt1)) %>%
    rowwise() %>%
    mutate(flag = make_flag(c(cnt0, cnt1), c(n0,n1)),
           res = broom::tidy(suppressWarnings(prop.test(c(cnt0, cnt1), c(n0,n1))))) %>%
    unnest(res) %>%
    mutate(diff = estimate2-estimate1,
           across(contains("conf"), ~-.x)) %>%
    select(-c(statistic, parameter, method, alternative))
  names(counts)[3:6] <- gsub("0$", paste0("_", levs[1]), names(counts)[3:6])
  names(counts)[3:6] <- gsub("1$", paste0("_", levs[2]), names(counts)[3:6])
  counts
}

#' Analyze Significance of Voting Gaps
#'
#' The function does a difference of proportions test for the
#' two identified groups for each of the parties.  Note, for gender,
#' there are not many surveys with non-binary/other response options
#' so those are removed from consideration here.
#'
#' @param data Data from inst/extdata/integrated_with_weights.RDS.
#' @param vote_type Use vote intention or choice
#' @param incl_undecided If using vote intention, should undecided voters be included?
#' @param years Select desired years
#' @param grouping_var For which group do you want separate estimates (gender, degree, union_household, community_size)
#' @param levels Character vector of length 2 giving values of `grouping_var` to use for comparison.  `NULL` is valid if `grouping_var` only has two valid values.  Otherwise, values must be specified.
#' @param ... Other arguments to be passed down, not implemented
#' @details See [response_gap_analysis()] for the same test generalized to
#'   any policy question, not just vote choice/intention.
#' @examples
#' data(vote_data)
#' gap_analysis(vote_data, "intention", years=2000:2022, grouping_var="gender")
#'
#' @importFrom tidyr pivot_wider unnest drop_na
#' @importFrom dplyr if_any starts_with if_all
#' @importFrom broom tidy
#' @export
gap_analysis <- function(
    data,
    vote_type = c("intention", "choice"),
    incl_undecided = FALSE,
    years = 1945:2022,
    grouping_var = c("age_cats", "religion", "degree", "gender", "province", "region", "language", "union_household", "community_size", "occupation"),
    levels = NULL,
    ...){
  v <- match.arg(vote_type)
  gv <- match.arg(grouping_var)
  if(gv == "gender"){
    data <- data %>% filter(gender %in% c("Man", "Woman"))
  }
  if(!is.null(levels)){
    data <- data %>% filter(!!sym(gv) %in% levels)
    if(!all(levels %in% unique(data[[gv]]))){
      stop(paste0(setdiff(levels, unique(data[[gv]])), " is not among the values of the grouping variable.\n"))
    }
  }
  nl <- length(unique(na.omit(data[[gv]])))
  if(nl != 2){
    stop("Grouping variable must have only two levels.\n")
  }
  res <- wtd_vote(data,
                  years = years,
                  incl_undecided = incl_undecided,
                  vote_type = v,
                  grouping_vars = gv,
                  ...)
  if(nrow(res) == 0)stop("No data available for selected years.\n")
  .cvpa_gap_stats(res, category_col = "vote", share_col = "party_support", group_col = gv)
}

#' Analyze Significance of Response Gaps for any question
#'
#' Generalizes [gap_analysis()] to any question's data (see
#' [question_data()]), not just vote choice/intention: a difference of
#' proportions test between two levels of `grouping_var`, for each response
#' category, for each year.
#'
#' @param data A data frame as returned by [question_data()].
#' @param grouping_var Which grouping variable to compare two levels of;
#'   must be one of [cvpa_grouping_vars] (or `avail_grps`, if you pass a
#'   narrower/different list).
#' @param years Select desired years. `NULL` (default) uses every year in
#'   `data`.
#' @param levels Character vector of length 2 giving values of
#'   `grouping_var` to use for comparison. `NULL` is valid if
#'   `grouping_var` only has two valid values in `data`; otherwise values
#'   must be specified.
#' @param avail_grps Grouping variables considered valid; defaults to
#'   [cvpa_grouping_vars].
#' @param response_var Name of the column in `data` holding the response
#'   category; defaults to `"response"`.
#' @return A data frame with one row per (year, response category):
#'   the two groups' counts and sample sizes (suffixed with each group's
#'   label), a reliability `flag` (see [make_flag()]), the two estimated
#'   proportions, a p-value, a confidence interval for the difference, and
#'   `diff`.
#' @examples
#' \dontrun{
#' update_cvpa_data(questions = "bri_econ_013")
#' d <- question_data("bri_econ_013")
#' response_gap_analysis(d, grouping_var = "gender")
#' }
#' @importFrom tidyr pivot_wider unnest drop_na
#' @importFrom dplyr if_any starts_with if_all
#' @importFrom broom tidy
#' @importFrom rlang sym
#' @export
response_gap_analysis <- function(data, grouping_var, years = NULL, levels = NULL,
                                   avail_grps = cvpa_grouping_vars, response_var = "response") {
  if (!is.null(levels)) {
    data <- data %>% filter(!!sym(grouping_var) %in% levels)
    if (!all(levels %in% unique(data[[grouping_var]]))) {
      stop(paste0(setdiff(levels, unique(data[[grouping_var]])), " is not among the values of the grouping variable.\n"))
    }
  }
  nl <- length(unique(na.omit(data[[grouping_var]])))
  if (nl != 2) {
    stop("Grouping variable must have only two levels.\n")
  }
  res <- wtd_response(data, years = years, grouping_vars = grouping_var,
                       avail_grps = avail_grps, response_var = response_var)
  if (nrow(res) == 0) stop("No data available for selected years.\n")
  .cvpa_gap_stats(res, category_col = response_var, share_col = "response_share", group_col = grouping_var)
}

#' Make Reliability Flag for Weighted Proportions Test
#'
#' Makes a flag to identify if the p-value for the difference
#' of proportions test is reliable (all expected counts greater than 5).
#' @param x A vector of two values of category counts.
#' @param n A vector of two values of sample sizes
#' @export
make_flag <- function(x, n){
  p <- sum(x)/sum(n)
  o <- outer(n, p, "*")
  flag <- any(o < 5)
  ifelse(flag, "p-val unreliable", "p-val reliable")
}
