utils::globalVariables(c("int", "count", "cnt0", "cnt1", "n0", "n1",
                         "year", "estimate1", "estimate2", "statistic",
                         "parameter", "method", "vote_intention",
                         "vote_choice", "alternative", "weight",
                         "party_support", "val", "type", "vote", "gender"))

#' Make weighted mean of vote intention/choice
#'
#' @param data Data from inst/extdata/integrated_with_weights.RDS.
#' @param vote_type Use vote intention or choice
#' @param incl_undecided If using vote intention, should undecided voters be included?
#' @param years Select desired years
#' @param grouping_vars For which groups do you want separate estimates (region or province, gender, age_cats, degree, religion, language, union_household, community_size)
#' @param ... Other arguments to be passed down, not implemented
#' @examples
#' data(vote_data)
#' wtd_vote(vote_data, "intention", years=2000:2022)
#'
#' @importFrom dplyr `%>%` filter select mutate summarise group_by all_of rowwise across contains
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
  avail_grps <- c("age_cats", "religion", "degree", "gender", "province", "region", "language", "union_household", "community_size", "occupation")
  if(any(!grouping_vars %in% avail_grps)){
    nogrp <- setdiff(grouping_vars, avail_grps)
    stop(paste0("The following grouping variables are not present in the data: ", paste(nogrp, collapse=", "), "\n"))
  }
  data <- data %>% filter(type == {{v}} & !is.na(vote))
  if(!incl_undecided)data <- data %>% filter(vote != "Undecided")
  avail_yrs <- sort(unique(data$year))
  if(any(!years %in% avail_yrs)){
    yrs_out <- setdiff(years, avail_yrs)
    years <- intersect(years, avail_yrs)
    message(paste0("The following years are not available: ", paste(yrs_out, collapse=", ")))
  }
  if("region" %in% grouping_vars & "province" %in% grouping_vars){
    message("You can only choose one of region or province; region has been selected.\n")
    grouping_vars <- grouping_vars[-which(grouping_vars == "province")]
  }
  gv2 <- c("year", grouping_vars)
  grouping_vars <- c("year", "vote", grouping_vars)
  res <- data %>%
    filter(year %in% years) %>%
    dplyr::select(all_of(c(grouping_vars, "weight"))) %>%
    na.omit() %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(count = sum(weight, na.rm=TRUE)) %>%
    group_by(across(all_of(gv2))) %>%
    mutate(party_support = count/sum(count)) 
return(res)
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
                  inc_undecided = incl_undecided,
                  vote_type = v,
                  grouping_vars = gv,
                  ...)
  if(nrow(res) == 0)stop("No data available for selected years.\n")
  counts <- res %>% 
    select(-party_support) %>%
    pivot_wider(names_from = all_of(gv), values_from = count, names_prefix="cnt") %>% 
    filter(if_all(starts_with("cat"), ~.x > 5)) %>% 
    drop_na() %>% 
    filter(!if_any(starts_with("cnt"), ~.x == 0))
  levs <- gsub("^cnt(.*)", "\\1", names(counts)[3:4])
  counts <- counts %>% setNames(c("year", "vote", "cnt0", "cnt1"))
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
