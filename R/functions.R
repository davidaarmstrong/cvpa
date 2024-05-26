utils::globalVariables(c("int", "count", "cnt0", "cnt1", "n0", "n1",
                         "year", "estimate1", "estimate2", "statistic",
                         "parameter", "method", "vote_intention",
                         "vote_choice", "alternative", "weight",
                         "party_support", "val", "type", "vote"))

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
#' @importFrom stats na.omit prop.test
#' @export
#'
wtd_vote <- function(
    data,
  vote_type = c("intention", "choice"),
  incl_undecided = FALSE,
  years = 1945:2022,
  grouping_vars = NULL,
  ...){
  v <- match.arg(vote_type)
  avail_grps <- c("age_cats", "religion", "degree", "gender", "province", "region", "language", "union_household", "community_size")
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
#' @param data Data from inst/extdata/integrated_with_weights.RDS.
#' @param vote_type Use vote intention or choice
#' @param incl_undecided If using vote intention, should undecided voters be included?
#' @param years Select desired years
#' @param grouping_var For which group do you want separate estimates (gender, degree, union_household, community_size)
#' @param ... Other arguments to be passed down, not implemented
#' @examples
#' data(vote_data)
#' gap_analysis(vote_data, "intention", years=2000:2022, grouping_var="gender")
#'
#' @importFrom tidyr pivot_wider unnest
#' @importFrom broom tidy
#' @export
gap_analysis <- function(
    data,
    vote_type = c("intention", "choice"),
    incl_undecided = FALSE,
    years = 1945:2022,
    grouping_var = c("gender", "degree", "union_household", "community_size"),
    ...){
  v <- match.arg(vote_type)
  gv <- match.arg(grouping_var)
  res <- wtd_vote(data,
                  years = years,
                  inc_undecided = incl_undecided,
                  vote_type = v,
                  grouping_vars = gv,
                  ...)
  if(nrow(res) == 0)stop("No data available for selected years.\n")
  counts <- res %>% select(-party_support) %>%
    pivot_wider(names_from = all_of(gv), values_from = count, names_prefix="cnt") %>%
    filter(cnt0 > 5 & cnt1 > 5) %>%
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
  counts
}
#' Make weighted mean of vote intention/choice
#'
#' @param data Data from inst/extdata/issue_data_with_weights.RDS.
#' @param issue Character string giving name of issue variable
#' @param years Select desired years
#' @param grouping_vars For which groups do you want separate estimates (region or province, gender, age_cats, degree, religion, language, union_household, community_size)
#' @param ... Other arguments to be passed down, not implemented
#' @examples
#' data(issue_data)
#' wtd_issue(issue_data, "wageprice_1", grouping_vars="degree")
#'
#' @importFrom rlang sym `:=`
#' @importFrom Hmisc wtd.mean
#' @export
#'
wtd_issue <- function(
    data,
    issue,
    years = 1945:2022,
    grouping_vars = NULL,
    ...){
  avail_grps <- c("age_cats", "religion", "degree", "woman", "province", "region", "language", "union_household", "community_size")
  if(any(!grouping_vars %in% avail_grps)){
    nogrp <- setdiff(grouping_vars, avail_grps)
    stop(paste0("The following grouping variables are not present in the data: ", paste(nogrp, collapse=", "), "\n"))
  }
  grouping_vars <- ifelse(grouping_vars == "gender", "woman", grouping_vars)
  grouping_vars <- ifelse(grouping_vars == "community_size", "com_100", grouping_vars)
  data <- data %>% select(all_of(c("issue", "val", "year", "weight", grouping_vars))) %>% 
    filter(issue == {{issue}}) %>% 
    na.omit()
  if(nrow(data) == 0){
    stop("No data available in selected grouping variables.\n")
  }
  avail_yrs <- sort(unique(data$year))
  use_yrs <- intersect(years, avail_yrs)
  data <- data %>% filter(year %in% use_yrs)
  if(nrow(data) == 0){
    stop("No data available in selected years.\n")
  }
  message(paste0("Years available for: ", issue, ": ", paste(use_yrs, collapse=", "), "\n"))
  if("region" %in% grouping_vars & "province" %in% grouping_vars){
    message("You can only choose one of region or province; region has been selected.\n")
    grouping_vars <- grouping_vars[-which(grouping_vars == "province")]
  }
  grouping_vars <- c("year", grouping_vars)
  res <- data %>%
    dplyr::select(all_of(c(grouping_vars, "issue", "val", "weight"))) %>%
    na.omit() %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise({{issue}} :=  Hmisc::wtd.mean(val, weights=weight, na.rm=TRUE))
  return(res)
}

#' Analyze Significance of Attitude Gaps
#'
#' @param data Data from inst/extdata/issue_data_with_weights.RDS.
#' @param issue Character string giving the issue to be used.
#' @param years Select desired years
#' @param grouping_var For which group do you want separate estimates (gender, degree, union_household, community_size)
#' @param ... Other arguments to be passed down, not implemented
#' @examples
#' data(issue_data)
#' attitude_gap_analysis(issue_data, "wageprice_1", grouping_var="gender")
#'
#' @importFrom tidyr pivot_wider unnest
#' @importFrom dplyr bind_rows
#' @importFrom stats aov reformulate
#' @export
attitude_gap_analysis <- function(
    data,
    issue,
    years = 1945:2022,
    grouping_var = c("gender", "degree", "union_household", "community_size"),
    ...){
  gv <- match.arg(grouping_var)
  data <- data %>% select(all_of(c(gv, "issue", "val", "year", "weight"))) %>% na.omit()
  if(nrow(data) == 0)stop("No data available for groups.\n")
  avail_yrs <- sort(unique(data$year))
  use_yrs <- intersect(years, avail_yrs)
  data <- data %>% filter(year %in% use_yrs)
  if(nrow(data) == 0){
    stop("No data available in selected years.\n")
  }
  # do weighted anova
  form <- reformulate(gv, response="val")
  sp <- split(data, data$year)
  aovs <- lapply(sp, \(x)aov(form, x, weights=weight))
  aovs <- lapply(aovs, sum_aov)
  bind_rows(aovs, .id="year")
}

#' Summarise aov object
#' 
#' Summarise an anova object as a weighted difference
#' of means test
#' @param x An object of class `aov`. 
#' @param ... Other arguments to be passed down, currently not implemented. 
#' @examples
#' data(issue_data)
#' a <- aov(val ~ union_household, 
#'          data=subset(issue_data, issue=="wageprice_1"))
#' sum_aov(a)
#' 
#' @importFrom dplyr tibble
#' @importFrom stats confint coef summary.aov
#' @export
sum_aov <- function(x, ...){
  tibble(est_0 = coef(x)[1], 
         est_1 = sum(coef(x)), 
         diff = coef(x)[2], 
         conf.low = confint(x)[2,1], 
         conf.high = confint(x)[2,2],
         pval = summary(x)[[1]][1,5])
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

