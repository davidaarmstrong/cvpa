utils::globalVariables(c("int", "count", "cnt0", "cnt1", "n0", "n1",
                         "year", "estimate1", "estimate2", "statistic",
                         "parameter", "method", "vote_intention",
                         "vote_choice", "alternative", "weight",
                         "party_support"))

# int <- readRDS("data/integrated_with_weights.RDS")

#' Make weighted mean of vote intention/choice
#'
#' @param data Data from inst/extdata/integrated_with_weights.RDS.
#' @param vote_type Use vote intention or choice
#' @param incl_undecided If using vote intention, should undecided voters be included?
#' @param years Select desired years
#' @param grouping_vars For which groups do you want separate estimates (region or province, gender, age_cats, degree, religion, language, union_household, community_size)
#' @param ... Other arguments to be passed down, not implemented
#' @examples
#' isd <- readRDS(system.file("extdata/integrated_with_weights.RDS", package = "cvpa"))
#' wtd_vote(isd, "intention", years=2000:2022)
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
  avail_grps <- c("age_cats", "religion", "degree", "woman", "province", "region", "language", "union_household", "community_size")
  if(!incl_undecided & v == "intention")data <- data %>% filter(vote_intention != "Undecided")
  vv <- ifelse(v == "intention", "vote_intention", "vote_choice")
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
  grouping_vars <- ifelse(grouping_vars == "gender", "woman", grouping_vars)
  grouping_vars <- ifelse(grouping_vars == "community_size", "com_100", grouping_vars)
  gv2 <- c("year", grouping_vars)
  grouping_vars <- c("year", vv, grouping_vars)
  res <- data %>%
    filter(year %in% years & !is.na(vv)) %>%
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
#' isd <- readRDS(system.file("extdata/integrated_with_weights.RDS", package = "cvpa"))
#' gap_analysis(isd, "intention", years=2000:2022, grouping_var="gender")
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
  make_flag <- function(x, n){
    p <- sum(x)/sum(n)
    o <- outer(n, p, "*")
    flag <- any(o < 5)
    ifelse(flag, "p-val unreliable", "p-val reliable")

  }
  v <- match.arg(vote_type)
  gv <- match.arg(grouping_var)
  gv <- ifelse(gv == "gender", "woman", gv)
  gv <- ifelse(gv == "community_size", "com_100", gv)
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


