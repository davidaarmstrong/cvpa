#' Vote Choice/Intention Data
#'
#' Data on vote choice and intention from several surveys. The data are in long form (in intentnion/choice) and aggregate to profile across the variables `region`-`language` described below. 
#'
#' @details Fill in some details
#'
#' @name vote_data
#' @docType data
#' @format A data frame with 78,614 rows and 11 variables
#' \describe{
#'   \item{vote}{Factor indicating vote choice or intention depending on \code{type}}
#'   \item{type}{String indicating vote whever \code{vote} is intention or actual choice}
#'   \item{year}{Year of survey}
#'   \item{region}{Region of residence: 1 = Atlantic, 2 = Quebec, 3 = Ontario, 4 = Prairies, 5 = BC, 6 = The North}
#'   \item{province}{Province of residence: 10 = Newfoundland and Labrador, 11 = PEI, 12 = Nova Scotia, 13 = New Brunswick, 
#'   24 = Quebec, 35 = Ontario, 46 = Manitoba, 47 = Saskatchewan, 48 = Alberta, 59 = BC, 
#'   60 = Yukon, 61 = Northwest Territories, 62 = Nunavut}
#'   \item{community_size}{Community size >= 100,000 (0/1)}
#'   \item{gender}{Respondent gender 1 = female, 0 = male}
#'   \item{age_cats}{Age categories: 1 = 17-29, 2 = 30-49, 3 = 50+}
#'   \item{religion}{Add religion description}
#'   \item{degree}{Respondent has a university degree (0/1)}
#'   \item{union_household}{Whether respondent is in a household with a union member (0/1)}
#'   \item{language}{Add language description}
#'   \item{weight}{Sum of weights for \code{region}-\code{langauge} profile}
#'}
#'
#' @references
#' One, Some 2024. "Describing the Data".
#'
#'
#' @keywords datasets
NULL

#' Issue Attitude Data
#'
#' Data on issue attitudes and demographics from several surveys. The data are in long form (in issue). 
#'
#' @details Fill in some details
#'
#' @name issue_data
#' @docType data
#' @format A data frame with 78,614 rows and 11 variables
#' \describe{
#'   \item{source}{Character string indicating source of survey data.}
#'   \item{survey_id}{Character string giving id of survey}
#'   \item{year}{Year of survey}
#'   \item{response_id}{Character string giving id of respondent}
#'   \item{issue}{Character indicating the issue being considered.}
#'   \item{val}{Value of respondent's choice on `issue`}
#'   \item{region}{Region of residence: 1 = Atlantic, 2 = Quebec, 3 = Ontario, 4 = Prairies, 5 = BC, 6 = The North}
#'   \item{province}{Province of residence: 10 = Newfoundland and Labrador, 11 = PEI, 12 = Nova Scotia, 13 = New Brunswick, 
#'   24 = Quebec, 35 = Ontario, 46 = Manitoba, 47 = Saskatchewan, 48 = Alberta, 59 = BC, 
#'   60 = Yukon, 61 = Northwest Territories, 62 = Nunavut}
#'   \item{community_size}{Community size >= 100,000 (0/1)}
#'   \item{gender}{Respondent gender 1 = female, 0 = male}
#'   \item{age_cats}{Age categories: 1 = 17-29, 2 = 30-49, 3 = 50+}
#'   \item{age}{Age of respondent, where available}
#'   \item{religion}{Add religion description}
#'   \item{degree}{Respondent has a university degree (0/1)}
#'   \item{union_household}{Whether respondent is in a household with a union member (0/1)}
#'   \item{language}{Add language description}
#'   \item{weight}{Sum of weights for \code{region}-\code{langauge} profile}
#'}
#'
#' @references
#' One, Some 2024. "Describing the Data".
#'
#'
#' @keywords datasets
NULL
