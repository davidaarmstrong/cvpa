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
#'   \item{region}{Factor indicating region of residence: Atlantic, Quebec, Ontario, Prairies, BC, The North}
#'   \item{province}{Factor indicating province of residence: Newfoundland and Labrador, PEI, Nova Scotia, New Brunswick, Quebec, Ontario, Manitoba, Saskatchewan, Alberta, BC, Yukon, Northwest Territories, Nunavut}
#'   \item{community_size}{Factor identifying community size of respondent's residence: < 100,000, >= 100,000}
#'   \item{gender}{Factor identifying respondent gender Man, Woman.  Non-binary category begins in 2019 and is available in the full data on the \href{https://www.queensu.ca/cora/}{Canadian Opinion Research Archive}.}
#'   \item{age_cats}{Factor identifying respondent's age category: 18-29, 30-49, 50+}
#'   \item{religion}{Factor giving respondent's religious affiliation: Catholic, Protestant, Jewish, Other, None}
#'   \item{degree}{Factor indicating respondent's level of education: No Degree, University Degree}
#'   \item{union_household}{Factor identifying union membership: Non-union Household, Union Household}
#'   \item{language}{Language spoken by respondent - English, French, English + French, Other}
#'   \item{occupation}{Respondent's Occupation: Managers/Executives/Business Owners, Professional, Routine Non-Manual, Working Class, Armed Forces, Farmer/Fisherman, Retired, Unemployed, Student, Other (incl. housewife, homemaker)}
#'   \item{weight}{Sum of weights for demographic profile}
#'}
#'
#' @references
#' Lucas, Jack, Tyler Romualdi, David A. Armstrong II and Elizabeth Goodyear-Grant. "Demographic Divides in Canadian Voting, 1945-2023: The Canadian Vote Intention Dataset", Canadian Journal of Political Science, Forthcoming.  
#'
#'
#' @keywords datasets
NULL

