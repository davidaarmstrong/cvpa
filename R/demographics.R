#' Demographic variables available on every question's response data
#'
#' Every table [question_data()] returns carries these eleven demographic
#' columns (denormalized onto each response row by cp3's data pipeline),
#' in addition to `vote_intention` and `vote_choice`, which are also always
#' present and usable as grouping variables in their own right (see
#' [cvpa_grouping_vars]). Category lists are given in the order the
#' underlying `code_labels` table sorts them, which is also the factor
#' level order [question_data()] applies.
#'
#' @format A data frame with 11 rows and 3 variables:
#' \describe{
#'   \item{variable}{Column name as it appears in [question_data()]'s output}
#'   \item{description}{What the variable measures}
#'   \item{categories}{Ordered category labels, separated by `"; "`}
#' }
#' @seealso [cvpa_grouping_vars], [question_data()]
#' @examples
#' demographic_vars
#' @export
demographic_vars <- data.frame(
  variable = c(
    "region", "province", "com_500", "com_100", "gender", "age_cats",
    "religion", "degree", "language", "union_household", "occupation"
  ),
  description = c(
    "Region of residence",
    "Province or territory of residence",
    "Community size, split at 500,000 population",
    "Community size, split at 100,000 population",
    "Respondent gender. Binary Man/Woman only -- a non-binary/other response option exists in some surveys from 2019 onward but is too sparse to include here; see the Canadian Opinion Research Archive (https://www.queensu.ca/cora/) for the full data.",
    "Age category",
    "Religious affiliation",
    "University degree attainment",
    "Language spoken",
    "Union membership in the household",
    "Respondent's occupation"
  ),
  categories = c(
    "Atlantic; Quebec; Ontario; Prairies; British Columbia; North",
    "NL; PEI; NS; NB; QC; ON; MB; SK; AB; BC; YT; NT; NU",
    "Population <= 500k; Population > 500k",
    "Population <= 100k; Population > 100k",
    "Man; Woman",
    "18-29; 30-49; 50+",
    "Catholic; Protestant; Jewish; Other; None",
    "No University Degree; University Degree",
    "French; English; French and English; Other",
    "No Union Member in Household; Union Member in Household",
    "Managers, Executives; Professional; Routine Non-manual; Working Class; Armed Forces; Farmer/Fiserman; Retired; Unemployed; Student; Other"
  ),
  stringsAsFactors = FALSE
)
