#' cd_codes
#'
#' A dataframe containing the coding for Crohn's Disease Phenotypes
#'
#' @format A data frame with 40 rows and 4 variables:
#' \describe{
#'   \item{Phenotype}{Crohn's Disease Phenotype}
#'   \item{CODING}{Yes, No or Unknown}
#'   \item{OBS_TEST_CONCEPT_CODE}{Corresponding Concept Code for each coding}
#'   \item{TIER}{Level of importance for data capture}
#'
#' }
"cd_codes"

#' eim_codes
#'
#' A dataframe containing the coding for Extra Intestinal Manifestations
#'
#' @format A data frame with 21 rows and 3 variables:
#' \describe{
#'   \item{EIM_DX}{Extra Intestional Manifestation}
#'   \item{CODING}{Yes/No or Unknown}
#'   \item{DIAG_CONCEPT_CODE}{Corresponding Concept Code for each coding}
#'
#' }
"eim_codes"


#' header
#'
#' A dataframe containing the column names in order for the Risk Summary Table.
#'
#' @format A data frame with 202 rows and 1 variables:
"header"


#' med_grp
#'
#' A dataframe containing the coding for Medications. Allows to group the same
#' medications together if they have different names (generic v. scientific)
#' from different source.
#'
#' @format A data frame with 90 rows and 3 variables:
#' \describe{
#'   \item{MEDICATION_NAME}{Medication Name as it appears in SF, ECRF or EMR}
#'   \item{med_type}{The class of medication}
#'   \item{new_med_name}{Standardized medication name}
#'
#' }
"med_grp"
