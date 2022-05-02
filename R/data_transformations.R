# This file contains functions that prepare single or multiple output columns 
# for higher level functions such as sparc_scores_at_index. 


#' extract_consent
#'
#' Extract consent data from a table with demographic information.
#' 
#' This function is based on sparc_summary_at_index_v2.R other similar versions may behave differently.
#' In contrast to the original. Any patient with a unique date on consent but multiple dates of withdrawal will be reported multiple times.
#'
#' @param demographics A dataframe with demographics data.
#' @param data_source Cohort from which the consent data should be extracted.
#'
#' @return A dataframe with consent data.
extract_consent <- function(demographics, data_source )
{
  consent = demographics %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DATE_OF_CONSENT, DATE_OF_CONSENT_WITHDRAWN) %>%
    mutate(DATE_OF_CONSENT = dmy(DATE_OF_CONSENT),
           DATE_OF_CONSENT_WITHDRAWN = dmy(DATE_OF_CONSENT_WITHDRAWN)) %>%
    filter(year(DATE_OF_CONSENT) >= 2016) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    slice_min(order_by = DATE_OF_CONSENT) %>%
    ungroup()
}