# This file contains functions that create the index date data frames for commonly asked time points.


#' extract_endoscopy
#'
#' Extract endoscopy data for SPARC.
#'
#'
#'
#' @param procedures A dataframe with procedures data.
#'
#' @return A dataframe with endoscopy date and indication data.

extract_endoscopy <- function(procedures) {
  endoscopy <- procedures %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    filter(PROC_CONCEPT_NAME %in% c("Colonoscopy/Sigmoidoscopy", "Pouchoscopy")) %>%
    mutate(PROC_START_DATE = dmy(PROC_START_DATE)) %>%
    rename(index_date = PROC_START_DATE) %>%
    drop_na(index_date) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, INDICATION) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
    fill(INDICATION, .direction = "downup") %>%
    ungroup() %>%
    distinct() %>%
    mutate(c = 1) %>%
    pivot_wider(names_from = c, values_from = INDICATION, values_fn = ~paste0(.x, collapse = "; ")) %>%
    mutate(INDICATION_FOR_ENDOSCOPY = ifelse(`1` == "NA", as.character(NA), `1`)) %>%
    select(-`1`)
}



#' extract_latest
#'
#' Extract most recent smartform or eCRF date for SPARC.
#'
#'
#'
#' @param encounter A dataframe with encounter data.
#' @param datasource the data source(s) of interest in a character string. Default is c("SF_SPARC", "ECRF_SPARC").
#'
#' @return A dataframe with  date and type of most recent encounter.
#' @export
extract_latest <- function(encounter, datasource = c("SF_SPARC", "ECRF_SPARC")) {
  if ("ECRF_SPARC" %in% datasource) {
    datasource <- append(datasource, "ECRF")
  }

  latest <- encounter %>%
    filter(DATA_SOURCE %in% datasource) %>%
    filter(grepl("Smartform|Survey", TYPE_OF_ENCOUNTER, ignore.case = T)) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, desc(DATA_SOURCE), desc(VISIT_ENCOUNTER_START_DATE)) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    slice(which.max(VISIT_ENCOUNTER_START_DATE)) %>%
    ungroup() %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_START_DATE, TYPE_OF_ENCOUNTER) %>%
    rename(
      index_date = VISIT_ENCOUNTER_START_DATE,
      MOST_RECENT_ENCOUNTER_TYPE = TYPE_OF_ENCOUNTER
    ) %>%
    mutate(MOST_RECENT_ENCOUNTER_TYPE = case_when(
      grepl("Smartform", MOST_RECENT_ENCOUNTER_TYPE, ignore.case = T) ~ "Smartform",
      TRUE ~ MOST_RECENT_ENCOUNTER_TYPE
    ))
}
