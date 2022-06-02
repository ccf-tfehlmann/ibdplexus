# This file contains functions that prepare single or multiple output columns
# for higher level functions such as sparc_scores_at_index.


#' extract_consent
#'
#' Extract consent data for SPARC & QORUS from a table with demographic information.
#'
#' When a patient withdraws and re-enrolls, the first date of consent is used.
#'
#'
#' @param demographics A dataframe with demographics data.
#' @param study Cohort from which the consent data should be extracted. Either SPARC or QORUS.
#'
#' @return A dataframe with consent data.
extract_consent <- function(demographics, study) {
  data_source <- paste0("ECRF_", toupper(study))

  consent <- demographics %>%
    filter(DATA_SOURCE == data_source) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DATE_OF_CONSENT, DATE_OF_CONSENT_WITHDRAWN) %>%
    mutate(
      DATE_OF_CONSENT = dmy(DATE_OF_CONSENT),
      DATE_OF_CONSENT_WITHDRAWN = dmy(DATE_OF_CONSENT_WITHDRAWN)
    ) %>%
    filter(year(DATE_OF_CONSENT) >= 2016) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, DATE_OF_CONSENT) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(c = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
    pivot_wider(names_from = c, values_from = c(DATE_OF_CONSENT, DATE_OF_CONSENT_WITHDRAWN)) %>%
    mutate(DATE_OF_CONSENT = DATE_OF_CONSENT_1) %>%
    mutate(DATE_OF_CONSENT_WITHDRAWN = if_else(is.na(DATE_OF_CONSENT_2), DATE_OF_CONSENT_WITHDRAWN_1, DATE_OF_CONSENT_WITHDRAWN_2)) %>%
    ungroup() %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, DATE_OF_CONSENT, DATE_OF_CONSENT_WITHDRAWN, everything())
}


#' extract_diagnosis
#'
#' Extract IBD diagnosis and diagnosis date for SPARC & QORUS from a table with diagnosis data.
#'
#'
#'
#' @param diagnosis A dataframe with diagnosis data.
#' @param encounter A dataframe with encounter data. Only required for SPARC.
#' @param demographics A dataframe with demographics data.
#' @param study Cohort for which diagnosis information is needed. Either SPARC or QORUS.
#'
#' @return A dataframe with diagnosis data.
extract_diagnosis <- function(diagnosis, encounter, demographics, study) {
  study <- toupper(study)

  if (study == "SPARC") {
    dx <- diagnosis %>%
      filter(DATA_SOURCE %in% c("SF_SPARC", "ECRF_SPARC")) %>%
      filter(DIAG_CONCEPT_NAME %in% c("Crohn's Disease", "IBD Unclassified", "Ulcerative Colitis")) %>%
      left_join(encounter, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "DEIDENTIFIED_PATIENT_ID", "DATA_SOURCE", "VISIT_ENCOUNTER_ID", "ADMISSION_TYPE")) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      mutate(DIAGNOSIS = DIAG_CONCEPT_NAME) %>%
      dplyr::mutate(keep = ifelse(DATA_SOURCE == "SF_SPARC" & is.na(DIAG_STATUS_CONCEPT_NAME), 0, 1)) %>% # Smartform Data should have a DIAG_STATUS_CONCEPT_NAME equal to yes
      filter(keep == 1) %>%
      arrange(DEIDENTIFIED_MASTER_PATIENT_ID, match(DATA_SOURCE, c("SF_SPARC", "ECRF_SPARC")), desc(dmy(VISIT_ENCOUNTER_START_DATE))) %>%
      slice(1) %>%
      select(DEIDENTIFIED_MASTER_PATIENT_ID, DIAGNOSIS) %>%
      ungroup()


    dx_date_ecrf <- diagnosis %>%
      filter(DATA_SOURCE == "ECRF_SPARC") %>%
      drop_na(DIAGNOSIS_DATE) %>%
      mutate(DIAGNOSIS_DATE = dmy(DIAGNOSIS_DATE)) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      slice(which.min(DIAGNOSIS_DATE)) %>%
      mutate(DIAGNOSIS_DATE = as.numeric(year(DIAGNOSIS_DATE))) %>%
      select(DEIDENTIFIED_MASTER_PATIENT_ID, DIAGNOSIS_DATE) %>%
      ungroup()

    dx_date_sf <- diagnosis %>%
      filter(DATA_SOURCE == "SF_SPARC") %>%
      filter(DIAG_CONCEPT_NAME %in% c("Crohn's Disease", "IBD Unclassified", "Ulcerative Colitis", "Inflammatory Bowel Disease")) %>%
      drop_na(DIAGNOSIS_DATE) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      slice(which.min(as.numeric(DIAGNOSIS_DATE))) %>%
      mutate(DIAGNOSIS_DATE = as.numeric(DIAGNOSIS_DATE)) %>%
      select(DEIDENTIFIED_MASTER_PATIENT_ID, DIAGNOSIS_DATE) %>%
      ungroup()

    dxy <- bind_rows(dx_date_sf, dx_date_ecrf) %>%
      left_join(demographics, by = "DEIDENTIFIED_MASTER_PATIENT_ID") %>%
      filter(DIAGNOSIS_DATE >= as.numeric(BIRTH_YEAR)) %>%
      arrange(DEIDENTIFIED_MASTER_PATIENT_ID, DIAGNOSIS_DATE) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      slice(which.min(DIAGNOSIS_DATE)) %>%
      ungroup() %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DIAGNOSIS_DATE)

    dx <- dx %>% left_join(dxy, by = "DEIDENTIFIED_MASTER_PATIENT_ID")
  } else {
    dx <- diagnosis %>%
      filter(DATA_SOURCE %in% c("ECRF_QORUS")) %>%
      filter(DIAG_CONCEPT_NAME %in% c("Crohn's Disease", "IBD Unclassified", "Ulcerative Colitis")) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      mutate(
        DIAGNOSIS = DIAG_CONCEPT_NAME,
        DIAGNOSIS_DATE = dmy(DIAGNOSIS_DATE),
        DISEASE_SITE = DIAG_SITE
      ) %>%
      arrange(DEIDENTIFIED_MASTER_PATIENT_ID, desc(DIAGNOSIS_DATE)) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      slice_max(order_by = DIAGNOSIS_DATE) %>%
      select(DEIDENTIFIED_MASTER_PATIENT_ID, DIAGNOSIS, DIAGNOSIS_DATE, DISEASE_SITE, DISEASE_PHENOTYPE) %>%
      ungroup()
  }
}

#' extract_demo
#'
#' Extract Birth Year and Sex for SPARC & QORUS from a table with demographic information.
#'
#' Preference is given to the EMR data source when available
#'
#'
#' @param demographics A dataframe with demographics data.
#' @param study Cohort from which the data should be extracted. Either SPARC or QORUS.
#'
#' @return A dataframe with birth year and sex information.
extract_demo <- function(demographics, study) {
  data_source <- paste0("ECRF_", toupper(study))

  demo <- demographics %>%
    filter(DATA_SOURCE %in% c("EMR", data_source)) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, desc(DATA_SOURCE)) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    slice(1) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, BIRTH_YEAR, GENDER) %>%
    mutate(BIRTH_YEAR = as.numeric(BIRTH_YEAR)) %>%
    rename(SEX = GENDER) %>%
    ungroup()
}

#' extract_race
#'
#' Extract race & ethnicity information SPARC & QORUS from a table with demographic information.
#' EMR and eCRF data sources are used
#'
#'
#' @param demographics A dataframe with demographics data.
#' @param study Cohort from which the data should be extracted. Either SPARC or QORUS.
#'
#' @return A dataframe with birth year and sex information.
extract_race <- function(demographics, study) {
  data_source <- paste0("ECRF_", toupper(study))

  race <- demographics %>%
    filter(DATA_SOURCE %in% c(data_source, "EMR")) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DATA_SOURCE, RACE, ETHNICITY, RACE_OTHER) %>%
    mutate(across(everything(), ~ replace(., . %in% c("N.A.", "NA", "N/A", ""), NA))) %>%
    rowwise() %>%
    mutate(s = sum(is.na(c_across(ETHNICITY:RACE_OTHER)))) %>%
    filter(s < 3) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, DATA_SOURCE) %>%
    mutate(RACE = ifelse(RACE %in% c("Other", "Other Race") & !is.na(RACE_OTHER), RACE_OTHER, RACE)) %>%
    select(-RACE_OTHER) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, RACE, ETHNICITY) %>%
    ungroup()

  race <- data.table::as.data.table(race)
  race <- race[, lapply(.SD, function(x) {
    paste0(unique(x[!is.na(x)]), collapse = "; ")
  }), by = DEIDENTIFIED_MASTER_PATIENT_ID]
}
