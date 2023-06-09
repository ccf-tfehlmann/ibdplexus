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
   dx_sf <- diagnosis %>%
      filter(DATA_SOURCE %in% c("SF_SPARC")) %>%
      filter(DIAG_CONCEPT_NAME %in% c("Crohn's Disease", "IBD Unclassified", "Ulcerative Colitis")) %>%
      left_join(encounter, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "DEIDENTIFIED_PATIENT_ID", "DATA_SOURCE", "VISIT_ENCOUNTER_ID", "ADMISSION_TYPE")) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      mutate(DIAGNOSIS = DIAG_CONCEPT_NAME) %>%
      dplyr::mutate(keep = ifelse(DATA_SOURCE == "SF_SPARC" & is.na(DIAG_STATUS_CONCEPT_NAME), 0, 1)) %>% # Smartform Data should have a DIAG_STATUS_CONCEPT_NAME equal to yes
      filter(keep == 1) %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_START_DATE,DATA_SOURCE, DIAGNOSIS) %>%
      arrange(DEIDENTIFIED_MASTER_PATIENT_ID, desc(VISIT_ENCOUNTER_START_DATE)) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      filter(VISIT_ENCOUNTER_START_DATE == max(VISIT_ENCOUNTER_START_DATE)) %>%
      mutate(c = paste0(DATA_SOURCE, "_", seq_along(DEIDENTIFIED_MASTER_PATIENT_ID))) %>%
      ungroup() %>%
      pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_START_DATE),
                  names_from = c,
                  values_from = DIAGNOSIS)

   dx_ecrf <-  diagnosis %>%
      filter(DATA_SOURCE %in% c( "ECRF_SPARC")) %>%
      filter(DIAG_CONCEPT_NAME %in% c("Crohn's Disease", "IBD Unclassified", "Ulcerative Colitis")) %>%
      left_join(encounter %>% filter(DATA_SOURCE %in% c( "ECRF_SPARC")), by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "DEIDENTIFIED_PATIENT_ID", "DATA_SOURCE", "VISIT_ENCOUNTER_ID", "ADMISSION_TYPE")) %>%
      mutate(DIAGNOSIS = DIAG_CONCEPT_NAME) %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_START_DATE,DATA_SOURCE, DIAGNOSIS, DIAGNOSIS_DATE) %>%
      arrange(DEIDENTIFIED_MASTER_PATIENT_ID,  desc(VISIT_ENCOUNTER_START_DATE)) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      filter(VISIT_ENCOUNTER_START_DATE == max(VISIT_ENCOUNTER_START_DATE)) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_START_DATE) %>%
      filter(case_when(!is.na(DIAGNOSIS_DATE) ~ dmy(DIAGNOSIS_DATE) == max(dmy(DIAGNOSIS_DATE)), TRUE ~ is.na(DIAGNOSIS_DATE))) %>%
      mutate(c = paste0(DATA_SOURCE, "_", seq_along(DEIDENTIFIED_MASTER_PATIENT_ID))) %>%
      ungroup() %>%
      pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_START_DATE),
                  names_from = c,
                  values_from = DIAGNOSIS)


  dx <- full_join(dx_sf, dx_ecrf, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "VISIT_ENCOUNTER_START_DATE")) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      filter(VISIT_ENCOUNTER_START_DATE == max(VISIT_ENCOUNTER_START_DATE)) #%>%
      #rowwise() %>%
      #mutate(`Crohn's Disease` = sum(c_across(contains("SPARC")) == "Crohn's Disease", na.rm = T),
      #       `Ulcerative Colitis` = sum(c_across(contains("SPARC")) == "Ulcerative Colitis", na.rm = T),
      #       `IBD Unclassified` = sum(c_across(contains("SPARC")) == "IBD Unclassified", na.rm = T))

    #START HERE ----
    # n <- grep("Crohn's Disease|Ulcerative Colitis|IBD Unclassified", names(dx))
    # dx$DIAGNOSIS <- names(dx)[n][max.col(dx[n], 'last')*NA^(rowSums(dx[n])==0)]

    if("SF_SPARC_2" %in% colnames(dx))
    {
      dx <- dx %>%
                    ungroup() %>%
        mutate(DIAGNOSIS = case_when(SF_SPARC_1 != SF_SPARC_2 & SF_SPARC_1 == ECRF_SPARC_1 ~ SF_SPARC_1,
                                     SF_SPARC_1 != SF_SPARC_2 & SF_SPARC_2 == ECRF_SPARC_1 ~ SF_SPARC_2,
                                     SF_SPARC_1 == SF_SPARC_2 ~ SF_SPARC_1,
                                     !is.na(SF_SPARC_1) & is.na(SF_SPARC_2) ~ SF_SPARC_1,
                                     TRUE ~ as.character(NA))) %>%
        mutate(DIAGNOSIS = ifelse(is.na(DIAGNOSIS), ECRF_SPARC_1, DIAGNOSIS))
    } else
    {
      dx <- dx %>%  ungroup() %>% mutate(DIAGNOSIS = ifelse(is.na(SF_SPARC_1), ECRF_SPARC_1, SF_SPARC_1))
    }


     dx <- dx %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DIAGNOSIS) %>%
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
    drop_na(GENDER) %>%
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
#' @export
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

  # combine multipe entries per subject into a single line.
  race <- race %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    summarise(across(everything(),~ paste0(sort(unique(.x[!is.na(.x)])), collapse = "; "))) %>%
    ungroup()
  # race <- data.table::as.data.table(race)
  # race <- race[, lapply(.SD, function(x) {
  #   paste0(unique(x[!is.na(x)]), collapse = "; ")
  # }), by = DEIDENTIFIED_MASTER_PATIENT_ID]
}


#' extract_labs
#'
#' Filter lab crf data for the hsCRP or fecal calprotectin lab results performed by the Crohn's & Colitis Foundation on biosamples collected for SPARC.
#'
#' @description
#' For the hsCRP analysis done by the central lab, the Beckman Coulter analyzer and calibration kit was used (https://mms.mckesson.com/product/727502/Beckman-Coulter-ODR3021). For the
#' fecal calprotectin analysis we used the Buhlmann fCAL ELISA (https://buhlmannlabs.com/buhlmann-fcal-elisa/#laboratory).
#'
#'
#' @param labs A dataframe with the laboratory results in it. This can be loaded using load_data(datadir = "data/", cohort = "SPARC", domains = c("labs"), data_type = "CRF")
#' @param test The test of interest - either fcal for fecal calprotectin or hscrp for High-sensitivity C-reactive Protein.
#'
#' @return A dataframe with the master patient id, lab result, units and date of specimen collection
#' @export
#'
extract_labs <- function(labs, test){

  if (test == "hscrp") {
    result <- labs %>%
      filter(LAB_TEST_CONCEPT_NAME == "HIGH-SENSITIVITY C-REACTIVE PROTEIN (MG/L)")
  } else {
    result <- labs %>%
      filter(grepl("FECAL CALPROTECTIN", LAB_TEST_CONCEPT_NAME))
  }

  result <- result %>%
    mutate(SPECIMEN_COLLECTION_DATE = dmy(SPECIMEN_COLLECTION_DATE)) %>%
    mutate(LAB_RESULTS = ifelse(is.na(LAB_RESULTS), TEST_RESULT_NUMERIC, LAB_RESULTS)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID,LAB_TEST_CONCEPT_NAME,SPECIMEN_COLLECTION_DATE, LAB_RESULTS, TEST_UNIT)


}
