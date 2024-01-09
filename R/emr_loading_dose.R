#' emr_loading_pattern
#'
#' Find if emr prescriptions follows known loading patterns for biologics
#'
#'
#'
#' @param medication A dataframe with filtered prescriptions data usually generated using load_data.
#' @param encounter a dataframe with encounter data usually generated using load_data.
#'
#' @return A dataframe with the first medication start date for each drug. Bionaive is 1 if a patient has no prior reported biologics (including JAK inhibitors and Tofacitinib). Started after enrollment is 1 if the medication start date is after the date of consent.
#'
#'
#'
emr_loading_pattern <- function(medication, encounter) {
  # EMR

  loading_emr <- medication %>%
    filter(DATA_SOURCE == "EMR") %>%
    filter(!is.na(MED_START_DATE) | !is.na(MED_END_DATE)) %>%
    left_join(encounter) %>%
    filter(year(MED_START_DATE) > 1900) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, MED_START_DATE) %>%
    select(
      DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_START_DATE, new_med_name, ROUTE_OF_MEDICATION, MEDICATION_DOMAIN, MED_START_DATE,
      MED_END_DATE, DOSE_OF_MEDICATION, CURRENT_MEDICATION, OTHER_MEDICATION, UNIT_OF_MEASURE_FOR_MEDICATION, MEDICATION_FREQUENCE,
      MEDICATION_ADMINISTRATED_CODE, MEDICATION_ADMINISTRATED, FREQUENCY_IN_DAYS, REASON_STOPPED, SUMMARY,
      MED_DISCONT_START_DATE, MEDICATION_STRENGTH, MED_STRENGTH_UNIT_OF_MEASURE, MEDICATION_QUANTITY, MED_QUANTITY_UOM, MED_FORM,
      MEDICATION_TREATMENT_COURSE, FREQUENCE_UNIT_OF_MEASURE, MEDICATION_ADMIN_DURATION, MED_ADMIN_DURATION_UOM,
      GENERIC_MEDICINE_FLAG, SUBSTITUTE_MED_INDICATION_FLAG, PLACE_OF_SERVICE, MEDICATION_REFILLS
    ) %>%
    distinct() %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name) %>%
    mutate(weeks_between_med = difftime(MED_START_DATE, lag(MED_START_DATE), units = "weeks")) %>%
    mutate(dose = ifelse(is.na(DOSE_OF_MEDICATION), MEDICATION_STRENGTH, DOSE_OF_MEDICATION)) %>%
    mutate(route = ifelse(is.na(ROUTE_OF_MEDICATION), MED_FORM, ROUTE_OF_MEDICATION)) %>%
    mutate(first_date = case_when(is.na(weeks_between_med) ~ MED_START_DATE)) %>%
    fill(first_date, .direction = c("down"))


  first_use_emr <- split(loading_emr, loading_emr$new_med_name)

  for (i in 1:length(first_use_emr)) {
    if ("Infliximab" %in% names(first_use_emr[i])) {
      first_use_emr[[i]] <- first_use_emr[[i]] %>%
        distinct(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, first_date, MED_START_DATE, MED_END_DATE, weeks_between_med, dose, route) %>%
        mutate(weeks_between_med = as.numeric(weeks_between_med)) %>%
        mutate(count = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
        pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, first_date), names_from = c(count, count, count), values_from = c(weeks_between_med, dose, route)) %>%
        ungroup() %>%
        dplyr::bind_rows(dplyr::tibble(weeks_between_med_3 = numeric())) %>%
        mutate(first_use_emr = if_else(weeks_between_med_2 >= 2 & weeks_between_med_2 < 3 &
                                         weeks_between_med_3 >= 3.5 & weeks_between_med_3 < 5.5, "Loading Dose", "Not Loading Dose"))
    } else if ("Adalimumab" %in% names(first_use_emr[i])) {
      first_use_emr[[i]] <- first_use_emr[[i]] %>%
        distinct(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, first_date, MED_START_DATE, MED_END_DATE, weeks_between_med, dose, route) %>%
        mutate(count = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
        mutate(weeks_between_med = as.numeric(weeks_between_med)) %>%
        pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, first_date), names_from = c(count, count, count), values_from = c(weeks_between_med, dose, route)) %>%
        ungroup() %>%
        dplyr::bind_rows(dplyr::tibble(weeks_between_med_3 = numeric())) %>%
        mutate(first_use_emr = ifelse(dose_1 == "160" & dose_2 == "80", "Loading Dose", "Not Loading Dose"))
    } else if ("Certolizumab Pegol" %in% names(first_use_emr[i])) {
      first_use_emr[[i]] <- first_use_emr[[i]] %>%
        distinct(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, first_date, MED_START_DATE, MED_END_DATE, weeks_between_med, dose, route) %>%
        mutate(count = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
        mutate(weeks_between_med = as.numeric(weeks_between_med)) %>%
        pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, first_date), names_from = c(count, count, count), values_from = c(weeks_between_med, dose, route)) %>%
        ungroup() %>%
        dplyr::bind_rows(dplyr::tibble(weeks_between_med_3 = numeric())) %>%
        mutate(first_use_emr = if_else(weeks_between_med_2 >= 2 & weeks_between_med_2 < 3 &
                                         weeks_between_med_3 >= 1.5 & weeks_between_med_3 < 3.5, "Loading Dose", "Not Loading Dose"))
    } else if ("Vedolizumab" %in% names(first_use_emr[i])) {
      first_use_emr[[i]] <- first_use_emr[[i]] %>%
        distinct(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, first_date, MED_START_DATE, MED_END_DATE, weeks_between_med, dose, route) %>%
        mutate(count = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
        mutate(weeks_between_med = as.numeric(weeks_between_med)) %>%
        pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, first_date), names_from = c(count, count, count), values_from = c(weeks_between_med, dose, route)) %>%
        ungroup() %>%
        dplyr::bind_rows(dplyr::tibble(weeks_between_med_3 = numeric())) %>%
        mutate(first_use_emr = if_else(weeks_between_med_2 >= 2 & weeks_between_med_2 < 3 &
                                         weeks_between_med_3 >= 3.5 & weeks_between_med_3 < 5.5, "Loading Dose", "Not Loading Dose"))
    } else if ("Ustekinumab" %in% names(first_use_emr[i])) {
      first_use_emr[[i]] <- first_use_emr[[i]] %>%
        distinct(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, first_date, MED_START_DATE, MED_END_DATE, weeks_between_med, dose, route) %>%
        mutate(count = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
        mutate(weeks_between_med = as.numeric(weeks_between_med)) %>%
        pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, first_date), names_from = c(count, count, count), values_from = c(weeks_between_med, dose, route)) %>%
        ungroup() %>%
        dplyr::bind_rows(dplyr::tibble(weeks_between_med_3 = numeric())) %>%
        mutate(first_use_emr = ifelse(toupper(route_1) == "INTRAVENOUS", "Loading Dose", "Not Loading Dose"))
    }
  }

  first_use_emr <- bind_rows(first_use_emr) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, first_date, first_use_emr) %>%
    filter(first_use_emr == "Loading Dose") %>%
    rename(
      MEDICATION = new_med_name,
      MED_START_DATE_EMR = first_date
    ) %>%
    mutate(LOADING_DOSE_EMR = 1) %>%
    select(-first_use_emr)
}
