
#' sparc_med_starts
#'
#' Extract medication first instance of medication start dates from SPARC patient reported and electronic medical record data. This
#' is only for long term medications including Biologics, Aminosalicylates, and
#' Immunomodulators.
#'
#'
#'
#' @param prescriptions A dataframe with prescriptions data usually generated using load_data.
#' @param demographics a dataframe with demographic data usually generated using load_data.
#' @param observations a dataframe with observations data usually generated using load_data.
#' @param encounter A dataframe with encounter data usually generated using load_data.
#' @param med_groups A list of medication groups of interest. For details, refer to \code{\link{sparc_med_filter}}.
#' @param export if excel spreadsheet should be exported. FALSE is default.
#'
#' @return A dataframe with the first medication start date for each drug.
#'
#' @details Medication start and stop dates are chosen independtly from both eCRF and EMR sources. Medications with a start or stop date before 1980 are dropped.
#' For EMR data, if a medication start date is missing, the visit encounter start date is used. These records are flagged in the column VISIT_ENCOUNTER_MED_START.
#'
#' If a patient has medication information for the same drug from eCRF and EMR, the eCRF data is preferred and used to generate MED_START_DATE and MED_END_DATE. If only EMR data is available for that medication, then EMR data is used.
#' Any overlap between medications is reported along with the number of days the medications overlap.
#' If no end date is given for a prescription, the duration of the overlap is calculated assuming an ongoing prescription. The effective end date
#' is set using a database wide cutoff based on the the date of the latest encounter any patient had (as returned by \code{\link{extract_latest}}).
#' Patient level cutoffs are not used because the last recorded encounter of a specific patient may precede the latest available EMR by years.
#'
#' The following columns are convenience flags and indices to facilitate easy filtering of the data:
#'
#' \describe{
#'   \item{MEDICATION_NUMBER}{counts the number of different medications in a patients' journey. Only medications in the selected med_groups are considered.
#' The medication with with earliest start date will have MEDICATION_NUMBER = 1.}
#'   \item{BIONAIVE}{is 1 if a patient has no prior reported biologics.}
#'   \item{FIRST_BIOLOGIC}{is 1 if a medication record is the first biologic a patient receives. If a record is the first biologic a patient receives, FIRST_BIOLOGIC_NUMBER is equal to the MEDICATION_NUMBER.}
#'   \item{STARTED_AFTER_ENROLLMENT}{is 1 if the medication start date is after the date of consent.}
#'   \item{BIOLOGIC}{is 1 if the record is for a biologic}
#' }
#'
#' @export
#'
sparc_med_starts <- function(prescriptions, demographics, observations, encounter,med_groups = c("Biologic", "Aminosalicylates", "Immunomodulators"), export = FALSE) {

  # Get medications of interest

  medication <- sparc_med_filter(prescriptions, observations, demographics, encounter, med_groups )


  # ECRF DATA ----

  # Find Start Date in eCRF Data ----

  start_ecrf <- medication %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    mutate(
      MED_START_DATE = dmy(MED_START_DATE),
      MED_END_DATE = dmy(MED_END_DATE)
    ) %>%
    mutate(
      MED_START_DATE = if_else(year(MED_START_DATE) > 1980, MED_START_DATE, as.Date(NA, format = "%d-%m-%y")),
      MED_END_DATE = if_else(year(MED_END_DATE) > 1980, MED_END_DATE, as.Date(NA, format = "%d-%m-%y"))
    ) %>%
    drop_na(MED_START_DATE) %>%
    pivot_longer(cols = c(MED_START_DATE, MED_END_DATE), names_to = "type", values_to = "date") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, DATA_SOURCE) %>%
    slice(which.min(date)) %>%
    pivot_wider(names_from = type, values_from = date) %>%
    ungroup() %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DATA_SOURCE, new_med_name, MED_START_DATE) %>%
    rename(med = new_med_name)

  # Find End Date in eCRF Data ----
  end_ecrf <- medication %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    mutate(
      MED_START_DATE = dmy(MED_START_DATE),
      MED_END_DATE = dmy(MED_END_DATE)
    ) %>%
    mutate(
      MED_START_DATE = if_else(year(MED_START_DATE) > 1980, MED_START_DATE, as.Date(NA, format = "%d-%m-%y")),
      MED_END_DATE = if_else(year(MED_END_DATE) > 1980, MED_END_DATE, as.Date(NA, format = "%d-%m-%y"))
    ) %>%
    pivot_longer(cols = c(MED_START_DATE, MED_END_DATE), names_to = "type", values_to = "date") %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, DATA_SOURCE, match(type, c("MED_END_DATE", "MED_START_DATE"))) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, DATA_SOURCE) %>%
    slice(which.max(date)) %>%
    pivot_wider(names_from = type, values_from = date) %>%
    ungroup() %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DATA_SOURCE, new_med_name, MED_START_DATE, MED_END_DATE, CURRENT_MEDICATION) %>%
    rename(MED_START_DATE_2 = MED_START_DATE) %>%
    rename(med = new_med_name)

  # Combine start and stop for eCRF ----
  med_ecrf <- full_join(start_ecrf, end_ecrf, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "DATA_SOURCE", "med")) %>%
    mutate(CURRENT_MEDICATION = case_when(
      is.na(MED_END_DATE) ~ "YES",
      !is.na(MED_END_DATE) ~ "NO",
      TRUE ~ CURRENT_MEDICATION
    )) %>%
    select(-MED_START_DATE_2) %>%
    mutate(DATA_SOURCE = gsub("_SPARC", "", DATA_SOURCE)) %>%
    pivot_wider(
      id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, med),
      names_from = DATA_SOURCE,
      values_from = c(MED_START_DATE, MED_END_DATE, CURRENT_MEDICATION)
    ) %>%
    rename(MEDICATION = med)

  # Number of Dose Changes in eCRF ----

  dose_ecrf <- medication %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    mutate(
      MED_START_DATE = dmy(MED_START_DATE),
      MED_END_DATE = dmy(MED_END_DATE)
    ) %>%
    mutate(
      MED_START_DATE = if_else(year(MED_START_DATE) > 1980, MED_START_DATE, as.Date(NA, format = "%d-%m-%y")),
      MED_END_DATE = if_else(year(MED_END_DATE) > 1980, MED_END_DATE, as.Date(NA, format = "%d-%m-%y"))
    ) %>%
    mutate(DOSE_OF_MEDICATION = readr::parse_number(DOSE_OF_MEDICATION)) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DATA_SOURCE, new_med_name, DOSE_OF_MEDICATION, MED_START_DATE, MED_END_DATE) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name,MED_START_DATE) %>%
    mutate(c = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
    pivot_wider(
      id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, DATA_SOURCE, new_med_name),
      names_from = c,
      values_from = c(DOSE_OF_MEDICATION),
      names_prefix = "DOSE_"
    ) %>%
    drop_na(DOSE_2) %>%
    rowwise() %>%
    mutate(
      NEW_DOSE_1 = ifelse(is.na(DOSE_1), DOSE_2, DOSE_1),
      DOSE_2 = ifelse(is.na(DOSE_1), as.numeric(NA), DOSE_2)
    ) %>%
    select(-DOSE_1) %>%
    rename(DOSE_1 = NEW_DOSE_1) %>%
    mutate(NUMBER_OF_DOSE_CHANGES = sum(!is.na(c_across(DOSE_2:DOSE_6)), na.rm = T)) %>%
    ungroup() %>%
    mutate(DATA_SOURCE = gsub("_SPARC", "", DATA_SOURCE)) %>%
    pivot_wider(
      id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name),
      names_from = DATA_SOURCE,
      values_from = c(NUMBER_OF_DOSE_CHANGES),
      names_prefix = "NUMBER_OF_DOSE_CHANGES_"
    ) %>%
    rename(MEDICATION = new_med_name)

  # Frequency Change in eCRF ----

  freq_ecrf <- medication %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    mutate(
      MED_START_DATE = dmy(MED_START_DATE),
      MED_END_DATE = dmy(MED_END_DATE)
    ) %>%
    mutate(
      MED_START_DATE = if_else(year(MED_START_DATE) > 1980, MED_START_DATE, as.Date(NA, format = "%d-%m-%y")),
      MED_END_DATE = if_else(year(MED_END_DATE) > 1980, MED_END_DATE, as.Date(NA, format = "%d-%m-%y"))
    ) %>%
    mutate(
      MEDICATION_FREQUENCE = toupper(MEDICATION_FREQUENCE),
      FREQUENCY_IN_DAYS = toupper(FREQUENCY_IN_DAYS)
    ) %>%
    mutate(MEDICATION_FREQUENCE = paste0(MEDICATION_FREQUENCE, "; ", FREQUENCY_IN_DAYS)) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DATA_SOURCE, new_med_name, MEDICATION_FREQUENCE) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name) %>%
    mutate(c = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
    pivot_wider(
      id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, DATA_SOURCE, new_med_name),
      names_from = c,
      values_from = c(MEDICATION_FREQUENCE),
      names_prefix = "FREQ_"
    ) %>%
    drop_na(FREQ_2) %>%
    rowwise() %>%
    mutate(
      NEW_FREQ_1 = ifelse(is.na(FREQ_1), FREQ_2, FREQ_1),
      FREQ_2 = ifelse(is.na(FREQ_1), NA, FREQ_2)
    ) %>%
    select(-FREQ_1) %>%
    rename(FREQ_1 = NEW_FREQ_1) %>%
    mutate(NUMBER_OF_FREQ_CHANGES = sum(!is.na(c_across(FREQ_2:FREQ_5)), na.rm = T)) %>%
    ungroup() %>%
    mutate(DATA_SOURCE = gsub("_SPARC", "", DATA_SOURCE)) %>%
    pivot_wider(
      id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name),
      names_from = DATA_SOURCE,
      values_from = c(NUMBER_OF_FREQ_CHANGES),
      names_prefix = "NUMBER_OF_FREQ_CHANGES_"
    ) %>%
    rename(MEDICATION = new_med_name)


  # Reason Stopped in eCRF ----

  stop_ecrf <- medication %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    mutate(
      MED_START_DATE = dmy(MED_START_DATE),
      MED_END_DATE = dmy(MED_END_DATE)
    ) %>%
    mutate(
      MED_START_DATE = if_else(year(MED_START_DATE) > 1980, MED_START_DATE, as.Date(NA, format = "%d-%m-%y")),
      MED_END_DATE = if_else(year(MED_END_DATE) > 1980, MED_END_DATE, as.Date(NA, format = "%d-%m-%y"))
    ) %>%
    mutate(REASON_STOPPED = toupper(REASON_STOPPED)) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DATA_SOURCE, new_med_name, REASON_STOPPED) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name) %>%
    mutate(c = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
    pivot_wider(
      id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, DATA_SOURCE, new_med_name),
      names_from = c,
      values_from = c(REASON_STOPPED),
      names_prefix = "STOP_"
    ) %>%
    rowwise() %>%
    unite(col = "REASON_STOPPED_ECRF", starts_with("STOP"), na.rm = T, sep = "; ") %>%
    rename(MEDICATION = new_med_name) %>%
    mutate(across(-"DEIDENTIFIED_MASTER_PATIENT_ID", ~ ifelse(. == "", NA, as.character(.))))


  # Add dose, frequency and reason stopped to med_ecrf ----

  med_ecrf <- med_ecrf %>%
    left_join(dose_ecrf, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "MEDICATION")) %>%
    left_join(freq_ecrf, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "MEDICATION")) %>%
    left_join(stop_ecrf, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "MEDICATION")) %>%
    select(-DATA_SOURCE)



  # EMR Data ----


  # Find Start Date in EMR Data ----

  start_emr <- medication %>%
    left_join(encounter, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "DEIDENTIFIED_PATIENT_ID", "DATA_SOURCE", "VISIT_ENCOUNTER_ID", "ADMISSION_TYPE", "SOURCE_OF_ADMISSION")) %>%
    mutate(VISIT_ENCOUNTER_MED_START = if_else(is.na(MED_START_DATE) & DATA_SOURCE == "EMR", 1, 0),
           MED_START_DATE = if_else(is.na(MED_START_DATE) & DATA_SOURCE == "EMR", VISIT_ENCOUNTER_START_DATE, MED_START_DATE)) %>%
    filter(DATA_SOURCE == "EMR") %>%
    mutate(
      MED_START_DATE = dmy(MED_START_DATE),
      MED_END_DATE = dmy(MED_END_DATE)
    ) %>%
    mutate(drop = case_when(
      (!is.na(MED_END_DATE) & !is.na(MED_START_DATE)) & MED_END_DATE < MED_START_DATE ~ 1,
      TRUE ~ 0
    )) %>%
    filter(drop == 0) %>%
    mutate(
      MED_START_DATE = if_else(year(MED_START_DATE) > 1980, MED_START_DATE, as.Date(NA, format = "%d-%m-%y")),
      MED_END_DATE = if_else(year(MED_END_DATE) > 1980, MED_END_DATE, as.Date(NA, format = "%d-%m-%y"))
    ) %>%
    pivot_longer(cols = c(MED_START_DATE, MED_END_DATE), names_to = "type", values_to = "date") %>%
    drop_na(date) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, DATA_SOURCE, match(type, c("MED_START_DATE", "MED_END_DATE")), date) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, DATA_SOURCE) %>%
    slice(which.min(date)) %>%
    pivot_wider(names_from = type, values_from = date) %>%
    ungroup() %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DATA_SOURCE, new_med_name, MED_START_DATE,VISIT_ENCOUNTER_MED_START) %>%
    rename(med = new_med_name)

  # Find End Date in EMR Data ----
  end_emr <- medication %>%
    left_join(encounter, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "DEIDENTIFIED_PATIENT_ID", "DATA_SOURCE", "VISIT_ENCOUNTER_ID", "ADMISSION_TYPE", "SOURCE_OF_ADMISSION")) %>%
    mutate(MED_END_DATE = if_else(is.na(MED_END_DATE) & DATA_SOURCE == "EMR", MED_DISCONT_START_DATE, MED_END_DATE)) %>%
    filter(DATA_SOURCE == "EMR") %>%
    mutate(
      MED_START_DATE = dmy(MED_START_DATE),
      MED_END_DATE = dmy(MED_END_DATE)
    ) %>%
    mutate(drop = case_when(
      (!is.na(MED_END_DATE) & !is.na(MED_START_DATE)) & MED_END_DATE < MED_START_DATE ~ 1,
      TRUE ~ 0
    )) %>%
    filter(drop == 0) %>%
    mutate(
      MED_START_DATE = if_else(year(MED_START_DATE) > 1980, MED_START_DATE, as.Date(NA, format = "%d-%m-%y")),
      MED_END_DATE = if_else(year(MED_END_DATE) > 1980, MED_END_DATE, as.Date(NA, format = "%d-%m-%y"))
    ) %>%
    pivot_longer(cols = c(MED_START_DATE, MED_END_DATE), names_to = "type", values_to = "date") %>%
    drop_na(date) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, DATA_SOURCE, match(type, c("MED_END_DATE", "MED_START_DATE"))) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, DATA_SOURCE) %>%
    slice(which.max(date)) %>%
    pivot_wider(names_from = type, values_from = date) %>%
    ungroup() %>%
    drop_na(MED_END_DATE) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DATA_SOURCE, new_med_name, MED_END_DATE) %>%
    rename(med = new_med_name)

  # Combine start and stop for EMR ----
  med_emr <- full_join(start_emr, end_emr, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "DATA_SOURCE", "med")) %>%
     pivot_wider(
      id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, med,VISIT_ENCOUNTER_MED_START),
      names_from = DATA_SOURCE,
      values_from = c(MED_START_DATE, MED_END_DATE)
    ) %>%
    rename(MEDICATION = med)

  # Loading Dose Pattern in EMR ----
  # does prescription pattern in EMR follow the loading dose of the biologic


  loading_emr <- medication %>%
    filter(DATA_SOURCE == "EMR") %>%
    filter(!is.na(MED_START_DATE) | !is.na(MED_END_DATE)) %>%
    left_join(encounter) %>%
    mutate(
      MED_START_DATE = dmy(MED_START_DATE),
      MED_END_DATE = dmy(MED_END_DATE),
      VISIT_ENCOUNTER_START_DATE = dmy(VISIT_ENCOUNTER_START_DATE)
    ) %>%
    filter(year(MED_START_DATE) > 1900) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, MED_START_DATE) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name) %>%
    select(
      DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_START_DATE, new_med_name, ROUTE_OF_MEDICATION, MEDICATION_DOMAIN, MED_START_DATE,
      MED_END_DATE, DOSE_OF_MEDICATION, CURRENT_MEDICATION, OTHER_MEDICATION, UNIT_OF_MEASURE_FOR_MEDICATION, MEDICATION_FREQUENCE,
      MEDICATION_ADMINISTRATED_CODE, MEDICATION_ADMINISTRATED, FREQUENCY_IN_DAYS, REASON_STOPPED, SUMMARY,
      MED_DISCONT_START_DATE, MEDICATION_STRENGTH, MED_STRENGTH_UNIT_OF_MEASURE, MEDICATION_QUANTITY, MED_QUANTITY_UOM, MED_FORM,
      MEDICATION_TREATMENT_COURSE, FREQUENCE_UNIT_OF_MEASURE, MEDICATION_ADMIN_DURATION, MED_ADMIN_DURATION_UOM,
      GENERIC_MEDICINE_FLAG, SUBSTITUTE_MED_INDICATION_FLAG, PLACE_OF_SERVICE, MEDICATION_REFILLS
    ) %>%
    distinct() %>%
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
        mutate(count = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
        pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, first_date), names_from = c(count, count, count), values_from = c(weeks_between_med, dose, route)) %>%
        ungroup() %>%
        mutate(first_use_emr = if_else(weeks_between_med_2 >= 2 & weeks_between_med_2 < 3 &
          weeks_between_med_3 >= 3.5 & weeks_between_med_3 < 5.5, "Loading Dose", "Not Loading Dose"))
    } else if ("Adalimumab" %in% names(first_use_emr[i])) {
      first_use_emr[[i]] <- first_use_emr[[i]] %>%
        distinct(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, first_date, MED_START_DATE, MED_END_DATE, weeks_between_med, dose, route) %>%
        mutate(count = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
        pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, first_date), names_from = c(count, count, count), values_from = c(weeks_between_med, dose, route)) %>%
        ungroup() %>%
        mutate(first_use_emr = ifelse(dose_1 == "160" & dose_2 == "80", "Loading Dose", "Not Loading Dose"))
    } else if ("Certolizumab Pegol" %in% names(first_use_emr[i])) {
      first_use_emr[[i]] <- first_use_emr[[i]] %>%
        distinct(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, first_date, MED_START_DATE, MED_END_DATE, weeks_between_med, dose, route) %>%
        mutate(count = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
        pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, first_date), names_from = c(count, count, count), values_from = c(weeks_between_med, dose, route)) %>%
        ungroup() %>%
        mutate(first_use_emr = if_else(weeks_between_med_2 >= 2 & weeks_between_med_2 < 3 &
          weeks_between_med_3 >= 1.5 & weeks_between_med_3 < 3.5, "Loading Dose", "Not Loading Dose"))
    } else if ("Vedolizumab" %in% names(first_use_emr[i])) {
      first_use_emr[[i]] <- first_use_emr[[i]] %>%
        distinct(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, first_date, MED_START_DATE, MED_END_DATE, weeks_between_med, dose, route) %>%
        mutate(count = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
        pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, first_date), names_from = c(count, count, count), values_from = c(weeks_between_med, dose, route)) %>%
        ungroup() %>%
        mutate(first_use_emr = if_else(weeks_between_med_2 >= 2 & weeks_between_med_2 < 3 &
          weeks_between_med_3 >= 3.5 & weeks_between_med_3 < 5.5, "Loading Dose", "Not Loading Dose"))
    } else if ("Ustekinumab" %in% names(first_use_emr[i])) {
      first_use_emr[[i]] <- first_use_emr[[i]] %>%
        distinct(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, first_date, MED_START_DATE, MED_END_DATE, weeks_between_med, dose, route) %>%
        mutate(count = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
        pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, first_date), names_from = c(count, count, count), values_from = c(weeks_between_med, dose, route)) %>%
        ungroup() %>%
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

  med_emr <- med_emr %>%
    left_join(first_use_emr, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "MEDICATION", "MED_START_DATE_EMR"))

  # Number of prescriptions ----

  pres_emr <- medication %>%
    filter(DATA_SOURCE == "EMR") %>%
    mutate(
      MED_START_DATE = dmy(MED_START_DATE),
      MED_END_DATE = dmy(MED_END_DATE)
    ) %>%
    mutate(drop = case_when(
      (!is.na(MED_END_DATE) & !is.na(MED_START_DATE)) & MED_END_DATE < MED_START_DATE ~ 1,
      TRUE ~ 0
    )) %>%
    filter(drop == 0) %>%
    mutate(
      MED_START_DATE = if_else(year(MED_START_DATE) > 1980, MED_START_DATE, as.Date(NA, format = "%d-%m-%y")),
      MED_END_DATE = if_else(year(MED_END_DATE) > 1980, MED_END_DATE, as.Date(NA, format = "%d-%m-%y"))
    ) %>%
    rename(MEDICATION = new_med_name) %>%
    distinct() %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION) %>%
    summarise(PRESCRIPTION_NUMBER_EMR = n())


  med_emr <- med_emr %>%
    left_join(pres_emr, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "MEDICATION"))

  # Number of refils at most recent prescription ----

  refills_emr <- medication %>%
    filter(DATA_SOURCE == "EMR") %>%
    mutate(
      MED_START_DATE = dmy(MED_START_DATE),
      MED_END_DATE = dmy(MED_END_DATE)
    ) %>%
    mutate(drop = case_when(
      (!is.na(MED_END_DATE) & !is.na(MED_START_DATE)) & MED_END_DATE < MED_START_DATE ~ 1,
      TRUE ~ 0
    )) %>%
    filter(drop == 0) %>%
    mutate(
      MED_START_DATE = if_else(year(MED_START_DATE) > 1980, MED_START_DATE, as.Date(NA, format = "%d-%m-%y")),
      MED_END_DATE = if_else(year(MED_END_DATE) > 1980, MED_END_DATE, as.Date(NA, format = "%d-%m-%y"))
    ) %>%
    pivot_longer(cols = c(MED_START_DATE, MED_END_DATE), names_to = "type", values_to = "date") %>%
    drop_na(date) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, DATA_SOURCE, match(type, c("MED_END_DATE", "MED_START_DATE"))) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, DATA_SOURCE) %>%
    slice(which.max(date)) %>%
    pivot_wider(names_from = type, values_from = date) %>%
    ungroup() %>%
    drop_na(MED_END_DATE) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, MED_END_DATE, MEDICATION_REFILLS) %>%
    rename(
      MEDICATION = new_med_name,
      MED_END_DATE_EMR = MED_END_DATE,
      MEDICATION_REFILLS_EMR = MEDICATION_REFILLS
    )


  med_emr <- med_emr %>%
    left_join(refills_emr, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "MEDICATION", "MED_END_DATE_EMR"))

  # Combine eCRF and EMR data ----

  # Use start date from eCRF otherwise use EMR data

  med <- full_join(med_ecrf, med_emr, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "MEDICATION")) %>%
    mutate(ECRF_DATA = case_when(!is.na(MED_START_DATE_ECRF) | !is.na(MED_END_DATE_ECRF) ~ 1, TRUE ~ 0)) %>%
    mutate(
      MED_START_DATE = MED_START_DATE_ECRF,
      MED_END_DATE = MED_END_DATE_ECRF
    ) %>%
    mutate(
      MED_START_DATE = case_when(
        ECRF_DATA == 0 & is.na(MED_START_DATE) & is.na(MED_END_DATE) ~ MED_START_DATE_EMR,
        TRUE ~ MED_START_DATE
      ),
      MED_END_DATE = case_when(
        ECRF_DATA == 0 & is.na(MED_END_DATE)  ~ MED_END_DATE_EMR,
        TRUE ~ MED_END_DATE
      )
    )


  # Create Mode of Action ----


  moa <- med_grp %>%
    filter(med_type %in% c("Biologic", "Aminosalicylates", "Immunomodulators")) %>%
    distinct(new_med_name, med_type) %>%
    rename(MEDICATION = new_med_name) %>%
    mutate(MOA = case_when(MEDICATION %in% c(
      "Adalimumab", "Certolizumab Pegol", "Golimumab", "Infliximab") ~ "antiTNF",
      MEDICATION %in% c("Tofacitinib","Upadacitinib") ~ "JAKi",
      MEDICATION %in% c("Vedolizumab", "Natalizumab") ~ "IRA",
      MEDICATION %in% c("Ustekinumab", "Risankizumab") ~ "ILA",
      MEDICATION %in% c("Ozanimod") ~ "S1P",
      TRUE ~ med_type)) %>%
    select(MEDICATION, MOA)

  med <- med %>% left_join(moa, by = "MEDICATION")




  # Create order of medications ----

  med_rank <- med %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION, MED_START_DATE, MED_END_DATE) %>%
    pivot_longer(cols = c(MED_START_DATE, MED_END_DATE), names_to = "type", values_to = "date") %>%
    drop_na(date) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, date) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(MEDICATION_NUMBER = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
    ungroup()

  med <- med %>%
    left_join(med_rank, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "MEDICATION"))


  # Flag if medications overlap START HERE ----
  last_encounter_date <- extract_latest(encounter) %>% pull("index_date") %>% max(na.rm = TRUE)

  overlap <- med %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(int = case_when(!is.na(MED_START_DATE) & !is.na(MED_END_DATE) ~ interval(MED_START_DATE, MED_END_DATE, tz = "UTC"),
                           !is.na(MED_START_DATE) & is.na(MED_END_DATE) ~  interval(MED_START_DATE ,last_encounter_date, tz = "UTC" ) ) )  %>%
    drop_na(int) %>%
    arrange(int_start(int), .by_group = TRUE) %>%
    # mutate(overlap = map_int(int, ~ ifelse(sum(int_overlaps(.x, int)) > 1, nrow(.x), NA))) %>%
    # ungroup()
    mutate(lag_overlap = case_when(int_overlaps(int, lag(int)) ~ lag(MOA)),
           lead_overlap = case_when(int_overlaps(int, lead(int)) ~ lead(MOA)),
           lag_overlap_days = day(as.period(intersect(int, lag(int)), "days")),
           lead_overlap_days = day(as.period(intersect(int, lead(int)), "days")),

    ) %>%
    mutate(OVERLAPPING_MOA = case_when(is.na(lag_overlap) ~ lead_overlap,
                                       is.na(lead_overlap) ~ lag_overlap,
                                       !is.na(lag_overlap) & !is.na(lead_overlap) ~ paste0(lag_overlap, "; ", lead_overlap)),
           OVERLAPPING_DAYS = if_else(is.na(lag_overlap_days), lead_overlap_days, lag_overlap_days)) %>%
    ungroup() %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION, MED_START_DATE, MED_END_DATE, OVERLAPPING_MOA, OVERLAPPING_DAYS)


  med <- med %>%
    left_join(overlap, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "MEDICATION", "MED_START_DATE", "MED_END_DATE"))





  # Flag if medication started after enrollment ----

  consent <- extract_consent(demographics, "SPARC")

  started_after_enrollment <- consent %>%
    left_join(med, by = "DEIDENTIFIED_MASTER_PATIENT_ID") %>%
    drop_na(MEDICATION) %>%
    mutate(STARTED_AFTER_ENROLLMENT = case_when(
      MED_START_DATE >= DATE_OF_CONSENT ~ 1,
      TRUE ~ 0
    )) %>%
    select(
      DEIDENTIFIED_MASTER_PATIENT_ID, DATE_OF_CONSENT, DATE_OF_CONSENT_WITHDRAWN, MEDICATION_NUMBER, MEDICATION, MED_START_DATE, MED_END_DATE,
      STARTED_AFTER_ENROLLMENT
    ) %>%
    ungroup()

  med <- med %>%
    left_join(started_after_enrollment, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "MEDICATION", "MED_START_DATE", "MED_END_DATE", "MEDICATION_NUMBER"))

  # Flag if no medication at enrollment

  no_med_enroll <- medication %>% distinct(DEIDENTIFIED_MASTER_PATIENT_ID, NO_CURRENT_IBD_MEDICATION_AT_ENROLLMENT)

  med <- med %>%
    left_join(no_med_enroll, by = "DEIDENTIFIED_MASTER_PATIENT_ID")

  # Flag if bionaive at medication start ----

  bionaive <- med %>%
    mutate(b = ifelse(MEDICATION %in% c(
      "Adalimumab", "Certolizumab Pegol", "Golimumab", "Infliximab",
      "Natalizumab", "Other Biologic",  "Ustekinumab", "Vedolizumab"), 1, 0)) %>%
    # filter(b == 1) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, b) %>%
    mutate(FIRST_BIOLOGIC = ifelse(b == 1 & MEDICATION_NUMBER == min(MEDICATION_NUMBER), 1, 0)) %>%
    mutate(FIRST_BIOLOGIC_NUMBER = ifelse(FIRST_BIOLOGIC == 1, MEDICATION_NUMBER, as.numeric(NA))) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    fill(FIRST_BIOLOGIC_NUMBER, .direction = "updown") %>%
    rowwise() %>%
    mutate(BIONAIVE = ifelse(MEDICATION_NUMBER < FIRST_BIOLOGIC_NUMBER, 1, 0)) %>%
    ungroup() %>%
    rename(BIOLOGIC = b)

  med <- med %>%
    left_join(bionaive)%>%
    rename(ECRF_PRESCRIPTION_DATA = ECRF_DATA)


  if (export == "TRUE") {
    write.xlsx(med, paste0("SPARC_medication_journey_", Sys.Date(), ".xlsx"), colnames = T)
  }

  return(med)
}
