



#' sparc_med_filter
#'
#' Logic to filter prescriptions table for medications of interest from the EMR and eCRF data sources.
#'
#'
#'
#' @param prescriptions A dataframe with prescriptions data usually generated using load_data.
#' @param observations a dataframe with observation data usually generated using load_data.
#' @param demographics A dataframe with demographic data usually generated using load_data.
#' @param encounter a dataframe with encounter data usually generated using load_data.
#' @param med_groups A character string that filters the med_grp dataframe. Options are Aminosalicylates, Antibiotics, Antidiarrheals, Biologic,  Corticosteroids, Immunomodulators, Other, Probiotic. Default does not use any filtering.
#'
#' @return table with medications of interest from eCRF and EMR data
#' @details Medication data is pulled forward if a patient answers "No" to the question "Are you currently taking any medication for your IBD?" on the quarterly survey.
#' The med_grp dataframe has the medication names to search for in the MEDICATION, OTHER_MEDICATION and SRC_DRUG_CODE_CONCEPT_NAME columns.
#'@export
sparc_med_filter <- function(prescriptions, observations, demographics, encounter, med_groups = c("Aminosalicylates", "Antibiotics", "Antidiarrheals", "Biologic", "Corticosteroids", "Immunomodulators", "Other", "Probiotic")) {

  # CONSENT INFORMATION ----

  consent <- extract_consent(demographics, "SPARC")


  # MEDICATION INFORMATION FROM OBSERVATION TABLE ----


  obs_med <- observations %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    left_join(encounter, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "DEIDENTIFIED_PATIENT_ID", "DATA_SOURCE", "VISIT_ENCOUNTER_ID", "ADMISSION_STATUS")) %>%
    filter(TYPE_OF_ENCOUNTER == "IBD Medication Survey")

  # No Medication at Enrollment (First Survey with Are you Currently Taking Any Medication = NO) Does not include antibiotics/probiotics ----

  med_enroll <- obs_med %>%
    filter(OBS_TEST_CONCEPT_NAME == "Are you currently taking any medication for your IBD?") %>%
    left_join(consent, by = "DEIDENTIFIED_MASTER_PATIENT_ID") %>%
    mutate(diff = OBS_TEST_RESULT_DATE - DATE_OF_CONSENT) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    filter(diff <= 30) %>%
    slice(which.min(diff)) %>%
    filter(DESCRIPTIVE_SYMP_TEST_RESULTS == "No") %>%
    left_join(prescriptions, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "DEIDENTIFIED_PATIENT_ID", "DATA_SOURCE", "VISIT_ENCOUNTER_ID", "ADMISSION_TYPE", "SOURCE_OF_ADMISSION")) %>%
    left_join(med_grp, "MEDICATION_NAME") %>%
    filter(is.na(new_med_name) | med_type %in% c("Antibiotics", "Probiotic")) %>%
    mutate(NO_CURRENT_IBD_MEDICATION_AT_ENROLLMENT = 1) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, NO_CURRENT_IBD_MEDICATION_AT_ENROLLMENT)


  # No Changes After 90 Days. If No Change Can Pull forward Past Medication Entered. ----
  # create survey order

  ecrf_med_encounters <- encounter %>%
    filter(TYPE_OF_ENCOUNTER == "IBD Medication Survey") %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, dmy(VISIT_ENCOUNTER_START_DATE)) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(medication_survey_n = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
    mutate(helper = paste0(DEIDENTIFIED_MASTER_PATIENT_ID, medication_survey_n))

  # find survey to pull forward
  med_changes <- obs_med %>%
    filter(OBS_TEST_CONCEPT_NAME %in% c("In the last 90 days, have you had any changes in your Medication(s)?")) %>%
    left_join(consent, by = "DEIDENTIFIED_MASTER_PATIENT_ID") %>%
    # filter(DESCRIPTIVE_SYMP_TEST_RESULTS == "No") %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, DESCRIPTIVE_SYMP_TEST_RESULTS)

  ecrf_med_encounters <- ecrf_med_encounters %>%
    full_join(med_changes, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "VISIT_ENCOUNTER_ID")) %>%
    mutate(nochange = case_when(
      DESCRIPTIVE_SYMP_TEST_RESULTS == "No" & medication_survey_n != 1 ~ "No Change",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "Yes" & medication_survey_n != 1 ~ "Yes Change",
      medication_survey_n == 1 ~ "First Survey",
      is.na(DESCRIPTIVE_SYMP_TEST_RESULTS) & medication_survey_n != 1 ~ "No Answer"
    ))

  ecrf_med_encounters <- split(ecrf_med_encounters, ecrf_med_encounters$DEIDENTIFIED_MASTER_PATIENT_ID)

  for (i in 1:length(ecrf_med_encounters)) {
    df <- data.frame(ecrf_med_encounters[[i]])
    x <- which(df$nochange == "Yes Change" | df$nochange == "First Survey" | df$nochange == "No Answer")
    z <- which(df$nochange == "No Change")
    if (length(z) != 0 & length(x) != 0) {
      df$med_survey_to_pull_forward[df$nochange == "No Change"] <- df$medication_survey_n[x[sapply(z, function(j) findInterval(j, x))]]
    } else {
      df$med_survey_to_pull_forward[df$nochange == "No Change"] <- as.numeric(NA)
    }
    ecrf_med_encounters[[i]] <- df
  }

  ecrf_med_encounters <- bind_rows(ecrf_med_encounters) %>%
    mutate(med_survey_to_pull_forward = ifelse(DESCRIPTIVE_SYMP_TEST_RESULTS == "No" & medication_survey_n == 1, as.numeric(NA), med_survey_to_pull_forward))

  # If first survey has no changes at 90 days then no medication at enrollment

  med_enroll_2 <- ecrf_med_encounters %>%
    filter(DESCRIPTIVE_SYMP_TEST_RESULTS == "No") %>%
    filter(medication_survey_n == 1) %>%
    filter(!(VISIT_ENCOUNTER_ID %in% prescriptions$VISIT_ENCOUNTER_ID)) %>%
    mutate(`NO_CURRENT_IBD_MEDICATION_AT_ENROLLMENT2` = 1) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, `NO_CURRENT_IBD_MEDICATION_AT_ENROLLMENT2`)

  no_med_enroll <- left_join(med_enroll, med_enroll_2) %>%
    mutate(NO_CURRENT_IBD_MEDICATION_AT_ENROLLMENT = ifelse(is.na(NO_CURRENT_IBD_MEDICATION_AT_ENROLLMENT), NO_CURRENT_IBD_MEDICATION_AT_ENROLLMENT2, NO_CURRENT_IBD_MEDICATION_AT_ENROLLMENT)) %>%
    select(-NO_CURRENT_IBD_MEDICATION_AT_ENROLLMENT2)

  # Pull Medication forward in ECRF
  pull_forward_encounter <- ecrf_med_encounters %>%
    drop_na(med_survey_to_pull_forward) %>%
    mutate(helper = paste0(DEIDENTIFIED_MASTER_PATIENT_ID, med_survey_to_pull_forward)) %>%
    mutate(new_encounter_id = VISIT_ENCOUNTER_ID) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, helper, new_encounter_id)

  pull_forward_prescriptions <- ecrf_med_encounters %>%
    mutate(helper = paste0(DEIDENTIFIED_MASTER_PATIENT_ID, medication_survey_n)) %>%
    left_join(pull_forward_encounter, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "helper")) %>%
    drop_na(new_encounter_id) %>%
    left_join(prescriptions, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "DEIDENTIFIED_PATIENT_ID", "DATA_SOURCE", "VISIT_ENCOUNTER_ID", "ADMISSION_TYPE", "SOURCE_OF_ADMISSION")) %>%
    mutate(VISIT_ENCOUNTER_ID = new_encounter_id)

  pull_forward_prescriptions <- pull_forward_prescriptions %>%
    select(intersect(names(pull_forward_prescriptions), names(prescriptions)))
  # Find Medications of Interest in eCRF and EMR Data ----

  meds <- med_grp %>% filter(med_type %in% med_groups)

  scripts <- prescriptions %>%
    bind_rows(pull_forward_prescriptions) %>%
    filter(DATA_SOURCE == "EMR" | DATA_SOURCE == "ECRF_SPARC") %>%
    mutate(
      med1 = ifelse(MEDICATION_NAME == "Other (IBD Medication)", OTHER_MEDICATION, MEDICATION_NAME),
      med2 = paste0(MEDICATION_NAME, "; ", OTHER_MEDICATION),
      med3 = ifelse(is.na(MEDICATION_NAME), SRC_DRUG_CODE_CONCEPT_NAME, MEDICATION_NAME)
    )

  m <- meds$MEDICATION_NAME

  med1 <- NULL

  for (i in 1:length(m)) {
    k <- scripts %>%
      filter(grepl(m[i], med1, ignore.case = T)) %>%
      mutate(drug = paste0(m[i]))
    med1[[i]] <- k
  }

  names(med1) <- m

  med1 <- med1[sapply(med1, nrow) > 0]

  med1 <- bind_rows(med1)


  med2 <- NULL

  for (i in 1:length(m)) {
    k <- scripts %>%
      filter(grepl(m[i], med2, ignore.case = T)) %>%
      mutate(drug = paste0(m[i]))
    med2[[i]] <- k
  }

  names(med2) <- m

  med2 <- med2[sapply(med2, nrow) > 0]

  med2 <- bind_rows(med2)

  med3 <- NULL

  for (i in 1:length(m)) {
    k <- scripts %>%
      filter(grepl(m[i], med3, ignore.case = T)) %>%
      mutate(drug = paste0(m[i]))
    med3[[i]] <- k
  }

  names(med3) <- m

  med3 <- med3[sapply(med3, nrow) > 0]

  med3 <- bind_rows(med3)


  rm(k)


  medication <- bind_rows(med1, med2, med3) %>%
    left_join(meds, by = c("drug" = "MEDICATION_NAME")) %>%
    drop_na(new_med_name) %>%
    select(-drug) %>%
    distinct() %>%
    full_join(no_med_enroll)
}
