

#' emr_loading_pattern
#'
#' Find if emr prescriptions follows known loading patterns for biologics
#'
#'
#'
#' @param prescriptions A dataframe with prescriptions data usually generated using load_data.
#' @param demographics a dataframe with demographic data usually generated using load_data.
#' @param observations a dataframe with observations data usually generated using load_data.
#' @param encounter a dataframe with encounter data usually generated using load_data.
#'
#' @return A dataframe with the first medication start date for each drug. Bionaive is 1 if a patient has no prior reported biologics (including JAK inhibitors and Tofacitinib). Started after enrollment is 1 if the medication start date is after the date of consent.
#'
#'
#'
emr_loading_pattern <- function(prescriptions, encounter) {


#EMR

  meds <- med_grp %>%
    filter(med_type %in% c("Biologic"))

  scripts <- prescriptions %>%
    bind_rows(pull_forward_prescriptions) %>%
    filter(DATA_SOURCE == "EMR") %>%
    mutate(
      med1 = ifelse(MEDICATION_NAME == "Other (IBD Medication)", "OTHER_MEDICATION", MEDICATION_NAME),
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
    distinct()


loading = medications %>%
  filter(!is.na(MED_START_DATE) | !is.na(MED_END_DATE)) %>%
  left_join(encounter) %>%
  mutate(MED_START_DATE = dmy(MED_START_DATE),
         MED_END_DATE = dmy(MED_END_DATE),
         VISIT_ENCOUNTER_START_DATE = dmy(VISIT_ENCOUNTER_START_DATE)) %>%
  filter(year(MED_START_DATE) > 1900) %>%
  arrange(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, MED_START_DATE) %>%
  group_by(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name) %>%
  select(DEIDENTIFIED_MASTER_PATIENT_ID,  VISIT_ENCOUNTER_START_DATE, new_med_name, ROUTE_OF_MEDICATION ,MEDICATION_DOMAIN ,MED_START_DATE,
         MED_END_DATE, DOSE_OF_MEDICATION,CURRENT_MEDICATION,OTHER_MEDICATION,UNIT_OF_MEASURE_FOR_MEDICATION,   MEDICATION_FREQUENCE   ,
         MEDICATION_ADMINISTRATED_CODE  ,MEDICATION_ADMINISTRATED   ,FREQUENCY_IN_DAYS  ,    REASON_STOPPED   ,SUMMARY   ,
         MED_DISCONT_START_DATE   ,MEDICATION_STRENGTH  ,  MED_STRENGTH_UNIT_OF_MEASURE  ,MEDICATION_QUANTITY,MED_QUANTITY_UOM,MED_FORM   ,
         MEDICATION_TREATMENT_COURSE  ,FREQUENCE_UNIT_OF_MEASURE   ,MEDICATION_ADMIN_DURATION   ,MED_ADMIN_DURATION_UOM  ,
         GENERIC_MEDICINE_FLAG   ,SUBSTITUTE_MED_INDICATION_FLAG ,PLACE_OF_SERVICE,MEDICATION_REFILLS) %>%
  distinct() %>%
  mutate(weeks_between_med = difftime(MED_START_DATE, lag(MED_START_DATE), units = "weeks")) %>%
  mutate(dose = ifelse(is.na(DOSE_OF_MEDICATION), MEDICATION_STRENGTH, DOSE_OF_MEDICATION)) %>%
  mutate(route = ifelse(is.na(ROUTE_OF_MEDICATION), MED_FORM, ROUTE_OF_MEDICATION)) %>%
  mutate(first_date = case_when(is.na(weeks_between_med) ~ MED_START_DATE)) %>%
  fill(first_date,.direction = c("down"))

first_use = split(loading, loading$med_grp)

for (i in 1:length(first_use)){
  if("Infliximab" %in% names(first_use[i])){
    first_use[[i]] = first_use[[i]] %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, med_grp,first_date,MED_START_DATE, MED_END_DATE, weeks_between_med, dose, route) %>%
      mutate(count = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
      pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID,  med_grp, first_date), names_from = c(count, count, count), values_from = c(weeks_between_med, dose, route)) %>%
      ungroup() %>%
      mutate(first_use = if_else(weeks_between_med_2 >=2 & weeks_between_med_2 < 3 &
                                   weeks_between_med_3 >= 3.5 & weeks_between_med_3 < 5.5, "Loading Dose", "Not Loading Dose"))
  } else if("Adalimumab" %in% names(first_use[i])){
    first_use[[i]] = first_use[[i]] %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, med_grp,first_date,MED_START_DATE, MED_END_DATE, weeks_between_med, dose, route) %>%
      mutate(count = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
      pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID,  med_grp, first_date), names_from = c(count, count, count), values_from = c(weeks_between_med, dose, route)) %>%
      ungroup() %>%
      mutate(first_use = ifelse(dose_1 == "160" & dose_2 == "80", "Loading Dose", "Not Loading Dose"))
  } else if("Certolizumab Pegol" %in% names(first_use[i])){
    first_use[[i]] = first_use[[i]] %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, med_grp,first_date,MED_START_DATE, MED_END_DATE, weeks_between_med, dose, route) %>%
      mutate(count = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
      pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID,  med_grp, first_date), names_from = c(count, count, count), values_from = c(weeks_between_med, dose, route)) %>%
      ungroup() %>%
      mutate(first_use = if_else(weeks_between_med_2 >=2 & weeks_between_med_2 < 3 &
                                   weeks_between_med_3 >= 1.5 & weeks_between_med_3 < 3.5, "Loading Dose", "Not Loading Dose"))
  } else if("Vedolizumab" %in% names(first_use[i])){
    first_use[[i]] = first_use[[i]] %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, med_grp,first_date,MED_START_DATE, MED_END_DATE, weeks_between_med, dose, route) %>%
      mutate(count = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
      pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID,  med_grp, first_date), names_from = c(count, count, count), values_from = c(weeks_between_med, dose, route)) %>%
      ungroup() %>%
      mutate(first_use = if_else(weeks_between_med_2 >=2 & weeks_between_med_2 < 3 &
                                   weeks_between_med_3 >= 3.5 & weeks_between_med_3 < 5.5, "Loading Dose", "Not Loading Dose"))
  } else if("Ustekinumab" %in% names(first_use[i])){
    first_use[[i]] = first_use[[i]] %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, med_grp,first_date,MED_START_DATE, MED_END_DATE, weeks_between_med, dose, route) %>%
      mutate(count = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
      pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID,  med_grp, first_date), names_from = c(count, count, count), values_from = c(weeks_between_med, dose, route)) %>%
      ungroup() %>%
      mutate(first_use = ifelse(toupper(route_1) == "INTRAVENOUS", "Loading Dose", "Not Loading Dose"))
  }
}

first_use_emr = bind_rows(first_use)  %>%
  distinct(DEIDENTIFIED_MASTER_PATIENT_ID, med_grp, first_date, first_use) %>%
  filter(first_use == "Loading Dose") %>%
  mutate(med_grp = paste0(med_grp, "_Loading_Date_EMR")) %>%
  pivot_wider(id_cols = DEIDENTIFIED_MASTER_PATIENT_ID, names_from = med_grp, values_from = first_date)
}
