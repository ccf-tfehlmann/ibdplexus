#' sparc_med_journey
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
#' @details Medication start and stop dates are chosen independently from both eCRF and EMR sources. Medications with a start or stop date before 1980 are dropped.
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
#'   \item{FIRST_ADVANCED_MED}{is 1 if a medication record is the first advanced medication a patient receives. If a record is the first advanced medication a patient receives, FIRST_BIOLOGIC_NUMBER is equal to the MEDICATION_NUMBER.}
#'   \item{STARTED_AFTER_ENROLLMENT}{is 1 if the medication start date is after the date of consent.}
#'   \item{ADVANCED_MED}{is 1 if the record is for an advanced medication}
#'   \item{ANY_STEROID}{1 if on any steroid at the time of medication listed}
#'   \item{RECTAL_STEROID}{1 if on any rectal steroid at the time of medication listed}
#'   \item{ORAL_IV_STEROID}{1 if on any oral or iv steroid at the time of medication listed}
#' }
#'
#'
#' @export
#'
sparc_med_journey <- function(prescriptions, demographics, observations, encounter, med_groups = c("Biologic", "Aminosalicylates", "Immunomodulators", "Targeted synthetic small molecules"), export = FALSE) {
  # Get medications of interest

  med_groups <- tolower(med_groups)

  medication <- sparc_med_filter(prescriptions, observations, demographics, encounter, med_groups)

  # Find medication start date ----

  med_start <- sparc_med_starts(medication, encounter)


  # Find medication end/discontinuation date ----

  med_end <- sparc_med_ends(medication)

  # Combine start & stop date ----

  med <- full_join(med_start, med_end, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "MEDICATION")) %>%
    mutate(
      MED_START_SOURCE = case_when(
        MED_START_DATE == MED_START_DATE_ECRF & MED_START_DATE != MED_START_DATE_EMR ~ "ECRF",
        MED_START_DATE != MED_START_DATE_ECRF & MED_START_DATE == MED_START_DATE_EMR ~ "EMR",
        MED_START_DATE == MED_START_DATE_ECRF & MED_START_DATE == MED_START_DATE_EMR ~ "BOTH",
        MED_START_DATE == MED_START_DATE_ECRF & is.na(MED_START_DATE_EMR) ~ "ECRF",
        MED_START_DATE == MED_START_DATE_EMR & is.na(MED_START_DATE_ECRF) ~ "EMR",
        TRUE ~ NA_character_
      ),
      MED_END_SOURCE = case_when(
        MED_END_DATE == MED_END_DATE_ECRF & (MED_END_DATE != MED_END_DATE_EMR & MED_END_DATE != MED_DISCONT_START_DATE_EMR) ~ "ECRF",
        MED_END_DATE != MED_END_DATE_ECRF & (MED_END_DATE == MED_END_DATE_EMR | MED_END_DATE == MED_DISCONT_START_DATE_EMR) ~ "EMR",
        MED_END_DATE == MED_END_DATE_ECRF & (MED_END_DATE == MED_END_DATE_EMR | MED_END_DATE == MED_DISCONT_START_DATE_EMR) ~ "BOTH",
        MED_END_DATE == MED_END_DATE_ECRF & (is.na(MED_END_DATE_EMR) & is.na(MED_DISCONT_START_DATE_EMR)) ~ "ECRF",
        (MED_END_DATE == MED_END_DATE_EMR | MED_END_DATE == MED_DISCONT_START_DATE_EMR) & is.na(MED_END_DATE_ECRF) ~ "EMR",
        TRUE ~ NA_character_
      )
    )


  # Add if medication is current ----

  current <- current_med(medication)

  med <- med %>%
    left_join(current, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "MEDICATION")) #%>%
    #mutate(CURRENT_MEDICATION = if_else(is.na(MED_END_DATE), "YES", CURRENT_MEDICATION))

  # Reason Stopped in Smartform or eCRF ----

  stop_crf <- reason_stopped(prescriptions)

  med <- med %>% left_join(stop_crf, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "MEDICATION"))




  # Loading Dose Pattern in EMR ----
  # does prescription pattern in EMR follow the loading dose of the biologic

if("biologic" %in% med_groups){

  first_use_emr <- emr_loading_pattern(medication, encounter)

  med <- med %>%
    left_join(first_use_emr, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "MEDICATION", "MED_START_DATE_EMR"))}



  # Create Mode of Action ----


  moa <- med_grp %>%
    filter(med_type %in% c("biologic", "aminosalicylates", "immunomodulators", "targeted synthetic small molecules")) %>%
    distinct(new_med_name, med_type) %>%
    rename(MEDICATION = new_med_name) %>%
    mutate(MOA = case_when(
      grepl("Adalimumab|Certolizumab|Golimumab|Infliximab", MEDICATION, ignore.case = T) ~ "antiTNF",
      MEDICATION %in% c("Tofacitinib", "Upadacitinib") ~ "JAKi",
      MEDICATION %in% c("Vedolizumab", "Natalizumab") ~ "IRA",
      MEDICATION %in% c("Ustekinumab", "Risankizumab") ~ "ILA",
      MEDICATION %in% c("Ozanimod") ~ "S1P",
      TRUE ~ med_type
    )) %>%
    select(MEDICATION, MOA)

  med <- med %>% left_join(moa, by = "MEDICATION")



  # Flags a Dose Escalation ----


  dose_escalation <- dose_escalation(medication) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION, DOSE_ESCALATION) %>%
    right_join(moa) %>%
    filter(!(MOA %in% c("aminosalicylates", "immunomodulators"))) %>%
    distinct() %>%
    filter(DOSE_ESCALATION == 1) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION, DOSE_ESCALATION)

  med <- med %>% left_join(dose_escalation, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "MEDICATION"))

  # Flags a Frequency Change ----

  if("biologic" %in% med_groups){
  frequency_change <- frequency_change(medication) %>%
    ungroup() %>%
    left_join(moa) %>%
    filter(!(MOA %in% c("aminosalicylates", "immunomodulators"))) %>%
    distinct() %>%
    filter(DECREASE_IN_FREQUENCY == 1) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION, DECREASE_IN_FREQUENCY)

  med <- med %>% left_join(frequency_change, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "MEDICATION"))}

  # Add last medication verification date ----

  verified <- medication %>%
    drop_na(LAST_MEDICATION_VERIFICATION_DATE__C) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID,new_med_name, LAST_MEDICATION_VERIFICATION_DATE__C) %>%
    mutate(LAST_MEDICATION_VERIFICATION_DATE = dmy(LAST_MEDICATION_VERIFICATION_DATE__C)) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name) %>%
    slice(which.max(LAST_MEDICATION_VERIFICATION_DATE)) %>%
    ungroup() %>%
    rename(MEDICATION = new_med_name) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID,MEDICATION, LAST_MEDICATION_VERIFICATION_DATE)

  # if med is verified use eCRF data

  med <- med %>% left_join(verified,by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "MEDICATION")) %>%
    mutate(MED_START_DATE = if_else(!is.na(LAST_MEDICATION_VERIFICATION_DATE), MED_START_DATE_ECRF, MED_START_DATE),
           MED_END_DATE = if_else(!is.na(LAST_MEDICATION_VERIFICATION_DATE), MED_END_DATE_ECRF, MED_END_DATE),
           ) %>%
    mutate(
      MED_START_SOURCE = case_when(
        MED_START_DATE == MED_START_DATE_ECRF & MED_START_DATE != MED_START_DATE_EMR ~ "ECRF",
        MED_START_DATE != MED_START_DATE_ECRF & MED_START_DATE == MED_START_DATE_EMR ~ "EMR",
        MED_START_DATE == MED_START_DATE_ECRF & MED_START_DATE == MED_START_DATE_EMR ~ "BOTH",
        MED_START_DATE == MED_START_DATE_ECRF & is.na(MED_START_DATE_EMR) ~ "ECRF",
        MED_START_DATE == MED_START_DATE_EMR & is.na(MED_START_DATE_ECRF) ~ "EMR",
        TRUE ~ NA_character_
      ),
      MED_END_SOURCE = case_when(
        MED_END_DATE == MED_END_DATE_ECRF & (MED_END_DATE != MED_END_DATE_EMR & MED_END_DATE != MED_DISCONT_START_DATE_EMR) ~ "ECRF",
        MED_END_DATE != MED_END_DATE_ECRF & (MED_END_DATE == MED_END_DATE_EMR | MED_END_DATE == MED_DISCONT_START_DATE_EMR) ~ "EMR",
        MED_END_DATE == MED_END_DATE_ECRF & (MED_END_DATE == MED_END_DATE_EMR | MED_END_DATE == MED_DISCONT_START_DATE_EMR) ~ "BOTH",
        MED_END_DATE == MED_END_DATE_ECRF & (is.na(MED_END_DATE_EMR) & is.na(MED_DISCONT_START_DATE_EMR)) ~ "ECRF",
        (MED_END_DATE == MED_END_DATE_EMR | MED_END_DATE == MED_DISCONT_START_DATE_EMR) & is.na(MED_END_DATE_ECRF) ~ "EMR",
        TRUE ~ NA_character_
      )
    )


  # Flag if on Steroids at the Same time ----

  med <- med %>%
    left_join(steroid_use(prescriptions, observations, demographics, encounter,med))



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


  # Flag if medications overlap  ----
  last_encounter_date <- extract_latest(encounter) %>%
    pull("index_date") %>%
    max(na.rm = TRUE)

  overlap <- med %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(int = case_when(
      !is.na(MED_START_DATE) & !is.na(MED_END_DATE) ~ interval(MED_START_DATE, MED_END_DATE, tz = "UTC"),
      !is.na(MED_START_DATE) & is.na(MED_END_DATE) ~ interval(MED_START_DATE, last_encounter_date, tz = "UTC")
    )) %>%
    drop_na(int) %>%
    arrange(int_start(int), .by_group = TRUE) %>%
    # mutate(overlap = map_int(int, ~ ifelse(sum(int_overlaps(.x, int)) > 1, nrow(.x), NA))) %>%
    # ungroup()
    mutate(
      lag_overlap = case_when(int_overlaps(int, lag(int)) ~ lag(MOA)),
      lead_overlap = case_when(int_overlaps(int, lead(int)) ~ lead(MOA)),
      lag_overlap_days = day(as.period(intersect(int, lag(int)), "days")), # check this ----
      lead_overlap_days = day(as.period(intersect(int, lead(int)), "days")),
    ) %>%
    ungroup() %>%
    mutate(
      OVERLAPPING_MOA = case_when(
        is.na(lag_overlap) ~ lead_overlap,
        is.na(lead_overlap) ~ lag_overlap,
        !is.na(lag_overlap) & !is.na(lead_overlap) ~ paste0(lag_overlap, "; ", lead_overlap)
      ),
      OVERLAPPING_DAYS = if_else(is.na(lag_overlap_days), lead_overlap_days, lag_overlap_days)
    ) %>%
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
    full_join(no_med_enroll, by = "DEIDENTIFIED_MASTER_PATIENT_ID")

  # Flag if bionaive at medication start ----
  # Update to account for patients that have no biologic information in the eCRF and EMR and then incorporate Smartform Ever/Never data
  bionaive <- med %>%
    mutate(b = ifelse(MEDICATION %in% c(
      "Adalimumab", "Certolizumab Pegol", "Golimumab", "Infliximab",
      "Natalizumab", "Other Biologic", "Ustekinumab", "Vedolizumab", "Adalimumab (Humira)", "Infliximab (Avsola)", "Infliximab (Inflectra)", "Infliximab (Renflexis)", "Infliximab (Remicade)", "Infliximab (Remsima)", "Tofacitinib", "Upadacitinib", "Ustekinumab", "Ozanimod", "Risankizumab"
    ), 1, 0)) %>%
    # filter(b == 1) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, b) %>%
    mutate(FIRST_ADVANCED_MED = ifelse(b == 1 & MEDICATION_NUMBER == min(MEDICATION_NUMBER), 1, 0)) %>%
    mutate(FIRST_ADVANCED_MED_NUMBER = ifelse(FIRST_ADVANCED_MED == 1, MEDICATION_NUMBER, as.numeric(NA))) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    fill(FIRST_ADVANCED_MED_NUMBER, .direction = "updown") %>%
    rowwise() %>%
    mutate(BIONAIVE = ifelse(MEDICATION_NUMBER < FIRST_ADVANCED_MED_NUMBER, 1, 0)) %>%
    ungroup() %>%
    rename(ADVANCED_MED = b)

  med <- med %>%
    left_join(bionaive) # %>%
  # rename(ECRF_PRESCRIPTION_DATA = ECRF_DATA)


  # names(med) <- gsub(" ", "_", toupper(names(med)))

  # fix col names
  med <- fix_col_names(med)

  if (export == "TRUE") {
    write.xlsx(med, paste0("SPARC_medication_journey_", Sys.Date(), ".xlsx"), colnames = T)
  }

  return(med)
}
