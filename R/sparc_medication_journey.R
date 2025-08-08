#' sparc_med_journey
#'
#' Extract medication first instance of medication start dates from SPARC patient reported and electronic medical record data. This
#' is only for long term medications including Biologics, Aminosalicylates, and
#' Immunomodulators.
#'
#'
#' @param prescriptions A dataframe with prescriptions data usually generated
#'   using load_data.
#' @param demographics a dataframe with demographic data usually generated using
#'   load_data.
#' @param observations a dataframe with observations data usually generated
#'   using load_data.
#' @param encounter A dataframe with encounter data usually generated using
#'   load_data.
#' @param med_groups A list of medication groups of interest. For details, refer
#'   to \code{\link{sparc_med_filter}}.
#' @param overlap A parameter that specifies if the med journey output should
#'   include OVERLAPPING_DAYS and OVERLAPPING_MOA columns.
#' @param export if excel spreadsheet should be exported. FALSE is default.
#'
#' @return A dataframe with the first medication start date for each drug.
#'
#' @details Medication start and stop dates are chosen independently from both
#'   eCRF and EMR sources. Medications with a start or stop date before 1980 are
#'   dropped. For EMR data, if a medication start date is missing, the visit
#'   encounter start date is used. These records are flagged in the column
#'   VISIT_ENCOUNTER_MED_START.
#'
#'   If a patient has medication information for the same drug from eCRF and
#'   EMR, the eCRF data is preferred and used to generate MED_START_DATE and
#'   MED_END_DATE. If only EMR data is available for that medication, then EMR
#'   data is used. Any overlap between medications is reported along with the
#'   number of days the medications overlap. If no end date is given for a
#'   prescription, the duration of the overlap is calculated assuming an ongoing
#'   prescription. The effective end date is set using a database wide cutoff
#'   based on the the date of the latest encounter any patient had (as returned
#'   by \code{\link{extract_latest}}). Patient level cutoffs are not used
#'   because the last recorded encounter of a specific patient may precede the
#'   latest available EMR by years.
#'
#'   The following columns are convenience flags and indices to facilitate easy
#'   filtering of the data:
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

sparc_med_journey <- function(prescriptions, demographics, observations, encounter, med_groups = c("Biologic", "Aminosalicylates", "Immunomodulators", "Targeted synthetic small molecules"), overlap = FALSE, export = FALSE) {
  # Get medications of interest

  med_groups <- tolower(med_groups)

  medication <- sparc_med_filter(prescriptions, observations, demographics, encounter, med_groups)


  # Add last medication verification date ----

  verify <- med_verification(medication)

  # med <- med %>% left_join(verify)

  # Find medication start date ----

  ## if there is last verification date, use ECRF start date, otherwise earliest

  med_start <- sparc_med_starts(medication, encounter) #  %>%
  # left join(verify) here ----
  # mutate(MED_START_DATE = if_else(!is.na(MED_START_DATE_ECRF), MED_START_DATE_ECRF, MED_START_DATE))


  # Find medication end/discontinuation date ----
  # using local version of sparc_med_ends -- keeping no end date from EMR right now
  med_end <- sparc_med_ends(medication)

  # Add if medication is current ----

  current <- current_med_dates(medication, encounter)

  # med <- med %>%
  #   left_join(current, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "MEDICATION")) #%>%
  # mutate(CURRENT_MEDICATION = if_else(is.na(MED_END_DATE), "YES", CURRENT_MEDICATION))

  # most recent EMR date from anything ----

  ##

  most_recent_EMR <- encounter %>%
    filter(DATA_SOURCE == "EMR") %>%
    # mutate(VISIT_ENCOUNTER_START_DATE = dmy(VISIT_ENCOUNTER_START_DATE)) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    slice(which.max(VISIT_ENCOUNTER_START_DATE)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_START_DATE) %>%
    rename(MOST_RECENT_EMR_ENCOUNTER_DATE = VISIT_ENCOUNTER_START_DATE) %>%
    ungroup()


  most_recent_EMR_pres <- prescriptions %>%
    filter(DATA_SOURCE == "EMR") %>%
    mutate(MED_START_DATE = dmy(MED_START_DATE)) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    slice(which.max(MED_START_DATE)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MED_START_DATE) %>%
    rename(MOST_RECENT_EMR_PRES_DATE = MED_START_DATE) %>%
    ungroup()

  # Combine start & stop date ----



  last_start_emr <- medication %>%
    filter(DATA_SOURCE == "EMR") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name) %>%
    slice(which.max(MED_START_DATE)) %>%
    rename(
      LAST_MED_START_EMR = MED_START_DATE,
      MEDICATION = new_med_name
    ) %>%
    ungroup() %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION, LAST_MED_START_EMR)

  #### 11/12/2024 ----
  # use last med start date from EMR if later than last med end date
  later_med_start <- medication %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name) %>%
    filter(DATA_SOURCE == "EMR") %>%
    filter(!is.na(MED_START_DATE)) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    mutate(R = row_number()) %>%
    mutate(MAX = max(R, na.rm = T)) %>%
    mutate(LAST_NO_END = if_else(MAX == R & is.na(MED_END_DATE), 1, 0)) %>%
    select(
      DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, R, MAX, LAST_NO_END, DATA_SOURCE,
      MED_START_DATE, MED_END_DATE
    ) %>%
    filter(LAST_NO_END == 1) %>%
    rename(
      LATER_MED_START = MED_START_DATE,
      MEDICATION = new_med_name
    ) %>%
    ungroup() %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID,MEDICATION, LAST_NO_END, LATER_MED_START)

  med <- full_join(med_start, med_end, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "MEDICATION")) %>%
    left_join(later_med_start, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION)) %>%
    left_join(verify, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION)) %>%
    left_join(current, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION)) %>%
    # join most recent EMR encounter dates
    left_join(most_recent_EMR, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
    left_join(most_recent_EMR_pres, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
    left_join(last_start_emr, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION)) %>%
    # apply med end date logic here ----
  mutate(MED_END_DATE = NA) %>%
    # select med end date from ecrf first
    mutate(MED_END_DATE = if_else(!is.na(MED_END_DATE_ECRF), MED_END_DATE_ECRF, MED_END_DATE)) %>%
    # make latest EMR med start date med_end_date when no med end date in EMR
    mutate(MED_END_DATE_EMR = if_else(!is.na(LATER_MED_START), LATER_MED_START, MED_END_DATE_EMR)) %>%
    # add other EMR med end dates
    mutate(MED_END_DATE = if_else(is.na(MED_END_DATE) & !is.na(MED_END_DATE_EMR), MED_END_DATE_EMR, MED_END_DATE)) %>%
    # use last med start date when all other med end dates are missing
    mutate(MED_END_DATE = if_else(is.na(MED_END_DATE), LAST_MED_START_EMR, MED_END_DATE)) %>%
    # create original med end date for emr to keep track of the original med end dates EMR
    mutate(ORIGINAL_MED_END_EMR = MED_END_DATE_EMR) %>%
    # check with most recent EMR prescription date and EMR encounter date
    # EMR medication end date <= 90 days of most recent EMR data
    ## flag for within 90 days of most recent EMR prescription date
    mutate(flag_pres90 = if_else(difftime(MOST_RECENT_EMR_PRES_DATE, MED_END_DATE,
                                          units = "days") < 90, 1, 0)) %>%
    ## flag for within 90 days of most recent EMR encounter date
    mutate(flag_enc90 = if_else(difftime(MOST_RECENT_EMR_ENCOUNTER_DATE, MED_END_DATE, units = "days") < 90, 1, 0)) %>%
    ## flag for med end date > last medication verification date
    mutate(flag_medver = if_else(!is.na(LAST_MEDICATION_VERIFICATION_DATE) &
                                   MED_END_DATE_EMR < LAST_MEDICATION_VERIFICATION_DATE, 1, 0)) %>%
    ## flag for med end date > current medication date
    mutate(flag_current = if_else(!is.na(CURRENT_MEDICATION) &
                                    MED_END_DATE_EMR < CURRENT_MEDICATION, 1, 0)) %>%
    mutate(across(flag_pres90:flag_current, ~replace_na(.x, 0))) %>%
    ## remove med end date when it is EMR med end date and any of the verification flags == 1
    mutate(MED_END_DATE = if_else(is.na(MED_END_DATE_ECRF) & flag_medver == 1 | is.na(MED_END_DATE_ECRF) & flag_current == 1,
                                  NA, MED_END_DATE)) %>%
    ## drop med end date if either the most recent emr prescription date > 90
    ## days or the most recent emr encounter date > 90 days from the latest med
    ## end date we have for that medication
    mutate(MED_END_DATE = if_else(is.na(MED_END_DATE_ECRF) & flag_pres90 == 1 & flag_enc90 == 1, MED_END_DATE, MED_END_DATE)) %>%
    ## add in logic using flags
    mutate(LOGIC_USED = case_when(
      !is.na(MED_END_DATE_ECRF) ~ "ECRF END DATE",
      is.na(MED_END_DATE_ECRF) & (flag_medver == 1 | flag_current == 1) ~ "MED VERIFICATION OR CURRENT FLAG OVERRIDE",
      is.na(MED_END_DATE_ECRF) & (flag_pres90 == 1 | flag_enc90 == 1) ~ "EMR END DATE WITHIN 90 DAYS of EMR ENCOUNTER DATE",
      is.na(MED_END_DATE_ECRF) & (!is.na(MED_END_DATE)) & !is.na(MED_END_DATE_EMR) ~ "EMR END DATE",
      is.na(MED_END_DATE_ECRF) & (!is.na(MED_END_DATE)) & is.na(MED_END_DATE_EMR) & MED_END_DATE == LAST_MED_START_EMR
~ "LAST MED START DATE IN EMR",

      TRUE ~ "NO MED END DATES"
    )) %>%
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
        MED_END_DATE == MED_END_DATE_ECRF & (MED_END_DATE != MED_END_DATE_EMR
                                             # & MED_END_DATE != MED_DISCONT_START_DATE_EMR
        ) ~ "ECRF",
        MED_END_DATE != MED_END_DATE_ECRF & (MED_END_DATE == MED_END_DATE_EMR
                                             ## | MED_END_DATE == MED_DISCONT_START_DATE_EMR
        ) ~ "EMR",
        MED_END_DATE == MED_END_DATE_ECRF & (MED_END_DATE == MED_END_DATE_EMR
                                             ## | MED_END_DATE == MED_DISCONT_START_DATE_EMR
        ) ~ "BOTH",
        MED_END_DATE == MED_END_DATE_ECRF & (is.na(MED_END_DATE_EMR)
                                             # & is.na(MED_DISCONT_START_DATE_EMR)
        ) ~ "ECRF",
        (MED_END_DATE == MED_END_DATE_EMR
         ## | MED_END_DATE == MED_DISCONT_START_DATE_EMR
        ) & is.na(MED_END_DATE_ECRF) ~ "EMR",
        MED_END_DATE != MED_END_DATE_ECRF & (MED_END_DATE == MED_END_DATE_EMR
                                             ## | MED_END_DATE == MED_DISCONT_START_DATE_EMR
        ) | (LAST_MED_START_EMR == MED_END_DATE) ~ "EMR",
        TRUE ~ NA_character_
      )
    ) %>%
    relocate(MED_END_DATE_EMR, .after = MED_END_DATE) %>%
    relocate(MOST_RECENT_EMR_ENCOUNTER_DATE, .after = MED_END_DATE) %>%
    relocate(CURRENT_MEDICATION, .after = MED_END_DATE) %>%
    relocate(LAST_MEDICATION_VERIFICATION_DATE, .after = MED_END_DATE) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION) %>%
    # create last update date for all patients
    mutate(LAST_UPDATE_DATE = if_else(!is.na(DEIDENTIFIED_MASTER_PATIENT_ID),
                                      max(c_across(LAST_MEDICATION_VERIFICATION_DATE:
                                                     MED_END_DATE_EMR), na.rm = T), NA
    )) %>%
    mutate(LAST_UPDATE_DATE = if_else(is.infinite(LAST_UPDATE_DATE), NA, LAST_UPDATE_DATE)) %>%
    ungroup() %>%
    select(-c(starts_with("flag_"), LATER_MED_START, LAST_NO_END)) %>%
    select(
      DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION, MED_START_DATE,
      MED_END_DATE, MED_START_SOURCE, MED_END_SOURCE, MED_START_DATE_ECRF,
      MED_END_DATE_ECRF, CURRENT_MEDICATION, LAST_MEDICATION_VERIFICATION_DATE,
      MED_START_DATE_EMR, VISIT_ENCOUNTER_MED_START,
      MED_END_DATE_EMR, MED_DISCONT_START_DATE_EMR, LAST_MED_START_EMR, MOST_RECENT_EMR_ENCOUNTER_DATE,
      MOST_RECENT_EMR_PRES_DATE,
      LAST_UPDATE_DATE, LOGIC_USED
    ) %>%
    rename(
      VISIT_ENCOUNTER_MED_START_EMR = VISIT_ENCOUNTER_MED_START,
      CURRENT_MEDICATION_ECRF = CURRENT_MEDICATION,
      LAST_MEDICATION_VERIFICATION_DATE_ECRF = LAST_MEDICATION_VERIFICATION_DATE
    )



  #### INTERNAL VERSION ----

  med_internal <- med %>%
    select(
      DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION,
      MED_START_DATE_ECRF, MED_START_DATE_EMR,
      VISIT_ENCOUNTER_MED_START_EMR, MED_START_DATE, MED_START_SOURCE,
      MED_END_DATE_ECRF, MED_END_DATE_EMR,
      MED_END_DATE, LAST_UPDATE_DATE,
      LOGIC_USED, MED_END_SOURCE, LAST_MEDICATION_VERIFICATION_DATE_ECRF,
      CURRENT_MEDICATION_ECRF, LAST_MED_START_EMR, MOST_RECENT_EMR_ENCOUNTER_DATE,
      MOST_RECENT_EMR_PRES_DATE
    ) %>%
    mutate(`Flag if MED_END_DATE is Latest Med Start From EMR` = ifelse(LOGIC_USED == "LAST MED START EMR", 1, 0)) %>%
    rename(
      `Flag if MED_START_DATE_EMR is Encounter Date` = VISIT_ENCOUNTER_MED_START_EMR,
      `MED_END_DATE Reason` = LOGIC_USED,
      `Last Current Medication Flag` = CURRENT_MEDICATION_ECRF,
      `EMR Prescription Last Start Date` = MOST_RECENT_EMR_PRES_DATE
    ) %>%
    relocate(`Flag if MED_END_DATE is Latest Med Start From EMR`, .after = "MED_END_DATE_EMR")



  # Reason Stopped in Smartform or eCRF ----

  stop_crf <- reason_stopped(prescriptions)

  med <- med %>% left_join(stop_crf, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "MEDICATION"))




  # Loading Dose Pattern in EMR ----
  # does prescription pattern in EMR follow the loading dose of the biologic

  if ("biologic" %in% med_groups) {
    first_use_emr <- emr_loading_pattern(medication, encounter)

    med <- med %>%
      left_join(first_use_emr, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "MEDICATION", "MED_START_DATE_EMR"))
  }



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

  if ("biologic" %in% med_groups) {
    frequency_change <- frequency_change(medication) %>%
      ungroup() %>%
      left_join(moa) %>%
      filter(!(MOA %in% c("aminosalicylates", "immunomodulators"))) %>%
      distinct() %>%
      filter(DECREASE_IN_FREQUENCY == 1) %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION, DECREASE_IN_FREQUENCY)

    med <- med %>% left_join(frequency_change, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "MEDICATION"))
  }


  # Flag if on Steroids at the Same time ----

  med <- med %>%
    left_join(steroid_use(prescriptions, observations, demographics, encounter, med))



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

  if (overlap == TRUE) {
    overlap <- overlap(med)

    med <- med %>%
      left_join(overlap, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "MEDICATION", "MED_START_DATE", "MED_END_DATE")) %>%
      select(-R)
  }

  # overlap <- med %>% group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
  #   mutate(int = case_when(!is.na(MED_START_DATE) & !is.na(MED_END_DATE) ~
  #                            interval(MED_START_DATE, MED_END_DATE, tz = "UTC"),
  #                          !is.na(MED_START_DATE) & is.na(MED_END_DATE) ~ interval(MED_START_DATE,
  #                                                                                  last_encounter_date, tz = "UTC"))) %>% drop_na(int) %>%
  #   arrange(int_start(int), .by_group = TRUE) %>% mutate(lag_overlap = case_when(int_overlaps(int,
  #                                                                                             lag(int)) ~ lag(MOA)),
  #                                                        lead_overlap = case_when(int_overlaps(int, lead(int)) ~ lead(MOA)),
  #                                                        lag_overlap_days = case_when(int_overlaps(int,
  #                                                                                                  lag(int)) ~ day(as.period(intersect(int, lag(int)), "days"))),
  #                                                        lead_overlap_days = case_when(int_overlaps(int, lead(int)) ~ day(as.period(intersect(int, lead(int)), "days")))) %>%
  # ungroup() %>% mutate(OVERLAPPING_MOA = case_when(is.na(lag_overlap) ~  lead_overlap, is.na(lead_overlap) ~ lag_overlap, !is.na(lag_overlap) &  !is.na(lead_overlap) ~ paste0(lag_overlap, "; ", lead_overlap)),
  #                      OVERLAPPING_DAYS = if_else(is.na(lag_overlap_days),  lead_overlap_days, lag_overlap_days)) %>%
  # select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION, MED_START_DATE, MED_END_DATE, OVERLAPPING_MOA,  OVERLAPPING_DAYS)




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
  mutate(b = case_when(
    MEDICATION %in% c(
      "Adalimumab", "Certolizumab Pegol", "Golimumab", "Infliximab", "Etrasimod", "Natalizumab", "Other Biologic", "Ustekinumab", "Vedolizumab", "Tofacitinib", "Upadacitinib", "Ustekinumab", "Ozanimod", "Risankizumab", "Guselkumab", "Mirikizumab"
    ) ~ 1,
    grepl("Adalimumab|Infliximab", MEDICATION, ignore.case = T) ~ 1,
    TRUE ~ 0
  )) %>%
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


  med <- med %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NUMBER)

  # names(med) <- gsub(" ", "_", toupper(names(med)))

  # fix col names
  med <- fix_col_names(med)

  # remove everyone without a date of consent
  med <- med %>% drop_na(DATE_OF_CONSENT)

  if (export == "TRUE") {
    write.xlsx(med, paste0("SPARC_medication_journey_", Sys.Date(), ".xlsx"), colnames = T)
  }

  return(med)
}
