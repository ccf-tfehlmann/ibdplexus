
#' sparc_medication
#'
#' Reads in SPARC data from IBD Plexus, and finds the medications the participant is prescribed from the
#' electronic medical record and patient reported case report forms at a
#' specific (“index”) date.
#'
#' @param datadir directory where unzipped data is saved.
#' @param index_info A dataframe with DEIDENTIFIED_MASTER_PATIENT_ID and a variable index_date.  Default is date of consent.
#' @param filename the name of the output file.
#' @param index_range the number of days to look out from index date.
#'
#' @return A dataframe with medication predictions and an excel file.
#' @export
sparc_medication <- function(datadir,
                             index_info = "DATE_OF_CONSENT",
                             filename = "SPARC_MEDICATION.xlsx",
                             index_range = "30"){


data = load_data(datadir = datadir, cohort = "SPARC", domains = c("Demographics", "Prescriptions", "Diagnosis", "Encounter", "Observations"), data_type = "BOTH")

#===============================
#DEMOGRAPHIC INFORMATION
#===============================
#demographic information

consent = data$demographics %>%
  filter(DATA_SOURCE == "ECRF_SPARC") %>%
  distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DATE_OF_CONSENT, DATE_OF_CONSENT_WITHDRAWN) %>%
  mutate(DATE_OF_CONSENT = dmy(DATE_OF_CONSENT)) %>%
  filter(year(DATE_OF_CONSENT) >= 2016) %>%
  group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
  slice(which.min(DATE_OF_CONSENT)) %>%
  ungroup()

demo = data$demographics %>%
  filter(DATA_SOURCE %in% c("EMR", "ECRF_SPARC")) %>%
  arrange(DEIDENTIFIED_MASTER_PATIENT_ID,desc(DATA_SOURCE)) %>%
  group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
  slice(1) %>%
  distinct(DEIDENTIFIED_MASTER_PATIENT_ID, BIRTH_YEAR, GENDER) %>%
  ungroup()

demo = full_join(consent, demo)
#===============================
#CONVERT DAYS AFTER INDEX  TO NUMERIC VALUE
#===============================

t = as.numeric(index_range)
#===============================
#DEFINE INDEX DATE
#===============================

if("DATE_OF_CONSENT" %in% index_info){cohort = demo %>% mutate(index_date = DATE_OF_CONSENT) } else {cohort = index_info %>% left_join(demo)}

  #===============================
  #ADD DIAGNOSIS
  #===============================

  #latest_diagnosis

if(0){
  dx = data$diagnosis %>%
    filter(DATA_SOURCE %in% c("SF_SPARC", "ECRF_SPARC", "ECRF_QORUS")) %>%
    filter(DIAG_CONCEPT_NAME %in% c("Crohn's Disease", "IBD Unclassified", "Ulcerative Colitis")) %>%
    left_join(data$encounter) %>%
    mutate(DIAG_STATUS_CONCEPT_CODE = ifelse(DATA_SOURCE != "SF_SPARC", "Yes", DIAG_STATUS_CONCEPT_CODE)) %>%
    filter(DIAG_STATUS_CONCEPT_CODE == "Yes") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(Diagnosis = DIAG_CONCEPT_NAME) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, match(DATA_SOURCE, c("SF_SPARC","ECRF_SPARC")),  match(Diagnosis, c("Crohn's Disease", "Ulcerative Colitis", "IBD Unclassified")),desc(dmy(VISIT_ENCOUNTER_START_DATE))) %>%
    right_join(cohort) %>%
    mutate(diff = abs(dmy(VISIT_ENCOUNTER_START_DATE) - index_date)) %>%
    slice(which.min(diff)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, Diagnosis)}


dx = data$diagnosis %>%
  filter(DATA_SOURCE %in% c("SF_SPARC", "ECRF_SPARC", "ECRF_QORUS")) %>%
  filter(DIAG_CONCEPT_NAME %in% c("Crohn's Disease", "IBD Unclassified", "Ulcerative Colitis")) %>%
  left_join(data$encounter) %>%
  group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
  mutate(Diagnosis = DIAG_CONCEPT_NAME) %>%
  dplyr::mutate(keep = ifelse(DATA_SOURCE == "SF_SPARC" & is.na(DIAG_STATUS_CONCEPT_NAME), 0, 1)) %>% #Smartform Data should have a DIAG_STATUS_CONCEPT_NAME equal to yes
  filter(keep ==1) %>%
  arrange(DEIDENTIFIED_MASTER_PATIENT_ID, match(DATA_SOURCE, c("SF_SPARC","ECRF_SPARC")), desc(dmy(VISIT_ENCOUNTER_START_DATE))) %>%
  slice(1) %>%
  select(DEIDENTIFIED_MASTER_PATIENT_ID, Diagnosis)


  cohort = left_join(cohort, dx)


#===============================
#DEFINE MEDICATION GROUPS
#===============================

  #upload medication mapping

  med_grp = med_grp %>%
    mutate(med_grp = case_when(med_type == "Corticosteroids" ~ "Corticosteroids",
                               med_type == "Antibiotics" ~ "Antibiotics",
                               TRUE ~ new_med_name)) %>%
    mutate(med_grp = gsub("\u00A0", " ", med_grp, fixed = TRUE))


  #separate out biologics

  bio_grp = med_grp %>%
    filter(med_type == "Biologic")

  med_grp = med_grp %>%
    filter(med_type != "Biologic")

#===============================
#MEDICATION INFORMATION FROM OBSERVATION TABLE
#===============================

  obs_med = data$observations %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    left_join(data$encounter) %>%
    filter(TYPE_OF_ENCOUNTER == "IBD Medication Survey") %>%
    mutate(OBS_TEST_RESULT_DATE = dmy(OBS_TEST_RESULT_DATE))

  #No Medication at Enrollment (First Survey with Are you Currently Taking Any Medication = NO) Does not include antibiotics/probiotics
  #Enrollment = +/- t days of Date of Consent

  med_enroll = obs_med %>%
    filter(OBS_TEST_CONCEPT_NAME == "Are you currently taking any medication for your IBD?") %>%
    left_join(demo) %>%
    mutate(diff = OBS_TEST_RESULT_DATE - DATE_OF_CONSENT) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    slice(which.min(diff)) %>%
    filter(diff <= t) %>%
    filter(DESCRIPTIVE_SYMP_TEST_RESULTS == "No") %>%
    left_join(data$prescriptions) %>%
    left_join(med_grp) %>%
    mutate(`No_IBD_Medication_At_Enrollment` = 1) %>%
    dplyr::rename(no_current_date = OBS_TEST_RESULT_DATE) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, `No_IBD_Medication_At_Enrollment`)
  cohort = left_join(cohort, med_enroll)

  #No Changes After 90 Days. If No Change Can Pull forward Past Medication Entered.
  #create survey order

  ecrf_med_encounters = data$encounter %>%
    filter(TYPE_OF_ENCOUNTER == "IBD Medication Survey") %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, dmy(VISIT_ENCOUNTER_START_DATE)) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(medication_survey_n = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
    mutate(helper = paste0(DEIDENTIFIED_MASTER_PATIENT_ID, medication_survey_n))

  #find survey to pull forward
  no_changes_med = obs_med %>%
    filter(OBS_TEST_CONCEPT_NAME %in% c("In the last 90 days, have you had any changes in your Medication(s)?")) %>%
    left_join(demo) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    filter(DESCRIPTIVE_SYMP_TEST_RESULTS == "No")

  ecrf_med_encounters = ecrf_med_encounters %>%
    mutate(nochange = ifelse(VISIT_ENCOUNTER_ID %in% no_changes_med$VISIT_ENCOUNTER_ID, "No Change", "Yes Change"))

  ecrf_med_encounters = split(ecrf_med_encounters, ecrf_med_encounters$DEIDENTIFIED_MASTER_PATIENT_ID)
  for (i in 1:length(ecrf_med_encounters)){
    df = data.frame(ecrf_med_encounters[[i]])
    x <- which(df$nochange == 'Yes Change')
    z <- which(df$nochange == 'No Change')
    if(length(z) != 0 & length(x) != 0){
      df$med_survey_to_pull_forward[df$nochange == "No Change"] <- df$medication_survey_n[x[sapply(z, function(j) findInterval(j, x))]]}
    else { df$med_survey_to_pull_forward[df$nochange == "No Change"] <- as.numeric(NA)}
    ecrf_med_encounters[[i]] = df
  }

  ecrf_med_encounters = bind_rows(ecrf_med_encounters) %>%
    mutate(med_survey_to_pull_forward = ifelse(nochange == "No Change" & medication_survey_n == 1, as.numeric(NA), med_survey_to_pull_forward))

  #If first survey has no changes at 90 days then no medication at enrollment

  med_enroll_2 = ecrf_med_encounters %>%
    filter(nochange == "No Change") %>%
    filter(medication_survey_n == 1) %>%
    filter(!(VISIT_ENCOUNTER_ID %in% data$prescriptions$VISIT_ENCOUNTER_ID)) %>%
    mutate(`No_IBD_Medication_At_Enrollment2` = 1) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, `No_IBD_Medication_At_Enrollment2`)

  cohort = left_join(cohort, med_enroll_2) %>%
    mutate(No_IBD_Medication_At_Enrollment = ifelse(is.na(No_IBD_Medication_At_Enrollment), No_IBD_Medication_At_Enrollment2, No_IBD_Medication_At_Enrollment)) %>%
    select(-No_IBD_Medication_At_Enrollment2)

  #Pull Medication forward in ECRF
  pull_forward_encounter = ecrf_med_encounters %>%
    drop_na(med_survey_to_pull_forward) %>%
    mutate(helper = paste0(DEIDENTIFIED_MASTER_PATIENT_ID, med_survey_to_pull_forward)) %>%
    mutate(new_encounter_id = VISIT_ENCOUNTER_ID) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, helper, new_encounter_id)

  pull_forward_prescriptions = ecrf_med_encounters %>%
    mutate(helper = paste0(DEIDENTIFIED_MASTER_PATIENT_ID, medication_survey_n)) %>%
    left_join(pull_forward_encounter) %>%
    drop_na(new_encounter_id) %>%
    left_join(data$prescriptions) %>%
    mutate(VISIT_ENCOUNTER_ID = new_encounter_id)

  pull_forward_prescriptions = pull_forward_prescriptions%>%
    select(intersect(names(pull_forward_prescriptions), names(data$prescriptions)))

  #===============================
  #MEDICATIONS FROM ECRF
  #===============================
  ecrf_med = data$prescriptions %>%
    bind_rows(pull_forward_prescriptions) %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    left_join(med_grp) %>%
    drop_na(med_grp) %>%
    group_by(VISIT_ENCOUNTER_ID, MEDICATION_NAME, MED_START_DATE) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, match(CURRENT_MEDICATION, c("YES", "NO"))) %>%
    slice(1) %>%
    ungroup() %>%
    left_join(data$encounter) %>%
    mutate(VISIT_ENCOUNTER_START_DATE = dmy(VISIT_ENCOUNTER_START_DATE)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, VISIT_ENCOUNTER_START_DATE, MEDICATION_NAME, CURRENT_MEDICATION, MED_START_DATE, MED_END_DATE, med_type, new_med_name, med_grp) %>%
    mutate(MED_START_DATE = dmy(MED_START_DATE),
           MED_END_DATE = if_else(CURRENT_MEDICATION == "NO" & is.na(MED_END_DATE), (VISIT_ENCOUNTER_START_DATE), dmy(MED_END_DATE))) %>%
    left_join(cohort) %>%
    drop_na(index_date) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID,med_grp, index_date) %>%
    mutate(diff = abs(index_date - VISIT_ENCOUNTER_START_DATE)) %>%
    slice(which.min(diff)) %>%
    ungroup() %>%
    mutate(int = MED_START_DATE %--% MED_END_DATE,
           int2 = if_else(is.na(MED_END_DATE), MED_START_DATE %--% VISIT_ENCOUNTER_START_DATE, as.interval(NA))) %>%
    mutate(status = case_when( !is.na(MED_END_DATE) & MED_END_DATE < index_date ~ "Medication Stopped Prior to Index",
                               index_date == MED_START_DATE ~ "Medication Started on Same Date as Index",
                               index_date == MED_END_DATE ~ "Medication End Date on Same Date as Index",
                               index_date %within% int ~ "On Medication at Time of Index",
                               index_date %within% int2 ~ "On Medication at Time of Index",
                               index_date < MED_START_DATE ~ "Medication Started After Index")) %>%
    mutate(possible_datediff = if_else(is.na(status), (as.numeric(index_date - VISIT_ENCOUNTER_START_DATE)/30.5), as.numeric(NA))) %>%
    mutate(possible_cat = case_when(possible_datediff <= 3 ~ "Possibly on Medication (Index Performed <= 3 Months After Last Reported Medication)",
                                    possible_datediff > 3 & possible_datediff <= 6 ~ "Possibly on Medication (Index Performed <= 6 Months After Last Reported Medication)",
                                    possible_datediff > 6 & possible_datediff <= 12 ~ "Possibly on Medication (Index Performed <= 12 Months After Last Reported Medication)",
                                    possible_datediff > 12 ~ "Possibly on Medication (Index Performed > 1 Year After Last Reported Medication)")) %>%
    mutate(status = ifelse(is.na(status), possible_cat, status)) %>%
    left_join(med_grp) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, med_grp, status) %>%
    mutate(onmed = case_when(grepl("Possibly", status) ~ "Possible",
                             status %in% c("On Medication at Time of Index") ~ "Yes",
                             TRUE ~ "No")) %>%
    mutate(onmed = ifelse(status == "Possibly on Medication (Index Performed <= 3 Months After Last Reported Medication)", "Yes", onmed))

  med_status_ecrf = ecrf_med %>%
    dcast(DEIDENTIFIED_MASTER_PATIENT_ID + index_date  ~ med_grp ,
          value.var = "onmed",  fun.aggregate = function(x) paste(x, collapse = "; ")) %>%
    dplyr::rename_at(vars(-DEIDENTIFIED_MASTER_PATIENT_ID, -index_date), ~ paste0(., '_ECRF')) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID,  index_date, everything())

  ecrf_med = ecrf_med %>%
    dcast(DEIDENTIFIED_MASTER_PATIENT_ID + index_date  ~ med_grp ,
          value.var = "status",  fun.aggregate = function(x) paste(x, collapse = "; ")) %>%
    dplyr::rename_at(vars(-DEIDENTIFIED_MASTER_PATIENT_ID, -index_date), ~ paste0(., '_ECRF')) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID,  index_date, everything())

  cohort = left_join(cohort, ecrf_med)

  #===============================
  #MEDICATIONS FROM EMR
  #===============================
  emr_med = data$prescriptions %>%
    filter(DEIDENTIFIED_MASTER_PATIENT_ID %in% cohort$DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    filter(DATA_SOURCE == "EMR")

   m = med_grp$MEDICATION_NAME

   med = NULL
  for( i in 1:length(m)){
    k = emr_med %>%
      filter(grepl(m[i], MEDICATION_NAME, ignore.case=T)) %>%
      mutate(drug = paste0(m[i]))
    med[[i]] = k
  }

   names(med) = m

   med = med[sapply(med, nrow)>0]

   #pick most recently prescribed for each medication group

   emr_med = bind_rows(med) %>%
    distinct() %>%
    left_join(med_grp, by = c("drug" = "MEDICATION_NAME")) %>%
    drop_na(med_grp) %>%
    mutate(med_grp = gsub("\u00A0", " ", med_grp, fixed = TRUE)) %>%
    ungroup() %>%
    filter(!is.na(MED_START_DATE) | !is.na(MED_END_DATE)) %>%
    left_join(data$encounter) %>%
    mutate(MED_START_DATE = dmy(MED_START_DATE),
           MED_END_DATE = dmy(MED_END_DATE),
           VISIT_ENCOUNTER_START_DATE = dmy(VISIT_ENCOUNTER_START_DATE)) %>%
    left_join(cohort) %>%
    drop_na(index_date) %>%
    mutate(int = MED_START_DATE %--% MED_END_DATE,
           int2 = if_else(is.na(MED_END_DATE), MED_START_DATE %--% VISIT_ENCOUNTER_START_DATE, as.interval(NA)),
           int3 = if_else(is.na(MED_START_DATE), VISIT_ENCOUNTER_START_DATE %--% MED_END_DATE, as.interval(NA))) %>%
    mutate(status = case_when( !is.na(MED_END_DATE) & MED_END_DATE < index_date ~ "Medication Stopped Prior to Index",
                               index_date == MED_START_DATE ~ "Medication Started on Same Date as Index",
                               index_date == MED_END_DATE ~ "Medication End Date on Same Date as Index",
                               index_date %within% int ~ "On Medication at Time of Index",
                               index_date %within% int2 ~ "On Medication at Time of Index",
                               index_date %within% int3 ~ "On Medication at Time of Index",
                               index_date < MED_START_DATE ~ "Medication Started After Index")) %>%
    mutate(possible_datediff = if_else(is.na(status), (as.numeric(index_date - VISIT_ENCOUNTER_START_DATE)/30.5), as.numeric(NA))) %>%
    mutate(possible_cat = case_when(possible_datediff <= 3 ~ "Possibly on Medication (Index Performed <= 3 Months After Last Reported Medication)",
                                    possible_datediff > 3 & possible_datediff <= 6 ~ "Possibly on Medication (Index Performed <= 6 Months After Last Reported Medication)",
                                    possible_datediff > 6 & possible_datediff <= 12 ~ "Possibly on Medication (Index Performed <= 12 Months After Last Reported Medication)",
                                    possible_datediff > 12 ~ "Possibly on Medication (Index Performed > 1 Year After Last Reported Medication)")) %>%
    mutate(status = ifelse(is.na(status), possible_cat, status)) %>%
    ungroup() %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, med_grp,
            match(status, c("On Medication at Time of Index","Medication End Date on Same Date as Index","Medication Started on Same Date as Index", "Possibly on Medication (Index Performed <= 3 Months After Last Reported Medication)",
                            "Possibly on Medication (Index Performed <= 6 Months After Last Reported Medication)","Possibly on Medication (Index Performed <= 12 Months After Last Reported Medication)",
                            "Possibly on Medication (Index Performed > 1 Year After Last Reported Medication)","Medication Started After Index","Medication Started After Index","Medication Stopped Prior to Index"))) %>%
    mutate(index_to_start = MED_START_DATE - index_date,
           index_to_end = index_date - MED_END_DATE) %>%
    mutate(status = case_when(status == "Medication Stopped Prior to Index" ~ paste0("Medication Stopped ", round(index_to_end,0), " days Prior to Index"),
                              status == "Medication Started After Index" ~ paste0("Medication Started ", round(index_to_start,0), " days After Index"),
                              TRUE ~ status)) %>%
    left_join(med_grp) %>%

     distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, med_grp, med_type, status, VISIT_ENCOUNTER_START_DATE,MED_START_DATE, MED_END_DATE,index_to_start, index_to_end) %>%
    mutate(indexdays = ifelse(index_to_start < 0, index_to_end, index_to_start )) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, med_grp, indexdays,
            match(status, c("On Medication at Time of Index","Medication End Date on Same Date as Index","Medication Started on Same Date as Index", "Possibly on Medication (Index Performed <= 3 Months After Last Reported Medication)",
                            "Possibly on Medication (Index Performed <= 6 Months After Last Reported Medication)","Possibly on Medication (Index Performed <= 12 Months After Last Reported Medication)",
                            "Possibly on Medication (Index Performed > 1 Year After Last Reported Medication)"))) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, med_grp, index_date) %>%
    slice(1) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, med_type) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, med_type, abs(index_to_start)) %>%
    mutate(count = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
    mutate(status = ifelse(med_type == "Biologic" & status %in% c("Possibly on Medication (Index Performed <= 3 Months After Last Reported Medication", "On Medication at Time of Index") & count > 1, "Medication Most Likely Stopped Prior to Index", status)) %>%

    mutate(onmed = case_when(grepl("Possibly", status) ~ "Possible",
                             status %in% c("On Medication at Time of Index") ~ "Yes",
                             TRUE ~ "No")) %>%
    mutate(onmed = ifelse(status == "Possibly on Medication (Index Performed <= 3 Months After Last Reported Medication)", "Yes", onmed))

   med_status_emr = emr_med %>%
    dcast(DEIDENTIFIED_MASTER_PATIENT_ID + index_date  ~ med_grp ,
          value.var = "onmed",  fun.aggregate = function(x) paste(x, collapse = "; ")) %>%
    dplyr::rename_at(vars(-DEIDENTIFIED_MASTER_PATIENT_ID, -index_date), ~ paste0(., '_EMR')) %>%
    # filter(SAMPLE_COLLECTED_DATE == index_date) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID,  index_date, everything())

   emr_med = emr_med %>%
    dcast(DEIDENTIFIED_MASTER_PATIENT_ID + index_date  ~ med_grp ,
          value.var = "status",  fun.aggregate = function(x) paste(x, collapse = "; ")) %>%
    dplyr::rename_at(vars(-DEIDENTIFIED_MASTER_PATIENT_ID, -index_date), ~ paste0(., '_EMR')) %>%
    # filter(SAMPLE_COLLECTED_DATE == index_date) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID,  index_date, everything())

   cohort = left_join(cohort, emr_med)

   rm(list = c("ecrf_med", "ecrf_med_encounters", "emr_med", "k", "med", "med_enroll", "med_enroll_2", "no_changes_med", "obs_med"))

   #===============================
   #COMPARE SOURCES FOR MEDICATIONS
   #===============================
   med_list = unique(med_grp$med_grp)

   ms = full_join(med_status_emr, med_status_ecrf)

   for (i in 1:length(med_list)){
     ii = (med_list[i])
     nums=grep(paste0(ii), names(ms))
     if(length(nums) > 1){
       ms = within(ms, assign(paste0(ii,"_status"), sapply(1:nrow(ms), function(x){paste0(ms[nums[1]][x,], ";", ms[nums[2]][x,])})))} else if(length(nums) == 1){
         ms = within(ms, assign(paste0(ii,"_status"), sapply(1:nrow(ms), function(x){paste0(ms[nums[1]][x,])})))} else{
           ms = within(ms, assign(paste0(ii,"_status"), sapply(1:nrow(ms), function(x){paste0(as.character(NA))}))) }
   }

   med_status = ms %>%
     mutate_all(funs(gsub("NA", "", .))) %>%
     mutate_all(funs(gsub("No;No", "No", .))) %>%
     mutate_all(funs(gsub("Possible;Possible", "Possible", .))) %>%
     mutate_all(funs(gsub("Possible;Yes|Yes;Yes|Yes;Possible", "Yes", .))) %>%
     mutate_all(funs(gsub("No;Yes|Possible;No|Yes;No|No;Possible", "Contradicts", .))) %>%
     mutate_all(funs(gsub(";", "", .)))  %>%
     mutate(index_date = ymd(index_date))

   med_status = med_status[grep("_status|DEIDENTIFIED_MASTER_PATIENT_ID|index_date", names(med_status))]

   cohort = left_join(cohort, med_status)
#===============================
#BIOLOGICS
#===============================

   #===============================
   #BIOLOGICS FROM ECRF
   #===============================
   ecrf_bio = data$prescriptions %>%
     bind_rows(pull_forward_prescriptions) %>%
     filter(DATA_SOURCE == "ECRF_SPARC") %>%
     left_join(bio_grp) %>%
     drop_na(med_grp) %>%
     group_by(VISIT_ENCOUNTER_ID, MEDICATION_NAME, MED_START_DATE) %>%
     arrange(DEIDENTIFIED_MASTER_PATIENT_ID, match(CURRENT_MEDICATION, c("YES", "NO"))) %>%
     slice(1) %>%
     ungroup() %>%
     left_join(data$encounter) %>%
     mutate(VISIT_ENCOUNTER_START_DATE = dmy(VISIT_ENCOUNTER_START_DATE)) %>%
     select(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, VISIT_ENCOUNTER_START_DATE, MEDICATION_NAME, CURRENT_MEDICATION, MED_START_DATE, MED_END_DATE, med_type, new_med_name, med_grp) %>%
     mutate(MED_START_DATE = dmy(MED_START_DATE),
            MED_END_DATE = if_else(CURRENT_MEDICATION == "NO" & is.na(MED_END_DATE), (VISIT_ENCOUNTER_START_DATE), dmy(MED_END_DATE))) %>%
     left_join(cohort) %>%
     drop_na(index_date) %>%
     group_by(DEIDENTIFIED_MASTER_PATIENT_ID,med_grp, index_date) %>%
     mutate(diff = abs(index_date - VISIT_ENCOUNTER_START_DATE)) %>%
     slice(which.min(diff)) %>%
     ungroup() %>%
     mutate(int = MED_START_DATE %--% MED_END_DATE,
            int2 = if_else(is.na(MED_END_DATE), MED_START_DATE %--% VISIT_ENCOUNTER_START_DATE, as.interval(NA))) %>%
     mutate(status = case_when( !is.na(MED_END_DATE) & MED_END_DATE < index_date ~ "Medication Stopped Prior to Index",
                                index_date == MED_START_DATE ~ "Medication Started on Same Date as Index",
                                index_date == MED_END_DATE ~ "Medication End Date on Same Date as Index",
                                index_date %within% int ~ "On Medication at Time of Index",
                                index_date %within% int2 ~ "On Medication at Time of Index",
                                index_date < MED_START_DATE ~ "Medication Started After Index")) %>%
     mutate(possible_datediff = if_else(is.na(status), (as.numeric(index_date - VISIT_ENCOUNTER_START_DATE)/30.5), as.numeric(NA))) %>%
     mutate(possible_cat = case_when(possible_datediff <= 3 ~ "Possibly on Medication (Index Performed <= 3 Months After Last Reported Medication)",
                                     possible_datediff > 3 & possible_datediff <= 6 ~ "Possibly on Medication (Index Performed <= 6 Months After Last Reported Medication)",
                                     possible_datediff > 6 & possible_datediff <= 12 ~ "Possibly on Medication (Index Performed <= 12 Months After Last Reported Medication)",
                                     possible_datediff > 12 ~ "Possibly on Medication (Index Performed > 1 Year After Last Reported Medication)")) %>%
     mutate(status = ifelse(is.na(status), possible_cat, status)) %>%
     left_join(bio_grp) %>%
     distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, med_grp, status,MED_START_DATE, VISIT_ENCOUNTER_START_DATE, MED_END_DATE) %>%
     mutate(onmed = case_when(grepl("Possibly", status) ~ "Possible",
                              status %in% c("On Medication at Time of Index") ~ "Yes",
                              TRUE ~ "No")) %>%
     mutate(onmed = ifelse(status == "Possibly on Medication (Index Performed <= 3 Months After Last Reported Medication)", "Yes", onmed))

   #if on both pick most recently reported
   mult_bio_ecrf = ecrf_bio %>%
     dcast(DEIDENTIFIED_MASTER_PATIENT_ID + index_date  ~ med_grp ,
           value.var = "onmed",  fun.aggregate = function(x) paste(x, collapse = "; ")) %>%
     dplyr::rename_at(vars(-DEIDENTIFIED_MASTER_PATIENT_ID, -index_date), ~ paste0(., '_ECRF')) %>%
     select(DEIDENTIFIED_MASTER_PATIENT_ID,  index_date, everything()) %>%
     unite("status", Adalimumab_ECRF:Vedolizumab_ECRF, sep = ";", remove = F, na.rm = T) %>%
     mutate(yescount = str_count(status, pattern = "Yes")) %>%
     filter(yescount > 1)


   if(dim(mult_bio_ecrf)[1] > 0){mult_bio_ecrf = mult_bio_ecrf %>%
     pivot_longer(cols = Adalimumab_ECRF:Vedolizumab_ECRF, names_to = "Biologic", values_to = "Biologic_Status" ) %>%
     filter(Biologic_Status == "Yes") %>%
     mutate(med_grp = gsub("_ECRF", "", Biologic)) %>%
     distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, med_grp, Biologic_Status) %>%
     left_join(ecrf_bio) %>%
     group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
     mutate(new_start_date = if_else(is.na(MED_START_DATE), VISIT_ENCOUNTER_START_DATE, MED_START_DATE)) %>%
     mutate(most_recent_date = max(new_start_date)) %>%
     mutate(keep = case_when(new_start_date == most_recent_date ~ 1,
                             med_grp %in% c("Ustekinumab", "Vedolizumab") ~ 2)) %>%
     slice(which.min(keep)) %>%
     distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, med_grp) %>%
     rename(bio_to_keep = med_grp)

   ecrf_bio = ecrf_bio %>%
     left_join(mult_bio_ecrf) %>%
     mutate(onmed = case_when(bio_to_keep == med_grp ~ "Yes",
                              !is.na(bio_to_keep) & bio_to_keep != med_grp ~ "No",
                              TRUE ~ onmed)) %>%
     ungroup()} else {ecrf_bio = ecrf_bio}


   bio_status_ecrf = ecrf_bio %>%
     dcast(DEIDENTIFIED_MASTER_PATIENT_ID + index_date  ~ med_grp ,
           value.var = "onmed",  fun.aggregate = function(x) paste(x, collapse = "; ")) %>%
     dplyr::rename_at(vars(-DEIDENTIFIED_MASTER_PATIENT_ID, -index_date), ~ paste0(., '_ECRF')) %>%
     select(DEIDENTIFIED_MASTER_PATIENT_ID,  index_date, everything())

   ecrf_bio = ecrf_bio %>%
     dcast(DEIDENTIFIED_MASTER_PATIENT_ID + index_date  ~ med_grp ,
           value.var = "status",  fun.aggregate = function(x) paste(x, collapse = "; ")) %>%
     dplyr::rename_at(vars(-DEIDENTIFIED_MASTER_PATIENT_ID, -index_date), ~ paste0(., '_ECRF')) %>%
     select(DEIDENTIFIED_MASTER_PATIENT_ID,  index_date, everything())

   cohort = left_join(cohort, ecrf_bio)

   #===============================
   #BIOLOGICS FROM EMR
   #===============================
   emr_bio = data$prescriptions %>%
     filter(DEIDENTIFIED_MASTER_PATIENT_ID %in% cohort$DEIDENTIFIED_MASTER_PATIENT_ID) %>%
     filter(DATA_SOURCE == "EMR")

   b = bio_grp$MEDICATION_NAME

   bio = NULL
   for( i in 1:length(b)){
     k = emr_bio %>%
       filter(grepl(b[i], MEDICATION_NAME, ignore.case=T)) %>%
       mutate(drug = paste0(b[i]))
     bio[[i]] = k
   }

   names(bio) = b

   bio = bio[sapply(bio, nrow)>0]

   #pick most recently prescribed for each medication group

   emr_bio = bind_rows(bio) %>%
     distinct() %>%
     left_join(bio_grp, by = c("drug" = "MEDICATION_NAME")) %>%
     drop_na(med_grp) %>%
     mutate(med_grp = gsub("\u00A0", " ", med_grp, fixed = TRUE)) %>%
     ungroup() %>%
     filter(!is.na(MED_START_DATE) | !is.na(MED_END_DATE)) %>%
     left_join(data$encounter) %>%
     mutate(MED_START_DATE = dmy(MED_START_DATE),
            MED_END_DATE = dmy(MED_END_DATE),
            VISIT_ENCOUNTER_START_DATE = dmy(VISIT_ENCOUNTER_START_DATE)) %>%
     left_join(cohort) %>%
     drop_na(index_date) %>%
     mutate(int = MED_START_DATE %--% MED_END_DATE,
            int2 = if_else(is.na(MED_END_DATE), MED_START_DATE %--% VISIT_ENCOUNTER_START_DATE, as.interval(NA)),
            int3 = if_else(is.na(MED_START_DATE), VISIT_ENCOUNTER_START_DATE %--% MED_END_DATE, as.interval(NA))) %>%
     mutate(status = case_when( !is.na(MED_END_DATE) & MED_END_DATE < index_date ~ "Medication Stopped Prior to Index",
                                index_date == MED_START_DATE ~ "Medication Started on Same Date as Index",
                                index_date == MED_END_DATE ~ "Medication End Date on Same Date as Index",
                                index_date %within% int ~ "On Medication at Time of Index",
                                index_date %within% int2 ~ "On Medication at Time of Index",
                                index_date %within% int3 ~ "On Medication at Time of Index",
                                index_date < MED_START_DATE ~ "Medication Started After Index")) %>%
     mutate(possible_datediff = if_else(is.na(status), (as.numeric(index_date - VISIT_ENCOUNTER_START_DATE)/30.5), as.numeric(NA))) %>%
     mutate(possible_cat = case_when(possible_datediff <= 3 ~ "Possibly on Medication (Index Performed <= 3 Months After Last Reported Medication)",
                                     possible_datediff > 3 & possible_datediff <= 6 ~ "Possibly on Medication (Index Performed <= 6 Months After Last Reported Medication)",
                                     possible_datediff > 6 & possible_datediff <= 12 ~ "Possibly on Medication (Index Performed <= 12 Months After Last Reported Medication)",
                                     possible_datediff > 12 ~ "Possibly on Medication (Index Performed > 1 Year After Last Reported Medication)")) %>%
     mutate(status = ifelse(is.na(status), possible_cat, status)) %>%
     ungroup() %>%
     arrange(DEIDENTIFIED_MASTER_PATIENT_ID, med_grp,
             match(status, c("On Medication at Time of Index","Medication End Date on Same Date as Index","Medication Started on Same Date as Index", "Possibly on Medication (Index Performed <= 3 Months After Last Reported Medication)",
                             "Possibly on Medication (Index Performed <= 6 Months After Last Reported Medication)","Possibly on Medication (Index Performed <= 12 Months After Last Reported Medication)",
                             "Possibly on Medication (Index Performed > 1 Year After Last Reported Medication)","Medication Started After Index","Medication Started After Index","Medication Stopped Prior to Index"))) %>%
     mutate(index_to_start = MED_START_DATE - index_date,
            index_to_end = index_date - MED_END_DATE) %>%
     mutate(status = case_when(status == "Medication Stopped Prior to Index" ~ paste0("Medication Stopped ", round(index_to_end,0), " days Prior to Index"),
                               status == "Medication Started After Index" ~ paste0("Medication Started ", round(index_to_start,0), " days After Index"),
                               TRUE ~ status)) %>%
     left_join(bio_grp) %>%

     distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, med_grp, med_type, status, VISIT_ENCOUNTER_START_DATE,MED_START_DATE, MED_END_DATE,index_to_start, index_to_end) %>%
     mutate(indexdays = ifelse(index_to_start < 0, index_to_end, index_to_start )) %>%
     arrange(DEIDENTIFIED_MASTER_PATIENT_ID, med_grp, indexdays,
             match(status, c("On Medication at Time of Index","Medication End Date on Same Date as Index","Medication Started on Same Date as Index", "Possibly on Medication (Index Performed <= 3 Months After Last Reported Medication)",
                             "Possibly on Medication (Index Performed <= 6 Months After Last Reported Medication)","Possibly on Medication (Index Performed <= 12 Months After Last Reported Medication)",
                             "Possibly on Medication (Index Performed > 1 Year After Last Reported Medication)"))) %>%
     group_by(DEIDENTIFIED_MASTER_PATIENT_ID, med_grp, index_date) %>%
     slice(1) %>%
     group_by(DEIDENTIFIED_MASTER_PATIENT_ID, med_type) %>%
     arrange(DEIDENTIFIED_MASTER_PATIENT_ID, med_type, abs(index_to_start)) %>%
     mutate(count = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
     mutate(status = ifelse(med_type == "Biologic" & status %in% c("Possibly on Medication (Index Performed <= 3 Months After Last Reported Medication", "On Medication at Time of Index") & count > 1, "Medication Most Likely Stopped Prior to Index", status)) %>%
     mutate(onmed = case_when(grepl("Possibly", status) ~ "Possible",
                              status %in% c("On Medication at Time of Index") ~ "Yes",
                              TRUE ~ "No")) %>%
     mutate(onmed = ifelse(status == "Possibly on Medication (Index Performed <= 3 Months After Last Reported Medication)", "Yes", onmed))

   #if on both pick most recently prescribed
   mult_bio_emr = emr_bio %>%
     dcast(DEIDENTIFIED_MASTER_PATIENT_ID + index_date  ~ med_grp ,
           value.var = "onmed",  fun.aggregate = function(x) paste(x, collapse = "; ")) %>%
     dplyr::rename_at(vars(-DEIDENTIFIED_MASTER_PATIENT_ID, -index_date), ~ paste0(., '_EMR')) %>%
     # filter(SAMPLE_COLLECTED_DATE == index_date) %>%
     select(DEIDENTIFIED_MASTER_PATIENT_ID,  index_date, everything()) %>%
     unite("status", Adalimumab_EMR:Vedolizumab_EMR, sep = ";", remove = F, na.rm = T) %>%
     mutate(yescount = str_count(status, pattern = "Yes")) %>%
     filter(yescount > 1)

   if(dim(mult_bio_emr)[1] > 0){
   mult_bio_emr = mult_bio_emr %>%
     pivot_longer(cols = Adalimumab_EMR:Vedolizumab_EMR, names_to = "Biologic", values_to = "Biologic_Status" ) %>%
     filter(Biologic_Status == "Yes") %>%
     mutate(med_grp = gsub("_EMR", "", Biologic)) %>%
     distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, med_grp, Biologic_Status) %>%
     left_join(emr_bio) %>%
     group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
     mutate(new_start_date = if_else(is.na(MED_START_DATE), VISIT_ENCOUNTER_START_DATE, MED_START_DATE)) %>%
     mutate(most_recent_date = max(new_start_date)) %>%
     mutate(keep = case_when(new_start_date == MED_START_DATE ~ 1,
                             (new_start_date != MED_START_DATE | is.na(MED_START_DATE)) & status == "On Medication at Time of Index" ~ 2,
                             (new_start_date != MED_START_DATE | is.na(MED_START_DATE)) & status != "On Medication at Time of Index" & new_start_date == most_recent_date ~ 3)) %>%
     slice(which.min(keep)) %>%
     distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, med_grp) %>%
     rename(bio_to_keep = med_grp)

   emr_bio = emr_bio %>%
     left_join(mult_bio_emr) %>%
     mutate(onmed = case_when(bio_to_keep == med_grp ~ "Yes",
                              !is.na(bio_to_keep) & bio_to_keep != med_grp ~ "No",
                              TRUE ~ onmed)) %>%
     ungroup()} else {emr_bio = emr_bio}


   bio_status_emr = emr_bio %>%
     dcast(DEIDENTIFIED_MASTER_PATIENT_ID + index_date  ~ med_grp ,
           value.var = "onmed",  fun.aggregate = function(x) paste(x, collapse = "; ")) %>%
     dplyr::rename_at(vars(-DEIDENTIFIED_MASTER_PATIENT_ID, -index_date), ~ paste0(., '_EMR')) %>%
     # filter(SAMPLE_COLLECTED_DATE == index_date) %>%
     select(DEIDENTIFIED_MASTER_PATIENT_ID,  index_date, everything())

   emr_bio = emr_bio %>%
     dcast(DEIDENTIFIED_MASTER_PATIENT_ID + index_date  ~ med_grp ,
           value.var = "status",  fun.aggregate = function(x) paste(x, collapse = "; ")) %>%
     dplyr::rename_at(vars(-DEIDENTIFIED_MASTER_PATIENT_ID, -index_date), ~ paste0(., '_EMR')) %>%
     # filter(SAMPLE_COLLECTED_DATE == index_date) %>%
     select(DEIDENTIFIED_MASTER_PATIENT_ID,  index_date, everything())

   cohort = left_join(cohort, emr_bio)


   #===============================
   #COMPARE SOURCES FOR BIOLOGICS
   #===============================
   bio_list = bio_grp %>% mutate(med_grp = gsub("\u00A0", " ", med_grp, fixed = TRUE))
   bio_list =  unique(bio_list$med_grp)

   bs = full_join(bio_status_emr, bio_status_ecrf)

   for (i in 1:length(bio_list)){
     ii = (bio_list[i])
     nubs=grep(paste0(ii), names(bs))
     if(length(nubs) > 1){
       bs = within(bs, assign(paste0(ii,"_status"), sapply(1:nrow(bs), function(x){paste0(bs[nubs[1]][x,], ";", bs[nubs[2]][x,])})))} else if(length(nubs) == 1){
         bs = within(bs, assign(paste0(ii,"_status"), sapply(1:nrow(bs), function(x){paste0(bs[nubs[1]][x,])})))} else{
           bs = within(bs, assign(paste0(ii,"_status"), sapply(1:nrow(bs), function(x){paste0(as.character(NA))}))) }
   }

   bio_status = bs %>%
     mutate_all(funs(gsub("NA", "", .))) %>%
     mutate_all(funs(gsub("No;No", "No", .))) %>%
     mutate_all(funs(gsub("Possible;Possible", "Possible", .))) %>%
     mutate_all(funs(gsub("Possible;Yes|Yes;Yes|Yes;Possible", "Yes", .))) %>%
     mutate_all(funs(gsub("No;Yes|Possible;No|Yes;No|No;Possible", "Contradicts", .))) %>%
     mutate_all(funs(gsub(";", "", .)))  %>%
     mutate(index_date = ymd(index_date))

  bio_status = bio_status[grep("_status|DEIDENTIFIED_MASTER_PATIENT_ID|index_date", names(bio_status))]

  #Pull all biologic columns from cohort
  #get biologics list
  bio_grp2 = bio_grp %>%
    mutate(med_grp = gsub("\u00A0", " ", med_grp, fixed = TRUE)) %>%
    select(med_grp) %>%
    distinct()

  match <- paste(bio_grp2$med_grp, collapse = "|")
  match <- paste0("DEIDENTIFIED_MASTER_PATIENT_ID|index_date|", match)

  cohort_bio = cohort[,grep(match, colnames(cohort))]

  cohort_bio = cohort_bio %>%
    mutate_if(is.character, ~replace_na(., ""))


  #For Multiple Biologic, Pick One
  mult_bio_status = bio_status %>%
    unite("status", Adalimumab_status:Vedolizumab_status, sep = ";", remove = F, na.rm = T) %>%
    mutate(yescount = str_count(status, pattern = "Yes")) %>%
    filter(yescount > 1)

  if(dim(mult_bio_status)[1] > 0){ mult_bio_status = mult_bio_status %>%
    pivot_longer(cols = Adalimumab_status:Vedolizumab_status, names_to = "Biologic", values_to = "Biologic_Status" ) %>%
    filter(Biologic_Status == "Yes") %>%
    mutate(med_grp = gsub("_status", "", Biologic)) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, med_grp, Biologic_Status) %>%
    left_join(cohort_bio) %>%
    select(colnames(.)[1:4],(sort(colnames(.)[5:26]))) %>%
    pivot_longer(cols = c(Adalimumab_ECRF:Vedolizumab_EMR), names_to = "column", values_to = "values") %>%
    filter(values != "") %>%
    filter(!grepl("status",column)) %>%
    separate(column, c("Drug", "Source")) %>%
    filter(med_grp == Drug) %>%
    distinct() %>%
    pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, Drug), names_from = Source, values_from = values) %>%
    mutate(emr_status = case_when(EMR %in% c("On Medication at Time of Index") ~ "Yes",
                                  EMR %in% c("Possibly on Medication (Index Performed <= 3 Months After Last Reported Medication", "Possibly on Medication (Index Performed <= 3 Months After Last Reported Medication)") ~ "More Possible",
                                  EMR %in% c("Possibly on Medication (Index Performed <= 12 Months After Last Reported Medication)","Possibly on Medication (Index Performed > 1 Year After Last Reported Medication)", "Possibly on Medication (Index Performed <= 6 Months After Last Reported Medication)") ~ "Less Possible",
                                  EMR %in% c("", " ") | is.na(EMR) ~ "No EMR Info",
                                  TRUE ~ "No")) %>%
    mutate(ecrf_status = case_when(ECRF %in% c("On Medication at Time of Index") ~ "Yes",
                                   ECRF %in% c("Possibly on Medication (Index Performed <= 3 Months After Last Reported Medication", "Possibly on Medication (Index Performed <= 3 Months After Last Reported Medication)") ~ "More Possible",
                                   ECRF %in% c("Possibly on Medication (Index Performed <= 12 Months After Last Reported Medication)","Possibly on Medication (Index Performed > 1 Year After Last Reported Medication)", "Possibly on Medication (Index Performed <= 6 Months After Last Reported Medication)") ~ "Less Possible",
                                   ECRF %in% c("", " ") | is.na(ECRF) ~ "No ECRF Info",
                                   TRUE ~ "No")) %>%
    mutate(Biologic_at_index_1 = case_when(emr_status == "Yes" & ecrf_status == "Yes" ~ paste0(Drug)),
           Biologic_at_index_2 = case_when(emr_status == "No EMR Info" & ecrf_status %in% c("Yes", "More Possible", "Less Possible") ~ paste0(Drug)),
           Biologic_at_index_3 = case_when(emr_status %in% c("Yes", "More Possible", "Less Possible") & ecrf_status == "No ECRF Info" ~ paste0(Drug)),
           Biologic_at_index_4 = case_when(emr_status == "More Possible" & ecrf_status == "More Possible" ~ paste0(Drug)),
           Biologic_at_index_5 = case_when(emr_status == "Less Possible" & ecrf_status == "Less Possible" ~ paste0(Drug)),
           Biologic_at_index_7 = case_when(emr_status == "Yes" & ecrf_status %in% c("Less Possible", "More Possible") ~  paste0(Drug)),
           Biologic_at_index_6 = case_when(emr_status %in% c("Less Possible", "More Possible") & ecrf_status %in% c("Yes")   ~ paste0(Drug)),
           Biologic_at_index_8 = case_when(emr_status == "No" & ecrf_status %in% c("Yes","Less Possible", "More Possible") ~  paste0(Drug)),
           Biologic_at_index_9 = case_when(emr_status %in% c("Yes","Less Possible", "More Possible") & ecrf_status %in% c("No")   ~ paste0(Drug))) %>%
    pivot_longer(cols = Biologic_at_index_1:Biologic_at_index_9, names_to = "Strength", values_to = "Biologic") %>%
    drop_na(Biologic) %>%
    mutate(Strength = as.numeric(gsub("Biologic_at_index_", "", Strength)))  %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, Strength)  %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
    slice(which.min(Strength)) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, Biologic, Strength)

  bio_status = bio_status %>%
    left_join(mult_bio_status) %>%
    pivot_longer(cols = Adalimumab_status:Vedolizumab_status, names_to = "Biologic2", values_to = "Biologic_Status" ) %>%
    mutate(Biologic2 = gsub("_status", "", Biologic2))    %>%
    mutate(Biologic_Status = case_when(Biologic == Biologic2 ~ "Yes",
                             (!(Biologic_Status %in% c(" ", ""))) & Biologic != Biologic2  ~ "No",
                             TRUE ~ Biologic_Status)) %>%
    mutate(Biologic2 = paste0(Biologic2, "_status")) %>%
    pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, index_date), names_from = Biologic2, values_from = Biologic_Status)} else{bio_status = bio_status}


   cohort = left_join(cohort, bio_status)


   #===============================
   #Biologic at Index
   #===============================

   #get biologics list
   bio_grp2 = bio_grp %>%
     mutate(med_grp = gsub("\u00A0", " ", med_grp, fixed = TRUE)) %>%
     select(med_grp) %>%
     distinct()

   match <- paste(bio_grp2$med_grp, collapse = "|")


   match <- paste0("DEIDENTIFIED_MASTER_PATIENT_ID|index_date|", match)


   biologics = cohort[,grep(match, colnames(cohort))]

   index_biologics = biologics %>%
     mutate_if(is.character, ~replace_na(., "")) %>%
        pivot_longer(cols = Adalimumab_status:Vedolizumab_status, names_to = "Biologic", values_to = "Biologic_Status" ) %>%
     filter(Biologic_Status == "Yes") %>%
     mutate(med_grp = gsub("_status", "", Biologic)) %>%
     distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, med_grp, Biologic_Status) %>%
     left_join(biologics) %>%
     select(colnames(.)[1:4],(sort(colnames(.)[5:26]))) %>%
     pivot_longer(cols = c(Adalimumab_ECRF:Vedolizumab_EMR), names_to = "column", values_to = "values") %>%
     filter(values != "") %>%
     filter(!grepl("status",column)) %>%
     separate(column, c("Drug", "Source"), sep = "_") %>%
     filter(med_grp == Drug) %>%
     distinct() %>%
     pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, Drug), names_from = Source, values_from = values)  %>%
     mutate(emr_status = case_when(EMR %in% c("Possibly on Medication (Index Performed <= 3 Months After Last Reported Medication", "On Medication at Time of Index") ~ "Yes",
                                   EMR %in% c("Possibly on Medication (Index Performed <= 3 Months After Last Reported Medication", "Possibly on Medication (Index Performed <= 3 Months After Last Reported Medication)") ~ "More Possible",
                                   EMR %in% c("Possibly on Medication (Index Performed <= 12 Months After Last Reported Medication)","Possibly on Medication (Index Performed > 1 Year After Last Reported Medication)", "Possibly on Medication (Index Performed <= 6 Months After Last Reported Medication)") ~ "Less Possible",
                                   EMR %in% c("", " ") | is.na(EMR) ~ "No EMR Info",
                                   TRUE ~ "No")) %>%
     mutate(ecrf_status = case_when(ECRF %in% c("On Medication at Time of Index") ~ "Yes",
                                    ECRF %in% c("Possibly on Medication (Index Performed <= 3 Months After Last Reported Medication", "Possibly on Medication (Index Performed <= 3 Months After Last Reported Medication)") ~ "More Possible",
                                    ECRF %in% c("Possibly on Medication (Index Performed <= 12 Months After Last Reported Medication)","Possibly on Medication (Index Performed > 1 Year After Last Reported Medication)", "Possibly on Medication (Index Performed <= 6 Months After Last Reported Medication)") ~ "Less Possible",
                                    ECRF %in% c("", " ") | is.na(ECRF) ~ "No ECRF Info",
                                    TRUE ~ "No")) %>%
     mutate(Biologic_at_index_1 = case_when(emr_status == "Yes" & ecrf_status == "Yes" ~ paste0(Drug)),
            Biologic_at_index_2 = case_when(emr_status == "No EMR Info" & ecrf_status %in% c("Yes", "More Possible", "Less Possible") ~ paste0(Drug)),
            Biologic_at_index_3 = case_when(emr_status %in% c("Yes", "More Possible", "Less Possible") & ecrf_status == "No ECRF Info" ~ paste0(Drug)),
            Biologic_at_index_4 = case_when(emr_status == "More Possible" & ecrf_status == "More Possible" ~ paste0(Drug)),
            Biologic_at_index_5 = case_when(emr_status == "Less Possible" & ecrf_status == "Less Possible" ~ paste0(Drug)),
            Biologic_at_index_7 = case_when(emr_status == "Yes" & ecrf_status %in% c("Less Possible", "More Possible") ~  paste0(Drug)),
            Biologic_at_index_6 = case_when(emr_status %in% c("Less Possible", "More Possible") & ecrf_status %in% c("Yes")   ~ paste0(Drug)),
            Biologic_at_index_8 = case_when(emr_status == "No" & ecrf_status %in% c("Yes","Less Possible", "More Possible") ~  paste0(Drug)),
            Biologic_at_index_9 = case_when(emr_status %in% c("Yes","Less Possible", "More Possible") & ecrf_status %in% c("No")   ~ paste0(Drug))) %>%
     pivot_longer(cols = Biologic_at_index_1:Biologic_at_index_9, names_to = "Strength", values_to = "Biologic") %>%
     drop_na(Biologic) %>%
     mutate(Strength = as.numeric(gsub("Biologic_at_index_", "", Strength)))  %>%
     arrange(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, Strength)  %>%
     group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
     slice(which.min(Strength)) %>%
     distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, Biologic, Strength) %>%
     rename(Biologic_at_index = Biologic) %>%
     rename(Biologic_at_index_confidence = Strength)

   cohort = left_join(cohort, index_biologics)


  #===============================
  #FIND IF FIRST TIME ON BIOLOGIC (LOADING DATE)
  #===============================

   #===============================
   #EMR
   #===============================


   loading = data$prescriptions %>%
    filter(DEIDENTIFIED_MASTER_PATIENT_ID %in% cohort$DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    filter(DATA_SOURCE == "EMR")

   m = bio_grp$MEDICATION_NAME

   med = NULL

   for( i in 1:length(m)){
    k = loading %>%
      filter(grepl(m[i], MEDICATION_NAME, ignore.case=T)) %>%
      mutate(drug = paste0(m[i]))
    med[[i]] = k
  }

   names(med) = m

   med = med[sapply(med, nrow)>0]

   loading = bind_rows(med) %>%
    distinct() %>%
    left_join(bio_grp, by = c("drug" = "MEDICATION_NAME")) %>%
    drop_na(med_grp) %>%
    mutate(med_grp = gsub("\u00A0", " ", med_grp, fixed = TRUE)) %>%
    ungroup() %>%
    filter(!is.na(MED_START_DATE) | !is.na(MED_END_DATE)) %>%
    left_join(data$encounter) %>%
    mutate(MED_START_DATE = if_else(is.na(MED_START_DATE), dmy(VISIT_ENCOUNTER_START_DATE), dmy(MED_START_DATE)),
           MED_END_DATE = if_else(is.na(MED_END_DATE), dmy(VISIT_ENCOUNTER_START_DATE), dmy(MED_END_DATE)),
           VISIT_ENCOUNTER_START_DATE = dmy(VISIT_ENCOUNTER_START_DATE)) %>%
    left_join(cohort) %>%
    drop_na(index_date) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, med_grp, MED_START_DATE) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, med_grp) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID,  VISIT_ENCOUNTER_START_DATE, med_grp, ROUTE_OF_MEDICATION ,MEDICATION_DOMAIN ,MED_START_DATE,
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


   rm(list = c("med", "m", "loading", "first_use"))

   #===============================
   #ECRF
   #===============================

   loading_ecrf= data$prescriptions %>%
     filter(DEIDENTIFIED_MASTER_PATIENT_ID %in% cohort$DEIDENTIFIED_MASTER_PATIENT_ID) %>%
     filter(DATA_SOURCE == "ECRF_SPARC")

   m = bio_grp$MEDICATION_NAME

   med = NULL

   for( i in 1:length(m)){
     k = loading_ecrf %>%
       filter(grepl(m[i], MEDICATION_NAME, ignore.case=T)) %>%
       mutate(drug = paste0(m[i]))
     med[[i]] = k
   }

   names(med) = m

   med = med[sapply(med, nrow)>0]

   loading = bind_rows(med) %>%
     distinct() %>%
     left_join(bio_grp, by = c("drug" = "MEDICATION_NAME")) %>%
     drop_na(med_grp) %>%
     mutate(med_grp = gsub("\u00A0", " ", med_grp, fixed = TRUE)) %>%
     ungroup() %>%
     filter(!is.na(MED_START_DATE) | !is.na(MED_END_DATE)) %>%
     left_join(data$encounter) %>%
     mutate(MED_START_DATE = if_else(is.na(MED_START_DATE), dmy(VISIT_ENCOUNTER_START_DATE), dmy(MED_START_DATE)),
            MED_END_DATE = if_else(is.na(MED_END_DATE), dmy(VISIT_ENCOUNTER_START_DATE), dmy(MED_END_DATE)),
            VISIT_ENCOUNTER_START_DATE = dmy(VISIT_ENCOUNTER_START_DATE)) %>%
     left_join(cohort) %>%
     drop_na(index_date) %>%
     arrange(DEIDENTIFIED_MASTER_PATIENT_ID, med_grp, MED_START_DATE) %>%
     group_by(DEIDENTIFIED_MASTER_PATIENT_ID, med_grp) %>%
     select(DEIDENTIFIED_MASTER_PATIENT_ID,  VISIT_ENCOUNTER_START_DATE, med_grp, ROUTE_OF_MEDICATION ,MEDICATION_DOMAIN ,MED_START_DATE,
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
     fill(first_date,.direction = c("down")) %>%
     distinct(DEIDENTIFIED_MASTER_PATIENT_ID, med_grp, first_date)

   first_use_ecrf = loading  %>%
     distinct(DEIDENTIFIED_MASTER_PATIENT_ID, med_grp, first_date) %>%
     mutate(med_grp = paste0(med_grp, "_Loading_Date_ECRF")) %>%
     pivot_wider(id_cols = DEIDENTIFIED_MASTER_PATIENT_ID, names_from = med_grp, values_from = first_date)

   #Combine ECRF with EMR and define if they agree

   first_use = full_join(first_use_emr, first_use_ecrf)


   #COMPARE Loading Dates from EMR and ECRF
   med_list = unique(bio_grp$med_grp)


   first_use_status =  first_use %>%
     pivot_longer(cols = starts_with(med_list), names_to = "med", values_to ="Loading_Date") %>%
     mutate(source = case_when(as.character(grepl("EMR", med)) == "TRUE" ~ "EMR",
                                 as.character(grepl("ECRF", med)) == "TRUE"  ~ "ECRF")) %>%
     mutate(med = gsub("_Loading_Date_EMR|_Loading_Date_ECRF", "", med)) %>%
     drop_na(Loading_Date) %>%
     #distinct(DEIDENTIFIED_MASTER_PATIENT_ID, med, Loading_Date, .keep_all = T) %>%
     group_by(DEIDENTIFIED_MASTER_PATIENT_ID, med) %>%
     mutate(count = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
     arrange(DEIDENTIFIED_MASTER_PATIENT_ID, med, Loading_Date) %>%
     mutate(diff = Loading_Date - lag(Loading_Date)) %>%
     filter( n() > 1 ) %>%
     mutate(Loading_Date_Status = if_else(diff == 0, "Same loading date", paste0(diff, " days between loading dates"))) %>%
     drop_na(Loading_Date_Status) %>%
     mutate(med = paste0(med, "_Loading_Date_Status")) %>%
     distinct() %>%
     pivot_wider(id_cols = DEIDENTIFIED_MASTER_PATIENT_ID, names_from = med, values_from = Loading_Date_Status)

   first_use = full_join(first_use, first_use_status)
   first_use = first_use[unlist(lapply(unique(gsub("Loading_Date_EMR_|Loading_Date_ECRF_|Loading_Date_Status_","", names(first_use))), function(x) grep(x, names(first_use))))]

   cohort = cohort %>% left_join(first_use)

  #===============================
  #NO MEDICATION AT INDEX DATE
  #===============================
  no_med_index = cohort %>%
    filter_at(vars(ends_with("_status")), all_vars(. %in% c("No", "", NA))) %>%
    mutate(No_IBD_Medication_At_Index = 1) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, No_IBD_Medication_At_Index)

   cohort = left_join(cohort, no_med_index)
    #===============================
  #REORDER COLUMNS
  #===============================



   bionames =  bio_grp %>%
     mutate(med_grp = gsub("\u00A0", " ", med_grp, fixed = TRUE)) %>%
     arrange(match(med_type, c("Biologic")), med_grp) %>%
     distinct(med_grp) %>%
     rename(name = med_grp) %>%
     add_row(name = c("Biologic_at_index"))  %>%
     add_row(name = c("No_IBD_Medication")) %>%
     arrange(match(name, c("No_IBD_Medication",  "Biologic_at_index")))


  mednames = med_grp %>%
    mutate(med_grp = gsub("\u00A0", " ", med_grp, fixed = TRUE)) %>%
    arrange(match(med_type, c("Antibiotics", "Corticosteroids", "Aminosalicylates", "Antidiarrheals", "Immunomodulators", "Probiotic", "Other")), med_grp) %>%
    distinct(med_grp) %>%
    rename(name = med_grp)


  names = bind_rows(data.frame(name = names(index_info)), data.frame(name = names(demo)), data.frame(name = names(dx))) %>%
    distinct() %>% bind_rows(bionames) %>% bind_rows(mednames) %>% filter(name != "DATE_OF_CONSENT_WITHDRAWN")

  cohort = cohort[unlist(lapply(names$name, function(x) grep(x, names(cohort))))]


  names(cohort) = toupper(names(cohort))

#===============================
#CREATE HEADER STYLES
#===============================
#blue
style1 <- createStyle(bgFill = "#BDD7EE", textDecoration = "bold")

#orange
style2 <- createStyle(bgFill="#F8CBAD", textDecoration = "bold")

#yellow
style3 <- createStyle(bgFill="#FFE699", textDecoration = "bold")

#green
style4 <-  createStyle(bgFill="#C6E0B4")

#grey
style5 <-  createStyle(bgFill="#D9D9D9")

#red
style6 <- createStyle(bgFill="#e84135")

#===============================
#CREATE WORKBOOK
#===============================

wb <- createWorkbook()
addWorksheet(wb, "med_at_Index")
writeData(wb, "med_at_Index", x=cohort, startCol=1, startRow=1, colNames=TRUE, rowNames=FALSE)

#===============================
#FORMAT CELLS
#===============================

democoln =  max(which(colnames(cohort) %in% c("DIAGNOSIS")))

rown = dim(cohort)[1]
coln = dim(cohort)[2]

conf = max(which(colnames(cohort) %in% toupper("Biologic_at_index")))

#column headers
conditionalFormatting(wb, "med_at_Index", cols=1:democoln, rows=1, rule="!=0", style = style1)
conditionalFormatting(wb, "med_at_Index", cols=(democoln+1):conf, rows=1, rule="!=0", style = style2)
conditionalFormatting(wb, "med_at_Index", cols=(conf+1):coln, rows=1, rule="!=0", style = style3)


conditionalFormatting(wb, "med_at_Index", cols = (conf+1):coln, rows = 2:rown, type = "contains", rule = "Possible", style = style5)
conditionalFormatting(wb, "med_at_Index", cols = (conf+1):coln, rows = 2:rown, type = "contains", rule = "Contradicts", style = style6)
conditionalFormatting(wb, "med_at_Index", cols = (conf+1):coln, rows = 2:rown, type = "contains", rule = "No", style = style6)
conditionalFormatting(wb, "med_at_Index", cols = (conf+1):coln, rows = 2:rown, type = "contains", rule = "Yes", style = style4)
conditionalFormatting(wb, "med_at_Index", cols = (conf-1), rows = 2:rown, type = "colourScale",  style = c("green", "yellow", "orange"))



#===============================
#SAVE REPORT
#===============================
saveWorkbook(wb, file = paste0(filename), overwrite = TRUE)
return(cohort)
}
