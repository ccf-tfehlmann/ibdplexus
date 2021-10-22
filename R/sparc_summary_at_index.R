#' sparc_summary
#'
#' Finds the medications the participant has been exposed to from the Smartform, lists other
#' Smartform data fields, and the encounter id or hist id at a specific
#' (“index”) date.
#'
#' @param datadir directory where unzipped data is located.
#' @param index_info A dataframe  with DEIDENTIFIED_MASTER_PATIENT_ID and a variable index_date.  Default is date of consent.
#' @param filename the name of the output file. Must be .xlsx.
#' @param index_range the number of days to look out from index date.
#' @param emr_codes the codes to search through in the extract
#'
#' @return A dataframe with the summary data and an excel file. If EMR codes are specified then a subset of those data are created for each domain where the code was found.
#' @export
sparc_summary <- function(datadir,
                             index_info = "DATE_OF_CONSENT",
                             filename = "SPARC_SUMMARY.xlsx",
                             index_range = "30",
                             emr_codes = NULL){


# LOAD FILES ----

  # GET FILES OF MOST RECENT DATA ----
  data = load_data(datadir = datadir, cohort = "SPARC", domains = c("Demographics", "Diagnosis", "Encounter", "Observation", "Prescriptions", "Procedures"), data_type = "BOTH")


# DEMOGRAPHIC INFORMATION ----

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

# CONVERT DAYS AFTER INDEX  TO NUMERIC VALUE ----

  t = as.numeric(index_range)

# DEFINE INDEX DATE ----

  if("DATE_OF_CONSENT" %in% index_info){cohort = demo %>% mutate(index_date = DATE_OF_CONSENT) } else {cohort = index_info %>% left_join(demo)}

# DIAGNOSIS ----


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


    # SMARTFORM & OTHER EMR DATA ----



    #ADD DIAGNOSIS YEAR ----


    dxy = data$diagnosis %>%
      filter(DATA_SOURCE == "SF_SPARC") %>%
      filter(DIAG_CONCEPT_NAME %in% c("Crohn's Disease", "IBD Unclassified", "Ulcerative Colitis","Inflammatory Bowel Disease")) %>%
      drop_na(DIAGNOSIS_DATE) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      slice(which.min(as.numeric(DIAGNOSIS_DATE))) %>%
      select(DEIDENTIFIED_MASTER_PATIENT_ID, DIAGNOSIS_DATE)

    cohort = left_join(cohort, dxy)



    #ADD RACE/ETHNICITY ----


    race = data$demographics %>%
      filter(DATA_SOURCE %in% c("ECRF_SPARC", "EMR")) %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DATA_SOURCE, RACE, ETHNICITY,RACE_OTHER) %>%
      rowwise() %>%
      mutate(s = sum(is.na(c_across(ETHNICITY:RACE_OTHER)))) %>%
      filter(s < 3) %>%
      arrange(DEIDENTIFIED_MASTER_PATIENT_ID, DATA_SOURCE) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      slice(1) %>%
      mutate(RACE = ifelse(RACE %in% c("Other", "Other Race") & !is.na(RACE_OTHER), RACE_OTHER, RACE)) %>%
      mutate(R = case_when(
        RACE %in% c("Declined", "Unable To Answer", "not reported", "Patient did not specify", "None Selected") ~ "Patient Declined",
        grepl("American Indian or Alaska Native", RACE, ignore.case = T) ~ "American Indian or Alaska Native",
        grepl("Asian|Indian", RACE, ignore.case = T) ~ "Asian",
        grepl("Black", RACE, ignore.case = T) ~ "Black Or African American",
        grepl("White", RACE, ignore.case = T) ~ "White",
        grepl("Unknown", RACE, ignore.case = T) ~ "Unknown",
        is.na(RACE) ~ as.character(NA),
        TRUE ~ "Other")) %>%
      mutate(R = ifelse(grepl(";|/", RACE) , "Mixed", R)) %>%
      mutate(E = case_when(grepl("Hispanic|Mexican|Cuban|Puerto", RACE, ignore.case = T) |
                             ETHNICITY %in% c("HISPANIC", "Hispanic or Latino") ~ "Hispanic or Latino",
                           !(grepl("Hispanic|Mexican|Cuban|Puerto", RACE, ignore.case = T)) &
                             ETHNICITY %in% c("NOT HISPANIC", "Not Hispanic or Latino") ~ "Not Hispanic or Latino",
                           TRUE ~ ETHNICITY)) %>%
      select(-RACE_OTHER) %>%
      dplyr::mutate(RACE = gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(R), perl=TRUE), ETHNICITY = gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(E), perl=TRUE)) %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID,  RACE, ETHNICITY)

    cohort = left_join(cohort, race)



    #PHENOTYPES: ----
    #  Crohn's Disease - just closest no date constraint if multiple before index date then keep worst one - don't make a call


    cdp =data$observations %>%
      filter(DATA_SOURCE == "SF_SPARC") %>%
      filter(OBS_TEST_CONCEPT_NAME %in% c("Crohn's Disease Phenotype", "IBD Manifestations - Abdominal Abscess, Fistula, or Other Penetrating Complication") | str_detect(OBS_TEST_CONCEPT_NAME, "^Phenotype")) %>%
      drop_na(DESCRIPTIVE_SYMP_TEST_RESULTS) %>%
      mutate(OBS_TEST_RESULT_DATE = dmy(OBS_TEST_RESULT_DATE)) %>%
      right_join(cohort) %>%
      mutate(diff = OBS_TEST_RESULT_DATE - index_date) %>%
      mutate(keep = case_when(DESCRIPTIVE_SYMP_TEST_RESULTS == "Inflammatory, non-penetrating, non-stricturing" & diff > t ~ "keep",
                              diff <= t ~ "keep")) %>%
      filter(keep == "keep") %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID,OBS_TEST_CONCEPT_NAME, index_date) %>%
      slice(which.min(abs(diff))) %>%
      ungroup() %>%
      pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, index_date), names_from = OBS_TEST_CONCEPT_NAME, values_from = DESCRIPTIVE_SYMP_TEST_RESULTS)



    cohort = cohort %>%
      left_join(cdp)



    #PHENOTYPES:
    #  Ulcerative Colitis- just closest no date constraint if multiple before index date then keep worst one

    ucp = data$observations %>%
      filter(DATA_SOURCE == "SF_SPARC")  %>%
      filter(OBS_TEST_CONCEPT_NAME %in% "Extent of Macroscopic Ulcerative Colitis" & (!is.na(DESCRIPTIVE_SYMP_TEST_RESULTS))) %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID,OBS_TEST_CONCEPT_NAME, DESCRIPTIVE_SYMP_TEST_RESULTS,OBS_TEST_CONCEPT_CODE ,OBS_TEST_RESULT_DATE) %>%
      mutate(OBS_TEST_RESULT_DATE = dmy(OBS_TEST_RESULT_DATE)) %>%
      left_join(cohort) %>%
      mutate(diff =  (OBS_TEST_RESULT_DATE)- index_date) %>%
      mutate(keep = case_when(DESCRIPTIVE_SYMP_TEST_RESULTS == "Ulcerative proctitis (rectum only)" & diff > t ~ "keep",
                              diff <= t ~ "keep")) %>%

      filter(keep == "keep") %>%
      arrange(DEIDENTIFIED_MASTER_PATIENT_ID, match(DESCRIPTIVE_SYMP_TEST_RESULTS, c("Pancolitis",
                                                                                     "Extensive ulcerative colitis (extends proximal to the splenic flexure)",
                                                                                     "Left-sided ulcerative colitis (distal to the splenic flexure only)",
                                                                                     "Ulcerative proctitis (rectum only)",
                                                                                     "Unknown"))) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID,index_date) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
      slice(which.min(abs(diff)))  %>%
      ungroup() %>%
      pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, index_date), names_from = OBS_TEST_CONCEPT_NAME, values_from = DESCRIPTIVE_SYMP_TEST_RESULTS)


    cohort = left_join(cohort, ucp)



    #PHENOTYPES:
    #  IBD-U- just closest no date constraint if multiple before index date then keep worst one


    ibdu = data$observations %>%
      filter(DATA_SOURCE == "SF_SPARC")  %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      filter(OBS_TEST_CONCEPT_CODE %in% c('EPIC#31000125051', 'EPIC#31000125052', 'EPIC#31000125053','EPIC#31000125054' , 'EPIC#31000125055', "SMART_Q61__C" ) & (!is.na(DESCRIPTIVE_SYMP_TEST_RESULTS))) %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID,OBS_TEST_CONCEPT_NAME, DESCRIPTIVE_SYMP_TEST_RESULTS,OBS_TEST_CONCEPT_CODE ,OBS_TEST_RESULT_DATE) %>%
      mutate(OBS_TEST_RESULT_DATE = dmy(OBS_TEST_RESULT_DATE)) %>%
      left_join(cohort) %>%
      mutate(diff =  (OBS_TEST_RESULT_DATE)- index_date) %>%
      mutate(keep = case_when(DESCRIPTIVE_SYMP_TEST_RESULTS == "Ulcerative proctitis (rectum only)" & diff > t ~ "keep",
                              diff <= t ~ "keep")) %>%
      filter(keep == "keep") %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
      slice(which.min(abs(diff))) %>%
      pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, index_date), names_from = OBS_TEST_CONCEPT_NAME, values_from = DESCRIPTIVE_SYMP_TEST_RESULTS)


    cohort = cohort %>%
      left_join(ibdu)


    #PHENOTYPES:
    #  UC Hospitalizations

    uch = data$observations %>% filter(DATA_SOURCE == "SF_SPARC")  %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      #filter(OBS_TEST_CONCEPT_NAME == "History of Hospitalization for severe Ulcerative Colitis") %>%
      filter(OBS_TEST_CONCEPT_CODE %in% c("EPIC#41201", "EPIC#41202", "11337-3", "SMART_Q60__C")& (!is.na(DESCRIPTIVE_SYMP_TEST_RESULTS))) %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID,  DESCRIPTIVE_SYMP_TEST_RESULTS, OBS_TEST_CONCEPT_CODE, OBS_TEST_RESULT_DATE) %>%
      arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_CONCEPT_CODE,desc(OBS_TEST_RESULT_DATE)) %>%
      left_join(cohort) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%

      mutate(diff =  dmy(OBS_TEST_RESULT_DATE)- index_date) %>%
      filter(diff <= t) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID,index_date) %>%
      slice(which.min(abs(diff))) %>%
      ungroup() %>%
      mutate('History of hospitalization for severe ulcerative colitis' = DESCRIPTIVE_SYMP_TEST_RESULTS,c=1) %>%
      left_join(dx) %>%
      filter(Diagnosis == "Ulcerative Colitis") %>%
      select(DEIDENTIFIED_MASTER_PATIENT_ID, 'History of hospitalization for severe ulcerative colitis', index_date)


    cohort = cohort %>%
      left_join(uch)




    #PHENOTYPES:
    #  IBDU Hospitalizations


    ibduh = data$observations %>% filter(DATA_SOURCE == "SF_SPARC")  %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      filter(OBS_TEST_CONCEPT_CODE %in% c("EPIC#31000125062", "EPIC#31000125063", "SMART_Q62__C")& (!is.na(DESCRIPTIVE_SYMP_TEST_RESULTS))) %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID,  DESCRIPTIVE_SYMP_TEST_RESULTS, OBS_TEST_CONCEPT_CODE, OBS_TEST_RESULT_DATE) %>%
      arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_CONCEPT_CODE,desc(OBS_TEST_RESULT_DATE)) %>%
      left_join(cohort) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%

      mutate(diff =  dmy(OBS_TEST_RESULT_DATE)- index_date) %>%
      filter(diff <= t) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID,index_date) %>%
      slice(which.min(abs(diff))) %>%
      ungroup() %>%
      mutate('History of hospitalization for severe IBD unclassified' = DESCRIPTIVE_SYMP_TEST_RESULTS,c=1) %>%
      left_join(dx) %>%
      filter(Diagnosis == "IBD Unclassified") %>%
      select(DEIDENTIFIED_MASTER_PATIENT_ID, 'History of hospitalization for severe IBD unclassified', index_date)

    cohort = cohort %>%
      left_join(ibduh)


    #DISEASE LOCATION - at index date ----


    location = data$observations %>%
      filter(DATA_SOURCE == "SF_SPARC") %>%
      filter(OBS_TEST_CONCEPT_NAME %in% c("Anal Phenotype",	"Duodenal Phenotype", "Esophageal Phenotype", "Gastric Phenotype",	"Ileal Phenotype", "Jejunal Phenotype", "Left Colonic Phenotype", "Rectal Phenotype", "Right Colonic Phenotype", "Transverse Colonic Phenotype")) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_CONCEPT_NAME) %>%
      drop_na(DESCRIPTIVE_SYMP_TEST_RESULTS) %>%
      mutate(OBS_TEST_RESULT_DATE = dmy(OBS_TEST_RESULT_DATE)) %>%
      right_join(cohort) %>%
      mutate(diff = OBS_TEST_RESULT_DATE - index_date) %>%
      mutate(keep = case_when(diff <= t ~ "keep")) %>%
      filter(keep == "keep") %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID,index_date, OBS_TEST_CONCEPT_NAME) %>%
      slice(which.min(abs(diff))) %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, OBS_TEST_CONCEPT_NAME, DESCRIPTIVE_SYMP_TEST_RESULTS) %>%
      mutate(Phenotype = case_when(DESCRIPTIVE_SYMP_TEST_RESULTS == "Yes" ~ "Yes",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "Anal inflammatory crohn's disease" ~ "Yes",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "Duodenal inflammatory crohn's disease" ~ "Yes",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "Esophageal inflammatory crohn's disease" ~ "Yes",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "Gastric inflammatory crohn's disease" ~ "Yes",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "Ileal inflammatory crohn's disease" ~ "Yes",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "Jejunal inflammatory crohn's disease" ~ "Yes",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "Left colonic inflammatory crohn's disease" ~ "Yes",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "Rectal inflammatory crohn's disease" ~ "Yes",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "Right colonic inflammatory crohn's disease" ~ "Yes",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "Transverse colonic inflammatory crohn's disease" ~ "Yes",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "No" ~ "No",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "No anal involvement" ~ "No",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "No rectal involvement" ~ "No",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "No right colonic involvement" ~ "No",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "No transverse colonic involvement" ~ "No",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "No duodenal involvement" ~ "No",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "No esophageal involvement" ~ "No",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "No gastric involvement" ~ "No",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "No ileal involvement" ~ "No",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "No jejunal involvement" ~ "No",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "No left colonic involvement" ~ "No",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "Unknown" ~ "Unknown",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "Unknown anal involvement" ~ "Unknown",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "Unknown rectal involvement" ~ "Unknown",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "Unknown right colonic involvement" ~ "Unknown",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "Unknown transverse colonic involvement" ~ "Unknown",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "Unknown duodenal involvement" ~ "Unknown",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "Unknown esophageal involvement" ~ "Unknown",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "Unknown gastric involvement" ~ "Unknown",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "Unknown ileal involvement" ~ "Unknown",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "Unknown jejunal involvement" ~ "Unknown",
                                   DESCRIPTIVE_SYMP_TEST_RESULTS == "Unknown left colonic involvement" ~ "Unknown",
                                   TRUE ~ DESCRIPTIVE_SYMP_TEST_RESULTS)) %>%
      pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, index_date), names_from = OBS_TEST_CONCEPT_NAME, values_from = Phenotype) %>%
      ungroup()


    cohort = left_join(cohort, location)



    #PERIANAL INVOLVEMENT ----


    perianal = data$observations %>%
      filter(DATA_SOURCE == "SF_SPARC") %>%
      filter(OBS_TEST_CONCEPT_NAME %in% c("Perianal Abcess",
                                          "Perianal Fistula",
                                          "Perianal Fistula - Complex Fistula",
                                          "Rectovaginal Fistula",
                                          "Anal Canal Ulcer",
                                          "Anal Canal Stricture",
                                          "Anal Fissure",
                                          "Large Skin Tags")) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_CONCEPT_NAME) %>%
      drop_na(DESCRIPTIVE_SYMP_TEST_RESULTS) %>%
      mutate(OBS_TEST_RESULT_DATE = dmy(OBS_TEST_RESULT_DATE)) %>%
      right_join(cohort) %>%
      mutate(diff = OBS_TEST_RESULT_DATE - index_date) %>%
      mutate(keep = case_when(diff <= t ~ "keep")) %>%
      filter(keep == "keep") %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID,index_date, OBS_TEST_CONCEPT_NAME) %>%
      slice(which.min(abs(diff))) %>%
      ungroup() %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, OBS_TEST_CONCEPT_NAME, DESCRIPTIVE_SYMP_TEST_RESULTS) %>%
      pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, index_date), names_from = OBS_TEST_CONCEPT_NAME, values_from = DESCRIPTIVE_SYMP_TEST_RESULTS) %>%
      ungroup()

    cohort = left_join(cohort, perianal)


    #SURGICAL HISTORY: -pull 0 back but otherwise only at enrollment ----
    # Number of IBD Surgeries



    surg = data$procedures %>%
      filter(DATA_SOURCE == "SF_SPARC") %>%
      filter(PROC_CONCEPT_NAME %in% c("IBD Surgeries")) %>%
      left_join(data$encounter) %>%
      drop_na(PHYSICIAN_NOTES_PROC_AVAIL) %>%
      mutate(VISIT_ENCOUNTER_START_DATE = dmy(VISIT_ENCOUNTER_START_DATE)) %>%
      mutate(`Number of IBD Surgeries` = PHYSICIAN_NOTES_PROC_AVAIL) %>%
      right_join(cohort) %>%
      mutate(diff = VISIT_ENCOUNTER_START_DATE - index_date) %>%
      mutate(keep = case_when(diff <= t ~ "keep",
                              `Number of IBD Surgeries` == "0" & diff > t ~ "keep")) %>%
      filter(keep == "keep") %>%
      arrange(match(`Number of IBD Surgeries`, c("greater than 5", "5", "4", "3", "2", "1", "0"))) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
      slice(which.min(abs(diff))) %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, `Number of IBD Surgeries`)

    cohort = left_join(cohort, surg)



    #SURGICAL HISTORY: -
    # Year of First IBD Surgery - year before or at time of enrollment


    surg_first = data$procedures %>%
      filter(DATA_SOURCE == "SF_SPARC") %>%
      filter(PROC_CONCEPT_NAME %in% c("IBD Surgeries")) %>%
      left_join(data$encounter) %>%
      filter(!is.na(PROC_START_DATE)|!is.na(PROC_END_DATE)) %>%
      mutate(`Year of Most Recent IBD Surgery` =  as.numeric(PROC_END_DATE),`Year of First IBD Surgery`= as.numeric(PROC_START_DATE)) %>%
      right_join(cohort) %>%
      mutate(keep_first = case_when(`Year of First IBD Surgery` <= year(index_date) ~ "keep"),
             diff = dmy(VISIT_ENCOUNTER_START_DATE) - index_date) %>%
      filter(keep_first == "keep") %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
      slice(which.min(abs(diff))) %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, `Year of First IBD Surgery`)

    cohort = left_join(cohort, surg_first)


    #SURGICAL HISTORY: -
    # Year of most recent IBD Surgery - year before  or at enrollment

    surg_recent = data$procedures %>%
      filter(DATA_SOURCE == "SF_SPARC") %>%
      filter(PROC_CONCEPT_NAME %in% c("IBD Surgeries")) %>%
      left_join(data$encounter) %>%
      filter(!is.na(PROC_START_DATE)|!is.na(PROC_END_DATE)) %>%
      mutate(`Year of Most Recent IBD Surgery` =  as.numeric(PROC_END_DATE),`Year of First IBD Surgery`= as.numeric(PROC_START_DATE)) %>%
      right_join(cohort) %>%
      mutate(keep_recent = case_when(`Year of Most Recent IBD Surgery` <= year(index_date) ~ "keep"),
             diff = dmy(VISIT_ENCOUNTER_START_DATE) - index_date) %>%
      filter(keep_recent == "keep") %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
      slice(which.min(abs(diff))) %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, `Year of Most Recent IBD Surgery`)


    cohort = left_join(cohort, surg_recent)




    #SURGICAL HISTORY:
    #   Esophogeal & Gastroduodenal


    eso =  data$procedures %>%
      filter(DATA_SOURCE == "SF_SPARC") %>%
      filter(PROC_CONCEPT_NAME %in% c("Esophageal Surgery","Gastroduodenal Surgery")) %>%
      left_join(data$encounter) %>%
      drop_na(PHYSICIAN_NOTES_PROC_AVAIL) %>%
      mutate(VISIT_ENCOUNTER_START_DATE = dmy(VISIT_ENCOUNTER_START_DATE)) %>%
      mutate(`Number of IBD Surgeries` = PHYSICIAN_NOTES_PROC_AVAIL) %>%
      right_join(cohort) %>%
      mutate(diff = VISIT_ENCOUNTER_START_DATE - index_date) %>%
      filter(diff <= t) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID, PROC_CONCEPT_NAME,index_date) %>%
      slice(which.min(abs(diff))) %>%
      ungroup() %>%
      pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, index_date), names_from = PROC_CONCEPT_NAME, values_from = PHYSICIAN_NOTES_PROC_AVAIL)

    cohort = left_join(cohort, eso)


    #SURGICAL HISTORY:
    #   Small bowel resection


    sbr = data$procedures %>% filter(DATA_SOURCE == "SF_SPARC")  %>%
      left_join(data$encounter) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      filter(PROC_CONCEPT_CODE %in% c("EPIC#41169", "SMART_Q79__C") | SRC_PROC_CONCEPT_CODE %in% c("EPIC#41169", "SMART_Q79__C")) %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, PROC_CONCEPT_NAME,	PROC_CONCEPT_CODE, SRC_PROC_CONCEPT_NAME,	SRC_PROC_CONCEPT_CODE,
               VISIT_ENCOUNTER_START_DATE, PHYSICIAN_NOTES_PROC_AVAIL) %>%
      left_join(cohort) %>%
      mutate(diff =  dmy(VISIT_ENCOUNTER_START_DATE)- index_date) %>%
      filter(diff <= t) %>%
      arrange(DEIDENTIFIED_MASTER_PATIENT_ID, PROC_CONCEPT_NAME,desc(VISIT_ENCOUNTER_START_DATE)) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID,index_date, PROC_CONCEPT_NAME) %>%
      slice(which.min(abs(diff))) %>%
      dplyr::rename('Small bowel resection' = PHYSICIAN_NOTES_PROC_AVAIL) %>%
      ungroup() %>%
      select(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, 'Small bowel resection')

    cohort = cohort %>%
      full_join(sbr)


    #SURGICAL HISTORY:
    #   Small bowel resection


    sbr_locations = data$procedures %>%
      filter(DATA_SOURCE == "SF_SPARC")  %>%
      left_join(data$encounter) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      filter(PROC_CONCEPT_NAME %in% c("Small Bowel Resection - Duodenum",
                                      "Small Bowel Resection - Jejunum",
                                      "Small Bowel Resection - Ileum")) %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, PROC_CONCEPT_NAME,	PROC_STATUS_CONCEPT_CODE, SRC_PROC_CONCEPT_NAME,	SRC_PROC_CONCEPT_CODE,
               VISIT_ENCOUNTER_START_DATE, PHYSICIAN_NOTES_PROC_AVAIL) %>%
      left_join(cohort) %>%
      mutate(diff =  dmy(VISIT_ENCOUNTER_START_DATE)- index_date) %>%
      filter(diff <= t) %>%
      arrange(DEIDENTIFIED_MASTER_PATIENT_ID, PROC_CONCEPT_NAME,desc(VISIT_ENCOUNTER_START_DATE)) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID,index_date, PROC_CONCEPT_NAME) %>%
      slice(which.min(abs(diff))) %>%
      ungroup() %>%
      pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, index_date), names_from = PROC_CONCEPT_NAME, values_from = PROC_STATUS_CONCEPT_CODE)

    cohort = cohort %>%
      full_join(sbr_locations)


    #SURGICAL HISTORY:
    #   Colon resection


    cr = data$procedures %>%
      filter(DATA_SOURCE == "SF_SPARC")  %>%
      left_join(data$encounter) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      filter(PROC_CONCEPT_NAME == "Colonic Resection") %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, PROC_CONCEPT_NAME,	PROC_CONCEPT_CODE, SRC_PROC_CONCEPT_NAME,	SRC_PROC_CONCEPT_CODE,
               VISIT_ENCOUNTER_START_DATE, PHYSICIAN_NOTES_PROC_AVAIL) %>%
      left_join(cohort) %>%
      mutate(diff =  dmy(VISIT_ENCOUNTER_START_DATE)- index_date) %>%
      filter(diff <= t) %>%
      arrange(DEIDENTIFIED_MASTER_PATIENT_ID, PROC_CONCEPT_NAME,desc(VISIT_ENCOUNTER_START_DATE)) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID,index_date, PROC_CONCEPT_NAME) %>%
      slice(which.min(abs(diff))) %>%
      dplyr::rename('Colon resection' = PHYSICIAN_NOTES_PROC_AVAIL) %>%
      ungroup() %>%
      select(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, 'Colon resection')

    cohort = cohort %>%
      full_join(cr)


    #SURGICAL HISTORY:
    #   Colon Resection Locations


    cr_locations = data$procedures %>%
      filter(DATA_SOURCE == "SF_SPARC")  %>%
      left_join(data$encounter) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      filter(PROC_CONCEPT_NAME %in% c("Colon Resection - Cecum",
                                      "Colon Resection - Ascending colon",
                                      "Colon Resection - Transverse colon",
                                      "Colon Resection - Descending colon",
                                      "Colon Resection - Sigmoid",
                                      "Colon Resection - Rectum")) %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, PROC_CONCEPT_NAME,	PROC_STATUS_CONCEPT_CODE, SRC_PROC_CONCEPT_NAME,	SRC_PROC_CONCEPT_CODE,
               VISIT_ENCOUNTER_START_DATE, PHYSICIAN_NOTES_PROC_AVAIL) %>%
      left_join(cohort) %>%
      mutate(diff =  dmy(VISIT_ENCOUNTER_START_DATE)- index_date) %>%
      filter(diff <= t) %>%
      arrange(DEIDENTIFIED_MASTER_PATIENT_ID, PROC_CONCEPT_NAME,desc(VISIT_ENCOUNTER_START_DATE)) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID,index_date, PROC_CONCEPT_NAME) %>%
      slice(which.min(abs(diff))) %>%
      ungroup() %>%
      pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, index_date), names_from = PROC_CONCEPT_NAME, values_from = PROC_STATUS_CONCEPT_CODE)

    cohort = cohort %>%
      full_join(cr_locations)


    #SURGICAL HISTORY:
    #   Complete colectomy


    cc = data$procedures %>%
      filter(DATA_SOURCE == "SF_SPARC")  %>%
      left_join(data$encounter) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      filter(PROC_CONCEPT_CODE %in% c("EPIC#41172", "EPIC#17655", "SMART_Q90__C")| SRC_PROC_CONCEPT_CODE %in% c("EPIC#41172", "EPIC#17655", "SMART_Q90__C")) %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID,  PROC_CONCEPT_CODE, PROC_STATUS_CONCEPT_CODE, SRC_PROC_CONCEPT_NAME,	SRC_PROC_CONCEPT_CODE,
               PROC_STATUS_CONCEPT_NAME, VISIT_ENCOUNTER_START_DATE, PROC_START_DATE,        INDICATION) %>%
      arrange(DEIDENTIFIED_MASTER_PATIENT_ID, desc(VISIT_ENCOUNTER_START_DATE)) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      mutate(PROC_STATUS_CONCEPT_NAME = case_when(
        PROC_STATUS_CONCEPT_NAME == "Yes" ~ "Performed",
        PROC_STATUS_CONCEPT_NAME == "No" ~ "Not Performed",
        TRUE ~ as.character(PROC_STATUS_CONCEPT_NAME))) %>%
      left_join(cohort) %>%
      mutate(diff =  dmy(VISIT_ENCOUNTER_START_DATE)- index_date) %>%
      filter(diff <= t) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID,index_date) %>%
      slice(which.min(abs(diff))) %>%
      ungroup() %>%
      mutate('Complete colectomy' = PROC_STATUS_CONCEPT_CODE,
             `Year of Complete Colectomy` = as.numeric(PROC_START_DATE),
             `Indication for total colectomy` = INDICATION) %>%
      select(DEIDENTIFIED_MASTER_PATIENT_ID, index_date,  'Complete colectomy',`Year of Complete Colectomy`,`Indication for total colectomy`)

    cohort = cohort %>%
      left_join(cc)


    #SURGICAL HISTORY:
    #   Complete Pouch, Ostomy, Strictoplasty,


    surg2 = data$procedures %>%
      filter(DATA_SOURCE == "SF_SPARC") %>%
      filter(PROC_CONCEPT_NAME %in% c("Stricturoplasty","Ileostomy/Colostomy", "J-Pouch")) %>%
      left_join(data$encounter) %>%
      filter(!is.na(PHYSICIAN_NOTES_PROC_AVAIL)| !is.na(PROC_STATUS_CONCEPT_NAME)) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID, PROC_CONCEPT_NAME) %>%
      mutate(VISIT_ENCOUNTER_START_DATE = dmy(VISIT_ENCOUNTER_START_DATE)) %>%
      left_join(cohort) %>%
      mutate(diff =  (VISIT_ENCOUNTER_START_DATE)- index_date) %>%
      filter(diff <= t) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID,PROC_CONCEPT_NAME,index_date) %>%
      slice(which.min(abs(diff))) %>%
      mutate(response = ifelse(is.na(PHYSICIAN_NOTES_PROC_AVAIL), PROC_STATUS_CONCEPT_NAME, PHYSICIAN_NOTES_PROC_AVAIL)) %>%
      mutate(response = case_when(response %in% c("Performed") ~ "Yes",
                                  response %in% c("Not Performed","No Previous Ostomy", "No Previous Pouch", "No Previous Blind Pouch") ~ "No",
                                  response %in% c("Colostomy Present",  "Ileostomy Present", "Ostomy Present", "Pouch Present", "Blind Pouch Present") ~ "Present",
                                  response %in% c("Prior Ostomy", "Prior Blind Pouch", "Prior Pouch") ~ "Prior",
                                  response %in% c("unknown", "Unknown") ~ "Unknown",
                                  TRUE ~ response)) %>%
      ungroup() %>%
      pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, index_date), names_from = PROC_CONCEPT_NAME, values_from = response)

    cohort = left_join(cohort, surg2)



    #SURGICAL HISTORY:
    #   Short Gut


    shortgut = data$diagnosis %>%
      filter(DATA_SOURCE == "SF_SPARC") %>%
      filter(DIAG_CONCEPT_NAME %in% c("Physiological Short Gut Syndrome")) %>%
      left_join(data$encounter) %>%
      filter(!is.na(DIAG_STATUS_CONCEPT_NAME)) %>%
      mutate(VISIT_ENCOUNTER_START_DATE = dmy(VISIT_ENCOUNTER_START_DATE)) %>%
      left_join(cohort) %>%
      mutate(diff =  (VISIT_ENCOUNTER_START_DATE)- index_date) %>%
      filter(diff <= t) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID,DIAG_CONCEPT_NAME,index_date) %>%
      slice(which.min(abs(diff))) %>%
      ungroup() %>%
      pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, index_date), names_from = DIAG_CONCEPT_NAME, values_from = DIAG_STATUS_CONCEPT_NAME)

    cohort = left_join(cohort, shortgut)



    #SMOKING STATUS ----


    tobacco = data$observations %>%
      filter(DATA_SOURCE == "SF_SPARC") %>%
      filter(grepl("Tobacco", OBS_TEST_CONCEPT_NAME)) %>%
      filter(!is.na(DESCRIPTIVE_SYMP_TEST_RESULTS)) %>%
      mutate(OBS_TEST_RESULT_DATE = dmy(OBS_TEST_RESULT_DATE)) %>%
      left_join(cohort) %>%
      mutate(diff =  (OBS_TEST_RESULT_DATE)- index_date) %>%
      filter(diff <= t) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID,OBS_TEST_CONCEPT_NAME,index_date) %>%
      slice(which.min(abs(diff))) %>%
      ungroup() %>%
      pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, index_date), names_from = OBS_TEST_CONCEPT_NAME, values_from = DESCRIPTIVE_SYMP_TEST_RESULTS)

    cohort = left_join(cohort, tobacco)






    #Narcotic Use ---


    narcotic = data$observations %>%
      filter(DATA_SOURCE == "SF_SPARC") %>%
      filter(grepl("narcotic", OBS_TEST_CONCEPT_NAME, ignore.case =T)) %>%
      filter(!is.na(DESCRIPTIVE_SYMP_TEST_RESULTS)) %>%
      mutate(OBS_TEST_RESULT_DATE = dmy(OBS_TEST_RESULT_DATE)) %>%
      left_join(cohort) %>%
      mutate(diff =  (OBS_TEST_RESULT_DATE)- index_date) %>%
      filter(diff <= t) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID,OBS_TEST_CONCEPT_NAME,index_date) %>%
      slice(which.min(abs(diff))) %>%
      ungroup() %>%
      pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, index_date), names_from = OBS_TEST_CONCEPT_NAME, values_from = DESCRIPTIVE_SYMP_TEST_RESULTS)

    cohort = left_join(cohort, narcotic)


    #EIMS ---


    eim_codes = eim_codes %>% mutate(SRC_DIAG_CONCEPT_CODE = DIAG_CONCEPT_CODE)

    EIMS = data$diagnosis %>%
      filter(DATA_SOURCE == "SF_SPARC") %>%
      left_join(data$encounter) %>%
      filter(DIAG_CONCEPT_NAME %in% eim_codes$EIM_DX | SRC_DIAG_CONCEPT_CODE %in% eim_codes$DIAG_CONCEPT_CODE | DIAG_CONCEPT_CODE %in% eim_codes$DIAG_CONCEPT_CODE) %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DIAG_CONCEPT_CODE, DIAG_CONCEPT_NAME, DIAG_STATUS_CONCEPT_NAME, VISIT_ENCOUNTER_START_DATE, SRC_DIAG_CONCEPT_CODE) %>%
      left_join(cohort) %>%
      mutate(diff =  dmy(VISIT_ENCOUNTER_START_DATE)- index_date) %>%
      filter(diff <= t) %>%
      arrange(DEIDENTIFIED_MASTER_PATIENT_ID, DIAG_CONCEPT_NAME,desc(VISIT_ENCOUNTER_START_DATE)) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID,DIAG_CONCEPT_NAME,index_date) %>%
      slice(which.min(abs(diff))) %>%
      mutate(NEW_VALUE = ifelse(DIAG_STATUS_CONCEPT_NAME=="Yes", "Yes",
                                ifelse(DIAG_STATUS_CONCEPT_NAME=="Unknown", "Unknown",
                                       ifelse(DIAG_STATUS_CONCEPT_NAME=="No", "No", as.character(NA))))) %>%
      select(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, DIAG_CONCEPT_NAME, NEW_VALUE) %>%
      pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, index_date), names_from = DIAG_CONCEPT_NAME, values_from = NEW_VALUE)

    cohort = cohort %>%
      left_join(EIMS)




    # Cancer History
    # History of Colorectal Cancer
    # History of IBD-associated dysplasia
    # History of Cervical Cancer
    # History of Melanoma



    cancer = data$diagnosis %>%
      filter(DATA_SOURCE == "SF_SPARC") %>%
      filter(DIAG_CONCEPT_NAME %in% c("Intestinal Dysplasia", "Malignant Neoplasm of Colon, Unspecified", "Personal History of Cervical Dysplasia", "Skin Cancer")) %>%
      drop_na(DIAG_STATUS_CONCEPT_NAME) %>%
      left_join(data$encounter) %>%
      right_join(cohort) %>%
      mutate(diff =  dmy(VISIT_ENCOUNTER_START_DATE)- index_date) %>%
      filter(diff <= t) %>%
      arrange(DEIDENTIFIED_MASTER_PATIENT_ID, DIAG_CONCEPT_NAME,desc(VISIT_ENCOUNTER_START_DATE)) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID,DIAG_CONCEPT_NAME, index_date) %>%
      slice(which.min(abs(diff))) %>%
      pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, index_date), names_from = DIAG_CONCEPT_NAME, values_from = DIAG_STATUS_CONCEPT_NAME)

    cohort = left_join(cohort, cancer)




# MEDICATION ----



       #MEDICATION EXPOSURE UP UNTIL INDEX DATE FROM SMARTFORM ----


       #if never can pull back

       sf_med = data$prescriptions %>%
         filter(DATA_SOURCE == "SF_SPARC") %>%
         left_join(data$encounter) %>%
         mutate(VISIT_ENCOUNTER_START_DATE = dmy(VISIT_ENCOUNTER_START_DATE)) %>%
         left_join(cohort) %>%
         drop_na(index_date) %>%
         group_by(DEIDENTIFIED_MASTER_PATIENT_ID,MEDICATION_NAME, index_date) %>%
         mutate(diff = (VISIT_ENCOUNTER_START_DATE - index_date)) %>%
         mutate(keep = case_when(MEDICATION_ADMINISTRATED == "Never" & diff > t ~ "keep",
                                 diff <= t ~ "keep")) %>%
         filter(keep == "keep") %>%
         slice(which.min(abs(diff))) %>%
         ungroup() %>%
         pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, index_date), names_from = MEDICATION_NAME, values_from = MEDICATION_ADMINISTRATED) %>%
         select(sort(names(.))) %>%
         select(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, everything()) %>%
         dplyr::rename_at(vars(-DEIDENTIFIED_MASTER_PATIENT_ID, -index_date), ~ paste0(., '_SF'))



         cohort = left_join(cohort, sf_med)



      #REORDER COLUMNS

      cohort <- cohort %>% arrange(DEIDENTIFIED_MASTER_PATIENT_ID, index_date)



      #DATA FROM EMR - encounter ids for where variable is reported for master report



      if(is.null(emr_codes) == FALSE){


        emr = NULL

        for (i in 1:length(data)){
          if("DATA_SOURCE" %in% names(data[[i]])){k = data[[i]] %>% filter(DATA_SOURCE == "EMR")} else{k = NULL}

          emr[[i]] = k}

        names(emr) = names(data)

       # emr$patient_history_old = NULL

        emr = Filter(length, emr)
        emr = emr[sapply(emr, nrow)>0]

        emr.set = as.vector(emr_codes)

        e = cohort %>% distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date)

        for (j in 1:length(emr.set)){

          code = emr.set[j]

          f=NULL

          for (k in 1:length(emr)){
            keep = emr[[k]] %>%
              right_join(e)

            #keep$c = apply(keep, 1, function(r) any(r %in% c(code)))

            keep$c =   apply(keep %>% select(contains("CODE")), 1, function(r) any(r %in% c(code)))

            keep = keep %>%  filter(c == 1)

            f[[k]] = keep
          }

          rm(keep)

          names(f) = names(emr)

          f = Filter(length, f)
          f = f[sapply(f, nrow)>0]

          g = NULL

          if(length(f) != 0){
          for(m in 1:length(f)){

            if("patient_history" %in% names(f[m])){

              keep = f[[m]] %>%
                filter(HISTORY_TYPE != "FAMILY") %>%
                mutate(VAR =  paste0("PATIENT_HISTORY_", HISTORY_CONCEPT_NAME)) %>%
                mutate(RESPONSE = "Patient History") %>%
                mutate(diff = dmy(EVENT_ONSET_DATE) - index_date) %>%
                filter(diff <= t) %>%
                arrange(DEIDENTIFIED_MASTER_PATIENT_ID, VAR, RESPONSE) %>%
                group_by(DEIDENTIFIED_MASTER_PATIENT_ID,VAR, index_date) %>%
                slice(which.min(abs(diff))) %>%
                ungroup() %>%
                pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, index_date), names_from = VAR, values_from = HIST_ID)


            } else if("prescriptions" %in% names(f[m])){

              keep = f[[m]] %>%
                #mutate(diff = dmy(MED_START_DATE) - index_date) %>%
                distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date,MEDICATION_NAME,VISIT_ENCOUNTER_ID, MED_START_DATE, MED_END_DATE) %>%
                ungroup() %>%
                mutate(VAR = paste0("Prescriptions_", MEDICATION_NAME)) %>%
                reshape2::dcast(DEIDENTIFIED_MASTER_PATIENT_ID + index_date ~ VAR ,
                      value.var = "VISIT_ENCOUNTER_ID",  fun.aggregate = function(x) paste(x, collapse = "; "))



            } else if("diagnosis" %in% names(f[m])){

              keep = f[[m]] %>%
                distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date,DIAG_CONCEPT_NAME,VISIT_ENCOUNTER_ID) %>%
                ungroup() %>%
                mutate(VAR = paste0("DIAGNOSIS_", DIAG_CONCEPT_NAME)) %>%
                reshape2::dcast(DEIDENTIFIED_MASTER_PATIENT_ID + index_date ~ VAR ,
                      value.var = "VISIT_ENCOUNTER_ID",  fun.aggregate = function(x) paste(x, collapse = "; "))

            }else if("patient_problem" %in% names(f[m])){

              keep = f[[m]] %>%
                distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date,PROB_CONCEPT_NAME,VISIT_ENCOUNTER_ID) %>%
                ungroup() %>%
                mutate(VAR = paste0("Patient_Problem_", PROB_CONCEPT_NAME)) %>%
                reshape2::dcast(DEIDENTIFIED_MASTER_PATIENT_ID + index_date ~ VAR ,
                      value.var = "VISIT_ENCOUNTER_ID",  fun.aggregate = function(x) paste(x, collapse = "; "))
              }else if("labs" %in% names(f[m])){

              keep = f[[m]] %>%
                distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date,LAB_TEST_CONCEPT_NAME,VISIT_ENCOUNTER_ID) %>%
                ungroup() %>%
                mutate(VAR = paste0("Labs_LOINC_", code)) %>%
                reshape2::dcast(DEIDENTIFIED_MASTER_PATIENT_ID + index_date ~ VAR ,
                      value.var = "VISIT_ENCOUNTER_ID",  fun.aggregate = function(x) paste(x, collapse = "; "))
              }else if("procedures" %in% names(f[m])){

                        keep = f[[m]] %>%
                          distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date,PROC_CONCEPT_NAME,VISIT_ENCOUNTER_ID) %>%
                          ungroup() %>%
                          mutate(VAR = paste0("Procedures_CPT_", code)) %>%
                          reshape2::dcast(DEIDENTIFIED_MASTER_PATIENT_ID + index_date ~ VAR ,
                                          value.var = "VISIT_ENCOUNTER_ID",  fun.aggregate = function(x) paste(x, collapse = "; "))}

            g[[m]] = keep


          }

          g = reduce(g, full_join)

          cohort = left_join(cohort, g)}

        }

      }else{cohort = cohort}




      #CREATE EMR SUBSET OF DATA FOR THESE ENCOUNTERS



      if(is.null(emr_codes) == FALSE){


        emr_subset = NULL

        for (i in 1:length(data)){
          if("DATA_SOURCE" %in% names(data[[i]])){
            k = data[[i]] %>% filter(DATA_SOURCE == "EMR")

            k$c =   apply(k %>% select(contains("CODE")), 1, function(r) any(r %in% emr_codes))

            k = k %>%  filter(c == 1)



          } else{k = NULL}

          emr_subset[[i]] = k}

        names(emr_subset) = names(data)

        #emr_subset$patient_history_old = NULL

        emr_subset = Filter(length, emr_subset)
        emr_subset = emr_subset[sapply(emr_subset, nrow)>0]


        for(j in 1:length(emr_subset)){

          keep = emr_subset[[j]]

          write.csv(keep, file = paste0(names(emr_subset[j]),".csv"), na = "", row.names = F)

        }


      }else{emr_subset = NULL}




# FORMAT COLUMN NAMES  ----

  cohort <- cohort %>% arrange(DEIDENTIFIED_MASTER_PATIENT_ID, index_date)


  names(cohort) = toupper(names(cohort))

  cohort$DATE_OF_CONSENT_WITHDRAWN = dmy(cohort$DATE_OF_CONSENT_WITHDRAWN)

# CREATE HEADER STYLES ----

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


# CREATE WORKBOOK ----

  wb <- createWorkbook()
  addWorksheet(wb, "sparc_summary")
  writeData(wb, "sparc_summary", x=cohort, startCol=1, startRow=1, colNames=TRUE, rowNames=FALSE)

# FORMAT CELLS ----

democoln =  max(which(colnames(cohort) %in% "INDEX_DATE"))

rown = dim(cohort)[1]
coln = dim(cohort)[2]


#column headers
conditionalFormatting(wb, "sparc_summary", cols=1:coln, rows=1, rule="!=0", style = style1)





# SAVE REPORT ----

saveWorkbook(wb, file = paste0(filename), overwrite = TRUE)

return(cohort)
}
