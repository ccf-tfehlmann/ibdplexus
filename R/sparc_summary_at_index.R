#' sparc_summary
#'
#' Finds the medications the participant is prescribed from the electronic
#' medical record, patient reported case report forms and Smartform, lists other
#' Smartform data fields, calculates diagnosis, phenotype, disease, and
#' endoscopy scores and lists the encounter id or hist id at a specific
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
  data = load_data(datadir = datadir, cohort = "SPARC", domains = "ALL", data_type = "BOTH")


# DEMOGRAPHIC INFORMATION ----

  demo = data$demographics %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DATE_OF_CONSENT, DATE_OF_CONSENT_WITHDRAWN, BIRTH_YEAR, GENDER) %>%
    mutate(DATE_OF_CONSENT = dmy(DATE_OF_CONSENT)) %>%
    filter(year(DATE_OF_CONSENT) >= 2016) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    slice(which.min(DATE_OF_CONSENT)) %>%
    ungroup()


# CONVERT DAYS AFTER INDEX  TO NUMERIC VALUE ----

  t = as.numeric(index_range)

# DEFINE INDEX DATE ----

  if("DATE_OF_CONSENT" %in% index_info){cohort = demo %>% mutate(index_date = DATE_OF_CONSENT) } else {cohort = index_info %>% left_join(demo)}

# DIAGNOSIS ----


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
      filter(DATA_SOURCE == "EMR") %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, RACE, ETHNICITY)

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



  # DEFINE MEDICATION GROUPS  ----


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


  # MEDICATION INFORMATION FROM OBSERVATION TABLE ----


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


    # MEDICATIONS FROM ECRF ----

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


    # MEDICATIONS FROM EMR ----

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


     # COMPARE SOURCES FOR MEDICATIONS ----

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

  # BIOLOGICS ----



     # BIOLOGICS FROM ECRF

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


     #BIOLOGICS FROM EMR

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



     # COMPARE SOURCES FOR BIOLOGICS

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



     # Biologic at Index


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



    # FIND IF FIRST TIME ON BIOLOGIC (LOADING DATE) ----



     #EMR



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


     #ECRF


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


  # NO MEDICATION AT INDEX DATE ----

    no_med_index = cohort %>%
      filter_at(vars(ends_with("_status")), all_vars(. %in% c("No", "", NA))) %>%
      mutate(No_IBD_Medication_At_Index = 1) %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, No_IBD_Medication_At_Index)

     cohort = left_join(cohort, no_med_index)



# SCORES ----

      #sCDAI

      #Smartform

      scdai_sf = data$observations %>%
        filter(DATA_SOURCE == "SF_SPARC") %>%
        filter(grepl("Abdominal Pain|General Well|Number of Daily Bowel Movements", OBS_TEST_CONCEPT_NAME, ignore.case = T)) %>%
        mutate(OBS_TEST_RESULT_DATE = dmy(OBS_TEST_RESULT_DATE)) %>%
        filter(OBS_TEST_CONCEPT_NAME != "Baseline Number of Daily Bowel Movements") %>%
        mutate(result = ifelse(is.na(DESCRIPTIVE_SYMP_TEST_RESULTS), TEST_RESULT_NUMERIC, DESCRIPTIVE_SYMP_TEST_RESULTS)) %>%
        mutate(result = case_when(result == "None" ~ 0,
                                  result == "Mild" ~ 1,
                                  result == "Moderate" ~ 2,
                                  result == "Severe" ~ 3,
                                  result == "Generally well" ~ 0,
                                  result == "Slightly under par" ~ 1,
                                  result == "Poor" ~ 2,
                                  result == "Very poor" ~ 3,
                                  result == "Terrible" ~ 4,
                                  TRUE ~ as.numeric(result))) %>%
        distinct(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE,OBS_TEST_CONCEPT_NAME,result) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE,OBS_TEST_CONCEPT_NAME) %>%
        arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_CONCEPT_NAME, desc(result)) %>%
        slice(1) %>%
        pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE), names_from = c(OBS_TEST_CONCEPT_NAME), values_from = c(result)) %>%
        mutate(A = ifelse(is.na(`Abdominal Pain - Pain Scale`), `Abdominal Pain Score`, `Abdominal Pain - Pain Scale`),
               B = ifelse(is.na(`Current Average Number of Daily Bowel Movements`), `Current Maximum Number of Daily Bowel Movements`, `Current Average Number of Daily Bowel Movements`),
               G = ifelse(is.na(`Constitutional - General Well-Being`), `Constitutional- General Well-Being`, `Constitutional - General Well-Being`),
               G = ifelse(is.na(G), `General Well Being Score`, G)) %>%
        ungroup() %>%
        arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
        mutate(diff = OBS_TEST_RESULT_DATE- lag(OBS_TEST_RESULT_DATE)) %>%
        mutate(diff = if_else(is.na(diff), 0, as.numeric(diff))) %>%
        ungroup() %>%
        mutate(Daily.BM.Question = case_when(B == `Current Maximum Number of Daily Bowel Movements` ~ "Current Maximum Number of Daily Bowel Movements",
                                             B == `Current Average Number of Daily Bowel Movements` ~ "Current Average Number of Daily Bowel Movements",
                                             TRUE ~ as.character(NA))) %>%
        mutate(A2 = A, B2 = B, G2 = G, DBQ2 = Daily.BM.Question) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
        fill(A2, .direction="downup") %>%
        fill(B2, .direction="downup") %>%
        fill(G2, .direction="downup") %>%
        fill(DBQ2, .direction = "downup") %>%
        mutate(A = ifelse(is.na(A) & diff <= 7,A2, A),
               G = ifelse(is.na(G) & diff <= 7, G2, G),
               B = ifelse(is.na(B) & diff <= 7, B2, B),
               Daily.BM.Question = ifelse(is.na(Daily.BM.Question) & diff <= 7, DBQ2, Daily.BM.Question)) %>%
        select(-A2, -B2, -G2,-diff,-DBQ2) %>%
        dplyr::rename(Daily.BM = B, Abdominal.Pain.Score = A, General.well.being.score = G) %>%
        mutate(sCDAI.score = 44+(2*7*Daily.BM)+(5*7*Abdominal.Pain.Score)+(7*7*General.well.being.score), Source = "SF") %>%
        dplyr::rename(sCDAI.date = OBS_TEST_RESULT_DATE) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, sCDAI.date, sCDAI.score, Source, Daily.BM, Abdominal.Pain.Score, General.well.being.score,Daily.BM.Question)


      #ECRF
      scdai_ecrf = data$observations %>%
        filter(DATA_SOURCE == "ECRF_SPARC") %>%
        filter(grepl("Abdominal Pain|General Well|Bowel Movements", OBS_TEST_CONCEPT_NAME, ignore.case = T)) %>%
        mutate(OBS_TEST_RESULT_DATE = dmy(OBS_TEST_RESULT_DATE)) %>%
        filter(OBS_TEST_CONCEPT_NAME != "Baseline Number of Daily Bowel Movements") %>%
        mutate(result = ifelse(is.na(DESCRIPTIVE_SYMP_TEST_RESULTS), TEST_RESULT_NUMERIC, DESCRIPTIVE_SYMP_TEST_RESULTS)) %>%
        mutate(result = case_when(result == "None" ~ 0,
                                  result == "Mild" ~ 1,
                                  result == "Moderate" ~ 2,
                                  result == "Severe" ~ 3,
                                  result == "Generally well" ~ 0,
                                  result == "Slightly under par" ~ 1,
                                  result == "Poor" ~ 2,
                                  result == "Very poor" ~ 3,
                                  result == "Terrible" ~ 4,
                                  result == "20+" ~ 20,
                                  TRUE ~ as.numeric(result))) %>%
        distinct(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE,OBS_TEST_CONCEPT_NAME,result) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE,OBS_TEST_CONCEPT_NAME) %>%
        arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_CONCEPT_NAME, desc(result)) %>%
        slice(1) %>%
        pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE), names_from = c(OBS_TEST_CONCEPT_NAME), values_from = c(result)) %>%
        mutate(aug18 = ifelse(OBS_TEST_RESULT_DATE < as.Date("01-AUG-2018", format = "%d-%b-%Y"),"before","after")) %>%
        mutate(A = `Abdominal Pain`,
               B = if_else(aug18=="before" & !is.na(`Current Average Number of Daily Bowel Movements`), `Current Average Number of Daily Bowel Movements`, `Current Average Number of Daily Liquid Bowel Movements`),
               B = ifelse(is.na(B),`Current Average Number of Daily Bowel Movements`, B),
               G = `General Well-Being`) %>%
        ungroup() %>%
        mutate(Daily.BM.Question = case_when(B == `Current Average Number of Daily Liquid Bowel Movements` ~ "Current Average Number of Daily Liquid Bowel Movements",
                                             B == `Current Average Number of Daily Bowel Movements` ~ "Current Average Number of Daily Bowel Movements",
                                             TRUE ~ as.character(NA))) %>%
        arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
        mutate(diff = OBS_TEST_RESULT_DATE- lag(OBS_TEST_RESULT_DATE)) %>%
        mutate(diff = if_else(is.na(diff), 0, as.numeric(diff))) %>%
        ungroup() %>%
        mutate(A2 = A, B2 = B, G2 = G, DBQ2 = Daily.BM.Question) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
        fill(A2, .direction="downup") %>%
        fill(B2, .direction="downup") %>%
        fill(G2, .direction="downup") %>%
        fill(DBQ2, .direction = "downup") %>%
        mutate(A = ifelse(is.na(A) & diff <= 7,A2, A),
               G = ifelse(is.na(G) & diff <= 7, G2, G),
               B = ifelse(is.na(B) & diff <= 7, B2, B),
               Daily.BM.Question = ifelse(is.na(Daily.BM.Question) & diff <= 7, DBQ2, Daily.BM.Question)) %>%
        select(-A2, -B2, -G2,-diff,-DBQ2) %>%
        dplyr::rename(Daily.BM = B, Abdominal.Pain.Score = A, General.well.being.score = G) %>%
        mutate(sCDAI.score = 44+(2*7*Daily.BM)+(5*7*Abdominal.Pain.Score)+(7*7*General.well.being.score), Source = "ECRF") %>%
        dplyr::rename(sCDAI.date = OBS_TEST_RESULT_DATE) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, sCDAI.date, sCDAI.score, Source, Daily.BM, Abdominal.Pain.Score, General.well.being.score,Daily.BM.Question)



      sCDAI = scdai_sf %>%  bind_rows(scdai_ecrf) %>%
        arrange(DEIDENTIFIED_MASTER_PATIENT_ID, desc(sCDAI.score), sCDAI.date) %>%
        dplyr::rename(scdai.source = Source)  %>% distinct_all() %>%
        mutate(Daily.BM.Version = case_when(Daily.BM.Question == "Current Average Number of Daily Bowel Movements" ~ 1,
                                            Daily.BM.Question == "Current Average Number of Daily Liquid Bowel Movements" ~ 2,
                                            Daily.BM.Question == "Current Maximum Number of Daily Bowel Movements" ~ 3)) %>%
        right_join(cohort) %>%
        mutate(datediff = abs(sCDAI.date - index_date)) %>%
        filter(Diagnosis == "Crohn's Disease" & datediff <= t) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
        arrange(DEIDENTIFIED_MASTER_PATIENT_ID, desc(sCDAI.score), datediff) %>%
        slice(1) %>%
        distinct(DEIDENTIFIED_MASTER_PATIENT_ID, sCDAI.date, sCDAI.score, index_date, scdai.source, Daily.BM, Daily.BM.Version,Abdominal.Pain.Score, General.well.being.score) %>%
        ungroup() %>%
        select(sort(names(.))) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, everything())



      cohort = left_join(cohort, sCDAI)




      #6/9 point Mayo/ucdai


      #Smartform

      ucdai_sf = data$observations %>%
        filter(DATA_SOURCE == "SF_SPARC") %>%
        filter(grepl("Stool Frequency|Blood in Stool|Global Assessment", OBS_TEST_CONCEPT_NAME, ignore.case = T)) %>%
        mutate(OBS_TEST_RESULT_DATE = dmy(OBS_TEST_RESULT_DATE)) %>%
        mutate(result = ifelse(is.na(DESCRIPTIVE_SYMP_TEST_RESULTS), TEST_RESULT_NUMERIC, DESCRIPTIVE_SYMP_TEST_RESULTS)) %>%
        mutate(result = case_when(result == "Normal" ~ 0,
                                  result == "1-2 stools/day more than normal" ~ 1,
                                  result == "3-4 stools/day more than normal" ~ 2,
                                  result %in% c(">4 stools/day more than normal", "&gt;4 stools/day more than normal") ~ 3,
                                  result == "None" ~ 0,
                                  result == "Visible blood in stool less than half the time" ~ 1,
                                  result == "Visible blood in stool half of the time or more" ~ 2,
                                  result == "Passing blood alone" ~ 3,
                                  result == "Quiescent" ~ 0,
                                  result == "Mild" ~ 1,
                                  result == "Moderate" ~ 2,
                                  result == "Severe" ~ 3,
                                  TRUE ~ as.numeric(result))) %>%
        distinct(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE,OBS_TEST_CONCEPT_NAME,result) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE,OBS_TEST_CONCEPT_NAME) %>%
        arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_CONCEPT_NAME, desc(result)) %>%
        slice(1) %>%
        pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE), names_from = c(OBS_TEST_CONCEPT_NAME), values_from = c(result)) %>%
        mutate(T = ifelse(is.na(`Physician's Global Assessment of Current Disease Status`), `Inflammatory Bowel Disease - Global Assessment Score`, `Physician's Global Assessment of Current Disease Status`),
               R = `Blood in Stool - Recent Change in Rectal Bleeding Amount`,
               S = ifelse(is.na(`Recent Change in Daily Stool Frequency`), `Stool Frequency Score`, `Recent Change in Daily Stool Frequency`)) %>%
        ungroup() %>%
        arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
        mutate(diff = OBS_TEST_RESULT_DATE- lag(OBS_TEST_RESULT_DATE)) %>%
        mutate(diff = if_else(is.na(diff), 0, as.numeric(diff))) %>%
        ungroup() %>%
        mutate(T2 = T, R2 = R, S2 = S) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
        fill(T2, .direction="downup") %>%
        fill(S2, .direction="downup") %>%
        fill(R2, .direction="downup") %>%
        mutate(T = ifelse(is.na(T) & diff <= 7,T2, T),
               S = ifelse(is.na(S) & diff <= 7, S2, S),
               R = ifelse(is.na(R) & diff <= 7, R2, R)) %>%
        select(-T2, -S2, -R2,-diff) %>%
        dplyr::rename(Stool.Freq.Score = S, Rectal.Bleeding.Score = R, Global.Assessment.Score = T) %>%
        mutate(UCDAI.6.score = Stool.Freq.Score + Rectal.Bleeding.Score,
               UCDAI.9.score = Stool.Freq.Score + Rectal.Bleeding.Score + Global.Assessment.Score, Source = "SF") %>%
        mutate(UCDAI.date = OBS_TEST_RESULT_DATE) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, UCDAI.6.score, UCDAI.9.score, UCDAI.date, Source, Stool.Freq.Score, Rectal.Bleeding.Score, Global.Assessment.Score)


      #ECRF

      ucdai_ecrf = data$observations %>%
        filter(DATA_SOURCE == "ECRF_SPARC") %>%
        filter(grepl("Stool|Blood", OBS_TEST_CONCEPT_NAME, ignore.case = T)) %>%
        mutate(OBS_TEST_RESULT_DATE = dmy(OBS_TEST_RESULT_DATE)) %>%
        mutate(result = ifelse(is.na(DESCRIPTIVE_SYMP_TEST_RESULTS), TEST_RESULT_NUMERIC, DESCRIPTIVE_SYMP_TEST_RESULTS)) %>%
        mutate(result = case_when(result %in% c("Normal","Normal (same as when I am well)") ~ 0,
                                  result == "1-2 stools/day more than normal" ~ 1,
                                  result == "3-4 stools/day more than normal" ~ 2,
                                  result == "5 or more stools per day more than normal" ~ 3,
                                  result == "No blood seen" ~ 0,
                                  result == "Blood less than 50% of the time" ~ 1,
                                  OBS_TEST_CONCEPT_NAME=="Blood in Stool" & result == "Blood 50% or more of the time" ~ 2,
                                  OBS_TEST_CONCEPT_NAME=="Blood Passed Alone" & result %in% c("Yes", "Blood 50% or more of the time") ~ 3,
                                  TRUE ~ as.numeric(result))) %>%
        distinct(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE,OBS_TEST_CONCEPT_NAME,result) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE,OBS_TEST_CONCEPT_NAME) %>%
        arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_CONCEPT_NAME, desc(result)) %>%
        slice(1) %>%
        pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE), names_from = c(OBS_TEST_CONCEPT_NAME), values_from = c(result)) %>%
        mutate(
          R = ifelse(is.na(`Blood Passed Alone`), `Blood in Stool`, `Blood Passed Alone`),
          S = `Recent Change in Daily Stool Frequency`) %>%
        ungroup() %>%
        arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
        mutate(diff = OBS_TEST_RESULT_DATE- lag(OBS_TEST_RESULT_DATE)) %>%
        mutate(diff = if_else(is.na(diff), 0, as.numeric(diff))) %>%
        ungroup() %>%
        mutate( R2 = R, S2 = S) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
        fill(S2, .direction="downup") %>%
        fill(R2, .direction="downup") %>%
        mutate(
          S = ifelse(is.na(S) & diff <= 7, S2, S),
          R = ifelse(is.na(R) & diff <= 7, R2, R)) %>%
        select(-S2, -R2,-diff) %>%
        dplyr::rename(Stool.Freq.Score = S, Rectal.Bleeding.Score = R) %>%
        mutate(UCDAI.6.score = Stool.Freq.Score + Rectal.Bleeding.Score,  Source = "ECRF") %>%
        mutate(UCDAI.date = OBS_TEST_RESULT_DATE) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, UCDAI.6.score,  UCDAI.date, Source, Stool.Freq.Score, Rectal.Bleeding.Score)

      ucdai = bind_rows(ucdai_sf, ucdai_ecrf) %>% arrange(DEIDENTIFIED_MASTER_PATIENT_ID, UCDAI.date) %>%
        dplyr::rename(ucdai.source = Source)  %>% distinct_all() %>%
        right_join(cohort) %>%
        mutate(datediff = abs(UCDAI.date - index_date)) %>%
        filter(Diagnosis == "Ulcerative Colitis" & datediff <= t) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
        arrange(DEIDENTIFIED_MASTER_PATIENT_ID, desc(UCDAI.6.score), datediff) %>%
        slice(1) %>%
        distinct(DEIDENTIFIED_MASTER_PATIENT_ID, UCDAI.6.score, UCDAI.9.score, index_date, UCDAI.date, ucdai.source, Stool.Freq.Score, Rectal.Bleeding.Score, Global.Assessment.Score) %>%
        ungroup() %>%
        select(sort(names(.))) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, everything())



      cohort = left_join(cohort, ucdai)

      rm(list=ls(pattern="_ecrf|_sf"))



      #SES-CD

      #ECRF

      ses = data$procedures %>%
        filter(!is.na(SES.CD_Subscore)) %>%
        filter(DATA_SOURCE == "ECRF_SPARC") %>%
        mutate(SES.CD_Subscore = as.numeric(gsub("Not reached", "0", SES.CD_Subscore))) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID, PROC_START_DATE, LOCATION) %>%
        arrange(DEIDENTIFIED_MASTER_PATIENT_ID, desc(SES.CD_Subscore)) %>%
        slice(which.max(SES.CD_Subscore)) %>%
        distinct(DEIDENTIFIED_MASTER_PATIENT_ID, PROC_START_DATE, LOCATION, SES.CD_Subscore) %>%
        pivot_wider(id_cols = c("DEIDENTIFIED_MASTER_PATIENT_ID", "PROC_START_DATE"), names_from = LOCATION, values_from = SES.CD_Subscore) %>%
        mutate(SES_Score = `Ileum` + `Left colon` + Rectum + `Right colon` + `Transverse colon`) %>%
        drop_na(SES_Score) %>%
        dplyr::rename(ses.date = PROC_START_DATE) %>%
        distinct_all() %>%
        mutate(ses.date = dmy(ses.date)) %>%
        right_join(cohort) %>%
        mutate(datediff = abs(ses.date - index_date)) %>%
        filter(Diagnosis == "Crohn's Disease" & datediff <= t) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
        slice(which.min(abs(datediff))) %>%
        distinct(DEIDENTIFIED_MASTER_PATIENT_ID, `Ileum` , `Left colon` , Rectum , `Right colon` , `Transverse colon`, SES_Score, index_date, ses.date) %>%
        ungroup() %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, everything())



      #Mayo Endocscopy Score - source: https://academic.oup.com/ecco-jcc/article/9/10/846/425061

      mes = data$procedures %>%
        filter(!is.na(MAYO_ENDOSCOPIC_SUBSCORE)) %>%
        filter(DATA_SOURCE == "ECRF_SPARC") %>%
        distinct(DEIDENTIFIED_MASTER_PATIENT_ID, MAX_EXTENT_ACTIVE_DISEASE, LOCATION, PROC_START_DATE, MAYO_ENDOSCOPIC_SUBSCORE) %>%
        mutate(MAYO_ENDOSCOPIC_SUBSCORE = as.numeric(gsub("Not seen", "0", MAYO_ENDOSCOPIC_SUBSCORE)))  %>%
        pivot_wider(id_cols = c("DEIDENTIFIED_MASTER_PATIENT_ID", "PROC_START_DATE", "MAX_EXTENT_ACTIVE_DISEASE"), names_from = LOCATION, values_from = MAYO_ENDOSCOPIC_SUBSCORE) %>%
        rowwise(DEIDENTIFIED_MASTER_PATIENT_ID, PROC_START_DATE, MAX_EXTENT_ACTIVE_DISEASE) %>%
        mutate(MAYO_ENDOSCOPY_SCORE = max(Rectum, `Sigmoid colon`, `Right colon`, `Descending colon`, `Transverse colon`),
               MODIFIED_MAYO_SCORE = sum(Rectum, `Sigmoid colon`, `Right colon`, `Descending colon`, `Transverse colon`),
               EXTENDED_MODIFIED_MAYO_SCORE = MODIFIED_MAYO_SCORE*(as.numeric(MAX_EXTENT_ACTIVE_DISEASE)/10),
               MODIFIED_MAYO_ENDOSCOPIC_SCORE = ifelse(sum(Rectum > 0, `Sigmoid colon` > 0, `Right colon` > 0, `Descending colon` > 0, `Transverse colon` >0) == 0, 0, EXTENDED_MODIFIED_MAYO_SCORE/sum(Rectum > 0, `Sigmoid colon` > 0, `Right colon` > 0, `Descending colon` > 0, `Transverse colon` >0))) %>%
        distinct_all() %>%
        mutate(mes.date = dmy(PROC_START_DATE)) %>%
        right_join(cohort) %>%
        mutate(datediff = abs(mes.date - index_date)) %>%
        filter(Diagnosis == "Ulcerative Colitis" & datediff <= t) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
        slice(which.min(abs(datediff))) %>%
        distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, MAX_EXTENT_ACTIVE_DISEASE, Rectum, `Sigmoid colon`, `Right colon`,
                 `Descending colon`, `Transverse colon`, MAYO_ENDOSCOPY_SCORE, MODIFIED_MAYO_SCORE, EXTENDED_MODIFIED_MAYO_SCORE, MODIFIED_MAYO_ENDOSCOPIC_SCORE, mes.date) %>%
        ungroup() %>%
        mutate()

      es = bind_rows(ses, mes) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, Ileum,`Descending colon`, `Left colon`, `Rectum`, `Right colon`, `Sigmoid colon`, `Transverse colon`,
               SES_Score, ses.date, MAX_EXTENT_ACTIVE_DISEASE, MAYO_ENDOSCOPY_SCORE, MODIFIED_MAYO_SCORE, EXTENDED_MODIFIED_MAYO_SCORE, MODIFIED_MAYO_ENDOSCOPIC_SCORE, mes.date) %>%
        distinct()

      cohort = left_join(cohort, es)


      #PHYSCIAN'S GLOBAL ASSESSMENT (PGA)


      #Smartform
      pga =data$observations %>%
        filter(DATA_SOURCE %in% c("SF_SPARC", "ECRF_SPARC")) %>%
        filter(grepl("Stool Frequency|Blood in Stool|Global Assessment", OBS_TEST_CONCEPT_NAME, ignore.case = T)) %>%
        mutate(OBS_TEST_RESULT_DATE = dmy(OBS_TEST_RESULT_DATE)) %>%
        mutate(result = ifelse(is.na(DESCRIPTIVE_SYMP_TEST_RESULTS), TEST_RESULT_NUMERIC, DESCRIPTIVE_SYMP_TEST_RESULTS)) %>%
        mutate(result = case_when(result == "Normal" ~ 0,
                                  result == "1-2 stools/day more than normal" ~ 1,
                                  result == "3-4 stools/day more than normal" ~ 2,
                                  result %in% c(">4 stools/day more than normal", "&gt;4 stools/day more than normal") ~ 3,
                                  result == "None" ~ 0,
                                  result == "Visible blood in stool less than half the time" ~ 1,
                                  result == "Visible blood in stool half of the time or more" ~ 2,
                                  result == "Passing blood alone" ~ 3,
                                  result == "Quiescent" ~ 0,
                                  result == "Mild" ~ 1,
                                  result == "Moderate" ~ 2,
                                  result == "Severe" ~ 3,
                                  TRUE ~ as.numeric(result))) %>%
        distinct(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE,OBS_TEST_CONCEPT_NAME,result) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE,OBS_TEST_CONCEPT_NAME) %>%
        arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_CONCEPT_NAME, desc(result)) %>%
        slice(1) %>%
        pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE), names_from = c(OBS_TEST_CONCEPT_NAME), values_from = c(result)) %>%
        mutate(T = ifelse(is.na(`Physician's Global Assessment of Current Disease Status`), `Inflammatory Bowel Disease - Global Assessment Score`, `Physician's Global Assessment of Current Disease Status`),
               R = `Blood in Stool - Recent Change in Rectal Bleeding Amount`,
               S = ifelse(is.na(`Recent Change in Daily Stool Frequency`), `Stool Frequency Score`, `Recent Change in Daily Stool Frequency`)) %>%
        ungroup() %>%
        arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
        mutate(diff = OBS_TEST_RESULT_DATE- lag(OBS_TEST_RESULT_DATE)) %>%
        mutate(diff = if_else(is.na(diff), 0, as.numeric(diff))) %>%
        ungroup() %>%
        mutate(T2 = T, R2 = R, S2 = S) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
        fill(T2, .direction="downup") %>%
        fill(S2, .direction="downup") %>%
        fill(R2, .direction="downup") %>%
        mutate(T = ifelse(is.na(T) & diff <= 7,T2, T),
               S = ifelse(is.na(S) & diff <= 7, S2, S),
               R = ifelse(is.na(R) & diff <= 7, R2, R)) %>%
        select(-T2, -S2, -R2,-diff) %>%
        dplyr::rename(PGA = T) %>%
        drop_na(PGA) %>%
        mutate(PGA.date = OBS_TEST_RESULT_DATE) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, PGA, PGA.date) %>%
        right_join(cohort) %>%
        mutate(datediff = abs(PGA.date - index_date)) %>%
        filter(datediff <= t) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
        slice(which.min(abs(datediff))) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, PGA, PGA.date)


      cohort = cohort %>%
        left_join(pga)

      #Ostomy


      ostomy_sf = data$procedures %>%
        filter(DATA_SOURCE %in% c("SF_SPARC")) %>%
        filter(PROC_CONCEPT_NAME == "Ileostomy/Colostomy") %>%
        mutate(ostomy = PROC_STATUS_CONCEPT_NAME) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, ostomy, VISIT_ENCOUNTER_ID) %>%
        left_join(data$encounter) %>%
        left_join(cohort) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
        mutate(diff =  dmy(VISIT_ENCOUNTER_START_DATE)- index_date) %>%
        slice(which.min(abs(diff))) %>%
        ungroup() %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, ostomy, index_date, diff)

      ostomy_ecrf = data$observations %>%
        filter(DATA_SOURCE == "ECRF_SPARC") %>%
        filter(OBS_TEST_CONCEPT_NAME %in% c("Baseline Number of Daily Bowel Movements",
                                            "Current Average Number of Daily Bowel Movements",
                                            "Recent Change in Daily Stool Frequency",
                                            "Current Average Number of Daily Liquid Bowel Movements")) %>%
        filter(DESCRIPTIVE_SYMP_TEST_RESULTS == "Not applicable, I have an ostomy") %>%
        mutate(ostomy = DESCRIPTIVE_SYMP_TEST_RESULTS) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, ostomy, VISIT_ENCOUNTER_ID) %>%
        left_join(data$encounter) %>%
        left_join(cohort) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
        mutate(diff =  dmy(VISIT_ENCOUNTER_START_DATE)- index_date) %>%
        drop_na(ostomy) %>%
        slice(which.min(abs(diff))) %>%
        ungroup() %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, ostomy,index_date,diff )

      ostomy = bind_rows(ostomy_sf, ostomy_ecrf) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
        slice(which.min(abs(diff))) %>%
        ungroup() %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, ostomy)

      cohort = cohort %>%
        left_join(ostomy) %>%
        mutate(ostomy = case_when(Diagnosis == "Crohn's Disease" & is.na(sCDAI.score) ~ ostomy,
                                  Diagnosis == "Ulcerative Colitis" & is.na(UCDAI.6.score) ~ ostomy,
                                  TRUE ~ as.character(NA)))



      #REORDER COLUMNS

      cohort <- cohort %>% arrange(DEIDENTIFIED_MASTER_PATIENT_ID, index_date)



      #DATA FROM EMR - encounter ids for where variable is reported for master report



      if(is.null(emr_codes) == FALSE){


        emr = NULL

        for (i in 1:length(data)){
          if("DATA_SOURCE" %in% names(data[[i]])){k = data[[i]] %>% filter(DATA_SOURCE == "EMR")} else{k = NULL}

          emr[[i]] = k}

        names(emr) = names(data)

        emr$patient_history_old = NULL

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
                mutate(VAR = ifelse(is.na(DIAGNOSIS_HISTORY_CONCEPT_NAME), paste0("PATIENT_HISTORY_", MED_HISTORY_CONCEPT_NAME), paste0("PATIENT_HISTORY_", DIAGNOSIS_HISTORY_CONCEPT_NAME))) %>%
                mutate(RESPONSE = "Patient History") %>%
                mutate(diff = dmy(EFFECTIVE_DATE) - index_date) %>%
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

        emr_subset$patient_history_old = NULL

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

conf = max(which(colnames(cohort) %in% toupper("No_IBD_Medication_At_Enrollment")))

#column headers
conditionalFormatting(wb, "sparc_summary", cols=1:democoln, rows=1, rule="!=0", style = style1)
conditionalFormatting(wb, "sparc_summary", cols=(democoln+1):conf, rows=1, rule="!=0", style = style2)
conditionalFormatting(wb, "sparc_summary", cols=(conf+1):coln, rows=1, rule="!=0", style = style3)


conditionalFormatting(wb, "sparc_summary", cols = (conf+1):coln, rows = 2:rown, type = "contains", rule = "Possible", style = style5)
conditionalFormatting(wb, "sparc_summary", cols = (conf+1):coln, rows = 2:rown, type = "contains", rule = "Contradicts", style = style6)
conditionalFormatting(wb, "sparc_summary", cols = (conf+1):coln, rows = 2:rown, type = "contains", rule = "No", style = style6)
conditionalFormatting(wb, "sparc_summary", cols = (conf+1):coln, rows = 2:rown, type = "contains", rule = "Yes", style = style4)
conditionalFormatting(wb, "sparc_summary", cols = (conf-1), rows = 2:rown, type = "colourScale",  style = c("green", "yellow", "orange"))




# SAVE REPORT ----

saveWorkbook(wb, file = paste0(filename), overwrite = TRUE)

return(cohort)
}
