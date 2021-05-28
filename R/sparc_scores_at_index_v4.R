#' sparc_scores
#'
#' reads in SPARC data from IBD Plexus, and calculates diagnosis, phenotype, disease, and endoscopy scores at a specific (“index”) date.
#'
#' @param datadir directory where unzipped data is located
#' @param index_info A dataframe with DEIDENTIFIED_MASTER_PATIENT_ID and a variable index_date.  Default is date of consent.
#' @param filename name of file to output. Must be xlsx format.
#' @param index_range numbers of days to look out of index date. For example, if you want data within +/- 30 days of the index date this value would be 30.
#'
#' @return A dataframe with scores at that date and an excel spreadsheet.
#' @export
sparc_scores <- function(datadir,
                             index_info = "DATE_OF_CONSENT",
                             filename = "SPARC_SCORES.xlsx",
                             index_range = "30"){


#===============================
#GET FILES OF MOST RECENT DATA
#===============================
  data = load_data(datadir = datadir,
                   cohort = "SPARC",
                   domains = c("Demographics", "Diagnosis", "Observations", "Prescriptions", "Procedures", "Encounter"),
                   data_type = "CRF")

#===============================
#DEMOGRAPHIC INFORMATION
#===============================
#demographic information
demo = data$demographics %>%
  filter(DATA_SOURCE == "ECRF_SPARC") %>%
  distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DATE_OF_CONSENT, DATE_OF_CONSENT_WITHDRAWN, BIRTH_YEAR, GENDER) %>%
  mutate(DATE_OF_CONSENT = dmy(DATE_OF_CONSENT)) %>%
  filter(year(DATE_OF_CONSENT) >= 2016) %>%
  group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
  slice(which.min(DATE_OF_CONSENT)) %>%
  ungroup()


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

  #closest diagnosis to index date from smartform then ecrf

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



  #===============================
  #PHENOTYPES:
  #  Crohn's Disease - just closest no date constraint
  #===============================


  cdp = data$observations %>% filter(DATA_SOURCE == "SF_SPARC")  %>%  left_join(data$encounter) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    filter(OBS_TEST_CONCEPT_NAME %in% "Crohn's Disease Phenotype") %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_CONCEPT_NAME, DESCRIPTIVE_SYMP_TEST_RESULTS,OBS_TEST_CONCEPT_CODE ,VISIT_ENCOUNTER_START_DATE, SRC_OBS_TEST_CONCEPT_CODE) %>%
    drop_na(DESCRIPTIVE_SYMP_TEST_RESULTS) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID,
            match(DESCRIPTIVE_SYMP_TEST_RESULTS, c("Both stricturing and penetrating", "Stricturing", "Penetrating", "Inflammatory, non-penetrating, non-stricturing", "Unknown")),
            desc(VISIT_ENCOUNTER_START_DATE)) %>%
    left_join(cohort) %>%
    mutate(diff =  dmy(VISIT_ENCOUNTER_START_DATE)- index_date) %>%
    mutate(keep = case_when(DESCRIPTIVE_SYMP_TEST_RESULTS == "Inflammatory, non-penetrating, non-stricturing" & diff > t ~ "keep",
                            diff <= t ~ "keep")) %>%
    #filter(keep == "keep") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID,index_date) %>%
    slice(which.min(abs(diff))) %>%
    mutate("Crohn's disease phenotype" = DESCRIPTIVE_SYMP_TEST_RESULTS, c = 1) %>%
    ungroup() %>%
    left_join(dx) %>%
    filter(Diagnosis == "Crohn's Disease") %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, "Crohn's disease phenotype", index_date)



  cohort = cohort %>%
    left_join(cdp)


  #===============================
  #PHENOTYPES:
  #  Ulcerative Colitis
  #===============================
  ucp = data$observations %>% filter(DATA_SOURCE == "SF_SPARC")  %>%  left_join(data$encounter) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    filter(OBS_TEST_CONCEPT_NAME %in% "Extent of Macroscopic Ulcerative Colitis" & (!is.na(DESCRIPTIVE_SYMP_TEST_RESULTS))) %>%
    #filter(OBS_TEST_CONCEPT_CODE %in% c("EPIC#41348", 'EPIC#41349', 'EPIC#41350','EPIC#41351' , "EPIC#41352") & (!is.na(DESCRIPTIVE_SYMP_TEST_RESULTS))) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DESCRIPTIVE_SYMP_TEST_RESULTS,OBS_TEST_CONCEPT_CODE ,VISIT_ENCOUNTER_START_DATE) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, match(DESCRIPTIVE_SYMP_TEST_RESULTS, c("Pancolitis",
                                                                                   "Extensive ulcerative colitis (extends proximal to the splenic flexure)",
                                                                                   "Left-sided ulcerative colitis (distal to the splenic flexure only)",
                                                                                   "Ulcerative proctitis (rectum only)",
                                                                                   "Unknown")),
            desc(VISIT_ENCOUNTER_START_DATE)) %>%
    left_join(cohort) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%

    mutate(diff =  dmy(VISIT_ENCOUNTER_START_DATE)- index_date) %>%
    mutate(keep = case_when(DESCRIPTIVE_SYMP_TEST_RESULTS == "Ulcerative proctitis (rectum only)" & diff > t ~ "keep",
                            diff <= t ~ "keep")) %>%
    #filter(keep == "keep") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID,index_date) %>%
    slice(which.min(abs(diff))) %>%
    ungroup() %>%
    mutate("Extent of macroscopic ulcerative colitis" = DESCRIPTIVE_SYMP_TEST_RESULTS, c=1) %>%
    left_join(dx) %>%
    filter(Diagnosis == "Ulcerative Colitis") %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, "Extent of macroscopic ulcerative colitis", index_date)


  cohort = left_join(cohort, ucp)


  #===============================
  #PHENOTYPES:
  #  IBD-U
  #===============================

  ibdu = data$observations %>% filter(DATA_SOURCE == "SF_SPARC")  %>%  left_join(data$encounter) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    filter(OBS_TEST_CONCEPT_CODE %in% c('EPIC#31000125051', 'EPIC#31000125052', 'EPIC#31000125053','EPIC#31000125054' , 'EPIC#31000125055', "SMART_Q61__C" ) & (!is.na(DESCRIPTIVE_SYMP_TEST_RESULTS))) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DESCRIPTIVE_SYMP_TEST_RESULTS,OBS_TEST_CONCEPT_CODE ,VISIT_ENCOUNTER_START_DATE) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, match(DESCRIPTIVE_SYMP_TEST_RESULTS, c("Pancolitis", "Extensive ulcerative colitis (extends proximal to the splenic flexure)",
                                                                                   "Left-sided ulcerative colitis (distal to the splenic flexure only)",
                                                                                   "Ulcerative proctitis (rectum only)",
                                                                                   "Unknown")),
            desc(VISIT_ENCOUNTER_START_DATE)) %>%
    left_join(cohort) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%

    mutate(diff =  dmy(VISIT_ENCOUNTER_START_DATE)- index_date) %>%
    mutate(keep = case_when(DESCRIPTIVE_SYMP_TEST_RESULTS == "Ulcerative proctitis (rectum only)" & diff > t ~ "keep",
                            diff <= t ~ "keep")) %>%
    #filter(keep == "keep") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID,index_date) %>%
    slice(which.min(abs(diff))) %>%
    ungroup() %>%
    mutate("Extent of macroscopic IBD unclassified" = DESCRIPTIVE_SYMP_TEST_RESULTS,
           c=1) %>%
    left_join(dx) %>%
    filter(Diagnosis == "IBD Unclassified") %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, "Extent of macroscopic IBD unclassified", index_date)

  cohort = cohort %>%
    left_join(ibdu)


  #===============================
  #MEDICATION EXPOSURE UP UNTIL INDEX DATE
  #===============================

  #if never can pull back

  sf_med = data$prescriptions %>%
    filter(DATA_SOURCE == "SF_SPARC") %>%
    left_join(data$encounter) %>%
    mutate(VISIT_ENCOUNTER_START_DATE = dmy(VISIT_ENCOUNTER_START_DATE)) %>%
    left_join(cohort) %>%
    drop_na(index_date) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID,MEDICATION_NAME, index_date) %>%
    mutate(diff = (VISIT_ENCOUNTER_START_DATE - index_date)) %>%
    mutate(keep = case_when(MEDICATION_ADMINISTRATED == "Never" & diff > 0 ~ "keep",
                            diff <= 0 ~ "keep")) %>%
    filter(keep == "keep") %>%
    slice(which.min(abs(diff))) %>%
    pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, index_date), names_from = MEDICATION_NAME, values_from = MEDICATION_ADMINISTRATED) %>%
    select(sort(names(.))) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, everything())


  cohort = left_join(cohort, sf_med)


  #===============================
  #EIMS within 30 days of enrollment
  #===============================

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
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID,DIAG_CONCEPT_NAME) %>%
    slice(which.min(abs(diff))) %>%
    mutate(NEW_VALUE = ifelse(DIAG_STATUS_CONCEPT_NAME=="Yes", "Yes",
                              ifelse(DIAG_STATUS_CONCEPT_NAME=="Unknown", "Unknown",
                                     ifelse(DIAG_STATUS_CONCEPT_NAME=="No", "No", as.character(NA))))) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, DIAG_CONCEPT_NAME, NEW_VALUE) %>%
    pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, index_date), names_from = DIAG_CONCEPT_NAME, values_from = NEW_VALUE)

  cohort = cohort %>%
    left_join(EIMS)

  #===============================
  #SURGICAL HISTORY:
  #   Small bowel resection
  #===============================

  sbr = data$procedures %>% filter(DATA_SOURCE == "SF_SPARC")  %>%  left_join(data$encounter) %>%
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

  #===============================
  #SURGICAL HISTORY:
  #   Complete colectomy
  #===============================

  cc = data$procedures %>%
    filter(DATA_SOURCE == "SF_SPARC")  %>%
    left_join(data$encounter) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    filter(PROC_CONCEPT_CODE %in% c("EPIC#41172", "EPIC#17655", "SMART_Q90__C")| SRC_PROC_CONCEPT_CODE %in% c("EPIC#41172", "EPIC#17655", "SMART_Q90__C")) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID,  PROC_CONCEPT_CODE, PROC_STATUS_CONCEPT_CODE, SRC_PROC_CONCEPT_NAME,	SRC_PROC_CONCEPT_CODE,
             PROC_STATUS_CONCEPT_NAME, VISIT_ENCOUNTER_START_DATE) %>%
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
           c = 1) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, index_date,  'Complete colectomy')

  cohort = cohort %>%
    left_join(cc)


  #===============================
  #PHENOTYPES:
  #   Involved Sites
  #===============================

  cd_codes = cd_codes %>% mutate(SRC_OBS_TEST_CONCEPT_CODE = OBS_TEST_CONCEPT_CODE)


  cdp.secohort = as.vector(t(unique(cd_codes[1])))

  cd_pheno = cohort %>% distinct(DEIDENTIFIED_MASTER_PATIENT_ID)

  for (i in 1:length(cdp.secohort)){

    pheno = cd_codes[ which(cd_codes$Phenotype %in% cdp.secohort[i]),]

    p=NULL

    for (j in 1:length(pheno)){

      p[[j]] = data$observations %>% filter(DATA_SOURCE == "SF_SPARC")  %>%
        left_join(data$encounter) %>%
        select_if(function(x){!all(is.na(x))}) %>%
        filter(OBS_TEST_CONCEPT_CODE %in% pheno$OBS_TEST_CONCEPT_CODE[j] & OBS_TEST_CONCEPT_NAME %in% pheno$Phenotype|
                 SRC_OBS_TEST_CONCEPT_CODE %in% pheno$OBS_TEST_CONCEPT_CODE[j] & OBS_TEST_CONCEPT_NAME %in% pheno$Phenotype)%>%
        mutate(SRC_OBS_TEST_CONCEPT_CODE = ifelse(is.na(SRC_OBS_TEST_CONCEPT_CODE), OBS_TEST_CONCEPT_CODE, SRC_OBS_TEST_CONCEPT_CODE)) %>%
        arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_CONCEPT_NAME,desc(VISIT_ENCOUNTER_START_DATE), OBS_ID) %>%

        group_by(DEIDENTIFIED_MASTER_PATIENT_ID,OBS_TEST_CONCEPT_NAME) %>%
        #slice(which.max(VISIT_ENCOUNTER_START_DATE))  %>%
        slice(1) %>%
        slice(which.max(OBS_ID)) %>%
        #mutate(new = ifelse(DIAG_STATUS_CONCEPT_NAME %in% pheno$DIAG_STATUS_CONCEPT_NAME[j], pheno$CODING[j], ""))
        mutate(new = case_when(grepl("SMART_", OBS_TEST_CONCEPT_CODE) ~ as.character(DESCRIPTIVE_SYMP_TEST_RESULTS) ,
                               TRUE ~ pheno$CODING[j]))


    }

   p =  p[sapply(p, nrow)>0]

    p = p %>%
      bind_rows() %>% slice(which.max(OBS_ID)) %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_START_DATE, new) %>%
      arrange(DEIDENTIFIED_MASTER_PATIENT_ID, new,desc(VISIT_ENCOUNTER_START_DATE)) %>%
      left_join(cohort) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID,new, index_date) %>%

      mutate(diff =  dmy(VISIT_ENCOUNTER_START_DATE)- index_date) %>%
      filter(diff <= t) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID,index_date) %>%
      slice(which.min(abs(diff))) %>%
      ungroup() %>%
      select(DEIDENTIFIED_MASTER_PATIENT_ID,new, index_date)

    names(p)[names(p) == 'new'] = cdp.secohort[i]

    cd_pheno = full_join(cd_pheno, p)
  }


  cd_pheno = cd_pheno %>% left_join(dx) %>% filter(Diagnosis == "Crohn's Disease") %>% drop_na(index_date)

  cohort = cohort %>%
    left_join(cd_pheno)




  #===============================
  #PHENOTYPES:
  #  UC Hospitalizations
  #===============================
  uch = data$observations %>% filter(DATA_SOURCE == "SF_SPARC")  %>%  left_join(data$encounter) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    #filter(OBS_TEST_CONCEPT_NAME == "History of Hospitalization for severe Ulcerative Colitis") %>%
    filter(OBS_TEST_CONCEPT_CODE %in% c("EPIC#41201", "EPIC#41202", "11337-3", "SMART_Q60__C")& (!is.na(DESCRIPTIVE_SYMP_TEST_RESULTS))) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID,  DESCRIPTIVE_SYMP_TEST_RESULTS, OBS_TEST_CONCEPT_CODE, VISIT_ENCOUNTER_START_DATE) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_CONCEPT_CODE,desc(VISIT_ENCOUNTER_START_DATE)) %>%
    left_join(cohort) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%

    mutate(diff =  dmy(VISIT_ENCOUNTER_START_DATE)- index_date) %>%
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



  #===============================
  #PHENOTYPES:
  #  IBDU Hospitalizations
  #===============================

  ibduh = data$observations %>% filter(DATA_SOURCE == "SF_SPARC")  %>%  left_join(data$encounter) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    filter(OBS_TEST_CONCEPT_CODE %in% c("EPIC#31000125062", "EPIC#31000125063", "SMART_Q62__C")& (!is.na(DESCRIPTIVE_SYMP_TEST_RESULTS))) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID,  DESCRIPTIVE_SYMP_TEST_RESULTS, OBS_TEST_CONCEPT_CODE, VISIT_ENCOUNTER_START_DATE) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_CONCEPT_CODE,desc(VISIT_ENCOUNTER_START_DATE)) %>%
    left_join(cohort) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%

    mutate(diff =  dmy(VISIT_ENCOUNTER_START_DATE)- index_date) %>%
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




#===============================
#Calculate disease activity scores +/- t days of index
#===============================


  data$observations = data$observations %>%
    mutate(OBS_TEST_CONCEPT_NAME = ifelse(OBS_TEST_CONCEPT_NAME == "Constitutional- General Well-Being", "Constitutional - General Well-Being", OBS_TEST_CONCEPT_NAME))

  #===============================
  #sCDAI
  #===============================
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
           G = ifelse(is.na(`Constitutional - General Well-Being`), `Constitutional - General Well-Being`, `Constitutional - General Well-Being`),
           G = ifelse(is.na(G), `General Well Being Score`, G)) %>%
    ungroup() %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
     mutate(diff = OBS_TEST_RESULT_DATE- lag(OBS_TEST_RESULT_DATE),              diff2 = lead(OBS_TEST_RESULT_DATE) - OBS_TEST_RESULT_DATE)  %>%
    #mutate(diff = if_else(is.na(diff), 0, as.numeric(diff))) %>%
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
    mutate(A = ifelse(is.na(A) & (diff  <= 7| diff2 <= 7),A2, A),
           G = ifelse(is.na(G) & (diff  <= 7| diff2 <= 7), G2, G),
           B = ifelse(is.na(B) & (diff  <= 7| diff2 <= 7), B2, B),
           Daily.BM.Question = ifelse(is.na(Daily.BM.Question) & (diff  <= 7| diff2 <= 7), DBQ2, Daily.BM.Question)) %>%
    select(-A2, -B2, -G2,-diff, -diff2,-DBQ2) %>%
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
     mutate(diff = OBS_TEST_RESULT_DATE- lag(OBS_TEST_RESULT_DATE),              diff2 = lead(OBS_TEST_RESULT_DATE) - OBS_TEST_RESULT_DATE)  %>%
    #mutate(diff = if_else(is.na(diff), 0, as.numeric(diff))) %>%
    ungroup() %>%
      mutate(A2 = A, B2 = B, G2 = G, DBQ2 = Daily.BM.Question) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      fill(A2, .direction="downup") %>%
      fill(B2, .direction="downup") %>%
      fill(G2, .direction="downup") %>%
      fill(DBQ2, .direction = "downup") %>%
      mutate(A = ifelse(is.na(A) & (diff  <= 7| diff2 <= 7),A2, A),
             G = ifelse(is.na(G) & (diff  <= 7| diff2 <= 7), G2, G),
             B = ifelse(is.na(B) & (diff  <= 7| diff2 <= 7), B2, B),
             Daily.BM.Question = ifelse(is.na(Daily.BM.Question) & (diff  <= 7| diff2 <= 7), DBQ2, Daily.BM.Question)) %>%
      select(-A2, -B2, -G2,-diff, -diff2,-DBQ2) %>%
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



  #===============================
  #6/9 point Mayo/ucdai
  #===============================

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
     mutate(diff = OBS_TEST_RESULT_DATE- lag(OBS_TEST_RESULT_DATE),              diff2 = lead(OBS_TEST_RESULT_DATE) - OBS_TEST_RESULT_DATE)  %>%
    #mutate(diff = if_else(is.na(diff), 0, as.numeric(diff))) %>%
    ungroup() %>%
    mutate(T2 = T, R2 = R, S2 = S) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    fill(T2, .direction="downup") %>%
    fill(S2, .direction="downup") %>%
    fill(R2, .direction="downup") %>%
    mutate(T = ifelse(is.na(T) & (diff  <= 7| diff2 <= 7),T2, T),
           S = ifelse(is.na(S) & (diff  <= 7| diff2 <= 7), S2, S),
           R = ifelse(is.na(R) & (diff  <= 7| diff2 <= 7), R2, R)) %>%
    select(-T2, -S2, -R2,-diff, -diff2) %>%
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
     mutate(diff = OBS_TEST_RESULT_DATE- lag(OBS_TEST_RESULT_DATE),              diff2 = lead(OBS_TEST_RESULT_DATE) - OBS_TEST_RESULT_DATE)  %>%
    #mutate(diff = if_else(is.na(diff), 0, as.numeric(diff))) %>%
    ungroup() %>%
    mutate( R2 = R, S2 = S) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    fill(S2, .direction="downup") %>%
    fill(R2, .direction="downup") %>%
    mutate(
           S = ifelse(is.na(S) & (diff  <= 7| diff2 <= 7), S2, S),
           R = ifelse(is.na(R) & (diff  <= 7| diff2 <= 7), R2, R)) %>%
    select(-S2, -R2,-diff, -diff2) %>%
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


  #===============================
  #SES-CD
  #===============================
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


  #===============================
  #Mayo Endocscopy Score - source: https://academic.oup.com/ecco-jcc/article/9/10/846/425061
  #===============================
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

  #===============================
  #PHYSCIAN'S GLOBAL ASSESSMENT (PGA)
  #===============================

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
     mutate(diff = OBS_TEST_RESULT_DATE- lag(OBS_TEST_RESULT_DATE),              diff2 = lead(OBS_TEST_RESULT_DATE) - OBS_TEST_RESULT_DATE)  %>%
    #mutate(diff = if_else(is.na(diff), 0, as.numeric(diff))) %>%
    ungroup() %>%
    mutate(T2 = T, R2 = R, S2 = S) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    fill(T2, .direction="downup") %>%
    fill(S2, .direction="downup") %>%
    fill(R2, .direction="downup") %>%
    mutate(T = ifelse(is.na(T) & (diff  <= 7| diff2 <= 7),T2, T),
           S = ifelse(is.na(S) & (diff  <= 7| diff2 <= 7), S2, S),
           R = ifelse(is.na(R) & (diff  <= 7| diff2 <= 7), R2, R)) %>%
    select(-T2, -S2, -R2,-diff, -diff2) %>%
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
  #===============================
  #Ostomy
  #===============================

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


#===============================
#REORDER COLUMNS
#===============================




  cohort <- cohort %>% arrange(DEIDENTIFIED_MASTER_PATIENT_ID, index_date)
  names(cohort) = toupper(names(cohort))



#===============================
#CREATE HEADER STYLES
#===============================
#blue
style1 <- createStyle(bgFill = "#BDD7EE", textDecoration = "bold")

#orange
style2 <- createStyle(bgFill="#F8CBAD", textDecoration = "bold")

#yellow
style3 <- createStyle(bgFill="#FFE699")

#green
style4 <-  createStyle(bgFill="#C6E0B4")

#grey
style5 <-  createStyle(bgFill="#D9D9D9", textDecoration = "bold")

#red
style6 <- createStyle(bgFill="#e84135")

#===============================
#CREATE WORKBOOK
#===============================

wb <- createWorkbook()
addWorksheet(wb, "scores_at_index")
writeData(wb, "scores_at_index", x=cohort, startCol=1, startRow=1, colNames=TRUE, rowNames=FALSE)

#===============================
#FORMAT CELLS
#===============================

rown = dim(cohort)[1]
coln = dim(cohort)[2]

#column headers
conditionalFormatting(wb, "scores_at_index", cols=1:coln, rows=1, rule="!=0", style = style1)


#===============================
#SAVE REPORT
#===============================
saveWorkbook(wb, file = paste0(filename), overwrite = TRUE)
return(cohort)
}
