
#' calculate_disease_scores
#'
#' Calculates Latest Diagnosis, sCDAI, 6 POINT MAYO, Manitoba, & PGA for SPARC cohort
#'
#' @param datadir directory where unzipped data is saved.
#'
#' @return A dataframe with the scores as different tabs and a list of dataframes with each score as a list element.
#' @export
calculate_disease_scores <- function(datadir = "."){


  data = load_data(datadir = datadir, cohort = "SPARC", domains = c("Demographics", "Diagnosis", "Procedures", "Encounter", "Observations"), data_type = "CRF")


  #===============================
  #SPARC
  #===============================
  #===============================
  #LATEST DIAGNOSIS
  #===============================

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


  #===============================
  #sCDAI
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
      mutate(diff = OBS_TEST_RESULT_DATE- lag(OBS_TEST_RESULT_DATE),
             diff2 = lead(OBS_TEST_RESULT_DATE) - OBS_TEST_RESULT_DATE) %>%
      #mutate(diff = if_else(is.na(diff), 0, as.numeric(diff))) %>%
      ungroup() %>%
      mutate(Daily.BM.Question = case_when(B == `Current Maximum Number of Daily Bowel Movements` ~ "Current Maximum Number of Daily Bowel Movements",
                                           B == `Current Average Number of Daily Bowel Movements` ~ "Current Average Number of Daily Bowel Movements",
                                           TRUE ~ as.character(NA))) %>%
      mutate(A2 = A, B2 = B, G2 = G, DBQ2 = Daily.BM.Question,A3 = A, B3 = B, G3 = G, DBQ3 = Daily.BM.Question) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      fill(A2, .direction="down") %>%
      fill(B2, .direction="down") %>%
      fill(G2, .direction="down") %>%
      fill(DBQ2, .direction = "down") %>%
      fill(A3, .direction="up") %>%
      fill(B3, .direction="up") %>%
      fill(G3, .direction="up") %>%
      fill(DBQ3, .direction = "up") %>%
      mutate(A = ifelse(is.na(A) & diff  <= 7,lag(A), A),
             G = ifelse(is.na(G) & diff  <= 7, lag(G), G),
             B = ifelse(is.na(B) & diff  <= 7, lag(B), B),
             Daily.BM.Question = ifelse(is.na(Daily.BM.Question) & (diff  <= 7), lag(Daily.BM.Question), Daily.BM.Question)) %>%
      mutate(A = ifelse(is.na(A) & (diff2 <= 7),lead(A), A),
             G = ifelse(is.na(G) & ( diff2 <= 7), lead(G), G),
             B = ifelse(is.na(B) & (diff2 <= 7), lead(B), B),
             Daily.BM.Question = ifelse(is.na(Daily.BM.Question) & ( diff2 <= 7), lead(Daily.BM.Question), Daily.BM.Question)) %>%
      select(-A2, -B2, -G2,-diff,-DBQ2, -diff2, - A3, -B3, -G3, -DBQ3) %>%
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
      mutate(diff = OBS_TEST_RESULT_DATE- lag(OBS_TEST_RESULT_DATE),
             diff2 = lead(OBS_TEST_RESULT_DATE) - OBS_TEST_RESULT_DATE) %>%
     # mutate(diff = if_else(is.na(diff), 0, as.numeric(diff))) %>%
      ungroup() %>%
      mutate(A2 = A, B2 = B, G2 = G, DBQ2 = Daily.BM.Question,A3 = A, B3 = B, G3 = G, DBQ3 = Daily.BM.Question) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      fill(A2, .direction="down") %>%
      fill(B2, .direction="down") %>%
      fill(G2, .direction="down") %>%
      fill(DBQ2, .direction = "down") %>%
      fill(A3, .direction="up") %>%
      fill(B3, .direction="up") %>%
      fill(G3, .direction="up") %>%
      fill(DBQ3, .direction = "up") %>%
      mutate(A = ifelse(is.na(A) & diff  <= 7,lag(A), A),
             G = ifelse(is.na(G) & diff  <= 7, lag(G), G),
             B = ifelse(is.na(B) & diff  <= 7, lag(B), B),
             Daily.BM.Question = ifelse(is.na(Daily.BM.Question) & (diff  <= 7), lag(Daily.BM.Question), Daily.BM.Question)) %>%
      mutate(A = ifelse(is.na(A) & (diff2 <= 7),lead(A), A),
             G = ifelse(is.na(G) & ( diff2 <= 7), lead(G), G),
             B = ifelse(is.na(B) & (diff2 <= 7), lead(B), B),
             Daily.BM.Question = ifelse(is.na(Daily.BM.Question) & ( diff2 <= 7), lead(Daily.BM.Question), Daily.BM.Question)) %>%
      select(-A2, -B2, -G2,-diff,-DBQ2, -diff2, - A3, -B3, -G3, -DBQ3) %>%
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
      right_join(dx) %>%
      filter(Diagnosis == "Crohn's Disease") %>%
      arrange(DEIDENTIFIED_MASTER_PATIENT_ID, desc(sCDAI.score)) %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, sCDAI.date, sCDAI.score,  scdai.source, Daily.BM, Daily.BM.Version,Abdominal.Pain.Score, General.well.being.score) %>%
      ungroup() %>%
      select(sort(names(.))) %>%
      select(DEIDENTIFIED_MASTER_PATIENT_ID, everything())




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
      mutate(diff = OBS_TEST_RESULT_DATE- lag(OBS_TEST_RESULT_DATE),
             diff2 = lead(OBS_TEST_RESULT_DATE) - OBS_TEST_RESULT_DATE) %>%
      #mutate(diff = if_else(is.na(diff), 0, as.numeric(diff))) %>%
      ungroup() %>%
      mutate(T2 = T, R2 = R, S2 = S, T3 = T, R3 = R, S3 = S) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      fill(T2, .direction="down") %>%
      fill(S2, .direction="down") %>%
      fill(R2, .direction="down") %>%
      fill(T3, .direction="up") %>%
      fill(S3, .direction="up") %>%
      fill(R3, .direction="up") %>%
      mutate(T = ifelse(is.na(T) & (diff <= 7),lag(T), T),
             S = ifelse(is.na(S) & (diff <= 7), lag(S), S),
             R = ifelse(is.na(R) & (diff <= 7 ), lag(R), R)) %>%
      mutate(T = ifelse(is.na(T) & (diff2 <= 7),lead(T), T),
             S = ifelse(is.na(S) & (diff2 <= 7), lead(S), S),
             R = ifelse(is.na(R) & (diff2 <= 7), lead(R), R)) %>%
      select(-T2, -S2, -R2,-diff, -diff2, -T3, -R3, -S3) %>%
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
      mutate(result = case_when(result %in% c("Normal","Normal (same as when I am well)","Less stool than normal") ~ 0,
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
      mutate(diff = OBS_TEST_RESULT_DATE- lag(OBS_TEST_RESULT_DATE),
             diff2 = lead(OBS_TEST_RESULT_DATE) - OBS_TEST_RESULT_DATE) %>%
      #mutate(diff = if_else(is.na(diff), 0, as.numeric(diff))) %>%
      ungroup() %>%
      mutate( R2 = R, S2 = S) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      mutate(R2 = R, S2 = S, T3 = T, R3 = R, S3 = S) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      fill(S2, .direction="down") %>%
      fill(R2, .direction="down") %>%
      fill(S3, .direction="up") %>%
      fill(R3, .direction="up") %>%
      mutate(
             S = ifelse(is.na(S) & (diff <= 7), lag(S), S),
             R = ifelse(is.na(R) & (diff <= 7 ), lead(R), R)) %>%
      mutate(
             S = ifelse(is.na(S) & (diff2 <= 7), lead(S), S),
             R = ifelse(is.na(R) & (diff2 <= 7), lead(R), R)) %>%
      select( -S2, -R2,-diff, -diff2, -R3, -S3) %>%
      dplyr::rename(Stool.Freq.Score = S, Rectal.Bleeding.Score = R) %>%
      mutate(UCDAI.6.score = Stool.Freq.Score + Rectal.Bleeding.Score,  Source = "ECRF") %>%
      mutate(UCDAI.date = OBS_TEST_RESULT_DATE) %>%
      select(DEIDENTIFIED_MASTER_PATIENT_ID, UCDAI.6.score,  UCDAI.date, Source, Stool.Freq.Score, Rectal.Bleeding.Score)

    ucdai = bind_rows(ucdai_sf, ucdai_ecrf) %>% arrange(DEIDENTIFIED_MASTER_PATIENT_ID, UCDAI.date) %>%
      dplyr::rename(ucdai.source = Source)  %>% distinct_all() %>%
      right_join(dx) %>%
      filter(Diagnosis == "Ulcerative Colitis") %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      arrange(DEIDENTIFIED_MASTER_PATIENT_ID, desc(UCDAI.6.score)) %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, UCDAI.6.score, UCDAI.9.score,  UCDAI.date, ucdai.source, Stool.Freq.Score, Rectal.Bleeding.Score, Global.Assessment.Score) %>%
      ungroup() %>%
      select(sort(names(.))) %>%
      select(DEIDENTIFIED_MASTER_PATIENT_ID,  everything())


    rm(list=ls(pattern="_ecrf|_sf"))


  #===============================
  #MANITOBA
  #===============================

  #ecrf
  manitoba = as.data.frame(data$observations)  %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    filter(OBS_TEST_CONCEPT_CODE %in% c("DISEASE_ACTIVITY__C") &
             !is.na(DESCRIPTIVE_SYMP_TEST_RESULTS)) %>%
    dplyr::rename(Manitoba = DESCRIPTIVE_SYMP_TEST_RESULTS) %>%
    mutate(Manitoba.date = dmy(OBS_TEST_RESULT_DATE)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, Manitoba, Manitoba.date) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, Manitoba.date) %>%
    mutate(Manitoba.source = "ECRF") %>% distinct_all()


  #===============================
  #PHYSCIAN'S GLOBAL ASSESSMENT (PGA)
  #===============================

  #Smartform
  pga = as.data.frame(data$observations)  %>%
    filter(DATA_SOURCE == "SF_SPARC") %>%
    filter(OBS_TEST_CONCEPT_CODE %in% c("EPIC#16411", "SMART_Q9__C") &
             !is.na(DESCRIPTIVE_SYMP_TEST_RESULTS)) %>%
    dplyr::rename(PGA = DESCRIPTIVE_SYMP_TEST_RESULTS) %>%
    mutate(PGA.date = dmy(OBS_TEST_RESULT_DATE)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, PGA, PGA.date) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, PGA.date) %>%
    mutate(PGA.source = "SF") %>% distinct_all()


  #===============================
  #SES-CD
  #===============================
  #ECRF

  ses = data$procedures %>% filter(!is.na(`SES-CD_Subscore`)) %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    mutate(`SES-CD_Subscore` = ifelse(`SES-CD_Subscore` == "Not reached", as.numeric(as.character("0")), as.numeric(as.character(`SES-CD_Subscore`)))) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, PROC_START_DATE) %>%
    dplyr::summarise(SES_Score = sum(`SES-CD_Subscore`)) %>%
    drop_na(SES_Score) %>%
    dplyr::rename(ses.date = PROC_START_DATE) %>%
    distinct_all() %>%
    left_join(dx) %>%
    mutate(ses.date = dmy(ses.date)) %>%
    filter(Diagnosis == "Crohn's Disease")

  #===============================
  #Mayo Endocscopy Score
  #===============================
  mes = data$procedures %>%   filter(!is.na(MAYO_ENDOSCOPIC_SUBSCORE)) %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, LOCATION, PROC_START_DATE, MAYO_ENDOSCOPIC_SUBSCORE) %>%
    mutate(MAYO_ENDOSCOPIC_SUBSCORE = ifelse(MAYO_ENDOSCOPIC_SUBSCORE == "Not seen", as.numeric(as.character(0)),
                                             as.numeric(as.character(MAYO_ENDOSCOPIC_SUBSCORE)))) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, PROC_START_DATE) %>%
    dplyr::summarise(MAYO_ENDOSCOPIC_SCORE = max(MAYO_ENDOSCOPIC_SUBSCORE)) %>%
    drop_na(MAYO_ENDOSCOPIC_SCORE) %>%
    dplyr::rename(mes.date = PROC_START_DATE) %>%
    distinct_all() %>%
    left_join(dx) %>%
    mutate(mes.date = dmy(mes.date)) %>%
    filter(Diagnosis == "Ulcerative Colitis")



  #===============================
  #ALL SCORES
  #===============================

  sparc_scores = list(latest_diagnosis = dx, sCDAI = sCDAI, ucdai = ucdai, manitoba = manitoba, pga = pga, "SES-CD" = ses, "Mayo Endoscopy Score" = mes)

    return(sparc_scores)

  write.xlsx(sparc_scores, paste0("SPARC_scores_", Sys.Date(), ".xlsx"), colnames=T)


}
