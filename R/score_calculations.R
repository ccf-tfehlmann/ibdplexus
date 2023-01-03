# This file contains functions that calculate different disease activity and severity scores for SPARC for use in other functions.

#' calculate_ses
#'
#' Calculates SES-CD score from SPARC data. If two subscores are available for the same location on the same date, then the max score is chosen.
#'
#' @param procedures procedures table usually uploaded using load_data
#'
#' @return A dataframe with all ses scores that could be calculated regardless of IBD diagnosis.

#'
#'
calculate_ses <- function(procedures) {
  ses <- procedures %>%
    filter(DEIDENTIFIED_MASTER_PATIENT_ID == "27158555") %>%
    rename(SES.CD_Subscore = `SES-CD_Subscore`) %>%
    filter(!is.na(SES.CD_Subscore)) %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    mutate(SES.CD_Subscore = as.numeric(gsub("Not reached", "0", SES.CD_Subscore))) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, PROC_START_DATE, LOCATION) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, desc(SES.CD_Subscore)) %>%
    # slice(which.max(SES.CD_Subscore)) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, PROC_START_DATE, LOCATION, SES.CD_Subscore) %>%
    pivot_wider(
      id_cols = c("DEIDENTIFIED_MASTER_PATIENT_ID", "PROC_START_DATE"),
      names_from = LOCATION,
      values_from = SES.CD_Subscore,
      values_fn = ~max(.x)
    ) %>%
    mutate(SES_Score = `Ileum` + `Left colon` + Rectum + `Right colon` + `Transverse colon`) %>%
    drop_na(SES_Score) %>%
    mutate(SCORE_DATE = PROC_START_DATE) %>%
    distinct_all() %>%
    mutate(SCORE_DATE = dmy(PROC_START_DATE)) %>%
    ungroup() %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, SES_Score, SCORE_DATE, Ileum, `Left colon`, Rectum, `Right colon`, `Transverse colon`) %>%
    mutate(ses_category = case_when(
      SES_Score <= 2 ~ "Remission",
      SES_Score > 2 & SES_Score <= 6 ~ "Mild",
      SES_Score > 6 & SES_Score <= 15 ~ "Moderate",
      SES_Score > 15 ~ "Severe"
    )) %>%
    ungroup() %>%
    rename_with( .fn = function(x) {paste("SES_SUBSCORE_",x,sep="" )},
                 .cols = c("Rectum", "Transverse colon", "Right colon", "Ileum", "Left colon") ) %>%
    setNames(toupper(names(.))) %>%
    setNames(gsub("\\.", "_", names(.))) %>%
    setNames(gsub(" ", "_", names(.)))
}

#' calculate_mes
#'
#' Calculates Mayo Endoscopy score from SPARC data
#'
#' @param procedures procedures table usually uploaded using load_data
#'
#' @return A dataframe with all mes scores that could be calculated regardless of IBD diagnosis.

#'
#'
calculate_mes <- function(procedures) {
  mes <- procedures %>%
    filter(!is.na(MAYO_ENDOSCOPIC_SUBSCORE)) %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, MAX_EXTENT_ACTIVE_DISEASE, LOCATION, PROC_START_DATE, MAYO_ENDOSCOPIC_SUBSCORE) %>%
    mutate(MAYO_ENDOSCOPIC_SUBSCORE = as.numeric(gsub("Not seen", "0", MAYO_ENDOSCOPIC_SUBSCORE))) %>%
    drop_na(LOCATION) %>%
    filter(LOCATION != "") %>%
    pivot_wider(
      id_cols = c("DEIDENTIFIED_MASTER_PATIENT_ID", "PROC_START_DATE", "MAX_EXTENT_ACTIVE_DISEASE"),
      names_from = LOCATION,
      values_from = MAYO_ENDOSCOPIC_SUBSCORE
    ) %>%
    rowwise(DEIDENTIFIED_MASTER_PATIENT_ID, PROC_START_DATE, MAX_EXTENT_ACTIVE_DISEASE) %>%
    mutate(
      MAYO_ENDOSCOPY_SCORE = max(Rectum, `Sigmoid colon`, `Right colon`, `Descending colon`, `Transverse colon`),
      MODIFIED_MAYO_SCORE = sum(Rectum, `Sigmoid colon`, `Right colon`, `Descending colon`, `Transverse colon`),
      EXTENDED_MODIFIED_MAYO_SCORE = MODIFIED_MAYO_SCORE * (as.numeric(MAX_EXTENT_ACTIVE_DISEASE) / 10),
      MODIFIED_MAYO_ENDOSCOPIC_SCORE = ifelse(sum(Rectum > 0, `Sigmoid colon` > 0, `Right colon` > 0, `Descending colon` > 0, `Transverse colon` > 0) == 0, 0, EXTENDED_MODIFIED_MAYO_SCORE / sum(Rectum > 0, `Sigmoid colon` > 0, `Right colon` > 0, `Descending colon` > 0, `Transverse colon` > 0))
    ) %>%
    distinct_all() %>%
    mutate(SCORE_DATE = dmy(PROC_START_DATE)) %>%
    ungroup() %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, MAX_EXTENT_ACTIVE_DISEASE, MAYO_ENDOSCOPY_SCORE, MODIFIED_MAYO_SCORE, EXTENDED_MODIFIED_MAYO_SCORE, MODIFIED_MAYO_ENDOSCOPIC_SCORE, SCORE_DATE,
             Rectum, `Sigmoid colon`, `Right colon`, `Descending colon`, `Transverse colon`) %>%
    mutate(mes_catgory = case_when(
      MAYO_ENDOSCOPY_SCORE == 0 ~ "Remission",
      MAYO_ENDOSCOPY_SCORE == 1 ~ "Mild",
      MAYO_ENDOSCOPY_SCORE == 2 ~ "Moderate",
      MAYO_ENDOSCOPY_SCORE == 3 ~ "Severe"
    )) %>%
    setNames(toupper(names(.))) %>%
    setNames(gsub("\\.", "_", names(.))) %>%
    setNames(gsub(" ", "_", names(.)))
}


#' calculate_scdai
#'
#' Calculates short CDAI score from SPARC data.
#'
#' @param observations observation table usually uploaded using load_data
#'
#' @return A dataframe with all sCDAI scores from eCRF and Smartform regardless of IBD diagnosis.
#'
#'
calculate_scdai <- function(observations) {


  # Smartform

  scdai_sf <- observations %>%
    filter(DATA_SOURCE == "SF_SPARC") %>%
    filter(grepl("Abdominal Pain|General Well|Number of Daily Bowel Movements", OBS_TEST_CONCEPT_NAME, ignore.case = T)) %>%
    filter(OBS_TEST_CONCEPT_NAME != "Baseline Number of Daily Bowel Movements") %>%
    mutate(result = ifelse(is.na(DESCRIPTIVE_SYMP_TEST_RESULTS), TEST_RESULT_NUMERIC, DESCRIPTIVE_SYMP_TEST_RESULTS)) %>%
    mutate(result = case_when(
      result == "None" ~ 0,
      result == "Mild" ~ 1,
      result == "Moderate" ~ 2,
      result == "Severe" ~ 3,
      result == "Generally well" ~ 0,
      result == "Slightly under par" ~ 1,
      result == "Poor" ~ 2,
      result == "Very poor" ~ 3,
      result == "Terrible" ~ 4,
      TRUE ~ as.numeric(result)
    )) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, OBS_TEST_RESULT_DATE, OBS_TEST_CONCEPT_NAME, result) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE, OBS_TEST_CONCEPT_NAME) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_CONCEPT_NAME, desc(result)) %>%
    slice(1) %>%
    pivot_wider(
      id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE),
      names_from = c(OBS_TEST_CONCEPT_NAME),
      values_from = c(result)
    ) %>%
    mutate(
      A = ifelse(is.na(`Abdominal Pain - Pain Scale`), `Abdominal Pain Score`, `Abdominal Pain - Pain Scale`),
      B = ifelse(is.na(`Current Average Number of Daily Bowel Movements`), `Current Maximum Number of Daily Bowel Movements`, `Current Average Number of Daily Bowel Movements`),
      G = ifelse(is.na(`Constitutional - General Well-Being`), `Constitutional - General Well-Being`, `Constitutional - General Well-Being`),
      G = ifelse(is.na(G), `General Well Being Score`, G)
    ) %>%
    ungroup() %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(
      diff = OBS_TEST_RESULT_DATE - lag(OBS_TEST_RESULT_DATE),
      diff2 = lead(OBS_TEST_RESULT_DATE) - OBS_TEST_RESULT_DATE
    ) %>%
    # mutate(diff = if_else(is.na(diff), 0, as.numeric(diff))) %>%
    ungroup() %>%
    mutate(Daily.BM.Question = case_when(
      B == `Current Maximum Number of Daily Bowel Movements` ~ "Current Maximum Number of Daily Bowel Movements",
      B == `Current Average Number of Daily Bowel Movements` ~ "Current Average Number of Daily Bowel Movements",
      TRUE ~ as.character(NA)
    )) %>%
     mutate(
      A = ifelse(is.na(A) & diff <= 7, lag(A), A),
      G = ifelse(is.na(G) & diff <= 7, lag(G), G),
      B = ifelse(is.na(B) & diff <= 7, lag(B), B),
      Daily.BM.Question = ifelse(is.na(Daily.BM.Question) & (diff <= 7), lag(Daily.BM.Question), Daily.BM.Question)
    ) %>%
    mutate(
      A = ifelse(is.na(A) & (diff2 <= 7), lead(A), A),
      G = ifelse(is.na(G) & (diff2 <= 7), lead(G), G),
      B = ifelse(is.na(B) & (diff2 <= 7), lead(B), B),
      Daily.BM.Question = ifelse(is.na(Daily.BM.Question) & (diff2 <= 7), lead(Daily.BM.Question), Daily.BM.Question)
    ) %>%
    dplyr::rename(Daily.BM = B, Abdominal.Pain.Score = A, General.well.being.score = G) %>%
    mutate(sCDAI.score = 44 + (2 * 7 * Daily.BM) + (5 * 7 * Abdominal.Pain.Score) + (7 * 7 * General.well.being.score), Source = "SF") %>%
    dplyr::rename(sCDAI.date = OBS_TEST_RESULT_DATE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, sCDAI.date, sCDAI.score, Source, Daily.BM, Abdominal.Pain.Score, General.well.being.score, Daily.BM.Question)


  # ECRF
  scdai_ecrf <- observations %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    filter(grepl("Abdominal Pain|General Well|Bowel Movements", OBS_TEST_CONCEPT_NAME, ignore.case = T)) %>%
    filter(OBS_TEST_CONCEPT_NAME != "Baseline Number of Daily Bowel Movements") %>%
    mutate(result = ifelse(is.na(DESCRIPTIVE_SYMP_TEST_RESULTS), TEST_RESULT_NUMERIC, DESCRIPTIVE_SYMP_TEST_RESULTS)) %>%
    mutate(result = case_when(
      result == "None" ~ 0,
      result == "Mild" ~ 1,
      result == "Moderate" ~ 2,
      result == "Severe" ~ 3,
      result == "Generally well" ~ 0,
      result == "Slightly under par" ~ 1,
      result == "Poor" ~ 2,
      result == "Very poor" ~ 3,
      result == "Terrible" ~ 4,
      result == "20+" ~ 20,
      TRUE ~ as.numeric(result)
    )) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE, OBS_TEST_CONCEPT_NAME, result) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE, OBS_TEST_CONCEPT_NAME) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_CONCEPT_NAME, desc(result)) %>%
    slice(1) %>%
    pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE), names_from = c(OBS_TEST_CONCEPT_NAME), values_from = c(result)) %>%
    mutate(aug18 = ifelse(OBS_TEST_RESULT_DATE < as.Date("01-AUG-2018", format = "%d-%b-%Y"), "before", "after")) %>%
    mutate(
      A = `Abdominal Pain`,
      B = if_else(aug18 == "before" & !is.na(`Current Average Number of Daily Bowel Movements`), `Current Average Number of Daily Bowel Movements`, `Current Average Number of Daily Liquid Bowel Movements`),
      B = ifelse(is.na(B), `Current Average Number of Daily Bowel Movements`, B),
      G = `General Well-Being`
    ) %>%
    ungroup() %>%
    mutate(Daily.BM.Question = case_when(
      B == `Current Average Number of Daily Liquid Bowel Movements` ~ "Current Average Number of Daily Liquid Bowel Movements",
      B == `Current Average Number of Daily Bowel Movements` ~ "Current Average Number of Daily Bowel Movements",
      TRUE ~ as.character(NA)
    )) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(
      diff = OBS_TEST_RESULT_DATE - lag(OBS_TEST_RESULT_DATE),
      diff2 = lead(OBS_TEST_RESULT_DATE) - OBS_TEST_RESULT_DATE
    ) %>%
    # mutate(diff = if_else(is.na(diff), 0, as.numeric(diff))) %>%
    ungroup() %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(
      A = ifelse(is.na(A) & diff <= 7, lag(A), A),
      G = ifelse(is.na(G) & diff <= 7, lag(G), G),
      B = ifelse(is.na(B) & diff <= 7, lag(B), B),
      Daily.BM.Question = ifelse(is.na(Daily.BM.Question) & (diff <= 7), lag(Daily.BM.Question), Daily.BM.Question)
    ) %>%
    mutate(
      A = ifelse(is.na(A) & (diff2 <= 7), lead(A), A),
      G = ifelse(is.na(G) & (diff2 <= 7), lead(G), G),
      B = ifelse(is.na(B) & (diff2 <= 7), lead(B), B),
      Daily.BM.Question = ifelse(is.na(Daily.BM.Question) & (diff2 <= 7), lead(Daily.BM.Question), Daily.BM.Question)
    ) %>%
    dplyr::rename(Daily.BM = B, Abdominal.Pain.Score = A, General.well.being.score = G) %>%
    mutate(sCDAI.score = 44 + (2 * 7 * Daily.BM) + (5 * 7 * Abdominal.Pain.Score) + (7 * 7 * General.well.being.score), Source = "ECRF") %>%
    dplyr::rename(sCDAI.date = OBS_TEST_RESULT_DATE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, sCDAI.date, sCDAI.score, Source, Daily.BM, Abdominal.Pain.Score, General.well.being.score, Daily.BM.Question)



  sCDAI <- scdai_sf %>%
    bind_rows(scdai_ecrf) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, desc(sCDAI.score), sCDAI.date) %>%
    dplyr::rename(scdai.source = Source) %>%
    distinct_all() %>%
    mutate(Daily.BM.Version = case_when(
      Daily.BM.Question == "Current Average Number of Daily Bowel Movements" ~ 1,
      Daily.BM.Question == "Current Average Number of Daily Liquid Bowel Movements" ~ 2,
      Daily.BM.Question == "Current Maximum Number of Daily Bowel Movements" ~ 3
    )) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, desc(sCDAI.score)) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, sCDAI.date, sCDAI.score, scdai.source, Daily.BM, Daily.BM.Version, Abdominal.Pain.Score, General.well.being.score) %>%
    ungroup() %>%
    select(sort(names(.))) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, everything()) %>%
    dplyr::mutate(sCDAI.category = case_when(
      sCDAI.score < 150 ~ "Remission",
      sCDAI.score >= 150 & sCDAI.score <= 219 ~ "Mild",
      sCDAI.score >= 220 & sCDAI.score <= 450 ~ "Moderate",
      sCDAI.score > 450 ~ "Severe",
      TRUE ~ as.character(NA)
    )) %>%
    setNames(toupper(names(.))) %>%
    setNames(gsub("\\.", "_", names(.)))
}


#' calculate_mayo
#'
#' Calculates 6pt & 9pt mayo scores from SPARC data.
#'
#' @param observations observation table usually uploaded using load_data
#'
#' @return A dataframe with all 6pt & 9pt scores from eCRF and Smartform regardless of IBD diagnosis.
#'
#'
calculate_mayo <- function(observations) {

  # Smartform

  ucdai_sf <- observations %>%
    filter(DATA_SOURCE == "SF_SPARC") %>%
    filter(grepl("Stool Frequency|Blood in Stool|Global Assessment", OBS_TEST_CONCEPT_NAME, ignore.case = T)) %>%
    mutate(result = ifelse(is.na(DESCRIPTIVE_SYMP_TEST_RESULTS), TEST_RESULT_NUMERIC, DESCRIPTIVE_SYMP_TEST_RESULTS)) %>%
    mutate(result = case_when(
      result == "Normal" ~ 0,
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
      TRUE ~ as.numeric(result)
    )) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE, OBS_TEST_CONCEPT_NAME, result) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE, OBS_TEST_CONCEPT_NAME) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_CONCEPT_NAME, desc(result)) %>%
    slice(1) %>%
    pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE), names_from = c(OBS_TEST_CONCEPT_NAME), values_from = c(result)) %>%
    mutate(
      T = ifelse(is.na(`Physician's Global Assessment of Current Disease Status`), `Inflammatory Bowel Disease - Global Assessment Score`, `Physician's Global Assessment of Current Disease Status`),
      R = `Blood in Stool - Recent Change in Rectal Bleeding Amount`,
      S = ifelse(is.na(`Recent Change in Daily Stool Frequency`), `Stool Frequency Score`, `Recent Change in Daily Stool Frequency`)
    ) %>%
    ungroup() %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(
      diff = OBS_TEST_RESULT_DATE - lag(OBS_TEST_RESULT_DATE),
      diff2 = lead(OBS_TEST_RESULT_DATE) - OBS_TEST_RESULT_DATE
    ) %>%
    ungroup() %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(
      T = ifelse(is.na(T) & (diff <= 7), lag(T), T),
      S = ifelse(is.na(S) & (diff <= 7), lag(S), S),
      R = ifelse(is.na(R) & (diff <= 7), lag(R), R)
    ) %>%
    mutate(
      T = ifelse(is.na(T) & (diff2 <= 7), lead(T), T),
      S = ifelse(is.na(S) & (diff2 <= 7), lead(S), S),
      R = ifelse(is.na(R) & (diff2 <= 7), lead(R), R)
    ) %>%
    dplyr::rename(Stool.Freq.Score = S, Rectal.Bleeding.Score = R, Global.Assessment.Score = T) %>%
    mutate(
      UCDAI.6.score = Stool.Freq.Score + Rectal.Bleeding.Score,
      UCDAI.9.score = Stool.Freq.Score + Rectal.Bleeding.Score + Global.Assessment.Score, Source = "SF"
    ) %>%
    mutate(UCDAI.date = OBS_TEST_RESULT_DATE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, UCDAI.6.score, UCDAI.9.score, UCDAI.date, Source, Stool.Freq.Score, Rectal.Bleeding.Score, Global.Assessment.Score)


  # ECRF

  ucdai_ecrf <- observations %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    filter(grepl("Stool|Blood", OBS_TEST_CONCEPT_NAME, ignore.case = T)) %>%
    mutate(result = ifelse(is.na(DESCRIPTIVE_SYMP_TEST_RESULTS), TEST_RESULT_NUMERIC, DESCRIPTIVE_SYMP_TEST_RESULTS)) %>%
    mutate(result = case_when(
      result %in% c("Normal", "Normal (same as when I am well)", "Less stool than normal") ~ 0,
      result == "1-2 stools/day more than normal" ~ 1,
      result == "3-4 stools/day more than normal" ~ 2,
      result == "5 or more stools per day more than normal" ~ 3,
      result == "No blood seen" ~ 0,
      result == "Blood less than 50% of the time" ~ 1,
      OBS_TEST_CONCEPT_NAME == "Blood in Stool" & result == "Blood 50% or more of the time" ~ 2,
      OBS_TEST_CONCEPT_NAME == "Blood Passed Alone" & result %in% c("Yes", "Blood 50% or more of the time") ~ 3,
      TRUE ~ as.numeric(result)
    )) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE, OBS_TEST_CONCEPT_NAME, result) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE, OBS_TEST_CONCEPT_NAME) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_CONCEPT_NAME, desc(result)) %>%
    slice(1) %>%
    pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE), names_from = c(OBS_TEST_CONCEPT_NAME), values_from = c(result)) %>%
    mutate(
      R = ifelse(is.na(`Blood Passed Alone`), `Blood in Stool`, `Blood Passed Alone`),
      S = `Recent Change in Daily Stool Frequency`
    ) %>%
    ungroup() %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(
      diff = OBS_TEST_RESULT_DATE - lag(OBS_TEST_RESULT_DATE),
      diff2 = lead(OBS_TEST_RESULT_DATE) - OBS_TEST_RESULT_DATE
    ) %>%
    # mutate(diff = if_else(is.na(diff), 0, as.numeric(diff))) %>%
    ungroup() %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(
      S = ifelse(is.na(S) & (diff <= 7), lag(S), S),
      R = ifelse(is.na(R) & (diff <= 7), lead(R), R)
    ) %>%
    mutate(
      S = ifelse(is.na(S) & (diff2 <= 7), lead(S), S),
      R = ifelse(is.na(R) & (diff2 <= 7), lead(R), R)
    ) %>%
    dplyr::rename(Stool.Freq.Score = S, Rectal.Bleeding.Score = R) %>%
    mutate(UCDAI.6.score = Stool.Freq.Score + Rectal.Bleeding.Score, Source = "ECRF") %>%
    mutate(UCDAI.date = OBS_TEST_RESULT_DATE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, UCDAI.6.score, UCDAI.date, Source, Stool.Freq.Score, Rectal.Bleeding.Score)

  ucdai <- bind_rows(ucdai_sf, ucdai_ecrf) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, UCDAI.date) %>%
    dplyr::rename(ucdai.source = Source) %>%
    distinct_all() %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, desc(UCDAI.6.score)) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, UCDAI.6.score, UCDAI.9.score, UCDAI.date, ucdai.source, Stool.Freq.Score, Rectal.Bleeding.Score, Global.Assessment.Score) %>%
    ungroup() %>%
    select(sort(names(.))) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, everything()) %>%
    dplyr::mutate(mayo6.category = case_when(
      UCDAI.6.score < 2 ~ "Remission",
      UCDAI.6.score >= 2 & UCDAI.6.score <= 3 ~ "Mild",
      UCDAI.6.score >= 4 & UCDAI.6.score <= 5 ~ "Moderate",
      UCDAI.6.score >= 6 ~ "Severe",
      TRUE ~ as.character(NA)
    )) %>%
    setNames(toupper(names(.))) %>%
    setNames(gsub("UCDAI", "MAYO", names(.))) %>%
    setNames(gsub("\\.", "_", names(.)))
}


#' calculate_manitoba
#'
#' Calculates manitoba score from SPARC data.
#'
#' @param observations observation table usually uploaded using load_data
#'
#' @return A dataframe with all manitoba from eCRF  regardless of IBD diagnosis.
#'
#'
#'
#'
calculate_manitoba <- function(observations) {
  manitoba <- observations %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    filter(OBS_TEST_CONCEPT_CODE %in% c("DISEASE_ACTIVITY__C") &
      !is.na(DESCRIPTIVE_SYMP_TEST_RESULTS)) %>%
    dplyr::rename(Manitoba = DESCRIPTIVE_SYMP_TEST_RESULTS) %>%
    mutate(Manitoba.date = (OBS_TEST_RESULT_DATE)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, Manitoba, Manitoba.date) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, Manitoba.date) %>%
    mutate(Manitoba.source = "ECRF") %>%
    distinct_all() %>%
    setNames(toupper(names(.))) %>%
    setNames(gsub("\\.", "_", names(.)))
}



#' calculate_pga
#'
#' Calculates PGA score from SPARC data.
#'
#' @param observations observation table usually uploaded using load_data
#'
#' @return A dataframe with all PGA from Smartform  regardless of IBD diagnosis.
#'
#'
#'
#'
calculate_pga <- function(observations) {
  pga <- observations %>%
    filter(DATA_SOURCE == "SF_SPARC") %>%
    filter(OBS_TEST_CONCEPT_CODE %in% c("EPIC#16411", "SMART_Q9__C") &
      !is.na(DESCRIPTIVE_SYMP_TEST_RESULTS)) %>%
    dplyr::rename(PGA = DESCRIPTIVE_SYMP_TEST_RESULTS) %>%
    mutate(PGA.date = (OBS_TEST_RESULT_DATE)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, PGA, PGA.date) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, PGA.date) %>%
    mutate(PGA.source = "SF") %>%
    distinct_all() %>%
    setNames(toupper(names(.))) %>%
    setNames(gsub("\\.", "_", names(.)))
}


#' calculate_pro2
#'
#' Calculates pro2 from SPARC data.
#'
#' @param observations observation table usually uploaded using load_data
#'
#' @return A dataframe with all pro2 scores from eCRF and Smartform regardless of IBD diagnosis.
#'
#'
calculate_pro2 <- function(observations) {

  #remission/mild/moderate/severe definition from https://pubmed.ncbi.nlm.nih.gov/25348809/

  # Smartform

  pro2_sf <- observations %>%
    filter(DATA_SOURCE == "SF_SPARC") %>%
    filter(OBS_TEST_CONCEPT_NAME %in% c("Current Average Number of Daily Liquid Bowel Movements", "Abdominal Pain Score", "Abdominal Pain - Pain Scale")) %>%
    mutate(result = ifelse(is.na(DESCRIPTIVE_SYMP_TEST_RESULTS), TEST_RESULT_NUMERIC, DESCRIPTIVE_SYMP_TEST_RESULTS)) %>%
    mutate(result = case_when(
      result == "None" ~ 0,
      result == "Mild" ~ 1,
      result == "Moderate" ~ 2,
      result == "Severe" ~ 3,
      TRUE ~ as.numeric(result)
    )) %>%
    mutate(result = ifelse(result > 20, 20, result)) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, OBS_TEST_RESULT_DATE, OBS_TEST_CONCEPT_NAME, result) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE, OBS_TEST_CONCEPT_NAME) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_CONCEPT_NAME, desc(result)) %>%
    slice(1) %>%
    pivot_wider(
      id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE),
      names_from = c(OBS_TEST_CONCEPT_NAME),
      values_from = c(result)
    ) %>%
    mutate(
      A = ifelse(is.na(`Abdominal Pain - Pain Scale`), `Abdominal Pain Score`, `Abdominal Pain - Pain Scale`),
      B = `Current Average Number of Daily Liquid Bowel Movements`
    ) %>%
    ungroup() %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(
      diff = OBS_TEST_RESULT_DATE - lag(OBS_TEST_RESULT_DATE),
      diff2 = lead(OBS_TEST_RESULT_DATE) - OBS_TEST_RESULT_DATE
    ) %>%
    ungroup() %>%
    mutate(
      A = ifelse(is.na(A) & diff <= 7, lag(A), A),
      B = ifelse(is.na(B) & diff <= 7, lag(B), B)
    ) %>%
    mutate(
      A = ifelse(is.na(A) & (diff2 <= 7), lead(A), A),
      B = ifelse(is.na(B) & (diff2 <= 7), lead(B), B)
    ) %>%
    dplyr::rename(Liquid.BM = B, Abdominal.Pain.Score = A) %>%
    mutate(pro2.score = (Liquid.BM*2) + (Abdominal.Pain.Score*5), Source = "SF") %>%
    dplyr::rename(pro2.date = OBS_TEST_RESULT_DATE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, pro2.date, pro2.score, Source, Liquid.BM, Abdominal.Pain.Score)


  # ECRF
  pro2_ecrf <- observations %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    filter(OBS_TEST_CONCEPT_NAME %in% c("Current Average Number of Daily Liquid Bowel Movements", "Abdominal Pain")) %>%
    mutate(result = ifelse(is.na(DESCRIPTIVE_SYMP_TEST_RESULTS), TEST_RESULT_NUMERIC, DESCRIPTIVE_SYMP_TEST_RESULTS)) %>%
    mutate(result = ifelse(is.na(DESCRIPTIVE_SYMP_TEST_RESULTS), TEST_RESULT_NUMERIC, DESCRIPTIVE_SYMP_TEST_RESULTS)) %>%
    mutate(result = case_when(
      result == "None" ~ 0,
      result == "Mild" ~ 1,
      result == "Moderate" ~ 2,
      result == "Severe" ~ 3,
      result == "20+" ~ 20,
      TRUE ~ as.numeric(result)
    )) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE, OBS_TEST_CONCEPT_NAME, result) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE, OBS_TEST_CONCEPT_NAME) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_CONCEPT_NAME, desc(result)) %>%
    slice(1) %>%
    pivot_wider(
      id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE),
      names_from = c(OBS_TEST_CONCEPT_NAME),
      values_from = c(result)
    ) %>%
    mutate(
      A = `Abdominal Pain`,
      B = `Current Average Number of Daily Liquid Bowel Movements`
    ) %>%
    ungroup() %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(
      diff = OBS_TEST_RESULT_DATE - lag(OBS_TEST_RESULT_DATE),
      diff2 = lead(OBS_TEST_RESULT_DATE) - OBS_TEST_RESULT_DATE
    ) %>%
    ungroup() %>%
    mutate(
      A = ifelse(is.na(A) & diff <= 7, lag(A), A),
      B = ifelse(is.na(B) & diff <= 7, lag(B), B)
    ) %>%
    mutate(
      A = ifelse(is.na(A) & (diff2 <= 7), lead(A), A),
      B = ifelse(is.na(B) & (diff2 <= 7), lead(B), B)
    ) %>%
    dplyr::rename(Liquid.BM = B, Abdominal.Pain.Score = A) %>%
    mutate(pro2.score = (Liquid.BM*2) + (Abdominal.Pain.Score*5), Source = "ECRF") %>%
    dplyr::rename(pro2.date = OBS_TEST_RESULT_DATE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, pro2.date, pro2.score, Source, Liquid.BM, Abdominal.Pain.Score)



  pro2 <- pro2_sf %>%
    bind_rows(pro2_ecrf) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID,  pro2.date) %>%
    dplyr::rename(pro2.source = Source) %>%
    distinct_all() %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, pro2.date, pro2.score, pro2.source, Liquid.BM, Abdominal.Pain.Score) %>%
    ungroup() %>%
    dplyr::mutate(pro2.category = case_when(
      pro2.score < 8 ~ "Remission",
      pro2.score >= 8 & pro2.score <= 13 ~ "Mild",
      pro2.score >= 14 & pro2.score <= 35 ~ "Moderate",
      pro2.score > 35 ~ "Severe",
      TRUE ~ as.character(NA)
    )) %>%
    select(sort(names(.))) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, everything()) %>%
    setNames(toupper(names(.))) %>%
    setNames(gsub("\\.", "_", names(.)))
}

#' calculate_pro3
#'
#' Calculates pro3 from SPARC data.
#'
#' @param observations observation table usually uploaded using load_data
#'
#' @return A dataframe with all pro3 scores from eCRF and Smartform regardless of IBD diagnosis.
#'
#'
calculate_pro3 <- function(observations) {

  #remission/mild/moderate/severe definition from https://pubmed.ncbi.nlm.nih.gov/25348809/

  # Smartform
  pro3_sf <- observations %>%
    filter(DATA_SOURCE == "SF_SPARC") %>%
    filter(OBS_TEST_CONCEPT_NAME %in% c("Current Average Number of Daily Liquid Bowel Movements", "Abdominal Pain Score", "Abdominal Pain - Pain Scale","General Well Being Score","Constitutional - General Well-Being")) %>%
    mutate(result = ifelse(is.na(DESCRIPTIVE_SYMP_TEST_RESULTS), TEST_RESULT_NUMERIC, DESCRIPTIVE_SYMP_TEST_RESULTS)) %>%
    mutate(result = case_when(
        result == "None" ~ 0,
        result == "Mild" ~ 1,
        result == "Moderate" ~ 2,
        result == "Severe" ~ 3,
        result == "Generally well" ~ 0,
        result == "Slightly under par" ~ 1,
        result == "Poor" ~ 2,
        result == "Very poor" ~ 3,
        result == "Terrible" ~ 4,
        TRUE ~ as.numeric(result)
      )) %>%
    mutate(result = ifelse(result > 20, 20, result)) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, OBS_TEST_RESULT_DATE, OBS_TEST_CONCEPT_NAME, result) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE, OBS_TEST_CONCEPT_NAME) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_CONCEPT_NAME, desc(result)) %>%
    slice(1) %>%
    pivot_wider(
      id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE),
      names_from = c(OBS_TEST_CONCEPT_NAME),
      values_from = c(result)
    ) %>%
    mutate(
      A = ifelse(is.na(`Abdominal Pain - Pain Scale`), `Abdominal Pain Score`, `Abdominal Pain - Pain Scale`),
      B = `Current Average Number of Daily Liquid Bowel Movements`,
      G =  ifelse(is.na(`Constitutional - General Well-Being`), `General Well Being Score`, `Constitutional - General Well-Being`)
    ) %>%
    ungroup() %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(
      diff = OBS_TEST_RESULT_DATE - lag(OBS_TEST_RESULT_DATE),
      diff2 = lead(OBS_TEST_RESULT_DATE) - OBS_TEST_RESULT_DATE
    ) %>%
    ungroup() %>%
    mutate(
      A = ifelse(is.na(A) & diff <= 7, lag(A), A),
      B = ifelse(is.na(B) & diff <= 7, lag(B), B),
      G = ifelse(is.na(G) & diff <= 7, lag(G), G)
    ) %>%
    mutate(
      A = ifelse(is.na(A) & (diff2 <= 7), lead(A), A),
      B = ifelse(is.na(B) & (diff2 <= 7), lead(B), B),
      G = ifelse(is.na(G) & (diff2 <= 7), lead(G), G)
    ) %>%
    dplyr::rename(Liquid.BM = B, Abdominal.Pain.Score = A,General.well.being.score = G) %>%
    mutate(pro3.score = (Liquid.BM*2) + (Abdominal.Pain.Score*5) + (General.well.being.score*7), Source = "SF") %>%
    dplyr::rename(pro3.date = OBS_TEST_RESULT_DATE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, pro3.date, pro3.score, Source, Liquid.BM, Abdominal.Pain.Score,General.well.being.score)


  # ECRF
  pro3_ecrf <- observations %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    filter(OBS_TEST_CONCEPT_NAME %in% c("Current Average Number of Daily Liquid Bowel Movements", "Abdominal Pain", "General Well-Being")) %>%
    mutate(result = ifelse(is.na(DESCRIPTIVE_SYMP_TEST_RESULTS), TEST_RESULT_NUMERIC, DESCRIPTIVE_SYMP_TEST_RESULTS)) %>%
    mutate(result = ifelse(is.na(DESCRIPTIVE_SYMP_TEST_RESULTS), TEST_RESULT_NUMERIC, DESCRIPTIVE_SYMP_TEST_RESULTS)) %>%
    mutate(result = case_when(
      result == "None" ~ 0,
      result == "Mild" ~ 1,
      result == "Moderate" ~ 2,
      result == "Severe" ~ 3,
      result == "Generally well" ~ 0,
      result == "Slightly under par" ~ 1,
      result == "Poor" ~ 2,
      result == "Very poor" ~ 3,
      result == "Terrible" ~ 4,
      result == "20+" ~ 20,
      TRUE ~ as.numeric(result)
    )) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE, OBS_TEST_CONCEPT_NAME, result) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE, OBS_TEST_CONCEPT_NAME) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_CONCEPT_NAME, desc(result)) %>%
    slice(1) %>%
    pivot_wider(
      id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE),
      names_from = c(OBS_TEST_CONCEPT_NAME),
      values_from = c(result)
    ) %>%
    mutate(
      A = `Abdominal Pain`,
      B = `Current Average Number of Daily Liquid Bowel Movements`,
      G = `General Well-Being`
    ) %>%
    ungroup() %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(
      diff = OBS_TEST_RESULT_DATE - lag(OBS_TEST_RESULT_DATE),
      diff2 = lead(OBS_TEST_RESULT_DATE) - OBS_TEST_RESULT_DATE
    ) %>%
    ungroup() %>%
    mutate(
      A = ifelse(is.na(A) & diff <= 7, lag(A), A),
      B = ifelse(is.na(B) & diff <= 7, lag(B), B),
      G = ifelse(is.na(G) & diff <= 7, lag(G), G)
    ) %>%
    mutate(
      A = ifelse(is.na(A) & (diff2 <= 7), lead(A), A),
      B = ifelse(is.na(B) & (diff2 <= 7), lead(B), B),
      G = ifelse(is.na(G) & (diff2 <= 7), lead(G), G)
    ) %>%
    dplyr::rename(Liquid.BM = B, Abdominal.Pain.Score = A,General.well.being.score = G) %>%
    mutate(pro3.score = (Liquid.BM*2) + (Abdominal.Pain.Score*5) + (General.well.being.score*7), Source = "ECRF") %>%
    dplyr::rename(pro3.date = OBS_TEST_RESULT_DATE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, pro3.date, pro3.score, Source, Liquid.BM, Abdominal.Pain.Score,General.well.being.score)


  pro3 <- pro3_sf %>%
    bind_rows(pro3_ecrf) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID,  pro3.date) %>%
    dplyr::rename(pro3.source = Source) %>%
    distinct_all() %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, pro3.date, pro3.score, pro3.source, Liquid.BM, Abdominal.Pain.Score,General.well.being.score) %>%
  ungroup() %>%
    dplyr::mutate(pro3.category = case_when(
      pro3.score < 13 ~ "Remission",
      pro3.score >= 13 & pro3.score <= 21 ~ "Mild",
      pro3.score >= 22 & pro3.score <= 53 ~ "Moderate",
      pro3.score > 53 ~ "Severe",
      TRUE ~ as.character(NA)
    )) %>%
    select(sort(names(.))) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, everything()) %>%
    setNames(toupper(names(.))) %>%
    setNames(gsub("\\.", "_", names(.)))
}
