#' sparc_sf
#'
#' Summarizes the Smartform data at different time points for SPARC patients.
#'
#' @param datadir Directory where unzipped data is saved.
#' @param index_info The time period of interest as either a default option or dataframe with two columns: DEIDENTIFIED_MASTER_PATIENT_ID, and index_date formatted as a date.
#' \itemize{
#'   \item ALL: All Smartform data. Each row will be a smartform entry.
#'   \item ENROLLMENT: Smartform data within 30 days of enrollment with one row per patient in the final table.
#'   \item LATEST: The latest Smartform data for each patient. If there are no changes at a patient visit, the Smartform is not updated and data is pulled forward from the previous visit.
#'   \item ENDOSCOPY: Smartform data within 30 days of endoscopy with one row per endoscopy visit in the final table.
#' }
#' @param filename the name of the output file. Must be .xlsx.
#' @param index_range If index date is specified, the number of days to look out from index date.

#'
#' @return A dataframe and an excel file.
#' @export

sparc_sf <- function(datadir,
                             index_info = c("ALL", "ENROLLMENT", "LATEST", "ENDOSCOPY"),
                             filename = "SMARTFORM_SUMMARY.xlsx"){


  if(class(index_info) == "character"){index_info = toupper(index_info)} else {index_info = index_info}

# LOAD FILES ----

  data = load_data(datadir = datadir,
                   cohort = "SPARC",
                   domains = c("ALL"),
                   data_type = "CRF")

  data = data[sapply(data, nrow)>1]


# DEMOGRAPHIC INFORMATION ----

  consent = data$demographics %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DATE_OF_CONSENT, DATE_OF_CONSENT_WITHDRAWN) %>%
    mutate(DATE_OF_CONSENT = dmy(DATE_OF_CONSENT),
           DATE_OF_CONSENT_WITHDRAWN = dmy(DATE_OF_CONSENT_WITHDRAWN)) %>%
    filter(year(DATE_OF_CONSENT) >= 2016) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    slice(which.min(DATE_OF_CONSENT)) %>%
    ungroup()

  demo = data$demographics %>%
    filter(DATA_SOURCE %in% c("ECRF_SPARC")) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    slice(1) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, BIRTH_YEAR) %>%
    ungroup()

  cohort = full_join(consent, demo)


# LATEST DIAGNOSIS ----


  dx = data$diagnosis %>%
    filter(DATA_SOURCE %in% c("SF_SPARC", "ECRF_SPARC", "ECRF_QORUS")) %>%
    filter(DIAG_CONCEPT_NAME %in% c("Crohn's Disease", "IBD Unclassified", "Ulcerative Colitis")) %>%
    left_join(data$encounter) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(DIAGNOSIS = DIAG_CONCEPT_NAME) %>%
    dplyr::mutate(keep = ifelse(DATA_SOURCE == "SF_SPARC" & is.na(DIAG_STATUS_CONCEPT_NAME), 0, 1)) %>% #Smartform Data should have a DIAG_STATUS_CONCEPT_NAME equal to yes
    filter(keep ==1) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, match(DATA_SOURCE, c("SF_SPARC","ECRF_SPARC")), desc(dmy(VISIT_ENCOUNTER_START_DATE))) %>%
    slice(1) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, DIAGNOSIS)


  cohort = full_join(cohort, dx)

# DIAGNOSIS DATE ----



  dx_date_ecrf = data$diagnosis %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    drop_na(DIAGNOSIS_DATE) %>%
    mutate(DIAGNOSIS_DATE = dmy(DIAGNOSIS_DATE)) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    slice(which.min(DIAGNOSIS_DATE)) %>%
    mutate(DIAGNOSIS_DATE = as.numeric(year(DIAGNOSIS_DATE))) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID,DIAGNOSIS_DATE)

  dx_date_sf = data$diagnosis %>%
    filter(DATA_SOURCE == "SF_SPARC") %>%
    filter(DIAG_CONCEPT_NAME %in% c("Crohn's Disease", "IBD Unclassified", "Ulcerative Colitis","Inflammatory Bowel Disease")) %>%
    drop_na(DIAGNOSIS_DATE) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    slice(which.min(as.numeric(DIAGNOSIS_DATE))) %>%
    mutate(DIAGNOSIS_DATE = as.numeric(DIAGNOSIS_DATE)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, DIAGNOSIS_DATE)

  dxy = bind_rows(dx_date_sf, dx_date_ecrf) %>%
    left_join(data$demographics) %>%
    filter(DIAGNOSIS_DATE >= as.numeric(BIRTH_YEAR)) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, DIAGNOSIS_DATE) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    slice(which.min(DIAGNOSIS_DATE)) %>%
    ungroup() %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DIAGNOSIS_DATE)

  cohort = cohort %>%
    left_join(dxy)


# NUMBER OF SMARTFORMS PER INDIVIDUAL   ----

  #Combine those that are within 7 days of each other

  sfn = data$encounter %>%
    filter(DATA_SOURCE == "SF_SPARC") %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_START_DATE) %>%
    mutate(VISIT_ENCOUNTER_START_DATE = dmy(VISIT_ENCOUNTER_START_DATE)) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_START_DATE) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(t = VISIT_ENCOUNTER_START_DATE - lag(VISIT_ENCOUNTER_START_DATE)) %>%
    filter(t >= 7 | is.na(t)) %>%
    count() %>%
    rename("SF_COUNT" = n) %>%
    ungroup

  cohort = full_join(cohort, sfn) %>%
    mutate(across(c("SF_COUNT"), ~ replace_na(.x, 0)))

# GET ALL SMARTFORM DATA  ----

  sf_dates = data$encounter %>%
    filter(DATA_SOURCE == "SF_SPARC") %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, VISIT_ENCOUNTER_START_DATE) %>%
    mutate(VISIT_ENCOUNTER_START_DATE = dmy(VISIT_ENCOUNTER_START_DATE)) %>%
    rename(index_date = VISIT_ENCOUNTER_START_DATE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID)

  sf = NULL

  for(i in 1:length(data)){

    if("DATA_SOURCE" %in% names(data[[i]])){ sf[[i]] = data[[i]] %>%
      filter(DATA_SOURCE == "SF_SPARC") %>%
      left_join(sf_dates,Joining, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "VISIT_ENCOUNTER_ID")) %>%
      drop_na(VISIT_ENCOUNTER_ID) %>%
      mutate(across(everything(), ~replace(., . %in% c("N.A.", "NA", "N/A", ""), NA)))} else {sf[[i]] = NULL}

  }


  names(sf) = names(data)

  sf = Filter(length, sf)
  sf = sf[sapply(sf, nrow)>0]

#CLEAN  DATA ----

  #Diagnosis
  sf$diagnosis <- sf$diagnosis %>% filter(!is.na(DIAG_STATUS_CONCEPT_NAME))
  sf$diagnosis = sf$diagnosis %>%
    filter(!(DIAG_CONCEPT_NAME %in% c("Crohn's Disease", "IBD Unclassified", "Ulcerative Colitis","Inflammatory Bowel Disease"))) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, match(DIAG_STATUS_CONCEPT_NAME, c("Yes", "No"))) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID,DIAG_CONCEPT_NAME) %>%
    slice(1) %>%
    ungroup()



  #Labs
  sf$labs <- sf$labs %>%
    filter(!is.na(LAB_RESULTS)) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, match(LAB_RESULTS, c("Postive", "Negative"))) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID,LAB_TEST_CONCEPT_NAME) %>%
    slice(1) %>%
    ungroup()

  #Prescriptions
  sf$prescriptions <- sf$prescriptions %>%
    drop_na(MEDICATION_ADMINISTRATED) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, match(MEDICATION_ADMINISTRATED, c("Ever", "Never"))) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID,MEDICATION_NAME) %>%
    slice(1) %>%
    ungroup()

  #Observations
  sf$observations <- sf$observations %>% filter(!is.na(DESCRIPTIVE_SYMP_TEST_RESULTS) | !is.na(TEST_RESULT_NUMERIC))
  #Select descriptive test results if numeric test results are not available
  sf$observations$TEST_RESULT_NUMERIC[is.na(sf$observations$TEST_RESULT_NUMERIC)] <- sf$observations$DESCRIPTIVE_SYMP_TEST_RESULTS[is.na(sf$observations$TEST_RESULT_NUMERIC)]

  sf$observations =  sf$observations %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, match(TEST_RESULT_NUMERIC, c("Yes", "No"))) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID,OBS_TEST_CONCEPT_NAME) %>%
    slice(1) %>%
    ungroup() %>%
    filter(!(toupper(OBS_TEST_CONCEPT_NAME) %in% c("INFLAMMATORY BOWEL DISEASE - 6-POINT UCDAI",
                                                   "INFLAMMATORY BOWEL DISEASE - 9-POINT UCDAI",
                                                   "INFLAMMATORY BOWEL DISEASE - SCDAI")))

  #Encounter
  sf$encounter$VISIT_ENCOUNTER_START_DATE = dmy(sf$encounter$VISIT_ENCOUNTER_START_DATE)

  #Procedures
  sf$procedures1 = sf$procedures %>%
    mutate(PROC_STATUS_CONCEPT_CODE = ifelse(is.na(PROC_STATUS_CONCEPT_CODE) | toupper(PROC_STATUS_CONCEPT_CODE) == "NOTE AVAILABLE",
           PHYSICIAN_NOTES_PROC_AVAIL, PROC_STATUS_CONCEPT_CODE)) %>%
    drop_na(PROC_STATUS_CONCEPT_CODE) %>%
    filter(!(PROC_STATUS_CONCEPT_CODE %in% c("", " "))) %>%
    mutate(PROC_CONCEPT_NAME = case_when(PROC_CONCEPT_NAME == "IBD Surgeries" ~ "Number IBD Surgeries",
                                         TRUE ~ PROC_CONCEPT_NAME)) %>%
    select(-c("PROC_START_DATE", "PROC_END_DATE", "INDICATION"))

  sf$procedures2 = sf$procedures %>%
    drop_na(PROC_START_DATE) %>%
    mutate(PROC_CONCEPT_NAME = case_when(PROC_CONCEPT_NAME == "IBD Surgeries" ~ "First IBD Surgery",
                                         TRUE ~paste0("Date of " ,PROC_CONCEPT_NAME))) %>%
    rename(PROC_CONCEPT_NAME_2 = PROC_CONCEPT_NAME) %>%
    select(-c("PROC_STATUS_CONCEPT_CODE", "PROC_END_DATE", "INDICATION"))

  sf$procedures3 = sf$procedures %>%
    drop_na(PROC_END_DATE) %>%
    mutate(PROC_CONCEPT_NAME = case_when(PROC_CONCEPT_NAME == "IBD Surgeries" ~ "Most Recent IBD Surgery",
                                         TRUE ~ PROC_CONCEPT_NAME))  %>%
    rename(PROC_CONCEPT_NAME_3 = PROC_CONCEPT_NAME) %>%
    select(-c("PROC_STATUS_CONCEPT_CODE", "PROC_START_DATE",  "INDICATION"))

  sf$procedures4 = sf$procedures %>%
    drop_na(INDICATION) %>%
    mutate(PROC_CONCEPT_NAME = paste0("Indication for ", PROC_CONCEPT_NAME)) %>%
    rename(PROC_CONCEPT_NAME_4 = PROC_CONCEPT_NAME) %>%
    select(-c("PROC_STATUS_CONCEPT_CODE", "PROC_START_DATE", "PROC_END_DATE"))

  sf$procedures <- NULL


#MAKE SMARTFORM DATA WIDE ----

  y.vars     <-  c("DIAG_CONCEPT_NAME",   "VISIT_ENCOUNTER_START_DATE", "LAB_TEST_CONCEPT_NAME",  "MEDICATION_NAME", "OBS_TEST_CONCEPT_NAME", "PROC_CONCEPT_NAME", "PROC_CONCEPT_NAME_2", "PROC_CONCEPT_NAME_3", "PROC_CONCEPT_NAME_4")
  value.vars <- c("DIAG_STATUS_CONCEPT_NAME",    "VISIT_ENCOUNTER_START_DATE",   "LAB_RESULTS",  "MEDICATION_ADMINISTRATED", "TEST_RESULT_NUMERIC", "PROC_STATUS_CONCEPT_CODE", "PROC_START_DATE", "PROC_END_DATE", "INDICATION" )

  #Select and reshape tables based on the selected columns and values
  wide <- lapply(sf, function(df){
    if(any(y.vars %in% names(df))){
      y.var <- y.vars[y.vars %in% names(df)]
      value.var <- value.vars[value.vars %in% names(df)]
      s <- Reduce(left_join, lapply(1:length(y.var), function(x) to_wide(df, y.var[x], value.var[x])))
      names(s) <- toupper(names(s))
      s
    }
  })

  #write.xlsx(wide, "~/wide.xlsx", overwrite = T)

  wide <- Filter(Negate(is.null), wide)

  for (i in 1:length(wide)){

    wide[[i]] = wide[[i]] %>% ungroup() %>% distinct()

  }

  o = data.frame(o = names(wide)) %>% arrange(match(o, c("encounter")))
  wide = wide[o$o]



#JOIN ALL TABLES TOGETHER ----

  visit <- Reduce(left_join, wide)

  if(any(names(visit) == "NA")){
    visit$`NA` <- NULL
  }

  visit <- visit %>%
    mutate(across(everything(), ~replace(., . %in% c("N.A.", "NA", "N/A", "", "NA;NA", "NA;NA;NA"), NA)))

#
#   #Collapse table by visit encounter id
#   if(any(names(visit)=="VISIT_ENCOUNTER_ID")){
#     visit <- data.table::as.data.table(visit)
#     visit <- visit[, lapply(.SD, function(x){paste0(unique(x[!is.na(x)]), collapse="; ")}), by = VISIT_ENCOUNTER_ID]
#   }

#  CALCULATE DISEASE LOCATION ----

  location = visit %>%
    mutate(ileal = ifelse(`ILEAL PHENOTYPE` ==  "Yes", 1, 0),
           colonic = ifelse(`LEFT COLONIC PHENOTYPE` == "Yes" |   `ILEAL PHENOTYPE` == "Yes" &
                              `RIGHT COLONIC PHENOTYPE` == "Yes"  | `TRANSVERSE COLONIC PHENOTYPE` == "Yes", 1, 0),
           ilealcolonic = ifelse(ileal == 1 & colonic == 1, 1, 0)) %>%
    mutate(Location = case_when(`ILEAL PHENOTYPE` ==  "Yes" &
                                  `LEFT COLONIC PHENOTYPE` == "No" &   `ILEAL PHENOTYPE` == "No" &
                                  `RIGHT COLONIC PHENOTYPE` == "No"  & `TRANSVERSE COLONIC PHENOTYPE` == "No" ~ "ILEAL",
                                `ILEAL PHENOTYPE` ==  "No" &
                                  (`LEFT COLONIC PHENOTYPE` == "Yes" |   `ILEAL PHENOTYPE` == "Yes" &
                                     `RIGHT COLONIC PHENOTYPE` == "Yes"  | `TRANSVERSE COLONIC PHENOTYPE` == "Yes") ~ "Colonic",
                                `ILEAL PHENOTYPE` ==  "Yes" &
                                  (`LEFT COLONIC PHENOTYPE` == "Yes" |   `ILEAL PHENOTYPE` == "Yes" &
                                     `RIGHT COLONIC PHENOTYPE` == "Yes"  | `TRANSVERSE COLONIC PHENOTYPE` == "Yes") ~ "Ileocolonic",
                                `ILEAL PHENOTYPE` ==  "Unknown" |
                                  `LEFT COLONIC PHENOTYPE` == "Unknown" |   `ILEAL PHENOTYPE` == "Unknown" |
                                  `RIGHT COLONIC PHENOTYPE` == "Unknown"  | `TRANSVERSE COLONIC PHENOTYPE` == "Unknown" ~ "Unknown"),
           UpperGI = case_when(`DUODENAL PHENOTYPE` == "Yes"|  `ESOPHAGEAL PHENOTYPE` == "Yes" |  `GASTRIC PHENOTYPE` == "Yes" | `JEJUNAL PHENOTYPE` == "Yes" ~ "Yes",
                               `DUODENAL PHENOTYPE` == "Unknown" &  `ESOPHAGEAL PHENOTYPE` == "Unknown" &  `GASTRIC PHENOTYPE` == "Unknown" & `JEJUNAL PHENOTYPE` == "Unknown" ~ "Unknown",
                               `DUODENAL PHENOTYPE` == "No" &  `ESOPHAGEAL PHENOTYPE` == "No" &  `GASTRIC PHENOTYPE` == "No" & `JEJUNAL PHENOTYPE` == "No" ~ "No"),
           Perianal = case_when(`ANAL PHENOTYPE` == "Yes" |
                                  `ANAL CANAL STRICTURE`== 'Yes'|
                                  `ANAL CANAL ULCER`== 'Yes'|
                                  `ANAL FISSURE`== 'Yes'|
                                  `PERIANAL ABCESS`== 'Yes'|
                                  `PERIANAL FISTULA - COMPLEX FISTULA`== 'Yes' ~ "Yes"  ,
                                `ANAL PHENOTYPE` == "Unknown" & `ANAL CANAL STRICTURE`== 'Unknown' &
                                  `ANAL CANAL ULCER`== 'Unknown' &
                                  `ANAL FISSURE`== 'Unknown' &
                                  `PERIANAL ABCESS`== 'Unknown' &
                                  `PERIANAL FISTULA - COMPLEX FISTULA`== 'Unknown' ~ "Unknown",
                                `ANAL PHENOTYPE` == "No" & `ANAL CANAL STRICTURE`== 'No' &
                                  `ANAL CANAL ULCER`== 'No' &
                                  `ANAL FISSURE`== 'No' &
                                  `PERIANAL ABCESS`== 'No' &
                                  `PERIANAL FISTULA - COMPLEX FISTULA`== 'No' ~ "No"),
           PHENOTYPE_Data = "Yes") %>%
    mutate(Location = case_when(is.na(Location) & ilealcolonic == 1 ~ "Ileocolonic",
                                is.na(Location) & (is.na(ilealcolonic) | ilealcolonic == 0) & ileal == 1 ~ "Ileal",
                                is.na(Location) & (is.na(ilealcolonic) | ilealcolonic == 0) & colonic == 1 ~ "Colonic",
                                TRUE ~ Location)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, Location, UpperGI, Perianal) %>%
    dplyr::rename(DISEASE_LOCATION = Location,  UPPERGI = UpperGI,  PERIANAL= Perianal)

  visit = visit %>%
    left_join(location)

  rm(location)

# CALCULATE DISEASE ACTIVITY  ----


  #scdai
  scdai = visit %>%
    mutate(A = ifelse(is.na((`ABDOMINAL PAIN - PAIN SCALE`)), (`ABDOMINAL PAIN SCORE`), (`ABDOMINAL PAIN - PAIN SCALE`)),
           B = ifelse(is.na((`CURRENT AVERAGE NUMBER OF DAILY BOWEL MOVEMENTS`)), (`CURRENT MAXIMUM NUMBER OF DAILY BOWEL MOVEMENTS`), (`CURRENT AVERAGE NUMBER OF DAILY BOWEL MOVEMENTS`)),
           G = ifelse(is.na((`CONSTITUTIONAL - GENERAL WELL-BEING`)), (`GENERAL WELL BEING SCORE`), (`CONSTITUTIONAL - GENERAL WELL-BEING`))) %>%
     mutate(across(c(A,B,G), ~case_when(. == "None" ~ 0,
                              . == "Mild" ~ 1,
                              . == "Moderate" ~ 2,
                              . == "Severe" ~ 3,
                              . == "Generally well" ~ 0,
                              . == "Slightly under par" ~ 1,
                              . == "Poor" ~ 2,
                              . == "Very poor" ~ 3,
                              . == "Terrible" ~ 4,
                              TRUE ~ as.numeric(.)))) %>%
    dplyr::rename(Daily.BM = B, Abdominal.Pain.Score = A, General.well.being.score = G) %>%
    mutate(SCDAI = 44+(2*7*Daily.BM)+(5*7*Abdominal.Pain.Score)+(7*7*General.well.being.score)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID,SCDAI)


  visit = visit %>%
    left_join(scdai)

  rm(scdai)

  #6pt mayo

  mayo = visit %>%
    mutate(T = ifelse(is.na(`PHYSICIAN'S GLOBAL ASSESSMENT OF CURRENT DISEASE STATUS`), `INFLAMMATORY BOWEL DISEASE - GLOBAL ASSESSMENT SCORE`, `PHYSICIAN'S GLOBAL ASSESSMENT OF CURRENT DISEASE STATUS`),
           R = `BLOOD IN STOOL - RECENT CHANGE IN RECTAL BLEEDING AMOUNT`,
           S = ifelse(is.na(`RECENT CHANGE IN DAILY STOOL FREQUENCY`), `STOOL FREQUENCY SCORE`, `RECENT CHANGE IN DAILY STOOL FREQUENCY`)) %>%
    mutate(across(c(T,R,S), ~case_when(. == "Normal" ~ 0,
                              . == "1-2 stools/day more than normal" ~ 1,
                              . == "3-4 stools/day more than normal" ~ 2,
                              . %in% c(">4 stools/day more than normal", "&gt;4 stools/day more than normal") ~ 3,
                              . == "None" ~ 0,
                              . == "Visible blood in stool less than half the time" ~ 1,
                              . == "Visible blood in stool half of the time or more" ~ 2,
                              . == "Passing blood alone" ~ 3,
                              . == "Quiescent" ~ 0,
                              . == "Mild" ~ 1,
                              . == "Moderate" ~ 2,
                              . == "Severe" ~ 3,
                              TRUE ~ as.numeric(.)))) %>%
    dplyr::rename(Stool.Freq.Score = S, Rectal.Bleeding.Score = R, Global.Assessment.Score = T) %>%
    mutate(MAYO_6PT = Stool.Freq.Score + Rectal.Bleeding.Score,
           MAYO_9PT = Stool.Freq.Score + Rectal.Bleeding.Score + Global.Assessment.Score) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, MAYO_6PT, MAYO_9PT)

  visit = visit %>%
    left_join(mayo)

  rm(mayo)


# Collapse table if visit encounter start date within 7 days of each other----


  visit = visit %>%
    mutate(VISIT_ENCOUNTER_START_DATE = ymd(VISIT_ENCOUNTER_START_DATE)) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_START_DATE) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(t1 = as.numeric(VISIT_ENCOUNTER_START_DATE - lag(VISIT_ENCOUNTER_START_DATE)) )%>%
    mutate(t1 = ifelse(is.na(t1), 0, t1)) %>%
    mutate(v = ifelse(t1 <= 7, lag(VISIT_ENCOUNTER_ID), VISIT_ENCOUNTER_ID)) %>%
    mutate(v = ifelse(is.na(v), VISIT_ENCOUNTER_ID, v)) %>%
    mutate(v2 = ifelse(t1 <= 7, lag(v), v)) %>%
    mutate(v2 = ifelse(is.na(v2), v, v2)) %>%
    mutate(v3 = ifelse(t1 <= 7, lag(v2), v2)) %>%
    mutate(v3 = ifelse(is.na(v3), v2, v3)) %>%
    mutate(v4 = ifelse(t1 <= 7, lag(v3), v3)) %>%
    mutate(v4 = ifelse(is.na(v4), v3, v4)) %>%
    mutate(v5 = ifelse(t1 <= 7, lag(v4), v4)) %>%
    mutate(v5 = ifelse(is.na(v5), v4, v5)) %>%
    select(-c(v2, v,t1,v3,v4)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, VISIT_ENCOUNTER_START_DATE, v5, everything())

    visit <- data.table::as.data.table(visit)
    visit <- visit[, lapply(.SD, function(x){paste0(unique(x[!is.na(x)]), collapse="; ")}), by = v5]


    e = sf$encounter %>%
      distinct(VISIT_ENCOUNTER_ID, VISIT_ENCOUNTER_START_DATE) %>%
      dplyr::rename(v5 = VISIT_ENCOUNTER_ID,
             d = VISIT_ENCOUNTER_START_DATE)

    visit = visit %>%
      left_join(e) %>%
      arrange(DEIDENTIFIED_MASTER_PATIENT_ID, d) %>%
      select(v5, d, everything())


    visit <- visit %>%
      mutate(across(everything(), ~replace(., . %in% c("N.A.", "NA", "N/A", "", "NA;NA", "NA;NA;NA", " "), NA)))



    # RE-CALCULATE DISEASE LOCATION FOR THOSE THAT DO NOT HAVE IT ----

    location = visit %>%
      mutate(ileal = ifelse(`ILEAL PHENOTYPE` ==  "Yes", 1, 0),
             colonic = ifelse(`LEFT COLONIC PHENOTYPE` == "Yes" |   `ILEAL PHENOTYPE` == "Yes" &
                                `RIGHT COLONIC PHENOTYPE` == "Yes"  | `TRANSVERSE COLONIC PHENOTYPE` == "Yes", 1, 0),
             ilealcolonic = ifelse(ileal == 1 & colonic == 1, 1, 0)) %>%
      mutate(Location = case_when(`ILEAL PHENOTYPE` ==  "Yes" &
                                    `LEFT COLONIC PHENOTYPE` == "No" &   `ILEAL PHENOTYPE` == "No" &
                                    `RIGHT COLONIC PHENOTYPE` == "No"  & `TRANSVERSE COLONIC PHENOTYPE` == "No" ~ "ILEAL",
                                  `ILEAL PHENOTYPE` ==  "No" &
                                    (`LEFT COLONIC PHENOTYPE` == "Yes" |   `ILEAL PHENOTYPE` == "Yes" &
                                       `RIGHT COLONIC PHENOTYPE` == "Yes"  | `TRANSVERSE COLONIC PHENOTYPE` == "Yes") ~ "Colonic",
                                  `ILEAL PHENOTYPE` ==  "Yes" &
                                    (`LEFT COLONIC PHENOTYPE` == "Yes" |   `ILEAL PHENOTYPE` == "Yes" &
                                       `RIGHT COLONIC PHENOTYPE` == "Yes"  | `TRANSVERSE COLONIC PHENOTYPE` == "Yes") ~ "Ileocolonic",
                                  `ILEAL PHENOTYPE` ==  "Unknown" |
                                    `LEFT COLONIC PHENOTYPE` == "Unknown" |   `ILEAL PHENOTYPE` == "Unknown" |
                                    `RIGHT COLONIC PHENOTYPE` == "Unknown"  | `TRANSVERSE COLONIC PHENOTYPE` == "Unknown" ~ "Unknown"),
             UpperGI = case_when(`DUODENAL PHENOTYPE` == "Yes"|  `ESOPHAGEAL PHENOTYPE` == "Yes" |  `GASTRIC PHENOTYPE` == "Yes" | `JEJUNAL PHENOTYPE` == "Yes" ~ "Yes",
                                 `DUODENAL PHENOTYPE` == "Unknown" &  `ESOPHAGEAL PHENOTYPE` == "Unknown" &  `GASTRIC PHENOTYPE` == "Unknown" & `JEJUNAL PHENOTYPE` == "Unknown" ~ "Unknown",
                                 `DUODENAL PHENOTYPE` == "No" &  `ESOPHAGEAL PHENOTYPE` == "No" &  `GASTRIC PHENOTYPE` == "No" & `JEJUNAL PHENOTYPE` == "No" ~ "No"),
             Perianal = case_when(`ANAL PHENOTYPE` == "Yes" |
                                    `ANAL CANAL STRICTURE`== 'Yes'|
                                    `ANAL CANAL ULCER`== 'Yes'|
                                    `ANAL FISSURE`== 'Yes'|
                                    `PERIANAL ABCESS`== 'Yes'|
                                    `PERIANAL FISTULA - COMPLEX FISTULA`== 'Yes' ~ "Yes"  ,
                                  `ANAL PHENOTYPE` == "Unknown" & `ANAL CANAL STRICTURE`== 'Unknown' &
                                    `ANAL CANAL ULCER`== 'Unknown' &
                                    `ANAL FISSURE`== 'Unknown' &
                                    `PERIANAL ABCESS`== 'Unknown' &
                                    `PERIANAL FISTULA - COMPLEX FISTULA`== 'Unknown' ~ "Unknown",
                                  `ANAL PHENOTYPE` == "No" & `ANAL CANAL STRICTURE`== 'No' &
                                    `ANAL CANAL ULCER`== 'No' &
                                    `ANAL FISSURE`== 'No' &
                                    `PERIANAL ABCESS`== 'No' &
                                    `PERIANAL FISTULA - COMPLEX FISTULA`== 'No' ~ "No"),
             PHENOTYPE_Data = "Yes") %>%
      mutate(Location = case_when(is.na(Location) & ilealcolonic == 1 ~ "Ileocolonic",
                                  is.na(Location) & (is.na(ilealcolonic) | ilealcolonic == 0) & ileal == 1 ~ "Ileal",
                                  is.na(Location) & (is.na(ilealcolonic) | ilealcolonic == 0) & colonic == 1 ~ "Colonic",
                                  TRUE ~ Location)) %>%
      select(DEIDENTIFIED_MASTER_PATIENT_ID, v5, Location, UpperGI, Perianal) %>%
      dplyr::rename(DISEASE_LOCATION = Location,  UPPERGI = UpperGI,  PERIANAL= Perianal)


    visit$DISEASE_LOCATION[is.na(visit$DISEASE_LOCATION)] <- location$DISEASE_LOCATION[match(visit$v5,location$v5)][which(is.na(visit$DISEASE_LOCATION))]
    visit$UPPERGI[is.na(visit$UPPERGI)] <- location$UPPERGI[match(visit$v5,location$v5)][which(is.na(visit$UPPERGI))]
    visit$PERIANAL[is.na(visit$PERIANAL)] <- location$PERIANAL[match(visit$v5,location$v5)][which(is.na(visit$PERIANAL))]

    rm(location)

    # CALCULATE DISEASE ACTIVITY  ----


    #scdai
    scdai = visit %>%
      mutate(A = ifelse(is.na((`ABDOMINAL PAIN - PAIN SCALE`)), (`ABDOMINAL PAIN SCORE`), (`ABDOMINAL PAIN - PAIN SCALE`)),
             B = ifelse(is.na((`CURRENT AVERAGE NUMBER OF DAILY BOWEL MOVEMENTS`)), (`CURRENT MAXIMUM NUMBER OF DAILY BOWEL MOVEMENTS`), (`CURRENT AVERAGE NUMBER OF DAILY BOWEL MOVEMENTS`)),
             G = ifelse(is.na((`CONSTITUTIONAL - GENERAL WELL-BEING`)), (`GENERAL WELL BEING SCORE`), (`CONSTITUTIONAL - GENERAL WELL-BEING`))) %>%
      mutate(across(c(A,B,G), ~case_when(. == "None" ~ 0,
                                         . == "Mild" ~ 1,
                                         . == "Moderate" ~ 2,
                                         . == "Severe" ~ 3,
                                         . == "Generally well" ~ 0,
                                         . == "Slightly under par" ~ 1,
                                         . == "Poor" ~ 2,
                                         . == "Very poor" ~ 3,
                                         . == "Terrible" ~ 4,
                                         TRUE ~ as.numeric(.)))) %>%
      dplyr::rename(Daily.BM = B, Abdominal.Pain.Score = A, General.well.being.score = G) %>%
      mutate(SCDAI = 44+(2*7*Daily.BM)+(5*7*Abdominal.Pain.Score)+(7*7*General.well.being.score)) %>%
      select(DEIDENTIFIED_MASTER_PATIENT_ID, v5,SCDAI)


    visit$SCDAI[is.na(visit$SCDAI)] <- scdai$SCDAI[match(visit$v5,scdai$v5)][which(is.na(visit$SCDAI))]

    rm(scdai)

    #6pt mayo

    mayo = visit %>%
      mutate(T = ifelse(is.na(`PHYSICIAN'S GLOBAL ASSESSMENT OF CURRENT DISEASE STATUS`), `INFLAMMATORY BOWEL DISEASE - GLOBAL ASSESSMENT SCORE`, `PHYSICIAN'S GLOBAL ASSESSMENT OF CURRENT DISEASE STATUS`),
             R = `BLOOD IN STOOL - RECENT CHANGE IN RECTAL BLEEDING AMOUNT`,
             S = ifelse(is.na(`RECENT CHANGE IN DAILY STOOL FREQUENCY`), `STOOL FREQUENCY SCORE`, `RECENT CHANGE IN DAILY STOOL FREQUENCY`)) %>%
      mutate(across(c(T,R,S), ~case_when(. == "Normal" ~ 0,
                                         . == "1-2 stools/day more than normal" ~ 1,
                                         . == "3-4 stools/day more than normal" ~ 2,
                                         . %in% c(">4 stools/day more than normal", "&gt;4 stools/day more than normal") ~ 3,
                                         . == "None" ~ 0,
                                         . == "Visible blood in stool less than half the time" ~ 1,
                                         . == "Visible blood in stool half of the time or more" ~ 2,
                                         . == "Passing blood alone" ~ 3,
                                         . == "Quiescent" ~ 0,
                                         . == "Mild" ~ 1,
                                         . == "Moderate" ~ 2,
                                         . == "Severe" ~ 3,
                                         TRUE ~ as.numeric(.)))) %>%
      dplyr::rename(Stool.Freq.Score = S, Rectal.Bleeding.Score = R, Global.Assessment.Score = T) %>%
      mutate(MAYO_6PT = Stool.Freq.Score + Rectal.Bleeding.Score,
             MAYO_9PT = Stool.Freq.Score + Rectal.Bleeding.Score + Global.Assessment.Score) %>%
      select(DEIDENTIFIED_MASTER_PATIENT_ID, v5, MAYO_6PT, MAYO_9PT)

     visit$MAYO_6PT[is.na(visit$MAYO_6PT)] <- mayo$MAYO_6PT[match(visit$v5,mayo$v5)][which(is.na(visit$MAYO_6PT))]
     visit$MAYO_9PT[is.na(visit$MAYO_9PT)] <- mayo$MAYO_9PT[match(visit$v5,mayo$v5)][which(is.na(visit$MAYO_9PT))]

     rm(mayo)

# LABEL WHEN VISIT OCCURED ----

     #endoscopy
     endoscopy = data$procedures %>%
       filter(PROC_CONCEPT_NAME %in% c("Colonoscopy/Sigmoidoscopy")) %>%
       mutate(PROC_START_DATE = dmy(PROC_START_DATE)) %>%
       rename(ENDOSCOPY_DATE = PROC_START_DATE) %>%
       drop_na(ENDOSCOPY_DATE) %>%
       select(DEIDENTIFIED_MASTER_PATIENT_ID, ENDOSCOPY_DATE,INDICATION) %>%
       group_by(DEIDENTIFIED_MASTER_PATIENT_ID, ENDOSCOPY_DATE) %>%
       fill(INDICATION, .direction = "downup") %>%
       ungroup() %>%
       distinct() %>%
       rename(INDICATION_FOR_ENDOSCOPY = INDICATION) %>%
       left_join(visit) %>%
       mutate(diff = abs(d - ENDOSCOPY_DATE)) %>%
       filter(diff <= 30) %>%
       mutate(ENDOSCOPY = 1) %>%
       distinct(DEIDENTIFIED_MASTER_PATIENT_ID, v5, ENDOSCOPY, ENDOSCOPY_DATE, INDICATION_FOR_ENDOSCOPY) %>%
       mutate(across(everything(), ~replace(., . %in% c("N.A.", "NA", "N/A", "", "NA;NA", "NA;NA;NA", " "), NA))) %>%
       arrange(DEIDENTIFIED_MASTER_PATIENT_ID, v5, ENDOSCOPY, ENDOSCOPY_DATE) %>%
       group_by(DEIDENTIFIED_MASTER_PATIENT_ID, v5, ENDOSCOPY_DATE) %>%
       fill(INDICATION_FOR_ENDOSCOPY, .direction = "downup") %>%
       ungroup() %>%
       distinct() %>%
       group_by(DEIDENTIFIED_MASTER_PATIENT_ID, v5) %>%
       mutate(c = seq_along(v5)) %>%
       pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, v5),
                   names_from = c,
                   values_from = c(ENDOSCOPY_DATE, INDICATION_FOR_ENDOSCOPY)) %>%
       mutate(ENDOSCOPY = 1) %>%
       ungroup()


     visit = visit %>%
       left_join(endoscopy) %>%
       mutate(across(c("ENDOSCOPY"), ~ replace_na(.x, 0)))



    #enrollment
     enrollment = visit %>%
       left_join(cohort) %>%
       arrange(DEIDENTIFIED_MASTER_PATIENT_ID, d) %>%
       mutate(diff = as.numeric(d - DATE_OF_CONSENT)) %>%
       select(DEIDENTIFIED_MASTER_PATIENT_ID, v5, d,  diff) %>%
       mutate(ENROLLMENT = if_else(diff >= -30 & diff <= 30,1,0),
              BEFORE_ENROLLMENT = if_else(diff < -30,1,0),
              FOLLOWUP = if_else(diff > 30, 1, 0),
              MONTHS_FROM_CONSENT_TO_SMARTFORM = round(diff/30.5,1)) %>%
       select(-diff) %>%
       distinct()


     visit = visit %>%
       left_join(enrollment) %>%
       mutate(across(c(ENROLLMENT, BEFORE_ENROLLMENT, FOLLOWUP), ~ replace_na(.x, 0)))

     #latest smartform
     latest = visit %>%
       arrange(DEIDENTIFIED_MASTER_PATIENT_ID, d) %>%
       group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
       slice(which.max(d)) %>%
       mutate(LATEST = 1) %>%
       select(DEIDENTIFIED_MASTER_PATIENT_ID, v5, d, LATEST) %>%
       ungroup() %>%
       distinct()


     visit = visit %>%
       left_join(latest) %>%
       mutate(across(c(LATEST), ~ replace_na(.x, 0)))

# SPLIT DATA BASED ON INDEX ----



     if(index_info == "ENROLLMENT"){

       #collapse data if multiple visits at enrollment;
       visit = visit %>%
         filter(ENROLLMENT == 1)

       visit <- data.table::as.data.table(visit)
       visit <- visit[, lapply(.SD, function(x){paste0(unique(x[!is.na(x)]), collapse="; ")}), by = DEIDENTIFIED_MASTER_PATIENT_ID]

       visit = visit %>%
         mutate(SMARTFORM_AVAILABLE_ENROLLMENT = 1)

       cohort = cohort %>%
         left_join(visit) %>%
         select(-v5, -d) %>%
         mutate(across(everything(), ~replace(., . %in% c("N.A.", "NA", "N/A", "", "NA;NA", "NA;NA;NA", " "), NA))) %>%
         mutate(across(c(SMARTFORM_AVAILABLE_ENROLLMENT), ~ replace_na(.x, 0)))
       } else if(index_info == "ENDOSCOPY"){

           visit = visit %>%
             filter(ENDOSCOPY == 1) %>%
             mutate(SMARTFORM_AVAILABLE_ENDOSCOPY = 1)

           cohort = cohort %>%
             left_join(visit) %>%
             select(-v5, -d) %>%
             mutate(across(everything(), ~replace(., . %in% c("N.A.", "NA", "N/A", "", "NA;NA", "NA;NA;NA", " "), NA))) %>%
             mutate(across(c(SMARTFORM_AVAILABLE_ENDOSCOPY), ~ replace_na(.x, 0)))
       }else if(index_info == "LATEST"){

         visit = visit %>%
           filter(LATEST == 1) %>%
           mutate(SMARTFORM_AVAILABLE = 1)

         cohort = cohort %>%
           left_join(visit) %>%
           select(-v5, -d) %>%
           mutate(across(everything(), ~replace(., . %in% c("N.A.", "NA", "N/A", "", "NA;NA", "NA;NA;NA", " "), NA))) %>%
           mutate(across(c(SMARTFORM_AVAILABLE), ~ replace_na(.x, 0)))}      else if(index_info == "ALL"){

           visit = visit %>%
             mutate(SMARTFORM_AVAILABLE = 1)


           cohort = cohort %>%
             left_join(visit) %>%
             select(-v5, -d) %>%
             mutate(across(everything(), ~replace(., . %in% c("N.A.", "NA", "N/A", "", "NA;NA", "NA;NA;NA", " "), NA))) %>%
             mutate(across(c(SMARTFORM_AVAILABLE), ~ replace_na(.x, 0))) } else{visit = visit %>%
           mutate(SMARTFORM_AVAILABLE = 1)


         cohort = cohort %>%
           left_join(visit) %>%
           select(-v5, -d) %>%
           mutate(across(everything(), ~replace(., . %in% c("N.A.", "NA", "N/A", "", "NA;NA", "NA;NA;NA", " "), NA))) %>%
           mutate(across(c(SMARTFORM_AVAILABLE), ~ replace_na(.x, 0))) }


         # FORMAT COLUMN NAMES  ----

         cohort <- cohort %>%
       arrange(DEIDENTIFIED_MASTER_PATIENT_ID, DATE_OF_CONSENT) %>%
       select(-DEIDENTIFIED_PATIENT_ID,-DATA_SOURCE) %>%

       mutate(across(everything(), ~replace(., . %in% c("N.A.", "NA", "N/A", "", "NA;NA", "NA;NA;NA", " "), NA))) %>%
       mutate(across(contains("SMARTFORM_AVAILABLE"), ~ replace_na(.x, 0)))


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
         addWorksheet(wb, "smartform_summary")
         writeData(wb, "smartform_summary", x=cohort, startCol=1, startRow=1, colNames=TRUE, rowNames=FALSE)

         # FORMAT CELLS ----

         rown = dim(cohort)[1]
         coln = dim(cohort)[2]


         #column headers
         conditionalFormatting(wb, "smartform_summary", cols=1:coln, rows=1, rule="!=0", style = style1)




         # SAVE REPORT ----

         saveWorkbook(wb, file = paste0(filename), overwrite = TRUE)

         return(cohort)


}











