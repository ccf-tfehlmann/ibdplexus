#' qorus_summary
#'
#' Summarizes QORUS data at a specific date.
#'
#'
#' @param datadir Directory where unzipped data is saved.
#' @param index_info A data frame with DEIDENTIFIED_MASTER_PATIENT_ID and a variable named index_date. Default is the Date of Consent.
#' @param filename The name of the output file.
#' @param index_range The number of days to look from index date. Default is 30 days.
#' @param emr_codes EMR codes of interest as a character string. Can be an ICD10, LOINC or CPT code.
#'
#' @return Returns a dataframe and excel spreadsheet. If EMR codes are specified, subset of data with that code.
#' @export
qorus_summary <- function(datadir,
                             index_info = "DATE_OF_CONSENT",
                             filename = "QORUS_SUMMARY.xlsx",
                             index_range = "30",
                             emr_codes = NULL){


# LOAD FILES ----

data =  load_data(datadir = datadir, cohort = "QORUS", domains = "ALL", data_type = "BOTH")

# DEMOGRAPHIC INFORMATION ----

  demo = data$demographics %>%
    filter(DATA_SOURCE == "ECRF_QORUS") %>%
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
      filter(DATA_SOURCE %in% c("ECRF_QORUS")) %>%
      filter(DIAG_CONCEPT_NAME %in% c("Crohn's Disease", "IBD Unclassified", "Ulcerative Colitis")) %>%
      left_join(data$encounter) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      mutate(DIAGNOSIS = DIAG_CONCEPT_NAME,
             DIAGNOSIS_DATE = dmy(DIAGNOSIS_DATE),
             DISEASE_SITE = DIAG_SITE) %>%
      arrange(DEIDENTIFIED_MASTER_PATIENT_ID, match(DIAGNOSIS, c("Crohn's Disease", "Ulcerative Colitis", "IBD Unclassified")),desc(dmy(VISIT_ENCOUNTER_START_DATE))) %>%
      right_join(cohort) %>%
      mutate(diff = abs(dmy(VISIT_ENCOUNTER_START_DATE) - index_date)) %>%
      slice(which.min(diff)) %>%
      select(DEIDENTIFIED_MASTER_PATIENT_ID, DIAGNOSIS, DIAGNOSIS_DATE, DISEASE_SITE, DISEASE_PHENOTYPE)

    cohort = left_join(cohort, dx)


    #ADD RACE/ETHNICITY ----


    race = data$demographics %>%
      filter(DATA_SOURCE == "EMR") %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, RACE, ETHNICITY)

    cohort = left_join(cohort, race)



# SCORES ----

      #sCDAI----


    scdai = data$observations %>%
      filter(DATA_SOURCE == "ECRF_QORUS") %>%
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
      rename(Daily.BM = B, Abdominal.Pain.Score = A, General.well.being.score = G) %>%
      mutate(sCDAI.score = 44+(2*7*Daily.BM)+(5*7*Abdominal.Pain.Score)+(7*7*General.well.being.score), Source = "ECRF") %>%
      dplyr::rename(sCDAI.date = OBS_TEST_RESULT_DATE) %>%
      select(DEIDENTIFIED_MASTER_PATIENT_ID, sCDAI.date, sCDAI.score, Source, Daily.BM, Abdominal.Pain.Score, General.well.being.score,Daily.BM.Question) %>%
      dplyr::mutate(sCDAI.category = case_when(
        sCDAI.score < 150 ~ "Remission",
        sCDAI.score >= 150 & sCDAI.score <= 219 ~ "Mild",
        sCDAI.score >= 220 & sCDAI.score <= 450 ~ "Moderate",
        sCDAI.score > 450 ~ "Severe",
        TRUE ~ as.character(NA))) %>%
      arrange(DEIDENTIFIED_MASTER_PATIENT_ID, desc(sCDAI.score), sCDAI.date) %>%
      dplyr::rename(scdai.source = Source)  %>% distinct_all() %>%
      right_join(cohort) %>%
      mutate(datediff = abs(sCDAI.date - index_date)) %>%
      filter(DIAGNOSIS == "Crohn's Disease" & datediff <= t) %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
      arrange(DEIDENTIFIED_MASTER_PATIENT_ID, desc(sCDAI.score), datediff) %>%
      slice(1) %>%
      distinct(DEIDENTIFIED_MASTER_PATIENT_ID, sCDAI.date, sCDAI.score, index_date,  Daily.BM, Abdominal.Pain.Score, General.well.being.score) %>%
      ungroup() %>%
      select(sort(names(.))) %>%
      select(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, everything())


    cohort = left_join(cohort, scdai)

    #6pt Mayo ----


    mayo6 = data$observations %>%
      filter(DATA_SOURCE == "ECRF_QORUS") %>%
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
      rename(Stool.Freq.Score = S, Rectal.Bleeding.Score = R) %>%
      mutate(UCDAI.6.score = Stool.Freq.Score + Rectal.Bleeding.Score,  Source = "ECRF") %>%
      mutate(UCDAI.date = OBS_TEST_RESULT_DATE) %>%
      select(DEIDENTIFIED_MASTER_PATIENT_ID, UCDAI.6.score,  UCDAI.date, Source, Stool.Freq.Score, Rectal.Bleeding.Score) %>%
      dplyr::mutate(mayo6.category = case_when(
        UCDAI.6.score < 2 ~ "Remission",
        UCDAI.6.score >= 2 & UCDAI.6.score <= 3 ~ "Mild",
        UCDAI.6.score >= 4 & UCDAI.6.score <= 5 ~ "Moderate",
        UCDAI.6.score >= 6 ~ "Severe",
        TRUE ~ as.character(NA))) %>%
        right_join(cohort) %>%
        mutate(datediff = abs(UCDAI.date - index_date)) %>%
        filter(DIAGNOSIS == "Ulcerative Colitis" & datediff <= t) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
        arrange(DEIDENTIFIED_MASTER_PATIENT_ID, desc(UCDAI.6.score), datediff) %>%
        slice(1) %>%
        distinct(DEIDENTIFIED_MASTER_PATIENT_ID, UCDAI.6.score,  index_date, UCDAI.date,  Stool.Freq.Score, Rectal.Bleeding.Score) %>%
        ungroup() %>%
        select(sort(names(.))) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, everything()) %>%
      rename(Mayo.6pt.Score = UCDAI.6.score,
             Mayo.6pt.Date = UCDAI.date)


      cohort = left_join(cohort, mayo6)


      #Ostomy ----


      ostomy = data$observations %>%
        filter(DATA_SOURCE == "ECRF_QORUS") %>%
        filter(OBS_TEST_CONCEPT_NAME %in% c("Baseline Number of Daily Bowel Movements",
                                            "Current Average Number of Daily Bowel Movements",
                                            "Recent Change in Daily Stool Frequency",
                                            "Current Average Number of Daily Liquid Bowel Movements")) %>%
        filter(DESCRIPTIVE_SYMP_TEST_RESULTS %in% c("Not applicable (i have an ostomy)",   "Not applicable, I have an ostomy")) %>%
        mutate(ostomy = "Ostomy Present") %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, ostomy, VISIT_ENCOUNTER_ID) %>%
        left_join(data$encounter) %>%
        left_join(cohort) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
        mutate(diff =  dmy(VISIT_ENCOUNTER_START_DATE)- index_date) %>%
        drop_na(ostomy) %>%
        slice(which.min(abs(diff))) %>%
        ungroup() %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, ostomy,index_date,diff )%>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
        slice(which.min(abs(diff))) %>%
        ungroup() %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, ostomy)

      cohort = cohort %>%
        left_join(ostomy) %>%
        mutate(ostomy = case_when(DIAGNOSIS == "Crohn's Disease" & is.na(sCDAI.score) ~ ostomy,
                                  DIAGNOSIS == "Ulcerative Colitis" & is.na(Mayo.6pt.Score) ~ ostomy,
                                  DIAGNOSIS == "IBD Unclassified" ~ ostomy,
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

  cohort <- cohort %>% arrange(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, everything())


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
  addWorksheet(wb, "qorus_summary")
  writeData(wb, "qorus_summary", x=cohort, startCol=1, startRow=1, colNames=TRUE, rowNames=FALSE)

# FORMAT CELLS ----

democoln =  max(which(colnames(cohort) %in% "INDEX_DATE"))

rown = dim(cohort)[1]
coln = dim(cohort)[2]

conf = max(which(colnames(cohort) %in% toupper("RACE")))

#column headers
conditionalFormatting(wb, "qorus_summary", cols=1:democoln, rows=1, rule="!=0", style = style1)
conditionalFormatting(wb, "qorus_summary", cols=(democoln+1):conf, rows=1, rule="!=0", style = style2)
conditionalFormatting(wb, "qorus_summary", cols=(conf+1):coln, rows=1, rule="!=0", style = style3)




# SAVE REPORT ----

saveWorkbook(wb, file = paste0(filename), overwrite = TRUE)
return(cohort)

}
