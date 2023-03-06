#' wpcdai
#'
#' Creates dataframe with patient and visit encounter ID's, wPCDAI calculation, and all PCDAI columns
#'
#' @param observations A dataframe with observation data usually generated using load_data.
#' @param labs A dataframe with lab data usually generated using load_data.
#' @param encounter A dataframe with encounter data usually generated using load_data.
#'
#' @return A dataframe with wPCDAI calcuation
#' @export
#'
#' @examples
#' # upload data
#' # dat <- load_data(dir, cohort = "RISK", domains = c("ALL"), data_type = "BOTH")
#' # get dataframe with wpcdai
#' # wpcdai <- wpcdai(dat)
#'
#'

wpcdai <- function(observations, encounter, labs){

  # create values for to_wide function
  # CLB remove values here not necessary for wpcdai function
  select.col = c("DIAG_CONCEPT_NAME", "DIAGNOSIS_DATE", "GENDER", "TYPE_OF_ENCOUNTER", "VISIT_ENCOUNTER_START_DATE", "AGE_AT_ENCOUNTER", "BIRTH_YEAR", "LAB_TEST_CONCEPT_NAME", "ASSAY NAME", "MEDICATION_NAME", "OBS_TEST_CONCEPT_NAME", "ANA_SITE_CONCEPT_NAME", "SRC_BIOSAMPLE_CONCEPT_NAME")
  select.val  = c("DIAG_STATUS_CONCEPT_NAME", "DIAGNOSIS_DATE", "GENDER", "TYPE_OF_ENCOUNTER", "VISIT_ENCOUNTER_START_DATE", "AGE_AT_ENCOUNTER", "BIRTH_YEAR", "TEST_RESULT_NUMERIC", "RAW DATA FILE NAME", "MED_START_DATE", "TEST_RESULT_NUMERIC", "TEST_RESULT_NUMERIC", "SAMPLE_STATUS")
  filter.col  = c("DIAG_CONCEPT_NAME", "OBS_TEST_CONCEPT_NAME")
  filter.val  = c( "Disease Location", "Endoscopic Assessment - Deep Ulceration", "Endoscopic Assessment - Superficial Ulceration", "Endoscopic Assessment - Amount of Surface Ulcerated", "Endoscopic Assessment - Amount of Surface Involved", "Perianal Disease -", "EIM", "Disease Behavior - Stricturing/Fibrostenotic", "Disease Behavior - Internally Pentrating", "PCDAI")

  #Read dictionary
  names <- header
  names <- names[,,1]

  #Apply filters for each data table
  observations <- observations %>% filter(DATA_SOURCE == "RISK")
  encounter <- encounter %>% filter(DATA_SOURCE == "RISK")
  labs <- labs %>% filter(DATA_SOURCE == "RISK")

  #Filter encounter for valid visit type
  names(encounter)[names(encounter) %in% "VISITENC_ID"] <- "VISIT_ENCOUNTER_ID"
  encounter <- encounter%>% filter(
    TYPE_OF_ENCOUNTER  %in%  c("Enrollment Visit",
                               "6-Month Follow-up Visit",
                               "12-Month Follow-up Visit",
                               "18-Month Follow-up Visit",
                               "24-Month Follow-up Visit",
                               "30-Month Follow-up Visit",
                               "36-Month Follow-up Visit",
                               "42-Month Follow-up Visit",
                               "48-Month Follow-up Visit",
                               "54-Month Follow-up Visit",
                               "60-Month Follow-up Visit",
                               "66-Month Follow-up Visit",
                               "72-Month Follow-up Visit",
                               "78-Month Follow-up Visit",
                               "84-Month Follow-up Visit",
                               "90-Month Follow-up Visit",
                               "96-Month Follow-up Visit"))
  #Remove time from visit encounter start date
  encounter$VISIT_ENCOUNTER_START_DATE <- gsub(" 00:00:00.0", "", encounter$VISIT_ENCOUNTER_START_DATE)

  #Filter out empty test results in labs
  labs <- labs %>% filter(!is.na(TEST_RESULT_NUMERIC))

  data <- list(observations, encounter, labs)
  names(data) = c("observations","encounter","labs")

  #Filter observations for valid test results
  data$observations <- data$observations %>% filter(!is.na(DESCRIPTIVE_SYMP_TEST_RESULTS) | !is.na(TEST_RESULT_NUMERIC))
  #Select descriptive test results if numeric test results are not available
  data$observations$TEST_RESULT_NUMERIC[is.na(data$observations$TEST_RESULT_NUMERIC)] <- data$observations$DESCRIPTIVE_SYMP_TEST_RESULTS[is.na(data$observations$TEST_RESULT_NUMERIC)]
  data$observations1 <- data$observations[!grepl("Endoscopic", data$observations$OBS_TEST_CONCEPT_NAME),]
  data$observations2 <- data$observations[grepl("Endoscopic", data$observations$OBS_TEST_CONCEPT_NAME) & data$observations$ANA_SITE_CONCEPT_NAME %in% c("Rectum", "RECTUM"),]
  data$observations3 <- data$observations[grepl("Endoscopic", data$observations$OBS_TEST_CONCEPT_NAME) & data$observations$ANA_SITE_CONCEPT_NAME %in% "Ileum",]
  data$observations4 <- data$observations[grepl("Endoscopic", data$observations$OBS_TEST_CONCEPT_NAME) & data$observations$ANA_SITE_CONCEPT_NAME %in% "Ascending Colon",]
  data$observations5 <- data$observations[grepl("Endoscopic", data$observations$OBS_TEST_CONCEPT_NAME) & data$observations$ANA_SITE_CONCEPT_NAME %in% "Descending Colon",]
  data$observations6 <- data$observations[grepl("Endoscopic", data$observations$OBS_TEST_CONCEPT_NAME) & data$observations$ANA_SITE_CONCEPT_NAME %in% "Sigmoid Colon",]
  data$observations7 <- data$observations[grepl("Endoscopic", data$observations$OBS_TEST_CONCEPT_NAME) & data$observations$ANA_SITE_CONCEPT_NAME %in% "Transverse Colon",]
  data$observations8 <- data$observations[grepl("Endoscopic", data$observations$OBS_TEST_CONCEPT_NAME) & is.na(data$observations$ANA_SITE_CONCEPT_NAME),]

  data$observations1$ANA_SITE_CONCEPT_NAME[!data$observations1$OBS_TEST_CONCEPT_NAME %in% "Disease Location"] <- NA
  data$observations2$ANA_SITE_CONCEPT_NAME <- NULL
  data$observations3$ANA_SITE_CONCEPT_NAME <- NULL
  data$observations4$ANA_SITE_CONCEPT_NAME <- NULL
  data$observations5$ANA_SITE_CONCEPT_NAME <- NULL
  data$observations6$ANA_SITE_CONCEPT_NAME <- NULL
  data$observations7$ANA_SITE_CONCEPT_NAME <- NULL
  data$observations8$ANA_SITE_CONCEPT_NAME <- NULL

  data$observations <- NULL

  # pivot_wider
  y.vars     <- select.col
  value.vars <- select.val

  #Select and reshape tables based on the selected columns and values
  wide <- lapply(data, function(df){
    if(any(filter.col %in% names(df))){
      col <- filter.col[filter.col %in% names(df)]
      if(any(grepl(paste0(filter.val, collapse = "|"), df[,..col]))){
        df <- df[grepl(paste0(filter.val, collapse = "|"), df[,..col]), ]
      }
    }
    if(any(y.vars %in% names(df))){
      y.var <- y.vars[y.vars %in% names(df)]
      value.var <- value.vars[value.vars %in% names(df)]
      s <- Reduce(left_join, lapply(1:length(y.var), function(x) to_wide(df, y.var[x], value.var[x])))
      names(s) <- toupper(names(s))
      s
    }
  })

  # summarize wide diagnosis table by VISIT_ENCOUNTER_ID
  coalesce_by_column <- function(df) {
    return(coalesce(!!! as.list(df)))
  }


  #Rename columns
  names(wide$observations2)[5:length(wide$observations2)] <- paste0(names(wide$observations2)[5:length(wide$observations2)], " - RECTUM")
  names(wide$observations3)[5:length(wide$observations3)] <- paste0(names(wide$observations3)[5:length(wide$observations3)], " - ILEUM")
  names(wide$observations4)[5:length(wide$observations4)] <- paste0(names(wide$observations4)[5:length(wide$observations4)], " - ASCENDING COLON")
  names(wide$observations5)[5:length(wide$observations5)] <- paste0(names(wide$observations5)[5:length(wide$observations5)], " - DESCENDING COLON")
  names(wide$observations6)[5:length(wide$observations6)] <- paste0(names(wide$observations6)[5:length(wide$observations6)], " - SIGMOID")
  names(wide$observations7)[5:length(wide$observations7)] <- paste0(names(wide$observations7)[5:length(wide$observations7)], " - TRANSVERSE COLON")

  wide <- Filter(Negate(is.null), wide)

  for (i in 1:length(wide)){

    wide[[i]] = wide[[i]] %>% ungroup() %>% distinct()

  }

  o = data.frame(o = names(wide)) %>% arrange(match(o, c("encounter")))
  wide = wide[o$o]


  #rm(data)
  gc()

  wide$observations1 <- wide$observations1 %>%
    select(!`NA`)

  #Join all tables together

  visit <- Reduce(left_join, wide)

  if(any(names(visit) == "NA")){
    visit$`NA` <- NULL
  }
  visit$`DISEASE LOCATION` <- NULL
  visit$`DISEASE LOCATION AND BEHAVIOR REASSESSED` <- NULL

  #Collapse table by visit encounter id
  if(any(names(visit)=="VISIT_ENCOUNTER_ID")){
    visit <- data.table::as.data.table(visit)
    visit <- visit[, lapply(.SD, function(x){paste0(unique(x[!is.na(x)]), collapse="; ")}), by = VISIT_ENCOUNTER_ID]
  }

  #Merge Crohn's Disease, Ulcerative Colitis,	IBD unclassified and Not IBD into one column
  visit$`CROHN'S DISEASE`[visit$`CROHN'S DISEASE` %in% "Yes"]          <- "Crohn's Disease"
  visit$`ULCERATIVE COLITIS`[visit$`ULCERATIVE COLITIS` %in% "Yes"]    <- "Ulcerative Colitis"
  visit$`IBD UNCLASSIFIED`[visit$`IBD UNCLASSIFIED` %in% "Yes"]        <- "IBD Unclassified"
  visit$`NOT IBD`[visit$`NOT IBD` %in% "Yes"]                          <- "Not IBD"

  visit <- visit %>% mutate(DIAGNOSIS = paste0(visit$`CROHN'S DISEASE`, visit$`ULCERATIVE COLITIS`, visit$`IBD UNCLASSIFIED`, visit$`NOT IBD`))
  visit[,c("CROHN'S DISEASE", "ULCERATIVE COLITIS", "IBD UNCLASSIFIED", "NOT IBD")] <- list(NULL)


  #### PCDAI score calculation
  visit$`PCDAI - MOST SEVERE ABDOMINAL PAIN PAST 7 DAYS` <- as.numeric(visit$`PCDAI - MOST SEVERE ABDOMINAL PAIN PAST 7 DAYS`)
  visit$`PCDAI-PT FUNCTING,GENERAL WELLBEING PAST 7 DAY` <- as.numeric(visit$`PCDAI-PT FUNCTING,GENERAL WELLBEING PAST 7 DAY`)
  visit$`PCDAI - STOOLS PER DAY PAST 7 DAYS`             <- as.numeric(visit$`PCDAI - STOOLS PER DAY PAST 7 DAYS`)
  visit$`ERYTHROCYTE SEDIMENTATION RATE (ESR)` <- as.numeric(visit$`ERYTHROCYTE SEDIMENTATION RATE (ESR)`)#need to recode
  visit$`PCDAI - ESR` <- sapply(visit$`ERYTHROCYTE SEDIMENTATION RATE (ESR)`, function(x){
    if(!is.na(x)){
      if(x<20){0
      }else if(x>=20 & x<50){7.5
      }else if(x>=50){ 15
      }
    }else NA
  })
  visit$ALBUMIN <- as.numeric(visit$ALBUMIN) #Need to recode
  visit$`PCDAI - ALBUMIN` <- sapply(visit$ALBUMIN, function(x){
    if(!is.na(x)){
      if(x>=3.5){0
      }else if(x>=3.1 & x<=3.4){10
      }else if(x<=3){ 20
      }
    }else NA
  })
  visit$`PCDAI - WEIGHT` <- as.numeric(visit$`PCDAI - WEIGHT`)


  visit$`PCDAI - PERIRECTAL DISEASE` <- sapply(1:nrow(visit), function(x){
    if(visit$`PCDAI -PERIRECTAL DISEASE: ASYMPTOMATIC TAGS`[x] %in% c("Dat_0_no", "Dat_1_yes") &
       visit$`PCDAI -PERIRECTAL DISEASE: INDOLENT FISTULA`[x] %in% "Dat_0_no" &
       visit$`PCDAI-PERIRECTAL DISEASE:DRAINAGE OR TENDERNESS`[x] %in% "Dat_0_no" &
       visit$`PCDAI-PERIRECTAL DISEASE:ACTIVE FISTULA/ABSCESS`[x] %in% "Dat_0_no" &
       visit$`PCDAI -PERIRECTAL DISEASE: FISSURE`[x] %in% "Dat_0_no" &
       visit$`PCDAI -PERIRECTAL DISEASE: INFLAMMED TAGS`[x] %in% "Dat_0_no")
    {0
    }else if((visit$`PCDAI -PERIRECTAL DISEASE: INDOLENT FISTULA`[x] %in% "Dat_1_yes" |
              visit$`PCDAI -PERIRECTAL DISEASE: FISSURE`[x] %in% "Dat_1_yes" |
              visit$`PCDAI -PERIRECTAL DISEASE: INFLAMMED TAGS`[x] %in% "Dat_1_yes") &
             (visit$`PCDAI-PERIRECTAL DISEASE:DRAINAGE OR TENDERNESS`[x] %in% "Dat_0_no" &
              visit$`PCDAI-PERIRECTAL DISEASE:ACTIVE FISTULA/ABSCESS`[x] %in% "Dat_0_no")){
      7.5
    }else if(visit$`PCDAI-PERIRECTAL DISEASE:DRAINAGE OR TENDERNESS`[x] %in% "Dat_1_yes" |
             visit$`PCDAI-PERIRECTAL DISEASE:ACTIVE FISTULA/ABSCESS`[x] %in% "Dat_1_yes"){
      15
    }else if(visit$`PCDAI -PERIRECTAL DISEASE: ASYMPTOMATIC TAGS`[x] %in% "" |
             visit$`PCDAI -PERIRECTAL DISEASE: INDOLENT FISTULA`[x] %in% "" |
             visit$`PCDAI-PERIRECTAL DISEASE:DRAINAGE OR TENDERNESS`[x] %in% "" |
             visit$`PCDAI-PERIRECTAL DISEASE:ACTIVE FISTULA/ABSCESS`[x] %in% "" |
             visit$`PCDAI -PERIRECTAL DISEASE: FISSURE`[x] %in% "" |
             visit$`PCDAI -PERIRECTAL DISEASE: INFLAMMED TAGS`[x] %in% ""
    ){NA}
  })



  visit$`PCDAI - EIM` <- sapply(1:nrow(visit), function(x){
    if(all(c(visit$`PCDAI - EIM: DEFINITE ARTHRITIS`[x], visit$`PCDAI - EIM: E NODOSUM`[x],
             visit$`PCDAI-EIM:FEVER>38.5C FOR 3 DAYS OVER PAST WEEK`[x],  visit$`PCDAI - EIM: ORAL ULCERS`[x],
             visit$`PCDAI - EIM: P GANGRENOSUM`[x], visit$`PCDAI - EIM: UVEITIS`[x]) %in% "")){
      NA
    }else if(all(c(visit$`PCDAI - EIM: DEFINITE ARTHRITIS`[x], visit$`PCDAI - EIM: E NODOSUM`[x],
                   visit$`PCDAI-EIM:FEVER>38.5C FOR 3 DAYS OVER PAST WEEK`[x],  visit$`PCDAI - EIM: ORAL ULCERS`[x],
                   visit$`PCDAI - EIM: P GANGRENOSUM`[x], visit$`PCDAI - EIM: UVEITIS`[x]) %in% c("Dat_0_no", ""))){
      0
    }else if(any(c(visit$`PCDAI - EIM: DEFINITE ARTHRITIS`[x], visit$`PCDAI - EIM: E NODOSUM`[x],
                   visit$`PCDAI-EIM:FEVER>38.5C FOR 3 DAYS OVER PAST WEEK`[x],  visit$`PCDAI - EIM: ORAL ULCERS`[x],
                   visit$`PCDAI - EIM: P GANGRENOSUM`[x], visit$`PCDAI - EIM: UVEITIS`[x]) %in% "Dat_1_yes")){
      10
    }
  })

  # can't have NULL in the column to calculate
  visit <- visit %>%
    mutate(`PCDAI - PERIRECTAL DISEASE` = ifelse(`PCDAI - PERIRECTAL DISEASE` == "NULL", NA, `PCDAI - PERIRECTAL DISEASE`)) %>%
    mutate(`PCDAI - EIM` = ifelse(`PCDAI - EIM` == "NULL", NA, `PCDAI - EIM`)) %>%
    mutate(`PCDAI - PERIRECTAL DISEASE` = as.numeric(`PCDAI - PERIRECTAL DISEASE`)) %>%
    mutate(`PCDAI - EIM` = as.numeric(`PCDAI - EIM`))

  # calculate sum for wpcdai
  visit <- visit %>%
    rowwise() %>%
    mutate(wpcdai = sum(c(`PCDAI - MOST SEVERE ABDOMINAL PAIN PAST 7 DAYS`, `PCDAI-PT FUNCTING,GENERAL WELLBEING PAST 7 DAY`,
                          `PCDAI - STOOLS PER DAY PAST 7 DAYS`, `PCDAI - ESR`, `PCDAI - ALBUMIN`,
                          `PCDAI - WEIGHT`, `PCDAI - PERIRECTAL DISEASE`, `PCDAI - EIM`)))


  #Update database code values from PCDAI and other observation concepts to simpler Yes/No
  visit[visit == "Dat_0_no"] <- "No"
  visit[visit == "Dat_1_yes"] <- "Yes"
  visit[visit == "Db_0_no"] <- "No"
  visit[visit == "Db_1_yes"] <- "Yes"
  visit[visit == "Db_96_unknown"] <- "Unknown"
  visit[visit == "Db_99_unavailable"] <- "Unavailable"
  visit[visit == "Eas_0_no"] <- "No"
  visit[visit == "Eas_1_yes"] <- "Yes"

  # make wpcdai column name
  visit <- visit %>%
    rename("WPCDAI" = "wpcdai") %>%
    select("DEIDENTIFIED_MASTER_PATIENT_ID", "DEIDENTIFIED_PATIENT_ID",
           "DATA_SOURCE", "VISIT_ENCOUNTER_ID", "TYPE_OF_ENCOUNTER", "VISIT_ENCOUNTER_START_DATE",
           "WPCDAI", starts_with("PCDAI"))


  return(visit)

}
