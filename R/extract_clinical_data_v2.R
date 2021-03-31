

#' risk_extract_clinical_data
#'
#'
#' version 1.3: February 11, 2019
#' IBD Plexus, Crohn's & Colitis Foundation
#' Created by Victoria Zadorozhny, Rancho Biosciences
#'
#'
#' The following script reads RISK study clinical data tables from the
#' specified input directory, extracts specified columns and values,
#' calculates wPCDAI score, orders columns and outputs
#' a summary table as an excel workbook.
#'
#'
#' @param dir The directory where the unzipped data is saved.
#' @param select.col Columns to select.
#' @param select.val Columns with values for the selected columns. If select.val
#'   is the same as the select.col, then the whole column will be selected. If
#'   value is different than the select.col the selected column will be reshaped
#'   into a wide format with values from the select.val column.
#' @param filter.col Columns that need to be filtered prior to reshaping.
#' @param filter.val Values to filter on filter.col.
#' @param filename A filename to write the final output to.
#'
#' @return dataframe and wide excel file.
#' @export
risk_extract_clinical_data <- function(dir,

                                  select.col  = c("DIAG_CONCEPT_NAME", "DIAGNOSIS_DATE", "GENDER", "TYPE_OF_ENCOUNTER", "VISIT_ENCOUNTER_START_DATE", "AGE_AT_ENCOUNTER", "BIRTH_YEAR", "LAB_TEST_CONCEPT_NAME", "ASSAY.NAME", "MEDICATION_NAME", "OBS_TEST_CONCEPT_NAME", "ANA_SITE_CONCEPT_NAME", "BIOSAMPLE_CONCEPT_NAME"),
                                  select.val  = c("DIAG_STATUS_CONCEPT_NAME", "DIAGNOSIS_DATE", "GENDER", "TYPE_OF_ENCOUNTER", "VISIT_ENCOUNTER_START_DATE", "AGE_AT_ENCOUNTER", "BIRTH_YEAR", "TEST_RESULT_NUMERIC", "RAW.DATA.FILE.NAME", "MED_START_DATE", "TEST_RESULT_NUMERIC", "TEST_RESULT_NUMERIC", "Sample.Status"),
                                  filter.col  = c("DIAG_CONCEPT_NAME",
                                                  "OBS_TEST_CONCEPT_NAME"),
                               filter.val  = c("IBD - Family History", "Disease Location", "Endoscopic Assessment - Deep Ulceration", "Endoscopic Assessment - Superficial Ulceration", "Endoscopic Assessment - Amount of Surface Ulcerated", "Endoscopic Assessment - Amount of Surface Involved", "Perianal Disease -", "EIM", "Disease Behavior - Stricturing/Fibrostenotic", "Disease Behavior - Internally Pentrating", "PCDAI"),
                                  filename    = "RISK_Summary.xlsx"){

  #Read dictionary
  names <- header[,,1]

  #List files in directory - works for both .txt and .csv data extract from plexus
  list_df = paste0(dir, list.files(paste0(dir), pattern = ".csv|.txt"))
  list_df = grep("Encounter|Demographics|Diagnosis|Labs|Observations|Prescriptions|Procedures|Omics|Biosample|History|Problem|Family|Master|Vaccines|Biosample", list_df, value = TRUE)
  #list_df

  #Load Data
data = load_data(datadir = dir, cohort = "RISK", domains = c("ALL"), data_type = "BOTH")

  #rename list elements of data

  data$fam_hist_dem = data$family_history_demographics
  data$fam_hist_diag = data$family_history_diagnosis
  data$omics_patient = data$omics_patient_mapping

  data = within(data, rm(family_history_demographics, family_history_diagnosis,omics_patient_mapping))

  #Apply filters for each data table


  #Filter out other Data Sources besides RISK
  data$labs <- data$labs %>% filter(DATA_SOURCE == "RISK")
  data$encounter <- data$encounter %>% filter(DATA_SOURCE == "RISK")
  data$demographics <- data$demographics %>% filter(DATA_SOURCE == "RISK")
  data$diagnosis <- data$diagnosis %>% filter(DATA_SOURCE == "RISK")
  data$fam_hist_diag <- data$fam_hist_diag %>% filter(DATA_SOURCE == "RISK")
  data$fam_hist_dem <- data$fam_hist_dem %>% filter(DATA_SOURCE == "RISK")
  data$observations <- data$observations %>% filter(DATA_SOURCE == "RISK")
  #data$patient_problem <- data$patient_problem %>% filter(DATA_SOURCE == "RISK")
  data$prescriptions <- data$prescriptions %>% filter(DATA_SOURCE == "RISK")
  data$procedures <- data$procedures %>% filter(DATA_SOURCE == "RISK")

  #Filter diagnosis for patient's medical conditions (excluding leading question), extra-intestinal manifestations leading question (combining enrollment and follow-up), diagnosis date for IBD diseases at enrollment, and valid status concept name for Ankylising Spondlitis
  data$diagnosis$DIAG_CONCEPT_NAME[data$diagnosis$DIAG_CONCEPT_NAME %in% "Extra-Intestinal Manifestations Follow-up"] <- "Extra-Intestinal Manifestations"
  data$diagnosis$DIAG_STATUS_CONCEPT_NAME[data$diagnosis$DIAG_STATUS_CONCEPT_NAME %in% c("Yes/unknown","Yes/Unknown")  & !data$diagnosis$DIAG_CONCEPT_NAME %in% "Extra-Intestinal Manifestations"] <- NA
  data$diagnosis <- data$diagnosis %>% filter(!is.na(DIAG_STATUS_CONCEPT_NAME))
  data$diagnosis$DIAGNOSIS_DATE[!data$diagnosis$DIAG_CONCEPT_NAME  %in%  c("Crohn's Disease", "Crohn's disease", "Ulcerative colitis", "IBD unclassified", "Not IBD")] <- NA
  #data$diagnosis <- data$diagnosis[!(data$diagnosis$DIAG_CONCEPT_NAME  %in% "Ankylosing Spondylitis" & !data$diagnosis$FAMILY_MEMBER  %in% "Pt"),]
  data$diagnosis = data$diagnosis %>% filter(FAMILY_MEMBER == "Pt" | is.na(FAMILY_MEMBER))
  data$diagnosis = data$diagnosis %>% filter(DIAG_CONCEPT_NAME != "IBD - Family History")

  #Filter empty values in family history diagnosis
  data$fam_hist_diag <- data$fam_hist_diag %>% filter(!is.na(DIAG_STATUS_CONCEPT_NAME))

  #Filter encounter for valid visit type
  names(data$encounter)[names(data$encounter) %in% "VISITENC_ID"] <- "VISIT_ENCOUNTER_ID"
  data$encounter <- data$encounter%>% filter(
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
                                                                      "60-Month Follow-up Visit"))
  #Remove time from visit encounter start date
  data$encounter$VISIT_ENCOUNTER_START_DATE <- gsub(" 00:00:00.0", "", data$encounter$VISIT_ENCOUNTER_START_DATE)

  #Filter out empty test results in labs
  data$labs <- data$labs %>% filter(!is.na(TEST_RESULT_NUMERIC))

  #Filter prescriptions for summary madication administrated and medication action concept name
  data$prescriptions <- data$prescriptions %>% filter(MEDICATION_ADMINISTRATED  %in%  c("Yes", "Current", "No", "Not Current") | !is.na(MED_START_DATE) | !is.na(DOSE_OF_MEDICATION)) %>%
    filter(MED_ACTION_CONCEPT_NAME  %in%  c("Started Since Last Review", "Ongoing Treatment", "Received","Administered Again Since Last Dose 1"))

  #Remove time from medication start date
  data$prescriptions$MED_START_DATE <- gsub(" 00:00:00.0", "", data$prescriptions$MED_START_DATE)
  #Medication administrated if medication start date is missing
  data$prescriptions$MED_START_DATE[is.na(data$prescriptions$MED_START_DATE)] <- data$prescriptions$MEDICATION_ADMINISTRATED[is.na(data$prescriptions$MED_START_DATE)]
  #Divide prescriptions into started since last review and ongoing treatment
  data$prescriptions1 <- data$prescriptions[data$prescriptions$MED_ACTION_CONCEPT_NAME  %in%  c("Started Since Last Review", "Received"),]
  data$prescriptions2 <- data$prescriptions1[data$prescriptions1$MEDICATION_NAME %in% "Methotrexate" & data$prescriptions1$ROUTE_OF_MEDICATION %in% "SC/IM",]
  data$prescriptions3 <- data$prescriptions1[data$prescriptions1$MEDICATION_NAME %in% "Methotrexate" & data$prescriptions1$ROUTE_OF_MEDICATION %in% "Oral",]
  data$prescriptions1 <- data$prescriptions1[!data$prescriptions1$MEDICATION_NAME %in% "Methotrexate",]
  data$prescriptions4 <- data$prescriptions[data$prescriptions$MED_ACTION_CONCEPT_NAME  %in%  c("Ongoing Treatment","Administered Again Since Last Dose 1"),]
  data$prescriptions5 <- data$prescriptions4[data$prescriptions4$MEDICATION_NAME %in% "Methotrexate" & data$prescriptions4$ROUTE_OF_MEDICATION %in% "SC/IM",]
  data$prescriptions6 <- data$prescriptions4[data$prescriptions4$MEDICATION_NAME %in% "Methotrexate" & data$prescriptions4$ROUTE_OF_MEDICATION %in% "Oral",]
  data$prescriptions4 <- data$prescriptions4[!data$prescriptions4$MEDICATION_NAME %in% "Methotrexate",]

  data$prescriptions <- NULL

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

  #remove those without birth year in demographics

  data$demographics = data$demographics %>% filter(!is.na(BIRTH_YEAR))


  #Rename data source to match other tables
  data$biosample$DATA_SOURCE <- gsub("BIOSTORAGE_", "", data$biosample$DATA_SOURCE)

  data$biosample = data$biosample %>% distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DEIDENTIFIED_PATIENT_ID, DATA_SOURCE,VISIT_ENCOUNTER_ID, BIOSAMPLE_CONCEPT_NAME,Sample.Status)

  #Remove time stamp from date

  #data$biosample$Date.Sample.Collected <-  gsub(" 00:00:00.0", "", data$biosample$Date.Sample.Collected)

  #Replace missing dates with sample status
  #data$biosample$Date.Sample.Collected[is.na(data$biosample$Date.Sample.Collected)] <- data$biosample$Sample.Status[is.na(data$biosample$Date.Sample.Collected)]


  y.vars     <- select.col
  value.vars <- select.val

  #Select and reshape tables based on the selected columns and values
  wide <- lapply(data, function(df){
    if(any(filter.col %in% names(df))){
      col <- filter.col[filter.col %in% names(df)]
      if(any(grepl(paste0(filter.val, collapse = "|"), df[,col]))){
        df <- df[grepl(paste0(filter.val, collapse = "|"), df[,col]), ]
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


  #Rename columns
  names(wide$prescriptions2)[5:length(wide$prescriptions2)] <- paste0(names(wide$prescriptions2)[5:length(wide$prescriptions2)], " - SC/IM")
  names(wide$prescriptions3)[5:length(wide$prescriptions3)] <- paste0(names(wide$prescriptions3)[5:length(wide$prescriptions3)], " - ORAL")
  names(wide$prescriptions4)[5:length(wide$prescriptions4)] <- paste0("ONGOING_", names(wide$prescriptions4)[5:length(wide$prescriptions4)])
  names(wide$prescriptions5)[5:length(wide$prescriptions5)] <- paste0("ONGOING_", names(wide$prescriptions5)[5:length(wide$prescriptions5)], " - SC/IM")
  names(wide$prescriptions6)[5:length(wide$prescriptions6)] <- paste0("ONGOING_", names(wide$prescriptions6)[5:length(wide$prescriptions6)], " - ORAL")
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

  ####PCDAI score calculation
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

  visit$`WPCDAI` <- sapply(1:nrow(visit), function(x){
    sum(visit$`PCDAI - MOST SEVERE ABDOMINAL PAIN PAST 7 DAYS`[x],
        visit$`PCDAI-PT FUNCTING,GENERAL WELLBEING PAST 7 DAY`[x],
        visit$`PCDAI - STOOLS PER DAY PAST 7 DAYS`[x],
        visit$`PCDAI - ESR`[x],
        visit$`PCDAI - ALBUMIN`[x],
        visit$`PCDAI - WEIGHT`[x],
        visit$`PCDAI - PERIRECTAL DISEASE`[x],
        visit$`PCDAI - EIM`[x])
  }
  )

  #Update database code values from PCDAI and other observation concepts to simpler Yes/No
  visit[visit == "Dat_0_no"] <- "No"
  visit[visit == "Dat_1_yes"] <- "Yes"
  visit[visit == "Db_0_no"] <- "No"
  visit[visit == "Db_1_yes"] <- "Yes"
  visit[visit == "Db_96_unknown"] <- "Unknown"
  visit[visit == "Db_99_unavailable"] <- "Unavailable"
  visit[visit == "Eas_0_no"] <- "No"
  visit[visit == "Eas_1_yes"] <- "Yes"

  #Sort table by deidentified master patient ID and encounter date
  visit <- visit[order(visit$DEIDENTIFIED_MASTER_PATIENT_ID, visit$VISIT_ENCOUNTER_START_DATE),]

  #change column order
  data.table::setcolorder(visit, names)

  #Write output file
  write.xlsx(visit, filename)
  return(visit)
}


