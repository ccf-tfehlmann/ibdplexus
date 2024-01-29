#' full_pcdai
#'
#' A function that creates a table with the available full PCDAI scores for RISK patients.
#'
#' @param observations A dataframe with observation data usually generated using load_data.
#' @param labs A dataframe with lab data usually generated using load_data.
#' @param encounter A dataframe with encounter data usually generated using load_data.
#'
#' @return A dataframe with PCDAI scores
#' @export

full_pcdai <- function (observations, encounter, labs) {
  select.col <- c("DIAG_CONCEPT_NAME", "DIAGNOSIS_DATE", "GENDER",
                  "TYPE_OF_ENCOUNTER", "VISIT_ENCOUNTER_START_DATE", "AGE_AT_ENCOUNTER",
                  "BIRTH_YEAR", "LAB_TEST_CONCEPT_NAME", "ASSAY NAME",
                  "MEDICATION_NAME", "OBS_TEST_CONCEPT_NAME", "ANA_SITE_CONCEPT_NAME",
                  "SRC_BIOSAMPLE_CONCEPT_NAME")
  select.val <- c("DIAG_STATUS_CONCEPT_NAME", "DIAGNOSIS_DATE",
                  "GENDER", "TYPE_OF_ENCOUNTER", "VISIT_ENCOUNTER_START_DATE",
                  "AGE_AT_ENCOUNTER", "BIRTH_YEAR", "TEST_RESULT_NUMERIC",
                  "RAW DATA FILE NAME", "MED_START_DATE", "TEST_RESULT_NUMERIC",
                  "TEST_RESULT_NUMERIC", "SAMPLE_STATUS")
  filter.col <- c("DIAG_CONCEPT_NAME", "OBS_TEST_CONCEPT_NAME")
  filter.val <- c("Disease Location", "Endoscopic Assessment - Deep Ulceration",
                  "Endoscopic Assessment - Superficial Ulceration", "Endoscopic Assessment - Amount of Surface Ulcerated",
                  "Endoscopic Assessment - Amount of Surface Involved",
                  "Perianal Disease -", "EIM", "Disease Behavior - Stricturing/Fibrostenotic",
                  "Disease Behavior - Internally Pentrating", "PCDAI")
  names <- header
  names <- names[, , 1]
  observations <- observations %>% filter(DATA_SOURCE == "RISK")
  encounter <- encounter %>% filter(DATA_SOURCE == "RISK")
  labs <- labs %>% filter(DATA_SOURCE == "RISK")
  names(encounter)[names(encounter) %in% "VISITENC_ID"] <- "VISIT_ENCOUNTER_ID"
  encounter <- encounter %>% filter(TYPE_OF_ENCOUNTER %in%
                                      c("Enrollment Visit", "6-Month Follow-up Visit", "12-Month Follow-up Visit",
                                        "18-Month Follow-up Visit", "24-Month Follow-up Visit",
                                        "30-Month Follow-up Visit", "36-Month Follow-up Visit",
                                        "42-Month Follow-up Visit", "48-Month Follow-up Visit",
                                        "54-Month Follow-up Visit", "60-Month Follow-up Visit",
                                        "66-Month Follow-up Visit", "72-Month Follow-up Visit",
                                        "78-Month Follow-up Visit", "84-Month Follow-up Visit",
                                        "90-Month Follow-up Visit", "96-Month Follow-up Visit"))
  encounter$VISIT_ENCOUNTER_START_DATE <- gsub(" 00:00:00.0",
                                               "", encounter$VISIT_ENCOUNTER_START_DATE)
  labs <- labs %>% filter(!is.na(TEST_RESULT_NUMERIC))
  data <- list(observations, encounter, labs)
  names(data) <- c("observations", "encounter", "labs")
  data$observations <- data$observations %>% filter(!is.na(DESCRIPTIVE_SYMP_TEST_RESULTS) |
                                                      !is.na(TEST_RESULT_NUMERIC))
  data$observations$TEST_RESULT_NUMERIC[is.na(data$observations$TEST_RESULT_NUMERIC)] <- data$observations$DESCRIPTIVE_SYMP_TEST_RESULTS[is.na(data$observations$TEST_RESULT_NUMERIC)]
  data$observations1 <- data$observations[!grepl("Endoscopic",
                                                 data$observations$OBS_TEST_CONCEPT_NAME), ]
  data$observations2 <- data$observations[grepl("Endoscopic",
                                                data$observations$OBS_TEST_CONCEPT_NAME) & data$observations$ANA_SITE_CONCEPT_NAME %in%
                                            c("Rectum", "RECTUM"), ]
  data$observations3 <- data$observations[grepl("Endoscopic",
                                                data$observations$OBS_TEST_CONCEPT_NAME) & data$observations$ANA_SITE_CONCEPT_NAME %in%
                                            "Ileum", ]
  data$observations4 <- data$observations[grepl("Endoscopic",
                                                data$observations$OBS_TEST_CONCEPT_NAME) & data$observations$ANA_SITE_CONCEPT_NAME %in%
                                            "Ascending Colon", ]
  data$observations5 <- data$observations[grepl("Endoscopic",
                                                data$observations$OBS_TEST_CONCEPT_NAME) & data$observations$ANA_SITE_CONCEPT_NAME %in%
                                            "Descending Colon", ]
  data$observations6 <- data$observations[grepl("Endoscopic",
                                                data$observations$OBS_TEST_CONCEPT_NAME) & data$observations$ANA_SITE_CONCEPT_NAME %in%
                                            "Sigmoid Colon", ]
  data$observations7 <- data$observations[grepl("Endoscopic",
                                                data$observations$OBS_TEST_CONCEPT_NAME) & data$observations$ANA_SITE_CONCEPT_NAME %in%
                                            "Transverse Colon", ]
  data$observations8 <- data$observations[grepl("Endoscopic",
                                                data$observations$OBS_TEST_CONCEPT_NAME) & is.na(data$observations$ANA_SITE_CONCEPT_NAME),
  ]
  data$observations1$ANA_SITE_CONCEPT_NAME[!data$observations1$OBS_TEST_CONCEPT_NAME %in%
                                             "Disease Location"] <- NA
  data$observations2$ANA_SITE_CONCEPT_NAME <- NULL
  data$observations3$ANA_SITE_CONCEPT_NAME <- NULL
  data$observations4$ANA_SITE_CONCEPT_NAME <- NULL
  data$observations5$ANA_SITE_CONCEPT_NAME <- NULL
  data$observations6$ANA_SITE_CONCEPT_NAME <- NULL
  data$observations7$ANA_SITE_CONCEPT_NAME <- NULL
  data$observations8$ANA_SITE_CONCEPT_NAME <- NULL
  data$observations <- NULL
  y.vars <- select.col
  value.vars <- select.val
  wide <- lapply(data, function(df) {
    if (any(filter.col %in% names(df))) {
      col <- filter.col[filter.col %in% names(df)]
      if (any(grepl(paste0(filter.val, collapse = "|"),
                    df[, ..col]))) {
        df <- df[grepl(paste0(filter.val, collapse = "|"),
                       df[, ..col]), ]
      }
    }
    if (any(y.vars %in% names(df))) {
      y.var <- y.vars[y.vars %in% names(df)]
      value.var <- value.vars[value.vars %in% names(df)]
      s <- Reduce(left_join, lapply(1:length(y.var), function(x) to_wide(df,
                                                                         y.var[x], value.var[x])))
      names(s) <- toupper(names(s))
      s
    }
  })
  coalesce_by_column <- function(df) {
    return(coalesce(!!!as.list(df)))
  }
  names(wide$observations2)[5:length(wide$observations2)] <- paste0(names(wide$observations2)[5:length(wide$observations2)],
                                                                    " - RECTUM")
  names(wide$observations3)[5:length(wide$observations3)] <- paste0(names(wide$observations3)[5:length(wide$observations3)],
                                                                    " - ILEUM")
  names(wide$observations4)[5:length(wide$observations4)] <- paste0(names(wide$observations4)[5:length(wide$observations4)],
                                                                    " - ASCENDING COLON")
  names(wide$observations5)[5:length(wide$observations5)] <- paste0(names(wide$observations5)[5:length(wide$observations5)],
                                                                    " - DESCENDING COLON")
  names(wide$observations6)[5:length(wide$observations6)] <- paste0(names(wide$observations6)[5:length(wide$observations6)],
                                                                    " - SIGMOID")
  names(wide$observations7)[5:length(wide$observations7)] <- paste0(names(wide$observations7)[5:length(wide$observations7)],
                                                                    " - TRANSVERSE COLON")
  wide <- Filter(Negate(is.null), wide)
  for (i in 1:length(wide)) {
    wide[[i]] <- wide[[i]] %>% ungroup() %>% distinct()
  }
  o <- data.frame(o = names(wide)) %>% arrange(match(o, c("encounter")))
  wide <- wide[o$o]
  gc()
  wide$observations1 <- wide$observations1 %>% select(!`NA`)
  visit <- Reduce(left_join, wide)
  if (any(names(visit) == "NA")) {
    visit$`NA` <- NULL
  }
  visit$`DISEASE LOCATION` <- NULL
  visit$`DISEASE LOCATION AND BEHAVIOR REASSESSED` <- NULL
  if (any(names(visit) == "VISIT_ENCOUNTER_ID")) {
    visit <- data.table::as.data.table(visit)
    visit <- visit[, lapply(.SD, function(x) {
      paste0(unique(x[!is.na(x)]), collapse = "; ")
    }), by = VISIT_ENCOUNTER_ID]
  }
  visit$`CROHN'S DISEASE`[visit$`CROHN'S DISEASE` %in% "Yes"] <- "Crohn's Disease"
  visit$`ULCERATIVE COLITIS`[visit$`ULCERATIVE COLITIS` %in%
                               "Yes"] <- "Ulcerative Colitis"
  visit$`IBD UNCLASSIFIED`[visit$`IBD UNCLASSIFIED` %in% "Yes"] <- "IBD Unclassified"
  visit$`NOT IBD`[visit$`NOT IBD` %in% "Yes"] <- "Not IBD"
  visit <- visit %>% mutate(DIAGNOSIS = paste0(visit$`CROHN'S DISEASE`,
                                               visit$`ULCERATIVE COLITIS`, visit$`IBD UNCLASSIFIED`,
                                               visit$`NOT IBD`))
  visit[, c("CROHN'S DISEASE", "ULCERATIVE COLITIS", "IBD UNCLASSIFIED",
            "NOT IBD")] <- list(NULL)
  visit$`PCDAI - MOST SEVERE ABDOMINAL PAIN PAST 7 DAYS` <- as.numeric(visit$`PCDAI - MOST SEVERE ABDOMINAL PAIN PAST 7 DAYS`)
  visit$`PCDAI-PT FUNCTING,GENERAL WELLBEING PAST 7 DAY` <- as.numeric(visit$`PCDAI-PT FUNCTING,GENERAL WELLBEING PAST 7 DAY`)
  visit$`PCDAI - STOOLS PER DAY PAST 7 DAYS` <- as.numeric(visit$`PCDAI - STOOLS PER DAY PAST 7 DAYS`)
  visit$`ERYTHROCYTE SEDIMENTATION RATE (ESR)` <- as.numeric(visit$`ERYTHROCYTE SEDIMENTATION RATE (ESR)`)
  visit$`PCDAI - ESR` <- sapply(visit$`ERYTHROCYTE SEDIMENTATION RATE (ESR)`,
                                function(x) {
                                  if (!is.na(x)) {
                                    if (x < 20) {
                                      0
                                    }
                                    else if (x >= 20 & x <= 50) {
                                      7.5
                                    }
                                    else if (x > 50) {
                                      15
                                    }
                                  }
                                  else {
                                    NA
                                  }
                                })


  visit$ALBUMIN <- as.numeric(visit$ALBUMIN)
  visit$`PCDAI - ALBUMIN` <- sapply(visit$ALBUMIN, function(x) {
    if (!is.na(x)) {
      if (x >= 3.5) {
        0
      }
      else if (x >= 3.1 & x <= 3.4) {
        10
      }
      else if (x <= 3) {
        20
      }
    }
    else {
      NA
    }
  })

  # full pcdai add hct -
  # NEED TO ADD BY AGE
  visit$HEMATOCRIT <- as.numeric(visit$HEMATOCRIT)
  visit$`PCDAI - HEMATOCRIT` <- sapply(visit$HEMATOCRIT, function(x) {
    if (!is.na(x)) {
      if (x >= 3.5) {
        0
      }
      else if (x >= 3.1 & x <= 3.4) {
        10
      }
      else if (x <= 3) {
        20
      }
    }
    else {
      NA
    }
  })


  visit$`PCDAI - WEIGHT` <- as.numeric(visit$`PCDAI - WEIGHT`)
  visit$`PCDAI - PERIRECTAL DISEASE` <- sapply(1:nrow(visit),
                                               function(x) {
                                                 if (visit$`PCDAI -PERIRECTAL DISEASE: ASYMPTOMATIC TAGS`[x] %in%
                                                     c("Dat_0_no", "Dat_1_yes") & visit$`PCDAI -PERIRECTAL DISEASE: INDOLENT FISTULA`[x] %in%
                                                     "Dat_0_no" & visit$`PCDAI-PERIRECTAL DISEASE:DRAINAGE OR TENDERNESS`[x] %in%
                                                     "Dat_0_no" & visit$`PCDAI-PERIRECTAL DISEASE:ACTIVE FISTULA/ABSCESS`[x] %in%
                                                     "Dat_0_no" & visit$`PCDAI -PERIRECTAL DISEASE: FISSURE`[x] %in%
                                                     "Dat_0_no" & visit$`PCDAI -PERIRECTAL DISEASE: INFLAMMED TAGS`[x] %in%
                                                     "Dat_0_no") {
                                                   0
                                                 }
                                                 else if ((visit$`PCDAI -PERIRECTAL DISEASE: INDOLENT FISTULA`[x] %in%
                                                           "Dat_1_yes" | visit$`PCDAI -PERIRECTAL DISEASE: FISSURE`[x] %in%
                                                           "Dat_1_yes" | visit$`PCDAI -PERIRECTAL DISEASE: INFLAMMED TAGS`[x] %in%
                                                           "Dat_1_yes") & (visit$`PCDAI-PERIRECTAL DISEASE:DRAINAGE OR TENDERNESS`[x] %in%
                                                                           "Dat_0_no" & visit$`PCDAI-PERIRECTAL DISEASE:ACTIVE FISTULA/ABSCESS`[x] %in%
                                                                           "Dat_0_no")) {
                                                   7.5
                                                 }
                                                 else if (visit$`PCDAI-PERIRECTAL DISEASE:DRAINAGE OR TENDERNESS`[x] %in%
                                                          "Dat_1_yes" | visit$`PCDAI-PERIRECTAL DISEASE:ACTIVE FISTULA/ABSCESS`[x] %in%
                                                          "Dat_1_yes") {
                                                   15
                                                 }
                                                 else if (visit$`PCDAI -PERIRECTAL DISEASE: ASYMPTOMATIC TAGS`[x] %in%
                                                          "" | visit$`PCDAI -PERIRECTAL DISEASE: INDOLENT FISTULA`[x] %in%
                                                          "" | visit$`PCDAI-PERIRECTAL DISEASE:DRAINAGE OR TENDERNESS`[x] %in%
                                                          "" | visit$`PCDAI-PERIRECTAL DISEASE:ACTIVE FISTULA/ABSCESS`[x] %in%
                                                          "" | visit$`PCDAI -PERIRECTAL DISEASE: FISSURE`[x] %in%
                                                          "" | visit$`PCDAI -PERIRECTAL DISEASE: INFLAMMED TAGS`[x] %in%
                                                          "") {
                                                   NA
                                                 }
                                               })
  visit$`PCDAI - EIM` <- sapply(1:nrow(visit), function(x) {
    if (all(c(visit$`PCDAI - EIM: DEFINITE ARTHRITIS`[x],
              visit$`PCDAI - EIM: E NODOSUM`[x], visit$`PCDAI-EIM:FEVER>38.5C FOR 3 DAYS OVER PAST WEEK`[x],
              visit$`PCDAI - EIM: ORAL ULCERS`[x], visit$`PCDAI - EIM: P GANGRENOSUM`[x],
              visit$`PCDAI - EIM: UVEITIS`[x]) %in% "")) {
      NA
    }
    else if (all(c(visit$`PCDAI - EIM: DEFINITE ARTHRITIS`[x],
                   visit$`PCDAI - EIM: E NODOSUM`[x], visit$`PCDAI-EIM:FEVER>38.5C FOR 3 DAYS OVER PAST WEEK`[x],
                   visit$`PCDAI - EIM: ORAL ULCERS`[x], visit$`PCDAI - EIM: P GANGRENOSUM`[x],
                   visit$`PCDAI - EIM: UVEITIS`[x]) %in% c("Dat_0_no",
                                                           ""))) {
      0
    }
    else if (any(c(visit$`PCDAI - EIM: DEFINITE ARTHRITIS`[x],
                   visit$`PCDAI - EIM: E NODOSUM`[x], visit$`PCDAI-EIM:FEVER>38.5C FOR 3 DAYS OVER PAST WEEK`[x],
                   visit$`PCDAI - EIM: ORAL ULCERS`[x], visit$`PCDAI - EIM: P GANGRENOSUM`[x],
                   visit$`PCDAI - EIM: UVEITIS`[x]) %in% "Dat_1_yes")) {
      10
    }
  })
  visit <- visit %>% mutate(`PCDAI - PERIRECTAL DISEASE` = ifelse(`PCDAI - PERIRECTAL DISEASE` ==
                                                                    "NULL", NA, `PCDAI - PERIRECTAL DISEASE`)) %>% mutate(`PCDAI - EIM` = ifelse(`PCDAI - EIM` ==
                                                                                                                                                   "NULL", NA, `PCDAI - EIM`)) %>% mutate(`PCDAI - PERIRECTAL DISEASE` = as.numeric(`PCDAI - PERIRECTAL DISEASE`)) %>%
    mutate(`PCDAI - EIM` = as.numeric(`PCDAI - EIM`))

  # full PCDAI

  pcdai_start1 <- visit %>%
    select(VISIT_ENCOUNTER_ID, DEIDENTIFIED_MASTER_PATIENT_ID, DATA_SOURCE, TYPE_OF_ENCOUNTER,
           VISIT_ENCOUNTER_START_DATE, AGE_AT_ENCOUNTER, starts_with("PCDAI"), HEMATOCRIT, `ERYTHROCYTE SEDIMENTATION RATE (ESR)`,
           ALBUMIN) %>%
    left_join(master_patient %>% select(DEIDENTIFIED_MASTER_PATIENT_ID, GENDER),
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID))

  ## labs
  # HCT (%), ESR (mm/hr), ALBUMIN (g/dL)

  ## abdominal pain -
  # none - 0
  # mild - 5
  # mod/sev - 10
  # PCDAI - MOST SEVERE ABDOMINAL PAIN PAST 7 DAYS

  ## stools (per day)
  # formed or one liquid, no blood - 0
  # Up to 2 semi-formed with small blood, or 2-5 liquid with or without small
  # blood - 5
  # any gross bleeding or 6 or more or nocturnal diarrhea - 10
  # PCDAI - STOOLS PER DAY PAST 7 DAYS

  ## general well being
  # no limits, well -  0
  # occassional difficulty in maintaining, below par - 5
  # frequent limitation, very poor - 10
  # PCDAI-PT FUNCTING,GENERAL WELLBEING PAST 7 DAY

  pcdai_start <- pcdai_start1 %>%
    # ibdplexus:::fix_col_names()
    mutate(`PCDAI - MOST SEVERE ABDOMINAL PAIN PAST 7 DAYS` = case_when(
      `PCDAI - MOST SEVERE ABDOMINAL PAIN PAST 7 DAYS` == 10 ~ 5,
      `PCDAI - MOST SEVERE ABDOMINAL PAIN PAST 7 DAYS` == 20 ~ 20,
      TRUE ~ `PCDAI - MOST SEVERE ABDOMINAL PAIN PAST 7 DAYS`
    )) %>%
    mutate(`PCDAI - STOOLS PER DAY PAST 7 DAYS` = case_when(
      `PCDAI - STOOLS PER DAY PAST 7 DAYS` == 7.5 ~ 5,
      `PCDAI - STOOLS PER DAY PAST 7 DAYS` == 15 ~ 10,
      TRUE ~ `PCDAI - STOOLS PER DAY PAST 7 DAYS`
    )) %>%
    mutate(`PCDAI-PT FUNCTING,GENERAL WELLBEING PAST 7 DAY` = case_when(
      `PCDAI-PT FUNCTING,GENERAL WELLBEING PAST 7 DAY` == 10 ~ 5,
      `PCDAI-PT FUNCTING,GENERAL WELLBEING PAST 7 DAY` == 20 ~ 10,
      TRUE ~ `PCDAI-PT FUNCTING,GENERAL WELLBEING PAST 7 DAY`
    )) %>%
    mutate(HEMATOCRIT = as.numeric(HEMATOCRIT)) %>%
    mutate(ALBUMIN = as.numeric(ALBUMIN)) %>%
    mutate(`ERYTHROCYTE SEDIMENTATION RATE (ESR)` = as.numeric(`ERYTHROCYTE SEDIMENTATION RATE (ESR)`)) %>%
    # check with tara why not available but HEMATOCRIT is still there
    mutate(`PCDAI - HCT` = case_when(
      # `PCDAI - HCT (%) WITHIN LAST 2 WEEKS` != "Not available" &
      AGE_AT_ENCOUNTER <= 10 & GENDER == "Male" & HEMATOCRIT > 33 ~ 0,
      # `PCDAI - HCT (%) WITHIN LAST 2 WEEKS` != "Not available" &
      AGE_AT_ENCOUNTER < 15 & AGE_AT_ENCOUNTER > 10 & GENDER == "Male" & HEMATOCRIT > 35 ~ 0,
      # `PCDAI - HCT (%) WITHIN LAST 2 WEEKS` != "Not available" &
      AGE_AT_ENCOUNTER >= 15 & GENDER == "Male" & HEMATOCRIT >= 37 ~ 0,
      # `PCDAI - HCT (%) WITHIN LAST 2 WEEKS` != "Not available" &
      AGE_AT_ENCOUNTER <= 10 & GENDER == "Male" & HEMATOCRIT < 33 & HEMATOCRIT >= 28 ~ 2.5,
      # `PCDAI - HCT (%) WITHIN LAST 2 WEEKS` != "Not available" &
      AGE_AT_ENCOUNTER < 15 & AGE_AT_ENCOUNTER > 10 & GENDER == "Male" & HEMATOCRIT < 35 & HEMATOCRIT >= 30 ~ 2.5,
      # `PCDAI - HCT (%) WITHIN LAST 2 WEEKS` != "Not available" &
      AGE_AT_ENCOUNTER >= 15 & GENDER == "Male" & HEMATOCRIT < 37 & HEMATOCRIT >= 32 ~ 2.5,
      # `PCDAI - HCT (%) WITHIN LAST 2 WEEKS` != "Not available" &
      AGE_AT_ENCOUNTER <= 10 & GENDER == "Male" & HEMATOCRIT < 28 ~ 5,
      # `PCDAI - HCT (%) WITHIN LAST 2 WEEKS` != "Not available" &
      AGE_AT_ENCOUNTER < 15 & AGE_AT_ENCOUNTER > 10 & GENDER == "Male" & HEMATOCRIT < 30 ~ 5,
      # `PCDAI - HCT (%) WITHIN LAST 2 WEEKS` != "Not available" &
      AGE_AT_ENCOUNTER >= 15 & GENDER == "Male" & HEMATOCRIT < 32 ~ 5,
      # `PCDAI - HCT (%) WITHIN LAST 2 WEEKS` != "Not available" &
      AGE_AT_ENCOUNTER >= 11 & AGE_AT_ENCOUNTER <= 19 & GENDER == "Female" & HEMATOCRIT >= 34 ~ 0,
      # `PCDAI - HCT (%) WITHIN LAST 2 WEEKS` != "Not available" &
      AGE_AT_ENCOUNTER >= 11 & AGE_AT_ENCOUNTER <= 19 & GENDER == "Female" & HEMATOCRIT < 34 & HEMATOCRIT >= 29 ~ 2.5,
      # `PCDAI - HCT (%) WITHIN LAST 2 WEEKS` != "Not available" &
      AGE_AT_ENCOUNTER >= 11 & AGE_AT_ENCOUNTER <= 19 & GENDER == "Female" & HEMATOCRIT < 29 ~ 5,
      TRUE ~ NA
    )) %>%
    mutate(`PCDAI - ESR`= case_when(
      # `PCDAI - ESR (MM/HR) WITHIN LAST 2 WEEKS` != "Not Available" &
      `ERYTHROCYTE SEDIMENTATION RATE (ESR)` < 20 ~ 0,
      # `PCDAI - ESR (MM/HR) WITHIN LAST 2 WEEKS` != "Not Available" &
      `ERYTHROCYTE SEDIMENTATION RATE (ESR)` >= 20 &
        `ERYTHROCYTE SEDIMENTATION RATE (ESR)` <= 50 ~ 2.5,
      # `PCDAI - ESR (MM/HR) WITHIN LAST 2 WEEKS` != "Not Available" &
      `ERYTHROCYTE SEDIMENTATION RATE (ESR)` > 50 ~ 5,
      TRUE ~ NA
    )) %>%
    mutate(`PCDAI - ALBUMIN`= case_when(
      # `PCDAI - ALBUMIN (G/DL) WITHIN LAST 2 WEEKS` != "Not Available" &
      `ALBUMIN` >= 3.5 ~ 0,
      # `PCDAI - ALBUMIN (G/DL) WITHIN LAST 2 WEEKS` != "Not Available" &
      `ALBUMIN` > 3 & ALBUMIN < 3.5  ~ 5,
      # `PCDAI - ALBUMIN (G/DL) WITHIN LAST 2 WEEKS` != "Not Available" &
      `ALBUMIN` <= 3 ~ 10,
      TRUE ~ NA
    )) %>%
    # weight all set - 0=Weight gain or voluntary weight stable/loss, 5=Involuntary
    # weight stable, weight loss 1-9%, 10=Weight loss >/= 10%
    # height all set - 0=< 1 channel decrease, 5=>/= 1 and < 2 channel decrease
    # , 10=> 2 channel decrease `PCDAI - CHANNEL DECREASE`
    # abdomen - combine `PCDAI - ABDOMINAL TENDERNESS` & `PCDAI - ABDOMINAL MASS`,
    # PCDAI -INVOLUNTARY ABDOMINAL GUARDING
    # check this logic -- what if guarding but no tenderness ex?
    mutate(across(c("PCDAI - ABDOMINAL TENDERNESS", "PCDAI - ABDOMINAL MASS",
                    "PCDAI -INVOLUNTARY ABDOMINAL GUARDING"), ~ifelse(.x == "Dat_0_no" | .x == "Dat_1_yes", .x, NA))) %>%
    mutate(`PCDAI - ABDOMEN` = case_when(
      `PCDAI - ABDOMINAL TENDERNESS` == "Dat_0_no" & `PCDAI - ABDOMINAL MASS` == "Dat_0_no" &
        `PCDAI -INVOLUNTARY ABDOMINAL GUARDING` != "Dat_1_yes" ~ 0,
      `PCDAI - ABDOMINAL TENDERNESS` == "Dat_1_yes" & is.na(`PCDAI - ABDOMINAL MASS`) |
        `PCDAI - ABDOMINAL MASS` == "Dat_1_yes" & `PCDAI - ABDOMINAL TENDERNESS` == "Dat_0_no" &
        `PCDAI -INVOLUNTARY ABDOMINAL GUARDING` != "Dat_1_yes" |
        `PCDAI - ABDOMINAL MASS` == "Dat_0_no" & `PCDAI - ABDOMINAL TENDERNESS` == "Dat_1_yes" &
        `PCDAI -INVOLUNTARY ABDOMINAL GUARDING` == "Dat_0_no" ~ 5,
      `PCDAI - ABDOMINAL TENDERNESS` == "Dat_1_yes" & `PCDAI - ABDOMINAL MASS` == "Dat_1_yes"  ~ 10,
      `PCDAI - ABDOMINAL TENDERNESS` == "Dat_0_no" & `PCDAI - ABDOMINAL MASS` == "Dat_0_no" &
        `PCDAI -INVOLUNTARY ABDOMINAL GUARDING` == "Dat_1_yes" ~ 10,
      `PCDAI - ABDOMINAL TENDERNESS` == "Dat_1_yes" & `PCDAI - ABDOMINAL MASS` == "Dat_0_no" &
        `PCDAI -INVOLUNTARY ABDOMINAL GUARDING` == "Dat_1_yes" ~ 10,
      TRUE ~ NA
    )) %>%
    # most conservative abdominal score approach
    mutate(`PCDAI - ABDOMEN` = ifelse(is.na(`PCDAI - ABDOMINAL TENDERNESS`) |
                                        is.na(`PCDAI -INVOLUNTARY ABDOMINAL GUARDING`) |
                                        is.na(`PCDAI - ABDOMINAL MASS`), NA, `PCDAI - ABDOMEN`)) %>%
    # convert perirectal disease column from wpcdai calculation
    mutate(`PCDAI - PERIRECTAL` = case_when(
      `PCDAI - PERIRECTAL DISEASE` == 0 ~ 0,
      `PCDAI - PERIRECTAL DISEASE` == 7.5 ~ 5,
      `PCDAI - PERIRECTAL DISEASE` == 15 ~ 10,
      TRUE ~ NA
    )) %>%
    # should I count blank as no eim or as having an eim
    mutate(across(c(starts_with("PCDAI - EIM:")), ~ifelse(.x == "", NA, .x))) %>%
    mutate(across(c(starts_with("PCDAI - EIM:")), ~ifelse(.x == "Dat_0_no", 0, .x))) %>%
    mutate(across(c(starts_with("PCDAI - EIM:")), ~ifelse(.x == "Dat_1_yes", 1, .x))) %>%
    mutate(across(c(starts_with("PCDAI-EIM:")), ~ifelse(.x == "", NA, .x))) %>%
    mutate(across(c(starts_with("PCDAI-EIM:")), ~ifelse(.x == "Dat_0_no", 0, .x))) %>%
    mutate(across(c(starts_with("PCDAI-EIM:")), ~ifelse(.x == "Dat_1_yes", 1, .x))) %>%
    mutate(`PCDAI - EIM SUM` = as.numeric(`PCDAI - EIM: DEFINITE ARTHRITIS`) + as.numeric(`PCDAI - EIM: E NODOSUM`) +
             as.numeric(`PCDAI-EIM:FEVER>38.5C FOR 3 DAYS OVER PAST WEEK`) + as.numeric(`PCDAI - EIM: ORAL ULCERS`) +
             as.numeric(`PCDAI - EIM: P GANGRENOSUM`) + as.numeric(`PCDAI - EIM: UVEITIS`)) %>%
    mutate(`PCDAI - EIM` = case_when(
      `PCDAI - EIM SUM` == 0 ~ 0,
      `PCDAI - EIM SUM` == 1 ~ 5,
      `PCDAI - EIM SUM` >= 2 ~ 10,
      TRUE ~ NA
    )) %>%
    mutate(FULL_PCDAI = as.numeric(`PCDAI - EIM`) + as.numeric(`PCDAI - MOST SEVERE ABDOMINAL PAIN PAST 7 DAYS`) +
             as.numeric(`PCDAI - STOOLS PER DAY PAST 7 DAYS`) + as.numeric(`PCDAI-PT FUNCTING,GENERAL WELLBEING PAST 7 DAY`) +
             as.numeric(`PCDAI - HCT`) + as.numeric(`PCDAI - ESR`) + as.numeric(`PCDAI - ALBUMIN`) +
             as.numeric(`PCDAI - ABDOMEN`) + as.numeric(`PCDAI - CHANNEL DECREASE`) + as.numeric(`PCDAI - WEIGHT`) +
             as.numeric(`PCDAI - PERIRECTAL`)) %>%
    mutate(FULL_PCDAI_CAT = case_when(FULL_PCDAI < 10 ~ "Remission",
                                      FULL_PCDAI >= 10 & FULL_PCDAI <= 30 ~ "Mild",
                                      FULL_PCDAI > 30 ~ "Mod/Severe",
                                      TRUE ~ NA)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, FULL_PCDAI, FULL_PCDAI_CAT,
           `PCDAI - EIM`, `PCDAI - MOST SEVERE ABDOMINAL PAIN PAST 7 DAYS`,
           `PCDAI - STOOLS PER DAY PAST 7 DAYS`, `PCDAI-PT FUNCTING,GENERAL WELLBEING PAST 7 DAY`,
           `PCDAI - HCT`, `PCDAI - ESR`, `PCDAI - ALBUMIN`,
           `PCDAI - ABDOMEN`, `PCDAI - CHANNEL DECREASE`, `PCDAI - WEIGHT`,
           `PCDAI - PERIRECTAL`) %>%
    filter(!is.na(FULL_PCDAI))

  return(pcdai_start)

}
