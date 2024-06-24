#' calculate_disease_scores
#'
#' Creates an excel spreadsheet and list of data frames with diagnosis, sCDAI
#' (CD patients only), 6pt UCDAI (UC Patients only), Manitoba, PGA, MES (UC
#' patients only), SES-CD (CD patients only), PRO2 (CD patients only) for SPARC
#' patients
#' Data can be loaded for this function using:
#' data <- load_data(datadir = datadir, cohort = "SPARC", domains = c("Demographics", "Diagnosis", "Procedures", "Encounter", "Observations"), data_type = "CRF")
#'
#' @param demographics demographics data
#' @param diagnosis diagnosis data from diagnosis_crf table
#' @param procedures procedures data from procedures_crf table
#' @param encounter encounter data from encounter_crf table
#' @param observations observations data from encounter_crf table
#' @param export if excel spreadsheet should be exported. TRUE is default.
#'
#' @return An excel spreadsheet with the scores as different tabs and a list of dataframes with each score as a list element.
#' @export
calculate_disease_scores <- function(demographics, diagnosis, procedures, encounter, observations, export = TRUE) {
  # Update observations

  # LATEST DIAGNOSIS ----

  dx <- extract_diagnosis(diagnosis, encounter, demographics, "SPARC")

  dx <- dx %>% select(DEIDENTIFIED_MASTER_PATIENT_ID, DIAGNOSIS)


  # sCDAI ----

  scdai <- calculate_scdai(observations)

  scdai <- scdai %>%
    left_join(dx) %>%
    filter(DIAGNOSIS == "Crohn's Disease") %>%
    select(-DIAGNOSIS) %>%
    distinct() %>%
    ungroup()





  # 6/9 point UCDAI ----

  ucdai <- calculate_ucdai(observations)

  ucdai <- ucdai %>%
    left_join(dx) %>%
    filter(DIAGNOSIS == "Ulcerative Colitis") %>%
    select(-DIAGNOSIS) %>%
    distinct() %>%
    ungroup()



  # MANITOBA ----


  manitoba <- calculate_manitoba(observations)



  # PHYSCIAN'S GLOBAL ASSESSMENT (PGA) ----

  pga <- calculate_pga(observations)



  # SES-CD ----



  ses <- calculate_ses(procedures)

  ses <- ses %>%
    left_join(dx) %>%
    filter(DIAGNOSIS == "Crohn's Disease") %>%
    select(-DIAGNOSIS) %>%
    distinct() %>%
    ungroup()



  # Mayo Endocscopy Score ----

  mes <- calculate_mes(procedures)

  mes <- mes %>%
    left_join(dx) %>%
    filter(DIAGNOSIS == "Ulcerative Colitis") %>%
    select(-DIAGNOSIS) %>%
    distinct() %>%
    ungroup()



  # PRO2 ----

  pro2 <- calculate_pro2(observations)

  pro2 <- pro2 %>%
    left_join(dx) %>%
    filter(DIAGNOSIS == "Crohn's Disease") %>%
    select(-DIAGNOSIS) %>%
    distinct() %>%
    ungroup()

  # PRO3 ----

  pro3 <- calculate_pro3(observations)

  pro3 <- pro3 %>%
    left_join(dx) %>%
    filter(DIAGNOSIS == "Crohn's Disease") %>%
    select(-DIAGNOSIS) %>%
    distinct() %>%
    ungroup()


  # ALL SCORES ----

  sparc_scores <- list(diagnosis = dx, scdai = scdai, ucdai = ucdai, manitoba = manitoba, pga = pga, ses = ses, mes = mes, pro2 = pro2, pro3 = pro3)

  # sparc_scores <- lapply(sparc_scores,function(x) {colnames(x) <- toupper(colnames(x));x})

  if (export == "TRUE") {
    write.xlsx(sparc_scores, paste0("SPARC_scores_", Sys.Date(), ".xlsx"), colnames = T)
  }


  return(sparc_scores)
}
