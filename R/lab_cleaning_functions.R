#' crp_active_clean
#'
#' Function that cleans and creates a flag for active level for CRP.
#'
#' @param crp The table of CRP labs created by extract_lab_emr.
#' @param labs The table of labs from the ddm extract
#'
#' @return Table with active or inactive flag for CRP level with an index date.

crp_active_clean <- function(crp, labs){

  crp_cleaned1 <- crp %>%
    mutate(drop = ifelse(is.na(test_unit_cln) & lab_result_cln != "LOWER LIMIT", 1, 0)) %>%
    mutate(test_unit_cln = ifelse(test_unit_cln == "MILLIGRAMS PER DECILTER", "MG/DL", test_unit_cln)) %>%
    mutate(drop = ifelse(lab_result_cln == "LOWER LIMIT" | lab_result_cln == "UPPER LIMIT", 0, drop)) %>%
    mutate(drop = ifelse(test_unit_cln != "MG/DL" & test_unit_cln != "MG/L", 1, drop)) %>%
    filter(drop != 1) %>%
    select(-drop) %>%
    mutate(CRP_ACTIVE = ifelse(lab_result_cln == "LOWER LIMIT", "INACTIVE", NA)) %>%
    mutate(CRP_ACTIVE = ifelse(lab_result_cln == "UPPER LIMIT", "ACTIVE", CRP_ACTIVE)) %>%
    mutate(lab_result_cln_numeric = as.numeric(lab_result_cln)) %>%
    mutate(lab_result_cln_numeric = ifelse(test_unit_cln == "MG/DL", lab_result_cln_numeric *10,
                                           lab_result_cln_numeric)) %>%
    mutate(CRP_ACTIVE = ifelse(lab_result_cln_numeric < 8, "INACTIVE", CRP_ACTIVE)) %>%
    mutate(CRP_ACTIVE = ifelse(is.na(CRP_ACTIVE), "ACTIVE", CRP_ACTIVE)) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, SPECIMEN_COLLECTION_DATE, CRP_ACTIVE) %>%
    mutate(CRP_SPECIMEN_COLLECTION_DATE = dmy(SPECIMEN_COLLECTION_DATE)) %>%
    select(-SPECIMEN_COLLECTION_DATE) %>%
    mutate(CRP_SOURCE = "EMR")

  crp_central_lab <- labs %>%
    filter(LAB_TEST_CONCEPT_CODE == "hsCRP (mg/L)")

  crp_cleaned <- crp_central_lab %>%
    mutate(CRP_ACTIVE = ifelse(TEST_RESULT_NUMERIC < 8, "INACTIVE", NA)) %>%
    mutate(CRP_ACTIVE = ifelse(is.na(CRP_ACTIVE), "ACTIVE", CRP_ACTIVE)) %>%
    mutate(CRP_SPECIMEN_COLLECTION_DATE = dmy(SPECIMEN_COLLECTION_DATE)) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, CRP_SPECIMEN_COLLECTION_DATE, CRP_ACTIVE) %>%
    mutate(CRP_SOURCE = "CENTRAL LAB") %>%
    rbind(crp_cleaned1)


  return(crp_cleaned)

}

#' fcal_active_clean
#'
#' Function that cleans and creates a flag for active level for fecalcalprotectin.
#'
#' @param fcal The table of FCAL labs created by extract_lab_emr
#' @param labs The ddm extract with the labs table
#'
#' @return Table with active or inactive flag for FCAL level with an index date.

fcal_active_clean <- function(fcal, labs) {

  fcal_cleaned1 <- fcal %>%
    mutate(test_unit_cln = ifelse(test_unit_cln == "MCG/G" | test_unit_cln == "UG/GRAM" |
                                    test_unit_cln == "UG/GRAM OF STOOL" | test_unit_cln == "MCG/GM" |
                                    test_unit_cln == "MCG.GM" | test_unit_cln == "MG/KG", 'UG/G', test_unit_cln)) %>%
    mutate(FCAL_ACTIVE = ifelse(lab_result_cln == "LOWER LIMIT", "INACTIVE", NA)) %>%
    mutate(FCAL_ACTIVE = ifelse(lab_result_cln == "UPPER LIMIT", "ACTIVE", FCAL_ACTIVE)) %>%
    mutate(lab_result_cln_numeric = as.numeric(lab_result_cln)) %>%
    mutate(FCAL_ACTIVE = ifelse(lab_result_cln_numeric < 50 & is.na(FCAL_ACTIVE) &
                                  test_unit_cln == "UG/G", "INACTIVE", FCAL_ACTIVE)) %>%
    mutate(FCAL_ACTIVE = ifelse(lab_result_cln_numeric >= 50 & lab_result_cln_numeric <= 200 & is.na(FCAL_ACTIVE) &
                                  test_unit_cln == "UG/G",
                                "INDETERMINATE", FCAL_ACTIVE)) %>%
    mutate(FCAL_ACTIVE = ifelse(lab_result_cln_numeric > 200 & is.na(FCAL_ACTIVE) &
                                  test_unit_cln == "UG/G", "ACTIVE", FCAL_ACTIVE)) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, SPECIMEN_COLLECTION_DATE, FCAL_ACTIVE) %>%
    filter(!is.na(FCAL_ACTIVE)) %>%
    mutate(FCAL_SPECIMEN_COLLECTION_DATE = dmy(SPECIMEN_COLLECTION_DATE)) %>%
    select(-SPECIMEN_COLLECTION_DATE) %>%
    mutate(FCAL_SOURCE = "EMR")

  fcal_central_lab <- labs %>%
    filter(LAB_TEST_CONCEPT_CODE == "BÜHLMANN fCAL® ELISA (1:50 dilution)")

  fcal_cleaned <- fcal_central_lab %>%
    mutate(FCAL_ACTIVE = ifelse(TEST_RESULT_NUMERIC < 50, "INACTIVE", NA)) %>%
    mutate(FCAL_ACTIVE = ifelse(TEST_RESULT_NUMERIC >= 50 & TEST_RESULT_NUMERIC <= 200 & is.na(FCAL_ACTIVE),
                                "INDETERMINATE", FCAL_ACTIVE)) %>%
    mutate(FCAL_ACTIVE = ifelse(TEST_RESULT_NUMERIC > 200 & is.na(FCAL_ACTIVE), "ACTIVE", FCAL_ACTIVE)) %>%
    mutate(FCAL_SPECIMEN_COLLECTION_DATE = dmy(SPECIMEN_COLLECTION_DATE)) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, FCAL_SPECIMEN_COLLECTION_DATE, FCAL_ACTIVE) %>%
    mutate(FCAL_SOURCE = "CENTRAL LAB") %>%
    rbind(fcal_cleaned1)

  return(fcal_cleaned)
}
