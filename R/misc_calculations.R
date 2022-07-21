#' calculate_location_az
#'
#' Calculates disease locations from SPARC data with criteria specific to AZ
#'
#' @param observations observations table usually generated using load_data
#' @param encounter encounter table usually generated using load_data
#' @param window Time window (in days) in which observations labeled "unknown" can be carried over.
#'
#' @return A dataframe with all CD locations could be calculated regardless of IBD diagnosis.
#'
#'@details Contradictions and ambiguities in a location's involvement are resolved as follows:
#' If more than one record for a location exists for the same patient on the
#' same date, any record of a "yes" is taken as evidence for this location
#' showing disease activity. In absence of any "yes", any record of a "no" is
#' taken of evidence of absence of evidence. Otherwise, any occurrence of
#' "unknown" among the records is collapsed to a single "unknown". Any remaining
#' combination of values is considered "INVALID".
#'
#' The window variable can be used to leverage the relative stability of the
#' involvement of locations in Crohn's disease. Missing (i.e. "Unknown") values
#' will be replaced if other observations exist in the specified time window.
#' Observations in the future of the visit with missing records are given
#' precedence over values in the past.
#'
#' The columns colonic involvement and upper GI are constructed assuming that
#' explicit records of the absence of involvement of a location may have been
#' omitted. Any record of an "no" in relevant sub-locations are considered
#' sufficient to assume "unknown" in related locations imply "no" as long as a
#' "yes" is located anywhere else. For instance, if records indicate "no" for
#' left colonic involvement, and all other records for colonic sites are unknown.
#' This "no" will be considered sufficient to assume no colonic involvement if
#' involvement of the ileum or upper GI tract is recorded.
#' @export
calculate_location_az <- function(observations,encounter,window = 90)
{
  cd_phenotypes <- observations %>%
    filter(.data$DATA_SOURCE == "SF_SPARC") %>%
    left_join(encounter,by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "DATA_SOURCE", "VISIT_ENCOUNTER_ID")) %>%
    filter(.data$OBS_TEST_CONCEPT_NAME %in% c("Anal Phenotype", "Duodenal Phenotype", "Esophageal Phenotype", "Gastric Phenotype", "Ileal Phenotype", "Jejunal Phenotype", "Left Colonic Phenotype", "Rectal Phenotype", "Right Colonic Phenotype", "Transverse Colonic Phenotype")) %>%
    drop_na(.data$DESCRIPTIVE_SYMP_TEST_RESULTS) %>%
    mutate(Phenotype = case_when(
      DESCRIPTIVE_SYMP_TEST_RESULTS == "Yes" ~ "Yes",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "Anal inflammatory crohn's disease" ~ "Yes",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "Duodenal inflammatory crohn's disease" ~ "Yes",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "Esophageal inflammatory crohn's disease" ~ "Yes",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "Gastric inflammatory crohn's disease" ~ "Yes",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "Ileal inflammatory crohn's disease" ~ "Yes",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "Jejunal inflammatory crohn's disease" ~ "Yes",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "Left colonic inflammatory crohn's disease" ~ "Yes",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "Rectal inflammatory crohn's disease" ~ "Yes",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "Right colonic inflammatory crohn's disease" ~ "Yes",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "Transverse colonic inflammatory crohn's disease" ~ "Yes",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "No" ~ "No",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "No anal involvement" ~ "No",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "No rectal involvement" ~ "No",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "No right colonic involvement" ~ "No",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "No transverse colonic involvement" ~ "No",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "No duodenal involvement" ~ "No",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "No esophageal involvement" ~ "No",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "No gastric involvement" ~ "No",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "No ileal involvement" ~ "No",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "No jejunal involvement" ~ "No",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "No left colonic involvement" ~ "No",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "Unknown" ~ "Unknown",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "Unknown anal involvement" ~ "Unknown",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "Unknown rectal involvement" ~ "Unknown",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "Unknown right colonic involvement" ~ "Unknown",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "Unknown transverse colonic involvement" ~ "Unknown",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "Unknown duodenal involvement" ~ "Unknown",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "Unknown esophageal involvement" ~ "Unknown",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "Unknown gastric involvement" ~ "Unknown",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "Unknown ileal involvement" ~ "Unknown",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "Unknown jejunal involvement" ~ "Unknown",
      DESCRIPTIVE_SYMP_TEST_RESULTS == "Unknown left colonic involvement" ~ "Unknown",
      TRUE ~ DESCRIPTIVE_SYMP_TEST_RESULTS
    ))


  id_cols <- c("DEIDENTIFIED_MASTER_PATIENT_ID","VISIT_ENCOUNTER_ID","VISIT_ENCOUNTER_START_DATE")

  cd_phenotypes_wide <- cd_phenotypes %>%
    pivot_wider(all_of(id_cols),names_from = "OBS_TEST_CONCEPT_NAME",values_from = "DESCRIPTIVE_SYMP_TEST_RESULTS",
                values_fill = "Unknown",
                values_fn = function(x) { case_when("Yes" %in% x ~ "Yes",
                                                    "No" %in% x ~ "No",
                                                    "Unknown" %in% x ~ "Unknown",
                                                    TRUE ~ "INVALID")  }
    )

  pt_names <- setdiff(colnames(cd_phenotypes_wide),id_cols)

  cd_phenotypes_wide <- cd_phenotypes_wide %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(date = as.Date(VISIT_ENCOUNTER_START_DATE,format = "%d-%b-%Y")) %>%
    arrange(date) %>%
    mutate(lag_dif = date - lag(date)) %>%
    mutate(lead_dif = lead(date) - date) %>%
    mutate(across(all_of(pt_names),  ~ if_else(.x == "Unkown" & lead_dif <= window, true = lead(.x), false = .x, missing = .x ) ) ) %>% # carry backward to unknown
    mutate(across(all_of(pt_names),  ~ if_else(.x == "Unkown" & lag_dif <= window, true = lag(.x), false = .x, missing = .x ) ) ) %>% # carry forward to unknown
    select(-c("lag_dif","lead_dif","date"))

 cd_phenotypes_wide %>%
    mutate(any_yes = if_else(`Right Colonic Phenotype` == "Yes" |
                               `Left Colonic Phenotype` == "Yes" |
                               `Transverse Colonic Phenotype` == "Yes"  |
                               `Rectal Phenotype` == "Yes" |
                               `Anal Phenotype` == "Yes" |
                               `Esophageal Phenotype` == "Yes" |
                               `Duodenal Phenotype` == "Yes" |
                               `Jejunal Phenotype` == "Yes"  |
                               `Gastric Phenotype` == "Yes" |
                               `Ileal Phenotype` == "Yes","Yes","No" )) %>%
    mutate("Colonic involvement" = if_else(`Right Colonic Phenotype` == "Yes" |
                                             `Left Colonic Phenotype` == "Yes" |
                                             `Transverse Colonic Phenotype` == "Yes"  |
                                             `Rectal Phenotype` == "Yes" |
                                             `Anal Phenotype` == "Yes", "Yes", #az clinician wants it here, CCF does not do this - includes as perianal modifer
                                           if_else(`Right Colonic Phenotype` == "No" |
                                                     `Left Colonic Phenotype` == "No" |
                                                     `Transverse Colonic Phenotype` == "No"  |
                                                     `Rectal Phenotype` == "No" |
                                                     `Anal Phenotype` == "No" & any_yes == "Yes", "No","Unknown" )  )) %>%
    mutate("Upper GI" = if_else(`Esophageal Phenotype` == "Yes" |
                                  `Duodenal Phenotype` == "Yes" |
                                  `Jejunal Phenotype` == "Yes"  |
                                  `Gastric Phenotype` == "Yes","Yes",
                                if_else( `Esophageal Phenotype` == "No" |
                                           `Duodenal Phenotype` == "No" |
                                           `Jejunal Phenotype` == "No"  |
                                           `Gastric Phenotype` == "No" & any_yes == "Yes","No","Unknown")  )) %>%
    mutate("Ilealcolonic Phenotype"  = if_else( `Ileal Phenotype` == "Yes" & `Colonic involvement` == "Yes","Yes",
                                                if_else(`Colonic involvement` == "Unknown" |  `Ileal Phenotype` == "Unknown" ,"Unknown","No") ) ) %>%
    mutate("Colonic Phenotype" = if_else(`Colonic involvement` == "Yes" &  `Ileal Phenotype` == "No","Yes",
                                         if_else(`Colonic involvement` == "Unknown" |  `Ileal Phenotype` == "Unknown", "Unknown",  "No"  ) )  ) %>%
    mutate("Pure Ileal Phenotype" = if_else(`Colonic involvement` == "No" &  `Ileal Phenotype` == "Yes","Yes",
                                            if_else(`Colonic involvement` == "Unknown" |  `Ileal Phenotype` == "Unknown", "Unknown",  "No"  ) )  ) %>%
    ungroup()  %>%
    mutate("CD Location" = case_when(`Ilealcolonic Phenotype` == "Yes" & `Colonic Phenotype` == "No" & `Pure Ileal Phenotype` == "No" ~  "Ilealcolonic",
                                     `Ilealcolonic Phenotype` == "No" & `Colonic Phenotype` == "Yes" & `Pure Ileal Phenotype` == "No" ~  "Colonic",
                                     `Ilealcolonic Phenotype` == "No" & `Colonic Phenotype` == "No" & `Pure Ileal Phenotype` == "Yes" ~  "Ileal",
                                     `Ilealcolonic Phenotype` == "Unknown" & `Colonic Phenotype` == "Unknown" & `Pure Ileal Phenotype` == "Unknown" ~  "Unknown",
                                     `Ilealcolonic Phenotype` == "No" & `Colonic Phenotype` == "No" & `Pure Ileal Phenotype` == "No" & `Upper GI` == "Yes" ~  "Upper GI only",
                                     `Ilealcolonic Phenotype` == "No" & `Colonic Phenotype` == "No" & `Pure Ileal Phenotype` == "No" & `Upper GI` == "No"~ "No Location",
                                     `Upper GI` == "Unknown" ~ "Unknown",
                                     TRUE ~ "THIS IS AN INCONSISTENCY")) %>%
  select(-c("any_yes","Ilealcolonic Phenotype","Colonic Phenotype","Pure Ileal Phenotype") )
}

#' calculate_uc_phenotype
#'
#' Calculates ulcerative colitis phenotypes from SPARC data
#'
#' @param observations observations table usually generated using load_data
#' @param worsening a logical indicating whether the worst extend of ulcerative colitis up to a specific date should be returned.
#'
#' @return A dataframe with all ulcerative colitis phenotypes that could be
#' derived from the data regardless of IBD disgnosis.
#'
#'@details If two or more records of ulcerative colitis phenotypes are available
#' for a patient on a single day the record showing the larger extend of
#' ulcerative colitis is chosen, with Pancolitis > Extensive ulcerative colitis >
#' Left-sided ulcerative colitis > Ulcerative proctitis > Unknown.

calculate_uc_phenotype <- function(observations, worsening = FALSE)
{
  priority_map <- tibble(term = c(
    "Pancolitis (E4)",
    "Extensive ulcerative colitis (extends proximal to the splenic flexure) (E3)",
    "Left-sided ulcerative colitis (distal to the splenic flexure only) (E2)",
    "Ulcerative proctitis (rectum only) (E1)",
    "Unknown"
  ), priority = 1:5)

ucp <- observations %>%
    filter(.data$DATA_SOURCE == "SF_SPARC") %>%
    filter(.data$OBS_TEST_CONCEPT_NAME %in% "Extent of Macroscopic Ulcerative Colitis" & (!is.na(.data$DESCRIPTIVE_SYMP_TEST_RESULTS))) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_CONCEPT_NAME, DESCRIPTIVE_SYMP_TEST_RESULTS, OBS_TEST_CONCEPT_CODE, OBS_TEST_RESULT_DATE) %>%
  mutate(DESCRIPTIVE_SYMP_TEST_RESULTS = case_when(
    DESCRIPTIVE_SYMP_TEST_RESULTS == "Ulcerative proctitis (rectum only)" ~ "Ulcerative proctitis (rectum only) (E1)",
    DESCRIPTIVE_SYMP_TEST_RESULTS == "Left-sided ulcerative colitis (distal to the splenic flexure only)" ~ "Left-sided ulcerative colitis (distal to the splenic flexure only) (E2)",
    DESCRIPTIVE_SYMP_TEST_RESULTS == "Extensive ulcerative colitis (extends proximal to the splenic flexure)" ~ "Extensive ulcerative colitis (extends proximal to the splenic flexure) (E3)",
    DESCRIPTIVE_SYMP_TEST_RESULTS == "Pancolitis" ~ "Pancolitis (E4)",
    TRUE ~ DESCRIPTIVE_SYMP_TEST_RESULTS
  )) %>%
  left_join(priority_map, by = c("DESCRIPTIVE_SYMP_TEST_RESULTS" = "term") ) %>%
  replace_na(list(priority = 5)) %>% # map anything not defined in priority_map to "Unknown"
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID,OBS_TEST_RESULT_DATE) %>%
    slice_min(priority, with_ties = FALSE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID,OBS_TEST_RESULT_DATE,`Extent of Macroscopic Ulcerative Colitis` = DESCRIPTIVE_SYMP_TEST_RESULTS,priority)


  if (worsening)
  {
    ucp <- ucp %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      mutate(worst = cummin (priority)) %>%
      left_join(priority_map, by = c("worst" =  "priority")) %>%
      select(-"worst") %>%
      rename("Worst Extent of Macroscopic Ulcerative Colitis" = term)
  }


return(ucp %>% select(-priority))
}
