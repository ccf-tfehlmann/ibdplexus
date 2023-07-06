#' calculate_location_az
#'
#' Calculates disease locations from SPARC data with criteria specific to AZ
#'
#' @param observations observations table usually generated using load_data
#' @param encounter encounter table usually generated using load_data
#' @param window Time window (in days) in which observations labeled "unknown" can be carried over.
#'
#' @return A dataframe with all CD locations that could be calculated regardless of IBD diagnosis.
#'
#' @details Contradictions and ambiguities in a location's involvement are resolved as follows:
#' If more than one record for a location exists for the same patient on the
#' same date, any record of a "yes" is taken as evidence for this location
#' showing disease activity. In absence of any "yes", any record of a "no" is
#' taken of evidence of absence of evidence. Otherwise, any occurrence of
#' "unknown" among the records is collapsed to a single "unknown". Any remaining
#' combination of values is considered "INVALID".
#'
#' The window variable can be used to leverage the relative stability of the
#' involvement of locations in Crohn's disease. Unknown values
#' will be replaced if other observations exist in the specified time window.
#' Only Observations in the future of the visit with missing records will be
#' considered.
#'
#' Anal involvement is considered as contributing to a colonic phenotype and not as a perianal modifier.
#'
#' In addition of the recorded CD locations in SF observations, we can use records of strictures to
#' add certainty to "unknown" disease activities or even correct records of absence of disease activity.
#' Any record of strictures within seven days of the date of the record of the CD location will be considered.
#' \itemize{
#'  \item{If there is a record of "Phenotype - Ileal Stricture" = "Yes"
#'  it's considered as evidence of disease activity in the Ileum.}
#'  \item{Any records of "Phenotype - Left Colonic Stricture",
#'  "Phenotype - Rectal Stricture", "Phenotype - Right Colonic Stricture" or
#'  "Phenotype - Transverse Colonic Stricture" equaling "Yes" are
#'  considered evidence of disease activity in the colon.}
#'  \item{ Any record of Phenotype - Esophageal Stricture",
#'  "Phenotype - Jejunal Stricture", "Phenotype - Duodenal Stricture" or
#'  "Phenotype - Gastric Stricture " equal to "Yes" will be considered evidence
#'  of disease activity in the upper GI tract.}
#'  }
#' @export
calculate_location_az <- function(observations, encounter, window = 90) {
  # fetch CD phenotype data to use information about strictures to address data
  # quality issues.

  cdpt <- calculate_cd_phenotype(observations, return_intermediate_results = TRUE)

  cd_locations <- observations %>%
    filter(.data$DATA_SOURCE == "SF_SPARC") %>%
    left_join(encounter, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "DATA_SOURCE", "VISIT_ENCOUNTER_ID")) %>%
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


  id_cols <- c("DEIDENTIFIED_MASTER_PATIENT_ID", "VISIT_ENCOUNTER_ID", "VISIT_ENCOUNTER_START_DATE")

  cd_locations_wide <- cd_locations %>%
    pivot_wider(all_of(id_cols),
      names_from = "OBS_TEST_CONCEPT_NAME", values_from = "DESCRIPTIVE_SYMP_TEST_RESULTS",
      values_fill = "Unknown",
      values_fn = function(x) {
        case_when(
          "Yes" %in% x ~ "Yes",
          "No" %in% x ~ "No",
          "Unknown" %in% x ~ "Unknown",
          TRUE ~ "INVALID"
        )
      }
    )

  pt_names <- setdiff(colnames(cd_locations_wide), id_cols)

  cd_locations_wide <- cd_locations_wide %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(date = as.Date(VISIT_ENCOUNTER_START_DATE, format = "%d-%b-%Y")) %>%
    arrange(date) %>%
    mutate(lag_dif = date - lag(date)) %>%
    mutate(lead_dif = lead(date) - date) %>%
    mutate(across(all_of(pt_names), ~ if_else(.x == "Unkown" & lead_dif <= window, true = lead(.x), false = .x, missing = .x))) %>% # carry backward to unknown
    select(-c("lag_dif", "lead_dif", "date"))

  cdloc <- cd_locations_wide %>%
    mutate("Colonic involvement" = if_else(`Right Colonic Phenotype` == "Yes" |
      `Left Colonic Phenotype` == "Yes" |
      `Transverse Colonic Phenotype` == "Yes" |
      `Rectal Phenotype` == "Yes" |
      `Anal Phenotype` == "Yes", "Yes", # az clinician wants it here, CCF does not do this - includes as perianal modifer
    if_else(`Right Colonic Phenotype` == "No" &
      `Left Colonic Phenotype` == "No" &
      `Transverse Colonic Phenotype` == "No" &
      `Rectal Phenotype` == "No" &
      `Anal Phenotype` == "No", "No", "Unknown")
    )) %>%
    mutate("Upper GI" = if_else(`Esophageal Phenotype` == "Yes" |
      `Duodenal Phenotype` == "Yes" |
      `Jejunal Phenotype` == "Yes" |
      `Gastric Phenotype` == "Yes", "Yes",
    if_else(`Esophageal Phenotype` == "No" &
      `Duodenal Phenotype` == "No" &
      `Jejunal Phenotype` == "No" &
      `Gastric Phenotype` == "No", "No", "Unknown")
    )) %>%
    mutate("Ilealcolonic Phenotype" = if_else(`Ileal Phenotype` == "Yes" & `Colonic involvement` == "Yes", "Yes",
      if_else(`Colonic involvement` == "Unknown" | `Ileal Phenotype` == "Unknown", "Unknown", "No")
    )) %>%
    mutate("Colonic Phenotype" = if_else(`Colonic involvement` == "Yes" & `Ileal Phenotype` == "No", "Yes",
      if_else(`Colonic involvement` == "Unknown" | `Ileal Phenotype` == "Unknown", "Unknown", "No")
    )) %>%
    mutate("Pure Ileal Phenotype" = if_else(`Colonic involvement` == "No" & `Ileal Phenotype` == "Yes", "Yes",
      if_else(`Colonic involvement` == "Unknown" | `Ileal Phenotype` == "Unknown", "Unknown", "No")
    )) %>%
    ungroup() %>%
    mutate(VISIT_ENCOUNTER_START_DATE = dmy(VISIT_ENCOUNTER_START_DATE)) %>%
    left_join(cdpt, by = c("DEIDENTIFIED_MASTER_PATIENT_ID")) %>%
    mutate(time_diff = `VISIT_ENCOUNTER_START_DATE` - `OBS_TEST_RESULT_DATE`) %>%
    filter(abs(time_diff) < 7) %>%
    mutate(`Colonic involvement` = if_else(`Phenotype - Transverse Colonic Stricture` == "Yes" |
      `Phenotype - Right Colonic Stricture` == "Yes" |
      `Phenotype - Rectal Stricture` == "Yes" |
      `Phenotype - Left Colonic Stricture` == "Yes", "Yes", `Colonic involvement`)) %>%
    mutate(`Ileal Phenotype` = if_else(`Phenotype - Ileal Stricture` == "Yes", "Yes", `Ileal Phenotype`)) %>%
    mutate(`Upper GI` = if_else(`Phenotype - Esophageal Stricture` == "Yes" |
      `Phenotype - Jejunal Stricture` == "Yes" |
      `Phenotype - Duodenal Stricture` == "Yes" |
      `Phenotype - Gastric Stricture` == "Yes", "Yes", `Upper GI`)) %>%
    mutate("CD Location" = case_when(
      `Ilealcolonic Phenotype` == "Yes" & `Colonic Phenotype` == "No" & `Pure Ileal Phenotype` == "No" ~ "Ilealcolonic",
      `Ilealcolonic Phenotype` == "No" & `Colonic Phenotype` == "Yes" & `Pure Ileal Phenotype` == "No" ~ "Colonic",
      `Ilealcolonic Phenotype` == "No" & `Colonic Phenotype` == "No" & `Pure Ileal Phenotype` == "Yes" ~ "Ileal",
      `Ilealcolonic Phenotype` == "Unknown" & `Colonic Phenotype` == "Unknown" & `Pure Ileal Phenotype` == "Unknown" ~ "Unknown",
      `Ilealcolonic Phenotype` == "No" & `Colonic Phenotype` == "No" & `Pure Ileal Phenotype` == "No" & `Upper GI` == "Yes" ~ "Upper GI only",
      `Ilealcolonic Phenotype` == "No" & `Colonic Phenotype` == "No" & `Pure Ileal Phenotype` == "No" & `Upper GI` == "No" ~ "No Location",
      `Upper GI` == "Unknown" ~ "Unknown",
      TRUE ~ "THIS IS AN INCONSISTENCY"
    )) %>%
    select(-c("Ilealcolonic Phenotype", "Colonic Phenotype", "Pure Ileal Phenotype"))

  # combining CD location data with stricture records may result in multiple CD
  # locations in the same patient on the same day. Select the worst phenotype as a consensus.
  # Under this rule, a colonic and ileal record would be combined to ilealcolonic

  location_summarizer <- function(x) {
    case_when(
      any(x == "Ilealcolonic") ~ "Ilealcolonic",
      any(x == "Colonic") & any(x == "Ileal") ~ "Ilealcolonic",
      any(x == "Colonic") ~ "Colonic",
      any(x == "Ileal") ~ "Ileal",
      any(x == "Upper GI only") ~ "Upper GI only",
      any(x == "Unknown") ~ "Unknown",
      any(x == "No Location") ~ "No Location",
      TRUE ~ "Inconsistent CD location summary."
    )
  }

  upper_gi_summarizer <- function(x) {
    case_when(
      any(x == "Yes") ~ "Yes",
      any(x == "Unknown") ~ "Unknown",
      all(is.na(x)) ~ "Unknown",
      TRUE ~ "No"
    )
  }


  cdloc %>%
    select(c("DEIDENTIFIED_MASTER_PATIENT_ID", "VISIT_ENCOUNTER_START_DATE", "CD Location", "Upper GI")) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_START_DATE) %>%
    summarise(
      "CD Location" = location_summarizer(`CD Location`),
      "Upper GI" = upper_gi_summarizer(`Upper GI`)
    )
}



disambiguation <- function(x) {
  case_when(
    any(x == "Yes") ~ "Yes", # take the worst outcome if there is any record of the phenotype assume it is present
    all(x == "No") ~ "No",
    all(x == "Stricturing") ~ "Stricturing",
    all(x == "Penetrating") ~ "Penetrating",
    all(x == "Inflammatory, non-penetrating, non-stricturing") ~ "Inflammatory, non-penetrating, non-stricturing",
    all(x %in% c("Unknown", "Penetrating") & any(x == "Penetrating")) ~ "Penetrating",
    all(x %in% c("Inflammatory, non-penetrating, non-stricturing", "Penetrating") & any(x == "Penetrating")) ~ "Penetrating",
    all(x %in% c("Unknown", "Stricturing") & any(x == "Stricturing")) ~ "Stricturing",
    all(x %in% c("Inflammatory, non-penetrating, non-stricturing", "Stricturing") & any(x == "Stricturing")) ~ "Stricturing",
    all(x %in% c("Unknown", "Inflammatory, non-penetrating, non-stricturing") & any(x == "Inflammatory, non-penetrating, non-stricturing")) ~ "Inflammatory, non-penetrating, non-stricturing",
    ("Penetrating" %in% x) & ("Stricturing" %in% x) ~ "Both stricturing and penetrating",
    any(x == "Both stricturing and penetrating") ~ "Both stricturing and penetrating",
    all(x %in% c("Unknown", "No")) ~ "No", # Assume that an explicit "No" in the presence of "Unknown" provides accurate information.
    TRUE ~ paste(x, collapse = ";")
  ) # return any unhandled cases unmodified.
}

# TODO add function that ignore stricture and fistula terms.

#' calculate_cd_phenotype
#'
#' Calculates Crohn's disease phenotypes from SPACR data
#'
#' @param observations observations table usually generated using load_data
#' @param disambiguation_fun a function used to disambiguate occurrences of multiple
#' records for the same patient on the same date. The function is expected to
#' take a character vector of any length and return a vector of length one.
#' It will be applied to vectors of DESCRIPTIVE_SYMP_TEST_RESULTS, all elements
#' of the vector will be derived from a single OBS_TEST_CONCEPT_NAME.
#'
#' @param return_intermediate_results if TRUE columns that were used to calculate
#' the final CD locations will be returned.
#'
#' @return A dataframe with all Crohn's disease phenotypes that could be
#' derived from the data regardless of IBD diagnosis.
#'
#' @details This function not only considers the recorded values for the Crohn's disease phenotype, but also explicit records that indicate the presence of a fistula or stricture.
#'
#' @export

calculate_cd_phenotype <- function(observations, disambiguation_fun = disambiguation, return_intermediate_results = FALSE) {
  stricture_terms <- c(
    "Phenotype - Anal Stricture",
    "Phenotype - Duodenal Stricture",
    "Phenotype - Esophageal Stricture",
    "Phenotype - Gastric Stricture",
    "Phenotype - Ileal Stricture",
    "Phenotype - Jejunal Stricture",
    "Phenotype - Left Colonic Stricture",
    "Phenotype - Rectal Stricture",
    "Phenotype - Right Colonic Stricture",
    "Phenotype - Transverse Colonic Stricture",
    "Anal Canal Stricture"
  )

  fistula_terms <- c(
    "Phenotype - Enterocutaneous Fistula",
    "Phenotype - Enteroenteric Fistula",
    "Phenotype - Enterovesical Fistula",
    "Phenotype - Other Fistula",
    "Fistula - Outside of Perianal Region",
    "IBD Manifestations - Abdominal Abscess, Fistula, or Other Penetrating Complication"
  )

  perianal_terms <- c(
    "Rectovaginal Fistula",
    "Perianal Fistula",
    "Perianal Fistula - Complex Fistula",
    "Anal Fistula"
  )

  cdpt <- observations %>%
    filter(.data$DATA_SOURCE == "SF_SPARC") %>%
    filter(.data$OBS_TEST_CONCEPT_NAME %in% c("Crohn's Disease Phenotype", stricture_terms, fistula_terms, perianal_terms)) %>%
    mutate(DESCRIPTIVE_SYMP_TEST_RESULTS = if_else(.data$DESCRIPTIVE_SYMP_TEST_RESULTS == "Possible", "Unknown", DESCRIPTIVE_SYMP_TEST_RESULTS)) %>% # treat "possible" as unknown
    drop_na(DESCRIPTIVE_SYMP_TEST_RESULTS) %>%
    mutate(`DESCRIPTIVE_SYMP_TEST_RESULTS` = case_when(
      OBS_TEST_CONCEPT_NAME == "Anal Fistula" & `DESCRIPTIVE_SYMP_TEST_RESULTS` == "Active drainage" ~ "Yes",
      OBS_TEST_CONCEPT_NAME == "Anal Fistula" & `DESCRIPTIVE_SYMP_TEST_RESULTS` == "No anal fistula" ~ "No",
      OBS_TEST_CONCEPT_NAME == "Anal Fistula" & `DESCRIPTIVE_SYMP_TEST_RESULTS` == "No or scant drainage" ~ "Yes",
      TRUE ~ `DESCRIPTIVE_SYMP_TEST_RESULTS`
    )) %>% # map terms used under concept anal fistula to yes/no
    pivot_wider(
      id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE),
      names_from = OBS_TEST_CONCEPT_NAME,
      values_from = DESCRIPTIVE_SYMP_TEST_RESULTS,
      values_fn = disambiguation
    ) %>%
    rowwise() %>%
    mutate(any_stricture = if_else(any(c_across(all_of(stricture_terms)) %in% "Yes"), "Yes", "No")) %>%
    mutate(any_fistula = if_else(any(c_across(all_of(c(fistula_terms, perianal_terms))) %in% "Yes"), "Yes", "No")) %>%
    ungroup() %>%
    mutate(any_stricture_incl_phenotype = if_else((.data$`Crohn's Disease Phenotype` %in% c("Both stricturing and penetrating", "Stricturing")) | any_stricture == "Yes", "Yes", "No")) %>%
    mutate(any_fistula_incl_phenotype = if_else((.data$`Crohn's Disease Phenotype` %in% c("Both stricturing and penetrating", "Penetrating")) | any_fistula == "Yes", "Yes", "No")) %>%
    mutate(final_cd_phenotype = case_when(
      (.data$any_stricture_incl_phenotype == "Yes") & (.data$any_fistula_incl_phenotype == "No") ~ "Stricturing",
      (.data$any_fistula_incl_phenotype == "Yes") & (.data$any_stricture_incl_phenotype == "No") ~ "Penetrating",
      (.data$any_stricture_incl_phenotype == "Yes") & (.data$any_fistula_incl_phenotype == "Yes") ~ "Both stricturing and penetrating",
      (.data$any_stricture_incl_phenotype == "No") & (.data$any_fistula_incl_phenotype == "No") ~ "Inflammatory, non-penetrating, non-stricturing",
      TRUE ~ NA_character_
    ))

  if (!return_intermediate_results) {
    cdpt <- cdpt %>%
      select("DEIDENTIFIED_MASTER_PATIENT_ID", "OBS_TEST_RESULT_DATE", "CD Phenotype" = "final_cd_phenotype")
  }
  return(cdpt)
}

#' calculate_uc_phenotype
#'
#' Calculates ulcerative colitis phenotypes from SPARC data
#'
#' @param observations observations table usually generated using load_data
#' @param worsening a logical indicating whether the worst extend of ulcerative colitis up to a specific date should be returned.
#'
#' @return A dataframe with all ulcerative colitis phenotypes that could be
#' derived from the data regardless of IBD diagnosis.
#'
#' @details If two or more records of ulcerative colitis phenotypes are available
#' for a patient on a single day the record showing the larger extend of
#' ulcerative colitis is chosen, with Pancolitis > Extensive ulcerative colitis >
#' Left-sided ulcerative colitis > Ulcerative proctitis > Unknown.
#' @export

calculate_uc_phenotype <- function(observations, worsening = FALSE) {
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
    left_join(priority_map, by = c("DESCRIPTIVE_SYMP_TEST_RESULTS" = "term")) %>%
    replace_na(list(priority = 5)) %>% # map anything not defined in priority_map to "Unknown"
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE) %>%
    slice_min(priority, with_ties = FALSE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, OBS_TEST_RESULT_DATE, `Extent of Macroscopic Ulcerative Colitis` = DESCRIPTIVE_SYMP_TEST_RESULTS, priority)


  if (worsening) {
    ucp <- ucp %>%
      group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
      mutate(worst = cummin(priority)) %>%
      left_join(priority_map, by = c("worst" = "priority")) %>%
      select(-"worst") %>%
      rename("Worst Extent of Macroscopic Ulcerative Colitis" = term)
  }


  return(ucp %>% select(-priority) %>% ungroup())
}
