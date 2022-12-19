#' dose_escalation
#'
#' For SPARC, this function finds if a medication has increased in dose over time.
#'
#'
#' @param medication A dataframe generated from sparc_med_filter()
#'
#' @return A dataframe with the master patient id, data source, medication name, original dose and any doses reported over time for that source.
#'
dose_escalation <- function(medication){

   medication %>%
    filter(!grepl('both eyes|Each Eye|external|Eyes (Each)|feeding tube|gastric tube|left eye|Misc.(Non-Drug; Combo Route)|MISCELLANEOUS|Mouth/Throat|nasogastric tube|ophthalmic|Per NG / OG tube|Per NG Tube|PO/NG/OG|Rectal|rectal|Topical|TOPICAL (LOTION OR CREAM)|Vaginal', ROUTE_OF_MEDICATION, ignore.case = T) | is.na(ROUTE_OF_MEDICATION)) %>%
    mutate(MED_START_DATE = dmy(MED_START_DATE), MED_END_DATE = dmy(MED_END_DATE)) %>%
    mutate(
      MED_START_DATE = if_else(year(MED_START_DATE) >
                                 1980, MED_START_DATE, as.Date(NA, format = "%d-%m-%y")),
      MED_END_DATE = if_else(year(MED_END_DATE) > 1980,
                             MED_END_DATE, as.Date(NA, format = "%d-%m-%y")
      )
    ) %>%
    mutate(DOSE_OF_MEDICATION = ifelse(is.na(DOSE_OF_MEDICATION), MEDICATION_STRENGTH, DOSE_OF_MEDICATION)) %>%
    mutate(DOSE_OF_MEDICATION = readr::parse_number(DOSE_OF_MEDICATION)) %>%
    drop_na(DOSE_OF_MEDICATION) %>%
    filter(DOSE_OF_MEDICATION > 0) %>%

    group_by(
      DEIDENTIFIED_MASTER_PATIENT_ID,
      new_med_name, DATA_SOURCE
    ) %>%
    arrange(
      DEIDENTIFIED_MASTER_PATIENT_ID,
      new_med_name, MED_START_DATE, .by_group = T
    ) %>%
    distinct(
      DEIDENTIFIED_MASTER_PATIENT_ID, DATA_SOURCE,
      new_med_name, DOSE_OF_MEDICATION
    ) %>%
    mutate(c = seq_along(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
    pivot_wider(
      id_cols = c(
        DEIDENTIFIED_MASTER_PATIENT_ID,
        DATA_SOURCE, new_med_name
      ), names_from = c, values_from = c(DOSE_OF_MEDICATION),
      names_prefix = "DOSE_"
    ) %>%
    rowwise() %>%
    mutate(nonmiss = sum(!is.na(c_across(starts_with("DOSE")))))  %>%
    filter(nonmiss >= 2) %>%
    rename(orig_dose = DOSE_1) %>%
    mutate(across(starts_with("DOSE"),
                  ~ case_when(.x > orig_dose ~ cur_column()), .names = "{.col}_new")) %>%
    unite(new, ends_with("_new"), na.rm = TRUE, sep = ", ") %>%
    na_if("") %>%
    mutate(DOSE_ESCALATION = case_when(is.na(new) ~ 0, TRUE ~ 1)) %>%
    rename(MEDICATION = new_med_name)

}
