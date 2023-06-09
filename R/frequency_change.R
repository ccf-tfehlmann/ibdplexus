#' frequency_change
#'
#' For SPARC, this function finds if a long term advanced therapy has changed in frequency over time.
#'
#'
#' @param medication A dataframe generated from sparc_med_filter()
#'
#' @return A dataframe with the master patient id, data source, medication name, original prescribed interval and changing frequency
#'


frequency_change <- function(medication){
  medication %>%
    filter(!grepl('both eyes|Each Eye|external|Eyes (Each)|feeding tube|gastric tube|left eye|Misc.(Non-Drug; Combo Route)|MISCELLANEOUS|Mouth/Throat|nasogastric tube|ophthalmic|Per NG / OG tube|Per NG Tube|PO/NG/OG|Rectal|rectal|Topical|TOPICAL (LOTION OR CREAM)|Vaginal', ROUTE_OF_MEDICATION, ignore.case = T) | is.na(ROUTE_OF_MEDICATION)) %>%
    mutate(
      MED_START_DATE = if_else(year(MED_START_DATE) >
                                 1980, MED_START_DATE, as.Date(NA, format = "%d-%m-%y")),
      MED_END_DATE = if_else(year(MED_END_DATE) > 1980,
                             MED_END_DATE, as.Date(NA, format = "%d-%m-%y")
      )
    ) %>%
    select(
      DEIDENTIFIED_MASTER_PATIENT_ID,  new_med_name,DATA_SOURCE, ROUTE_OF_MEDICATION, MEDICATION_DOMAIN, MED_START_DATE,
      MED_END_DATE, DOSE_OF_MEDICATION, CURRENT_MEDICATION, OTHER_MEDICATION, UNIT_OF_MEASURE_FOR_MEDICATION, MEDICATION_FREQUENCE,
      MEDICATION_ADMINISTRATED_CODE, MEDICATION_ADMINISTRATED, FREQUENCY_IN_DAYS, REASON_STOPPED, SUMMARY,
      MED_DISCONT_START_DATE, MEDICATION_STRENGTH, MED_STRENGTH_UNIT_OF_MEASURE, MEDICATION_QUANTITY, MED_QUANTITY_UOM, MED_FORM,
      MEDICATION_TREATMENT_COURSE, FREQUENCE_UNIT_OF_MEASURE, MEDICATION_ADMIN_DURATION, MED_ADMIN_DURATION_UOM,
      GENERIC_MEDICINE_FLAG, SUBSTITUTE_MED_INDICATION_FLAG, PLACE_OF_SERVICE, MEDICATION_REFILLS
    ) %>%
    distinct() %>%
    filter(new_med_name %in% c("Adalimumab", "Certolizumab Pegol", "Golimumab", "Infliximab", "Natalizumab", "Ozanimod", "Risankizumab", "Tofacitinib", "Upadacitinib", "Ustekinumab", "Vedolizumab")) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name,DATA_SOURCE) %>%
    mutate(weeks_between_med = difftime(MED_START_DATE, lag(MED_START_DATE), units = "weeks")) %>%
    mutate(freq1 = str_extract(SUMMARY, "Frequency.*"),
           freq2 = paste(MEDICATION_FREQUENCE, FREQUENCE_UNIT_OF_MEASURE, sep =" ")) %>%
    mutate(freq = ifelse(is.na(freq1), freq2, freq1)) %>%
    ungroup() %>%
    filter(grepl("week", freq, ignore.case = T)) %>%
    mutate(freq = case_when(freq == "every 14 days Week" ~ "every 2 weeks",
                            freq == "every 28 days Week" ~ "every 4 weeks",
                            freq == "Frequency: Once a week every" ~ "1 week",
                            TRUE ~ freq)) %>%
    mutate(freq_weeks = parse_number(freq)) %>%
    drop_na(freq_weeks) %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, DATA_SOURCE, new_med_name,MED_START_DATE) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DATA_SOURCE, new_med_name, freq_weeks) %>%
    mutate(helper = paste0(DEIDENTIFIED_MASTER_PATIENT_ID, DATA_SOURCE)) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name,DATA_SOURCE) %>%
    mutate(count = seq_along(helper)) %>%
    mutate(freq_weeks = as.numeric(freq_weeks)) %>%
    pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name,DATA_SOURCE), names_from = c(count), values_from = c(freq_weeks), names_prefix = "weeks_between_med_") %>%
    dplyr::bind_rows(dplyr::tibble(weeks_between_med_3=numeric())) %>%

    mutate(loading_dose = case_when(new_med_name == "Infliximab" & weeks_between_med_2 >= 2 & weeks_between_med_2 < 3 & weeks_between_med_3 >= 3.5 & weeks_between_med_3 < 5.5 ~ 1,
                                    new_med_name == "Certolizumab Pegol" & weeks_between_med_2 >= 2 & weeks_between_med_2 < 3 &
                                      weeks_between_med_3 >= 1.5 & weeks_between_med_3 < 3.5 ~ 1,
                                    new_med_name == "Vedolizumab" & weeks_between_med_2 >= 2 & weeks_between_med_2 < 3 &
                                      weeks_between_med_3 >= 3.5 & weeks_between_med_3 < 5.5 ~ 1,
                                    TRUE ~ 0)) %>%
    pivot_longer(cols = starts_with("weeks_between_med"), names_to = "number", values_to = "weeks_between_med") %>%
    drop_na(weeks_between_med) %>%
    filter(loading_dose == 0) %>%
    mutate(helper = paste0(DEIDENTIFIED_MASTER_PATIENT_ID, DATA_SOURCE)) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name,DATA_SOURCE) %>%
    mutate(count = seq_along(helper)) %>%
    pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name,DATA_SOURCE), names_from = count, values_from = c(weeks_between_med), names_prefix = "weeks_between_med_") %>%
    mutate(nonmiss = sum(!is.na(c_across(starts_with("weeks")))))  %>%
    filter(nonmiss >= 2) %>%
    rename(orig_freq = weeks_between_med_1) %>%
    mutate(across(starts_with("weeks"),
                  ~ case_when(orig_freq - .x >= 2 ~ cur_column()), .names = "{.col}_new")) %>%
    unite(new, ends_with("_new"), na.rm = TRUE, sep = ", ") %>%
    mutate(new = ifelse(new %in% c("", " "), as.character(NA), new)) %>%
    mutate(DECREASE_IN_FREQUENCY = case_when(is.na(new) ~ 0, TRUE ~ 1)) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name) %>%
    slice(which.max(DECREASE_IN_FREQUENCY)) %>%
    ungroup() %>%
    distinct() %>%
    rename(MEDICATION = new_med_name)

}
