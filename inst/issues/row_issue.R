
# ISSUE:
# I’ve run into some issues with sparce_med_starts returning different results
# based on the order in which records are in the input. For instance, for the
# “end_emr” calculation (copied from the sparc_med_filter source, except for the
# last lines relying on the presence of a column “MED_END_DATE” ) the following
# produces two different outputs, either MED_START_DATE or MED_END_DATE. Similar
# issues arise for a number of other columns. Would you be available discuss
# tomorrow? By then I hope to have a fix for this case, but I would like to get
# your feedback, since you are much more familiar with the code. I’d be
# available starting 9:00 am eastern time.



# in this instance just want most recent end date - currently each med-start-date and med-end-date is looked at independently
# Does it matter that rows are different if end date is the same?
# Do we remove  all the other columns and only keep the ones we need for this specific
# instance? (e.g. DEIDENTIFIED_MASTER_PATIENT_ID, MED_END_DATE)
# Do we consider encounter type when picking from EMR? Office visit, patient message, etc
# How do we pick the "correct" row?


data <- load_data(datadir = "C:/Users/tfehlmann/Documents/r_input/",
          cohort = "SPARC",
          domains = c("prescriptions", "demographics", "observations", "encounter"),
          data_type = "BOTH")

prescriptions<- data$prescriptions #%>% mutate(DEIDENTIFIED_MASTER_PATIENT_ID = bit64::as.integer64(DEIDENTIFIED_MASTER_PATIENT_ID))
demographics<- data$demographics# %>% mutate(DEIDENTIFIED_MASTER_PATIENT_ID = bit64::as.integer64(DEIDENTIFIED_MASTER_PATIENT_ID))
observations<- data$observations# %>% mutate(DEIDENTIFIED_MASTER_PATIENT_ID = bit64::as.integer64(DEIDENTIFIED_MASTER_PATIENT_ID))
encounter<- data$encounter# %>% mutate(DEIDENTIFIED_MASTER_PATIENT_ID = bit64::as.integer64(DEIDENTIFIED_MASTER_PATIENT_ID))

med_groups = c("Aminosalicylates",
               "Biologic",
               "Corticosteroids",
               "Immunomodulators" )


medication <- sparc_med_filter(prescriptions, observations, demographics, encounter, med_groups )

medication_chk <- medication %>% left_join(encounter)

write.csv(medication_chk, "~/medication_with_encounter.csv", na= "")

med_A <- medication %>% filter(.$DEIDENTIFIED_MASTER_PATIENT_ID == 10312755) %>% filter(.$new_med_name == "Tacrolimus") %>%
  mutate(row = row_number())

end_emr <- med_A %>%
  left_join(encounter, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "DEIDENTIFIED_PATIENT_ID", "DATA_SOURCE", "VISIT_ENCOUNTER_ID", "ADMISSION_TYPE", "SOURCE_OF_ADMISSION")) %>%
  mutate(MED_END_DATE = if_else(is.na(MED_END_DATE) & DATA_SOURCE == "EMR", MED_DISCONT_START_DATE, MED_END_DATE)) %>%
  filter(DATA_SOURCE == "EMR") %>%
  mutate(
    MED_START_DATE = dmy(MED_START_DATE),
    MED_END_DATE = dmy(MED_END_DATE)
  ) %>%
  mutate(drop = case_when(
    (!is.na(MED_END_DATE) & !is.na(MED_START_DATE)) & MED_END_DATE < MED_START_DATE ~ 1,
    TRUE ~ 0
  )) %>%
  filter(drop == 0) %>%
  mutate(
    MED_START_DATE = if_else(year(MED_START_DATE) > 1980, MED_START_DATE, as.Date(NA, format = "%d-%m-%y")),
    MED_END_DATE = if_else(year(MED_END_DATE) > 1980, MED_END_DATE, as.Date(NA, format = "%d-%m-%y"))
  ) %>%
  pivot_longer(cols = c(MED_START_DATE, MED_END_DATE), names_to = "type", values_to = "date") %>%
  drop_na(date) %>%
  arrange(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, DATA_SOURCE, match(type, c("MED_END_DATE", "MED_START_DATE"))) %>%
  group_by(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, DATA_SOURCE) %>%
  slice(which.max(date)) %>%
  pivot_wider(names_from = type, values_from = date) %>%
   ungroup() #%>% colnames()

end_emr[94]

end_emr[68]


med_B <- med_A %>% arrange(desc(row_number()))

end_emr_b <- med_B %>%
  left_join(encounter, by = c("DEIDENTIFIED_MASTER_PATIENT_ID", "DEIDENTIFIED_PATIENT_ID", "DATA_SOURCE", "VISIT_ENCOUNTER_ID", "ADMISSION_TYPE", "SOURCE_OF_ADMISSION")) %>%
  mutate(MED_END_DATE = if_else(is.na(MED_END_DATE) & DATA_SOURCE == "EMR", MED_DISCONT_START_DATE, MED_END_DATE)) %>%
  filter(DATA_SOURCE == "EMR") %>%
  mutate(
    MED_START_DATE = dmy(MED_START_DATE),
    MED_END_DATE = dmy(MED_END_DATE)
  ) %>%
  mutate(drop = case_when(
    (!is.na(MED_END_DATE) & !is.na(MED_START_DATE)) & MED_END_DATE < MED_START_DATE ~ 1,
    TRUE ~ 0
  )) %>%
  filter(drop == 0) %>%
  mutate(
    MED_START_DATE = if_else(year(MED_START_DATE) > 1980, MED_START_DATE, as.Date(NA, format = "%d-%m-%y")),
    MED_END_DATE = if_else(year(MED_END_DATE) > 1980, MED_END_DATE, as.Date(NA, format = "%d-%m-%y"))
  ) %>%
  pivot_longer(cols = c(MED_START_DATE, MED_END_DATE), names_to = "type", values_to = "date") %>%
  drop_na(date) %>%
  arrange(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, DATA_SOURCE, match(type, c("MED_END_DATE", "MED_START_DATE"))) %>%
  group_by(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, DATA_SOURCE) %>%
  slice(which.max(date)) %>%
  pivot_wider(names_from = type, values_from = date) %>%
  ungroup() # %>% colnames()

end_emr_b[94]

end_emr_b[68]

test <- tibble(
       id = bit64::as.integer64(1),
       year = c(1, 3, 2)
   ) %>%
       arrange(id, year)

