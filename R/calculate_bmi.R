#' calculate_bmi
#'
#' Calculate BMI from electronic medical record data for SPARC & QORUS.
#'
#' For patients with more than 1 bmi, outliers are removed.
#'
#'
#' @param observations A dataframe with demographics data.
#'
#' @return A long dataframe with the deidentified_master_patient_id, date of bmi measurement and bmi.
#' @export
calculate_bmi <- function(observations) {


weight <- observations %>%
  filter(DATA_SOURCE == "EMR") %>%
  filter(OBS_TEST_CONCEPT_NAME %in% c("Weight/Scale", "Weight")) %>%
  drop_na(TEST_RESULT_NUMERIC) %>%
  mutate(weight_kg = as.numeric(TEST_RESULT_NUMERIC) / 35.274) %>%
  mutate(date = dmy(OBS_TEST_RESULT_DATE)) %>%
  distinct(DEIDENTIFIED_MASTER_PATIENT_ID, weight_kg, date)


height <- data$observations %>%
  filter(DATA_SOURCE == "EMR") %>%
  filter(OBS_TEST_CONCEPT_NAME %in% c("Height")) %>%
  drop_na(TEST_RESULT_NUMERIC) %>%
  mutate(height_m = as.numeric(TEST_RESULT_NUMERIC) / 39.37) %>%
  mutate(date = dmy(OBS_TEST_RESULT_DATE)) %>%
  distinct(DEIDENTIFIED_MASTER_PATIENT_ID, height_m, date)


bmi <- full_join(weight, height) %>%
  mutate(bmi = weight_kg / (height_m^2)) %>%
  drop_na(bmi) %>%
  group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
  mutate(new = remove_outliers(bmi)) %>%
  drop_na(new) %>%
  ungroup() %>%
  select(-new) %>%
  distinct(DEIDENTIFIED_MASTER_PATIENT_ID, bmi, date) %>%
  rename(bmi_date = date) %>%
  ungroup()

}