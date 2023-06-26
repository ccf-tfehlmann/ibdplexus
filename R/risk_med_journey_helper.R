#' overlapping_meds
#'
#' create table with overlapping meds column and overlapping med days columns for RISK patients
#'
#'
#' @param table A dataframe created by the first part of the risk medication journey function.
#'
#' @return A dataframe of RISK patients with additional columns of the overlapping medications and the days the medications overlapped
#'
#'

overlapping_meds <- function(table){

  all_meds <- table %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME,
           MED_START_DATE, MED_END_DATE) %>%
    # rbind(antibiotic_rounds %>%
    #         select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME,
    #                MED_START_DATE, MED_END_DATE)) %>%
    # rbind(steroid_rounds %>%
    #         select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME,
    #                MED_START_DATE, MED_END_DATE)) %>%
    mutate(INTERVAL = interval(MED_START_DATE, MED_END_DATE)) %>%
    # create med number for all meds
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    # add row number for number therapy the patient has been on
    mutate(MED_ORDER = row_number())

  ## 8 overall meds
  # find one lead and one lag, then create groups like I did for
  # the intervals in the prof billing inpatient intervals ?

  overlaps <- all_meds %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID)

  # make numbering variable go down backward to fill NA when there is no lead
  # interval to compare with
  overlaps_lead <- all_meds %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    arrange(desc(MED_ORDER), .by_group = T) %>%
    mutate(num_end = row_number()) %>%
    arrange(MED_ORDER, .by_group = T)

  max_rounds <- overlaps_lead %>%
    ungroup() %>%
    select(MED_ORDER) %>%
    slice(which.max(MED_ORDER))

  max_rounds <- max_rounds$MED_ORDER

  # for loop comparing all intervals
  for(i in 1:max_rounds) {
    # Head of for-loop
    new <- ifelse(int_overlaps(overlaps_lead$INTERVAL, lead(overlaps_lead$INTERVAL, i)), 1, 0) # Create new column
    overlaps_lead[ , ncol(overlaps_lead) + 1] <- new                  # Append new column
    colnames(overlaps_lead)[ncol(overlaps_lead)] <- paste0("x_", i)  # Rename column name
  }

  name <- names(overlaps_lead)
  name_1 <- name[8:as.numeric(max_rounds+7)]

  # make the values NA when they are checking for a lead for a different patient ID
  for (i in name_1){

    overlaps_lead[[i]] <- ifelse(overlaps_lead$num_end <= parse_number(i), NA, overlaps_lead[[i]])

  }

  # replace 1's with the actual med name
  for (i in name_1){
    overlaps_lead[[i]] <- ifelse(overlaps_lead[[i]] == 1, paste0(lead(overlaps_lead$MEDICATION_NAME,
                                                                      parse_number(i))), overlaps_lead[[i]])
  }

  lead_overlaps <- overlaps_lead %>%
    mutate(across(starts_with("x_"), as.character)) %>%
    pivot_longer(starts_with("x_"), names_to = "which_lead", values_to = "med_name") %>%
    filter(!is.na(med_name)) %>%
    filter(med_name != "0") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE,
             MED_END_DATE) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE, MED_END_DATE,
             med_name, .keep_all = T) %>%
    mutate(MEDS_OVERLAP_LEAD = paste0(med_name, collapse = '; ')) %>%
    select(-starts_with("x_")) %>%
    select(-c(num_end, which_lead, med_name)) %>%
    distinct()


  #### lag overlaps ---
  overlaps_lag <- all_meds %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    arrange(desc(MED_ORDER), .by_group = T) %>%
    mutate(num_end = row_number()) %>%
    arrange(MED_ORDER, .by_group = T)

  # for loop comparing all intervals
  for(i in 1:max_rounds) {
    # Head of for-loop
    new <- ifelse(int_overlaps(overlaps_lag$INTERVAL, lag(overlaps_lag$INTERVAL, i)), 1, 0) # Create new column
    overlaps_lag[ , ncol(overlaps_lag) + 1] <- new                  # Append new column
    colnames(overlaps_lag)[ncol(overlaps_lag)] <- paste0("x_", i)  # Rename column name
  }

  name <- names(overlaps_lag)
  name_1 <- name[8:as.numeric(max_rounds+7)]

  # make the values NA when they are checking for a lead for a different patient ID
  for (i in name_1){

    overlaps_lag[[i]] <- ifelse(overlaps_lag$MED_ORDER <= parse_number(i), NA, overlaps_lag[[i]])

  }

  # replace 1's with the actual med name
  for (i in name_1){
    overlaps_lag[[i]] <- ifelse(overlaps_lag[[i]] == 1, paste0(lag(overlaps_lag$MEDICATION_NAME,
                                                                   parse_number(i))), overlaps_lag[[i]])
  }

  lag_overlaps <- overlaps_lag %>%
    mutate(across(starts_with("x_"), as.character)) %>%
    pivot_longer(starts_with("x_"), names_to = "which_lag", values_to = "med_name") %>%
    filter(!is.na(med_name)) %>%
    filter(med_name != "0") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE,
             MED_END_DATE) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE, MED_END_DATE,
             med_name, .keep_all = T) %>%
    mutate(MEDS_OVERLAP_LAG = paste0(med_name, collapse = '; ')) %>%
    select(-starts_with("x_")) %>%
    select(-c(num_end, which_lag, med_name)) %>%
    distinct()

  all_overlaps <- lag_overlaps %>%
    full_join(lead_overlaps, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE, MED_END_DATE, INTERVAL,
                                          MED_ORDER)) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    arrange(MED_ORDER, .by_group = T) %>%
    pivot_longer(starts_with("MEDS_OVERLAP"), names_to = "leadorlag", values_to = "MEDS_OVERLAP_draft") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE, MED_END_DATE) %>%
    filter(!is.na(MEDS_OVERLAP_draft)) %>%
    mutate(MEDS_OVERLAP = paste0(MEDS_OVERLAP_draft, collapse = "; ")) %>%
    select(-c(leadorlag, MEDS_OVERLAP_draft)) %>%
    distinct() %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE,
           MED_END_DATE, MED_ORDER, MEDS_OVERLAP)

  #### OVERLAPPING DAYS ----

  overlaps_ints <- all_meds %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID)

  # make numbering variable go down backward to fill NA when there is no lead
  # interval to compare with
  overlaps_lead_ints <- all_meds %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    arrange(desc(MED_ORDER), .by_group = T) %>%
    mutate(num_end = row_number()) %>%
    arrange(MED_ORDER, .by_group = T)

  max_rounds <- overlaps_lead_ints %>%
    ungroup() %>%
    select(MED_ORDER) %>%
    slice(which.max(MED_ORDER))

  max_rounds <- max_rounds$MED_ORDER

  # for loop comparing all intervals
  for(i in 1:max_rounds) {
    # Head of for-loop
    new <- ifelse(int_overlaps(overlaps_lead_ints$INTERVAL, lead(overlaps_lead_ints$INTERVAL, i)), 1, 0) # Create new column
    overlaps_lead_ints[ , ncol(overlaps_lead_ints) + 1] <- new                  # Append new column
    colnames(overlaps_lead_ints)[ncol(overlaps_lead_ints)] <- paste0("x_", i)  # Rename column name
  }

  name <- names(overlaps_lead_ints)
  name_1 <- name[8:as.numeric(max_rounds+7)]

  # make the values NA when they are checking for a lead for a different patient ID
  for (i in name_1){

    overlaps_lead_ints[[i]] <- ifelse(overlaps_lead_ints$num_end <= parse_number(i), NA, overlaps_lead_ints[[i]])

  }

  # paste med interval after the med name
  for (i in name_1){
    overlaps_lead_ints[[i]] <- ifelse(overlaps_lead_ints[[i]] == 1, paste0(lead(overlaps_lead_ints$INTERVAL,
                                                                                parse_number(i)),
                                                                           "; ",
                                                                           lead(overlaps_lead_ints$MEDICATION_NAME,
                                                                                parse_number(i))), overlaps_lead_ints[[i]])
  }

  lead_overlaps_int <- overlaps_lead_ints %>%
    mutate(across(starts_with("x_"), as.character)) %>%
    pivot_longer(starts_with("x_"), names_to = "which_lead", values_to = "med_name") %>%
    filter(!is.na(med_name)) %>%
    filter(med_name != "0") %>%
    separate(med_name, sep = "; ", into = c("int", "med")) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE,
             MED_END_DATE) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE, MED_END_DATE,
             med, .keep_all = T) %>%
    mutate(int = interval(as.Date(int), as.Date(sub(".*--", "", int)))) %>%
    mutate(OVERLAP_DAYS = time_length(intersect(INTERVAL, int), unit = "days"))  %>%
    select(-which_lead)

  #### lag overlaps ---
  overlaps_lag_ints <- all_meds %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    arrange(desc(MED_ORDER), .by_group = T) %>%
    mutate(num_end = row_number()) %>%
    arrange(MED_ORDER, .by_group = T)

  # for loop comparing all intervals
  for(i in 1:max_rounds) {
    # Head of for-loop
    new <- ifelse(int_overlaps(overlaps_lag_ints$INTERVAL, lag(overlaps_lag_ints$INTERVAL, i)), 1, 0) # Create new column
    overlaps_lag_ints[ , ncol(overlaps_lag_ints) + 1] <- new                  # Append new column
    colnames(overlaps_lag_ints)[ncol(overlaps_lag_ints)] <- paste0("x_", i)  # Rename column name
  }

  name <- names(overlaps_lag_ints)
  name_1 <- name[8:as.numeric(max_rounds+7)]

  # make the values NA when they are checking for a lead for a different patient ID
  for (i in name_1){

    overlaps_lag_ints[[i]] <- ifelse(overlaps_lag_ints$MED_ORDER <= parse_number(i), NA, overlaps_lag_ints[[i]])

  }

  # replace 1's with the actual med name
  for (i in name_1){
    overlaps_lag_ints[[i]] <- ifelse(overlaps_lag_ints[[i]] == 1, paste0(lag(overlaps_lag_ints$INTERVAL,
                                                                             parse_number(i)),
                                                                         "; ",
                                                                         lag(overlaps_lag_ints$MEDICATION_NAME,
                                                                             parse_number(i)))
                                     , overlaps_lag_ints[[i]])
  }

  lag_overlaps_int <- overlaps_lag_ints %>%
    mutate(across(starts_with("x_"), as.character)) %>%
    pivot_longer(starts_with("x_"), names_to = "which_lag", values_to = "med_name") %>%
    filter(!is.na(med_name)) %>%
    filter(med_name != "0") %>%
    separate(med_name, sep = "; ", into = c("int", "med")) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE,
             MED_END_DATE) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE, MED_END_DATE,
             med, .keep_all = T) %>%
    mutate(int = interval(as.Date(int), as.Date(sub(".*--", "", int)))) %>%
    mutate(OVERLAP_DAYS = time_length(intersect(INTERVAL, int), unit = "days"))  %>%
    select(-which_lag)


#### ZERO DAYS OVERLAP REMOVE ----
  # create table with the overlapping meds of 0 days to remove them from the
  # lists
  zero_overlaps_remove <-  lag_overlaps_int %>%
    rbind(lead_overlaps_int) %>%
    filter(OVERLAP_DAYS == 0) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE, MED_END_DATE, med) %>%
    mutate(MED_GROUP = case_when(MEDICATION_NAME %in% c("Mesalamine", "Olsalazine", "Sulfasalazine") ~ "Aminosalicylates",
                                 MEDICATION_NAME %in% c("Azathioprine", "Mercaptopurine", "Tacrolimus", "Cyclosporine",
                                                        "Methotrexate") ~ "Immunomodulators",
                                 MEDICATION_NAME %in% c("Adalimumab", "Certolizumab Pegol", "Infliximab (Unspecified)",
                                                        "Natalizumab") ~ "Biologic")) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE, MED_END_DATE) %>%
    summarise(meds=paste(med,collapse='; ')) %>%
    separate(meds, into = c("meds1", "meds2", "meds3"), "; ")

  all_overlaps_int <- lag_overlaps_int %>%
    rbind(lead_overlaps_int) %>%
    filter(OVERLAP_DAYS != 0) %>%
    mutate(OVERLAP_DAYS = ifelse(OVERLAP_DAYS < 0, abs(OVERLAP_DAYS), OVERLAP_DAYS)) %>%
    rename(MED = med) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE, MED_END_DATE,
           INTERVAL, MED, int, OVERLAP_DAYS) %>%
    mutate(flag = ifelse(MEDICATION_NAME == MED, 1, 0)) %>%
    filter(flag == 0) %>%
    select(-flag) %>%
    # distinct() %>%
    pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, INTERVAL, MED_START_DATE,
                            MED_END_DATE),
                names_from = MED, names_prefix = "OVERLAP_DAYS_", values_from = OVERLAP_DAYS,
                values_fn = ~paste0(.x, collapse = "; ")) %>%
    rename_with(str_to_upper)

  final <- all_overlaps %>%
    left_join(all_overlaps_int, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME,
                                             MED_START_DATE, MED_END_DATE)) %>%
    # remove 0 day overlaps
    left_join(zero_overlaps_remove, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                                                 MEDICATION_NAME, MED_START_DATE, MED_END_DATE)) %>%
    ungroup() %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE, MED_END_DATE) %>%
    mutate(MEDS_OVERLAP = ifelse(!is.na(meds1) & str_detect(MEDS_OVERLAP, meds1),
                                 gsub(meds1, " ", MEDS_OVERLAP), MEDS_OVERLAP),
           MEDS_OVERLAP = ifelse(!is.na(meds2) & str_detect(MEDS_OVERLAP, meds2),
                                 gsub(meds2, " ", MEDS_OVERLAP), MEDS_OVERLAP),
           MEDS_OVERLAP = ifelse(!is.na(meds3) & str_detect(MEDS_OVERLAP, meds3),
                                 gsub(meds3, " ", MEDS_OVERLAP), MEDS_OVERLAP)) %>%
    mutate(MEDS_OVERLAP = gsub(" ; ", "", MEDS_OVERLAP)) %>%
    mutate(MEDS_OVERLAP = ifelse(MEDS_OVERLAP == " ", NA, MEDS_OVERLAP)) %>%
    select(-c(meds1, meds2, meds3))
}

#' risk_steroid_rounds
#'
#' create table with steroid rounds for RISK patients
#'
#'
#' @param prescriptions A dataframe with prescriptions for RISK data usually generated using load_data.
#' @param encounter A dataframe with encounter data usually generated using load_data.
#'
#' @return A dataframe of RISK patients the rounds of steroids they were on
#' @export
#'
#'
#'

risk_steroid_rounds <- function(prescriptions, encounter){

  # filter prescriptions
  prescriptions <- prescriptions %>%
    filter(DATA_SOURCE == "RISK")

  # keep only certain columns from prescriptions
  keep_cols <- as_tibble(as.list(remove_empty_cols(prescriptions))) %>%
    pivot_longer(everything(), names_to = "cols", values_to = "full") %>%
    filter(full == T) %>%
    select(cols)

  keep_cols <- keep_cols$cols

  #### FILTER PRESCRIPTIONS TABLE FOR STEROIDS ----

  steroids <- prescriptions %>%
    select(all_of(keep_cols)) %>%
    # make med start and med end dates
    mutate(MED_START_DATE = dmy(MED_START_DATE),
           MED_END_DATE = dmy(MED_END_DATE)) %>%
    # want to filter for the relevant rows (Yes, No for
    # MEDICATION_ADMINISTRATED_CODE). For Not Checked, there are no start or end
    # dates. For Checked there is no start or end date either. There are some NA
    # rows for MEDICATION_ADMINISTRATED that have a start or end date. Don't want
    # to just filter for rows that have start or end date because think there is
    # something with Ongoing I will want to check
    filter(MEDICATION_ADMINISTRATED == "No" | MEDICATION_ADMINISTRATED == "Yes" |
             is.na(MEDICATION_ADMINISTRATED)) %>%
    # two rows that are NA for MEDICATION_ADMINISTRATED but have a dosage amount,
    # not being included at this point
    mutate(flag = ifelse(is.na(MEDICATION_ADMINISTRATED) & !is.na(MED_START_DATE), 1, 0),
           flag = ifelse(is.na(MEDICATION_ADMINISTRATED) & !is.na(MED_END_DATE), 1, flag),
           flag = ifelse(!is.na(MEDICATION_ADMINISTRATED), 1, flag)) %>%
    filter(flag == 1 | is.na(flag)) %>%
    select(-flag) %>%
    mutate(MEDICATION_NAME = str_to_title(MEDICATION_NAME)) %>%
    filter(MEDICATION_NAME %in% c("Budesonide", "Methylprednisolone",
                                  "Prednisolone", "Hydrocortisone",
                                  "Corticosteroids"))

  #### GET FIRST START DATE ----
  earliest_start <- steroids %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    slice(which.min(MED_START_DATE)) %>%
    ungroup() %>%
    rename(MED_START_DATE_1 = MED_START_DATE)

  #### COUNT END DATES ----
  multiple_end_dates <- steroids %>%
    filter(!is.na(MED_END_DATE)) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE,
             .keep_all = T) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    add_count() %>%
    filter(n > 1) %>%
    # max stop dates is 7
    select(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, MEDICATION_NAME,
           MED_END_DATE, n) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    arrange(MED_END_DATE, .by_group = T) %>%
    mutate(order = row_number()) %>%
    ungroup()

  #### CREATE TABLES WITH END DATES

  ##  find FIRST med end dates
  first_med_end <- multiple_end_dates %>%
    filter(order == "1") %>%
    # join to steroids table to make sure there is another start date after the end
    # date. If not drop those patients, assume the latest end date is true end
    left_join(steroids %>%
                select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME,
                       MED_START_DATE) %>%
                filter(!is.na(MED_START_DATE)),
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                           MEDICATION_NAME), multiple = "all") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    # create flag to find med_start dates that are after the med end date
    mutate(flag = ifelse(MED_START_DATE > MED_END_DATE, 1, 0)) %>%
    # create flag for if there is no later med start date. want to filter these
    # patients out and have the last med end date be the only one. TDF: check
    # should I use first or last end date
    mutate(no_restart = ifelse(any(flag == 1), 0, 1))

  # first_med_end_dates_final -- no med start date after the min end date identified above
  # pts with second med end date but no third restart
  first_med_end_dates_final <- first_med_end %>%
    filter(no_restart == 1) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE) %>%
    rename(MED_END_DATE_1 = MED_END_DATE) %>%
    distinct() %>%
    mutate(MED_START_DATE_2 = NA)

  # continue creating first med end date table
  first_med_end_dates <- first_med_end %>%
    filter(no_restart == 0) %>%
    # filter for just the med start dates that are after the med end date. slice
    # for min.
    filter(flag == 1) %>%
    slice(which.min(MED_START_DATE)) %>%
    rename(MED_START_DATE_2 = MED_START_DATE,
           MED_END_DATE_1 = MED_END_DATE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE_1,
           MED_START_DATE_2) %>%
    # bind the patients from above that don't have a restart date
    rbind(first_med_end_dates_final)

  ## find SECOND med end dates
  # same logic as above, comments removed
  second_med_end <- multiple_end_dates %>%
    filter(order == "2") %>%
    left_join(steroids %>%
                select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME,
                       MED_START_DATE) %>%
                filter(!is.na(MED_START_DATE)),
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                           MEDICATION_NAME), multiple = "all") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    mutate(flag = ifelse(MED_START_DATE > MED_END_DATE, 1, 0)) %>%
    mutate(no_restart = ifelse(any(flag == 1), 0, 1))


  second_med_end_dates_final <- second_med_end %>%
    filter(no_restart == 1) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE) %>%
    rename(MED_END_DATE_2 = MED_END_DATE) %>%
    distinct() %>%
    mutate(MED_START_DATE_3 = NA)

  second_med_end_dates <- second_med_end %>%
    filter(no_restart == 0) %>%
    filter(flag == 1) %>%
    slice(which.min(MED_START_DATE)) %>%
    rename(MED_START_DATE_3 = MED_START_DATE,
           MED_END_DATE_2 = MED_END_DATE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE_2,
           MED_START_DATE_3) %>%
    rbind(second_med_end_dates_final)

  ## find THIRD med end dates
  # same logic as above
  third_med_end <- multiple_end_dates %>%
    filter(order == "3") %>%
    left_join(steroids %>%
                select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME,
                       MED_START_DATE) %>%
                filter(!is.na(MED_START_DATE)),
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                           MEDICATION_NAME), multiple = "all") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    mutate(flag = ifelse(MED_START_DATE > MED_END_DATE, 1, 0)) %>%
    mutate(no_restart = ifelse(any(flag == 1), 0, 1))


  third_med_end_dates_final <- third_med_end %>%
    filter(no_restart == 1) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE) %>%
    rename(MED_END_DATE_3 = MED_END_DATE) %>%
    distinct() %>%
    mutate(MED_START_DATE_4 = NA)

  third_med_end_dates <- third_med_end %>%
    filter(no_restart == 0) %>%
    filter(flag == 1) %>%
    slice(which.min(MED_START_DATE)) %>%
    rename(MED_START_DATE_4 = MED_START_DATE,
           MED_END_DATE_3 = MED_END_DATE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE_3,
           MED_START_DATE_4) %>%
    rbind(third_med_end_dates_final)

  ## find FOURTH med end dates
  # same logic as above
  fourth_med_end <- multiple_end_dates %>%
    filter(order == "4") %>%
    left_join(steroids %>%
                select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME,
                       MED_START_DATE) %>%
                filter(!is.na(MED_START_DATE)),
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                           MEDICATION_NAME), multiple = "all") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    mutate(flag = ifelse(MED_START_DATE > MED_END_DATE, 1, 0)) %>%
    mutate(no_restart = ifelse(any(flag == 1), 0, 1))

  fourth_med_end_dates_final <- fourth_med_end %>%
    filter(no_restart == 1) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE) %>%
    rename(MED_END_DATE_4 = MED_END_DATE) %>%
    distinct() %>%
    mutate(MED_START_DATE_5 = NA)

  fourth_med_end_dates <- fourth_med_end %>%
    filter(no_restart == 0) %>%
    filter(flag == 1) %>%
    slice(which.min(MED_START_DATE)) %>%
    rename(MED_START_DATE_5 = MED_START_DATE,
           MED_END_DATE_4 = MED_END_DATE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE_4,
           MED_START_DATE_5) %>%
    rbind(fourth_med_end_dates_final)

  ## find FIFTH med end dates
  # same logic as above
  fifth_med_end <- multiple_end_dates %>%
    filter(order == "5") %>%
    left_join(steroids %>%
                select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME,
                       MED_START_DATE) %>%
                filter(!is.na(MED_START_DATE)),
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                           MEDICATION_NAME), multiple = "all") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    mutate(flag = ifelse(MED_START_DATE > MED_END_DATE, 1, 0)) %>%
    mutate(no_restart = ifelse(any(flag == 1), 0, 1))


  fifth_med_end_dates_final <- fifth_med_end %>%
    filter(no_restart == 1) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE) %>%
    rename(MED_END_DATE_5 = MED_END_DATE) %>%
    distinct() %>%
    mutate(MED_START_DATE_6 = NA)

  fifth_med_end_dates <- fifth_med_end %>%
    filter(no_restart == 0) %>%
    filter(flag == 1) %>%
    slice(which.min(MED_START_DATE)) %>%
    rename(MED_START_DATE_6 = MED_START_DATE,
           MED_END_DATE_5 = MED_END_DATE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE_5,
           MED_START_DATE_6) %>%
    rbind(fifth_med_end_dates_final)

  ## find SIXTH med end dates
  # same logic as above
  sixth_med_end <- multiple_end_dates %>%
    filter(order == "6") %>%
    left_join(steroids %>%
                select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME,
                       MED_START_DATE) %>%
                filter(!is.na(MED_START_DATE)),
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                           MEDICATION_NAME), multiple = "all") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    mutate(flag = ifelse(MED_START_DATE > MED_END_DATE, 1, 0)) %>%
    mutate(no_restart = ifelse(any(flag == 1), 0, 1))


  sixth_med_end_dates_final <- sixth_med_end %>%
    filter(no_restart == 1) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE) %>%
    rename(MED_END_DATE_6 = MED_END_DATE) %>%
    distinct() %>%
    mutate(MED_START_DATE_7 = NA)

  sixth_med_end_dates <- sixth_med_end %>%
    filter(no_restart == 0) %>%
    filter(flag == 1) %>%
    slice(which.min(MED_START_DATE)) %>%
    rename(MED_START_DATE_7 = MED_START_DATE,
           MED_END_DATE_6 = MED_END_DATE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE_6,
           MED_START_DATE_7) %>%
    rbind(sixth_med_end_dates_final)

  ## find SEVENTH med end dates
  # same logic as above
  seventh_med_end <- multiple_end_dates %>%
    filter(order == "7") %>%
    left_join(steroids %>%
                select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME,
                       MED_START_DATE) %>%
                filter(!is.na(MED_START_DATE)),
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                           MEDICATION_NAME), multiple = "all") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    mutate(flag = ifelse(MED_START_DATE > MED_END_DATE, 1, 0)) %>%
    mutate(no_restart = ifelse(any(flag == 1), 0, 1))


  seventh_med_end_dates_final <- seventh_med_end %>%
    filter(no_restart == 1) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE) %>%
    rename(MED_END_DATE_7 = MED_END_DATE) %>%
    distinct() %>%
    mutate(MED_START_DATE_8 = NA)

  seventh_med_end_dates <- seventh_med_end %>%
    filter(no_restart == 0) %>%
    filter(flag == 1) %>%
    slice(which.min(MED_START_DATE)) %>%
    rename(MED_START_DATE_8 = MED_START_DATE,
           MED_END_DATE_7 = MED_END_DATE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE_7,
           MED_START_DATE_8) %>%
    rbind(seventh_med_end_dates_final)

  #### JOIN ALL TABLES ----

  # list the ID's and their medication names as base for final table
  multiple_end_dates_ids <- multiple_end_dates %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)

  steroid_rounds <- multiple_end_dates_ids %>%
    left_join(earliest_start, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                                           MEDICATION_NAME)) %>%
    left_join(first_med_end_dates,
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    left_join(second_med_end_dates,
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    left_join(third_med_end_dates,
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    left_join(fourth_med_end_dates,
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    left_join(fifth_med_end_dates,
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    left_join(sixth_med_end_dates,
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    left_join(seventh_med_end_dates,
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    # no start 8th time, drop that column
    select(-MED_START_DATE_8) %>%
    # pivot table longer
    pivot_longer(cols = starts_with('MED_START'), names_to = "order", values_to = "MED_START_DATE") %>%
    pivot_longer(cols = starts_with('MED_END'), names_to = "order2", values_to = "MED_END_DATE") %>%
    mutate(order = parse_number(order),
           order2 = parse_number(order2)) %>%
    # create flag to match start and end dates, drop all other pairings
    mutate(flag = ifelse(order == order2, 1, 0)) %>%
    filter(flag == 1) %>%
    select(-flag) %>%
    filter(!is.na(MED_START_DATE) & !is.na(MED_START_DATE)) %>%
    select(-starts_with("order")) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    ungroup() %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    mutate(STEROID_ROUND = row_number()) %>%
    mutate(STEROID_ROUND = paste0(MEDICATION_NAME, " ", STEROID_ROUND)) %>%
    ungroup() %>%
    mutate(date_flag_error = ifelse(MED_START_DATE > MED_END_DATE, 1, 0))

  #### FILL MISSING END DATES ----

  # Ids of patients with missing end dates
  missing_ends_ids <- steroid_rounds %>%
    filter(is.na(MED_END_DATE)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE) %>%
    distinct()

  # fill in missing ends if the medication administrated code is no for an ongoing
  # med action concept name
  missing_ends_ongoing_no <- steroid_rounds %>%
    filter(is.na(MED_END_DATE)) %>%
    left_join(prescriptions %>% select(
      DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, MEDICATION_NAME,
      MED_ACTION_CONCEPT_NAME, MEDICATION_ADMINISTRATED), multiple = "all",
      by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    left_join(encounter, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID)) %>%
    # create flag to filter table down to just encounters that are after med start date
    mutate(flag = if_else(MED_START_DATE > VISIT_ENCOUNTER_START_DATE, 1, 0)) %>%
    filter(flag != 1) %>%
    select(-flag) %>%
    # if the medication is ongoing and medication administrated is no, choose that
    # visit encounter start date as the med end date
    mutate(MED_END_DATE = if_else(MED_ACTION_CONCEPT_NAME == "Ongoing Treatment" &
                                    MEDICATION_ADMINISTRATED == "No", VISIT_ENCOUNTER_START_DATE, NA)) %>%
    filter(!is.na(MED_END_DATE)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE, MED_END_DATE,
           STEROID_ROUND) %>%
    distinct()

  # make the med end date for all other patients the day of their last visit
  # encounter ID
  missing_ends_other <- steroid_rounds %>%
    filter(is.na(MED_END_DATE)) %>%
    anti_join(missing_ends_ongoing_no %>%
                select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME),
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    left_join(prescriptions %>% select(
      DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, MEDICATION_NAME,
      MED_ACTION_CONCEPT_NAME, MEDICATION_ADMINISTRATED), multiple = "all",
      by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    left_join(encounter %>%
                select(DEIDENTIFIED_MASTER_PATIENT_ID,
                       VISIT_ENCOUNTER_START_DATE), by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID),
              multiple = "all") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    slice(which.max(VISIT_ENCOUNTER_START_DATE)) %>%
    # flag if med is after max visit encounter start date
    mutate(flag = if_else(MED_START_DATE > VISIT_ENCOUNTER_START_DATE, 1, 0)) %>%
    # all flagged patients only have one visit encounter, just use med_start_date
    # as the med_end_date
    mutate(MED_END_DATE = if_else(flag != 1, VISIT_ENCOUNTER_START_DATE,
                                  MED_START_DATE)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE,
           MED_END_DATE, STEROID_ROUND)

  all_missing_med_ends_final <- missing_ends_other %>%
    rbind(missing_ends_ongoing_no) %>%
    rename(MED_END_DATE_new = MED_END_DATE)

  steroid_rounds <- steroid_rounds %>%
    left_join(all_missing_med_ends_final, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                                                       MEDICATION_NAME, MED_START_DATE, STEROID_ROUND)) %>%
    mutate(MED_END_DATE = if_else(is.na(MED_END_DATE), MED_END_DATE_new, MED_END_DATE)) %>%
    select(-MED_END_DATE_new)  %>%
    mutate(date_flag_error = ifelse(MED_START_DATE > MED_END_DATE, 1, 0))

}

#' risk_antibiotics_rounds
#'
#' create table with antibiotics rounds for RISK patients
#'
#'
#' @param prescriptions A dataframe with prescriptions for RISK data usually generated using load_data.
#' @param encounter A dataframe with encounter data usually generated using load_data.
#'
#' @return A dataframe of RISK patients the rounds of antibiotics they were on
#' @export
#'
#'
#'

risk_antibiotic_rounds <- function(prescriptions, encounter){

  # filter prescriptions table
  prescriptions <- prescriptions %>%
    filter(DATA_SOURCE == "RISK")

  keep_cols <- as_tibble(as.list(remove_empty_cols(prescriptions))) %>%
    pivot_longer(everything(), names_to = "cols", values_to = "full") %>%
    filter(full == T) %>%
    select(cols)

  keep_cols <- keep_cols$cols

  #### FILTER PRESCRIPTIONS TABLE FOR ANTIBIOTICS ----

  antibiotics <- prescriptions %>%
    select(all_of(keep_cols)) %>%
    # make med start and med end dates
    mutate(MED_START_DATE = dmy(MED_START_DATE),
           MED_END_DATE = dmy(MED_END_DATE)) %>%
    # want to filter for the relevant rows (Yes, No for
    # MEDICATION_ADMINISTRATED_CODE). For Not Checked, there are no start or end
    # dates. For Checked there is no start or end date either. There are some NA
    # rows for MEDICATION_ADMINISTRATED that have a start or end date. Don't want
    # to just filter for rows that have start or end date because think there is
    # something with Ongoing I will want to check
    filter(MEDICATION_ADMINISTRATED == "No" | MEDICATION_ADMINISTRATED == "Yes" |
             is.na(MEDICATION_ADMINISTRATED)) %>%
    # two rows that are NA for MEDICATION_ADMINISTRATED but have a dosage amount,
    # not being included at this point
    mutate(flag = ifelse(is.na(MEDICATION_ADMINISTRATED) & !is.na(MED_START_DATE), 1, 0),
           flag = ifelse(is.na(MEDICATION_ADMINISTRATED) & !is.na(MED_END_DATE), 1, flag),
           flag = ifelse(!is.na(MEDICATION_ADMINISTRATED), 1, flag)) %>%
    filter(flag == 1 | is.na(flag)) %>%
    select(-flag) %>%
    mutate(MEDICATION_NAME = str_to_title(MEDICATION_NAME)) %>%
    filter(MEDICATION_NAME %in% c("Ciprofloxacin", "Metronidazole", "Rifaximin"))

  #### GET FIRST START DATE ----
  earliest_start <- antibiotics %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    slice(which.min(MED_START_DATE)) %>%
    ungroup() %>%
    rename(MED_START_DATE_1 = MED_START_DATE)

  #### COUNT END DATES ----
  multiple_end_dates <- antibiotics %>%
    filter(!is.na(MED_END_DATE)) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE,
             .keep_all = T) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    add_count() %>%
    filter(n > 1) %>%
    # max stop dates is 7
    select(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, MEDICATION_NAME,
           MED_END_DATE, n) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    arrange(MED_END_DATE, .by_group = T) %>%
    mutate(order = row_number()) %>%
    ungroup()

  #### CREATE TABLES WITH END DATES

  ##  find FIRST med end dates
  first_med_end <- multiple_end_dates %>%
    filter(order == "1") %>%
    # join to antibiotics table to make sure there is another start date after the end
    # date. If not drop those patients, assume the latest end date is true end
    left_join(antibiotics %>%
                select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME,
                       MED_START_DATE) %>%
                filter(!is.na(MED_START_DATE)),
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                           MEDICATION_NAME), multiple = "all") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    # create flag to find med_start dates that are after the med end date
    mutate(flag = ifelse(MED_START_DATE > MED_END_DATE, 1, 0)) %>%
    # create flag for if there is no later med start date. want to filter these
    # patients out and have the last med end date be the only one. TDF: check
    # should I use first or last end date
    mutate(no_restart = ifelse(any(flag == 1), 0, 1))

  # first_med_end_dates_final -- no med start date after the min end date identified above
  # pts with second med end date but no third restart
  first_med_end_dates_final <- first_med_end %>%
    filter(no_restart == 1) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE) %>%
    rename(MED_END_DATE_1 = MED_END_DATE) %>%
    distinct() %>%
    mutate(MED_START_DATE_2 = NA)

  # continue creating first med end date table
  first_med_end_dates <- first_med_end %>%
    filter(no_restart == 0) %>%
    # filter for just the med start dates that are after the med end date. slice
    # for min.
    filter(flag == 1) %>%
    slice(which.min(MED_START_DATE)) %>%
    rename(MED_START_DATE_2 = MED_START_DATE,
           MED_END_DATE_1 = MED_END_DATE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE_1,
           MED_START_DATE_2) %>%
    # bind the patients from above that don't have a restart date
    rbind(first_med_end_dates_final)

  ## find SECOND med end dates
  # same logic as above, comments removed
  second_med_end <- multiple_end_dates %>%
    filter(order == "2") %>%
    left_join(antibiotics %>%
                select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME,
                       MED_START_DATE) %>%
                filter(!is.na(MED_START_DATE)),
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                           MEDICATION_NAME), multiple = "all") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    mutate(flag = ifelse(MED_START_DATE > MED_END_DATE, 1, 0)) %>%
    mutate(no_restart = ifelse(any(flag == 1), 0, 1))


  second_med_end_dates_final <- second_med_end %>%
    filter(no_restart == 1) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE) %>%
    rename(MED_END_DATE_2 = MED_END_DATE) %>%
    distinct() %>%
    mutate(MED_START_DATE_3 = NA)

  second_med_end_dates <- second_med_end %>%
    filter(no_restart == 0) %>%
    filter(flag == 1) %>%
    slice(which.min(MED_START_DATE)) %>%
    rename(MED_START_DATE_3 = MED_START_DATE,
           MED_END_DATE_2 = MED_END_DATE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE_2,
           MED_START_DATE_3) %>%
    rbind(second_med_end_dates_final)

  ## find THIRD med end dates
  # same logic as above
  third_med_end <- multiple_end_dates %>%
    filter(order == "3") %>%
    left_join(antibiotics %>%
                select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME,
                       MED_START_DATE) %>%
                filter(!is.na(MED_START_DATE)),
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                           MEDICATION_NAME), multiple = "all") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    mutate(flag = ifelse(MED_START_DATE > MED_END_DATE, 1, 0)) %>%
    mutate(no_restart = ifelse(any(flag == 1), 0, 1))


  third_med_end_dates_final <- third_med_end %>%
    filter(no_restart == 1) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE) %>%
    rename(MED_END_DATE_3 = MED_END_DATE) %>%
    distinct() %>%
    mutate(MED_START_DATE_4 = NA)

  third_med_end_dates <- third_med_end %>%
    filter(no_restart == 0) %>%
    filter(flag == 1) %>%
    slice(which.min(MED_START_DATE)) %>%
    rename(MED_START_DATE_4 = MED_START_DATE,
           MED_END_DATE_3 = MED_END_DATE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE_3,
           MED_START_DATE_4) %>%
    rbind(third_med_end_dates_final)

  ## find FOURTH med end dates
  # same logic as above
  fourth_med_end <- multiple_end_dates %>%
    filter(order == "4") %>%
    left_join(antibiotics %>%
                select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME,
                       MED_START_DATE) %>%
                filter(!is.na(MED_START_DATE)),
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                           MEDICATION_NAME), multiple = "all") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    mutate(flag = ifelse(MED_START_DATE > MED_END_DATE, 1, 0)) %>%
    mutate(no_restart = ifelse(any(flag == 1), 0, 1))

  fourth_med_end_dates_final <- fourth_med_end %>%
    filter(no_restart == 1) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE) %>%
    rename(MED_END_DATE_4 = MED_END_DATE) %>%
    distinct() %>%
    mutate(MED_START_DATE_5 = NA)

  fourth_med_end_dates <- fourth_med_end %>%
    filter(no_restart == 0) %>%
    filter(flag == 1) %>%
    slice(which.min(MED_START_DATE)) %>%
    rename(MED_START_DATE_5 = MED_START_DATE,
           MED_END_DATE_4 = MED_END_DATE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE_4,
           MED_START_DATE_5) %>%
    rbind(fourth_med_end_dates_final)

  ## find FIFTH med end dates
  # same logic as above
  fifth_med_end <- multiple_end_dates %>%
    filter(order == "5") %>%
    left_join(antibiotics %>%
                select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME,
                       MED_START_DATE) %>%
                filter(!is.na(MED_START_DATE)),
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                           MEDICATION_NAME), multiple = "all") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    mutate(flag = ifelse(MED_START_DATE > MED_END_DATE, 1, 0)) %>%
    mutate(no_restart = ifelse(any(flag == 1), 0, 1))


  fifth_med_end_dates_final <- fifth_med_end %>%
    filter(no_restart == 1) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE) %>%
    rename(MED_END_DATE_5 = MED_END_DATE) %>%
    distinct() %>%
    mutate(MED_START_DATE_6 = NA)

  fifth_med_end_dates <- fifth_med_end %>%
    filter(no_restart == 0) %>%
    filter(flag == 1) %>%
    slice(which.min(MED_START_DATE)) %>%
    rename(MED_START_DATE_6 = MED_START_DATE,
           MED_END_DATE_5 = MED_END_DATE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE_5,
           MED_START_DATE_6) %>%
    rbind(fifth_med_end_dates_final)

  ## find SIXTH med end dates
  # same logic as above
  sixth_med_end <- multiple_end_dates %>%
    filter(order == "6") %>%
    left_join(antibiotics %>%
                select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME,
                       MED_START_DATE) %>%
                filter(!is.na(MED_START_DATE)),
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                           MEDICATION_NAME), multiple = "all") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    mutate(flag = ifelse(MED_START_DATE > MED_END_DATE, 1, 0)) %>%
    mutate(no_restart = ifelse(any(flag == 1), 0, 1))


  sixth_med_end_dates_final <- sixth_med_end %>%
    filter(no_restart == 1) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE) %>%
    rename(MED_END_DATE_6 = MED_END_DATE) %>%
    distinct() %>%
    mutate(MED_START_DATE_7 = NA)

  sixth_med_end_dates <- sixth_med_end %>%
    filter(no_restart == 0) %>%
    filter(flag == 1) %>%
    slice(which.min(MED_START_DATE)) %>%
    rename(MED_START_DATE_7 = MED_START_DATE,
           MED_END_DATE_6 = MED_END_DATE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE_6,
           MED_START_DATE_7) %>%
    rbind(sixth_med_end_dates_final)

  #### JOIN ALL TABLES ----

  # list the ID's and their medication names as base for final table
  multiple_end_dates_ids <- multiple_end_dates %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)

  antibiotics_rounds <- multiple_end_dates_ids %>%
    left_join(earliest_start, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                                           MEDICATION_NAME)) %>%
    left_join(first_med_end_dates,
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    left_join(second_med_end_dates,
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    left_join(third_med_end_dates,
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    left_join(fourth_med_end_dates,
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    left_join(fifth_med_end_dates,
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    left_join(sixth_med_end_dates,
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    # no start 8th time, drop that column
    select(-MED_START_DATE_7) %>%
    # pivot table longer
    pivot_longer(cols = starts_with('MED_START'), names_to = "order", values_to = "MED_START_DATE") %>%
    pivot_longer(cols = starts_with('MED_END'), names_to = "order2", values_to = "MED_END_DATE") %>%
    mutate(order = parse_number(order),
           order2 = parse_number(order2)) %>%
    # create flag to match start and end dates, drop all other pairings
    mutate(flag = ifelse(order == order2, 1, 0)) %>%
    filter(flag == 1) %>%
    select(-flag) %>%
    filter(!is.na(MED_START_DATE) & !is.na(MED_START_DATE)) %>%
    select(-starts_with("order")) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    ungroup() %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    mutate(ANTIBIOTIC_ROUND_NUM = row_number()) %>%
    mutate(ANTIBIOTIC_ROUND = paste0(MEDICATION_NAME, " ", ANTIBIOTIC_ROUND_NUM)) %>%
    select(-ANTIBIOTIC_ROUND_NUM) %>%
    ungroup() %>%
    mutate(date_flag_error = ifelse(MED_START_DATE > MED_END_DATE, 1, 0))

  #### FILL MISSING END DATES ----

  # Ids of patients with missing end dates
  missing_ends_ids <- antibiotics_rounds %>%
    filter(is.na(MED_END_DATE)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE) %>%
    distinct()

  # fill in missing ends if the medication administrated code is no for an ongoing
  # med action concept name
  missing_ends_ongoing_no <- antibiotics_rounds %>%
    filter(is.na(MED_END_DATE)) %>%
    left_join(prescriptions %>% select(
      DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, MEDICATION_NAME,
      MED_ACTION_CONCEPT_NAME, MEDICATION_ADMINISTRATED), multiple = "all",
      by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    left_join(encounter, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID)) %>%
    # create flag to filter table down to just encounters that are after med start date
    mutate(flag = if_else(MED_START_DATE > VISIT_ENCOUNTER_START_DATE, 1, 0)) %>%
    filter(flag != 1) %>%
    select(-flag) %>%
    # if the medication is ongoing and medication administrated is no, choose that
    # visit encounter start date as the med end date
    mutate(MED_END_DATE = if_else(MED_ACTION_CONCEPT_NAME == "Ongoing Treatment" &
                                    MEDICATION_ADMINISTRATED == "No", VISIT_ENCOUNTER_START_DATE, NA)) %>%
    filter(!is.na(MED_END_DATE)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE, MED_END_DATE,
           ANTIBIOTIC_ROUND) %>%
    distinct()

  # make the med end date for all other patients the day of their last visit
  # encounter ID
  missing_ends_other <- antibiotics_rounds %>%
    filter(is.na(MED_END_DATE)) %>%
    anti_join(missing_ends_ongoing_no %>%
                select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME),
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    left_join(prescriptions %>% select(
      DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, MEDICATION_NAME,
      MED_ACTION_CONCEPT_NAME, MEDICATION_ADMINISTRATED), multiple = "all",
      by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    left_join(encounter %>%
                select(DEIDENTIFIED_MASTER_PATIENT_ID,
                       VISIT_ENCOUNTER_START_DATE), by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID),
              multiple = "all") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    slice(which.max(VISIT_ENCOUNTER_START_DATE)) %>%
    # flag if med is after max visit encounter start date
    mutate(flag = if_else(MED_START_DATE > VISIT_ENCOUNTER_START_DATE, 1, 0)) %>%
    # all flagged patients only have one visit encounter, just use med_start_date
    # as the med_end_date
    mutate(MED_END_DATE = if_else(flag != 1, VISIT_ENCOUNTER_START_DATE,
                                  MED_START_DATE)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE,
           MED_END_DATE, ANTIBIOTIC_ROUND)

  all_missing_med_ends_final <- missing_ends_other %>%
    rbind(missing_ends_ongoing_no) %>%
    rename(MED_END_DATE_new = MED_END_DATE)

  antibiotics_rounds <- antibiotics_rounds %>%
    left_join(all_missing_med_ends_final, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                                                       MEDICATION_NAME, MED_START_DATE, ANTIBIOTIC_ROUND)) %>%
    mutate(MED_END_DATE = if_else(is.na(MED_END_DATE), MED_END_DATE_new, MED_END_DATE)) %>%
    select(-MED_END_DATE_new)  %>%
    mutate(date_flag_error = ifelse(MED_START_DATE > MED_END_DATE, 1, 0))
}
