#' risk_med_journey
#'
#' Creates RISK medication journey table for long term medications including Biologics, 5-ASA's and Immunomodulators.
#'
#' @param prescriptions A dataframe with prescriptions for RISK data usually generated using load_data.
#' @param encounter A dataframe with encounter data usually generated using load_data.
#'
#' @return A table with the RISK medication journey table.
#'
#' @details Medication start and stop dates are from RISK case report forms. If there is no medication end date, but a new medication with the same mechanism of action was started, the day before the start of the new medication is used as the medication end date.
#'
#' The following columns are convenience flags and indices to facilitate easy filtering of the data:
#'
#' \describe{
#'   \item{MEDICATION_NUMBER}{counts the number of different medications in a patients' journey.
#' The medication with with earliest start date will have MEDICATION_NUMBER = 1.}
#'   \item{STEROID_OVERLAP}{is 1 if a patient also reported being on steroids at some point that overlaps with the medication interval.}
#'   \item{ANTIBIOTIC_OVERLAP}{is 1 if a patient also reported being on an antibiotic at some point that overlaps with the medication interval.}
#' }
#' @export
#'
#' @examples
#' # upload data
#' # dat <- load_data("~/r_input/", cohort = "RISK", domains = c("prescriptions", "encounter"), data_type = "CRF")
#' # export RISK Summary Table to xlsx
#' # risk_med_journey_df <- risk_med_journey(dat$prescriptions, dat$encounter)
#'
risk_med_journey <- function(prescriptions, encounter) {
  # filter data
  prescriptions <- prescriptions %>%
    filter(DATA_SOURCE == "RISK") %>%
    mutate(END_DATE_IMPUTED = ifelse(is.na(MED_END_DATE), 1, 0))

  # find columns to keep
  keep_cols <- as_tibble(as.list(remove_empty_cols(prescriptions))) %>%
    pivot_longer(everything(), names_to = "cols", values_to = "full") %>%
    filter(full == T) %>%
    select(cols)

  keep_cols <- keep_cols$cols

  # eventually: left join encounter table to use visit encounter start date when medication
  # administrated is Yes but there is no med start date? Need to pull forward
  # med start dates first.

  ### CLB FIX: some med start dates after med end dates. One adalimumab pt is just
  ### entered like that. methotrexate problem when med start date is the same as
  ### med end date

  #### MED_GROUP_TABLE ----

  med_group <- prescriptions %>%
    distinct(MEDICATION_NAME) %>%
    # select out all of the medication names that are just leading questions at
    # enrollment
    filter(MEDICATION_NAME != "Immunomodulators" & MEDICATION_NAME != "Corticosteroids" &
             MEDICATION_NAME != "Antibiotics" & MEDICATION_NAME != "5-ASA Oral" &
             MEDICATION_NAME != "Biologic Agents" & MEDICATION_NAME != "Exclusive Enteral Therapy" &
             MEDICATION_NAME != "Supplemental Enteral Therapy") %>%
    mutate(MEDICATION_NAME = str_to_title(MEDICATION_NAME)) %>%
    mutate(MED_GROUP = case_when(
      MEDICATION_NAME %in% c("Mesalamine", "Olsalazine", "Sulfasalazine") ~ "Aminosalicylates",
      MEDICATION_NAME %in% c(
        "Azathioprine", "Mercaptopurine", "Tacrolimus", "Cyclosporine",
        "Methotrexate"
      ) ~ "Immunomodulators",
      MEDICATION_NAME %in% c(
        "Adalimumab", "Certolizumab Pegol", "Infliximab (Unspecified)",
        "Natalizumab"
      ) ~ "Biologic",
      MEDICATION_NAME %in% c(
        "Budesonide", "Methylprednisolone",
        "Prednisolone", "Hydrocortisone",
        "Corticosteroids"
      ) ~ "Corticosteroids",
      MEDICATION_NAME %in% c("Ciprofloxacin", "Metronidazole", "Rifaximin") ~ "Antibiotics"
    ))

  #### FILTER PRESCRIPTIONS TABLE ----

  meds <- prescriptions %>%
    select(all_of(keep_cols)) %>%
    # make med start and med end dates
    mutate(
      MED_START_DATE = dmy(MED_START_DATE),
      MED_END_DATE = dmy(MED_END_DATE)
    ) %>%
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
    mutate(
      flag = ifelse(is.na(MEDICATION_ADMINISTRATED) & !is.na(MED_START_DATE), 1, 0),
      flag = ifelse(is.na(MEDICATION_ADMINISTRATED) & !is.na(MED_END_DATE), 1, flag),
      flag = ifelse(!is.na(MEDICATION_ADMINISTRATED), 1, flag)
    ) %>%
    filter(flag == 1 | is.na(flag)) %>%
    select(-flag) %>%
    # filter for the meds in sparc script first. Try to work on prednisone/on-off
    # meds later
    mutate(MEDICATION_NAME = str_to_title(MEDICATION_NAME)) %>%
    mutate(MED_GROUP = case_when(
      MEDICATION_NAME %in% c("Mesalamine", "Olsalazine", "Sulfasalazine") ~ "Aminosalicylates",
      MEDICATION_NAME %in% c(
        "Azathioprine", "Mercaptopurine", "Tacrolimus", "Cyclosporine",
        "Methotrexate"
      ) ~ "Immunomodulators",
      MEDICATION_NAME %in% c(
        "Adalimumab", "Certolizumab Pegol", "Infliximab (Unspecified)",
        "Natalizumab"
      ) ~ "Biologic"
    )) %>%
    filter(!is.na(MED_GROUP))

  #### MOA ----

  # create MOA dataframe (from SPARC)

  moa <- meds %>%
    distinct(MEDICATION_NAME, MED_GROUP) %>%
    mutate(MOA = case_when(
      grepl("Adalimumab|Certolizumab Pegol", MEDICATION_NAME, ignore.case = T) ~ "antiTNF",
      MEDICATION_NAME %in% c("Natalizumab") ~ "IRA",
      MEDICATION_NAME == "Infliximab (Unspecified)" ~ "antiTNF",
      TRUE ~ MEDICATION_NAME
    )) %>%
    select(MEDICATION_NAME, MOA)

  # join moa to meds
  meds <- meds %>%
    left_join(moa, by = join_by(MEDICATION_NAME))


  #### ANTI-TNFs DATES ----

  # CLB: check with tara what to do when the med end for one anti-tnf is after
  # the med start for another anti-tnf

  # create the logic for the start of one biologic with same MOA to be the end of
  # previous one. Only have to worry about anti-TNF for RISK

  antitnfs <- meds %>%
    filter(MOA == "antiTNF") %>%
    filter(MEDICATION_NAME != "Infliximab (Unspecified)" & MEDICATION_NAME != "Natalizumab") %>%
    # join encounter table here right now, if do it earlier can remove
    left_join(encounter %>%
                select(
                  DEIDENTIFIED_MASTER_PATIENT_ID,
                  VISIT_ENCOUNTER_ID,
                  VISIT_ENCOUNTER_START_DATE
                ), by = join_by(
                  DEIDENTIFIED_MASTER_PATIENT_ID,
                  VISIT_ENCOUNTER_ID
                )) %>%
    # use visit encounter start date if medication administrated is yes and there
    # is no med start date. will slice for earliest med start date later so it is
    # okay if the first medication administrated date is earlier than visit
    # encounter start
    mutate(MED_START_DATE = if_else(is.na(MED_START_DATE) & MEDICATION_ADMINISTRATED == "Yes" & is.na(MED_END_DATE),
                                    VISIT_ENCOUNTER_START_DATE, MED_START_DATE
    ))

  # get starts of antiTNFS to create interval with end date, and choose end date if pt started new antiTNF
  antitnfs_starts <- antitnfs %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    slice(which.min(MED_START_DATE)) %>%
    ungroup() %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MOA, MED_START_DATE)

  # get antiTNF ends
  antitnfs_ends <- antitnfs %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    slice(which.max(MED_END_DATE)) %>%
    ungroup() %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MOA, MED_END_DATE)

  # create tibble with all TNFs, use one day before start date as end date if pt
  # doesn't have end date and switches to a drug with same MOA
  antitnf_dates <- antitnfs_starts %>%
    full_join(antitnfs_ends, by = join_by(
      DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME,
      MOA
    )) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    mutate(order = row_number()) %>%
    mutate(MED_END_DATE = if_else(is.na(MED_END_DATE), lead(MED_START_DATE) - 1, MED_END_DATE)) %>%
    # create flag for pts who have a med end and a med start date the same day.
    # subtract 1 day from the med end dates to match the logic previously and
    # avoid having those intervals overlap
    mutate(flag = if_else(MED_END_DATE == lead(MED_START_DATE), 1, 0)) %>%
    # need this flag to avoid recorded end dates, ungroup first, then regroup
    mutate(flag = if_else(!is.na(MED_END_DATE) & flag != 1, 0, flag)) %>%
    mutate(MED_END_DATE = if_else(flag == "1", MED_END_DATE - 1, MED_END_DATE)) %>%
    select(-flag) %>%
    # find if any patients have an end date after the start date for the next med
    ## CHECK TDF: 3 patients with an end date after the med start date for a drug
    ## with same MOA, what to do w these pts?
    mutate(flag = if_else(MED_END_DATE > lead(MED_START_DATE), 1, 0)) %>%
    # CLB: LOGIC - if med start date for same MOA before med end date of previous
    # drug, use one day before med start date as previous med end date
    mutate(MED_END_DATE = if_else(flag == 1, lead(MED_START_DATE) - 1, MED_END_DATE)) %>%
    select(-flag)

  # use last day administrated as end date for infliximab and natalizumab if
  # other antiTNF is not started
  inflix_nata <- meds %>%
    filter(MEDICATION_NAME == "Infliximab (Unspecified)" | MEDICATION_NAME == "Natalizumab") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    filter(!is.na(MED_START_DATE)) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    arrange(MED_START_DATE, .by_group = T)

  # end dates for infliximab and natalizumab are the latest medication administrated date
  inflix_nata_end <- inflix_nata %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    slice(which.max(MED_START_DATE)) %>%
    mutate(MED_END_DATE = MED_START_DATE) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MOA, MED_END_DATE)

  # infliximab and natalizumab start dates, select earliest
  inflix_nata_start <- inflix_nata %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    slice(which.min(MED_START_DATE)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MOA, MED_START_DATE)

  # FINAL infliximab/natalizumab dates
  inflix_nata_final <- inflix_nata_start %>%
    left_join(inflix_nata_end, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MOA))

  # join infliximab and natalizumab to other antitfs
  antitnf_dates <- antitnf_dates %>%
    select(-order) %>%
    rbind(inflix_nata_final) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    mutate(order = row_number())

  #### MULTIPLE MED END DATES  ----

  multiple_end_dates <- meds %>%
    # filter(MOA != "antiTNF") %>%
    filter(!is.na(MED_END_DATE)) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE,
             .keep_all = T
    ) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    add_count() %>%
    filter(n > 1) %>%
    # max stop dates is 4
    select(
      DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, MEDICATION_NAME,
      MED_END_DATE, MED_GROUP, n
    ) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    arrange(MED_END_DATE, .by_group = T) %>%
    mutate(order = row_number()) %>%
    ungroup()

  # find the first med end date
  first_med_end <- multiple_end_dates %>%
    filter(order == "1") %>%
    # join to meds table to make sure there is another start date after the end
    # date. If not drop those patients, assume the latest end date is true end
    left_join(
      meds %>%
        select(
          DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME,
          MED_START_DATE, MED_GROUP
        ) %>%
        filter(!is.na(MED_START_DATE)),
      by = join_by(
        DEIDENTIFIED_MASTER_PATIENT_ID,
        MEDICATION_NAME, MED_GROUP
      ), multiple = "all"
    ) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    # create flag to find med_start dates that are after the med end date
    mutate(flag = ifelse(MED_START_DATE > MED_END_DATE, 1, 0)) %>%
    # create flag for if there is no later med start date. want to filter these
    # patients out and have the last med end date be the only one. TDF: check
    # should I use first or last end date
    mutate(no_restart = ifelse(any(flag == 1), 0, 1))

  # get first end dates for patients that have no restart date later
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
    rename(
      MED_START_DATE_2 = MED_START_DATE,
      MED_END_DATE_1 = MED_END_DATE
    ) %>%
    # bind patients with no second restart
    rbind(first_med_end_dates_final)

  ##  find second med end dates
  second_med_end <- multiple_end_dates %>%
    filter(order == "2") %>%
    # join to meds table to make sure there is another start date after the end
    # date. If not drop those patients, assume the latest end date is true end
    left_join(
      meds %>%
        select(
          DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME,
          MED_START_DATE, MED_GROUP
        ) %>%
        filter(!is.na(MED_START_DATE)),
      by = join_by(
        DEIDENTIFIED_MASTER_PATIENT_ID,
        MEDICATION_NAME, MED_GROUP
      ), multiple = "all"
    ) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    # create flag to find med_start dates that are after the med end date
    mutate(flag = ifelse(MED_START_DATE > MED_END_DATE, 1, 0)) %>%
    # create flag for if there is no later med start date. want to filter these
    # patients out and have the last med end date be the only one. TDF: check
    # should I use first or last end date
    mutate(no_restart = ifelse(any(flag == 1), 0, 1))

  # second_med_end_dates_final
  # pts with second med end date but no third restart
  second_med_end_dates_final <- second_med_end %>%
    filter(no_restart == 1) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE) %>%
    rename(MED_END_DATE_2 = MED_END_DATE) %>%
    distinct() %>%
    mutate(MED_START_DATE_3 = NA)

  # continue creating first med end date table
  second_med_end_dates <- second_med_end %>%
    filter(no_restart == 0) %>%
    # filter for just the med start dates that are after the med end date. slice
    # for min.
    filter(flag == 1) %>%
    slice(which.min(MED_START_DATE)) %>%
    rename(
      MED_START_DATE_3 = MED_START_DATE,
      MED_END_DATE_2 = MED_END_DATE
    ) %>%
    select(
      DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE_2,
      MED_START_DATE_3
    ) %>%
    # bind the patients from above that don't have a restart date
    rbind(second_med_end_dates_final)

  ## find third end med dates
  ##  find second med end dates
  third_med_end <- multiple_end_dates %>%
    filter(order == "3") %>%
    # join to meds table to make sure there is another start date after the end
    # date. If not drop those patients, assume the latest end date is true end
    left_join(
      meds %>%
        select(
          DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME,
          MED_START_DATE, MED_GROUP
        ) %>%
        filter(!is.na(MED_START_DATE)),
      by = join_by(
        DEIDENTIFIED_MASTER_PATIENT_ID,
        MEDICATION_NAME, MED_GROUP
      ), multiple = "all"
    ) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    # create flag to find med_start dates that are after the med end date
    mutate(flag = ifelse(MED_START_DATE > MED_END_DATE, 1, 0)) %>%
    # create flag for if there is no later med start date. want to filter these
    # patients out and have the last med end date be the only one. TDF: check
    # should I use first or last end date
    mutate(no_restart = ifelse(any(flag == 1), 0, 1))

  # third_med_end_dates_final
  # pts with third med end date but no fourth restart
  third_med_end_dates_final <- third_med_end %>%
    filter(no_restart == 1) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE) %>%
    rename(MED_END_DATE_3 = MED_END_DATE) %>%
    distinct() %>%
    mutate(MED_START_DATE_4 = NA)

  # continue creating first med end date table
  third_med_end_dates <- third_med_end %>%
    filter(no_restart == 0) %>%
    # filter for just the med start dates that are after the med end date. slice
    # for min.
    filter(flag == 1) %>%
    slice(which.min(MED_START_DATE)) %>%
    rename(
      MED_START_DATE_4 = MED_START_DATE,
      MED_END_DATE_3 = MED_END_DATE
    ) %>%
    select(
      DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE_3,
      MED_START_DATE_4
    ) %>%
    # bind the patients from above that don't have a restart date
    rbind(third_med_end_dates_final)

  ## find fourth end med dates
  fourth_med_end <- multiple_end_dates %>%
    filter(order == "4") %>%
    # join to meds table to make sure there is another start date after the end
    # date. If not drop those patients, assume the latest end date is true end
    left_join(
      meds %>%
        select(
          DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME,
          MED_START_DATE, MED_GROUP
        ) %>%
        filter(!is.na(MED_START_DATE)),
      by = join_by(
        DEIDENTIFIED_MASTER_PATIENT_ID,
        MEDICATION_NAME, MED_GROUP
      ), multiple = "all"
    ) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    # create flag to find med_start dates that are after the med end date
    mutate(flag = ifelse(MED_START_DATE > MED_END_DATE, 1, 0)) %>%
    # create flag for if there is no later med start date. want to filter these
    # patients out and have the last med end date be the only one. TDF: check
    # should I use first or last end date
    mutate(no_restart = ifelse(any(flag == 1), 0, 1))

  # fourth_med_end_dates_final
  # pts with fourth med end date but no fifth restart
  fourth_med_end_dates_final <- fourth_med_end %>%
    filter(no_restart == 1) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE) %>%
    rename(MED_END_DATE_4 = MED_END_DATE) %>%
    distinct() %>%
    mutate(MED_START_DATE_5 = NA)

  # continue creating first med end date table
  fourth_med_end_dates <- fourth_med_end %>%
    filter(no_restart == 0) %>%
    # filter for just the med start dates that are after the med end date. slice
    # for min.
    filter(flag == 1) %>%
    slice(which.min(MED_START_DATE)) %>%
    rename(
      MED_START_DATE_5 = MED_START_DATE,
      MED_END_DATE_4 = MED_END_DATE
    ) %>%
    select(
      DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE_4,
      MED_START_DATE_5
    ) %>%
    # bind the patients from above that don't have a restart date
    rbind(fourth_med_end_dates_final)

  ## create table with all the med end and start dates for medications that
  ## patients start and stop more than once

  # get earliest med date for these patients
  earliest_start_multiple_end <- meds %>%
    right_join(multiple_end_dates %>%
                 select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
                 distinct(), by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    slice(which.min(MED_START_DATE)) %>%
    ungroup() %>%
    rename(MED_START_DATE_1 = MED_START_DATE)

  # list the ID's with multiple end dates and their medication name
  multiple_end_dates_ids <- multiple_end_dates %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)

  ### JOINS HERE FOR MULTIPLE END DATES
  multiple_end_dates_final <- multiple_end_dates_ids %>%
    left_join(earliest_start_multiple_end, by = join_by(
      DEIDENTIFIED_MASTER_PATIENT_ID,
      MEDICATION_NAME
    )) %>%
    left_join(
      first_med_end_dates %>%
        select(-c(n, order, flag, no_restart, VISIT_ENCOUNTER_ID, MED_GROUP)),
      by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)
    ) %>%
    # join all patients with two med ends dates
    left_join(second_med_end_dates, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    left_join(third_med_end_dates,
              by = join_by(
                DEIDENTIFIED_MASTER_PATIENT_ID,
                MEDICATION_NAME
              )
    ) %>%
    # join fourth med end dates
    left_join(fourth_med_end_dates,
              by = join_by(
                DEIDENTIFIED_MASTER_PATIENT_ID,
                MEDICATION_NAME
              )
    ) %>%
    # MULTIPLE END DATES, FEWER START DATES, USE LAST END DATE AVAILABLE
    # CLB: use last end date if two end dates and no second start date
    mutate(MED_END_DATE_1 = if_else(is.na(MED_START_DATE_2) & !is.na(MED_END_DATE_2),
                                    MED_END_DATE_2, MED_END_DATE_1
    )) %>%
    # CLB: use last end date if three end dates and no third start date
    mutate(MED_END_DATE_2 = if_else(is.na(MED_START_DATE_3) & !is.na(MED_END_DATE_3),
                                    MED_END_DATE_3, MED_END_DATE_2
    )) %>%
    # CLB: use last end date if four end dates and no fourth start date
    mutate(MED_END_DATE_3 = if_else(is.na(MED_START_DATE_4) & !is.na(MED_END_DATE_4),
                                    MED_END_DATE_4, MED_END_DATE_3
    )) %>%
    # pivot table longer
    pivot_longer(cols = starts_with("MED_START"), names_to = "order", values_to = "MED_START_DATE") %>%
    pivot_longer(cols = starts_with("MED_END"), names_to = "order2", values_to = "MED_END_DATE") %>%
    # create flag to match start and end dates, drop all other pairings
    mutate(flag = case_when(
      order == "MED_START_DATE_1" & order2 == "MED_END_DATE_1" ~ 1,
      order == "MED_START_DATE_2" & order2 == "MED_END_DATE_2" ~ 1,
      order == "MED_START_DATE_3" & order2 == "MED_END_DATE_3" ~ 1,
      order == "MED_START_DATE_4" & order2 == "MED_END_DATE_4" ~ 1,
      TRUE ~ 0
    )) %>%
    filter(flag == 1) %>%
    select(-flag) %>%
    # create another flag when there is no med end or start date to filter out
    mutate(flag = ifelse(is.na(MED_START_DATE) & is.na(MED_END_DATE), 0, 1)) %>%
    filter(flag == 1) %>%
    select(-flag) %>%
    filter(!is.na(MED_START_DATE)) %>%
    select(-starts_with("order")) %>%
    ungroup()

  #### JOIN ANTI-TNF INTERVALS WITH MULTIPLE END DATE INTERVALS ----

  # find patients with an anti-tnf medication with multiple start and stop dates.
  # filter those ID's and meds out of anti-tnf table before join with multiple end
  # table to avoid duplication

  # 4 - patients with multiple end dates for an anti-tnf, no
  # overlaps, everything looks good

  anti_tnf_ids <- antitnf_dates %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    distinct()

  multiple_end_anti_tnf_ids <- multiple_end_dates_final %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    distinct()

  chk <- anti_tnf_ids %>%
    inner_join(multiple_end_anti_tnf_ids, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME))

  antitnf_dates_final <- antitnf_dates %>%
    anti_join(chk, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    ungroup()

  # remove these tables
  rm(anti_tnf_ids, multiple_end_anti_tnf_ids, chk)

  ## JOIN ANTI-TNF FINAL TABLE AND MULTIPLE END DATE TABLE

  table <- antitnf_dates_final %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE, MED_END_DATE) %>%
    rbind(multiple_end_dates_final)

  #### SPECIAL CASE IDS ----

  # find the ids and medications of patients on anti-tnfs or with multiple start
  # and stop dates to avoid creating duplicates below

  special_ids <- table %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)

  #### MED START DATES - ONE INTERVAL ----

  # find the first medication start date for meds by patient
  # group by patient and medication name, everything but anti-tnfs
  first_med_starts <- meds %>%
    anti_join(special_ids, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    # filter(MOA != "antiTNF") %>%
    # join encounter table here, again use visit encounter start date if
    # medication was administrated but no start or end date in the row. flag these
    # and slice for actual min start date?
    left_join(encounter %>%
                select(
                  DEIDENTIFIED_MASTER_PATIENT_ID,
                  VISIT_ENCOUNTER_ID,
                  VISIT_ENCOUNTER_START_DATE
                ), by = join_by(
                  DEIDENTIFIED_MASTER_PATIENT_ID,
                  VISIT_ENCOUNTER_ID
                )) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    # creating flag here to see how many times visit_encounter_start_date is used
    # as the med_start_date
    mutate(flag = if_else(is.na(MED_START_DATE) & MEDICATION_ADMINISTRATED == "Yes" & is.na(MED_END_DATE), 1, 0)) %>%
    mutate(MED_START_DATE = if_else(is.na(MED_START_DATE) & MEDICATION_ADMINISTRATED == "Yes" & is.na(MED_END_DATE),
                                    VISIT_ENCOUNTER_START_DATE, MED_START_DATE
    )) %>%
    slice(which.min(MED_START_DATE)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE) %>%
    ungroup()

  #### MED END DATES - ONE INTERVAL ----

  # find med end dates
  last_med_ends <- meds %>%
    anti_join(special_ids, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME) %>%
    filter(!is.na(MED_END_DATE)) %>%
    # filter(MOA != "antiTNF") %>%
    slice(which.max(MED_END_DATE)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE) %>%
    ungroup()

  #### ONE INTERVAL, NO ANTI-TNF, FULL JOIN ----

  meds_dates_final <- first_med_starts %>%
    full_join(last_med_ends, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME))


  #### JOIN ALL ----

  table <- meds_dates_final %>%
    rbind(antitnf_dates_final %>%
            select(
              DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE,
              MED_END_DATE
            )) %>%
    rbind(multiple_end_dates_final) %>%
    # create med order number
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    # add row number for number therapy the patient has been on
    mutate(MED_ORDER = row_number()) %>%
    ungroup() %>%
    # create flag for messed up dates
    mutate(
      date_error_flag = if_else(MED_START_DATE > MED_END_DATE, 1, 0),
      date_error_flag = if_else(is.na(MED_END_DATE), 0, date_error_flag)
    )

  #### ADD SAME START DATE SAME MED FLAG ----

  # some meds double counted for a
  # patient with same start date but only one end date, remove those duplicates and
  # select for the end date available

  table <- table %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    arrange(MED_START_DATE) %>%
    # create flag for patients that have a med that has two rows with the same start date
    mutate(same_start_date_flag = if_else(lead(MEDICATION_NAME) == MEDICATION_NAME &
                                            lead(MED_START_DATE) == MED_START_DATE, 1, 0)) %>%
    mutate(same_start_date_flag = if_else(lag(MEDICATION_NAME) == MEDICATION_NAME &
                                            lag(MED_START_DATE) == MED_START_DATE, 1, same_start_date_flag)) %>%
    mutate(same_start_date_flag = ifelse(is.na(same_start_date_flag), 0, same_start_date_flag)) %>%
    ungroup() %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, same_start_date_flag) %>%
    # if no med end date but the same start date for a med, fill the end date
    fill(MED_END_DATE, .direction = "downup") %>%
    # going to use the later med end date for everything
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE,
             MED_END_DATE,
             .keep_all = T
    ) %>%
    # remake the order of meds with the filtered out duplicates
    ungroup() %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    arrange(MED_START_DATE) %>%
    mutate(MED_ORDER = row_number()) %>%
    ungroup()

  # add med simple to table
  table <- table %>%
    left_join(med_group, by = join_by(MEDICATION_NAME))

  #### FILL MISSING END DATES ----

  # Ids of patients with missing end dates
  missing_ends_ids <- table %>%
    filter(is.na(MED_END_DATE)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE) %>%
    distinct()

  # fill in missing ends if the medication administrated code is no for an ongoing
  # med action concept name
  missing_ends_ongoing_no <- table %>%
    filter(is.na(MED_END_DATE)) %>%
    left_join(
      prescriptions %>% select(
        DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, MEDICATION_NAME,
        MED_ACTION_CONCEPT_NAME, MEDICATION_ADMINISTRATED
      ),
      multiple = "all",
      by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)
    ) %>%
    left_join(encounter, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID)) %>%
    # create flag to filter table down to just encounters that are after med start date
    mutate(flag = if_else(MED_START_DATE > VISIT_ENCOUNTER_START_DATE, 1, 0)) %>%
    filter(flag != 1) %>%
    select(-flag) %>%
    # if the medication is ongoing and medication administrated is no, choose that
    # visit encounter start date as the med end date:
    #### CLB change this logic ----
  # mutate(MED_END_DATE = if_else(MED_ACTION_CONCEPT_NAME == "Ongoing Treatment" &
  #   MEDICATION_ADMINISTRATED == "No", VISIT_ENCOUNTER_START_DATE, NA)) %>%
  filter(!is.na(MED_END_DATE)) %>%
    select(
      DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE, MED_END_DATE,
      MED_ORDER, date_error_flag, same_start_date_flag, MED_GROUP
    ) %>%
    distinct()

  # make the med end date for all other patients the day of their last visit
  # encounter ID

  # create table with last visit encounter
  last_visit_encounter <- encounter %>%
    filter(DATA_SOURCE == "RISK") %>%
    filter(TYPE_OF_ENCOUNTER != "IBD HOSPITALIZATION") %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    slice(which.max(VISIT_ENCOUNTER_END_DATE)) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_START_DATE)

  missing_ends_other <- table %>%
    filter(is.na(MED_END_DATE)) %>%
    anti_join(
      missing_ends_ongoing_no %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME),
      by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME)
    ) %>%
    left_join(last_visit_encounter, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
    # # flag if med is after max visit encounter start date
    mutate(flag = if_else(MED_START_DATE > VISIT_ENCOUNTER_START_DATE, 1, 0)) %>%
    # all flagged patients only have one visit encounter, just use med_start_date
    # as the med_end_date
    mutate(MED_END_DATE = if_else(flag != 1, VISIT_ENCOUNTER_START_DATE,
                                  MED_START_DATE
    )) %>%
    select(
      DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_START_DATE,
      MED_END_DATE, MED_ORDER, date_error_flag, same_start_date_flag, MED_GROUP
    )

  all_missing_med_ends_final <- missing_ends_other %>%
    rbind(missing_ends_ongoing_no) %>%
    rename(MED_END_DATE_new = MED_END_DATE)

  table <- table %>%
    left_join(all_missing_med_ends_final, by = join_by(
      DEIDENTIFIED_MASTER_PATIENT_ID,
      MEDICATION_NAME, MED_START_DATE, MED_ORDER,
      date_error_flag, same_start_date_flag,
      MED_GROUP
    )) %>%
    mutate(MED_END_DATE = if_else(is.na(MED_END_DATE), MED_END_DATE_new, MED_END_DATE)) %>%
    select(-MED_END_DATE_new)

  #### ADD COLUMNS ----

  # add lubridate interval so that you can see if dates fall within an interval
  table <- table %>%
    mutate(INTERVAL = interval(MED_START_DATE, MED_END_DATE)) %>%
    relocate(INTERVAL, .after = MEDICATION_NAME)

  #### OVERLAPPING MEDS ----
  # will only be immunomodulating medications, will not be steroids or antibiotics

  overlapping_medications <- overlapping_meds(table)
  table <- table %>%
    right_join(overlapping_medications, by = join_by(
      DEIDENTIFIED_MASTER_PATIENT_ID,
      MEDICATION_NAME, MED_START_DATE, MED_END_DATE, MED_ORDER,
      INTERVAL
    ))

  #### OVERLAPPING MOA ----

  table <- table %>%
    mutate(
      OVERLAPPING_MOA = gsub("Adalimumab|Certolizumab Pegol", "antiTNF", MEDS_OVERLAP),
      OVERLAPPING_MOA = gsub("Natalizumab", "IRA", OVERLAPPING_MOA),
      OVERLAPPING_MOA = gsub("[()]", "", OVERLAPPING_MOA),
      OVERLAPPING_MOA = gsub("Infliximab Unspecified", "antiTNF", OVERLAPPING_MOA),
      OVERLAPPING_MOA = gsub("Tacrolimus|Methotrexate|Mercaptopurine|Azathioprine", "Immunomodulator", OVERLAPPING_MOA),
      OVERLAPPING_MOA = gsub("Olsalazine|Mesalamine|Sulfasalazine", "5-ASA", OVERLAPPING_MOA)
    ) %>%
    relocate(OVERLAPPING_MOA, .after = MEDS_OVERLAP)

  #### ANTIBIOTICS FLAG ----

  # create table with antibiotics rounds
  antibiotic_rounds <- risk_antibiotic_rounds(prescriptions = prescriptions, encounter = encounter) %>%
    filter(date_flag_error == 0) %>%
    mutate(ANTIBIOTICS_INTERVAL = interval(MED_START_DATE, MED_END_DATE)) %>%
    rename(ANTIBIOTIC_NAME = MEDICATION_NAME)

  # create flag for patient if they were on an antibiotic, filter for if there is
  # an overlap, join to table and then fill with 0s
  antibiotics_flags <- table %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, INTERVAL) %>%
    # right now filter out antibiotics that have a date error flag
    right_join(antibiotic_rounds,
               by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID),
               multiple = "all"
    ) %>%
    mutate(ANTIBIOTICS_OVERLAP = if_else(int_overlaps(INTERVAL, ANTIBIOTICS_INTERVAL), 1, 0)) %>%
    filter(ANTIBIOTICS_OVERLAP == 1) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, INTERVAL, ANTIBIOTICS_OVERLAP) %>%
    distinct()

  # join to table
  table <- table %>%
    left_join(antibiotics_flags, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, INTERVAL)) %>%
    # fill with 0's
    mutate(ANTIBIOTICS_OVERLAP = ifelse(is.na(ANTIBIOTICS_OVERLAP), 0, ANTIBIOTICS_OVERLAP))

  #### STEROIDS FLAG ----

  steroid_rounds <- risk_steroid_rounds(prescriptions = prescriptions, encounter = encounter) %>%
    filter(date_flag_error == 0) %>%
    mutate(STEROID_INTERVAL = interval(MED_START_DATE, MED_END_DATE)) %>%
    rename(STEROID_NAME = MEDICATION_NAME)

  # create flag for patient if they were on an antibiotic, filter for if there is
  # an overlap, join to table and then fill with 0s
  steroid_flags <- table %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, INTERVAL) %>%
    # right now filter out antibiotics that have a date error flag
    right_join(steroid_rounds,
               by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID),
               multiple = "all"
    ) %>%
    mutate(STEROID_OVERLAP = if_else(int_overlaps(INTERVAL, STEROID_INTERVAL), 1, 0)) %>%
    filter(STEROID_OVERLAP == 1) %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, INTERVAL, STEROID_OVERLAP) %>%
    distinct()

  # join to table
  table <- table %>%
    left_join(steroid_flags, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, INTERVAL)) %>%
    # fill with 0's
    mutate(STEROID_OVERLAP = ifelse(is.na(STEROID_OVERLAP), 0, STEROID_OVERLAP))

  # if the end date is the same as the end date for a different interval for
  # that medication for that patient, use most recent visit encounter start date
  # instead (they all are incorrect interval)
  table <- table %>%
    # need to arrange so the second row is the incorrect one
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    mutate(MED_ORDER = row_number()) %>%
    arrange(MED_ORDER, .by_group = T) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE) %>%
    mutate(wrong_date = row_number()) %>%
    left_join(last_visit_encounter, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID)) %>%
    mutate(MED_END_DATE = if_else(wrong_date == 2, VISIT_ENCOUNTER_START_DATE, MED_END_DATE)) %>%
    select(-c(wrong_date, VISIT_ENCOUNTER_START_DATE))

  #### MED END DATE IMPUTED COLUMN

  med_end_date_imputed <- prescriptions %>%
    select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE,
           END_DATE_IMPUTED) %>%
    filter(END_DATE_IMPUTED == 0) %>%
    mutate(MED_END_DATE = dmy(MED_END_DATE))

  #### FINAL ARRANGE ----
  table <- table %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    # remake final MED_ORDER
    select(-MED_ORDER) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    arrange(MED_START_DATE, .by_group = T) %>%
    mutate(MED_ORDER = row_number()) %>%
    arrange(MED_ORDER, .by_group = T) %>%
    ungroup() %>%
    # CLB: DROP THE DATE ERROR FLAG AND SAME START DATE FLAG FOR NOW, USE LATER
    # DROP interval column, same as med start and end
    select(-c(date_error_flag, same_start_date_flag, INTERVAL)) %>%
    # left join the med date imputed and fill in for imputed dates
    left_join(med_end_date_imputed, by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME, MED_END_DATE)) %>%
    mutate(END_DATE_IMPUTED = ifelse(is.na(END_DATE_IMPUTED), 1, END_DATE_IMPUTED))
}


