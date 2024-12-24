#' current_med_dates
#'
#' current_med_dates function for CLB version of SPARC med journey
#'
#'
#' @param medication A dataframe with all medication data created using sparc_med_filter.
#' @param encounter A dataframe with encounter data usually generated using load_data.
#'
#' @return A dataframe with the first medication start date for each drug.

current_med_dates <- function (medication, encounter) {
  current <- medication %>% filter(DATA_SOURCE %in% c("ECRF_SPARC",
                                                      "ECRF")) %>%
    mutate(MED_START_DATE = if_else(year(MED_START_DATE) >  1900,
                                    MED_START_DATE, as.Date(NA, format = "%d-%m-%y")),
           MED_END_DATE = if_else(year(MED_END_DATE) > 1900, MED_END_DATE,  as.Date(NA, format = "%d-%m-%y"))) %>%
    pivot_longer(cols = c(MED_START_DATE,  MED_END_DATE), names_to = "type", values_to = "date") %>%
    arrange(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name,
            DATA_SOURCE, match(type, c("MED_END_DATE", "MED_START_DATE"))) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name,
             DATA_SOURCE) %>% slice(which.max(date)) %>% pivot_wider(names_from = type,
                                                                     values_from = date) %>% ungroup() %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, new_med_name, CURRENT_MEDICATION, VISIT_ENCOUNTER_ID) %>% filter(CURRENT_MEDICATION == "YES") %>%
    rename(MEDICATION = new_med_name, CURRENT_MEDICATION_ECRF = CURRENT_MEDICATION) %>%
    left_join(encounter %>% filter(DATA_SOURCE %in% c("ECRF_SPARC",
                                                      "ECRF")) %>%
                distinct(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, VISIT_ENCOUNTER_START_DATE),
              by = join_by(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID)) %>%
    mutate(CURRENT_MEDICATION = VISIT_ENCOUNTER_START_DATE) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION, CURRENT_MEDICATION)

}

#' overlaps
#'
#' current_med_dates function for CLB version of SPARC med journey
#'
#'
#' @param med A dataframe med created halfway through the sparc med journey function
#'
#' @return A dataframe with the medication overlaps for each drug.

overlap <- function (med) {

  #### CLB Overlaps NEW CODE 12/13/2024----
  ## might need to make this its own function

  library(hablar)

  # find max number of medications, check lead overlap by that number
  num <- med %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(R = row_number()) %>%
    ungroup() %>%
    slice(which.max(R))

  max_num <- num$R

  # initiate blank dataframe

  overlap <- data.frame()

  # create data frame that lists patient id and number of medications, keep all
  # medications and med start and end dates in this dataframe

  pt_med_ct <- med %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    mutate(R = n()) %>%
    # slice(which.max(R)) %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID,MEDICATION, MED_START_DATE, MED_END_DATE, MOA, R) %>%
    ungroup() %>%
    # make med end date Sys.Date() for this purpose
    mutate(MED_END_DATE = if_else(is.na(MED_END_DATE), Sys.Date(), MED_END_DATE)) %>%
    # check that organized in the correct way
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    arrange(MED_START_DATE, .by_group = T)

  # https://www.projectpro.io/recipes/append-output-from-for-loop-dataframe-r

  for (i in 1:13) {

    if (i == 1) {
      med1 <- pt_med_ct %>%
        filter(R == 1) %>%
        mutate(OVERLAPPING_DAYS = 0,
               OVERLAPPING_MOA = NA)
    }

    else if (i == 2) {
      med2 <- pt_med_ct %>%
        filter(R == 2) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
        mutate(int = interval(MED_START_DATE, MED_END_DATE)) %>%
        mutate(OVERLAPPING_DAYS = day(as.period(intersect(int, lead(int, i - 1)), "days"))) %>%
        fill(OVERLAPPING_DAYS, .direction = "down") %>%
        mutate(OVERLAPPING_MOA = NA) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION, MED_START_DATE, MED_END_DATE,
               MOA, R, OVERLAPPING_DAYS,
               OVERLAPPING_MOA)

    }


    else if (i == 3) {

      med3 <- pt_med_ct %>%
        filter(R == 3) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
        mutate(int = interval(MED_START_DATE, MED_END_DATE)) %>%
        mutate(OVERLAPPING_DAYS1 = day(as.period(intersect(int, lead(int, i - (i-1))), "days"))) %>%
        # if not already checked, do lag overlapping days for the same amount.
        # don't know if this will work for more intervals. m means minus
        mutate(OVERLAPPING_DAYS1m = day(as.period(intersect(int, lag(int, i - (i-1))), "days"))) %>%
        mutate(OVERLAPPING_DAYS2 = day(as.period(intersect(int, lead(int, i - 1)), "days"))) %>%

        mutate(OVERLAPPING_DAYS2m = day(as.period(intersect(int, lag(int, i - 1)), "days"))) %>%
        rowwise() %>%
        mutate(OVERLAPPING_DAYS = max(c_across(OVERLAPPING_DAYS1:OVERLAPPING_DAYS2m), na.rm = T)) %>%
        mutate(OVERLAPPING_DAYS = ifelse(OVERLAPPING_DAYS == -Inf, NA, OVERLAPPING_DAYS)) %>%
        mutate(OVERLAPPING_MOA = NA) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION, MED_START_DATE, MED_END_DATE,
               MOA, R, OVERLAPPING_DAYS,
               OVERLAPPING_MOA)

    }

    else if (i == 4) {


      med4 <- pt_med_ct %>%
        filter(R == 4) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
        mutate(int = interval(MED_START_DATE, MED_END_DATE)) %>%
        mutate(OVERLAPPING_DAYS1 = day(as.period(intersect(int, lead(int, i - (i-1))), "days"))) %>%
        # if not already checked, do lag overlapping days for the same amount.
        # don't know if this will work for more intervals. m means minus
        mutate(OVERLAPPING_DAYS1m = day(as.period(intersect(int, lag(int, i - (i-1))), "days"))) %>%
        mutate(OVERLAPPING_DAYS2 = day(as.period(intersect(int, lead(int, i - (i-2))), "days"))) %>%
        mutate(OVERLAPPING_DAYS2m = day(as.period(intersect(int, lag(int, i - (i-2))), "days"))) %>%
        mutate(OVERLAPPING_DAYS3 = day(as.period(intersect(int, lead(int, i - 1)), "days"))) %>%
        mutate(OVERLAPPING_DAYS3m = day(as.period(intersect(int, lag(int, i - 1)), "days"))) %>%
        rowwise() %>%
        mutate(OVERLAPPING_DAYS = max(c_across(OVERLAPPING_DAYS1:OVERLAPPING_DAYS3m), na.rm = T)) %>%
        mutate(OVERLAPPING_DAYS = ifelse(OVERLAPPING_DAYS == -Inf, NA, OVERLAPPING_DAYS)) %>%
        mutate(OVERLAPPING_MOA = NA) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION, MED_START_DATE, MED_END_DATE,
               MOA, R, OVERLAPPING_DAYS,
               OVERLAPPING_MOA)

    }


    else if (i == 5) {

      med5 <- pt_med_ct %>%
        filter(R == 5) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
        mutate(int = interval(MED_START_DATE, MED_END_DATE)) %>%
        mutate(OVERLAPPING_DAYS1 = day(as.period(intersect(int, lead(int, i - (i-1))), "days"))) %>%
        mutate(OVERLAPPING_DAYS1m = day(as.period(intersect(int, lag(int, i - (i-1))), "days"))) %>%
        mutate(OVERLAPPING_DAYS2 = day(as.period(intersect(int, lead(int, i - (i-2))), "days"))) %>%
        mutate(OVERLAPPING_DAYS2m = day(as.period(intersect(int, lag(int, i - (i-2))), "days"))) %>%
        mutate(OVERLAPPING_DAYS3 = day(as.period(intersect(int, lead(int, i - (i-3))), "days"))) %>%
        mutate(OVERLAPPING_DAYS3m = day(as.period(intersect(int, lag(int, i - (i-3))), "days"))) %>%
        mutate(OVERLAPPING_DAYS4 = day(as.period(intersect(int, lead(int, i - 1)), "days"))) %>%
        mutate(OVERLAPPING_DAYS4m = day(as.period(intersect(int, lag(int, i - 1)), "days"))) %>%
        rowwise() %>%
        mutate(OVERLAPPING_DAYS = max(c_across(OVERLAPPING_DAYS1:OVERLAPPING_DAYS4m), na.rm = T)) %>%
        mutate(OVERLAPPING_DAYS = ifelse(OVERLAPPING_DAYS == -Inf, NA, OVERLAPPING_DAYS)) %>%
        mutate(OVERLAPPING_MOA = NA) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION, MED_START_DATE, MED_END_DATE,
               MOA, R, OVERLAPPING_DAYS,
               OVERLAPPING_MOA)
    }

    else if (i == 6) {

      med6 <- pt_med_ct %>%
        filter(R == 6) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
        mutate(int = interval(MED_START_DATE, MED_END_DATE)) %>%
        mutate(OVERLAPPING_DAYS1 = day(as.period(intersect(int, lead(int, i - (i-1))), "days"))) %>%
        mutate(OVERLAPPING_DAYS1m = day(as.period(intersect(int, lag(int, i - (i-1))), "days"))) %>%
        mutate(OVERLAPPING_DAYS2 = day(as.period(intersect(int, lead(int, i - (i-2))), "days"))) %>%
        mutate(OVERLAPPING_DAYS2m = day(as.period(intersect(int, lag(int, i - (i-2))), "days"))) %>%
        mutate(OVERLAPPING_DAYS3 = day(as.period(intersect(int, lead(int, i - (i-3))), "days"))) %>%
        mutate(OVERLAPPING_DAYS3m = day(as.period(intersect(int, lag(int, i - (i-3))), "days"))) %>%
        mutate(OVERLAPPING_DAYS4 = day(as.period(intersect(int, lead(int, i - (i-4))), "days"))) %>%
        mutate(OVERLAPPING_DAYS4m = day(as.period(intersect(int, lag(int, i - (i-4))), "days"))) %>%
        mutate(OVERLAPPING_DAYS5 = day(as.period(intersect(int, lead(int, i - 1)), "days"))) %>%
        mutate(OVERLAPPING_DAYS5m = day(as.period(intersect(int, lag(int, i - 1)), "days"))) %>%
        rowwise() %>%
        mutate(OVERLAPPING_DAYS = max(c_across(OVERLAPPING_DAYS1:OVERLAPPING_DAYS5m), na.rm = T)) %>%
        mutate(OVERLAPPING_DAYS = ifelse(OVERLAPPING_DAYS == -Inf, NA, OVERLAPPING_DAYS)) %>%
        mutate(OVERLAPPING_MOA = NA) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION, MED_START_DATE, MED_END_DATE,
               MOA, R, OVERLAPPING_DAYS,
               OVERLAPPING_MOA)
    }

    else if (i == 7) {

      med7 <- pt_med_ct %>%
        filter(R == 7) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
        mutate(int = interval(MED_START_DATE, MED_END_DATE)) %>%
        mutate(OVERLAPPING_DAYS1 = day(as.period(intersect(int, lead(int, i - (i-1))), "days"))) %>%
        mutate(OVERLAPPING_DAYS1m = day(as.period(intersect(int, lag(int, i - (i-1))), "days"))) %>%
        mutate(OVERLAPPING_DAYS2 = day(as.period(intersect(int, lead(int, i - (i-2))), "days"))) %>%
        mutate(OVERLAPPING_DAYS2m = day(as.period(intersect(int, lag(int, i - (i-2))), "days"))) %>%
        mutate(OVERLAPPING_DAYS3 = day(as.period(intersect(int, lead(int, i - (i-3))), "days"))) %>%
        mutate(OVERLAPPING_DAYS3m = day(as.period(intersect(int, lag(int, i - (i-3))), "days"))) %>%
        mutate(OVERLAPPING_DAYS4 = day(as.period(intersect(int, lead(int, i - (i-4))), "days"))) %>%
        mutate(OVERLAPPING_DAYS4m = day(as.period(intersect(int, lag(int, i - (i-4))), "days"))) %>%
        mutate(OVERLAPPING_DAYS5 = day(as.period(intersect(int, lead(int, i - (i-5))), "days"))) %>%
        mutate(OVERLAPPING_DAYS5m = day(as.period(intersect(int, lag(int, i - (i-5))), "days"))) %>%
        mutate(OVERLAPPING_DAYS6 = day(as.period(intersect(int, lead(int, i - 1)), "days"))) %>%
        mutate(OVERLAPPING_DAYS6m = day(as.period(intersect(int, lag(int, i - 1)), "days"))) %>%
        rowwise() %>%
        mutate(OVERLAPPING_DAYS = max(c_across(OVERLAPPING_DAYS1:OVERLAPPING_DAYS6m), na.rm = T)) %>%
        mutate(OVERLAPPING_DAYS = ifelse(OVERLAPPING_DAYS == -Inf, NA, OVERLAPPING_DAYS)) %>%
        mutate(OVERLAPPING_MOA = NA) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION, MED_START_DATE, MED_END_DATE,
               MOA, R, OVERLAPPING_DAYS,
               OVERLAPPING_MOA)
    }

    else if (i == 8) {

      med8 <- pt_med_ct %>%
        filter(R == 8) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
        mutate(int = interval(MED_START_DATE, MED_END_DATE)) %>%
        mutate(OVERLAPPING_DAYS1 = day(as.period(intersect(int, lead(int, i - (i-1))), "days"))) %>%
        mutate(OVERLAPPING_DAYS1m = day(as.period(intersect(int, lag(int, i - (i-1))), "days"))) %>%
        mutate(OVERLAPPING_DAYS2 = day(as.period(intersect(int, lead(int, i - (i-2))), "days"))) %>%
        mutate(OVERLAPPING_DAYS2m = day(as.period(intersect(int, lag(int, i - (i-2))), "days"))) %>%
        mutate(OVERLAPPING_DAYS3 = day(as.period(intersect(int, lead(int, i - (i-3))), "days"))) %>%
        mutate(OVERLAPPING_DAYS3m = day(as.period(intersect(int, lag(int, i - (i-3))), "days"))) %>%
        mutate(OVERLAPPING_DAYS4 = day(as.period(intersect(int, lead(int, i - (i-4))), "days"))) %>%
        mutate(OVERLAPPING_DAYS4m = day(as.period(intersect(int, lag(int, i - (i-4))), "days"))) %>%
        mutate(OVERLAPPING_DAYS5 = day(as.period(intersect(int, lead(int, i - (i-5))), "days"))) %>%
        mutate(OVERLAPPING_DAYS5m = day(as.period(intersect(int, lag(int, i - (i-5))), "days"))) %>%
        mutate(OVERLAPPING_DAYS6 = day(as.period(intersect(int, lead(int, i - (i-6))), "days"))) %>%
        mutate(OVERLAPPING_DAYS6m = day(as.period(intersect(int, lag(int, i - (i-6))), "days"))) %>%
        mutate(OVERLAPPING_DAYS7 = day(as.period(intersect(int, lead(int, i - 1)), "days"))) %>%
        mutate(OVERLAPPING_DAYS7m = day(as.period(intersect(int, lag(int, i - 1)), "days"))) %>%
        rowwise() %>%
        mutate(OVERLAPPING_DAYS = max(c_across(OVERLAPPING_DAYS1:OVERLAPPING_DAYS7m), na.rm = T)) %>%
        mutate(OVERLAPPING_DAYS = ifelse(OVERLAPPING_DAYS == -Inf, NA, OVERLAPPING_DAYS)) %>%
        mutate(OVERLAPPING_MOA = NA) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION, MED_START_DATE, MED_END_DATE,
               MOA, R, OVERLAPPING_DAYS,
               OVERLAPPING_MOA)
    }

    else if (i == 9) {

      med9 <- pt_med_ct %>%
        filter(R == 9) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
        mutate(int = interval(MED_START_DATE, MED_END_DATE)) %>%
        mutate(OVERLAPPING_DAYS1 = day(as.period(intersect(int, lead(int, i - (i-1))), "days"))) %>%
        mutate(OVERLAPPING_DAYS1m = day(as.period(intersect(int, lag(int, i - (i-1))), "days"))) %>%
        mutate(OVERLAPPING_DAYS2 = day(as.period(intersect(int, lead(int, i - (i-2))), "days"))) %>%
        mutate(OVERLAPPING_DAYS2m = day(as.period(intersect(int, lag(int, i - (i-2))), "days"))) %>%
        mutate(OVERLAPPING_DAYS3 = day(as.period(intersect(int, lead(int, i - (i-3))), "days"))) %>%
        mutate(OVERLAPPING_DAYS3m = day(as.period(intersect(int, lag(int, i - (i-3))), "days"))) %>%
        mutate(OVERLAPPING_DAYS4 = day(as.period(intersect(int, lead(int, i - (i-4))), "days"))) %>%
        mutate(OVERLAPPING_DAYS4m = day(as.period(intersect(int, lag(int, i - (i-4))), "days"))) %>%
        mutate(OVERLAPPING_DAYS5 = day(as.period(intersect(int, lead(int, i - (i-5))), "days"))) %>%
        mutate(OVERLAPPING_DAYS5m = day(as.period(intersect(int, lag(int, i - (i-5))), "days"))) %>%
        mutate(OVERLAPPING_DAYS6 = day(as.period(intersect(int, lead(int, i - (i-6))), "days"))) %>%
        mutate(OVERLAPPING_DAYS6m = day(as.period(intersect(int, lag(int, i - (i-6))), "days"))) %>%
        mutate(OVERLAPPING_DAYS7 = day(as.period(intersect(int, lead(int, i - (i-7))), "days"))) %>%
        mutate(OVERLAPPING_DAYS7m = day(as.period(intersect(int, lag(int, i - (i-7))), "days"))) %>%
        mutate(OVERLAPPING_DAYS8 = day(as.period(intersect(int, lead(int, i - 1)), "days"))) %>%
        mutate(OVERLAPPING_DAYS8m = day(as.period(intersect(int, lag(int, i - 1)), "days"))) %>%
        rowwise() %>%
        mutate(OVERLAPPING_DAYS = max(c_across(OVERLAPPING_DAYS1:OVERLAPPING_DAYS8m), na.rm = T)) %>%
        mutate(OVERLAPPING_DAYS = ifelse(OVERLAPPING_DAYS == -Inf, NA, OVERLAPPING_DAYS)) %>%
        mutate(OVERLAPPING_MOA = NA) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION, MED_START_DATE, MED_END_DATE,
               MOA, R, OVERLAPPING_DAYS,
               OVERLAPPING_MOA)
    }

    else if (i == 10) {

      med10 <- pt_med_ct %>%
        filter(R == 10) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
        mutate(int = interval(MED_START_DATE, MED_END_DATE)) %>%
        mutate(OVERLAPPING_DAYS1 = day(as.period(intersect(int, lead(int, i - (i-1))), "days"))) %>%
        mutate(OVERLAPPING_DAYS1m = day(as.period(intersect(int, lag(int, i - (i-1))), "days"))) %>%
        mutate(OVERLAPPING_DAYS2 = day(as.period(intersect(int, lead(int, i - (i-2))), "days"))) %>%
        mutate(OVERLAPPING_DAYS2m = day(as.period(intersect(int, lag(int, i - (i-2))), "days"))) %>%
        mutate(OVERLAPPING_DAYS3 = day(as.period(intersect(int, lead(int, i - (i-3))), "days"))) %>%
        mutate(OVERLAPPING_DAYS3m = day(as.period(intersect(int, lag(int, i - (i-3))), "days"))) %>%
        mutate(OVERLAPPING_DAYS4 = day(as.period(intersect(int, lead(int, i - (i-4))), "days"))) %>%
        mutate(OVERLAPPING_DAYS4m = day(as.period(intersect(int, lag(int, i - (i-4))), "days"))) %>%
        mutate(OVERLAPPING_DAYS5 = day(as.period(intersect(int, lead(int, i - (i-5))), "days"))) %>%
        mutate(OVERLAPPING_DAYS5m = day(as.period(intersect(int, lag(int, i - (i-5))), "days"))) %>%
        mutate(OVERLAPPING_DAYS6 = day(as.period(intersect(int, lead(int, i - (i-6))), "days"))) %>%
        mutate(OVERLAPPING_DAYS6m = day(as.period(intersect(int, lag(int, i - (i-6))), "days"))) %>%
        mutate(OVERLAPPING_DAYS7 = day(as.period(intersect(int, lead(int, i - (i-7))), "days"))) %>%
        mutate(OVERLAPPING_DAYS7m = day(as.period(intersect(int, lag(int, i - (i-7))), "days"))) %>%
        mutate(OVERLAPPING_DAYS8 = day(as.period(intersect(int, lead(int, i - (i-8))), "days"))) %>%
        mutate(OVERLAPPING_DAYS8m = day(as.period(intersect(int, lag(int, i - (i-8))), "days"))) %>%
        mutate(OVERLAPPING_DAYS9 = day(as.period(intersect(int, lead(int, i - 1)), "days"))) %>%
        mutate(OVERLAPPING_DAYS9m = day(as.period(intersect(int, lag(int, i - 1)), "days"))) %>%
        rowwise() %>%
        mutate(OVERLAPPING_DAYS = max(c_across(OVERLAPPING_DAYS1:OVERLAPPING_DAYS9m), na.rm = T)) %>%
        mutate(OVERLAPPING_DAYS = ifelse(OVERLAPPING_DAYS == -Inf, NA, OVERLAPPING_DAYS)) %>%
        mutate(OVERLAPPING_MOA = NA) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION, MED_START_DATE, MED_END_DATE,
               MOA, R, OVERLAPPING_DAYS,
               OVERLAPPING_MOA)
    }

    else if (i == 11) {

      med11 <- pt_med_ct %>%
        filter(R == 11) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
        mutate(int = interval(MED_START_DATE, MED_END_DATE)) %>%
        mutate(OVERLAPPING_DAYS1 = day(as.period(intersect(int, lead(int, i - (i-1))), "days"))) %>%
        mutate(OVERLAPPING_DAYS1m = day(as.period(intersect(int, lag(int, i - (i-1))), "days"))) %>%
        mutate(OVERLAPPING_DAYS2 = day(as.period(intersect(int, lead(int, i - (i-2))), "days"))) %>%
        mutate(OVERLAPPING_DAYS2m = day(as.period(intersect(int, lag(int, i - (i-2))), "days"))) %>%
        mutate(OVERLAPPING_DAYS3 = day(as.period(intersect(int, lead(int, i - (i-3))), "days"))) %>%
        mutate(OVERLAPPING_DAYS3m = day(as.period(intersect(int, lag(int, i - (i-3))), "days"))) %>%
        mutate(OVERLAPPING_DAYS4 = day(as.period(intersect(int, lead(int, i - (i-4))), "days"))) %>%
        mutate(OVERLAPPING_DAYS4m = day(as.period(intersect(int, lag(int, i - (i-4))), "days"))) %>%
        mutate(OVERLAPPING_DAYS5 = day(as.period(intersect(int, lead(int, i - (i-5))), "days"))) %>%
        mutate(OVERLAPPING_DAYS5m = day(as.period(intersect(int, lag(int, i - (i-5))), "days"))) %>%
        mutate(OVERLAPPING_DAYS6 = day(as.period(intersect(int, lead(int, i - (i-6))), "days"))) %>%
        mutate(OVERLAPPING_DAYS6m = day(as.period(intersect(int, lag(int, i - (i-6))), "days"))) %>%
        mutate(OVERLAPPING_DAYS7 = day(as.period(intersect(int, lead(int, i - (i-7))), "days"))) %>%
        mutate(OVERLAPPING_DAYS7m = day(as.period(intersect(int, lag(int, i - (i-7))), "days"))) %>%
        mutate(OVERLAPPING_DAYS8 = day(as.period(intersect(int, lead(int, i - (i-8))), "days"))) %>%
        mutate(OVERLAPPING_DAYS8m = day(as.period(intersect(int, lag(int, i - (i-8))), "days"))) %>%
        mutate(OVERLAPPING_DAYS9 = day(as.period(intersect(int, lead(int, i - (i-9))), "days"))) %>%
        mutate(OVERLAPPING_DAYS9m = day(as.period(intersect(int, lag(int, i - (i-9))), "days"))) %>%
        mutate(OVERLAPPING_DAYS10 = day(as.period(intersect(int, lead(int, i - 1)), "days"))) %>%
        mutate(OVERLAPPING_DAYS10m = day(as.period(intersect(int, lag(int, i - 1)), "days"))) %>%
        rowwise() %>%
        mutate(OVERLAPPING_DAYS = max(c_across(OVERLAPPING_DAYS1:OVERLAPPING_DAYS10m), na.rm = T)) %>%
        mutate(OVERLAPPING_DAYS = ifelse(OVERLAPPING_DAYS == -Inf, NA, OVERLAPPING_DAYS)) %>%
        mutate(OVERLAPPING_MOA = NA) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION, MED_START_DATE, MED_END_DATE,
               MOA, R, OVERLAPPING_DAYS,
               OVERLAPPING_MOA)
    }

    else if (i == 12) {

      med12 <- pt_med_ct %>%
        filter(R == 12) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
        mutate(int = interval(MED_START_DATE, MED_END_DATE)) %>%
        mutate(OVERLAPPING_DAYS1 = day(as.period(intersect(int, lead(int, i - (i-1))), "days"))) %>%
        mutate(OVERLAPPING_DAYS1m = day(as.period(intersect(int, lag(int, i - (i-1))), "days"))) %>%
        mutate(OVERLAPPING_DAYS2 = day(as.period(intersect(int, lead(int, i - (i-2))), "days"))) %>%
        mutate(OVERLAPPING_DAYS2m = day(as.period(intersect(int, lag(int, i - (i-2))), "days"))) %>%
        mutate(OVERLAPPING_DAYS3 = day(as.period(intersect(int, lead(int, i - (i-3))), "days"))) %>%
        mutate(OVERLAPPING_DAYS3m = day(as.period(intersect(int, lag(int, i - (i-3))), "days"))) %>%
        mutate(OVERLAPPING_DAYS4 = day(as.period(intersect(int, lead(int, i - (i-4))), "days"))) %>%
        mutate(OVERLAPPING_DAYS4m = day(as.period(intersect(int, lag(int, i - (i-4))), "days"))) %>%
        mutate(OVERLAPPING_DAYS5 = day(as.period(intersect(int, lead(int, i - (i-5))), "days"))) %>%
        mutate(OVERLAPPING_DAYS5m = day(as.period(intersect(int, lag(int, i - (i-5))), "days"))) %>%
        mutate(OVERLAPPING_DAYS6 = day(as.period(intersect(int, lead(int, i - (i-6))), "days"))) %>%
        mutate(OVERLAPPING_DAYS6m = day(as.period(intersect(int, lag(int, i - (i-6))), "days"))) %>%
        mutate(OVERLAPPING_DAYS7 = day(as.period(intersect(int, lead(int, i - (i-7))), "days"))) %>%
        mutate(OVERLAPPING_DAYS7m = day(as.period(intersect(int, lag(int, i - (i-7))), "days"))) %>%
        mutate(OVERLAPPING_DAYS8 = day(as.period(intersect(int, lead(int, i - (i-8))), "days"))) %>%
        mutate(OVERLAPPING_DAYS8m = day(as.period(intersect(int, lag(int, i - (i-8))), "days"))) %>%
        mutate(OVERLAPPING_DAYS9 = day(as.period(intersect(int, lead(int, i - (i-9))), "days"))) %>%
        mutate(OVERLAPPING_DAYS9m = day(as.period(intersect(int, lag(int, i - (i-9))), "days"))) %>%
        mutate(OVERLAPPING_DAYS10 = day(as.period(intersect(int, lead(int, i - (i-10))), "days"))) %>%
        mutate(OVERLAPPING_DAYS10m = day(as.period(intersect(int, lag(int, i - (i-10))), "days"))) %>%
        mutate(OVERLAPPING_DAYS11 = day(as.period(intersect(int, lead(int, i - 1)), "days"))) %>%
        mutate(OVERLAPPING_DAYS11m = day(as.period(intersect(int, lag(int, i - 1)), "days"))) %>%
        rowwise() %>%
        mutate(OVERLAPPING_DAYS = max(c_across(OVERLAPPING_DAYS1:OVERLAPPING_DAYS11m), na.rm = T)) %>%
        mutate(OVERLAPPING_DAYS = ifelse(OVERLAPPING_DAYS == -Inf, NA, OVERLAPPING_DAYS)) %>%
        mutate(OVERLAPPING_MOA = NA) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION, MED_START_DATE, MED_END_DATE,
               MOA, R, OVERLAPPING_DAYS,
               OVERLAPPING_MOA)
    }

    else if (i == 13) {

      med13 <- pt_med_ct %>%
        filter(R == 13) %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
        mutate(int = interval(MED_START_DATE, MED_END_DATE)) %>%
        mutate(OVERLAPPING_DAYS1 = day(as.period(intersect(int, lead(int, i - (i-1))), "days"))) %>%
        mutate(OVERLAPPING_DAYS1m = day(as.period(intersect(int, lag(int, i - (i-1))), "days"))) %>%
        mutate(OVERLAPPING_DAYS2 = day(as.period(intersect(int, lead(int, i - (i-2))), "days"))) %>%
        mutate(OVERLAPPING_DAYS2m = day(as.period(intersect(int, lag(int, i - (i-2))), "days"))) %>%
        mutate(OVERLAPPING_DAYS3 = day(as.period(intersect(int, lead(int, i - (i-3))), "days"))) %>%
        mutate(OVERLAPPING_DAYS3m = day(as.period(intersect(int, lag(int, i - (i-3))), "days"))) %>%
        mutate(OVERLAPPING_DAYS4 = day(as.period(intersect(int, lead(int, i - (i-4))), "days"))) %>%
        mutate(OVERLAPPING_DAYS4m = day(as.period(intersect(int, lag(int, i - (i-4))), "days"))) %>%
        mutate(OVERLAPPING_DAYS5 = day(as.period(intersect(int, lead(int, i - (i-5))), "days"))) %>%
        mutate(OVERLAPPING_DAYS5m = day(as.period(intersect(int, lag(int, i - (i-5))), "days"))) %>%
        mutate(OVERLAPPING_DAYS6 = day(as.period(intersect(int, lead(int, i - (i-6))), "days"))) %>%
        mutate(OVERLAPPING_DAYS6m = day(as.period(intersect(int, lag(int, i - (i-6))), "days"))) %>%
        mutate(OVERLAPPING_DAYS7 = day(as.period(intersect(int, lead(int, i - (i-7))), "days"))) %>%
        mutate(OVERLAPPING_DAYS7m = day(as.period(intersect(int, lag(int, i - (i-7))), "days"))) %>%
        mutate(OVERLAPPING_DAYS8 = day(as.period(intersect(int, lead(int, i - (i-8))), "days"))) %>%
        mutate(OVERLAPPING_DAYS8m = day(as.period(intersect(int, lag(int, i - (i-8))), "days"))) %>%
        mutate(OVERLAPPING_DAYS9 = day(as.period(intersect(int, lead(int, i - (i-9))), "days"))) %>%
        mutate(OVERLAPPING_DAYS9m = day(as.period(intersect(int, lag(int, i - (i-9))), "days"))) %>%
        mutate(OVERLAPPING_DAYS10 = day(as.period(intersect(int, lead(int, i - (i-10))), "days"))) %>%
        mutate(OVERLAPPING_DAYS10m = day(as.period(intersect(int, lag(int, i - (i-10))), "days"))) %>%
        mutate(OVERLAPPING_DAYS11 = day(as.period(intersect(int, lead(int, i - (i-11))), "days"))) %>%
        mutate(OVERLAPPING_DAYS11m = day(as.period(intersect(int, lag(int, i - (i-11))), "days"))) %>%
        mutate(OVERLAPPING_DAYS12 = day(as.period(intersect(int, lead(int, i - 1)), "days"))) %>%
        mutate(OVERLAPPING_DAYS12m = day(as.period(intersect(int, lag(int, i - 1)), "days"))) %>%
        rowwise() %>%
        mutate(OVERLAPPING_DAYS = max(c_across(OVERLAPPING_DAYS1:OVERLAPPING_DAYS12m), na.rm = T)) %>%
        mutate(OVERLAPPING_DAYS = ifelse(OVERLAPPING_DAYS == -Inf, NA, OVERLAPPING_DAYS)) %>%
        mutate(OVERLAPPING_MOA = NA) %>%
        select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION, MED_START_DATE, MED_END_DATE,
               MOA, R, OVERLAPPING_DAYS,
               OVERLAPPING_MOA)
    }

  }



  #### IDEALLY IN FUTURE MAKE THIS CODE ITERATIVE AND NOT SO LONG, AND ABLE TO
  #### HANDLE ANY NUMBER OF rows per patient, need time to do this -

  # # Repeat for i == 6 to i == 13
  # for (j in 6:7) {
  #   eval(parse(text = paste0(
  #     "else if (i == ", j, ") {",
  #     "\n  med", j, " <- pt_med_ct %>%",
  #     "\n    filter(R == ", j, ") %>%",
  #     "\n    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%",
  #     "\n    mutate(int = interval(MED_START_DATE, MED_END_DATE)) %>%"
  #   )))
  #
  # # Generate overlapping day columns dynamically
  #   for (k in 1:j) {
  #     eval(parse(text = paste0(
  #       "  mutate(OVERLAPPING_DAYS", k, " = day(as.period(intersect(int, ",
  #       ifelse(k == 1, "lead", "lag"), "(int, ", j - (k-1), "))), \"days\"))) %>%"
  #     )))
  #   }
  #
  #   # Add closing operations
  #   eval(parse(text = paste0(
  #     "rowwise() %>%\n",
  #     "  mutate(OVERLAPPING_DAYS = max(c_across(OVERLAPPING_DAYS1:OVERLAPPING_DAYS", j, "m), na.rm = T)) %>%\n",
  #     "  mutate(OVERLAPPING_DAYS = ifelse(OVERLAPPING_DAYS == -Inf, NA, OVERLAPPING_DAYS)) %>%\n",
  #     "  mutate(OVERLAPPING_MOA = NA) %>%\n",
  #     "  select(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION, MED_START_DATE, MED_END_DATE, MOA, R, OVERLAPPING_DAYS, OVERLAPPING_MOA)"
  #   )))
  #   eval(parse(text = "}"))
  # }

  overlap <- med1 %>%
    rbind(med2) %>%
    rbind(med3) %>%
    rbind(med4) %>%
    rbind(med5) %>%
    rbind(med6) %>%
    rbind(med7) %>%
    rbind(med8) %>%
    rbind(med9) %>%
    rbind(med10) %>%
    rbind(med11) %>%
    rbind(med12) %>%
    rbind(med13) %>%
    select(-MOA) %>%
    mutate(MED_END_DATE = if_else(MED_END_DATE == Sys.Date(), NA, MED_END_DATE))

  return(overlap)

}
