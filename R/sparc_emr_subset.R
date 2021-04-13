#' sparc_emr
#'
#' Subsets EMR data for specific EMR codes and provides spreadsheet with encounter or hist id
#'
#' @param datadir directory where unzipped data is located.
#' @param filename the name of the output file. Must be .xlsx.
#' @param emr_codes the codes to search through in the extract
#'
#' @return A dataframe with the summary data and an excel file. If EMR codes are specified then a subset of those data are created for each domain where the code was found.
#' @export
sparc_emr <- function(datadir, filename = "SPARC_EMR.xlsx", emr_codes = NULL){


# LOAD FILES ----

  # GET FILES OF MOST RECENT DATA ----
  data = load_data(datadir = datadir, cohort = "SPARC", domains = "ALL", data_type = "BOTH")


# DEMOGRAPHIC INFORMATION ----

  demo = data$demographics %>%
    filter(DATA_SOURCE == "ECRF_SPARC") %>%
    distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DATE_OF_CONSENT, DATE_OF_CONSENT_WITHDRAWN, BIRTH_YEAR, GENDER) %>%
    mutate(DATE_OF_CONSENT = dmy(DATE_OF_CONSENT)) %>%
    filter(year(DATE_OF_CONSENT) >= 2016) %>%
    group_by(DEIDENTIFIED_MASTER_PATIENT_ID) %>%
    slice(which.min(DATE_OF_CONSENT)) %>%
    ungroup()



      #DATA FROM EMR - encounter ids for where variable is reported for master report


        emr = NULL

        for (i in 1:length(data)){
          if("DATA_SOURCE" %in% names(data[[i]])){k = data[[i]] %>% filter(DATA_SOURCE == "EMR")} else{k = NULL}

          emr[[i]] = k}

        names(emr) = names(data)

        emr$patient_history_old = NULL

        emr = Filter(length, emr)
        emr = emr[sapply(emr, nrow)>0]

        emr.set = as.vector(emr_codes)

        e = demo %>% distinct(DEIDENTIFIED_MASTER_PATIENT_ID)

        for (j in 1:length(emr.set)){

          code = emr.set[j]

          f=NULL

          for (k in 1:length(emr)){
            keep = emr[[k]] %>%
              right_join(e)

            #keep$c = apply(keep, 1, function(r) any(r %in% c(code)))

            keep$c =   apply(keep %>% select(contains("CODE")), 1, function(r) any(r %in% c(code)))

            keep = keep %>%  filter(c == 1)

            f[[k]] = keep
          }

          rm(keep)

          names(f) = names(emr)

          f = Filter(length, f)
          f = f[sapply(f, nrow)>0]

          g = NULL

          if(length(f) != 0){
          for(m in 1:length(f)){

            if("patient_history" %in% names(f[m])){

              keep = f[[m]] %>%
                filter(HISTORY_TYPE != "FAMILY") %>%
                mutate(VAR = ifelse(is.na(DIAGNOSIS_HISTORY_CONCEPT_NAME), paste0("PATIENT_HISTORY_", MED_HISTORY_CONCEPT_NAME), paste0("PATIENT_HISTORY_", DIAGNOSIS_HISTORY_CONCEPT_NAME))) %>%
                mutate(RESPONSE = "Patient History") %>%
                arrange(DEIDENTIFIED_MASTER_PATIENT_ID, VAR, RESPONSE) %>%
                group_by(DEIDENTIFIED_MASTER_PATIENT_ID,VAR) %>%
                ungroup() %>%
                pivot_wider(id_cols = c(DEIDENTIFIED_MASTER_PATIENT_ID), names_from = VAR, values_from = HIST_ID)


            } else if("prescriptions" %in% names(f[m])){

              keep = f[[m]] %>%
                #mutate(diff = dmy(MED_START_DATE) - index_date) %>%
                distinct(DEIDENTIFIED_MASTER_PATIENT_ID, MEDICATION_NAME,VISIT_ENCOUNTER_ID, MED_START_DATE, MED_END_DATE) %>%
                ungroup() %>%
                mutate(VAR = paste0("Prescriptions_", MEDICATION_NAME)) %>%
                reshape2::dcast(DEIDENTIFIED_MASTER_PATIENT_ID  ~ VAR ,
                      value.var = "VISIT_ENCOUNTER_ID",  fun.aggregate = function(x) paste(x, collapse = "; "))



            } else if("diagnosis" %in% names(f[m])){

              keep = f[[m]] %>%
                distinct(DEIDENTIFIED_MASTER_PATIENT_ID, DIAG_CONCEPT_NAME,VISIT_ENCOUNTER_ID) %>%
                ungroup() %>%
                mutate(VAR = paste0("DIAGNOSIS_", DIAG_CONCEPT_NAME)) %>%
                reshape2::dcast(DEIDENTIFIED_MASTER_PATIENT_ID  ~ VAR ,
                      value.var = "VISIT_ENCOUNTER_ID",  fun.aggregate = function(x) paste(x, collapse = "; "))

            }else if("patient_problem" %in% names(f[m])){

              keep = f[[m]] %>%
                distinct(DEIDENTIFIED_MASTER_PATIENT_ID, PROB_CONCEPT_NAME,VISIT_ENCOUNTER_ID) %>%
                ungroup() %>%
                mutate(VAR = paste0("Patient_Problem_", PROB_CONCEPT_NAME)) %>%
                reshape2::dcast(DEIDENTIFIED_MASTER_PATIENT_ID ~ VAR ,
                      value.var = "VISIT_ENCOUNTER_ID",  fun.aggregate = function(x) paste(x, collapse = "; "))
              }else if("labs" %in% names(f[m])){

              keep = f[[m]] %>%
                distinct(DEIDENTIFIED_MASTER_PATIENT_ID, LAB_TEST_CONCEPT_NAME,VISIT_ENCOUNTER_ID) %>%
                ungroup() %>%
                mutate(VAR = paste0("Labs_LOINC_", code)) %>%
                reshape2::dcast(DEIDENTIFIED_MASTER_PATIENT_ID  ~ VAR ,
                      value.var = "VISIT_ENCOUNTER_ID",  fun.aggregate = function(x) paste(x, collapse = "; "))
              }else if("procedures" %in% names(f[m])){

                        keep = f[[m]] %>%
                          distinct(DEIDENTIFIED_MASTER_PATIENT_ID, PROC_CONCEPT_NAME,VISIT_ENCOUNTER_ID) %>%
                          ungroup() %>%
                          mutate(VAR = paste0("Procedures_CPT_", code)) %>%
                          reshape2::dcast(DEIDENTIFIED_MASTER_PATIENT_ID  ~ VAR ,
                                          value.var = "VISIT_ENCOUNTER_ID",  fun.aggregate = function(x) paste(x, collapse = "; "))}

            g[[m]] = keep


          }

          g = reduce(g, full_join)

          demo = left_join(demo, g)}

        }




      #CREATE EMR SUBSET OF DATA FOR THESE ENCOUNTERS


        emr_subset = NULL

        for (i in 1:length(data)){
          if("DATA_SOURCE" %in% names(data[[i]])){
            k = data[[i]] %>% filter(DATA_SOURCE == "EMR")

            k$c =   apply(k %>% select(contains("CODE")), 1, function(r) any(r %in% emr_codes))

            k = k %>%  filter(c == 1)



          } else{k = NULL}

          emr_subset[[i]] = k}

        names(emr_subset) = names(data)

        emr_subset$patient_history_old = NULL

        emr_subset = Filter(length, emr_subset)
        emr_subset = emr_subset[sapply(emr_subset, nrow)>0]


        for(j in 1:length(emr_subset)){

          keep = emr_subset[[j]]

          write.csv(keep, file = paste0(names(emr_subset[j]),".csv"), na = "", row.names = F)

        }







# FORMAT COLUMN NAMES  ----

  demo <- demo %>% arrange(DEIDENTIFIED_MASTER_PATIENT_ID)


  names(demo) = toupper(names(demo))


# CREATE HEADER STYLES ----

  #blue
  style1 <- createStyle(bgFill = "#BDD7EE", textDecoration = "bold")

  #orange
  style2 <- createStyle(bgFill="#F8CBAD", textDecoration = "bold")

  #yellow
  style3 <- createStyle(bgFill="#FFE699", textDecoration = "bold")

  #green
  style4 <-  createStyle(bgFill="#C6E0B4")

  #grey
  style5 <-  createStyle(bgFill="#D9D9D9")

  #red
  style6 <- createStyle(bgFill="#e84135")


# CREATE WORKBOOK ----

  wb <- createWorkbook()
  addWorksheet(wb, "sparc_emr")
  writeData(wb, "sparc_emr", x=cohort, startCol=1, startRow=1, colNames=TRUE, rowNames=FALSE)



# SAVE REPORT ----

saveWorkbook(wb, file = paste0(filename), overwrite = TRUE)

return(demo)
}
