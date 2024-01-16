#' custom_exclusions
#'
#' A function that creates custom exclusion categories of ICD 10 codes for extract_emr_diagnosis_function. Add new custom exclusion categories requested to this function.
#'
#' @param emr_filtered A data frame of filtered emr data produced in extract_emr_diagnosis (either dxemrid, ppemricd, phemricd)
#' @param exclusion1 diagnosis category of ICD 10 codes to exclude from search. All uppercase.
#' @param datecutoff Type of date cutoff to be used for EMR extract. Current options include "ENROLLMENT" - number of days before consent into study.
#' @param source_column The source code column relevant for the diagnosis, patient_problem or patient_history table.
#'
#' @return The filtered data frames of emr data without any diagnosis that are in the exclusion category.
custom_exclusions <- function(emr_filtered,
                              exclusion1 = NULL,
                              datecutoff = NULL,
                              source_column = NULL){
  if ("SKIN CARCINOMA" %in% exclusion1) {
    exc <- "C44.01|C44.02|C44.11|C44.12|C44.21|C44.22|C44.31|C44.32|C44.41|C44.42|C44.51|C44.52|C44.61|C44.62|C44.71|C44.72|C44.81|C44.82|C44.91|C44.92"
    emr_filtered1 <- emr_filtered
    emr_filtered <- emr_filtered1 %>% filter(!grepl(exc, source_column,
                                                    ignore.case = TRUE))
  }
  if ("CERVIX CARCINOMA" %in% exclusion1) {
    exc <- "d06"
    emr_filtered1 <- emr_filtered
    emr_filtered <- emr_filtered1 %>% filter(!grepl(exc, source_column,
                                                    ignore.case = TRUE))
  }
  if ("CUSTOM" %in% exclusion1) {
    exc <- customexc
    emr_filtered1 <- emr_filtered
    emr_filtered <- emr_filtered1 %>% filter(!grepl(exc, source_column,
                                                    ignore.case = TRUE))
  }
  if ("ENROLLMENT" %in% datecutoff) {
    emr_filtered2 <- emr_filtered
    emr_filtered <- data$demographics %>% extract_consent("SPARC") %>%
      mutate(dateconsentcalc = as.Date(DATE_OF_CONSENT,
                                       format = "%d-%B-%Y")) %>% mutate(earliestemr = dateconsentcalc -
                                                                          index_range) %>% merge(emr_filtered2, by = "DEIDENTIFIED_MASTER_PATIENT_ID",
                                                                                                 allow.cartesian = TRUE) %>% mutate(dateforfilter = as.Date(PROBLEM_START_DATE,
                                                                                                                                                            format = "%d-%B-%Y")) %>% filter(earliestemr <=
                                                                                                                                                                                               dateforfilter & dateforfilter <= dateconsentcalc)

  }
  return(emr_filtered)
}

#' return_tables
#'
#' A function that returns the filtered dataframes that were imputed into extract_emr_diagnosis function. Can be any combination of diagnosis, patient_problem and/or patient_history.
#'
#' @param data The data frame that is inputted into the emr_extract_diagnosis function
#' @param dxemricd filtered diagnosis data frame from emr_extract_diagnosis function
#' @param ppemricd filtered patient_problem data frame from emr_extract_diagnosis function
#' @param phemricd filtered patient_history data frame from emr_extract_diagnosis function
#'
#' @return a list of the data frames available that are filtered using the emr_extract_diagnosis function
#'
return_tables <- function(data,
                          dxemricd = NULL,
                          ppemricd = NULL,
                          phemricd = NULL){

  if ("diagnosis" %in% names(data) & !"patient_problem" %in%
      names(data) & !"patient_history" %in% names(data)) {
    result <- list(dxemricd)
    names(result) <- c("diagnosis")
  }
  if ("patient_problem" %in% names(data) & !"diagnosis" %in%
      names(data) & !"patient_history" %in% names(data)) {
    result <- list(ppemricd)
    names(result) <- c("patient_problem")
  }
  if ("patient_problem" %in% names(data) & "diagnosis" %in%
      names(data) & !"patient_history" %in% names(data)) {
    result <- list(dxemricd, ppemricd)
    names(result) <- c("diagnosis", "patient_problem")
  }
  if ("diagnosis" %in% names(data) & !"patient_problem" %in%
      names(data) & "patient_history" %in% names(data)) {
    result <- list(dxemricd, phemricd)
    names(result) <- c("diagnosis", "patient_history")
  }
  if ("patient_problem" %in% names(data) & !"diagnosis" %in%
      names(data) & "patient_history" %in% names(data)) {
    result <- list(ppemricd, phemricd)
    names(result) <- c("patient_problem", "patient_history")
  }
  if ("patient_problem" %in% names(data) & "diagnosis" %in%
      names(data) & "patient_history" %in% names(data)) {
    result <- list(dxemricd, ppemricd, phemricd)
    names(result) <- c("diagnosis", "patient_problem", "patient_history")
  }
  if (!"patient_problem" %in% names(data) & !"diagnosis" %in%
      names(data) & "patient_history" %in% names(data)) {
    result <- list(phemricd)
    names(result) <- c("patient_history")
  }
  return(result)
}


#' emr_extract_diagnosis
#'
#' A function to extract relevant diagnosis codes from
#' diagnosis and/or patient problem EMR data for a specific condition of
#' interest.
#'
#' @param data A list of dataframes, typically generated by a call to load_data. Must include demographics, and the diagnosis AND/OR patient problem tables. Only EMR data needs to be uploaded.
#' @param code_search Option to extract relevant diagnosis codes using only ICD 10 codes. Defaults to true.
#' @param text_search Option to extract relevant diagnosis codes using keyword text search. Defaults to false.
#' @param inclusion When searching by code, the diagnosis category of ICD 10 codes to search for. Can choose between predefined categories (included codes are detailed in Diagnosis_EMR_Extract.Rd) or enter a custom defined list. When text searching, a regular expression string of keywords to search for (eg. "argument1|argument2|argument3")
#' @param exclusion diagnosis category of ICD 10 codes to exclude from search. Will be input as custom list, may add predefined lists in future updates.
#' @param custominc optional list of custom ICD 10 codes to be searched for. The variable "inclusion" must also be set to "CUSTOM" for this option. If multiple inputs, needs to be in a regular expression string (eg. "argument1|argument2|argument3")
#' @param customexc optional list of custom ICD 10 codes to be excluded from the search. The variable "exclusion" must also be set to "CUSTOM" for this option. If multiple inputs, needs to be in a regular expression string (eg. "argument1|argument2|argument3")
#' @param datecutoff Type of date cutoff to be used for EMR extract. Current options include "ENROLLMENT" - number of days before consent into study.
#' @param index_range Number of days before date cutoff to be used for EMR extract. Time must be specified in number of days. Default is 36500 days (100 years).
#'
#' @return A list of dataframes with all relevant EMR data.
#' @export
#'
#' @examples
#'
#' # example with predefined category (all cancer diagnoses excluding skin carcinomas)
#'
#' # cancerdx_woutskin = diagnosis_emr_extract(data, inclusion="CANCER", exclusion = "SKIN CARCINOMA")
#'
#' # example with custom category (all cancer diagnoses excluding cervical carcinomas)
#' #example with predefined category
#'
#' #cancerdx_woutskin = diagnosis_emr_extract(data, inclusion="CANCER", exclusion = "SKIN CARCINOMA")
#'
#' #example with custom category
#'
#' #cancerdx_woutcervical=emr_extract_diagnosis(data,inclusion="CUSTOM", exclusion = "CUSTOM",custominc="c|d0|d1|d2|d3|d4",customexc="d06")
#'
#' #example of text search only
#' #textonly_abscess <- emr_extract_diagnosis(data, text_search = T, code_search = F, inclusion = "abscess")

emr_extract_diagnosis <- function(data,
                                      text_search = F,
                                      code_search = T,
                                      inclusion = NULL,
                                      exclusion = NULL,
                                      custominc = NULL,
                                      customexc = NULL,
                                      datecutoff = NULL,
                                      index_range = 36500) {

  inclusion1 <- toupper(inclusion)
  exclusion1 <- toupper(exclusion)
  index_range <- as.numeric(index_range)

  ## CHECK TO MAKE SURE CUSTOM INCLUSION IS A CODE NOT A WORD?
  # forgot why I started this but leaving for now in case I remember

  # if (str_detect(custominc, "[0-9]")) {
  #
  # } else {
  #
  #
  #   }

  # to do - add option to search for a specific code character and an unspecified keyword in the same search
  list_of_inclusion_categories <- c("SYSTEMIC FUNGAL INFECTION", "CANCER", "COLORECTAL CANCER", "WEIGHT LOSS",
                                    "ABDOMINAL PAIN", "STOMA", "PSC", "GI BLEEDING",
                                    "GI ULCER", "PERIANAL ABSCESS OR FISTULA", "SYSTEMATIC FUNGAL INFECTION",
                                    "DEMYELINATING DISORDER", "CELIAC", "B2 OR B3", "MALNOURISHMENT",
                                    "ANEMIA", "DIARRHEA", "NAUSEA OR VOMITING", "FEVER", "CDI",
                                    "ARTHRITIS OR LOW BACK PAIN", "DACTYLITIS",
                                    "HYPOALBUMINEMIA", "NON UC IBD DIAGNOSIS", "TOXIC MEGACOLON",
                                    "FULMINANT COLITIS", "INTRAABDOMINAL ABSCESS",
                                    "STRICTURE STENOSIS", "COLON ADENOMA", "INFECTION",
                                    "TUBERCULOSIS", "DIABETES", "HYPERTENSION", "COPD",
                                    "CKD STAGE IIB OR MORE", "UNSTABLE ANGINA OR MYOCARDIAL INFARCTION",
                                    "AUTOIMMUNE INFLAMMATORY DISEASE", "HEPATITIS B", "HEPATITIS C",
                                    "INHERITED AUTOIMMUNE DISORDER", "CUSTOM")

  if (!(inclusion1 %in% list_of_inclusion_categories) & code_search == T){
    stop("This function will only work to code search if one of the predefined code categories is supplied,
         or a custom defined list is provided and the inclusion criteria is set to 'CUSTOM'.")
  }

  # first code search only, same code as before
  # has set groups for codes, can easily add more groups

  if (code_search == T){
    # set up all the inclusion criteria outside of code search only chunk so not
    # repeated for text and code search chunk
    if ("CANCER" %in% inclusion1) {
      ##  fix cancer codes to be malignant cancers
      # inc <- "c|d0|d1|d2|d3|d4|^14|\\b15|\\b16|\\b17|\\b18|\\b19|\\b2"
      inc <- "c|d0|d37|d38|d39|d4|^14|^15|^16|^17|^18|^19|^20|^23"
    } else if ("COLORECTAL CANCER" %in% inclusion1) {
      inc <- "C18|C19|C20|^153|^154"
    } else if ("WEIGHT LOSS" %in% inclusion1) {
      inc <- "R63.4|^783.1|^783.2"
    } else if ("ABDOMINAL PAIN" %in% inclusion1) {
      inc <- "R10|^789.0"
    } else if ("STOMA" %in% inclusion1) {
      # CLB any other ICD9 codes here?
      inc <- "L24.B0|L24.B1|L24.B3|Z93.3|Z93.2|V44.2|V44.3"
    } else if ("PSC" %in% inclusion1) {
      inc <- "K83.01|^576.1"
    } else if ("GI BLEEDING" %in% inclusion1) {
      inc <- "K92.1|^569.3|^578.1|^599.70|^777.3|^792.1"
    } else if ("GI ULCER" %in% inclusion1) {
      inc <- "K25|K27|K28|K26|K63.3|K62.6|^531|^532|^533|^534"
    } else if ("PERIANAL ABSCESS OR FISTULA" %in% inclusion1) {
      inc <- "K50.913|K50.914|K50.813|K50.814|K50.013|K50.014|K50.113|K50.114|K51.013|K51.014|K51.213|K51.214|K51.313|K51.314|K51.413|K51.414|K51.513|K51.514|K51.813|K51.814|K51.913|K51.914|K60|K61|^565.1|^566"
    } else if ("SYSTEMIC FUNGAL INFECTION" %in% inclusion1) {
      inc <- "B39|B45|B38|B40|B46|B44|B37|B59|^110|^111|^112|^113|^114|^115|^116|^117|^118"
    } else if ("DEMYELINATING DISORDER" %in% inclusion1) {
      inc <- "G35|G36|G37|^340|^341"
    } else if ("CELIAC" %in% inclusion1) {
      inc <- "K90.0|^579.0"
    } else if ("B2 OR B3" %in% inclusion1) {
      inc <- "K50.912|K50.112|K50.012|K50.812|^560.89|^560.9"
    } else if ("MALNOURISHMENT" %in% inclusion1) {
      inc <- "E4|^263.9|^269.9"
    } else if ("ANEMIA" %in% inclusion1) {
      inc <- "D50|D51|D52|D53|^280|^281"
    } else if ("DIARRHEA" %in% inclusion1) {
      inc <- "R19.7|K59.1|K58.0|^564.5|^787.91"
    } else if ("NAUSEA OR VOMITING" %in% inclusion1) {
      inc <- "R11|^787.0"
    } else if ("FEVER" %in% inclusion1) {
      inc <- "R50.9|R61|^780.6"
    } else if ("CDI" %in% inclusion1) {
      inc <- "A04.7|^008.45"
    } else if ("ARTHRITIS OR LOW BACK PAIN" %in% inclusion1) {
      inc <- "M13|M05|M06|M07|M08|M10|M11|M12|M14|M1A|M54.5|^710|^711|^712|^713|^714|^715|^716|^724.2"
    } else if ("DACTYLITIS" %in% inclusion1) {
      inc <- "L08.9|^686.9"
    } else if ("HYPOALBUMINEMIA" %in% inclusion1) {
      inc <- "E88.09|^273.8"
    } else if ("NON UC IBD DIAGNOSIS" %in% inclusion1) {
      inc <- "K50|K52.3|K52.83|K55.9|^555|^558.9"
    } else if ("TOXIC MEGACOLON" %in% inclusion1) {
      inc <- "K59.31|^564.7"
    } else if ("FULMINANT COLITIS" %in% inclusion1) {
      inc <- "K55.03|^557.0"
    } else if ("INTRAABDOMINAL ABSCESS" %in% inclusion1) {
      inc <- "L02.211|K65.1|^567.22|^682.2"
    } else if ("STRICTURE STENOSIS" %in% inclusion1) {
      inc <- "K56.69|^560.89"
    } else if ("COLON ADENOMA" %in% inclusion1) {
      inc <- "D12.2|D12.3|D12.4|D12.5|D12.6|K31.A2|K55.20|^211.3|^235.2"
    } else if ("INFECTION" %in% inclusion1) {
      inc <- "L0|A49|A0|^00|^130|^131|^132|^133|^134|^135|^136"
    } else if ("TUBERCULOSIS" %in% inclusion1) {
      inc <- "A15|A17|A18|A19|^010|^011|^012|^013|^014|^015|^016|^017|^018"
    } else if ("DIABETES" %in% inclusion1) {
      inc <- "E08|E09|E10|E11|E13|^250"
    } else if ("HYPERTENSION" %in% inclusion1) {
      inc <- "I10|I11|I12|I13|I15|I16|^401|^402|^403|^404|^405"
    } else if ("COPD" %in% inclusion1) {
      inc <- "J44|^491.21|^493.2|^496"
    } else if ("CKD STAGE IIB OR MORE" %in% inclusion1) {
      inc <- "N18.32|N18.4|N18.5|^585.2|^585.3|^585.4|^585.5"
    } else if ("UNSTABLE ANGINA OR MYOCARDIAL INFARCTION" %in% inclusion1) {
      inc <- "I20|I21|^410|^412|^413"
    } else if ("AUTOIMMUNE INFLAMMATORY DISEASE" %in% inclusion1) {
      inc <- "M05|M06|M3|M04|^714|^710"
    } else if ("HEPATITIS B" %in% inclusion1) {
      inc <- "B16|B18.0|B18.1|B19.1|^070.2|^070.3"
    } else if ("HEPATITIS C" %in% inclusion1) {
      inc <- "B17.1|B18.2|B19.2|^070.41|^070.44|^070.51|^070.54|^070.7"
    } else if ("INHERITED AUTOIMMUNE DISORDER" %in% inclusion1) {
      inc <- "D80.0|D82.0|D80.4|D82.3|N41.4|Q82.8|D81.0|D81.1|D81.2|D81.3|E70.330|D76.1|D82.4|D82.2|D81.6|D81.7|D83|D80.2|D84.1|G11.3|Q82.8|D81.5|D81.8|^279.04|^279.12|^279.02|^279.8|^601.8|^757.2|^757.39|^279.2|^270.2|^288.4|^279.8|^279.06|^279.01|^277.6|^334.8|^277.2|^266.2"
    } else if ("CUSTOM" %in% inclusion1) {
      inc <- custominc
    }

  }

  if (text_search == F & code_search == T){
    if ("diagnosis" %in% names(data)) {
      dxemricd <- data$diagnosis %>% group_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                                              DIAG_CONCEPT_CODE, DIAGNOSIS_DATE) %>% filter(row_number() ==
                                                                                              1) %>% filter(DIAG_SYSTEM_NAME != "Local" & DATA_SOURCE ==
                                                                                                              "EMR") %>% filter(grepl(inc, SRC_DIAG_CONCEPT_CODE,
                                                                                                                                      ignore.case = TRUE))

      dxemricd <- custom_exclusions(dxemricd, source_column = SRC_DIAG_CONCEPT_CODE)
    }

    if ("patient_problem" %in% names(data)) {
      ppemricd <- data$patient_problem %>% group_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                                                    SOURCE_PROB_CODE, PROBLEM_START_DATE) %>% filter(row_number() ==
                                                                                                       1) %>% filter(SOURCE_PROB_CODE_SYSTEM_NAME != "Local" &
                                                                                                                       DATA_SOURCE == "EMR") %>% filter(grepl(inc, SOURCE_PROB_CODE, ignore.case = TRUE))

      ppemricd <- custom_exclusions(ppemricd, source_column = SOURCE_PROB_CODE)
    }

    if ("patient_history" %in% names(data)) {
      phemricd <- data$patient_history %>% group_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                                                    SRC_HISTORY_CONCEPT_CODE, EVENT_ONSET_DATE) %>%
        filter(row_number() == 1) %>% filter(HISTORY_TYPE ==
                                               "Medical_HX" & DATA_SOURCE == "EMR") %>% filter(grepl(inc,
                                                                                                     SRC_HISTORY_CONCEPT_CODE, ignore.case = TRUE))
      phemricd <- custom_exclusions(phemricd, source_column = SRC_HISTORY_CONCEPT_CODE)
    }

    # return tables
    x <- return_tables(data, dxemricd, ppemricd, phemricd)

    return(x)

  }


  # next if only text search is true and code search is false
  if (text_search == T & code_search == F){
    text_inc <- str_to_lower(inclusion1)
    # make it so that if multiple words are put in as a string it is converted to
    # something that works with grepl
    text_inc <- gsub(", ", "|", text_inc)
    text_inc <- gsub("; ", "|", text_inc)
    text_inc <- gsub(": ", "|", text_inc)
    # text_inc <- gsub(" ", "", text_inc)

    # character string to keep before the text inclusion word to look for stop words
    keepbefore <- paste0(" ", text_inc, ".*")


    if ("diagnosis" %in% names(data)) {
      dxemricd <- data$diagnosis %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID, DIAG_CONCEPT_CODE, DIAGNOSIS_DATE) %>%
        filter(row_number() == 1) %>%
        filter(DIAG_SYSTEM_NAME != "Local" & DATA_SOURCE == "EMR") %>%
        filter(grepl(text_inc, SRC_DIAG_CONCEPT_NAME, ignore.case = TRUE)) %>%
        mutate(without_before_drop = gsub(keepbefore, "", SRC_DIAG_CONCEPT_NAME, ignore.case = T)) %>%
        mutate(without_before_drop= ifelse(grepl("without|W/O", without_before_drop, ignore.case = T), T, F)) %>%
        filter(without_before_drop == F) %>%
        select(-without_before_drop) %>%
        mutate(screening = ifelse(grepl("screen|screening|scan", SRC_DIAG_CONCEPT_NAME, ignore.case = T), T, F)) %>%
        filter(screening == F) %>%
        select(-screening) %>%
        mutate(fh = ifelse(grepl("FAMILY HISTORY|FH", SRC_DIAG_CONCEPT_NAME, ignore.case = T), T, F)) %>%
        filter(fh == F) %>%
        select(-fh) %>%
        mutate(risk = ifelse(grepl("RISK FOR", SRC_DIAG_CONCEPT_NAME, ignore.case = T), T, F)) %>%
        filter(risk == F) %>%
        select(-risk) %>%
        mutate(closure = ifelse(grepl("CLOSURE", SRC_DIAG_CONCEPT_NAME, ignore.case = T), T, F)) %>%
        filter(closure == F) %>%
        select(-closure)

      dxemricd <- custom_exclusions(dxemricd, source_column = SRC_DIAG_CONCEPT_CODE)
    }

    if ("patient_problem" %in% names(data)) {
      ppemricd <- data$patient_problem %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID, SOURCE_PROB_CODE, PROBLEM_START_DATE) %>%
        filter(row_number() == 1) %>%
        filter(SOURCE_PROB_CODE_SYSTEM_NAME != "Local" & DATA_SOURCE == "EMR") %>%
        filter(grepl(text_inc, SOURCE_PROB_DESC, ignore.case = TRUE)) %>%
        mutate(without_before_drop = gsub(keepbefore, "", SOURCE_PROB_DESC, ignore.case = T)) %>%
        mutate(without_before_drop= ifelse(grepl("without|W/O", without_before_drop, ignore.case = T), T, F)) %>%
        filter(without_before_drop == F) %>%
        select(-without_before_drop) %>%
        mutate(screening = ifelse(grepl("screen|screening|scan", SOURCE_PROB_DESC, ignore.case = T), T, F)) %>%
        filter(screening == F) %>%
        select(-screening) %>%
        mutate(fh = ifelse(grepl("FAMILY HISTORY|FH", SOURCE_PROB_DESC, ignore.case = T), T, F)) %>%
        filter(fh == F) %>%
        select(-fh)  %>%
        mutate(risk = ifelse(grepl("RISK FOR", SOURCE_PROB_DESC, ignore.case = T), T, F)) %>%
        filter(risk == F) %>%
        select(-risk)  %>%
        mutate(closure = ifelse(grepl("CLOSURE", SOURCE_PROB_DESC, ignore.case = T), T, F)) %>%
        filter(closure == F) %>%
        select(-closure)

      ppemricd <- custom_exclusions(ppemricd, source_column = SOURCE_PROB_CODE)
    }

    if ("patient_history" %in% names(data)) {
      phemricd <- data$patient_history %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID, SRC_HISTORY_CONCEPT_CODE, EVENT_ONSET_DATE) %>%
        filter(row_number() == 1) %>%
        filter(HISTORY_TYPE == "Medical_HX" & DATA_SOURCE == "EMR") %>%
        filter(grepl(text_inc, SRC_HISTORY_CONCEPT_NAME, ignore.case = TRUE)) %>%
        mutate(without_before_drop = gsub(keepbefore, "", SRC_HISTORY_CONCEPT_NAME, ignore.case = T)) %>%
        mutate(without_before_drop= ifelse(grepl("without|W/O", without_before_drop, ignore.case = T), T, F)) %>%
        filter(without_before_drop == F) %>%
        select(-without_before_drop)  %>%
        mutate(screening = ifelse(grepl("screen|screening|scan", SRC_HISTORY_CONCEPT_NAME, ignore.case = T), T, F)) %>%
        filter(screening == F) %>%
        select(-screening)  %>%
        mutate(fh = ifelse(grepl("FAMILY HISTORY|FH", SRC_HISTORY_CONCEPT_NAME, ignore.case = T), T, F)) %>%
        filter(fh == F) %>%
        select(-fh)  %>%
        mutate(risk = ifelse(grepl("RISK FOR", SRC_HISTORY_CONCEPT_NAME, ignore.case = T), T, F)) %>%
        filter(risk == F) %>%
        select(-risk) %>%
        mutate(closure = ifelse(grepl("CLOSURE", SRC_HISTORY_CONCEPT_NAME, ignore.case = T), T, F)) %>%
        filter(closure == F) %>%
        select(-closure)

      phemricd <- custom_exclusions(phemricd, source_column = SRC_HISTORY_CONCEPT_CODE)
    }

    # return the tables
    x <- return_tables(data, dxemricd, ppemricd, phemricd)

    return(x)

  }


  ## CODE & TEXT
  # if both code search and text search, run the same as above, join the
  # different tables and run distinct_all() to not duplicate. use suffix _text
  # for the text searches to be able to rbind the tables at the end

  if(text_search == T & code_search == T){
    ##  first text search
    text_inc <- str_to_lower(inclusion1)
    # make it so that if multiple words are put in as a string it is converted to
    # something that works with grepl
    text_inc <- gsub(", ", "|", text_inc)
    text_inc <- gsub("; ", "|", text_inc)
    text_inc <- gsub(": ", "|", text_inc)
    ## can't get rid of all spaces because of two word search terms
    # text_inc <- gsub(" ", "", text_inc)

    # character string to keep before the text inclusion word to look for stop words
    # Cass - need to change this for multiple strings
    keepbefore <- paste0(" ", text_inc, ".*")

    # text inclusion for specified categories -
    if(inclusion1 %in% list_of_inclusion_categories) {
      if ("CANCER" %in% inclusion1) {
        text_inc <- "CANCER|MALIGNANT NEOPLASM|NEOPLASM OF UNCERTAIN BEHAVIOR"
      } else if ("COLORECTAL CANCER" %in% inclusion1) {
        text_inc <- "COLORECTAL CANCER|COLON ADENOCARCINOMA|MALIGNANT NEOPLASM OF COLON|RECTAL CANCER|
        MALIGNANT NEOPLASM OF SIGMOID COLON|MALIGNANT NEOPLASM OF RECTOSIGMOID|RECTOSIGMOID CANCER|
        ADENOCARCINOMA OF COLON|CANCER OF COLON|CANCER OF SIGMOID COLON|MALIGNANT NEOPLASM OF DESCENDING COLON|
        COLON CANCER|MALIGNANT NEOPLASM OF RECTUM|MALIGNANT TUMOR OF RECTUM|RECTAL CANCER|
        MALIGNANT NEOPLASM OF TRANSVERSE COLON"
      } else if ("WEIGHT LOSS" %in% inclusion1) {
        text_inc <- "WEIGHT LOSS|LOSS OF WEIGHT"
      } else if ("ABDOMINAL PAIN" %in% inclusion1) {
        text_inc <- "ABDOMINAL PAIN|DYSPEPSIA|FLANK PAIN|PELVIC PAIN|EPIGASTRIC PAIN|ABDOMINAL DISCOMFORT|
        ABDOMINAL CRAMPING|ACUTE ABDOMEN"
      } else if ("STOMA" %in% inclusion1) {
        text_inc <-  " STOMA |ILEOSTOMY|COLOSTOMY"
      } else if ("PSC" %in% inclusion1) {
        text_inc <- "PSC |PRIMARY SCLEROSING CHOLANGITIS"
      } else if ("GI BLEEDING" %in% inclusion1) {
        text_inc <- "GI BLEEDING|BLOOD IN STOOL|HEMATOCHEZIA|MELENA|BLOODY STOOLS|GASTROINTESTINAL HEMORRHAGE|
        BLACK TARRY STOOLS"
      } else if ("GI ULCER" %in% inclusion1) {
        text_inc <- "GI ULCER|ULCER OF ILEUM|ULCER OF ANUS|UCLER OF RECTUM"
      } else if ("PERIANAL ABSCESS OR FISTULA" %in% inclusion1) {
        text_inc <- "PERIANAL ABSCESS OR FISTULA"
      } else if ("SYSTEMIC FUNGAL INFECTION" %in% inclusion1) {
        text_inc <- "SYSTEMIC FUNGAL INFECTION"
      } else if ("DEMYELINATING DISORDER" %in% inclusion1) {
        text_inc <- "DEMYELINATING DISORDER|MULTIPLE SCLEROSIS|DEMYELINATING DISEASE"
      } else if ("CELIAC" %in% inclusion1) {
        text_inc <- "CELIAC"
      } else if ("B2 OR B3" %in% inclusion1) {
        # CASS: COME BACK HERE
        text_inc <- "CROHN'S DISEASE WITH INTESTINAL OBSTRUCTION|CROHN'S DISEASE OF SMALL INTESTINE WITH INTESTINAL OBSTRUCTION|
        CROHN DISEASE, WITH INTESTINAL OBSTRUCTION|CROHN'S DISEASE OF BOTH SMALL AND LARGE INTESTINE WITH INTESTINAL OBSTRUCTION|
        CROHN'S DISEASE OF ILEUM WITH INTESTINAL OBSTRUCTION|CROHN'S DISEASE OF LARGE INTESTINE WITH INTESTINAL OBSTRUCTION"
      } else if ("MALNOURISHMENT" %in% inclusion1) {
        text_inc <- "MALNOURISHMENT|ALIMENTARY EDEMA|MALNUTRITION"
      } else if ("ANEMIA" %in% inclusion1) {
        text_inc <- "ANEMIA"
      } else if ("DIARRHEA" %in% inclusion1) {
        text_inc <- "DIARRHEA"
      } else if ("NAUSEA OR VOMITING" %in% inclusion1) {
        text_inc <- "NAUSEA|VOMITING"
      } else if ("FEVER" %in% inclusion1) {
        text_inc <- "FEVER|FEBRILE"
      } else if ("CDI" %in% inclusion1) {
        text_inc <- "CLOSTRIDIUM DIFFICILE|C. DIFFICILE"
      } else if ("ARTHRITIS OR LOW BACK PAIN" %in% inclusion1) {
        text_inc <- "ARTHRITIS|LOW BACK PAIN|ARTHOPATHY"
      } else if ("DACTYLITIS" %in% inclusion1) {
        # CASS COME BACK -
        text_inc <- "DACTYLITIS"
      } else if ("HYPOALBUMINEMIA" %in% inclusion1) {
        # CASS COME BACK
        text_inc <- "HYPOALBUMINEMIA"
      } else if ("NON UC IBD DIAGNOSIS" %in% inclusion1) {
        text_inc <- "NON UC IBD DIAGNOSIS|CROHN'S|IBD UNCLASSIFIED|INDETERMINATE COLITIS"
      } else if ("TOXIC MEGACOLON" %in% inclusion1) {
        text_inc <- "TOXIC MEGACOLON"
      } else if ("FULMINANT COLITIS" %in% inclusion1) {
        text_inc <- "FULMINANT COLITIS"
      } else if ("INTRAABDOMINAL ABSCESS" %in% inclusion1) {
        text_inc <- "INTRAABDOMINAL ABSCESS|ABSCESS OF ABDOMINAL|ABDOMINAL WALL ABSCESS|
        MESENTERIC ABSCESS|PERITONEAL ABSCESS|INTRA-ABDOMINAL ABSCESS"
      } else if ("STRICTURE STENOSIS" %in% inclusion1) {
        text_inc <- "STRICTURE|STENOSIS"
      } else if ("COLON ADENOMA" %in% inclusion1) {
        text_inc <- "COLON ADENOMA|BENIGN NEOPLASM OF COLON|DYSPLASIA OF COLON|
        ADENOMA OF COLON|COLON DYSPLASIA|COLONIC ADENOMA"
      } else if ("INFECTION" %in% inclusion1) {
        # CASS -
        text_inc <- "INFECTION"
      } else if ("TUBERCULOSIS" %in% inclusion1) {
        text_inc <- "TUBERCULOSIS|TB INFECTION"
      } else if ("DIABETES" %in% inclusion1) {
        text_inc <- "DIABETES"
      } else if ("HYPERTENSION" %in% inclusion1) {
        text_inc <- " HTN |HYPERTENSION"
      } else if ("COPD" %in% inclusion1) {
        text_inc <- " COPD |CHRONIC OBSTRUCTIVE PULMONARY DISEASE|CROHNIC BRONCHITIS|
        CHRONIC AIRWAY OBSTRUCTION|CHRONIC OBSTRUCTIVE ASTHMA"
      } else if ("CKD STAGE IIB OR MORE" %in% inclusion1) {
        text_inc <- " CKD |CHRONIC KIDNEY DISEASE|CHRONIC RENAL FAILURE"
      } else if ("UNSTABLE ANGINA OR MYOCARDIAL INFARCTION" %in% inclusion1) {
        text_inc <- "UNSTABLE ANGINA|MYOCARDIAL INFARCTION|ANGINA PECTORIS"
      } else if ("AUTOIMMUNE INFLAMMATORY DISEASE" %in% inclusion1) {
        text_inc <- "AUTOIMMUNE INFLAMMATORY DISEASE"
      } else if ("HEPATITIS B" %in% inclusion1) {
        text_inc <- "HEPATITIS B"
      } else if ("HEPATITIS C" %in% inclusion1) {
        text_inc <- "HEPATITIS C "
      } else if ("INHERITED AUTOIMMUNE DISORDER" %in% inclusion1) {
        text_inc <- "INHERITED AUTOIMMUNE DISORDER"
      }

    }


    if ("diagnosis" %in% names(data)) {
      dxemricd_text <- data$diagnosis %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID, DIAG_CONCEPT_CODE, DIAGNOSIS_DATE) %>%
        filter(row_number() == 1) %>%
        filter(DIAG_SYSTEM_NAME != "Local" & DATA_SOURCE == "EMR") %>%
        filter(grepl(text_inc, SRC_DIAG_CONCEPT_NAME, ignore.case = TRUE)) %>%
        mutate(without_before_drop = gsub(keepbefore, "", SRC_DIAG_CONCEPT_NAME, ignore.case = T)) %>%
        mutate(without_before_drop= ifelse(grepl("without|W/O", without_before_drop, ignore.case = T), T, F)) %>%
        filter(without_before_drop == F) %>%
        select(-without_before_drop)  %>%
        mutate(screening = ifelse(grepl("screen|screening|scan", SRC_DIAG_CONCEPT_NAME, ignore.case = T), T, F)) %>%
        filter(screening == F) %>%
        select(-screening) %>%
        mutate(fh = ifelse(grepl("FAMILY HISTORY|FH", SRC_DIAG_CONCEPT_NAME, ignore.case = T), T, F)) %>%
        filter(fh == F) %>%
        select(-fh) %>%
        mutate(risk = ifelse(grepl("RISK FOR", SRC_DIAG_CONCEPT_NAME, ignore.case = T), T, F)) %>%
        filter(risk == F) %>%
        select(-risk) %>%
        mutate(closure = ifelse(grepl("CLOSURE", SRC_DIAG_CONCEPT_NAME, ignore.case = T), T, F)) %>%
        filter(closure == F) %>%
        select(-closure)

      dxemricd_text <- custom_exclusions(dxemricd_text, source_column = SRC_DIAG_CONCEPT_CODE)

      dxemricd <- data$diagnosis %>% group_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                                              DIAG_CONCEPT_CODE, DIAGNOSIS_DATE) %>% filter(row_number() ==
                                                                                              1) %>% filter(DIAG_SYSTEM_NAME != "Local" & DATA_SOURCE ==
                                                                                                              "EMR") %>% filter(grepl(inc, SRC_DIAG_CONCEPT_CODE,
                                                                                                                                      ignore.case = TRUE))

      dxemricd <- custom_exclusions(dxemricd, source_column = SRC_DIAG_CONCEPT_CODE)

      dxemricd <- dxemricd %>%
        rbind(dxemricd_text) %>%
        distinct_all()
    }

    if ("patient_problem" %in% names(data)) {
      ppemricd_text <- data$patient_problem %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID, SOURCE_PROB_CODE, PROBLEM_START_DATE) %>%
        filter(row_number() == 1) %>%
        filter(SOURCE_PROB_CODE_SYSTEM_NAME != "Local" & DATA_SOURCE == "EMR") %>%
        filter(grepl(text_inc, SOURCE_PROB_DESC, ignore.case = TRUE)) %>%
        mutate(without_before_drop = gsub(keepbefore, "", SOURCE_PROB_DESC, ignore.case = T)) %>%
        mutate(without_before_drop= ifelse(grepl("without|W/O", without_before_drop, ignore.case = T), T, F)) %>%
        filter(without_before_drop == F) %>%
        select(-without_before_drop)  %>%
        mutate(screening = ifelse(grepl("screen|screening|scan", SOURCE_PROB_DESC, ignore.case = T), T, F)) %>%
        filter(screening == F) %>%
        select(-screening) %>%
        mutate(fh = ifelse(grepl("FAMILY HISTORY|FH", SOURCE_PROB_DESC, ignore.case = T), T, F)) %>%
        filter(fh == F) %>%
        select(-fh) %>%
        mutate(risk = ifelse(grepl("RISK FOR", SOURCE_PROB_DESC, ignore.case = T), T, F)) %>%
        filter(risk == F) %>%
        select(-risk) %>%
        mutate(closure = ifelse(grepl("CLOSURE", SOURCE_PROB_DESC, ignore.case = T), T, F)) %>%
        filter(closure == F) %>%
        select(-closure)

      ppemricd_text <- custom_exclusions(ppemricd_text, source_column = SOURCE_PROB_CODE)

      ppemricd <- data$patient_problem %>% group_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                                                    SOURCE_PROB_CODE, PROBLEM_START_DATE) %>% filter(row_number() ==
                                                                                                       1) %>% filter(SOURCE_PROB_CODE_SYSTEM_NAME != "Local" &
                                                                                                                       DATA_SOURCE == "EMR") %>% filter(grepl(inc, SOURCE_PROB_CODE, ignore.case = TRUE))

      ppemricd <- custom_exclusions(ppemricd, source_column = SOURCE_PROB_CODE)

      ppemricd <- ppemricd %>%
        rbind(ppemricd_text) %>%
        distinct_all()
    }

    if ("patient_history" %in% names(data)) {
      phemricd_text <- data$patient_history %>%
        group_by(DEIDENTIFIED_MASTER_PATIENT_ID, SRC_HISTORY_CONCEPT_CODE, EVENT_ONSET_DATE) %>%
        filter(row_number() == 1) %>%
        filter(HISTORY_TYPE == "Medical_HX" & DATA_SOURCE == "EMR") %>%
        filter(grepl(text_inc, SRC_HISTORY_CONCEPT_NAME, ignore.case = TRUE)) %>%
        mutate(without_before_drop = gsub(keepbefore, "", SRC_HISTORY_CONCEPT_NAME, ignore.case = T)) %>%
        mutate(without_before_drop= ifelse(grepl("without|W/O", without_before_drop, ignore.case = T), T, F)) %>%
        filter(without_before_drop == F) %>%
        select(-without_before_drop) %>%
        mutate(screening = ifelse(grepl("screen|screening|scan", SRC_HISTORY_CONCEPT_NAME, ignore.case = T), T, F)) %>%
        filter(screening == F) %>%
        select(-screening) %>%
        mutate(fh = ifelse(grepl("FAMILY HISTORY|FH", SRC_HISTORY_CONCEPT_NAME, ignore.case = T), T, F)) %>%
        filter(fh == F) %>%
        select(-fh) %>%
        mutate(risk = ifelse(grepl("RISK FOR", SRC_HISTORY_CONCEPT_NAME, ignore.case = T), T, F)) %>%
        filter(risk == F) %>%
        select(-risk) %>%
        mutate(closure = ifelse(grepl("CLOSURE", SRC_HISTORY_CONCEPT_NAME, ignore.case = T), T, F)) %>%
        filter(closure == F) %>%
        select(-closure)

      phemricd_text <- custom_exclusions(phemricd_text, source_column = SRC_HISTORY_CONCEPT_CODE)

      phemricd <- data$patient_history %>% group_by(DEIDENTIFIED_MASTER_PATIENT_ID,
                                                    SRC_HISTORY_CONCEPT_CODE, EVENT_ONSET_DATE) %>%
        filter(row_number() == 1) %>% filter(HISTORY_TYPE ==
                                               "Medical_HX" & DATA_SOURCE == "EMR") %>% filter(grepl(inc,
                                                                                                     SRC_HISTORY_CONCEPT_CODE, ignore.case = TRUE))
      phemricd <- custom_exclusions(phemricd, source_column = SRC_HISTORY_CONCEPT_CODE)

      phemricd <- phemricd %>%
        rbind(phemricd_text) %>%
        distinct_all()
    }

    x <- return_tables(data, dxemricd, ppemricd, phemricd)

    return(x)

  }

}
