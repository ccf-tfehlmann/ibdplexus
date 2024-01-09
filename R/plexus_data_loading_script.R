#' load_data
#'
#' Load unzipped DDM txt or csv files.If multiple cohorts are unzipped in one directory from different times, only the most recent one will load.
#'
#' @param datadir The directory where data is saved.
#' @param cohort The cohort to load. Either RISK, QORUS, or SPARC.
#' @param domains The domains to load. Default is "ALL". Must be a character string. Other valid values are patterns matching input file names common examples are:  "demographics", "diagnosis", "encounter", "procedures", "observations", "biosample", "omics_patient_mapping", "prescriptions".
#' @param data_type The data source to load either case report forms (including covid survey), electronic medical record, or both. Options are both, crf or emr.
#'
#' @return A list of dataframes for each domain. If both sources are loaded, emr and crf data are combined.
#' @export
load_data <- function(datadir, cohort = c("RISK", "QORUS", "SPARC"), domains = c("ALL"), data_type = c("BOTH", "CRF", "EMR")) {
  cohort <- toupper(cohort)
  domains <- toupper(domains)
  data_type <- toupper(data_type)
  datadir <- folder_fix(datadir)

  # INCLUDE COVID SURVEY IN CRF DATA SOURCE ----

  if (data_type == "CRF") {
    data_type <- paste0(data_type, "|COVID")
  }

  # GET FILES OF MOST RECENT DATA FOR EACH COHORT OF INTEREST ----

  folders <- list.files(path = paste0(datadir))


  list_names <- grep("demographic", folders, ignore.case = T, value = T)
  list_names <- grep("family", list_names, ignore.case = T, value = T, invert = T)


  cohorts <- lapply(paste0(datadir, list_names), function(x) {
    data.table::fread(x, header = T, select = c("DATA_SOURCE"), nrows = 10)
  })

  names(cohorts) <- gsub("(.*/\\s*)|.txt|.csv|[a-z]|[A-Z]|_\\d+\\.", "", list_names)
  names(cohorts) <- gsub("___", "__", names(cohorts))

  names(cohorts) <- gsub("^([^__]*__[^_]*).*", "\\1", names(cohorts))


  cohorts <- bind_rows(cohorts, .id = "df")

  cohorts <- cohorts %>%
    filter(DATA_SOURCE != "EMR") %>%
    distinct(df, DATA_SOURCE) %>%
    mutate(
      Date = lubridate::ymd(gsub(".*_", "", df)),
      Cohort = gsub("ECRF_|COVID_SURVEY_|SF_", "", DATA_SOURCE)
    ) %>%
    distinct(Cohort, Date, df) %>%
    mutate(df = gsub("_.*", "", df)) %>%
    group_by(Cohort) %>%
    filter(Date == max(lubridate::ymd(Date)))



  folderinfo <- file.info(paste0(datadir, folders)) %>%
    tibble::rownames_to_column()

  files <- folderinfo %>%
    filter(grepl(".txt|.csv", rowname)) %>%
    filter(grepl(paste0(datadir, "[0-9]"), rowname)) %>%
    mutate(time = as.character.POSIXt(strptime(mtime, "%Y-%m-%d"))) %>%
    mutate(df = gsub("(.*/\\s*)|.txt|[a-z]|[A-Z]|_\\d+\\.", "", rowname)) %>%
    mutate(df = gsub("_.*", "", df)) %>%
    left_join(cohorts, by = "df") %>%
    arrange(desc(time)) %>%
    group_by(Cohort) %>%
    filter(Cohort %in% cohort) %>%
    ungroup() %>%
    select(rowname)

  # remove patient history old

  files <- files$rowname[grep("Old", files$rowname, ignore.case = T, invert = T)]



  if ("ALL" %in% domains) {
    files <- files
  } else {
    filestring <- paste(domains, collapse = "|")
    files <- grep(filestring, files, value = T, ignore.case = T)
  }


  if (data_type == "BOTH") {
    files <- files
  } else if ("DEMOGRAPHICS" %in% domains | "MASTER" %in% domains | "BIOSAMPLE" %in% domains | "OMICS" %in% domains) {
    filestring <- paste0(data_type, "|DEMOGRAPHICS|MASTER|BIOSAMPLE|OMICS")
    files <- grep(filestring, files, value = T, ignore.case = T)
  } else {
    filestring <- paste0(data_type, "|DEMOGRAPHICS|MASTER|BIOSAMPLE|OMICS")
    files <- grep(filestring, files, value = T, ignore.case = T)
  }

  if (length(cohort) == 1) {
    if (cohort == "SPARC") {
      files <- files[grep("family|study", files, invert = T, ignore.case = T)]
    } else {
      files <- files
    }
  }


  # LOAD DATA ----
  data <- lapply(files, function(x) data.table::fread(x))

  # Assign Names
  names(data) <- gsub(paste0(datadir, "|[0-9]*|[0-9]|.txt|\\/|.csv"), "", (files))
  names(data) <- gsub("_SiteExtract", "", names(data))
  names(data) <- gsub("^[_]|_$|__$|___$|____$", "", names(data))
  names(data) <- gsub("_CRF|_EMR|_COVID", "", names(data), ignore.case = T)
  names(data) <- gsub("sparc_", "", names(data), ignore.case = T)



  # Combine Data with the Same Name (Collapses EMR and CRF data together)
  data <- data[order(names(data))]


  data <- data %>%
    lapply(., mutate_if, is.integer, as.character) %>%
    lapply(., mutate_if, is.numeric, as.character) %>%
    lapply(., mutate_if, is.logical, as.character) %>%
    lapply(., mutate_if, is.factor, as.character)


  dslist <- unique(names(data))

  for (i in 1:length(dslist)) {
    ii <- (dslist[i])


    nums <- grep(paste0(ii), names(data))

    assign(paste0(ii), (data.table::rbindlist(data[nums], fill = TRUE)) %>% distinct())
    gc()
  }


  gc()

  data <- Filter(function(x) is(x, "data.frame"), mget(intersect(ls(), names(data))))
  gc()
  names(data) <- tolower(names(data))

  remove(list = dslist)


  # Standardize variable names
  if ("encounter" %in% names(data)) {
    data$encounter <- data$encounter %>% rename(VISIT_ENCOUNTER_ID = VISITENC_ID)
  }


  data <- data[sapply(data, nrow) > 1]

  data <- lapply(data, function(x) x %>% mutate(across(everything(), ~ replace(., . %in% c(""), NA))))


  if ("observations" %in% names(data)) {
    data$observations <- data$observations %>%
      mutate(OBS_TEST_CONCEPT_NAME = ifelse(OBS_TEST_CONCEPT_NAME == "Constitutional- General Well-Being", "Constitutional - General Well-Being", OBS_TEST_CONCEPT_NAME)) %>%
      mutate(across(everything(), ~ replace(., . %in% c("N.A.", "NA", "N/A", "", " "), NA))) %>%
      mutate(OBS_TEST_RESULT_DATE = lubridate::dmy(OBS_TEST_RESULT_DATE))
  }

  if ("encounter" %in% names(data)) {
    data$encounter <- data$encounter %>%
      mutate(
        VISIT_ENCOUNTER_START_DATE = lubridate::dmy(VISIT_ENCOUNTER_START_DATE),
        VISIT_ENCOUNTER_END_DATE = lubridate::dmy(VISIT_ENCOUNTER_END_DATE)
      )
  }

  if ("labs" %in% names(data)) {
    data$labs <- data$labs %>%
      mutate(LAB_TEST_CONCEPT_NAME = case_when(
        grepl("FECAL CALPROTECTIN \\(10-600", LAB_TEST_CONCEPT_NAME, ignore.case = T) ~ "FECAL CALPROTECTIN (10-600 uG/G)",
        grepl("FECAL CALPROTECTIN \\(30-1800", LAB_TEST_CONCEPT_NAME, ignore.case = T) ~ "FECAL CALPROTECTIN (30-1800 uG/G)",
        TRUE ~ LAB_TEST_CONCEPT_NAME
      ))
  }

  rm(list = c("files", "folderinfo"))
  data <- lapply(data, function(x) {
    x %>%
      setNames(gsub(" |\\.|-", "_", names(.))) %>%
      setNames(toupper(names(.)))
  })


  return(data)
  gc()
}





#' load_zipped_data
#'
#' Load compressed files extracted from IBD Plexus.
#'
#' @param datadir The directory where data is saved.
#' @param cohort The cohort to load. Either RISK, QORUS, or SPARC.
#' @param domains The domains to load. Default is "All". Must be a character string.
#' @param data_type The data source to load either case report forms, electronic medical record or both. Options are both, crf or emr.
#' @param exdir The file to save the files once unzipped. If left blank will be the working directory. Do not include backslash at the end of the file location.
#'
#'
#' @return A list of dataframes for each domain. If both sources are loaded, emr and crf data are combined.
#' @export
load_zipped_data <- function(datadir, cohort = c("RISK", "QORUS", "SPARC"), domains = c("ALL"), data_type = c("BOTH", "CRF", "EMR"), exdir = ".") {
  cohort <- toupper(cohort)
  domains <- toupper(domains)
  data_type <- toupper(data_type)
  datadir <- folder_fix(datadir)


  # INCLUDE COVID SURVEY IN CRF DATA SOURCE ----

  if (data_type == "CRF") {
    data_type <- paste0(data_type, "|COVID")
  }



  # GET FILES OF MOST RECENT DATA FOR EACH COHORT OF INTEREST ----


  zipfiles <- unique(grep(".zip", paste0(datadir, list.files(datadir)), value = TRUE))
  zipfileinfo <- file.info(path = zipfiles)
  filepath <- rownames(zipfileinfo[which.max(zipfileinfo$mtime), ])


  folders <- unzip(filepath, list = TRUE)$Name

  list_names <- grep("demographic", folders, ignore.case = T, value = T)
  list_names <- grep("family", list_names, ignore.case = T, value = T, invert = T)



  cohorts <- lapply(list_names, function(x) {
    data.table::fread(unzip(filepath, files = x, exdir = exdir), header = T, select = c("DATA_SOURCE"), nrows = 10)
  })

  names(cohorts) <- gsub("(.*/\\s*)|.txt|.csv|[a-z]|[A-Z]|_\\d+\\.", "", list_names)
  names(cohorts) <- gsub("___", "__", names(cohorts))

  names(cohorts) <- gsub("^([^__]*__[^_]*).*", "\\1", names(cohorts))


  cohorts <- bind_rows(cohorts, .id = "df")

  cohorts <- cohorts %>%
    filter(DATA_SOURCE != "EMR") %>%
    distinct(df, DATA_SOURCE) %>%
    mutate(
      Date = ymd(gsub(".*_", "", df)),
      Cohort = gsub("ECRF_|COVID_SURVEY_|SF_", "", DATA_SOURCE)
    ) %>%
    distinct(Cohort, Date, df) %>%
    mutate(df = gsub("_.*", "", df)) %>%
    group_by(Cohort) %>%
    filter(Date == max(ymd(Date)))


  files <- data.frame(files = folders) %>%
    filter(grepl(".txt|.csv", files)) %>%
    mutate(df = gsub("(.*/\\s*)|.txt|[a-z]|[A-Z]|_\\d+\\.", "", files)) %>%
    mutate(df = gsub("_.*", "", df)) %>%
    left_join(cohorts, by = "df") %>%
    group_by(Cohort) %>%
    filter(Cohort %in% cohort) %>%
    ungroup() %>%
    select(files)

  # files = files$files
  files <- files$files[grep("Old", files$files, ignore.case = T, invert = T)]


  if ("ALL" %in% domains) {
    files <- files
  } else {
    filestring <- paste(domains, collapse = "|")
    files <- grep(filestring, files, value = T, ignore.case = T)
  }



  if (data_type == "BOTH") {
    files <- files
  } else if ("DEMOGRAPHICS" %in% domains | "MASTER" %in% domains | "BIOSAMPLE" %in% domains | "OMICS" %in% domains) {
    filestring <- paste0(data_type, "|DEMOGRAPHICS|MASTER|BIOSAMPLE|OMICS")
    files <- grep(filestring, files, value = T, ignore.case = T)
  } else {
    filestring <- paste0(data_type, "|DEMOGRAPHICS|MASTER|BIOSAMPLE|OMICS")
    files <- grep(filestring, files, value = T, ignore.case = T)
  }


  if (length(cohort) == 1) {
    if (cohort == "SPARC") {
      files <- files[grep("family|study", files, invert = T, ignore.case = T)]
    } else {
      files <- files
    }
  }



  # LOAD IN ALL FILES ----


  data <- lapply(files, function(x) {
    data.table::fread(unzip(filepath, files = x, exdir = exdir), stringsAsFactors = F, na.strings = c(NA, "", "NA"), header = T)
  })


  # Assign Names
  names(data) <- gsub(paste0(datadir, "|[0-9]*|[0-9]|.txt|\\/|.csv"), "", (files))
  names(data) <- gsub("_SiteExtract", "", names(data))
  names(data) <- gsub("^[_]|_$|__$|___$|____$", "", names(data))

  names(data) <- gsub("_CRF|_EMR|_COVID", "", names(data), ignore.case = T)
  names(data) <- gsub("sparc_", "", names(data), ignore.case = T)


  # Combine Data with the Same Name (Collapses EMR and CRF data)




  # Combine Data with the Same Name (Collapses EMR and CRF data together)
  data <- data[order(names(data))]

  data <- data %>%
    lapply(., mutate_if, is.integer, as.character) %>%
    lapply(., mutate_if, is.numeric, as.character) %>%
    lapply(., mutate_if, is.logical, as.character) %>%
    lapply(., mutate_if, is.factor, as.character)


  dslist <- unique(names(data))

  for (i in 1:length(dslist)) {
    ii <- (dslist[i])

    nums <- grep(paste0(ii), names(data))

    assign(paste0(ii), (bind_rows(data[nums])) %>% distinct())
  }

  data <- Filter(function(x) is(x, "data.frame"), mget(intersect(ls(), names(data))))

  names(data) <- tolower(names(data))

  remove(list = dslist)


  # Standardize variable names
  if ("encounter" %in% names(data)) {
    data$encounter <- data$encounter %>% rename(VISIT_ENCOUNTER_ID = VISITENC_ID)
  }


  data <- data[sapply(data, nrow) > 1]

  data <- lapply(data, function(x) x %>% mutate(across(everything(), ~ replace(., . %in% c(""), NA))))


  if ("observations" %in% names(data)) {
    data$observations <- data$observations %>%
      mutate(OBS_TEST_CONCEPT_NAME = ifelse(OBS_TEST_CONCEPT_NAME == "Constitutional- General Well-Being", "Constitutional - General Well-Being", OBS_TEST_CONCEPT_NAME)) %>%
      mutate(across(everything(), ~ replace(., . %in% c("N.A.", "NA", "N/A", "", " "), NA))) %>%
      mutate(OBS_TEST_RESULT_DATE = lubridate::dmy(OBS_TEST_RESULT_DATE))
  }

  if ("encounter" %in% names(data)) {
    data$encounter <- data$encounter %>%
      mutate(
        VISIT_ENCOUNTER_START_DATE = lubridate::dmy(VISIT_ENCOUNTER_START_DATE),
        VISIT_ENCOUNTER_END_DATE = lubridate::dmy(VISIT_ENCOUNTER_END_DATE)
      )
  }


  if ("labs" %in% names(data)) {
    data$labs <- data$labs %>%
      mutate(LAB_TEST_CONCEPT_NAME = case_when(
        grepl("FECAL CALPROTECTIN \\(10-600", LAB_TEST_CONCEPT_NAME, ignore.case = T) ~ "FECAL CALPROTECTIN (10-600 uG/G)",
        grepl("FECAL CALPROTECTIN \\(30-1800", LAB_TEST_CONCEPT_NAME, ignore.case = T) ~ "FECAL CALPROTECTIN (30-1800 uG/G)",
        TRUE ~ LAB_TEST_CONCEPT_NAME
      ))
  }

  data <- lapply(data, function(x) {
    x %>%
      setNames(gsub(" |\\.|-", "_", names(.))) %>%
      setNames(toupper(names(.)))
  })

  return(data)
}
