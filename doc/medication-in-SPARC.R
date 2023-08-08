## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

options(rmarkdown.html_vignette.check_title = FALSE)

## ----eval = TRUE, warning = FALSE, message = FALSE----------------------------
library(ibdplexus, quietly = T)
library(tidyr, quietly = T)
library(dplyr, quietly = T)
library(lubridate, quietly = T)

# Here is the table of the medication types in med_grp
knitr::kable(med_grp %>% distinct(med_type))

# Here is the first 5 rows of the med_grp data.frame

knitr::kable(head(med_grp, 5))

# Here is how Infliximab and its' bio-similars are mapped

knitr::kable(med_grp %>% filter(grepl("Infliximab", new_med_name, ignore.case = T)))

## -----------------------------------------------------------------------------
#  # Load EMR and eCRF data necessary for sparc_med_filter
#  
#  data <- load_data(datadir = "~/r_input/", cohort = "SPARC", domains = c("prescriptions", "observations", "demographics", "encounter"), data_type = "Both")
#  
#  # Filter SPARC IBD data for biologics & immunomodulators
#  
#  meds_of_interest <- sparc_med_filter(
#    data$prescriptions,
#    data$observations,
#    data$demographics,
#    data$encounter,
#    med_groups = c("Biologic", "Immunomodulators")
#  )

## -----------------------------------------------------------------------------
#  # Find Medication Start Dates for Biologics and Immunomodulators.
#  # If export = TRUE then an excel spreadsheet is generated.
#  
#  medication_starts <- sparc_med_starts(
#    data$prescriptions,
#    data$observations,
#    data$demographics,
#    data$encounter,
#    med_groups = c("Biologic", "Immunomodulators"),
#    export = TRUE
#  )

## -----------------------------------------------------------------------------
#  # Load Data needed for sparc_medication function
#  
#  data <- load_data(datadir = "~/r_input/", cohort = "SPARC", domains = c("demographics", "diagnosis", "encounter", "procedures", "observations", "biosample", "omics_patient_mapping", "prescriptions"), data_type = "Both")
#  
#  # Find Biologics and Immunomodulators a patient is on at enrollment.
#  
#  med_at_enrollment <- sparc_medication(
#    data = data,
#    index_info = "Enrollment",
#    med_groups = c("Biologic", "Immunomodulators"),
#    filename = "SPARC_MEDICATION_AT_ENROLLMENT.xlsx"
#  )
#  
#  # Find Biologics and Immunomodulators a patient is on at endoscopy
#  
#  med_at_enrollment <- sparc_medication(
#    data = data,
#    index_info = "Endoscopy",
#    med_groups = c("Biologic", "Immunomodulators"),
#    filename = "SPARC_MEDICATION_AT_ENDOSCOPY.xlsx"
#  )

