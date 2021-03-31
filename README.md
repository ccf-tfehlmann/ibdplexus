
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ibdplexus

<!-- badges: start -->

<!-- badges: end -->

## Overview

ibdplexus is designed to work with data downloaded from the [IBD Plexus
Portal](https://ibdplexus.org) from the Crohn’s & Colitis Foundation. It
includes functions for the cohorts RISK, SPARC and QORUS. More
information on Plexus and the cohorts can be found
[here](https://www.crohnscolitisfoundation.org/research/current-research-initiatives/ibd-plexus).
Functions include:

  - `load_data()` loads unzipped data from Plexus
  - `load_zipped_data()` loads zipped data from Plexus
  - `risk_extract_clinical_data()` creates a summary table by visit for
    RISK
  - `qorus_summary()` creates a summary table for QORUS
  - `sparc_summary()` creates a summary table for SPARC
  - `calculate_disease_scores()` calculates disease scores for SPARC
  - `sparc_scores()` calculates disease scores at a specific time for
    SPARC
  - `sparc_medication()` predicts the medication a SPARC participant is
    on at a specific time

## Installation

You can install the released version of ibdplexus from
[github](https://github.com/ccf-tfehlmann/ibdplexus) with:

``` r
# install.packages("devtools")
devtools::install_github("ccf-tfehlmann/ibdplexus")
```

## User Guides

  - <a href="https://github.com/ccf-tfehlmann/ibdplexus/blob/master/Calculating%20Disease%20Activity%20Scores%20for%20SPARC.pdf">`calculate_disease_scores()`</a>

  - <a href="https://github.com/ccf-tfehlmann/ibdplexus/blob/master/SPARC_Summary_userguide.pdf">`sparc_summarry()`</a>

  - <a href="https://github.com/ccf-tfehlmann/ibdplexus/blob/master/sparc_medication_userguide.pdf">`sparc_medication()`</a>

  - <a href="https://github.com/ccf-tfehlmann/ibdplexus/blob/master/sparc_scores_at_index_userguide.pdf">`sparc_scores()`</a>

## Example

``` r
library(ibdplexus)

## loading unzipped SPARC Encounter, Demographics and Prescriptions CRF data only ----
# data = load_data(datadir = ".",cohort = "SPARC", domains = c("Demographics", "Prescriptions", "Encounter"), data_type = "CRF")


## sparc medication at the time of endoscopy (+/- 30 days) with indication ----
# 
#   data = load_data(datadir = "~/r_input/",cohort = "SPARC", domains = c("Procedures", "Encounter"), data_type = "BOTH")
# 
#   endoscopy = data$procedures %>% 
#     filter(PROC_CONCEPT_NAME %in% c("Colonoscopy/Sigmoidoscopy")) %>%
#     distinct(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_ID, PROC_START_DATE, LOCATION, INDICATION) %>%
#     left_join(data$encounter) %>%
#     mutate(PROC_START_DATE = dmy(PROC_START_DATE), 
#            VISIT_ENCOUNTER_START_DATE = dmy(VISIT_ENCOUNTER_START_DATE)) %>%
#     rename(endoscopy_date = PROC_START_DATE) %>%
#     drop_na(endoscopy_date) %>%
#     select(DEIDENTIFIED_MASTER_PATIENT_ID, VISIT_ENCOUNTER_START_DATE, endoscopy_date, LOCATION, INDICATION) %>%
#     rename(index_date = endoscopy_date) %>%
#     distinct(DEIDENTIFIED_MASTER_PATIENT_ID, index_date, INDICATION) %>%
#     group_by(DEIDENTIFIED_MASTER_PATIENT_ID, index_date) %>%
#     fill(INDICATION, .direction = "downup") %>%
#     slice(1) %>%
#     ungroup() %>%
#     distinct()
#   
#   medication_at_endo = sparc_medication(datadir = "~/r_input/",
#                              index_info = endoscopy,
#                              filename = "SPARC_MEDICATION_AT_ENDOSCOPY.xlsx",
#                              index_range = "30")
```