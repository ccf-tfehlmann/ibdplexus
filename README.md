
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

-   `load_data()` loads unzipped data from Plexus
-   `load_zipped_data()` loads zipped data from Plexus
-   `risk_extract_clinical_data()` creates a summary table by visit for
    RISK
-   `qorus_summary()` creates a summary table for QORUS
-   `sparc_summary()` creates a summary table for SPARC
-   `calculate_disease_scores()` calculates disease scores for SPARC
-   `sparc_scores()` calculates disease scores at a specific time for
    SPARC
-   `sparc_medication()` predicts the medication a SPARC participant is
    on at a specific time
-   `sparc_emr()` subsets the plexus extract based on specified ICD10,
    LONIC or CPT codes

## Installation

You can install the released version of ibdplexus from
[github](https://github.com/ccf-tfehlmann/ibdplexus) with:

``` r
# install.packages("devtools")
devtools::install_github("ccf-tfehlmann/ibdplexus")
```

## User Guides

-   <a href="https://github.com/ccf-tfehlmann/ibdplexus/blob/master/userguides/Calculating_Disease_Activity_Scores_for_SPARC.pdf">`calculate_disease_scores()`</a>

-   <a href="https://github.com/ccf-tfehlmann/ibdplexus/blob/master/userguides/SPARC_Summary_userguide_updated_102221.pdf">`sparc_summary()`</a>

-   <a href="https://github.com/ccf-tfehlmann/ibdplexus/blob/master/userguides/sparc_medication_at_index_userguide_updated_102721.pdf">`sparc_medication()`</a>

-   <a href="https://github.com/ccf-tfehlmann/ibdplexus/blob/master/userguides/sparc_scores_at_index_userguide.pdf">`sparc_scores()`</a>

-   <a href="https://github.com/ccf-tfehlmann/ibdplexus/blob/master/userguides/RISK_Extract_Clinical_Data_User_Guide_v1.1.pdf">`risk_extract_clinical_data()`</a>

## Example

``` r
library(ibdplexus)

## loading unzipped SPARC Encounter, Demographics and Prescriptions CRF data only ----
# data = load_data(datadir = ".",cohort = "SPARC", domains = c("Demographics", "Prescriptions", "Encounter"), data_type = "CRF")


## sparc medication at the time of endoscopy (+/- 30 days) with indication ----
# 
#   data = load_data(datadir = "~/r_input/",cohort = "SPARC", domains = c("ALL"), data_type = "BOTH")
# 
#   
#   medication_at_endo = sparc_medication(data = data,
#                              index_info = "endoscopy",
#                              filename = "SPARC_MEDICATION_AT_ENDOSCOPY.xlsx")
```
