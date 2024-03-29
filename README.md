
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ibdplexus

<!-- badges: start -->
<!-- badges: end -->

## Overview

ibdplexus is designed to work with data downloaded from the [IBD Plexus
Portal](https://ibdplexus.org) from the Crohn’s & Colitis Foundation.
More information on Plexus and the cohorts can be found
[here](https://www.crohnscolitisfoundation.org/research/current-research-initiatives/ibd-plexus).
Functions include:

- `load_data()` loads unzipped data from Plexus
- `load_zipped_data()` loads zipped data from Plexus
- `risk_summary()` creates a summary table by visit for RISK
- `sparc_summary()` creates a summary table for SPARC
- `calculate_disease_scores()` calculates disease scores for SPARC
- `sparc_scores()` calculates disease scores at a specific time for
  SPARC
- `sparc_medication()` predicts the medication a SPARC participant is on
  at a specific time
- `emr_extract_diagnosis()` subsets the plexus extract based on
  specified ICD10 codes

## Installation

You can install the released version of ibdplexus from
[github](https://github.com/ccf-tfehlmann/ibdplexus) with:

``` r
# install.packages("devtools")
devtools::install_github("ccf-tfehlmann/ibdplexus")
```

## User Guides

- <a href="https://github.com/ccf-tfehlmann/ibdplexus/blob/master/inst/userguides/sparc_summary-overview.md">`sparc_summary()`</a>

- <a href="https://github.com/ccf-tfehlmann/ibdplexus/blob/master/inst/userguides/medication-in-SPARC.md">`sparc_medication()`</a>

- <a href="https://github.com/ccf-tfehlmann/ibdplexus/blob/master/inst/userguides/risk-summary.md">`risk_summary()`</a>

- <a href="https://github.com/ccf-tfehlmann/ibdplexus/blob/master/inst/userguides/Diagnosis_EMR_Extract.md">`Diagnosis_EMR_Extract`</a>

## Example

``` r
library(ibdplexus)

# Load in all data in IBD Plexus for the SPARC cohort ----
data <- load_data(datadir = "~/r_input/", cohort = "SPARC", domains = "ALL", data_type = "BOTH")


# Get Excel Tables and Data.Frames with Data Summarised within 60 days of the Enrollment Time Point ----

# SPARC Summary Table
e_sum <- sparc_summary(
  data = data,
  index_info = "ENROLLMENT",
  filename = "SPARC_SUMMARY_ENROLLMENT.xlsx",
  index_range = "60"
)


# SPARC Scores

e_scores <- sparc_scores(
  data = data,
  index_info = "ENROLLMENT",
  filename = "SPARC_SCORES_ENROLLMENT.xlsx",
  index_range = "60"
)


# SPARC Medication

e_med <- sparc_medication(
  data = data,
  index_info = "ENROLLMENT",
  filename = "SPARC_MEDICATION_ENROLLMENT.xlsx"
)
```
