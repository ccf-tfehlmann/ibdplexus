% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diagnosis_emr_extract.R
\name{custom_exclusions}
\alias{custom_exclusions}
\title{custom_exclusions}
\usage{
custom_exclusions(
  emr_filtered,
  exclusion1 = NULL,
  datecutoff = NULL,
  source_column = NULL
)
}
\arguments{
\item{emr_filtered}{A data frame of filtered emr data produced in extract_emr_diagnosis (either dxemrid, ppemricd, phemricd)}

\item{exclusion1}{diagnosis category of ICD 10 codes to exclude from search. All uppercase.}

\item{datecutoff}{Type of date cutoff to be used for EMR extract. Current options include "ENROLLMENT" - number of days before consent into study.}

\item{source_column}{The source code column relevant for the diagnosis, patient_problem or patient_history table.}
}
\value{
The filtered data frames of emr data without any diagnosis that are in the exclusion category.
}
\description{
A function that creates custom exclusion categories of ICD 10 codes for extract_emr_diagnosis_function. Add new custom exclusion categories requested to this function.
}
