% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_calculations.R
\name{calculate_cd_phenotype}
\alias{calculate_cd_phenotype}
\title{calculate_cd_phenotype}
\usage{
calculate_cd_phenotype(
  observations,
  disambiguation_fun = disambiguation,
  return_intermediate_results = FALSE
)
}
\arguments{
\item{observations}{observations table usually generated using load_data}

\item{disambiguation_fun}{a function used to disambiguate occurrences of multiple
records for the same patient on the same date. The function is expected to
take a character vector of any length and return a vector of length one.
It will be applied to vectors of DESCRIPTIVE_SYMP_TEST_RESULTS, all elements
of the vector will be derived from a single OBS_TEST_CONCEPT_NAME.}

\item{return_intermediate_results}{if TRUE columns that were used to calculate
the final CD locations will be returned.}
}
\value{
A dataframe with all Crohn's disease phenotypes that could be
derived from the data regardless of IBD diagnosis.
}
\description{
Calculates Crohn's disease phenotypes from SPACR data
}
\details{
This function not only considers the recorded values for the Crohn's disease phenotype, but also explicit records that indicate the presence of a fistula or stricture.
}
