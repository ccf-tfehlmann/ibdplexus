% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_bmi.R
\name{calculate_bmi}
\alias{calculate_bmi}
\title{calculate_bmi}
\usage{
calculate_bmi(observations)
}
\arguments{
\item{observations}{A dataframe with demographics data.}
}
\value{
A long dataframe with the deidentified_master_patient_id, date of bmi measurement and bmi.
}
\description{
Calculate BMI from electronic medical record (EMR) data for SPARC & QORUS.
}
\details{
For patients with more than 1 bmi, outliers are removed. Weight is converted to kg and height is convereted to meters.
}
