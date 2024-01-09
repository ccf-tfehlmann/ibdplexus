#' count_patients
#'
#' Count unique Sparc and Qorus Patients in a dataframe using unique MPI.
#'
#' @param data A dataframe containing SPARC/Qorus data of any type.
#'
#' @return Numeric result containing number of patients
#' @export


count_patients <- function(
    data) {
  return(length(unique(data$DEIDENTIFIED_MASTER_PATIENT_ID)))
}
