# LOAD HELPER FUNCTIONS ----




#' read_data
#'
#' Function to read data tables into a list and combine tables of each type. From rancho.
#'
#' @param df dataframe
#'
#' @return  data from csv/txt files
read_data <- function(df) {
  data <- lapply(df, function(x) read.csv(x, stringsAsFactors = F, na.strings = c(NA, "", "NA"), encoding = "UTF-8"))
  data <- do.call(rbind, data)
}


#' remove_empty_cols
#'
#' Function to remove empty columns from a table. From rancho.
#'
#' @param df dataframe
#'
#' @return non empty columns
remove_empty_cols <- function(df) {
  df[, colSums(!is.na(df)) != 0]
}


#' to_wide
#'
#' Function to reshape table into a wide format. From rancho.
#'
#' @param df dataframe
#' @param y.var column names to be transposed
#' @param value.var values to be transposed
#'
#' @return transposed data
to_wide <- function(df, y.var, value.var) {
  x.vars <- c("DEIDENTIFIED_MASTER_PATIENT_ID", "DEIDENTIFIED_PATIENT_ID", "DATA_SOURCE", "VISIT_ENCOUNTER_ID")
  x.var <- names(df)[names(df) %in% x.vars]
  if (y.var == value.var) {
    df %>% select(x.var, y.var)
  } else {
    d <- reshape2::dcast(df, paste0(paste(x.var, collapse = " + "), "~", y.var), value.var = value.var, fun.aggregate = function(x) paste(unique(x), collapse = "; "))
  }
}


#' proper
#'
#' Makes string with proper capitalization
#'
#' @param x string
#'
#' @return string
#'
proper <- function(x) {
  paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
}



#' remove outliers
#'
#' remove outliers from BMI
#'
#' @param x bmi
#' @param na.rm default is true
#'
#' @return bmi
#'
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs = c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
