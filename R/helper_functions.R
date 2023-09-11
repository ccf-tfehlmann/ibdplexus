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
#' Function to reshape table into a wide format.
#'
#' @param df dataframe
#' @param y.var column names to be transposed
#' @param value.var values to be transposed
#'
#' @return transposed data
to_wide <- function(df, y.var, value.var) {
  x.vars <- c("DEIDENTIFIED_MASTER_PATIENT_ID", "DEIDENTIFIED_PATIENT_ID", "DATA_SOURCE", "VISIT_ENCOUNTER_ID")
  x.var <- names(df)[names(df) %in% x.vars]
  all.var <- c(x.var, y.var)
  if (y.var == value.var) {
    df %>% select(all_of(all.var))
  } else {
    d <- pivot_wider(df,
      id_cols = all_of(x.var), names_from = all_of(y.var),
      values_from = all_of(value.var), values_fn = ~ paste0(.x, collapse = "; ")
    )
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
#' @param ... Passed to quantile
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




#' folder_fix
#'
#' add "/" to end of folder name for functions
#'
#' @param folder The folder specified for the function
#'
#' @return the folder name in the correct format
folder_fix <- function(folder) {
  folder <- if (endsWith(folder, "/")) {
    folder
  } else {
    paste0(folder, "/")
  }
}


#' fix_col_names
#'
#' standardize column names for summary tables
#'
#' @param df The dataframe created for the summary table
#'
#' @return The dataframe with corrected names
fix_col_names <- function(df){

  # get current names, make all uppercase
  names_start <- names(df) %>%
    str_to_upper()

  # replace periods followed by letters with "_", leave periods when specifying volume
  names_fix <- NA

  for (i in names_start){
    if (grepl("\\.[a-zA-Z]", i)) {

      names_fix[i] <- gsub("\\.", "_", i)

    } else {

      names_fix[i] <- i

    }
  }

  # replace multiple instances of underscores and replace spaces with underscores
  names_final <- names_fix[2:length(names_fix)] %>%
    unname() %>%
    str_replace("__", "_") %>%
    str_replace("___", "_") %>%
    str_replace_all(" ", "_") %>%
    as.tibble()

  # rename old column names new column names
  df_colnames <- tibble(
    "new_name" = names_final$value,
    "old_name" = names(df)
  )

  var_names <- deframe(df_colnames)

  df_new <- df %>%
    rename(!!!var_names)

  return(df_new)

}
