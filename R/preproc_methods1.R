
#' Checks for a list of Required Columns in a data frame
#'
#' \code{jc_check_req_cols} checks a list of columns to see if all columns are
#' present in a source data set
#'
#' @param data Data frame with source data on which the columns will be checked
#' @param required_cols Character vector containing the list of required columns
#' @param loud If TRUE, warning messagea are shown listing missing columns, '
#' if any
#' @return TRUE if all required columns are present, otherwise FALSE
#' @export
#'
jc_check_req_cols <- function(data, required_cols, loud = TRUE) {
  req_cols <- as.vector(required_cols)
  df <- as.data.frame(data)
  avail_cols <- as.vector(names(df))
  fails <- 0
  for (col in req_cols) {
    if (!col %in% avail_cols)
    {
      .logmessage(c("Required column '", col, "' not available in raw data"))
      fails <- fails + 1
    }
  }
  if (fails == 0) {
    .logmessage("Required columns check PASSED;")
    return(TRUE)
  }

  .logmessage(c("Required columns check FAILED; ", fails,
              " required column(s) not found"), 1)
  return(FALSE)
}


#' Clamps values in numeric columns to specified minimum and maximum values
#'
#' \code{jc_clamp_columns} Loops over a set of specified columns, each with an
#' accompanying minimum and maximum allowed. For each column, any values outside
#' the specified minimum or maximum are clamped to the minimum or maximum,
#' respectively.
#'
#' This method will automatically convert all of the columns listed in
#' the dataframe \code{min_max_values} to numeric type, and any text values
#' found will be converted to NA.
#'
#' @param data Data frame with source data in which the columns will be clamped
#' @param min_max_values Data frame containing information on the columns to
#' be clampled. This dataframe must contain column names: 'column_name',
#' 'min_allowed' and 'max_allowed' (case sensitive) with matching values mapping
#' to the column name in the source data (assumed to be a numeric column).
#' @return A data frame identical to the source data but with values clamped to
#' the minimums and maximums for the specified columns
#' @export
#'
jc_clamp_columns <- function(data, min_max_values) {

  .check_required_cols(c("column_name", "min_allowed", "max_allowed"),
                       min_max_values, "Column Clamp Setup")

  df <- as.data.frame(data)

  for (icol in 1:nrow(min_max_values)) {

    row <- min_max_values[icol, ]
    col_name <- as.character(row[["column_name"]])
    min <- as.numeric(row[["min_allowed"]])
    max <- as.numeric(row[["max_allowed"]])

    if (!col_name %in% names(df)) {
      stop(paste0("Column to be clamped '", col_name, "' is not in source data"))
    }

    if (min >= max) {
      stop(paste0("Minimum allowed must be greater than maximum allowed; ",
                  "Check setup for column '", col_name, "'"))
    }

    df <- .clamp_column(df, col_name, min, max)

  }
  return(df)

}


#' Converts columns to specified types
#'
#' \code{jc_type_columns} Loops over a set of specified columns, each with an
#' accompanying data type specifier. Columns in the raw data set are then
#' converted to the specified type. Any columns in the raw input data that are
#' not listed in the columns parameter are left untouched.
#'
#' For numeric columns, and any text values found in the column will be
#' automatically converted to NA.
#'
#' @param data Data frame with raw input data in which the columns will
#' be converted
#' @param columns Data frame containing information on the columns to
#' be converted. This dataframe must contain column names: 'column_name' and
#' 'data_type'. Column names specified here must match (case sensitive) the
#' column names in the raw input data otherwise an error will be thrown
#' @return A data frame identical to the source data but with columns specified
#' in the \code{columns} parameter converted to the specified types.
#' @export
#'
jc_type_columns <- function(data, columns)  {

  .check_required_cols(c("column_name", "data_type"),
                       columns, "Column Clamp Setup")
  df <- as.data.frame(data)

  ncols <- nrow(columns)
  for (icol in 1:ncols) {

    row <- columns[icol, ]
    col_name <- row[["column_name"]]
    if (!col_name %in% names(df)) {
      stop(paste0("Column '", col_name, "' not found in raw data"))
    }

    col_type <- tolower(row[["data_type"]])

    suppressWarnings(

      if (startsWith(col_type, "num")) {
        df[ , col_name] <- as.numeric(df[ , col_name])
      }
      else if (startsWith(col_type, "text") | startsWith(col_type, "str") |
          startsWith(col_type, "char")) {
        df[ , col_name] <- as.character(df[ , col_name])
      }
      else {
        stop(paste0("Data type '", col_type, "' for Column '", col_name,
                    "' is not handled. Specify 'text' or 'numeric'"))
      }
    )
  }

  return(df)

}


#' Creates a subset that retains only a certain number of columns
#'
#' \code{jc_get_subset} returns a subset of the input data that contains only
#' the specified number of columns
#'
#' @param data Data frame with source data on which the columns will be checked
#' @param columns Character vector containing the list of columns to retain. If
#' @return data frame with only the required columns retained
#' @export
#'
jc_get_subset <- function(data, columns) {
  retain_cols <- as.vector(columns)
  df <- as.data.frame(data)
  avail_cols <- as.vector(names(df))
  fails <- 0
  for (col in retain_cols) {
    if (!col %in% avail_cols)
    {
      stop(paste0("Column '", col, "' to be retained is not available in data"))
    }
  }
  df <- df %>% dplyr::select(dplyr::all_of(retain_cols))
  return(df)
}


.clamp_column <- function(df, col_name, min, max) {

  # use clamp() function in terra package. The parameter 'values = TRUE'
  # ensures that Values outside the limits outside values get the min/max value
  suppressWarnings(df[ , {{col_name}}] <- as.numeric(df[ , {{col_name}}]))
  df[ , {{col_name}}] <- terra::clamp(df[ , {{col_name}}],
                                      lower = min, upper = max,
                                      values = TRUE)
  return(df)
}

