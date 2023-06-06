

#' Assigns data within an existing or new column based on a lookup set
#'
#' \code{jc_do_multi_col_lookups} does character data assignment in multiple
#' columns based on lookup sets specified for each column. The looked-up values
#' can be assigned into an new column or into an in-place column.
#'
#' For example, an existing column may contain long descriptive names and you
#' want to convert these to shorter codes. A lookup table for that column is
#' then used to either re-assign the values in the column (in this case the
#' 'source column' is the same as the 'target column'), or to add a new
#' column ('target column' does NOT match 'source column') that will contain the
#' shorter codes.
#'
#' This method essentially mimics a repeated VLOOKUP() in Excel, that is
#' performed over a set of specified columns.
#'
#' NOTE: Any values in the source column that are not found in the lookup set for the
#' column will be converted to NA
#'
#' @param data Data frame with raw input data in which the columns will
#' be converted
#' @param lookup_columns Data frame containing information on the columns to
#' be looked up. This dataframe must contain column names: 'source_column',
#' 'lookup_name' and 'target_column'. Here, source_column must map to a name in
#' the raw input data, and lookup_name must map to one of the lookup names
#' specified in the lookup_sets parameter
#' @param  lookup_sets Data frame containing all the lookup sets to be used for
#' the various columns.This dataframe must contain column names: 'lookup_name',
#' 'keys' and 'values'. Keys specifies the allowed values that will be looked
#' up on, and values specifies the values that will be assigned.
#' @return A data frame with the looked-up values
#' @export
#'
jc_do_multi_col_lookups <- function(data, lookup_columns, lookup_sets) {

  df <- as.data.frame(data)

  .check_required_cols(c("source_column", "lookup_name", "target_column"),
                       lookup_columns, "Lookup Columns")

  .check_required_cols(c("lookup_name",	"keys", "values"), lookup_sets,
                       "Lookup Sets")

  if ("values" %in% names(df)) {
    stop("source data cannot have column named 'values' for multi-column lookup")
  }

  lkp_val <- "keys"

  for (icol in 1:nrow(lookup_columns)) {

    row <- lookup_columns[icol, ]
    source_col <- row[["source_column"]]
    target_col <- row[["target_column"]]
    lookup_name <- row[["lookup_name"]]

    #Get lookup set for the current column
    lkp_set <- lookup_sets %>% dplyr::filter(lookup_name == {{lookup_name}}) %>%
      dplyr::select("keys", "values")

    #for setnames() see this post:
    # https://stackoverflow.com/questions/28125816/r-standard-evaluation-for-join-dplyr
    df <- df %>% dplyr::left_join(lkp_set, by = stats::setNames(lkp_val, source_col))

    # the joined-in column will have name 'values'. Now set this to the target
    # column and then remove the 'values' column
    df[ , target_col] <- df[ , "values"]
    df <- df[ , !names(df) %in% c("values")]

  }

  return(df)
}

#' Gets all invalid values in a set of columns based on a lookup set
#'
#' \code{jc_get_invalid_vals} finds all invalid values in a set of columns in
#' the source data, where 'invalid' means any value that is not listed in the
#' lookup set for that column.
#'
#' For example, if the lookup set for a column contains keys 'small', 'medium'
#' and 'large', then this method will find any values (excluding NA)  that are
#' not in that list. For example, if the column contains 'very small' and
#' 'very large' then the output will list those two values for this column.
#'
#' This method is useful to quickly find values that are approximately correct
#' so that they can be corrected rather than assigned NA values.
#'
#' @param data Data frame with raw input data to check
#' @param lookup_columns Data frame containing information on the columns to
#' be looked up. This dataframe must contain column names: 'source_column',
#' 'lookup_name' and 'target_column'. Here, source_column must map to a name in
#' the raw input data, and lookup_name must map to one of the lookup names
#' specified in the lookup_sets parameter
#' @param  lookup_sets Data frame containing all the lookup sets to be used for
#' the various columns.This dataframe must contain column names: 'lookup_name',
#' and 'keys'. Keys specifies the allowed values that will be used to find
#' invalid values
#' @return A data frame containing two columns: column 'column_name' contains
#' the name of the column in the raw data on which invalid values were found,
#' and ' invalid_values' contains all invalid values for that column
#' @export
#'
jc_get_invalid_vals <- function(data, lookup_columns, lookup_sets) {

  df <- as.data.frame(data)

  .check_required_cols(c("source_column", "lookup_name"),
                       lookup_columns, "Lookup Columns")

  # Note: for checking invalids, we only need the 'keys' column. 'values' is not
  # needed here, only in 'jc_do_multi_col_lookups'
  .check_required_cols(c("lookup_name",	"keys"), lookup_sets,
                       "Lookup Sets")
  #...BUT we add a values column so that we can see the result of the join
  # based on the key
  if (!"values" %in% names(lookup_sets)) {
    lookup_sets$values <- lookup_sets$keys
  }

  if ("values" %in% names(df)) {
    stop("source data cannot have column named 'values' for multi-column lookup")
  }

  lkp_val <- "keys"

  column_name <- c()
  invalid_values <- c()

  for (icol in 1:nrow(lookup_columns)) {

    row <- lookup_columns[icol, ]
    source_col <- row[["source_column"]]
    lookup_name <- row[["lookup_name"]]

    if (!source_col %in% names(df)) {
      stop(paste0("Lookup column '", source_col, "' not found in data set"))
    }

    #Get lookup set for the current column
    lkp_set <- lookup_sets %>% dplyr::filter(lookup_name == {{lookup_name}}) %>%
      dplyr::select("keys", "values")

    #for setnames() see this post:
    # https://stackoverflow.com/questions/28125816/r-standard-evaluation-for-join-dplyr
    df <- df %>% dplyr::left_join(lkp_set, by = stats::setNames(lkp_val, source_col))

    invalids <- unique(df[is.na(df$values), source_col])
    n_invalids <- length(invalids)
    column_name <- c(column_name, rep(source_col, n_invalids))
    invalid_values <- c(invalid_values, invalids)

    df <- df %>% dplyr::select(-c("values"))

  }

  result <- data.frame(column_name = column_name,
                       invalid_values = invalid_values)
  return(result)
}


