
#' Convert a set of columns to a specific type
#'
#' \code{jc_convert_cols} explicitly converts a set of columns in a data frame
#' to a specific type.
#'
#' @param df data frame to convert
#' @param cols_to_convert vector containing the names of columns to convert
#' @param conv_type conversion type. default is 'numeric'. Can also handle
#' 'character' and 'date'
#' @return data frame with updated column types
#' @export
#'
jc_convert_cols <- function(df, cols_to_convert, conv_type = "numeric") {

  conv_type <- tolower(conv_type)
  df <- as.data.frame(df)
  col_names <- names(df)

  for (col in cols_to_convert) {

    if (!col %in% col_names) {
      stop(paste0("Column name '", col,  "' not found in data frame"))
    }

    if (startsWith(conv_type, "num")) {

      df[ , col] <- as.numeric(df[ , col])

    } else if (conv_type %in% c("string", "char", "character", "txt", "text")) {

      df[ , col] <- as.character(df[ , col])

    } else if (conv_type %in% c("date", "datetime")) {

      df[ , col] <- as.Date(df[ , col])

    } else {

      stop(paste0("Column conversion type '", conv_type, "' is not handled"))

    }
  }

  return(df)

}

#' Utility function to get number and percentage of NAs in each column
#'
#' \code{jc_get_NA_percentages} gets the number of NA values in each column and
#' also converts this to a percentage of NA based on the total number of rows in
#' df
#' @param df data frame to evaluate
#' @param cols_to_check columns to check or pass in NULL to check all columns
#' @return data frame with columns:
#' \enumerate{
#'   \item 'column' containing the column name
#'   \item 'na_count' containing the number of NA values in each column
#'   \item 'na_percent' containing the percentage of NA values in each column
#' }
#' @export
#'
jc_get_NA_percentages <- function(df, cols_to_check = NULL) {
  if (is.null(cols_to_check)) {
    cols_to_check <- names(df)
  }

  n_rows <- nrow(df)
  n_cols <- length(cols_to_check)
  cols <- rep(NA, n_cols)
  na_counts <- rep(NA, n_cols)
  na_percents <- rep(NA, n_cols)

  i <- 1
  for (col in cols_to_check) {

    col_data <- df[, col]
    na_count <- sum(is.na(col_data))
    na_perc <- 100 * na_count/n_rows

    cols[i] <- col
    na_counts[i] <- na_count
    na_percents[i] <- na_perc

    i <- i + 1
  }

  result <- data.frame(column = cols, na_count = na_counts,
                       na_percent = na_percents)
  return(result)
}

#' Gets a boolean value from a general value
#'
#' \code{jc_get_boolean} returns TRUE or FALSE based on the following
#' conditions:
#' \enumerate{
#'   \item if value is NULL or NA returns FALSE
#'   \item if value as numeric is 1 returns TRUE
#'   \item if value as numeric is 0 returns FALSE
#'   \item if lowercase of value is any of 'true', 't', 'yes' or 'y':
#'   return TRUE
#'   \item if lowercase of value is any of 'false', 'f', 'no' or 'n':
#'   return FALSE
#'   \item if length of value as character after trimming is 0, return FALSE
#'   \item if none of the above, return TRUE
#' }
#' @param value value (character or number) to evaluate
#' @return TRUE or FALSE
#' @export
#'
jc_get_boolean <- function(value) {
  suppressWarnings({

    if (is.null(value)) return(FALSE)
    if (is.na(value)) return(FALSE)

    txt_value <- tolower(stringr::str_trim(as.character(value), side="both"))
    if (nchar(value) == 0) return(FALSE)

    #put most likely incoming values at top
    if (txt_value == "1") return(TRUE)
    if (txt_value == "true") return(TRUE)

    if (txt_value == "0") return(FALSE)
    if (txt_value == "false") return(FALSE)

    if (txt_value == "no") return(FALSE)
    if (txt_value == "yes") return(TRUE)

    if (txt_value == "t") return(TRUE)
    if (txt_value == "f") return(FALSE)

    if (txt_value == "n") return(FALSE)
    if (txt_value == "y") return(TRUE)


    #If we get here, check for numeric
    num_val <- as.numeric(value)
    if (is.na(num_val)) return(FALSE)
    if (num_val > 0) return(TRUE)

  })
  return(FALSE)
}

#' Does secure lookup on a lookup set
#'
#' \code{jc_get_lookup_value} Finds the value in a required column of a
#' specified lookup table. If either the lookup table key or the lookup column
#' does not exist an informative error is thrown.
#'
#' @param lookups List object containing lookup sets read with method
#' \code{jc_get_lookups}
#' @param lookup_set_key key that identifies the required lookup set. If this
#' key is not found in the names of parameter lookups, an error is thrown
#' @param key_value key that identifies the row on which the value should be
#' retrieved. If the key does not exist, an error is thrown
#' @param key_column column that contains the key on which to look up on. If
#' the column does not exist, an informative error is thrown
#' @param lookup_column column on which to look up they value on. If the column
#' does not exist, an informative error is thrown
#' @export
#'
jc_get_lookup_value <- function(lookups, lookup_set_key, key_value,
                                key_column = "key", lookup_column = "value") {

  if(!lookup_set_key %in% names(lookups)) {
    stop(paste0("Cannot find lookup set '", lookup_set_key, "'"))
  }


  lkpSet <- lookups[[lookup_set_key]]
  if (!key_column %in% names(lkpSet)) {
    stop(paste0("Cannot find key column '", key_column, "' in lookup set '",
                lookup_set_key, "'"))
  }

  if (!lookup_column %in% names(lkpSet)) {
    stop(paste0("Cannot find required column '", lookup_column,
    "' in lookup set '", lookup_set_key, "'"))
  }

  values <- lkpSet[lkpSet[, key_column] == key_value, lookup_column]
  if (is.na(values) || length(values) == 0) {
    stop(paste0("Cannot find value matching key '", key_value,
    "in lookup set '", lookup_set_key, "'"))
  }

  if (length(values) > 1) {
    warning(paste0("More than one value matching key '", key_value,
                   "in lookup set '", lookup_set_key, "'"))
  }

  return(values[[1]])

}

.logmessage <- function(parts, dividers = 0, divider_before = TRUE,
                        log_mode = "detail", log_type = "detail") {

  if (log_mode != log_type) return()

  if (divider_before) {
    if (dividers > 0) {

      for (i in 1:dividers) {
        message(rep("-",80))
      }
    }
    msg <- paste0(parts)
    message(msg)
  }
  else {
    msg <- paste0(parts)
    message(msg)
    if (dividers > 0) {

      for (i in 1:dividers) {
        message(rep("-",80))
      }
    }
  }
}


.getperiod_from_calendar <- function(model, period_calendar) {
  return (period_calendar - model$init_cal_epoch)
}

.getepoch_from_calendar <- function(model, period_calendar) {
  return (period_calendar - model$init_cal_epoch)
}

.check_required_cols <- function(required_cols, df, df_label) {
  cols_on_df <- names(df)
  ierrors <- 0
  for (col in required_cols) {
    if (! col %in% cols_on_df) {
      .logmessage(c("Required column '", col, "' not found in ", df_label,
                  " data;"))
      ierrors <- ierrors + 1
    }
  }
  if (ierrors == 0) return(TRUE)
  return(FALSE)
}

.get_period_from_epoch_key <- function(key) {
  # Note: since epoch_1 follows period 1, the result returned for
  # 'epoch_0' will be period = 0, for 'epoch_1' = period 1 etc
  tryCatch(
    {
      vals <- unlist(strsplit(key, "_", fixed = TRUE))
      epoch <- as.numeric(vals[2])
      period <- epoch
      return(period)
    },
    error = function(cond) {
    errmsg <- paste0("Error parsing period from key ", key, "; Details: ", cond)
    stop(errmsg)
    })
}

.get_scale_factor = function(scale_factor) {
  result <- switch(
  #Note: for ggplot scales::label_xxx scale factor is inverse
  scale_factor,
  "1"= 1,
  "10k"= 1/10000,
  "100k"= 1/10000,
  "million"= 1/1e6,
  "mil"= 1/1e6,
  "mill"= 1/1e6
 )
  return(result)
}

.get_scale_label = function(scale_factor) {
  result <- switch(
  scale_factor,
  "1"= "",
  "10k"= "(10 thou)",
  "100k"= "(100 thou)",
  "million"= "(million)",
  "mil"= "(million)",
  "mill"= "(million)"
  )
  return(result)
}

.jc_get_string <- function(string_value, to_lower = FALSE) {

  string_value <- stringr::str_trim(string_value, side = "both")
  if (is.na(string_value)) return(NA)
  if (string_value == "") return(NA)
  if (to_lower) string_value = tolower(string_value)
  return(as.character(string_value))
}


.jc_check_column_name_ok <- function(name, context_label, stop_if_false = TRUE) {

  #TODO: Check if name starts with a number

  browser()
  #First check for spaces
  if (grepl(" ", name, fixed = TRUE)) {
    if (stop_if_false) {
      stop("Column name '", name, "' is not valid. Name cannot contain spaces.",
           "Check: ", context_label)
    }
    else {
      return(FALSE)
    }
  }

  #TODO: Freaking not working! Fix
  #Check for special characters
  pattern <- "/|:|?|<|>|$|#|@|!|%|^|&|*|(|)|-|~|`|+|\\*"
  if (grepl(pattern, name)) {
    if (stop_if_false) {
      stop("Column name '", name, "' is not valid. Name cannot special ",
           "characters. Check: ", context_label)
    }
    else {
      return(FALSE)
    }
  }

  return(TRUE)


}

.jc_check_df_ids_match_ok <- function(df1, df2) {
  return(identical(df1$elem_id, df2$elem_id))
}

# .jc_get_maint_category <- function(model) {
#   # Get the budget category associated with the designated routine maintenance
#   # treatment
#   treats <- model$treatments
#
#   maint_treat_cat <- model$get_generalparam("routine_maint_treatment")
#   if (maint_treat_name != "none") {
#     treat_row <- treats[treats$treatment_name == maint_treat_name, ]
#     maint_cat <- treat_row[[1, "category"]]
#     return(maint_cat)
#   }
#   else {
#     return("none")
#   }
# }
