

#' Updates a parameter in a jcass setup file
#'
#' \code{jc_update_setup_parameter} Is a utility function that allows you to
#' update a parameter in an Excel setup file for jcass without having to open
#' the file in Excel. Ensure the file is NOT already open!
#'
#' This function allows you to dymically update parameters for example when
#' you want to run a model in a loop with different treshold parameters.
#'
#' IMPORTANT! Be careful when using this function to update a parameter in your
#' lookup lists. This function will search for the FIRST key value that matches
#' the value passed as 'setting_key'. If the values in the 'key_column' are not
#' unique, then only the first occurence's value will be updated.
#'
#' @param setup_file relative path to the setup file to update
#' @param sheet_name name of the sheet that holds the data
#' @param setting_key unique KEY that identifies the setting value to be updated
#' @param setting_value new value to set for this setting
#' @param key_column name of the column that contains the key on which to look
#' up the row
#' @param value_column name of the column that holds the value to be updated.
#' @export
#' @importFrom readxl excel_sheets
#'
jc_update_setup_parameter <- function(setup_file, sheet_name,
                                      setting_key, setting_value,
                                      key_column = "setting_name",
                                      value_column = "value") {

  df <- read_xlsx(setup_file)
  sheet_names <- excel_sheets(setup_file)
  sheet_index <- match(sheet_name, sheet_names)

  start_row <- which(df[, key_column] == setting_key)
  start_row <- start_row + 1  # Adjust to allow for header

  value_col_index <- which(names(df) == value_column)

  wb <- xlsx::loadWorkbook(setup_file)
  sheets <- xlsx::getSheets(wb)
  sheet <- sheets[[sheet_index]]  # or another
  values <- c(setting_value)
  xlsx::addDataFrame(values, sheet, col.names = FALSE, row.names = FALSE,
                     startRow = start_row, startColumn = value_col_index)
  xlsx::saveWorkbook(wb, setup_file)
}
