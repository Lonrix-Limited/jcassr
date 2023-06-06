

#' Set's up a dictionary of Lookup tables for fast lookup of constants or
#' control parameters
#'
#' This method reads a set of lookups from Excel and builds a \code{list} object
#' with keys/attribute names matching the sheet names. Lookups are only read
#' from sheets starting with 'lkp_'.
#'
#' Each lookup key (sheet name) in the returned object maps to a dataframe in
#' which the row names match the values read from the left-most column. Values
#' in other columns can then be lookup up (assuming exact matches) in any of the
#' other columns.
#'
#' The method getlookupvalue() on the model ('jc_model') object can be used to
#' conveniently find the value mapping to a specific lookup set, key and column.
#'
#' @param lkp_url URL for the Excel file containing the lookup sheets
#' @param is_local Flag to indicate if the source URL maps to a local file (TRUE),
#' or a file on the web
#' @return \code{list} object with keys mapping to lookup set names, and values
#' containing data frames that can be lookup up on
#' @export
#'
jc_get_lookups <- function(lkp_url, is_local = TRUE) {

  lkp.file <- lkp_url
  if (is_local == FALSE) {
    httr::GET(lkp_url, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
    lkp.file <- tf
    unlink(tf)
  }

  # read excel sheets
  sheets <- readxl::excel_sheets(lkp.file)
  lookup.sheets <- sheets[grep("^[lkp_].*", sheets)]
  lookups <- lapply(lookup.sheets, function(sheet)
    readxl::read_excel(lkp.file, sheet = sheet)
  )
  names(lookups) <- lookup.sheets

  lkp.keys <- names(lookups)
  for (lkp.key in lkp.keys) {
    lkp <- lookups[[lkp.key]]
    names(lkp)[1] <- "key"
    lookups[[lkp.key]] <- lkp
  }

  class(lookups) <- "jc_lookup_set"

  return(lookups)
}


#' Loads data on sheets in an Excel file into a List of data frames
#'
#' This method reads a set of data frames from Excel and builds a \code{list}
#' object in which the keys (attribute names) match the sheet names. The sheets
#' from which to read are identified by a specified prefix.
#'
#' For example, to read the data from all sheets starting with "model_", you
#' should provide the prefix 'model_" as a parameter. The data on each sheet
#' should be a monolithic block of data with top-left corner in cell A1, and
#' with valid column names#'
#'
#' @param lkp_url URL for the Excel file containing the lookup sheets
#' @param sheet_prefix Prefix that identifies sheets to read data from
#' @param is_local Flag to indicate if the source URL maps to a local file (TRUE),
#' or a file on the web
#' @return \code{list} object with keys mapping to lookup set names, and values
#' containing data frames
#' @export
#'
jc_get_sheets_data <- function(lkp_url, sheet_prefix, is_local = TRUE) {

  lkp.file <- lkp_url
  if (is_local == FALSE) {
    httr::GET(lkp_url, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
    lkp.file <- tf
    unlink(tf)
  }

  search_pattern <- paste0("^[", sheet_prefix, "].*")

  # read excel sheets
  sheets <- readxl::excel_sheets(lkp.file)
  lookup.sheets <- sheets[grep(search_pattern, sheets)]
  lookups <- lapply(lookup.sheets, function(sheet)
    readxl::read_excel(lkp.file, sheet = sheet)
  )
  names(lookups) <- lookup.sheets
  return(lookups)
}
