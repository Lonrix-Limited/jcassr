
#' Plot the Area-Under-the-Curve for various Strategies
#'
#' \code{jc_get_elem_forecast} Gets the forecasted values for all parameters for
#' a specific element identified with its ONE-based element index. That is, to
#' get the first element in the element set, pass element_index = 1
#'
#' @param elem_index ONE-based index of the element to view
#' @param param_names Vector containing the model parameter names to explort.
#' Typically these will be all parameter names, which you can load from the
#' 'parameters' sheet in the model setup file
#' @param params_export_file relative path to the Parameter export/output file
#' @param identifiers_to_retain Vector of the identifiers to retain from 
#' amongst the identifiers included in the parameter export (e.g. 'area_name', 
#' etc. as defined in model lookups under the 'general' list of the model setup file)
#' @param calendar_epochs Vector of calendar epochs for which to show forecast.
#' You should ensure this vector matches the number of modelling periods exactly.
#' @param treatments Data Frame with treatments loaded from the jcass model
#' treatment exports file
#' @return Data Frame with forecast table for the element
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr all_of
#' @importFrom dplyr everything
#'
jc_get_elem_forecast <- function(elem_index, param_names,
                                 params_export_file,
                                 identifiers_to_retain,
                                 calendar_epochs,
                                 treatments) {
  result <- NULL
  index <- elem_index + 1
  for (param_name in param_names) {
    # Get the parameter data from the sheet in question
    param_data <- as.data.frame(read_xlsx(params_export_file, param_name))
    param_values <- param_data[index, ]
    param_values$parameter <- param_name
    if (is.null(result)) {
      result <- param_values
    } else {
      result <- rbind(result, param_values)
    }
  }
  
  cols_to_retain <- c()
  for (col in names(result)) {
    if (col == "parameter") {
      cols_to_retain <- c(cols_to_retain, col)
    } else if (col %in% identifiers_to_retain) {
      cols_to_retain <- c(cols_to_retain, col)
    } else if (col %in% calendar_epochs) {
      cols_to_retain <- c(cols_to_retain, col)
    }
  }
  
  result <- result %>% select(all_of(cols_to_retain))
  result <- result %>% select(.data$parameter, everything()) #move parameter col to left
  result <- .add_treatments(result, elem_index, calendar_epochs, treatments)
  result <- rbind(result[nrow(result), ], result[-nrow(result), ]) # Move the bottom row to the top
  row.names(result) <- seq(1, nrow(result))
  
  return(result)
}

.add_treatments <- function(fc_data, elem_index, calendar_epochs, treats) {
  fc_data <- as.data.frame(fc_data)
  n <- nrow(fc_data) + 1
  fc_data[n , "parameter"] <- "treatment"
  for (ep in calendar_epochs) {
    tmp <- treats %>% filter(.data$elem_index == !!elem_index &
                               .data$period_calendar == !!ep)
    if (nrow(tmp) == 1) {
      fc_data[n , as.character(ep)] <- tmp$treatment
    } else {
      fc_data[n , as.character(ep)] <- "-"
    }
  }
  return(fc_data)
}
