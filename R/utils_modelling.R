
#' Updates a specific setting in a General Parameters data frame
#'
#' \code{jc_update_gen_param} Is a utility function to quickly update
#' a specific setting (e.g. discount rate) in a data frame containing
#' General Model parameters.
#'
#' @param gen_params Data frame containing General Modelling parameters with
#' columns 'setting_name' and 'value'
#' @param param_code The code for the parameter to update. See online help for
#' the valid parameter codes for General Parameters (case sensitive!)
#' @param value The new value to set for the specified parameter
#' @export
#'
jc_update_gen_param <- function(gen_params, param_code, value) {

  if (!"setting_name" %in% names(gen_params)) {
    stop(paste0("Required column 'setting_name' not found on ",
                "the General Parameters data frame"))
  }

  row <- gen_params[gen_params$setting_name == param_code, ]
  if (nrow(row) == 0) {
    stop(paste0("Cannot find setting nameed '", param_code, "'in ",
                "the General Parameters data frame"))
  }

  gen_params[gen_params$setting_name == param_code, ]$value <- as.character(value)
  return(gen_params)

}

#' Updates the number of modelling periods in General Parameters
#'
#' \code{jc_update_num_mod_periods} Is a utility function to quickly update
#' the value associated with the parameter 'number_of_modelling_periods' in
#' a data frame containing General Model parameters.
#'
#' @param gen_params Data frame containing General Modelling parameters with
#' columns 'setting_name' and 'value'
#' @param num_periods Number of modelling periods
#' @export
#'
jc_update_num_mod_periods <- function(gen_params, num_periods) {

  param_code <- "number_of_modelling_periods"
  return(jc_update_gen_param(gen_params, param_code, num_periods))

}
