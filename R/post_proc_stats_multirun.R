

#' Gets a table showing statistics for a parameter for multiple scenarios.
#'
#' \code{jc_get_param_stats_table_multirun} Gets a table showing the statistics
#' for a specific model and statistics parameter.
#'
#' @param model_param Name of the model parameter to get
#' See \code{link{jc_get_spend_summary_multi_budget}}.
#' @param stat_param Statistics parameter to get data for.
#' @param run_codes Vector of run codes. It is assumed that a matching parameter
#' output file (with run code appended) are in the outputs/ subfolder of the
#' working folder.
#' @param calendar_epochs Vector with the calendar epochs for which to get stats. The numbers
#' in this vector must match the columns in the parameter output file, otherwise
#' errors will occur.
#' @param outputs_base Base stub for relative path to parameter output files.
#' @export
#' @importFrom ggplot2 element_blank
#'
jc_get_param_stats_table_multirun <- function(model_param, stat_param,
                                              run_codes, calendar_epochs,
                                      outputs_base = "outputs/paramdata_") {

  result <- NULL
  for (run_code in run_codes) {

    exp_file <- paste0(outputs_base, run_code, ".xlsx")
    param_data <- as.data.frame(read_xlsx(exp_file, model_param))
    tmp <- jc_get_param_stats_table(param_data, calendar_epochs)
    tmp$run_key <- run_code

    if (is.null(result)) {
      result <- data.frame(epoch = tmp$epoch, run_key = tmp$run_key)
      result[, stat_param] <- tmp[, stat_param]
    }
    else {
      tmp <- tmp %>% select(all_of(c("epoch", "run_key", stat_param)))
      result <- rbind(result, tmp)
    }
  }
  return(result)
}



