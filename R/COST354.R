

#' Calculate a Health Index using the COST354 Report Method
#'
#' \code{get_COST354_index} Calculates a normalised, weighted index based on the
#' methodology in the European COST354 report.
#'
#' @param index_values vector containing the values for the various sub-criteria.
#' These are assumed to be normalised to the same scale as per COST354
#' methodology
#' @param weights Relative Importance Weights for the various indexes to be
#' aggregated. Weights should be between 0 and 1, with zero denoting low
#' importance and 1 denoting highest importance. Weights do NOT have to add
#' up to 1 (see COST354 report pages 34 to 36 for examples)
#' @param max_allowed Maximum return value allowed. If the calculation results
#' in a number greater than this, then \code{max_allowed} is returned.
#' @param influence_fact 'influence factor' as per COST354 methodology. This
#' value is typically in the range of 0.3 to 0.5. See COST354 report for details.
#' @return Index value that aggregates the index values using COST354 method
#' @export
#'
get_COST354_index <- function(index_values, weights, max_allowed,
                              influence_fact = 0.4) {

  if (length(index_values) != length(weights)) {
    stop("Error in get_COST354_index: index and weight vectors not same size")
  }

  max_weight <- max(weights)
  if (max_weight > 1.0) {
    stop("Error in get_COST354_index: max weight is greater than 1.0")
  }
  if (max_weight <= 0) {
    stop("Error in get_COST354_index: max weight is less than zero")
  }

  weight_adjust_factor <- 1 / max_weight

  weighted_values <- (weights * weight_adjust_factor) * index_values

  max_value <- max(weighted_values)

  # Create a dummy set in which the maximum value is set to NA
  dummy_set <- ifelse(weighted_values == max_value, NA, weighted_values)

  # Get number of values excluding the maximum
  n_adj <- length(weighted_values) -1

  # Calculate the average of the weighted values, excluding the maximum which
  # has been set to NA
  avg_non_max_values <- sum(dummy_set, na.rm = TRUE)/n_adj

  #final value is the minimum of:
  # 1. max allowed, and 2. [max value + (infl_fact/100) x avg_non_max_values]
  tmp_fact <- max_value + (influence_fact/100) * avg_non_max_values
  result <- min(max_allowed, tmp_fact)
  return(result)

}


