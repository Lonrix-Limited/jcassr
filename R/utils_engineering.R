
#' Gets the Present Worth Factor (PWF)
#'
#' \code{jc_debug_elem} Gets the Present Worth Factor (PWF) for a given
#' number of periods, a given inflation rate and a given discount rate
#'
#' @param period number of periods over which to discount and inflate
#' @param inflation_percent Inflation rate as a percentage (i.e. 4.5, not 0.045)
#' @param discount_percent Discount rate as a percentage (i.e. 4.5, not 0.045)
#' @return real number which is the Present Worth Factor. Multiply cost by this
#' factor to get the present value of the amount
#' @export
jc_get_PWF <- function(period, inflation_percent,
                                   discount_percent) {
  #see:
  #https://www.pveducation.org/pvcdrom/discount-inflation-and-interest-rates

  disc_factor <- (1 + discount_percent/100)^period
  infl_factor <- (1 + inflation_percent/100)^period
  return(infl_factor/disc_factor)
}

#' Converts values to Min-Max Normalised values
#'
#' \code{jc_normalise_min_max} takes a vector of values and returns a matching
#' min-max normalised set. If min and max values are not explicitly supplied,
#' the minimum and maximum values will be automatically calculated from the
#' values passed
#'
#' For more information on min-max normalisation of values, see
#' \url{https://en.wikipedia.org/wiki/Feature_scaling}
#'
#' @param values numerical vector of values to normalise
#' @param min_val optional custom minimum value to use. If ommitted, the minimum
#' used for normalisation will be automatically calculated from \code{values}
#' @param max_val optional custom maximum value to use. If ommitted, the maximum
#' used for normalisation will be automatically calculated from \code{values}
#' @return vector of normalised values
#' @export
#' @examples
#' values <- c(0,2,4,6,8,10) #vector of values to normalise
#' #for this example, the mean is 5 and the standard deviation is 3.7417
#' norm_values <- jc_normalise_min_max(values)
#' norm_values  #Should be 0, 0.2, 0.4, 0.6, 0.8, 1
#'
#' # Example with scalar values and custom min and max
#' # 65 lies halfway between 10 and 120. Should get 0.5
#' jc_normalise_min_max(65, 10, 120)
jc_normalise_min_max <- function(values, min_val = NA, max_val = NA) {

  values <- as.numeric(values)
  if (is.na(min_val)) min_val <- min(values, na.rm = TRUE)
  if (is.na(max_val)) max_val <- max(values, na.rm = TRUE)
  range <- max_val - min_val
  if (range == 0) return(rep(0, length(values)))

  result <- ifelse(values < min_val, min_val, values)
  result <- ifelse(result > max_val, max_val, result)

  result <- (result - min_val)/range

  return(result)

}

#' Converts values to Z-score Normalised values
#'
#' \code{jc_normalise_z} takes a vector of values and returns a matching
#' set of Z-score normalised values. The Z-score is defined as:
#' \code{(value - mean)/std_dev}. If the mean and standard deviation values are
#' not explicitly supplied, they will be automatically calculated from the
#' values passed
#'
#' Note: If standard deviation is NA or zero, then a score of 0 is returned for
#' all values. This is because, if there is no standard deviation, then all
#' values are the same which will be the mean. In that case, all z-scores are
#' zero.
#'
#' For more information on Z-Score normalisation of values, see
#' \url{https://en.wikipedia.org/wiki/Feature_scaling}
#'
#' @param values numerical vector of values to normalise
#' @param mean_val optional custom mean value to use. If ommitted, the mean
#' used for normalisation will be automatically calculated from \code{values}
#' @param stdev optional custom standard deviation to use. If ommitted, the
#' standard deviation used for normalisation will be automatically calculated
#' from \code{values}
#' @return vector of normalised values
#' @export
#' @examples
#' values <- c(0,2,4,6,8,10) #vector of values to normalise
#' #for this example, the mean is 5 and the standard deviation is 3.7417
#' norm_values <- jc_normalise_z(values)
#' #Should be -1.3363,	-0.8018,	-0.2673,	0.2673,	0.8018,	1.3363
jc_normalise_z <- function(values, mean_val = NA, stdev = NA) {
  values <- as.numeric(values)
  if (is.na(mean_val)) mean_val <- mean(values, na.rm = TRUE)
  if (is.na(stdev)) stdev <- stats::sd(values, na.rm = TRUE)

  # If there is no standard deviation, then all values are the same which will
  # be the mean. In that case, all z-scores are zero
  if (is.na(stdev)) {
    result <- 0
  }
  else if (stdev == 0) {
    result <- 0
  }
  else {
    result <- (values - mean_val)/stdev
  }
  return(result)

}


#' Gets a Percent Rank based on data in a column of a data frame
#'
#' \code{jc_get_rankpercent} does a rank percent calculation on a column in a
#' data frame.
#'
#' When parameter \code{reverse} is FALSE (the default), the largest absolute
#' value will have a percentage rank of 100. Note that the minimum value will
#' not necessarily have a corresponding rank percent value of zero.
#'
#' When parameter \code{reverse} is TRUE, the smallest absolute
#' value will have a percentage rank of 100. Note that the maximum value will
#' not necessarily have a corresponding rank percent value of zero.
#'
#' @param df data frame with column on which to rank
#' @param column_name name of the column on which to get percent ranking
#' @param reverse If TRUE, the smallest value will have a rank percent of 100
#' @return vector with percent rank values, which will have a maximum of 100, but
#' minimum may not be zero.
#' @export
jc_get_rankpercent <- function(df, column_name, reverse = FALSE) {
  if (!column_name %in% names(df)) {
    stop(paste0("Error in 'jc_get_rankpercent()'; Details: Column '",
                column_name, "' not found in data frame."))
  }
  data_values <- df[ , column_name]

  if (reverse) {
    data_values = -data_values
  }

  result = 100 *
    base::rank(data_values, ties.method = "average")/length(data_values)


  return(result)
}


#' Converts values to Percentile-Based Scaled Normalised values
#'
#' \code{jc_normalise_percentiles} takes a vector of values and returns a matching
#' normalised set. In this approach, a scale is first built that matches
#' quantiles with specified scaled values. For example, for quantile 0.5 you may
#' have a matching scaled value of zero, and for quantile 1.0 (maximum) you may
#' have a scaled value of 10. A piecewise linear model is then use to
#' interpolate between the quantile values and the scaled values.
#'
#' @param values numerical vector of values to normalise
#' @param quantiles vector of quantiles to match with the scales (must contain
#' values between 0 and 1)
#' @param norm_scales scaled values that represent the normalised range of
#' values to match each specified quantile. Size of this vector must match the
#' size of the \code{quantiles} parameter.
#' @export
jc_normalise_percentiles <- function(values, quantiles, norm_scales) {

  raw_values <- stats::quantile(values, quantiles, na.rm = TRUE)
  tmp <- data.frame(values = raw_values, scale = norm_scales)

  #Ensure that all X-values are unique
  raw_vals_unique <- tmp %>% dplyr::distinct(values, .keep_all = TRUE)
  norm_vals <- jc_utils_get_plmvalues(values, tmp$values, tmp$scale)
  return(norm_vals)
}
