
#' Multi-Objective Optimisation on the basis of Ratio Analysis (MOORA)
#'
#' Calculations and unit testing based on this video:
# https://www.youtube.com/watch?v=4ikr_4OltUw
#'
#' Rank (1 is best) and the normalised value used to rank is added to the
#' data set which is then returned
#'
#' @param data alternatives in rows and criteria in columns. It is assumed that
#' all columns are objectives
#' @param weights unit weights for each objective (must add up to one)
#' @param obj_types Objective types for each objective. Must match the number
#' of columns in data. Use 'max' to indicate an objective to maximize, and 'min'
#' to indicate an objective to minimize (e.g. cost)
#' @export
#'
jc_moora_rank <- function(data, weights, obj_types) {

  # Do Multi-Objective Optimisation on the basis of Ratio Analysis (MOORA)
  # Calculations and unit testing based on this video:
  # https://www.youtube.com/watch?v=4ikr_4OltUw

  # Rank (1 is best) and the normalised value used to rank is added to the
  # data set which is then returned

  attribs <- names(data)        # Get attributes on which to rank
  n_attribs <- length(attribs)  # Get number of attributes
  n_options <- nrow(data)       # Get number of options

  obj_signs <- ifelse(obj_types == "max", 1, -1)

  norm_data <- data

  # Do normalisation and weighting and addition based on signs in one loop
  for (icol in 1:n_attribs) {

    squares <- data[icol]^2
    sum <- sum(squares)
    col_sum <- sqrt(sum)
    norm_data[ , icol] <- weights[icol] *
      (data[ , icol] / col_sum) * obj_signs[icol]
  }

  # calculate the weighted sum over all attributes
  norm_data[ , "sum"] = rowSums(norm_data[, attribs], na.rm = TRUE)

  # Get the rank: 1 is the best
  data$moora_rank <- floor(rank(-norm_data$sum)) # add rank to data set
  data$moora_value <- norm_data$sum       # add normalised weighted sum values

  # Return the data with the rank and normalised weighted sums as added columns
  return(data)

}

.jc_moora_check_weights <- function(weights_raw) {
  # Ensures the weights add up to 1

  weight_sum <- sum(weights_raw)
  if (weight_sum <= 0) {
    stop("Sum of weights is less than or equal to zero")
  }
  weights_final <- weights_raw/weight_sum
  return(weights_final)
}
