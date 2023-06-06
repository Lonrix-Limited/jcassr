
#' @importFrom dplyr desc
#' @importFrom rlang .data
.subtract_treatcosts_from_budget <- function(treats_data, budget) {

  # subtracts the cost for all treatments from the budget and returns the
  # updated budget (balances)
  # Unit test = TRUE

  row.names(budget) <- as.character(budget$period)
  if (is.null(treats_data)) return(budget)
  if (nrow(treats_data) == 0) return(budget)

  # Get the sum of costs by Period and Category
  sumry <- treats_data %>%  dplyr::group_by(.data$period, .data$category) %>%
   dplyr::summarise(cost_total = sum(cost))

  n_rows <- nrow(sumry)
  for (irow in 1:n_rows) {

    sum_row <- sumry[irow, ]

    cost <- sum_row[["cost_total"]]
    treat_cat <- sum_row[["category"]]
    period <- as.character(sum_row[["period"]])

    balance <- budget[period, ][[treat_cat]]
    budget[period, ][[treat_cat]] <- balance - cost

  }

  return(budget)

}

#' Creates an Unlimited Budget suitable for a specified model
#'
#' \code{jc_get_unlimited_budget} Creates an unlimited budget with automatically
#' added budget categories based on the treatment categories defined in your
#' model. The number of budget periods added are also automatically set to
#' match the number of modelling periods
#'
#' @param model \code{jm_model} model object correctly set up
#' @return a data frame containing budget categories in columns, and a matching
#' amount for each modelling period in rows.
#' @export
#'
jc_get_unlimited_budget <- function(model)  {

  nper <- length(model$periods)
  ncols <- 1 + length(model$treat_cats)
  df <- data.frame(matrix(NA, nrow = nper,
                          ncol = ncols))
  names(df) <- c("period", model$treat_cats)

  df$period <- model$periods
  for (cat in model$treat_cats) {
    df[ , cat] <- 1e20
  }

  row.names(df) <- as.character(model$periods)
  return(df)
}

