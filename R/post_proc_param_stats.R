

#' Gets a table showing statistics for a parameter.
#'
#' \code{jc_get_param_stats_table_multirun} Gets a table showing the statistics
#' for a specific model and statistics parameter.
#'
#' @param param_data Data frame containing the data for the model parameter as
#' loaded from the jcass output file.
#' @param epochs Vector with the calendar epochs for which to get stats. The numbers
#' in this vector must match the columns in the parameter output file, otherwise
#' errors will occur.
#' @export
#' @importFrom stats quantile
#'
jc_get_param_stats_table <- function(param_data, epochs) {
  i <- 0
  param_data <- as.data.frame(param_data)
  n_epochs <- length(epochs)

  mins <- rep(NA, n_epochs)
  perc_02s <- rep(NA, n_epochs)
  perc_05s <- rep(NA, n_epochs)
  perc_10s <- rep(NA, n_epochs)
  perc_15s <- rep(NA, n_epochs)
  perc_20s <- rep(NA, n_epochs)
  perc_25s <- rep(NA, n_epochs)
  medians <- rep(NA, n_epochs)
  means <- rep(NA, n_epochs)
  perc_75s <- rep(NA, n_epochs)
  perc_80s <- rep(NA, n_epochs)
  perc_85s <- rep(NA, n_epochs)
  perc_90s <- rep(NA, n_epochs)
  perc_95s <- rep(NA, n_epochs)
  perc_98s <- rep(NA, n_epochs)
  maxs <- rep(NA, n_epochs)
  sums <- rep(NA, n_epochs)

  epochs <- as.character(epochs)

  i <- 1
  for (epoch in epochs) {

    data_for_epoch <- as.numeric(param_data[, epoch])

    mins[i] <- min(data_for_epoch, na.rm = TRUE)
    perc_02s[i] <- quantile(data_for_epoch, 0.02, na.rm = TRUE)
    perc_05s[i] <- quantile(data_for_epoch, 0.05, na.rm = TRUE)
    perc_10s[i] <- quantile(data_for_epoch, 0.1, na.rm = TRUE)
    perc_15s[i] <- quantile(data_for_epoch, 0.15, na.rm = TRUE)
    perc_20s[i] <- quantile(data_for_epoch, 0.2, na.rm = TRUE)
    perc_25s[i] <- quantile(data_for_epoch, 0.25, na.rm = TRUE)

    medians[i] <- quantile(data_for_epoch, 0.5, na.rm = TRUE)
    means[i] <- mean(data_for_epoch, na.rm = TRUE)

    perc_75s[i] <- quantile(data_for_epoch, 0.75, na.rm = TRUE)
    perc_80s[i] <- quantile(data_for_epoch, 0.80, na.rm = TRUE)
    perc_85s[i] <- quantile(data_for_epoch, 0.85, na.rm = TRUE)
    perc_90s[i] <- quantile(data_for_epoch, 0.9, na.rm = TRUE)
    perc_95s[i] <- quantile(data_for_epoch, 0.95, na.rm = TRUE)
    perc_98s[i] <- quantile(data_for_epoch, 0.98, na.rm = TRUE)
    maxs[i] <- max(data_for_epoch, na.rm = TRUE)
    sums[i] <- sum(data_for_epoch, na.rm = TRUE)

    i <- i + 1
  }

  df <- data.frame(epoch = as.numeric(epochs),
                   min = mins, perc_02 = perc_02s,
                   perc_05 = perc_05s, perc_10 = perc_10s,
                   perc_15 = perc_15s, perc_20 = perc_20s, perc_25 = perc_25s,
                   median = medians, mean = means,
                   perc_75 = perc_75s, perc_80 = perc_80s, perc_85 = perc_85s,
                   perc_90 = perc_90s, perc_95 = perc_95s,
                   perc_98 = perc_98s, max = maxs,
                   sum = sums)

  return (df)
}


#' Gets a table showing above and below threshold for a parameter.
#'
#' \code{jc_get_param_threshold_lengths} Gets a table showing the lengths and
#' percentages above and below a given threshold for a specific model parameter.
#' 
#' Note: this function is relevant for elements in linear networks (e.g. road
#' and water networks). For domains that require counts instead of lengths, please
#' notify Lonrix and we can add an extension to this method that gives counts
#' instead of lengths.
#'
#' @param param_data Data frame containing the data for the model parameter as
#' loaded from the jcass output file.
#' @param epochs Vector with the calendar epochs for which to get stats. The numbers
#' in this vector must match the columns in the parameter output file, otherwise
#' errors will occur.
#' @param threshold threshold to evaluate
#' @param from_col name of the from/start column for element lengths. This column
#' must exist in your model output data (specified in model setup as an identifier
#' column)
#' @param to_col name of the to/end column for element lengths. This column
#' must exist in your model output data (specified in model setup as an identifier
#' column)
#' @export
#' @importFrom stats quantile
#'
jc_get_param_threshold_lengths <- function(param_data, epochs, threshold,
                                            from_col, to_col) {
  i <- 0
  param_data <- as.data.frame(param_data)
  ok <- .check_required_cols(c(from_col, to_col), 
                            param_data, "lengths above threshold")
  if (ok == FALSE) {stop("Required columns not found in parameter data")}
  
  param_data$tmp_length <- param_data[ , to_col] - param_data[ , from_col]
  total_length <- sum(param_data$tmp_length)
  
  n_epochs <- length(epochs)
  
  aboves_length <- rep(NA, n_epochs)
  belows_length <- rep(NA, n_epochs)
  aboves_perc <- rep(NA, n_epochs)
  belows_perc <- rep(NA, n_epochs)
  
  epochs <- as.character(epochs)
  
  i <- 1
  for (epoch in epochs) {
    
    data_for_epoch <- as.numeric(param_data[, epoch])
    
    over_threshold <- param_data[which(data_for_epoch > threshold), "tmp_length"]
    under_threshold <- param_data[which(data_for_epoch <= threshold), "tmp_length"]
    
    aboves_length[i] <- sum(over_threshold)
    belows_length[i] <- sum(under_threshold)
    
    aboves_perc[i] <- 100*sum(over_threshold)/total_length
    belows_perc[i] <- 100*sum(under_threshold)/total_length
    
    i <- i + 1
  }
  
  df <- data.frame(epoch = as.numeric(epochs),
                   lte_threshold_len = belows_length, 
                   lte_threshold_perc = belows_perc,
                   gt_threshold_len = aboves_length, 
                   gt_threshold_perc = aboves_perc
  )
  
  return (df)
}