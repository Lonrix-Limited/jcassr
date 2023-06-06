

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
