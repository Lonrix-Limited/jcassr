

#' Gets a breakdown of values for a specific parameter over epochs.
#'
#' \code{jc_get_breakdown_table} Gets a table with the frequency breakdown of
#' predicted parameter data for a specific parameter over epochs. Here, the
#' frequency is the number of elements with condition in each bin.
#'
#' @param param_data Forecasted data for the parameter, as loaded from the export
#' file created by jcass.
#' @param epochs Vector with the calendar epochs for which to get breakdown The numbers
#' in this vector must match the columns in the parameter output file, otherwise
#' errors will occur.
#' @param bins Bins with the limits for different breakdown groups.
#' @export
#'
jc_get_breakdown_table <- function(param_data, epochs, bins) {
  i <- 0
  for (epoch in epochs) {
    label <- as.character(epoch)
    if (label %in% names(param_data)) {
      data_for_epoch <- param_data[ , label]
      breakdown_for_epoch <- DescTools::Freq(data_for_epoch, bins)

      if (i == 0) df <- data.frame(bin = breakdown_for_epoch$level)
      df[ ,{{label}}] <- breakdown_for_epoch$freq
      i <- i + 1
    } else {
      warning(paste0("Epoch ", label, " column not found in data"))
    }
  }
  return (df)
}


#' Gets a breakdown of percentage length for a specific parameter over epochs.
#'
#' \code{jc_get_breakdown_length_pct} Gets a table with the breakdown of the
#' percentage of network length in each bin for predicted parameter data for a
#' specific parameter over epochs.
#'
#' @param param_data Forecasted data for the parameter, as loaded from the export
#' file created by jcass.
#' @param epochs Vector with the calendar epochs for which to get breakdown The numbers
#' in this vector must match the columns in the parameter output file, otherwise
#' errors will occur.
#' @param bins Bins with the limits for different breakdown groups.
#' @param network_length Total network length on which to base percentages.
#' Assumed to be in same unit as the location from and to column data.
#' @param loc_from_col Name of the column with the From/Start locations.
#' @param loc_to_col Name of the column with the To/End locations.
#' @export
#' @importFrom dplyr group_by_at
#' @importFrom dplyr vars
#'
jc_get_breakdown_length_pct <- function(param_data, epochs, bins,
                                        network_length,
                                        loc_from_col = "loc_from",
                                        loc_to_col = "loc_to") {
  
  result <- NULL
  df <- .get_bin_breakdown(param_data, epochs, bins)
  df$length_var <- df[ , loc_to_col] - df[ , loc_from_col]
  
  bin_labels <- .get_cut_labels(bins)
  n_labels <- length(bin_labels)
  
  for (ep in epochs)  {
    
    bin_col <- paste0("bin_", as.character(ep))
    tmp1 <- df %>% group_by_at(vars(bin_col)) %>% summarise(
      dummy = sum(.data$length_var)
    )
    
    tmp <- data.frame(bin_col = bin_labels, dummy = rep(0, n_labels))
    names(tmp) <- c(bin_col, "dummy")
    for (lbl in bin_labels) {
      sset <- tmp1[tmp1[bin_col] == lbl, "dummy"]
      if (nrow(sset) == 1) {
        value <- sset[[1, "dummy"]]
        tmp[[bin_labels == lbl, "dummy"]] <- value
      }
    }
    
    sum_col <- paste0(as.character(ep))
    if (is.null(result)) {
      result <- stats::setNames(data.frame(matrix(ncol = 1,
                                                  nrow = nrow(tmp))),
                                c(sum_col))
      
      result[, "bin"] <- tmp[ , bin_col]
    }
    result[ , sum_col] <- tmp$dummy
    result[ , sum_col] <- result[ , sum_col]/network_length
  }
  result <- result %>% relocate("bin") # move the id column to the left
  return(result)
}

.get_bin_breakdown <- function(param_data, calendar_epochs, bins) {
  
  for (ep in calendar_epochs)  {
    col_raw <- as.character(ep)
    col_name <- paste0("bin_", as.character(ep))
    param_data[ , col_name] <- cut(param_data[ , col_raw], breaks = bins,
                                   include.lowest = TRUE, right = FALSE)
  }
  return(param_data)
}

.get_cut_labels <- function(breaks, include.lowest = TRUE, right = FALSE) {
  n <- length(breaks)-1
  result <- rep(NA, n)
  
  for (i in 1:n) {
    
    low <- as.character(breaks[i])
    high <- as.character(breaks[i+1])
    
    if (i == 1) {
      if (include.lowest) {
        lbl <- paste0("[", low,",")
      }
      else {
        lbl <- paste0("(", low,",")
      }
    } else {
      if (right) {
        lbl <- paste0("(", low,",") 
      } else {
        lbl <- paste0("[", low,",")
      }
    }
    
    if (right || i == n) {
      lbl <- paste0(lbl, high,"]")
    }
    else {
      lbl <- paste0(lbl, high,")")
    }
    result[i] <- lbl
  }
  return(result)
}
