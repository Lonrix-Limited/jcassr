
#' Gets all increments (excluding resets) as a data frame
#'
#' \code{jc_get_increments} parses the increments from a parameter export file
#' and returns the information in a dataset for further graphics and statistical
#' analysis.
#'
#' Any resets, in which the increment is opposite to what is expected, are
#' assigned NA values and thus will be excluded from downstream statistics and
#' graphics analysis.
#'
#' @param params_export_file relative path to the jcass parameter exports file
#' @param param_name name of the parameter for which to get increments
#' @param calendar_epochs calendar epochs for which to get increments.
#' @return Vector of increments in which the increment for calendar epoch X is
#' the increment from X-1 to X. For example, the increment shown for epoch
#' 2025 will be the increment from 2024 to 2025
#' @param expected expected slope of increments. Use 'pos' when you expect
#' positive increments such as for rut depth (which increases over time), and
#' 'neg' when you expect negative increments such as for texture depth (which
#' decreases over time).
#' @importFrom tidyr gather
#' @export
#'
jc_get_increments <- function(params_export_file, param_name, calendar_epochs,
                              expected = "pos") {


  # Get the parameter data from the sheet in question
  param_data <- as.data.frame(read_xlsx(params_export_file,
                                        param_name))

  calendar_epochs <- as.character(calendar_epochs)
  result <- param_data %>% select(-all_of(calendar_epochs))
  n <- length(calendar_epochs)
  for (i in 2:n) {

    epoch <- calendar_epochs[i]
    increments <- apply(param_data, 1, .get_increment, epoch, expected)
    result[, epoch] <- increments
  }

  cal_eps <- calendar_epochs[2: length(calendar_epochs)]
  result <- result %>% gather(as.character(cal_eps), key = "epoch",
                              value = "increment")

  return(result)
}

.get_increment <- function(row, epoch, expected = "pos") {
  prev_epoch <- as.character(as.numeric(epoch) - 1)
  epoch <- as.character(epoch)

  val_now <- as.numeric(row[[epoch]])
  val_prev <- as.numeric(row[[prev_epoch]])
  increm <- val_now - val_prev
  if (expected == "pos") {
    #expecting positive increment (e.g. rut).
    # If negative, it means a reset, so return NA
    if (increm < 0) return(NA)
  } else {
    # expecting negative increment (e.g. Skid Resistance).
    # If positive, it means a reset, so return NA
    if (increm > 0) return(NA)
  }
  return(increm)
}

#' Gets a Density Plot for increments for a specific parameter
#'
#' \code{jc_get_increm_density} Draws a Density Plot (similar to histogram but
#' smoothed) of the increments for a specific parameter.
#'
#' @param increm_data increment data extracted from parameter export file for
#' this parameter. This should be the result of method \code{jc_get_increments}.
#' See \code{link{jc_get_increments}}
#' @param x_axis_info List object containing the following attributes:
#' \enumerate{
#'   \item 'title' Report-friendly title for the axis
#'   \item 'min' minimum for the axis scale
#'   \item 'max' maximum for the axis scale
#'   \item 'increm' Increment for the axis scale - this will also determine
#'   the bin size if this is a histogram
#' }
#' @export
#' @importFrom ggplot2 geom_density
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#'
jc_get_increm_density <- function(increm_data, x_axis_info) {

  font_fam <- "sans"
  x_ticks <- seq(x_axis_info$min, x_axis_info$max, by=x_axis_info$increm)
  x_limits <- c(x_axis_info$min, x_axis_info$max)

  gr <- ggplot2::ggplot(increm_data) +
    geom_density(aes(x = .data$increment), colour="steelblue",
                 fill = "lightsteelblue", alpha = 0.3) +
    scale_x_continuous(name = x_axis_info$title,
                       limits = x_limits,
                       breaks = x_ticks) +
    scale_y_continuous(name = "Density\n") +
    theme_classic() +
    theme(text = element_text(size = 12, family = font_fam))
  return(gr)

}

#' Gets a Histogram for increments for a specific parameter
#'
#' \code{jc_get_increm_histogram} Draws a Histogram of the increments for a
#' specific parameter.
#'
#' @param increm_data increment data extracted from parameter export file for
#' this parameter. This should be the result of method \code{jc_get_increments}.
#' See \code{link{jc_get_increments}}
#' @param x_axis_info List object containing the following attributes:
#' \enumerate{
#'   \item 'title' Report-friendly title for the axis
#'   \item 'min' minimum for the axis scale
#'   \item 'max' maximum for the axis scale
#'   \item 'increm' Increment for the axis scale - this will also determine
#'   the bin size if this is a histogram
#' }
#' @export
#' @importFrom ggplot2 geom_density
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 geom_histogram
#'
jc_get_increm_histogram <- function(increm_data, x_axis_info) {

  font_fam <- "sans"
  x_ticks <- seq(x_axis_info$min, x_axis_info$max, by=x_axis_info$increm)
  x_limits <- c(x_axis_info$min, x_axis_info$max)

  gr <- ggplot2::ggplot(increm_data) +
    geom_histogram(aes(x = .data$increment), colour="steelblue",
                   fill = "lightsteelblue",
                   binwidth = x_axis_info$increm) +
    scale_y_continuous(name = "Number of Observations\n") +
    scale_x_continuous(name = x_axis_info$title,
                       limits = x_limits,
                       breaks = x_ticks) +
    theme_classic() +
    theme(text = element_text(size = 12, family = font_fam))
  return(gr)

}

#' Gets a Box plot showing increments over epochs for a specific parameter
#'
#' \code{jc_get_increm_boxplot} Draws a Box Plot showing the distribution of
#' increments in each epoch for specific parameter.
#'
#' @param increm_data increment data extracted from parameter export file for
#' this parameter. This should be the result of method \code{jc_get_increments}.
#' See \code{link{jc_get_increments}}
#' @param axis_info List object containing the following attributes describing
#' the Y-axis:
#' \enumerate{
#'   \item 'title' Report-friendly title for the axis
#'   \item 'min' minimum for the axis scale
#'   \item 'max' maximum for the axis scale
#'   \item 'increm' Increment for the axis scale - this will also determine
#'   the bin size if this is a histogram
#' }
#' @param x_label_angle angle for the X-Axis labels (epochs). Default is zero
#' which shows labels horizontally. To rotate slightly when labels are a bit
#' cramped, suggest you try -35.
#' @export

#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 element_blank
#'
jc_get_increm_boxplot <- function(increm_data, axis_info, x_label_angle = 0) {

  y_limits <- c(axis_info$min, axis_info$max)
  y_increms <- seq(axis_info$min, axis_info$max, axis_info$increm)
  x_min <- as.numeric(min(increm_data$epoch))
  x_max <- as.numeric(max(increm_data$epoch))

  gr <- ggplot2::ggplot(increm_data, aes(x = .data$epoch, y=.data$increment)) +
    geom_boxplot(fill = "lightsteelblue", outlier.alpha = 0.2, outlier.size = 0.5) +
    scale_y_continuous(limits = y_limits, breaks = y_increms) +
    xlab("") +
    ylab(paste0(axis_info$title, "\n")) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_line(color = "lightgray",
                                        size = 0.15,linetype = 2),
      axis.text.x=element_text(angle = 0, hjust = 0),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank())
  return(gr)
}

