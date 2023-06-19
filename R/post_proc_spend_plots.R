


#' Gets a graph showing percentage Budget Utilisation over time.
#'
#' \code{jc_get_spend_graph} Gets ggplot object showing lines representing
#' budget utilisation for different treatment categories over time.
#' 
#' Note: this function will plot the specified column values vs the 'period'
#' column. If you want the treatment period to be in terms of calendar-epochs,
#' then you should adjust the period column BEFORE calling this function.
#'
#' @param spending_info Spending info retrieved with
#' \code{link{jc_get_spending_info}}
#' @param spend_param spending parameter to plot - this should be one of the
#' columns in \code{spending_info}
#' @param x_info List object containing attributes 'min', 'max', 'increm' and
#' 'title' for X-axis (recommend leaving x-title blank).
#' @param y_info List object containing attributes 'min', 'max', 'increm' and
#' 'title' for Y-axis.
#' @param categories Vector of category names to consider for the graph
#' @param colours Vector of colour names to match category names
#' @param line_types Vector of line style names to match category names
#' the statistics codes passed in as parameter \code{stat_codes} (valid
#' values are: 'solid', 'dashed', 'dotted', dotdash', 'longdash' etc. ).
#' @param line_width Line width for the lines. Try values in range 0.8 to 1.2.
#' @param legend_title Title for the legend.
#' @param scale_factor scaling factor for values to be plotted (e.g. use 1e6 to
#' scale costs to $-million)
#' @export
#' @importFrom ggplot2 geom_line
#'
jc_get_spend_graph <- function(spending_info, spend_param,
                            x_info, y_info, categories,
                            colours, line_types, line_width = 1,
                            legend_title = "Category",
                            scale_factor = 1) {
  
  spending_info <- as.data.frame(spending_info)
  x_ticks <- seq(x_info$min, x_info$max, by = x_info$increm)
  y_ticks <- seq(y_info$min, y_info$max, by = y_info$increm)
  
  spending_info <- spending_info %>% dplyr::filter(
    .data$treat_category %in% categories)
  
  spending_info$treat_category <- factor(spending_info$treat_category,
                                         levels = categories)
  
  spending_info$plotvalue <- spending_info[, spend_param]/scale_factor
  
  gr <- ggplot2::ggplot(data=spending_info, aes(x=.data$period,
                                                y=.data$plotvalue,
                                                group=.data$treat_category)) +
    geom_line(aes(linetype=.data$treat_category,
                  colour = .data$treat_category), linewidth = line_width,
              alpha = 0.8)+
    geom_point(aes(shape=.data$treat_category, colour = .data$treat_category)) +
    scale_color_manual(values = colours) +
    scale_linetype_manual(values=line_types) +
    scale_x_continuous(limits = c(x_info$min, x_info$max), breaks = x_ticks) +
    scale_y_continuous(limits = c(y_info$min, y_info$max), breaks = y_ticks) +
    labs(
      x = "",
      y = y_info$title,
      colour = legend_title,
      linetype = legend_title,
      shape = legend_title
    ) +
    theme_classic() +
    theme(text = element_text(size = 14, family = "sans"))
  return(gr)
}
