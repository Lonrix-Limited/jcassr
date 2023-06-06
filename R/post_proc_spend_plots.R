


#' Gets a graph showing percentage Budget Utilisation over time.
#'
#' \code{jc_get_spend_graph} Gets ggplot object showing lines representing
#' budget utilisation for different treatment categories over time.
#'
#' @param spending_info Spending info retrieved with
#' \code{link{jc_get_spending_info}}
#' @param y_info List object containing attributes 'min', 'max', 'increm' and
#' 'title' for Y-axis.
#' @param categories Vector of category names
#' @param colours Vector of colour names to match category names
#' @param line_types Vector of line style names to match category names
#' the statistics codes passed in as parameter \code{stat_codes} (valid
#' values are: 'solid', 'dashed', 'dotted', dotdash', 'longdash' etc. ).
#' @param line_width Line width for the lines. Try values in range 0.8 to 1.2.
#' @param legend_title Title for the legend.
#' @export
#' @importFrom ggplot2 geom_line
#'
jc_get_spend_graph <- function(spending_info, categories, y_info,
                               colours, line_types, line_width = 1,
                               legend_title = "Category") {

  y_ticks <- seq(y_info$min, y_info$max, by = y_info$increm)

  spending_info <- spending_info %>% dplyr::filter(
    .data$treat_category %in% categories)

  spending_info$treat_category <- factor(spending_info$treat_category,
                                         levels = categories)
  gr <- ggplot2::ggplot(data=spending_info, aes(x=.data$period,
                                                y=.data$budget_util,
                                       group=.data$treat_category)) +
    geom_line(aes(linetype=.data$treat_category,
                  colour = .data$treat_category), linewidth = line_width,
              alpha = 0.8)+
    geom_point(aes(shape=.data$treat_category, colour = .data$treat_category)) +
    scale_color_manual(values = colours) +
    scale_linetype_manual(values=line_types) +
    scale_x_continuous(breaks = seq(0, 10, by = 1)) +
    scale_y_continuous(limits = c(y_info$min, y_info$max), breaks = y_ticks) +
    labs(
      x = "",
      y = "% Of Budget Utilised",
      colour = legend_title,
      linetype = legend_title,
      shape = legend_title
    ) +
    theme_classic() +
    theme(text = element_text(size = 14, family = "sans"))
  return(gr)
}
