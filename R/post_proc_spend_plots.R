


#' Gets a line graph showing a spending parameter over time.
#'
#' \code{jc_get_spend_graph} Gets ggplot object showing lines representing
#' a spending parameter for different treatment categories over time.
#' 
#' Note: this function will plot the specified column values vs the 'period'
#' column. If you want the treatment period to be in terms of calendar-epochs,
#' then you should adjust the period column BEFORE calling this function.
#'
#' @param spending_info Spending info retrieved with
#' \code{link{jc_get_spending_info}}
#' @param spend_param spending parameter to plot - this should be one of the
#' columns in \code{spending_info} (e.g. 'total_cost', total_length',
#' 'total_perc' or 'budget_util')
#' @param x_info List object containing attributes 'min', 'max', 'increm' and
#' 'title' for X-axis (recommend leaving x-title blank).
#' @param y_info List object containing attributes 'min', 'max', 'increm' and
#' 'title' for Y-axis.
#' @param cats_or_types Vector containing either the treatment Categories or the
#' treatment Types to include in the plot. If parameter \code{group_type} is
#' 'category' then this list should be the treatment Categories you want to plot,
#' otherwise if parameter \code{group_type} is 'treatment' then this should be 
#' the list of treatment Types you want to include in the plot.
#' @param colours Vector of colour names to match category names
#' @param line_types Vector of line style names to match category names
#' the statistics codes passed in as parameter \code{stat_codes} (valid
#' values are: 'solid', 'dashed', 'dotted', dotdash', 'longdash' etc. ).
#' @param line_width Line width for the lines. Try values in range 0.8 to 1.2.
#' @param group_type specifies whether grouping of treatments is by treatment 
#' category or by treatment type (more specific). Pass either 'category' or 
#' 'treatment'
#' @param legend_title Title for the legend.
#' @param scale_factor scaling factor for values to be plotted (e.g. use 1e6 to
#' scale costs to $-million)
#' @export
#' @importFrom ggplot2 geom_line
#'
jc_get_spend_line <- function(spending_info, spend_param,
                                   x_info, y_info, cats_or_types,
                                   colours, line_types, line_width = 1,
                                   group_type = "category",
                                   legend_title = "Category",
                                   scale_factor = 1) {
  
  
  spending_info <- as.data.frame(spending_info)
  x_ticks <- seq(x_info$min, x_info$max, by = x_info$increm)
  y_ticks <- seq(y_info$min, y_info$max, by = y_info$increm)
  
  if (grepl( "cat", group_type, fixed = TRUE)) {
    
    spending_info <- spending_info %>% dplyr::filter(
      .data$treat_category %in% cats_or_types)
    
    spending_info$group_fact <- factor(spending_info$treat_category,
                                       levels = cats_or_types)
    
  } else {
    
    spending_info <- spending_info %>% dplyr::filter(
      .data$treatment %in% cats_or_types)
    
    spending_info$group_fact <- factor(spending_info$treatment,
                                       levels = cats_or_types)
  }
  
  spending_info$plotvalue <- spending_info[, spend_param]/scale_factor
  
  gr <- ggplot2::ggplot(data=spending_info, aes(x=.data$period,
                                                y=.data$plotvalue,
                                                group=.data$group_fact)) +
    geom_line(aes(linetype=.data$group_fact,
                  colour = .data$group_fact), linewidth = line_width,
              alpha = 0.8)+
    geom_point(aes(shape=.data$group_fact, colour = .data$group_fact)) +
    scale_color_manual(values = colours) +
    scale_linetype_manual(values=line_types) +
    scale_x_continuous(limits = c(x_info$min, x_info$max), breaks = x_ticks) +
    scale_y_continuous(limits = c(y_info$min, y_info$max), breaks = y_ticks) +
    labs(
      x = paste0(x_info$title),
      y = paste0(y_info$title, "\n"),
      colour = legend_title,
      linetype = legend_title,
      shape = legend_title
    ) +
    theme_classic() +
    theme(text = element_text(size = 14, family = "sans"),
          panel.grid.major = element_line(color = "lightsteelblue",
                                          linewidth = 0.25,linetype = 2),
          panel.grid.minor = element_line(color = "lightsteelblue",
                                          linewidth = 0.15,linetype = 3))
  return(gr)
}


#' Gets a stacked bar graph showing a spending parameter over time.
#'
#' \code{jc_get_spend_stacked} Gets ggplot object showing lines representing
#' a spending parameter for different treatment categories over time.
#' 
#' Note: this function will plot the specified column values vs the 'period'
#' column. If you want the treatment period to be in terms of calendar-epochs,
#' then you should adjust the period column BEFORE calling this function.
#'
#' @param spending_info Spending info retrieved with
#' \code{link{jc_get_spending_info}}
#' @param spend_param spending parameter to plot - this should be one of the
#' columns in \code{spending_info} (e.g. 'total_cost', total_length',
#' 'total_perc' or 'budget_util')
#' @param x_info List object containing attributes 'min', 'max', 'increm' and
#' 'title' for X-axis (recommend leaving x-title blank).
#' @param y_info List object containing attributes 'min', 'max', 'increm' and
#' 'title' for Y-axis.
#' @param cats_or_types Vector containing either the treatment Categories or the
#' treatment Types to include in the plot. If parameter \code{group_type} is
#' 'category' then this list should be the treatment Categories you want to plot,
#' otherwise if parameter \code{group_type} is 'treatment' then this should be 
#' the list of treatment Types you want to include in the plot.
#' @param colours Vector of colour names to match category names
#' @param line_types Vector of line style names to match category names
#' the statistics codes passed in as parameter \code{stat_codes} (valid
#' values are: 'solid', 'dashed', 'dotted', dotdash', 'longdash' etc. ).
#' @param line_width Line width for the lines. Try values in range 0.8 to 1.2.
#' @param group_type specifies whether grouping of treatments is by treatment 
#' category or by treatment type (more specific). Pass either 'category' or 
#' 'treatment'
#' @param legend_title Title for the legend.
#' @param scale_factor scaling factor for values to be plotted (e.g. use 1e6 to
#' scale costs to $-million)
#' @export
#' @importFrom ggplot2 geom_col
#'
jc_get_spend_stacked <- function(spending_info, spend_param,
                               x_info, y_info, cats_or_types,
                               colours, line_types, line_width = 1,
                               group_type = "category",
                               legend_title = "Category",
                               scale_factor = 1) {

  spending_info <- as.data.frame(spending_info)
  x_ticks <- seq(x_info$min, x_info$max, by = x_info$increm)
  y_ticks <- seq(y_info$min, y_info$max, by = y_info$increm)
  
  if (grepl( "cat", group_type, fixed = TRUE)) {
    
    spending_info <- spending_info %>% dplyr::filter(
      .data$treat_category %in% cats_or_types)
    
    spending_info$group_fact <- factor(spending_info$treat_category,
                                       levels = cats_or_types)
    
  } else {
    
    spending_info <- spending_info %>% dplyr::filter(
      .data$treatment %in% cats_or_types)
    
    spending_info$group_fact <- factor(spending_info$treatment,
                                       levels = cats_or_types)
  }
  
  
  spending_info$plotvalue <- spending_info[, spend_param]/scale_factor
  
  gr <- ggplot2::ggplot(data=spending_info, aes(x=.data$period,
                                                y=.data$plotvalue)) +
    geom_col(aes(fill = .data$group_fact)) +
    scale_color_manual(values = colours) +
    scale_linetype_manual(values=line_types) +
    scale_x_continuous(limits = c(x_info$min, x_info$max), breaks = x_ticks) +
    scale_y_continuous(limits = c(y_info$min, y_info$max), breaks = y_ticks) +
    labs(
      x = paste0(x_info$title),
      y = paste0(y_info$title, "\n"),
      fill = legend_title,
    ) +
    theme_classic() +
    theme(text = element_text(size = 14, family = "sans"),
          panel.grid.major = element_line(color = "lightsteelblue",
                             linewidth = 0.25,linetype = 2),
          panel.grid.minor = element_line(color = "lightsteelblue",
                             linewidth = 0.15,linetype = 3))
  return(gr)
}
