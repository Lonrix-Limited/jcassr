

#' Gets a graph showing stats for a parameter over epochs.
#'
#' \code{jc_get_param_stats_plot} Gets ggplot object showing a lines representing
#' various statistics for a specific parameter over the
#' modelling epochs.
#'
#' Also see \code{link{jc_get_spend_summary_multi_budget}}
#'
#' @param param_stats Data frame with statistics for the parameter, as provided
#' by method \code{link{jc_get_param_stats_table}}.
#' @param stat_codes Vector of stats parameters to plot. See the output for method
#' \code{link{jc_get_param_stats_table}} to see what stats are available, and
#' also how the codes (column names) to use. Generally, for percentiles, you can
#' use 'perc_90', 'perc_25' etc. Also 'mean', 'median', 'max' and 'min.
#' @param colours Vector of colour names or HTML colour codes, to match
#' the statistics codes passed in as parameter \code{stat_codes}.
#' @param line_types Vector of line style names to match
#' the statistics codes passed in as parameter \code{stat_codes} (valid
#' values are: 'solid', 'dashed', 'dotted', dotdash', 'longdash' etc. ).
#' @param x_info List object containing attributes 'min', 'max', 'increm' and
#' 'title' for X-axis.
#' @param y_info List object containing attributes 'min', 'max', 'increm' and
#' 'title' for Y-axis.
#' @param line_width Line width for the lines. Try values in range 0.8 to 1.2.
#' @param legend_title Title for the legend.
#' @export
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_color_brewer
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_linetype_manual
#'
jc_get_param_stats_plot <- function(param_stats, stat_codes, colours,
                                    line_types, x_info, y_info,
                                    line_width = 1,
                                    legend_title = "Statistic") {

  x_ticks <- seq(x_info$min, x_info$max, by = x_info$increm)
  y_ticks <- seq(y_info$min, y_info$max, by = y_info$increm)

  param_stats <- param_stats %>% select("epoch", all_of(stat_codes))
  param_stats <- param_stats %>% gather(all_of(stat_codes),
                                        key = "statistic", value = "value")

  gr <- ggplot2::ggplot(data=param_stats, aes(x=.data$epoch, y=.data$value,
                                              group=.data$statistic)) +
    geom_line(aes(linetype=.data$statistic, colour = .data$statistic),
              linewidth = line_width)+
    geom_point(aes(shape=.data$statistic, colour = .data$statistic)) +
    scale_color_manual(values = colours) +
    scale_linetype_manual(values=line_types) +
    scale_x_continuous(limits = c(x_info$min, x_info$max), breaks = x_ticks) +
    scale_y_continuous(limits = c(y_info$min, y_info$max), breaks = y_ticks) +
    labs(
      x = "",
      y = paste0(y_info$title, "\n"),
      colour = legend_title,
      linetype = legend_title,
      shape = legend_title
    ) +
    theme_classic() +
    theme(text = element_text(size = 12, family = "sans"))
  return(gr)
}
