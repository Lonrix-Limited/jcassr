
#' Gets a graph showing stats for a parameter for multiple scenarios.
#'
#' \code{jc_get_param_stats_plot_multirun} Gets ggplot object showing a line
#' graph with lines representing the statistics for a specific parameter over the
#' modelling epochs and over all the modelling runs in run_codes. Colours are
#' used to define budget scenario, and line style is used to define statistic.
#'
#' Also see \code{link{jc_get_spend_summary_multi_budget}}
#'
#' @param model_param Name of the model parameter to get the graph for.
#' @param stat_params Vector of stats parameters to plot. See the output for method
#' \code{link{jc_get_param_stats_table}} to see what stats are available, and
#' also how the codes (column names) to use. Generally, for percentiles, you can
#' use 'perc_90', 'perc_25' etc. Also 'mean', 'median', 'max' and 'min.
#' @param budget_codes Vector of run/budget codes, e.g. 'upper', 'lower', 'base' etc.
#' It is assumed that there will be output files matching these codes and also
#' that the budgets file will have matching sheets containing the budgets for
#' each of these codes.
#' @param colours Vector of colour names or HTML colour codes, to match budget
#' codes
#' @param line_types Vector of line style names to match statistics parameters
#' (valid values are: 'solid', 'dashed', 'dotted', dotdash', 'longdash' etc. )
#' @param x_info List object containing attributes 'min', 'max', 'increm' and
#' 'title' for X-axis.
#' @param y_info List object containing attributes 'min', 'max', 'increm' and
#' 'title' for Y-axis.
#' @param calendar_epochs Vector with the calendar epochs for which to get stats. The numbers
#' in this vector must match the columns in the parameter output file, otherwise
#' errors will occur.
#' @param line_width Line width for the lines. Try values in range 0.8 to 1.2.
#' @param outputs_base Base stub for relative path to parameter output files.
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
jc_get_param_stats_plot_multirun <- function(model_param, stat_params, budget_codes,
                                              colours, line_types,
                                              x_info, y_info, calendar_epochs,
                                              line_width = 1,
                                              outputs_base = "outputs/paramdata_",
                                              legend_title = "Budget Level") {

  x_ticks <- seq(x_info$min, x_info$max, by = x_info$increm)
  y_ticks <- seq(y_info$min, y_info$max, by = y_info$increm)

  if (length(line_types) != length(stat_params)) {
    stop("Size of 'line_types' vector must match size of 'stat_params' vector")
  }

  df <- NULL
  for (stat_param in stat_params) {

    tmp <- jc_get_param_stats_table_multirun(model_param, stat_param, budget_codes,
                                             calendar_epochs,
                                             outputs_base = outputs_base)
    tmp$value <- tmp[, stat_param]
    tmp <- tmp %>% select(-c(stat_param))
    tmp$stat_param <- stat_param
    tmp$run_key <- factor(tmp$run_key, levels = budget_codes)
    tmp$unique_key <- paste0(tmp$run_key, "_", tmp$stat_param)
    if (is.null(df)) {
      df <- tmp
    } else {
      df <- rbind(df, tmp)
    }
  }

  gr <- ggplot2::ggplot(data=df, aes(x=.data$epoch, y=.data$value,
                                     group=.data$unique_key)) +
    geom_line(aes(linetype=.data$stat_param, colour = .data$run_key),
              linewidth = line_width)+
    geom_point(aes(shape=.data$run_key, colour = .data$run_key)) +
    scale_color_manual(values = colours) +
    scale_linetype_manual(values=line_types) +
    scale_x_continuous(limits = c(x_info$min, x_info$max), breaks = x_ticks) +
    scale_y_continuous(limits = c(y_info$min, y_info$max), breaks = y_ticks) +
    labs(
      x = "",
      y = paste0(y_info$title, "\n"),
      colour = legend_title,
      linetype = "Statistic",
      shape = legend_title
    ) +
    guides(fill=guide_legend(title=legend_title)) +
    theme_classic() +
    theme(text = element_text(size = 12, family = "sans"))

  return(gr)

}
