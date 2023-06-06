

#' Gets a graph showing percentage Budget Utilisation over time.
#'
#' \code{jc_get_spend_graph} Gets ggplot object showing lines representing
#' budget utilisation for different treatment categories over time.
#'
#' @param epoch Epoch for which to get the cross-cut information
#' @param stats_params Vector of statistics to get
#' @param budget_codes Budget codes for which to get data
#' @param budget_xvals Relative X-values associated with each budget code
#' @param param_name Name of the model parameter
#' @param calendar_epochs Vector with the calendar epochs for which to get stats.
#' The numbers in this vector must match the columns in the parameter output
#' file, otherwise errors will occur.
#' @param outputs_base Base stub for relative path to parameter output files.
#' @export
#' @importFrom ggplot2 geom_line
#'
jc_get_crosscut_data <- function(epoch, stats_params, budget_codes,
                                 budget_xvals, param_name,
                                 calendar_epochs,
                                 outputs_base = "outputs/paramdata_") {

  n <- length(budget_codes)
  stats_values <- list()
  for (stat in stats_params) {
    stats_values[[stat]] <- rep(NA, n)
  }
  epochs <- rep(epoch, n)

  i <- 1
  for (budget_code in budget_codes) {

    params_export <- paste0(outputs_base, budget_code, ".xlsx")
    param_data <- as.data.frame(read_xlsx(params_export, param_name))
    stats <- jc_get_param_stats_table(param_data, calendar_epochs)

    for (stat in stats_params) {
      stats_values[[stat]][i] <- stats[stats$epoch == epoch, stat]
    }
    i <- i + 1
  }

  df <- data.frame(budget_code = budget_codes, budget_xval = budget_xvals,
                   crosscut_epoch = epochs)

  for (stat in stats_params) {
    df[, stat] <- stats_values[[stat]]
  }

  df <- df %>% dplyr::arrange(.data$budget_xval)
  return(df)

}

#' Gets a graph showing a Cross-Cut statistic for a specific parameter.
#'
#' \code{jc_get_crosscut_graph} Gets ggplot object showing a crosscut graph that
#' represents a specific statistic for each budget at a specific epoch.
#'
#' @param crosscut_data Data frame with crosscut statistics for the parameter,
#' as provided by method \code{link{jc_get_crosscut_data}}.
#' @param stats_params Vector of stats parameters to plot. See the output for method
#' \code{link{jc_get_param_stats_table}} to see what stats are available, and
#' also how the codes (column names) to use. Generally, for percentiles, you can
#' use 'perc_90', 'perc_25' etc. Also 'mean', 'median', 'max' and 'min.
#' @param colours Vector of colour names or HTML colour codes, to match
#' the statistics codes passed in as parameter \code{stat_codes}.
#' @param line_types Vector of line style names to match
#' the statistics codes passed in as parameter \code{stat_codes} (valid
#' values are: 'solid', 'dashed', 'dotted', dotdash', 'longdash' etc. ).
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
jc_get_crosscut_graph <- function(crosscut_data, stats_params, colours,
                                  line_types,
                                  y_info, line_width = 1,
                                  legend_title = "Statistic") {

  y_ticks <- seq(y_info$min, y_info$max, by = y_info$increm)

  budget_labels <- crosscut_data$budget_code

  crosscut_data <- crosscut_data %>% gather(all_of(stats_params),
                                            key = "statistic",
                                            value = "value")

  gr <- ggplot2::ggplot(data=crosscut_data,
                        aes(x= .data$budget_xval, y=.data$value,
                            group=.data$statistic)) +
    geom_line(aes(linetype=.data$statistic, colour = .data$statistic),
              linewidth = line_width)+
    geom_point(aes(shape=.data$statistic, colour = .data$statistic)) +
    scale_color_manual(values = colours) +
    scale_linetype_manual(values=line_types) +
    scale_x_continuous(breaks = crosscut_data$budget_xval,
                       labels = crosscut_data$budget_code) +
    scale_y_continuous(limits = c(y_info$min, y_info$max),
                       breaks = y_ticks) +
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
