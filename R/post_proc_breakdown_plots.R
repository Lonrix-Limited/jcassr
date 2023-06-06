
#' Gets a breakdown plot of values for a specific parameter over epochs.
#'
#' \code{jc_get_breakdown_plot} Gets a plot with the frequency breakdown of
#' predicted parameter data for a specific parameter over epochs.
#'
#' @param param_data Forecasted data for the parameter, as loaded from the export
#' file created by jcass.
#' @param epochs Vector with the calendar epochs for which to get breakdown The numbers
#' in this vector must match the columns in the parameter output file, otherwise
#' errors will occur.
#' @param bins Bins with the limits for different breakdown groups.
#' @param colours Optional vector with colours to match each bin. Number should
#' be at least one less than the size of the bins vector. If the default value
#' of NULL is passed, then jcassr will automatically choose stoplight colours.
#' @param legend_title Title for the legend.
#' @export
jc_get_breakdown_plot <- function(param_data, epochs, bins, colours = NULL,
                                  legend_title = "Category") {

  if (is.null(colours)) {
    colours <- jc_get_colours(length(bins))
  }
  df <- jc_get_breakdown_table(param_data, epochs, bins)
  bin_codes <- (unique(df$bin))
  df$bin <- factor(df$bin, levels = bin_codes)

  df <- df %>% gather(as.character(epochs), key = "epoch", value = "value")

  gr <- ggplot2::ggplot(df, ggplot2::aes(x = .data$epoch, y = .data$value,
                                         fill = .data$bin)) +
    ggplot2::geom_col(position = "fill") +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_fill_manual(values = colours) + ggplot2::theme_classic() +
    labs(
      x = "",
      y = "% of Elements",
      colour = legend_title,
      linetype = legend_title,
      shape = legend_title
    ) +
    guides(fill=guide_legend(title=legend_title)) +
    ggplot2::theme_classic() +
    ggplot2::theme(text = element_text(size = 14, family = "sans"))
  return(gr)
}

#' Gets a breakdown plot of percentage length for a specific parameter over epochs.
#'
#' \code{jc_get_breakdown_plot_pct_len} Gets a plot with the percentage length
#' breakdown of predicted parameter data for a specific parameter over epochs.
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
#' @param colours Optional vector with colours to match each bin. Number should
#' be at least one less than the size of the bins vector. If the default value
#' of NULL is passed, then jcassr will automatically choose stoplight colours.
#' @param legend_title Title for the legend.
#' @export
jc_get_breakdown_plot_pct_len <- function(param_data, epochs, bins, network_length,
                                          loc_from_col = "loc_from",
                                          loc_to_col = "loc_to",
                                          colours = NULL,
                                          legend_title = "Category") {

  if (is.null(colours)) {
    colours <- jc_get_colours(length(bins))
  }
  df <- jc_get_breakdown_length_pct(param_data, epochs, bins, network_length,
                                    loc_from_col = loc_from_col,
                                    loc_to_col = loc_to_col)
  bin_codes <- (unique(df$bin))
  df$bin <- factor(df$bin, levels = bin_codes)

  df <- df %>% gather(as.character(epochs), key = "epoch", value = "value")

  gr <- ggplot2::ggplot(df, ggplot2::aes(x = .data$epoch, y = .data$value,
                                         fill = .data$bin)) +
    ggplot2::geom_col(position = "fill") +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_fill_manual(values = colours) + ggplot2::theme_classic() +
    labs(
      x = "",
      y = "Percentage of Network Length",
      colour = legend_title,
      linetype = legend_title,
      shape = legend_title
    ) +
    guides(fill=guide_legend(title=legend_title)) +
    ggplot2::theme_classic() +
    ggplot2::theme(text = element_text(size = 14, family = "sans"))
  return(gr)
}
