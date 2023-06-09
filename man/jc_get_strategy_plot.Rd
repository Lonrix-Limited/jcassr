% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/area_under_curve_helper.R
\name{jc_get_strategy_plot}
\alias{jc_get_strategy_plot}
\title{Plot the Area-Under-the-Curve for various Strategies}
\usage{
jc_get_strategy_plot(strat_detail_data)
}
\arguments{
\item{strat_detail_data}{data frame with data loaded from the 'detail' sheet
of the Strategy Debug export file}
}
\value{
GGPLOT graph that can be further customised
}
\description{
\code{jc_get_strategy_plot} Does a nice plot of the area under the curve
using data from a jcass Strategy Debug export file. The graph plots the
objective values for each strategy and shows when and what treatments are
involved.
}
