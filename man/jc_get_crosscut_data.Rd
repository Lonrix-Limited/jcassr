% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post_proc_cross_cut.R
\name{jc_get_crosscut_data}
\alias{jc_get_crosscut_data}
\title{Gets a graph showing percentage Budget Utilisation over time.}
\usage{
jc_get_crosscut_data(
  epoch,
  stats_params,
  budget_codes,
  budget_xvals,
  param_name,
  calendar_epochs,
  outputs_base = "outputs/paramdata_"
)
}
\arguments{
\item{epoch}{Epoch for which to get the cross-cut information}

\item{stats_params}{Vector of statistics to get}

\item{budget_codes}{Budget codes for which to get data}

\item{budget_xvals}{Relative X-values associated with each budget code}

\item{param_name}{Name of the model parameter}

\item{calendar_epochs}{Vector with the calendar epochs for which to get stats.
The numbers in this vector must match the columns in the parameter output
file, otherwise errors will occur.}

\item{outputs_base}{Base stub for relative path to parameter output files.}
}
\description{
\code{jc_get_spend_graph} Gets ggplot object showing lines representing
budget utilisation for different treatment categories over time.
}
