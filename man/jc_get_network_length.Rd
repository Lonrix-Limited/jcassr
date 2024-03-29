% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/network_breakdown_info.R
\name{jc_get_network_length}
\alias{jc_get_network_length}
\title{Gets network length based on specified 'from' and 'to' location columns.}
\usage{
jc_get_network_length(raw_data, loc_from_col, loc_to_col)
}
\arguments{
\item{raw_data}{raw data (as data frame) from which to calculate lengths.}

\item{loc_from_col}{Name of the column that contains the 'from location' or
'start' of each element}

\item{loc_to_col}{Name of the column that contains the 'to location' or
'end' of each element}
}
\description{
\code{jc_get_network_length} is a worker function that gets total network length
from a data frame based on specified 'from' and 'to' location columns. It is of
course assumed that the raw input provided represents the entire network.
}
