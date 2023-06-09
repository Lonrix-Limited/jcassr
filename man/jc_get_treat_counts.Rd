% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/treat_counts.R
\name{jc_get_treat_counts}
\alias{jc_get_treat_counts}
\title{Gets number of times a treatment occurs on each element.}
\usage{
jc_get_treat_counts(
  elem_data,
  treatments,
  treat_codes,
  sum_col_name = "any_treats"
)
}
\arguments{
\item{elem_data}{Data Frame with element level data (e.g. the raw input set).
This set must have a column 'elem_index' containing the zero based index of
each element as processed by Juno Cassandra.}

\item{treatments}{List of treatments which should be the treatments output
from a Juno Cassandra run.}

\item{treat_codes}{Vector containing the names of the treatments for which to
get counts.}

\item{sum_col_name}{Name of the column that holds the sum of all counts for
the treatments considered (i.e. the treatments passed as 'treat_codes')}
}
\description{
\code{jc_get_treat_counts} Takes a set of elements and a set of treatments
and adds to the element set the number of occurrences for each treatment type.
Also adds a sum that shows the occurrences for any treatment in the list of
supplied treatment codes.
}
