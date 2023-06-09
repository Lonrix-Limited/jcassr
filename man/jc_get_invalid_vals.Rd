% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lookup_helpers.R
\name{jc_get_invalid_vals}
\alias{jc_get_invalid_vals}
\title{Gets all invalid values in a set of columns based on a lookup set}
\usage{
jc_get_invalid_vals(data, lookup_columns, lookup_sets)
}
\arguments{
\item{data}{Data frame with raw input data to check}

\item{lookup_columns}{Data frame containing information on the columns to
be looked up. This dataframe must contain column names: 'source_column',
'lookup_name' and 'target_column'. Here, source_column must map to a name in
the raw input data, and lookup_name must map to one of the lookup names
specified in the lookup_sets parameter}

\item{lookup_sets}{Data frame containing all the lookup sets to be used for
the various columns.This dataframe must contain column names: 'lookup_name',
and 'keys'. Keys specifies the allowed values that will be used to find
invalid values}
}
\value{
A data frame containing two columns: column 'column_name' contains
the name of the column in the raw data on which invalid values were found,
and ' invalid_values' contains all invalid values for that column
}
\description{
\code{jc_get_invalid_vals} finds all invalid values in a set of columns in
the source data, where 'invalid' means any value that is not listed in the
lookup set for that column.
}
\details{
For example, if the lookup set for a column contains keys 'small', 'medium'
and 'large', then this method will find any values (excluding NA)  that are
not in that list. For example, if the column contains 'very small' and
'very large' then the output will list those two values for this column.

This method is useful to quickly find values that are approximately correct
so that they can be corrected rather than assigned NA values.
}
