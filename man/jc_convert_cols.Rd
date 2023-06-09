% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_general.R
\name{jc_convert_cols}
\alias{jc_convert_cols}
\title{Convert a set of columns to a specific type}
\usage{
jc_convert_cols(df, cols_to_convert, conv_type = "numeric")
}
\arguments{
\item{df}{data frame to convert}

\item{cols_to_convert}{vector containing the names of columns to convert}

\item{conv_type}{conversion type. default is 'numeric'. Can also handle
'character' and 'date'}
}
\value{
data frame with updated column types
}
\description{
\code{jc_convert_cols} explicitly converts a set of columns in a data frame
to a specific type.
}
