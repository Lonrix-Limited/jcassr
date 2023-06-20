



#' Gets number of times a treatment occurs on each element.
#'
#' \code{jc_get_treat_counts} Takes a set of elements and a set of treatments
#' and adds to the element set the number of occurrences for each treatment type.
#' Also adds a sum that shows the occurrences for any treatment in the list of
#' supplied treatment codes.
#' 
#'
#' @param elem_data Data Frame with element level data (e.g. the raw input set).
#' This set must have a column 'elem_index' containing the zero based index of
#' each element as processed by Juno Cassandra.
#' @param treatments List of treatments which should be the treatments output
#' from a Juno Cassandra run.
#' @param treat_codes Vector containing the names of the treatments for which to
#' get counts.
#' @param sum_col_name Name of the column that holds the sum of all counts for 
#' the treatments considered (i.e. the treatments passed as 'treat_codes')
#' @export
#' @importFrom ggplot2 geom_line
#'
jc_get_treat_counts <- function(elem_data, treatments, treat_codes, 
                                sum_col_name = "any_treats") {
  for (treat_code in treat_codes) {
    df[, treat_code] <- apply(elem_data, 1, .get_treat_count, treatments, treat_code)    
  }
  
  df[ , sum_col_name] = rowSums(df[, treat_codes], na.rm = TRUE)
  return(df)
}

.get_treat_count <- function(row, treatments, treat_code) {
  elem_index <- as.numeric(row[["elem_index"]])
  treats_on_elem <- treatments %>% 
    dplyr::filter(.data$elem_index == !!elem_index & 
                  .data$treatment == !!treat_code)
  return(nrow(treats_on_elem))
}
