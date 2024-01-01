

#' Gets a breakdown table based on a specific grouping column
#'
#' \code{jc_get_network_breakdown} Gets a breakdown table based on a specific 
#' column that shows the length and percentage length for each group.
#'
#' @param raw_data raw data (as data frame) from which to calculate lengths.
#' @param group_col columns on which to get length grouping. Assumed to be a 
#' factor or character column.
#' @param loc_from_col Name of the column that contains the 'from location' or
#' 'start' of each element
#' @param loc_to_col Name of the column that contains the 'to location' or
#' 'end' of each element
#' @param scale_fact Scaling factor for converting lengths. E.g. pass 1000 to 
#' express the lengths in KM when the original lengths are in Metres.
#' @export
#' 
jc_get_network_breakdown <- function(raw_data, group_col, loc_from_col, 
                                     loc_to_col, scale_fact = 1000) {
  
  network_length <- jc_get_network_length(raw_data, loc_from_col, loc_to_col)/1000
  raw_data[ , "grouper"] <- raw_data[ , group_col]
  raw_data[ , "length"] <- raw_data[ , loc_to_col] - raw_data[ , loc_from_col]
  len_bkdn <- raw_data %>% group_by(.data$grouper) %>% dplyr::summarise(
    count = dplyr::n(),
    length = sum(length)/1000,
    length_perc = round(100 * length/network_length,2)
  )
  
  len_bkdn$length <- len_bkdn$length/scale_fact
  
  len_bkdn <- len_bkdn %>% dplyr::arrange(-length)
  return(len_bkdn)
}


#' Gets a nicely formatted breakdown table based on a specific grouping column
#'
#' \code{jc_get_network_breakdown_nice} Gets a gt breakdown table based on a specific 
#' column that shows the length and percentage length for each group.
#'
#' @param raw_data raw data (as data frame) from which to calculate lengths.
#' @param group_col columns on which to get length grouping. Assumed to be a 
#' factor or character column.
#' @param group_col_label User/Report-friendly label for the grouping column.
#' @param length_label User/Report-friendly label for the length column.
#' @param loc_from_col Name of the column that contains the 'from location' or
#' 'start' of each element
#' @param loc_to_col Name of the column that contains the 'to location' or
#' 'end' of each element
#' @param scale_fact Scaling factor for converting lengths. E.g. pass 1000 to 
#' express the lengths in KM when the original lengths are in Metres.
#' @param length_decimals Number of decimals to show on the length column.
#' @param percent_decimals Number of decimals to show on the percentage length
#' column.
#' @export
#' 
jc_get_network_breakdown_nice <- function(raw_data, group_col, 
                                          group_col_label, length_label, 
                                          loc_from_col, loc_to_col, 
                                          scale_fact = 1000, 
                                          length_decimals = 2,
                                          percent_decimals = 2) {
  
  len_bkdn <- jc_get_network_breakdown(raw_data, group_col, loc_from_col, 
                                       loc_to_col, scale_fact)
  
  tt <- len_bkdn |> gt::gt() |> gt::cols_label(
    grouper = group_col_label,
    count = "Elements",
    length = length_label,
    length_perc = "% of Network Length"
  ) |> 
    gt::fmt_integer(
      columns = c("count"),
      use_seps = TRUE,
    ) |> 
    gt::fmt_number(
      columns = c("length"),
      use_seps = TRUE,
      decimals = length_decimals,
    ) |> 
    gt::fmt_number(
      columns = c("length_perc"),
      use_seps = TRUE,
      decimals = percent_decimals,
    )
  
  return(tt)
  
}


#' Gets network length based on specified 'from' and 'to' location columns.
#'
#' \code{jc_get_network_length} is a worker function that gets total network length 
#' from a data frame based on specified 'from' and 'to' location columns. It is of 
#' course assumed that the raw input provided represents the entire network.
#'
#' @param raw_data raw data (as data frame) from which to calculate lengths.
#' @param loc_from_col Name of the column that contains the 'from location' or
#' 'start' of each element
#' @param loc_to_col Name of the column that contains the 'to location' or
#' 'end' of each element
#' @export
#' 
jc_get_network_length <- function(raw_data, loc_from_col, loc_to_col) {
  
  # Calculate network length
  network_length <- sum(raw_data[, loc_to_col] - raw_data[ , loc_from_col])
  return(network_length)
}