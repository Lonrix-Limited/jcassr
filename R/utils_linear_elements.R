
#' Get overlap betweeen two linear elements
#'
#' \code{jc_get_element_overlap} gets the overlap between two linear elements
#' defined with start locations (from1, to1) and (from2, to2), respectively
#'
#' @param from1 Start location for the first element
#' @param to1 End location for the first element
#' @param from2 Start location for the second element
#' @param to2 End location for the second element
#'
#' @return length of overlap, in same unit as the location parameters
#' @export
#' @examples
#' jc_get_element_overlap(0,6,3,10)
#'
jc_get_element_overlap <- function(from1, to1, from2, to2) {

  #case 0: segment2 is completely before segment1 OR
  #        seg2 is completely after seg1
  if (to2 <= from1 | from2 >= to1) {
    return(0.0) # zero overlap
  }

  #case 1: seg2 is completely inside seg1
  if (from1 <= from2 & to2 <= to1) {
    return(to2 - from2) #overlap = length of segment2
  }

  #case 2: seg1 is completely inside seg2
  if (from2 <= from1 & to1 <= to2) {
    return(to1 - from1) #overlap = length of seg1
  }

  #case 3: seg2 partially overlaps with start of seg1
  if (from2 <= from1 & to2 <= to1) {
    #Note: we already excluded the case where seg2 is entire before seg1
    return(to2 - from1) #overlap length
  }

  #case 4: seg2 partially overlaps with end of seg1
  if (from2 >= from1 & to2 >= to1) {
    #Note: we already excluded the case where seg2 is entire before seg1
    return(to1 - from2) #overlap length
  }

}
