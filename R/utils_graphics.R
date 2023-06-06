

#' Gets a vector of colour codes from green to red
#'
#' \code{jc_get_colours} gets a set of colours representing a green-to-red
#' spectrum, based on a given number of matching bins. Order of colours
#' returned is green first, then yellow, then on to red etc.
#'
#'
#' @param nbins number of bins to match the colours to. Valid values are 3,4 and 6
#' @return Vector of HTML colour codes
#' @export
#'
jc_get_colours <- function(nbins) {

  if (nbins == 3) {
    colours <- c("#33ff33", "#ffff66", "#ff4d4d")
  }
  else if (nbins == 4) {
    colours <- c("#33ff33", "#ffff66", "#ff944d", "#ff4d4d")
  }
  else if (nbins == 5) {
    colours <- c("#33ff33", "#ffff66", "#ffc34d", "#ff944d", "#ff4d4d")
  }
  else if (nbins == 6) {
    colours <- c("#33ff33", "#a6ff4d", "#ffff66", "#ffc34d",
                 "#ff944d", "#ff4d4d")
  }
  else {
    stop("jcassr can only get colours for 3, 4 or 6 bins")
  }
  return(colours)
}
