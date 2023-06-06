
#' Do PieceWise Linear Interpolation and return result.
#'
#' \code{jc_utils_get_plmvalue} does an interpolation on a piecewise linear
#' model. The function can optionally constrain the results at the outer limits
#' or extrapolate.
#'
#' @param x x coordinate on which to get the function value
#' @param xpoints x coordinates that define the piecewise linear model. Assumed
#' to be in order of increasing values.
#' @param ypoints y coordinates that define the piecewise linear model
#' @param extrapolate TRUE or FALSE value indicating whether to extrapolate for
#' x-values below or above the limits in xpoints
#' @return real value representing the interpolated or extrapolated value
#' @export
#'
jc_utils_get_plmvalue <- function(x, xpoints, ypoints, extrapolate = FALSE) {

  n <- length(xpoints)

  #First check for values at or outside outer limits
  if (!extrapolate) {
    if (x <= xpoints[1]) return (ypoints[1])
    if (x >= xpoints[n]) return (ypoints[n])
  }
  else {
    if (x <= xpoints[1]) {

      x1 <- xpoints[1]
      x2 <- xpoints[2]
      y1 <- ypoints[1]
      y2 <- ypoints[2]
      slope <- (y2 - y1)/(x2-x1)
      y <- y1 -slope*(x1 - x)
      return(y)
    }

    if (x >= xpoints[n]) {

      x1 <- xpoints[n-1]
      x2 <- xpoints[n]
      y1 <- ypoints[n-1]
      y2 <- ypoints[n]
      slope <- (y2 - y1)/(x2-x1)
      y <- y1 + slope*(x - x1)
      return(y)

    }
  }

  #now find the appropriate segment and interpolate
  for (i in 2:n) {

    x1 <- xpoints[i-1]
    x2 <- xpoints[i]

    if (x1 <= x && x <= x2) {

      y1 <- ypoints[i-1]
      y2 <- ypoints[i]
      slope <- (y2 - y1)/(x2-x1)
      y <- y1 + slope*(x - x1)
      return(y)

    }
  }

  stop(paste0("Piecewise linear interpolation failed for x = ", x))

}


#' Vectorised version to Do PieceWise Linear Interpolation and return results.
#'
#' \code{jc_utils_get_plmvalues} does an interpolation on a piecewise linear
#' model. The function can optionally constrain the results at the outer limits
#' or extrapolate.
#'
#' @param xvalues vector of x coordinates on which to get the function values
#' @param xpoints x coordinates that define the piecewise linear model. Assumed
#' to be in order of increasing values.
#' @param ypoints y coordinates that define the piecewise linear model
#' @param extrapolate TRUE or FALSE value indicating whether to extrapolate for
#' x-values below or above the limits in xpoints
#' @return vector of real values representing the interpolated or extrapolated
#' values
#' @export
#'
jc_utils_get_plmvalues <- function(xvalues, xpoints, ypoints,
                                   extrapolate = FALSE) {

  vectorised_func <- Vectorize(jc_utils_get_plmvalue,
                      vectorize.args = c("x"), TRUE)

  result <- vectorised_func(xvalues, xpoints, ypoints, extrapolate)
  return(result)

}

