#' @title Cardinality of a fuzzy number
#'
#' @description This function estimates/approximates the value of the cardinality (area) of a fuzzy number given by
#' a \code{fzn} object.
#'
#' @usage cardinality(fx,
#'            h = 0.01)
#'
#' @param fx A fuzzy number given by a \code{fzn} object.
#' @param h Numerical, between 0 and 1. Step for alphas in "spline" interpolation.
#' @return A number that approximates the value of the cardinality.
#'
#' @examples
#' fx <- fzn(alpha = c(0, 1),
#'           l = c(3, 4),
#'           u = c(7, 6))
#' plot(fx)
#' cardinality(fx)
#'
#' @export
#'

cardinality <- function(fx,
                        h = 0.01) {

  if (!is.fzn(fx)) {
    stop("fx should be of class fzn!")
  }

  res <- 0
  if (fx$interp == "linear") {
    nalpha <- length(fx$alpha)
    for (i in 2:nalpha) {
      res <- res + (fx$alpha[i] - fx$alpha[i - 1]) * (fx$u[i - 1] - fx$l[i - 1] + fx$u[i] - fx$l[i]) / 2
    }
  } else if (fx$interp == "spline") {
    kk <- seq(from = 0, to = 1, by = h)
    ac <- alphacut(fx, kk)
    nalpha <- length(kk)
    for (i in 2:nalpha) {
      res <- res + h * (ac[3, i - 1] - ac[2, i - 1] + ac[3, i] - ac[2, i]) / 2
    }
    res <- as.double(res)
  } else {
    nalpha <- length(fx$alpha)
    for (i in 1:(nalpha - 1)) {
      res <- res + (fx$alpha[i + 1] - fx$alpha[i]) * (fx$u[i] - fx$l[i])
    }
  }

  return(res)

}
