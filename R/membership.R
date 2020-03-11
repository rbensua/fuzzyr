#' @title Degree of membership
#'
#' @description This function computes the degree of membership of a vector of real values
#' in a given fuzzy number of class \code{fzn}.
#'
#' @usage membership(fx,
#'                   xout,
#'                   h = 0.01)
#'
#' @param fx A fuzzy number given by a \code{fzn} object.
#' @param xout A numeric vector of real values from which their degrees of membership will
#' be computed.
#' @param h Numerical, between 0 and 1. Step for alphas in "spline" interpolation.
#'
#' @return A list with \code{xout} and their corresponding degrees of membership.
#'
#' @examples
#'
#' @export
#'

membership <-
  function(fx,
           xout,
           h = 0.01) {

    if (!is.fzn(fx)) {
      stop("fx should be of class fzn!")
    }

    if (fx$interp == "linear") {

      res <- approx(x = c(fx$l, rev(fx$u)),
                    y = c(fx$alpha, rev(fx$alpha)),
                    xout = xout,
                    method = "linear",
                    ties = max)

    } else if (fx$interp == "spline") {

      kk <- seq(from = 0, to = 1, by = h)
      ac <- alphacut(fx, kk)
      res <- approx(x = c(ac[2, ], rev(ac[3, ])),
                    y = c(kk, rev(kk)),
                    xout = xout,
                    method = "linear",
                    ties = max)

    } else {

      n <- length(fx$alpha) - 1
      res <- approx(x = c(fx$l[1], fx$l[1:n], fx$u[n:1], fx$u[1]),
                    y = c(fx$alpha, fx$alpha[n:1], 0),
                    xout = xout,
                    method = "constant",
                    ties = max)

    }

    res$y[is.na(res$y)] <- 0
    return(res)

  }

