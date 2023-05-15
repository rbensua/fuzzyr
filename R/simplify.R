#' @title Simplify a fuzzy number
#'
#' @description Simplify a fuzzy number of class \code{fzn} or a fuzzy array of class \code{fzarray}.
#' It eliminates unnecessary alpha-cuts.
#'
#' @usage simplify(x,
#'                abstol = 1e-6)
#'
#' @param x A fuzzy number of class \code{fzn} or a fuzzy array of class \code{fzarray}.
#' @param abstol Absolute tolerance for simplifying. By default, it is 1e-6.
#'
#' @return It returns a fuzzy number of class \code{fzn} or a fuzzy array of class \code{fzarray}.
#'
#' @examples
#' x <- fzn(alpha = c(0, 0.2, 0.5, 0.8, 1),
#'          l = c(-1, 0, 1, 2, 3),
#'          u = c(8, 7, 6, 5, 4),
#'          interp = "approx")
#' x2 <- simplify(x)
#' xx <- fzn(alpha = c(0, 0.2, 0.5, 0.8, 1),
#'          l = c(-1, 0, 1, 2, 3),
#'          u = c(8, 7, 6, 5, 4),
#'          interp = "spline")
#' xx2 <- simplify(xx)
#' # For a sufficiently large abstol, it returns a trapezoidal (or triangular) fuzzy number:
#' x3 <- simplify(x, abstol = 0.25)
#'
#' @export

simplify <- function(x,
                     abstol = 1e-6) {

  if (!inherits(x, "fzn")) {
    stop("x should be of class fzn or fzarray!")
  }

  if (inherits(x, "fzarray")) {

    res <- structure(lapply(X = x, FUN = "simplify", abstol = abstol),
                     class = c("fzn", "fzarray"))

  } else {

    nalpha <- length(x$alpha)
    acheck <- rep(TRUE, nalpha)

    if (nalpha > 2) {

      if (x$interp == "step") {

        for (a in 2:(nalpha - 1)) {
          acheck[a] <- FALSE
          lerr <- abs(x$l[a] - x$l[a - 1])
          uerr <- abs(x$u[a] - x$u[a - 1])
          if (max(lerr, uerr) > abstol) {
            acheck[a] <- TRUE
          }
        }

      } else {

        if (x$interp == "approx") {
          method <- "linear"
        } else {
          method <- "hyman"
        }

        for (a in 2:(nalpha - 1)) {
          acheck[a] <- FALSE
          lcheck <- do.call(x$interp, args = list(x$alpha[acheck], x$l[acheck], xout = x$alpha[!acheck], method = method))$y
          ucheck <- do.call(x$interp, args = list(x$alpha[acheck], x$u[acheck], xout = x$alpha[!acheck], method = method))$y
          lerr <- abs(lcheck - x$l[!acheck])
          uerr <- abs(ucheck - x$u[!acheck])
          if (max(lerr, uerr) > abstol) {
            acheck[a] <- TRUE
          }
        }

      }

    }

    res <- fzn(alpha = x$alpha[acheck],
               l = x$l[acheck],
               u = x$u[acheck],
               interp = x$interp)

  }

  return(res)

}
