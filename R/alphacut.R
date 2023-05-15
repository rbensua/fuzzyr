#' @title Alpha-cuts of a fuzzy number
#'
#' @description This function gives the alpha-cuts of a fuzzy number \code{x} of
#' class \code{fzn} or a fuzzy array of class \code{fzarray}, using the interpolation
#' method given by the field "interp" of each fuzzy number.
#'
#' @usage alphacut(x,
#'                 alpha)
#'
#' @param x A fuzzy number of class \code{fzn} or a fuzzy array of class \code{fzarray}.
#' @param alpha A numeric vector of numbers between 0 and 1 corresponding to the desired
#' possibility values.
#'
#' @return If \code{x} is a fuzzy number, there are two possibilities: if \code{length(alpha)} == 1 it returns a vector of length 2 containing the range of
#' the alpha-cut (lower value, upper value); otherwise it returns a matrix of size 3 x \code{length(alpha)}.
#' In this case, the 1st row is the alphas considered, while the 2nd and 3rd rows are the lower and upper values of
#' the corresponding alpha-cut, respectively.
#' If \code{x} is a fuzzy array, it returns a list with the outputs of each fuzzy number.
#'
#' @examples
#' x <- fzn(alpha = c(0, 1), l = c(0, 1), u = c(2, 1))
#' par(mfrow = c(2, 2))
#' plotfzn(x, main = "Triangular x")
#' y <- fuzzyfun(fun = "sqrt", x)
#' plotfzn(y, main = "sqrt(x)")
#' x2 <- fzn(alphacut(x, alpha = seq( from = 0, to = 1, by = 0.1)))
#' plotfzn(x2, main = "x with extra alpha-cuts")
#' y2 <- fuzzyfun(fun = "sqrt", x2)
#' plotfzn(y2, main = "sqrt(x)")
#'
#' x <- fzn(alpha = c(0, 0.1, 0.5, 0.8, 1),
#'          l = c(1, 3, 3, 4.5, 5),
#'          u = c(12, 10, 9, 6.5, 6),
#'          interp = "spline") # interp = "approx" by default
#' par(mfrow = c(1, 2))
#' plotfzn(x, main = "interp = spline")
#' # Adding new alpha-cuts using spline interpolation (since x$interp == "spline")
#' x <- fzn(alphacut(x, alpha = seq(from = 0, to = 1, by = 0.1)))
#' plotfzn(x, main = "interp = approx, with new alphacuts")
#'
#' @export

alphacut <- function(x,
                     alpha) {

  if (any(alpha < 0) || any(alpha > 1)) {
    stop("All alpha values should be numbers between 0 and 1.")
  }
  alpha <- sort(unique(alpha)) # Order the values

  if (inherits(x, "fzn")) {

    if (inherits(x, "fzarray")) {

      res <- lapply(X = x, FUN = "alphacut",
                    alpha = alpha)

    } else {

      if (x$interp == "approx") {

        l <- do.call(x$interp, args = list(x$alpha, x$l, xout = alpha, method = "linear"))$y
        u <- do.call(x$interp, args = list(x$alpha, x$u, xout = alpha, method = "linear"))$y

      } else if (x$interp == "spline") {

        l <- do.call(x$interp, args = list(x$alpha, x$l, xout = alpha, method = "hyman"))$y
        u <- do.call(x$interp, args = list(x$alpha, x$u, xout = alpha, method = "hyman"))$y

      } else if (x$interp == "step") {

        nalpha <- length(alpha)
        l <- rep(0, nalpha)
        u <- l
        for (i in 1:nalpha) {
          ix <- sum(x$alpha <= alpha[i])
          l[i] <- x$l[ix]
          u[i] <- x$u[ix]
        }

      } else if (x$interp == "step2") {

        nalpha <- length(alpha)
        l <- rep(0, nalpha)
        u <- l
        for (i in 1:nalpha) {
          ix <- sum(x$alpha < alpha[i]) + 1
          l[i] <- x$l[ix]
          u[i] <- x$u[ix]
        }

      }

      if (length(alpha) == 1) {
        res <- c(l, u)
      } else {
        res <- rbind(alpha, l, u)
        return(res)
      }
    }

  } else {
    stop("x should be of class fzn or fzarray!")
  }

  return(res)

}

