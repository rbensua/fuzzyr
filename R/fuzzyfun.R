#' @title Fuzzy version of a function
#'
#' @description It computes the fuzzy version of a function applied to a fuzzy number
#' \code{x} of class \code{fzn} or a fuzzy array of class \code{fzarray}.
#'
#' @usage fuzzyfun(x,
#'                 fun,
#'                 n = 1001,
#'                 ...)
#'
#' @param x A fuzzy number of class \code{fzn} or a fuzzy array of class \code{fzarray}.
#' @param fun A string with the function name.
#' @param n Number of subintervals for finding optimal values in the non-monotone case.
#' @param ... Other arguments for function \code{fun}.
#'
#' @return A fuzzy number of class \code{fzn} or a fuzzy array of class \code{fzarray}.
#'
#' @examples
#' x <- fzn(alpha = c(0, 1), l = c(0, 1), u = c(2, 1))
#' par(mfrow = c(2, 2))
#' plotfzn(x, main = "Triangular x")
#' y <- fuzzyfun(x, fun = "sqrt")
#' plotfzn(y, main = "sqrt(x)")
#' x2 <- fzn(alphacut(x, alpha = seq( from = 0, to = 1, by = 0.1)))
#' plotfzn(x2, main = "x with extra alpha-cuts")
#' y2 <- fuzzyfun(x2, fun = "sqrt")
#' plotfzn(y2, main = "sqrt(x)")
#'
#' x <- fzn(alpha = c(0, 1), l = c(-1, 0), u = c(1, 0))
#' x <- fzn(alphacut(x, alpha = seq( from = 0, to = 1, by = 0.1)))
#' plotfzn(x, main = "Triangular x")
#' z <- fuzzyfun(fuzzyfun(x, fun = "abs"), fun = "sqrt")
#' plotfzn(z, main = "sqrt(abs(x))")
#' xx <- x * x
#' plotfzn(xx, main = "x * x")
#' x2 <- fuzzyfun(x, fun = "^", y = 2) # It is not the same!
#' plotfzn(x2, main = "x ^ 2")
#'
#' x <- seq(from = 0, to = 2 * pi, length.out = 101)
#' fzt <- fzn(alpha = c(0, 1), l = c(-0.5, 0), u = c(0.5, 0))
#' fzx1 <- x + fzt
#' fzy1 <- fuzzyfun(fzx1, fun = "^", y = 2)
#' fzx2 <- x + fzn(alphacut(fzt, alpha = c(0, 0.25, 0.5, 0.75, 1)))
#' fzy2 <- fuzzyfun(fzx2, fun = "^", y = 2)
#' fy1b <- fuzzyfun(fzx2, fun = "sin")
#' plotfzn(x = x, y = fzx1)
#' plotfzn(x = x, y = fzy1)
#' plotfzn(x = x, y = fzy2)
#' plotfzn(x = x, y = fy1b)
#'
#' @export

fuzzyfun <- function(x,
                     fun,
                     n = 1001,
                     ...) {

  if (!inherits(x, "fzn")) {
    stop("x should be of class fzn or fzarray!")
  }

  if (inherits(x, "fzarray")) {

    res <- structure(lapply(X = x, FUN = "fuzzyfun", fun = fun, n = n, ... = ...),
                     class = c("fzn", "fzarray"))

  } else {

    interp <- x$interp
    #if (x$interp == "step") {
    #  interp <- "step"
    #} else if (x$interp == "step2") {
    #  interp <- "step2"
    #} else {
    #  interp <- "spline"
    #}

    xx <- seq(from = x$l[1], to = x$u[1], length.out = 101)
    dfxx <- diff(do.call(fun, args = list(xx, ...)))
    if (any(is.nan(dfxx))) {
      stop("NaNs produced")
    }

    #if ((length(x$alpha) == 2) && (!is.crisp(x))) {
    #  x <- fzn(alphacut(x = x, alpha = c(0, 0.5, 1)))
    #}

    # Checking monotonicity
    if (any(dfxx > 0) && any(dfxx < 0)) {
      nalpha <- length(x$alpha)
      l <- rep(0, nalpha)
      u <- l
      xx <- sort(unique(c(seq(from = x$l[1], to = x$u[1], length.out = n), x$l, x$u)))
      fxx <- do.call(fun, args = list(xx, ...))
      for (i in 1:nalpha) {
        fxxa <- fxx[(xx >= x$l[i]) & (xx <= x$u[i])]
        l[i] <- min(fxxa)
        u[i] <- max(fxxa)
      }
    } else {
      fl <- do.call(fun, args = list(x$l, ...))
      fu <- do.call(fun, args = list(x$u, ...))
      l <- pmin(fl, fu)
      u <- pmax(fl, fu)
    }

    res <- fzn(alpha = x$alpha, l = l, u = u, interp = interp)

  }

  return(res)

}

