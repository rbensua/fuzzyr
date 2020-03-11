#' @title Fuzzy version of a function
#'
#' @description It computes the fuzzy version of a function applied to a fuzzy number
#' \code{fx} given by a \code{fzn} object.
#'
#' @usage fuzzyfun(fun,
#'                 fx,
#'                 n = 1001,
#'                 ...)
#' @param fun A string with the function name.
#' @param fx A fuzzy number given by a \code{fzn} object.
#' @param n Number of subintervals for finding optimal values in the non-monotone case.
#' @param ... Other arguments for function \code{fun}.
#'
#' @return A fuzzy number of class \code{fzn}.
#'
#' @examples
#' fx <- fzn(c(0, 1), c(-1, 0), c(1, 0))
#' par(mfrow = c(2, 2))
#' plot(fx, main = "fx")
#' fxx <- fx * fx
#' plot(fxx, main = "fx * fx")
#' fx2 <- fuzzyfun("^", fx, y = 2) # It is not the same!
#' plot(fx2, main = "fx^2")
#' fz <- fuzzyfun("sqrt", fuzzyfun("abs", fx))
#' plot(fz, main = "sqrt(abs(fx))")
#'
#' @export

fuzzyfun <- function(fun,
                     fx,
                     n = 1001,
                     ...) {

  if (!is.fzn(fx)) {
    stop("fx should be of class fzn!")
  }
  if (fx$interp == "step") {
    interp <- "step"
  } else {
    interp <- "spline"
  }

  fxx <- seq(from = fx$l[1], to = fx$u[1], length.out = 101)
  dfxx <- diff(do.call(fun, args = list(fxx, ...)))
  if (any(is.nan(dfxx))) {
    stop("NaNs produced")
  }

  if ((length(fx$alpha) == 2) && (!is.crisp(fx))) {
    fx <- fzn(alphacut(fx, alpha = c(0, 0.5, 1)))
  }

  # Checking monotonicity
  if (any(dfxx > 0) && any(dfxx < 0)) {
    nalpha <- length(fx$alpha)
    l <- rep(0, nalpha)
    u <- l
    fxx <- sort(unique(c(seq(from = fx$l[1], to = fx$u[1], length.out = n), fx$l, fx$u)))
    ffxx <- do.call(fun, args = list(fxx, ...))
    for (i in 1:nalpha) {
      ffxxa <- ffxx[(fxx >= fx$l[i]) & (fxx <= fx$u[i])]
      l[i] <- min(ffxxa)
      u[i] <- max(ffxxa)
    }
  } else {
    fl <- do.call(fun, args = list(fx$l, ...))
    fu <- do.call(fun, args = list(fx$u, ...))
    l <- pmin(fl, fu)
    u <- pmax(fl, fu)
  }

  res <- fzn(alpha = fx$alpha, l = l, u = u, interp = interp)
  return(res)
}

