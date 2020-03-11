#' @title Alpha-cuts of a fuzzy number
#'
#' @description This function gives the alpha-cuts of a fuzzy number given by an object
#' \code{fx} of class \code{fzn}, using the interpolation method given by the field
#' "interp" of \code{x}.
#'
#' @usage alphacut(fx,
#'                 alpha)
#' @param fx A fuzzy number given by a \code{fzn} object.
#' @param alpha A numeric vector of numbers between 0 and 1 corresponding to the desired
#' possibility values.
#' @return If \code{length(alpha)} == 1 it returns a vector of length 2 containing the range of
#' the alpha-cut (lower value, upper value). Otherwise it returns a matrix of size 3 x \code{length(alpha)}.
#' The first row will be the alphas considered and the rows 2 and 3 are the lower and upper values of
#' the corresponding alpha-cut, respectively.
#'
#' @examples
#' fx <- fzn(alpha = c(0, 0.1, 0.5, 0.8, 1),
#'           l = c(1, 3, 3, 4.5, 5),
#'           u = c(12, 10, 9, 6.5, 6),
#'           interp = "spline")
#' # Computing some alpha-cuts using spline interpolation (since fx$interp == "spline")
#' alphacut(fx, alpha = seq(from = 0, to = 1, by = 0.1))
#' # Adding new alpha-cuts to fx using spline interpolation
#' fx <- fzn(alphacut(fx, alpha = seq(from = 0, to = 1, by = 0.1)))
#'
#' @export

alphacut <- function(fx,
                     alpha) {

  if (!is.fzn(fx)) {
    stop("fx should be of class fzn!")
  }
  if (any(alpha < 0) || any(alpha > 1)) {
    stop("All alpha values should be numbers between 0 and 1.")
  }
  alpha <- sort(alpha) # Order the values
  nalpha <- length(alpha)

  if (fx$interp == "step") {

    l <- rep(0, nalpha)
    u <- l
    for (i in 1:nalpha) {
      ix <- sum(fx$alpha <= alpha[i])
      l[i] <- fx$l[ix]
      u[i] <- fx$u[ix]
    }

  } else if (fx$interp == "linear") {

    l <- approx(fx$alpha, fx$l, xout = alpha, method = "linear", ties = "ordered")$y
    u <- approx(fx$alpha, fx$u, xout = alpha, method = "linear", ties = "ordered")$y

  } else {

    nxa <- length(fx$alpha)
    alphatrams_idx <- matrix(c(TRUE, rep(FALSE, nxa - 1)), nrow = 1)
    ntram <- 1
    for (i in 2:nxa) {
      if (fx$alpha[i] == fx$alpha[i-1]) {
        alphatrams_idx <- rbind(alphatrams_idx, c(rep(FALSE, i - 1), TRUE, rep(FALSE, nxa - i)))
        ntram <- ntram + 1
      } else {
        alphatrams_idx[ntram, i] <- TRUE
      }
    }
    l <- rep(0, nalpha)
    u <- l
    for (i in 1:nalpha) {
      if (alpha[i] == 0) {
        l[i] <- fx$l[1]
        u[i] <- fx$u[1]
      } else if (alpha[i] == 1) {
        l[i] <- fx$l[nxa]
        u[i] <- fx$u[nxa]
      } else {
        aux <- max(which(fx$alpha <= alpha[i]))
        tram <- sum(diff(fx$alpha)[1:aux] == 0) + 1
        tramrow <- alphatrams_idx[tram, ]
        l[i] <- spline(fx$alpha[tramrow], fx$l[tramrow], xout = alpha[i], method = "hyman")$y
        u[i] <- spline(fx$alpha[tramrow], fx$u[tramrow], xout = alpha[i], method = "hyman")$y
      }
    }

  }

  if (length(alpha) == 1) {
    return(c(l, u))
  } else {
    res <- rbind(alpha, l, u)
    return(res)
  }
}

