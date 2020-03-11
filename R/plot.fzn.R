#' @title Plot an fuzzy number
#'
#' @description Plots an fzn object.
#'
#' @usage plot(fx,
#'             main = NULL,
#'             xlab = "x",
#'             ylab = "alpha",
#'             col = "black",
#'             plot_alpha = FALSE,
#'             h = 0.01)
#'
#' @param fx A fuzzy number defined by an object of \code{fzn} class.
#' @param main Argument passed to function plot.
#' @param xlab Argument passed to function plot.
#' @param ylab Argument passed to function plot.
#' @param col Argument passed to function plot.
#' @param plot_alpha Logical. If \code{TRUE}, it plots the alpha-cuts.
#' @param h Numerical, between 0 and 1. Step for alphas in "spline" graphic.
#'
#' @examples
#' fx <- fzn(alpha = c(0, 0.1, 0.5, 0.8, 1),
#'          l = c(1, 3, 3, 4.5, 5),
#'          u = c(12, 10, 9, 6.5, 6),
#'          interp = "spline")
#' plot(fx, main = "Example of fuzzy number", plot_alpha = TRUE)
#' @method plot fzn
#' @export
#'
plot.fzn <- function(fx,
                     main = NULL,
                     xlab = "x",
                     ylab = "alpha",
                     col = "black",
                     plot_alpha = FALSE,
                     h = 0.01)
{
  if (is.fzn(fx)) {
    #marg <- max(c(diff(range(fx$l)), diff(range(fx$u)))) / 2
    marg <- 0
    xmin <- fx$l[1] - marg
    xmax <- fx$u[1] + marg
    if (fx$interp == "linear") {
      coordx <- c(xmin, fx$l, rev(fx$u), xmax)
      coordy <- c(0, fx$alpha, rev(fx$alpha), 0)
      plot(x = coordx,
           y = coordy,
           type = "l",
           main = main,
           xlab = xlab,
           ylab = ylab,
           col = col)
    } else if (fx$interp == "spline") {
      kk <- seq(from = 0, to = 1, by = h)
      ac <- alphacut(fx, kk)
      coordx <- c(xmin, ac[2, ], rev(ac[3, ]), xmax)
      coordy <- c(0, kk, rev(kk), 0)
      plot(x = coordx,
           y = coordy,
           type = "l",
           main = main,
           xlab = xlab,
           ylab = ylab,
           col = col)
    } else {
      n <- length(fx$alpha) - 1
      coordx <- c(xmin, fx$l[1:n], fx$u[n:1], xmax)
      coordy <- c(fx$alpha, fx$alpha[n:1], 0)
      plot(x = coordx,
           y = coordy,
           type = "s",
           main = main,
           xlab = xlab,
           ylab = ylab,
           col = col)
    }
    if (plot_alpha) {
      for (i in 1:length(fx$alpha)) {
        lines(x = c(fx$l[1], fx$u[1]), y = rep(fx$alpha[i], 2), col = "grey", lty = "dotted")
      }
    }

  } else {
    stop("Input fx is not a fzn class variable!")
  }
}
