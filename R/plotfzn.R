#' @title Plot of fuzzy number or fuzzy array
#'
#' @description It plots a fuzzy number of class \code{fzn} or a fuzzy array of
#' class \code{fzarray}. The values at
#' each alpha-cut are joined by the corresponding interpolation method in order
#' to show how new alpha-cuts would be added.
#'
#' @usage plotfzn(x = NULL,
#'                y = NULL,
#'                alpha = NULL,
#'                main = NULL,
#'                xlab = "x",
#'                ylab = "y",
#'                col = "black",
#'                plot_alpha = TRUE,
#'                plot_points = FALSE)
#'
#' @param x A fuzzy number of class \code{fzn} or a fuzzy array of class \code{fzarray}.
#' In this case, all the fuzzy numbers of \code{x} are plotted separately an \code{y} is
#' ignored. Alternatively, if \code{y} is a fuzzy array, \code{x} is a numerical array
#' with the x coordinates of the plot.
#' @param y A fuzzy array of class \code{fzarray} with the y coordinates of the plot.
#' @param alpha A numerical array with the alpha-cuts to be plotted, apart from 0 and 1.
#' @param main Argument passed to function plot.
#' @param xlab Argument passed to function plot.
#' @param ylab Argument passed to function plot.
#' @param col Argument passed to function plot.
#' @param plot_alpha Logical. If \code{TRUE} and \code{y} is \code{NULL}, it plots the alpha-cuts.
#' @param plot_points Logical. If \code{TRUE} and \code{y} is \code{NULL}, it plots the bounds of the intervals
#' [\code{l},\code{u}] as points.
#'
#' @examples
#' x <- fzn(alpha = c(0, 0.1, 0.5, 0.8, 1),
#'          l = c(1, 3, 3, 4.5, 5),
#'          u = c(12, 10, 9, 6.5, 6)) # interp = "approx" by default
#' par(mfrow = c(2, 2))
#' plotfzn(x, main = "interp = \"approx\" (linear)")
#' x$interp <- "spline"
#' plotfzn(x, main = "interp = \"spline\"")
#' x$interp <- "step"
#' plotfzn(x, main = "interp = \"step\" (wide)", plot_points = TRUE)
#' x$interp <- "step2"
#' plotfzn(x, main = "interp = \"step2\" (narrow)", plot_points = TRUE)
#' x$interp <- "spline"
#' par(mfrow = c(1, 2))
#' plotfzn(x, main = "interp = \"spline\"")
#' # Adding new alpha-cuts using spline interpolation (since x$interp == "spline")
#' x <- fzn(alphacut(x, alpha = seq(from = 0, to = 1, by = 0.1)))
#' plotfzn(x, main = "interp = \"approx\", with new alphacuts")
#'
#' par(mfrow = c(2, 2))
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
#' par(mfrow = c(1, 2))
#' fpol <- function(x) { return(0.2 * (-10 + 2* x + 6 * x^2 - x^3)) }
#' x <- seq(from = -2, t = 6, length.out = 101)
#' fzx1 <- x + fzt
#' plotfzn(x = x, y = fuzzyfun(x = fzx1, fun = "fpol"),
#'         alpha = seq(from = 0, to = 1, by = 0.1))
#' fzx2 <- x + fzn(alphacut(fzt, alpha = seq(from = 0, to = 1, by = 0.1)))
#' plotfzn(x = x, y = fuzzyfun(x = fzx2, fun = "fpol"))
#'
#' @export
#'
#' @importFrom graphics lines plot points
#' @importFrom stats splinefun

plotfzn <- function(x = NULL,
                    y = NULL,
                    alpha = NULL,
                    main = NULL,
                    xlab = "x",
                    ylab = "y",
                    col = "black",
                    plot_alpha = TRUE,
                    plot_points = FALSE)
{

  if (inherits(x, "fzn")) {

    if (ylab == "y") {
      ylab <- "alpha"
    }

    if (inherits(x, "fzarray")) {

      aux <- lapply(X = x, FUN = "plotfzn",
                    alpha = alpha,
                    main = main,
                    xlab = xlab,
                    ylab = ylab,
                    col = col,
                    plot_alpha = plot_alpha,
                    plot_points = plot_points)

    } else {

      if (!is.null(alpha)) {
        alpha <- sort(unique(c(0, alpha, 1)))
        x <- fzn(alphacut(x, alpha))
      }

      marg <- max(c(diff(range(x$l)), diff(range(x$u)))) / 2
      xmin <- x$l[1] - marg
      xmax <- x$u[1] + marg

      if (x$interp == "approx") {

        coordx <- c(xmin, x$l, rev(x$u), xmax)
        coordy <- c(0, x$alpha, rev(x$alpha), 0)
        plot(x = coordx,
             y = coordy,
             type = "l",
             main = main,
             xlab = xlab,
             ylab = ylab,
             col = col)

      } else if (x$interp == "spline") {

        h <- 0.01
        kk1 <- seq(from = 0, to = 1, by = h)
        kk2 <- rev(kk1)
        s1 <- splinefun(x$alpha, x$l, method = "hyman")
        s2 <- splinefun(x$alpha, x$u, method = "hyman")
        coordx <- c(xmin, s1(kk1), s2(kk2), xmax)
        coordy <- c(0, kk1, kk2, 0)
        plot(x = coordx,
             y = coordy,
             type = "l",
             main = main,
             xlab = xlab,
             ylab = ylab,
             col = col)

      } else if (x$interp == "step") {

        n <- length(x$alpha) - 1
        marg <- max(c(diff(range(x$l)), diff(range(x$u)))) / 2
        xmin <- x$l[1] - marg
        xmax <- x$u[1] + marg
        coordx <- c(xmin, x$l[1:n], x$u[n:1], xmax)
        coordy <- c(x$alpha, x$alpha[n:1], 0)
        plot(x = coordx, y = coordy,
             type = "s",
             main = main,
             xlab = xlab,
             ylab = ylab,
             col = col)

      } else if (x$interp == "step2") {

        n <- length(x$alpha) - 1
        marg <- max(c(diff(range(x$l)), diff(range(x$u)))) / 2
        xmin <- x$l[1] - marg
        xmax <- x$u[1] + marg
        coordx <- c(xmin, x$l, rev(x$u), xmax)
        coordy <- c(0, x$alpha[1:n], rev(x$alpha), 0, 0)
        plot(x = coordx, y = coordy,
             type = "s",
             main = main,
             xlab = xlab,
             ylab = ylab,
             col = col)

      }

      if (plot_alpha) {
        for (i in 1:length(x$alpha)) {
          lines(x = c(x$l[1], x$u[1]), y = rep(x$alpha[i], 2), col = "grey", lty = "dotted")
        }
      }

      if (plot_points) {
        points(x = c(x$l, x$u), y = rep(x$alpha, 2), type = "p", col = "black")
      }

    }

  } else if (inherits(y, "fzarray")) {

    nfz <- length(y)
    if (is.null(x)) {
      x <- seq(from = 0, to = nfz - 1)
    } else if (length(x) != nfz) {
      stop("x and y must have the same length.")
    }
    if (is.null(alpha)) {
      alpha <- sort(unique(unlist(lapply(X = y,
                                         FUN = function(z) { return(z$alpha) }))))
      #alpha <- c(0, 0.25, 0.5, 1)
    } else {
      alpha <- sort(unique(c(0, alpha, 1)))
    }
    y <- lapply(X = alphacut(y, alpha = alpha), FUN = "fzn")
    nalpha <- length(alpha)
    ly <- matrix(0, nrow = nalpha, ncol = nfz)
    uy <- ly
    for (i in 1:nfz) {
      ly[, i] <- y[[i]]$l
      uy[, i] <- y[[i]]$u
    }
    ylim <- c(min(ly[1, ]), max(uy[1, ]))
    plot(x = x,
         y = ly[nalpha, ],
         ylim = ylim,
         type = "l",
         lwd = 2,
         main = main,
         xlab = xlab,
         ylab = ylab,
         col = col)
    lines(x = x,
          y = uy[nalpha, ],
          type = "l",
          lwd = 2,
          col = col)
    for (i in 1:(nalpha - 1)) {
      lines(x = x,
            y = ly[i, ],
            type = "l",
            col = col)
      lines(x = x,
            y = uy[i, ],
            type = "l",
            col = col)
    }

  } else {
    stop("Only plots of fzn or fzarray class variables.")
  }
}
