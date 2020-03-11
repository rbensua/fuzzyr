#' @title Simplify a fuzzy number
#'
#' @description Simplify a fuzzy number given by an object of class \code{fzn}. It eliminates
#' unnecessary alpha-cuts.
#'
#' @usage simplify(fx,
#'                 abstol_alpha = 1e-3,
#'                 abstol_interp = 1e-6)
#' @param fx A fuzzy  number given by a \code{fzn} object.
#' @param abstol_alpha Absolute tolerance for simplifying nearby alpha-cuts. If three different alpha-cuts
#' differ less than \code{abstol_alpha}, the middle one is eliminated. By default, it is 1e-3.
#' @param abstol_interp Absolute tolerance for simplifying taking into account interpolation. By default,
#' it is 1e-6.
#' @return It returns a fuzzy number.
#'
#' @examples
#' fx <- fzn(alpha = c(0, 0.2, 0.5, 0.8, 1),
#'           l = c(-1, 0, 1, 2, 3),
#'           u = c(8, 7, 6, 5, 4)) # interp = linear by default
#' par(mfrow = c(2, 2))
#' plot(fx, main = "fx (interp = linear)", plot_alpha = TRUE)
#' fx2 <- simplify(fx)
#' plot(fx2, main = "fx simplification using linear interpolation", plot_alpha = TRUE)
#' fx$interp <- "spline"
#' plot(fx, main = "fx (interp = spline)", plot_alpha = TRUE)
#' fx3 <- simplify(fx)
#' plot(fx3, main = "fx simplification using spline interpolation", plot_alpha = TRUE)
#' # For a sufficiently large abstol_alpha or abstol_interp, it returns a trapezoidal (or triangular) fuzzy number:
#' fx4 <- simplify(fx, abstol_alpha = 1)
#' plot(fx4, main = "Trapezoidal version of fx", plot_alpha = TRUE)
#'
#' @export

simplify <- function(fx,
                     abstol_alpha = 1e-3,
                     abstol_interp = 1e-6) {

  if (!is.fzn(fx)) {
    stop("fx should be of class fzn!")
  }

  nalpha <- length(fx$alpha)
  acheck <- rep(TRUE, nalpha)

  if (nalpha > 2) {

    # Primera criba: 3 alphas muy juntos, sobra el del medio

    da <- diff(fx$alpha)
    for (i in 2:(nalpha - 1)) {
      if (da[i - 1] + da[i] < abstol_alpha) {
        acheck[i] <- FALSE
      }
    }

    fx <- fzn(alpha = fx$alpha[acheck], l = fx$l[acheck], u = fx$u[acheck], interp = fx$interp)
    nalpha <- length(fx$alpha)
    acheck <- rep(TRUE, nalpha)

    if (nalpha > 2) {

      # Segunda criba: interpolación

      if (fx$interp == "step") {

        for (a in 2:(nalpha - 1)) {
          acheck[a] <- FALSE
          lerr <- abs(fx$l[a] - fx$l[a - 1])
          uerr <- abs(fx$u[a] - fx$u[a - 1])
          if (max(lerr, uerr) > abstol_interp) {
            acheck[a] <- TRUE
          }
        }

      } else if (fx$interp == "linear") {

        for (a in 2:(nalpha - 1)) {
          acheck[a] <- FALSE
          lcheck <- approx(fx$alpha[acheck],
                           fx$l[acheck],
                           xout = fx$alpha[!acheck],
                           method = "linear",
                           ties = "ordered")$y
          ucheck <- approx(fx$alpha[acheck],
                           fx$u[acheck],
                           xout = fx$alpha[!acheck],
                           method = "linear",
                           ties = "ordered")$y
          lerr <- abs(lcheck - fx$l[!acheck])
          uerr <- abs(ucheck - fx$u[!acheck])
          if (max(lerr, uerr) > abstol_interp) {
            acheck[a] <- TRUE
          }
        }

      } else {

        for (a in 2:(nalpha - 1)) {
          acheck[a] <- FALSE
          xcheck <- fzn(alpha = fx$alpha[acheck], l = fx$l[acheck], u = fx$u[acheck], interp = "spline")
          check <- alphacut(xcheck, fx$alpha[!acheck])
          if (sum(!acheck) > 1) {
            lcheck <- check[2, ]
            ucheck <- check[3, ]
          } else {
            lcheck <- check[1]
            ucheck <- check[2]
          }
          lerr <- abs(lcheck - fx$l[!acheck])
          uerr <- abs(ucheck - fx$u[!acheck])
          if (max(lerr, uerr) > abstol_interp) {
            acheck[a] <- TRUE
          }
        }

      }

    }

  }

  return(fzn(alpha = fx$alpha[acheck],
             l = fx$l[acheck],
             u = fx$u[acheck],
             interp = fx$interp))

}
