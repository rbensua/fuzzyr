#' @title Fuzzy Number
#'
#' @description This function creates an object of class \code{fzn}, which is a list
#' containing the alpha-cuts of the membership function, the lower and upper limits of the
#' corresponding alpha-cuts, and the interpolation method.
#'
#' @usage fzn(alpha,
#'     l,
#'     u,
#'     interp = c("linear", "spline", "step"))
#'
#' @param alpha A vector with the possibility values, a.k.a. alpha-cuts. It must start at
#' 0 and end at 1, in ascending order. Alternatively, it can be a matrix of 3 rows in
#' which the first row is the alpha-cuts vector, the second row is the corresponding lower
#' limits vector and the third row is the corresponding upper limits vector. If \code{interp == "step"}
#' the alpha-cut at level 1 is not necessary and if it is not provided then it is automatically built.
#' @param l A vector of the same length as \code{alpha} with the values of the lower
#' limits of the corresponding alpha-cuts. If \code{alpha} is a matrix with 3 rows,
#' \code{l} is ignored.
#' @param u A vector of the same length as \code{alpha} with the values of the upper
#' limits of the corresponding alpha-cuts. If \code{alpha} is a matrix with 3 rows,
#' \code{u} is ignored.
#' @param interp A string with the method used to interpolate new alpha-cuts. Choices
#' are "\code{linear}" (default), "\code{spline}" or "\code{step}". For "\code{linear}", it is
#' used the \code{approx} function with "\code{linear}" method. For "\code{spline}", it is used the
#' \code{spline} function with "\code{hyman}" method (monotone cubic spline using Hyman filtering).
#' For "\code{step}", it is used a step interpolation. In this case, the alpha-cut at level 1 is not relevant.
#'
#' @return An object of class \code{fzn}.
#'
#' @examples
#' fx <- fzn(alpha = c(0, 0.1, 0.5, 0.5, 0.8, 1),
#'           l = c(1, 3, 3, 3, 4.5, 5),
#'           u = c(12, 10, 9, 8, 6.5, 6)) # interp = "linear" by default
#' par(mfrow = c(2, 2))
#' plot(fx, main = "interp = linear", plot_alpha = TRUE)
#' fx$interp <- "step"
#' plot(fx, main = "interp = step", plot_alpha = TRUE)
#' fx$interp <- "spline"
#' plot(fx, main = "interp = spline", plot_alpha = TRUE)
#' # Adding new alpha-cuts using spline interpolation (since fx$interp == "spline")
#' zero <- fzn(alphacut(fuzzyfy(0), alpha = seq(from = 0, to = 1, by = 0.1)))
#' fx <- fx + zero
#' fx$interp <- "linear"
#' plot(fx, main = "Adding alpha-cuts using splines", plot_alpha = TRUE)
#'
#' @export

fzn <-
  function(alpha,
           l,
           u,
           interp = c("linear", "spline", "step")) {

    interp <- tolower(interp)
    interp <- match.arg(interp)

    # Checking alpha
    if (is.matrix(alpha)) {
      if (nrow(alpha) == 3) {
        l <- alpha[2, ]
        u <- alpha[3, ]
        alpha <- alpha[1, ]
      } else {
        stop("alpha must be a vector or a matrix with 3 rows.")
      }
    }

    n <- length(alpha)
    if (alpha[1] != 0) {
      stop("alpha must start at 0.")
    }

    if (alpha[n] != 1) {
      if (interp == "step") {
        alpha <- c(alpha, 1)
        l <- c(l, l[n])
        u <- c(u, u[n])
      } else {
        stop("alpha must end at 1.")
      }
    }

    if (any(diff(alpha) < 0)) {
      stop("alpha must be in ascending order.")
    }

    # Checking l and u
    if (length(l) != n) {
      stop("l must be of the same length as alpha.")
    }
    if (length(u) != n) {
      stop("u must be of the same length as alpha.")
    }
    if (any(diff(c(l, rev(u))) < 0)) {
      stop("Not ordered lower and upper limits. Check the lower and upper limits!")
    }

    # The same alpha-cut must appear twice at most
    alphalog <- rep(TRUE, n)
    for (i in 2:(n - 1)) {
      if ((alpha[i] == alpha[i - 1]) && (alpha[i] == alpha[i + 1])) {
        alphalog[i] <- FALSE
      }
    }
    alpha <- alpha[alphalog]
    l <- l[alphalog]
    u <- u[alphalog]

    return(structure(
           list(alpha = alpha,
                l = l,
                u = u,
                interp = interp),
           class = "fzn"))
  }
