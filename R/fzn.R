#' @title Fuzzy Number
#'
#' @description This function creates an object of class \code{fzn}, which is a list
#' containing the alpha-cuts of the membership function, the lower and upper limits of the
#' corresponding alpha-cuts, and the interpolation method. This structure is not the
#' fuzzy number itself because it does not contain the full membership function, but the values
#' of some alpha-cuts, i.e. the values that the fuzzy number can take at some alpha-levels.
#' The reason behind this methodology is that, in most applications, we are only interested
#' in certain alpha-cuts and therefore it is not necessary to work with the full membership
#' function.
#'
#' @usage fzn(alpha,
#'            l,
#'            u,
#'            interp = c("approx", "spline", "step", "step2"))
#'
#' @param alpha A vector with the possibility values, a.k.a. alpha-cuts. It must start at
#' 0 and end at 1, in ascending order. Alternatively, it can be a matrix of 3 rows in
#' which the first row is the alpha-cuts vector, the second row is the corresponding lower
#' limits vector and the third row is the corresponding upper limits vector.
#' @param l A vector of the same length as \code{alpha} with the values of the lower
#' limits of the corresponding alpha-cuts. If \code{alpha} is a matrix with 3 rows,
#' \code{l} is ignored.
#' @param u A vector of the same length as \code{alpha} with the values of the upper
#' limits of the corresponding alpha-cuts. If \code{alpha} is a matrix with 3 rows,
#' \code{u} is ignored.
#' @param interp A string with the function or method used to interpolate new alpha-cuts. Choices
#' are "\code{approx}" (default), "\code{spline}", "\code{step}" or "\code{step2}". For "\code{approx}", it is
#' used the \code{approx} function with "\code{linear}" method. For "\code{spline}", it is used the
#' \code{spline} function with "\code{hyman}" method (monotone cubic spline using Hyman filtering).
#' For "\code{step}", it is used a step interpolation choosing the interval [\code{l},\code{u}]
#' as wide as possible. For "\code{step2}", it is used a step interpolation choosing the interval
#' [\code{l},\code{u}] as narrow as possible.
#'
#' @return An object of class \code{fzn}.
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
#' @export

fzn <-
  function(alpha,
           l,
           u,
           interp = c("approx", "spline", "step", "step2")) {

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
      stop("alpha must end at 1.")
    }

    if (any(diff(alpha) <= 0)) {
      stop("alpha must be in strictly ascending order.")
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

    return(structure(
           list(alpha = alpha,
                l = l,
                u = u,
                interp = interp),
           class = "fzn"))
  }