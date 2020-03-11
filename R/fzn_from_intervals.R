#' @title Generate Fuzzy Number from Sample Intervals
#'
#' @description This function creates an object of class \code{fzn}, which is a list
#' containing the alpha-cuts of the membership function, the lower and upper limits of the
#' corresponding alpha-cuts, and the interpolation method.
#'
#' @usage fzn(alpha,
#'            l,
#'            u,
#'            interp = c("approx", "spline", "step"))
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
#' @param interp A string with the function or method used to interpolate new alpha-cuts. Choices
#' are "\code{approx}" (default), "\code{spline}" or "\code{step}". For "\code{approx}", it is
#' used the \code{approx} function with "\code{linear}" method. For "\code{spline}", it is used the
#' \code{spline} function with "\code{hyman}" method (monotone cubic spline using Hyman filtering).
#' For "\code{step}", it is used a step interpolation. In this case, the alpha-cut at level 1 is not relevant.
#'
#' @return An object of class \code{fzn}.
#'
#' @examples
#' l <- c(0.75,1.75)
#' u <- c(4.5,3.25)
#'
#' nest_l <- c(1.5,1,2, 0.5)
#' nest_u <- c(3.5,4,3, 5)
#'
#' fx <- fzn_from_intervals(l, u, nest_l, nest_u)
#' plot(fx)
#'
#' @export
#'
#'

fzn_from_intervals <- function(l, u, nest_l, nest_u, interp = c("linear", "spline", "step")) {

  interp <- tolower(interp)
  interp <- match.arg(interp)

  #  l <- c(0.75,1.75)
  #  u <- c(4.5,3.25)

  # comprobación
  if ( max(l) > min(u)) stop("Intersection of all intervals is the empty set!")

  #  nest_l <- c(1.5,1,2, 0.5)
  #  nest_u <- c(3.5,4,3, 5)
  ## Ordenar intervalos encajados
  ord_l <- sort.int(nest_l, index.return = TRUE, decreasing = TRUE)
  nest_l <- ord_l$x
  nest_u <- nest_u[ord_l$ix]


  # checked nested intervals
  if (any(diff(nest_l) >  0) || any(diff(nest_u) < 0 )) stop("Nested intervals provided are not nested!")
  M <- numeric(length(nest_l))

  # For ,,,,, :()
  for (i in seq_along(nest_l)){
    M[i] <- sum((l >= nest_l[i]) & u <= nest_u[i])
  }
  revalpha <- M / length(l)
  fx <- fzn(alpha = revalpha, l = rev(nest_l), u = rev(nest_u), interp = interp)
  return(fx)
}

