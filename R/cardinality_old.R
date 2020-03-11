#' @title Calculate the cardinality of a fuzzy number by approximation
#'
#' @description This function estimates/approximates the value of the cardinality (area) of a fuzzy number \code{fzn},which is
#' a list containing the alpha-cuts of the membership function, the lower and upper limits of the
#' corresponding alpha-cuts, and the interpolation method. It is necessary to specify a resolution (h) of the estimationand the
#' calculus use the function \code{memebership}, which proporcione to us the value of the membership function at points
#' in the support of the fuzzy number separated a distance h.
#'
#'
#' @usage cardinality_old(fx,
#'            h)
#'
#' @param fx A fuzzy number defined by an object of \code{fzn} class.
#' @param h A double number that stablish the size of the grid. This value must be smaller that the support of the fuzzy number.
#' @return A double number that approximate the true value of cardinality.
#'
#' @examples
#' fx <- fzn(alpha = c(0, 1),
#'           l = c(3, 4),
#'           u = c(7, 6))
#' plot(fx)
#' cardinality_old(fx,0.1)
#'
#' @export
#'

cardinality_old <- function(fx,h) {

  a=(fx$u[1]-fx$l[1])
  x_0=c(seq(fx$l[1],fx$u[1],h))
  h*sum(membership(fx,x_0)$y)

}
