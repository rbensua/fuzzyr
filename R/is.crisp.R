#' @title Crisp check
#'
#' @description Checks whether an \code{fzn} object corresponds to a crisp number or not.
#'
#' @usage is.crisp(x)
#'
#' @param x Any \code{fzn} or \code{fzarray} object.
#'
#' @return Returns \code{TRUE} if \code{x} is a crisp number and \code{FALSE} otherwise.
#' If \code{x} is a fuzzy array, it returns a logical vector.
#'
#' @export


is.crisp <- function(x) {

  if (inherits(x, "fzarray")) {
    res <- sapply(X = x, FUN = function(z) { z$l[1] == z$u[1] })
  } else if (inherits(x, "fzn")) {
    res <- (x$l[1] == x$u[1])
  } else {
    stop("x should be of fzn class.")
  }

  return(res)

}
