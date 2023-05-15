#' @title Fuzzy array check
#'
#' @description Checks whether an R object is an \code{fzarray} or not.
#'
#' @usage is.fzarray(x)
#'
#' @param x Any \bold{R} object.
#'
#' @return Returns \code{TRUE} if its argument is an \code{fzarray} object (that is, it has "fzarray"
#'   amongst its classes) and \code{FALSE} otherwise.
#'
#' @export


is.fzarray <- function(x) {
  inherits(x, "fzarray")
}
