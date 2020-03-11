#' @title Fuzzy number check.
#'
#' @description Checks whether an R object is of class \code{fzn} or not.
#' @usage is.fzn(x)
#' @param x Any \bold{R} object.
#' @return Returns \code{TRUE} if its argument is a \code{fzn} number (that is, has "fzn"
#'   amongst its classes) and \code{FALSE} otherwise.
#'
#' @export


is.fzn <- function(x) {
  inherits(x, "fzn")
}
