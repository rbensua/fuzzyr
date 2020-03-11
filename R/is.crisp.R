#' @title Crisp check.
#'
#' @description Checks whether an \code{fzn} object correspond to a crisp number or not.
#' @usage is.crisp(fx)
#' @param fx Any \code{fzn} object.
#' @return Returns \code{TRUE} if \code{fx} is a crisp number and \code{FALSE} otherwise.
#'
#' @export


is.crisp <- function(fx) {
  if (class(fx) != "fzn") {
    stop("fx should be of fzn class!")
  }
  res <- ((fx$l[1] - fx$u[1]) == 0)
  return(res)
}
