#' @title Write a crisp number as a fuzzy number.
#'
#' @description This function creates a fuzzy number of class \code{fzn} from a crisp
#' numeric value, using as membership function the characteristic function of the number.
#'
#' @usage fuzzyfy(x)
#' @param x A numeric value.
#' @return A \code{fzn} object corresponding to the crisp number \code{x}.
#'
#' @examples
#'
#' @export

fuzzyfy <- function(x) {
  if (!is.fzn(x)) {
    if (!is.numeric(x)){
      stop("x should be a single numeric value.")
    }
    if (length(x) > 1) {
      res <- lapply(x, fuzzyfy)
      res <- structure(res, class = "fzarray")
    } else {
      res <- fzn(alpha = c(0, 1), l = c(x, x), u = c(x, x))
    }
  } else {
    res <- x
  }
  return(res)
}

