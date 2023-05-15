#' @title Write a crisp number as a fuzzy number
#'
#' @description This function creates a fuzzy number of class \code{fzn} from a crisp
#' numeric value, using as membership function the characteristic function of the number.
#' Moreover, it can build an object of class \code{fzarray} from a numeric array.
#'
#' @usage fuzzyfy(x)
#'
#' @param x A numeric value (or array or matrix), or a fuzzy number of class
#' \code{fzn}, or a fuzzy array of class \code{fzarray}. If \code{x} is of class
#' \code{fzn} or \code{fzarray} then the function \code{fuzzyfy} returns \code{x}.
#'
#' @return A \code{fzn} object corresponding to the crisp number x.
#'
#' @examples
#' x <- fuzzyfy(3)
#' plotfzn(x)
#' xarray <- fuzzyfy(c(3, 4))
#'
#' @export

fuzzyfy <- function(x) {
  if (!inherits(x, "fzn")) {
    if (!is.numeric(x)){
      stop("x cannot be fuzzyfied.")
    }
    if (length(x) > 1) {
      res <- lapply(x, fuzzyfy)
      res <- structure(res, class = c("fzn", "fzarray"))
    } else {
      res <- fzn(alpha = c(0, 1), l = c(x, x), u = c(x, x))
    }
  } else {
    res <- x
  }
  return(res)
}

