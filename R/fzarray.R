#' @title Fuzzy array
#'
#' @description This function constructs a fuzzy array of class \code{fzarray} (and \code{fzn}), which is a list
#' of fuzzy numbers of class \code{fzn}. It can be used for adding elements to a fuzzy array,
#' or concatenate several fuzzy arrays.
#'
#' @usage fzarray(...)
#'
#' @param ... Fuzzy numbers of class \code{fzn} or fuzzy arrays of class \code{fzarray}.
#' It can be a list of fuzzy numbers of class \code{fzn}.
#'
#' @return An object of class \code{fzarray}.
#'
#' @examples
#' x <- fzn(alpha = c(0, 0.1, 0.5, 0.8, 1),
#'          l = c(1, 3, 3, 4.5, 5),
#'          u = c(12, 10, 9, 6.5, 6))
#' c <- fuzzyfy(1)
#' # Construct a fuzzy array with x and c
#' fza1 <- fzarray(x, c)
#' # Construct a fuzzy array from crisp values using function fuzzyfy
#' fza2 <- fuzzyfy(c(1, 2, 3))
#' # Construct a fuzzy array whose elements are x, c, 1, 2, 3 and x again
#' fza3 <- fzarray(fza1, fza2, x)
#'
#' @export

fzarray <-
  function(...) {

    args <- list(...)
    nargs <- length(args)

    n <- rep(0, nargs)
    for (i in 1:nargs) {
      if (all(class(args[[i]]) == "fzn")) {
        n[i] <- 1
      } else if (is.list(args[[i]]) && all(sapply(args[[i]], function(x) { all(class(x) == "fzn") }))) {
        n[i] <- length(args[[i]])
      } else {
        stop("Arguments must be fzn, fzarray or a list of fzn.")
      }
    }

    nres <- sum(n)
    if (nres == 1) {

      res <- args[[1]]

    } else {

      res <- vector(mode = "list", length = nres)
      k <- 1
      for (i in 1:nargs) {
        if (n[i] == 1){
          res[[k]] <- args[[i]]
          k <- k + 1
        } else {
          for (j in 1:n[i]) {
            res[[k]] <- args[[i]][[j]]
            k <- k + 1
          }
        }

      }
      res <- structure(res, class = c("fzn", "fzarray"))

    }

    return(res)

  }
