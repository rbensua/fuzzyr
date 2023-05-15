#' @title Extracts efficiencies from Kao-Liu result
#'
#' @description Extracts efficiencies as \code{fzarray} from Kao-Liu result.
#'
#' @usage eff_kaoliu_fzn(x)
#'
#' @param x Any \code{dea_fuzzy} object from any Kao-Liu model.
#'
#' @return Returns a \code{fzarray} object.
#'
#' @export


eff_kaoliu_fzn <- function(x) {

  nde <- length(x$dmu_eval)
  res <- vector(mode = "list", length = nde)
  names(res) <- names(x$dmu_eval)
  nalpha <- length(x$alpha)

  for (i in 1:nde) {
    l <- sapply(x$alphacut, FUN = function(xx) { xx$DMU$Worst[[i]]$efficiency })
    u <- sapply(x$alphacut, FUN = function(xx) { xx$DMU$Best[[i]]$efficiency })
    lu <- c(l, rev(u)) # numerical errors make l > u sometimes
    dlu <- diff(lu)
    if (any(dlu < 0)) {
      if (any(dlu < -1e-5)) {
        warning("Not ordered lower and upper limits. Reordering")
      }
      lu <- sort(lu)
      l <- lu[1:nalpha]
      u <- rev(lu[(nalpha + 1):length(lu)])
    }
    res[[i]] <- structure(list(alpha = x$alpha, l = l, u = u, interp = "approx"),
                          class = "fzn")
    #res[[i]] <- fzn(alpha = x$alpha, l = l, u = u)
  }

  res <- structure(res, class = c("fzn", "fzarray"))

  return(res)

}
