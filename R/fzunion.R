#' @title fzunion
#'
#' @description This function adds alpha-cuts...
#'
#' @usage fzunion(fx1,
#'                fx2)
#' @param fx1 A fuzzy number given by a \code{fzn} object.
#' @param fx2 A fuzzy number given by a \code{fzn} object.
#'
#' @return A list with the two resulting fuzzy numbers.
#'
#' @examples
#'
#' @export

fzunion <- function(fx1,
                    fx2) {

  alpha_union <- sort(unique(c(fx1$alpha, fx2$alpha)))
  alpha_extra1 <- fx1$alpha[diff(fx1$alpha) == 0]
  alpha_extra2 <- fx2$alpha[diff(fx2$alpha) == 0]
  alpha_extra <- unique(c(alpha_extra1, alpha_extra2))
  alpha_union <- sort(c(alpha_union, alpha_extra))
  fx1_union <- fzn(alphacut(fx1, alpha_union))
  fx2_union <- fzn(alphacut(fx2, alpha_union))
  na1 <- length(alpha_extra1)
  if (na1 > 0) {
    for (i in 1:na1) {
      idxkk1 <- which(fx1$alpha == alpha_extra1[i])
      idxkk2 <- which(alpha_union == alpha_extra1[i])
      fx1_union$l[idxkk2] <- fx1$l[idxkk1]
      fx1_union$u[idxkk2] <- fx1$u[idxkk1]
    }
  }
  na2 <- length(alpha_extra2)
  if (na2 > 0) {
    for (i in 1:na2) {
      idxkk1 <- which(fx2$alpha == alpha_extra2[i])
      idxkk2 <- which(alpha_union == alpha_extra2[i])
      fx2_union$l[idxkk2] <- fx2$l[idxkk1]
      fx2_union$u[idxkk2] <- fx2$u[idxkk1]
    }
  }

  return(list(fx1 = fx1_union, fx2 = fx2_union))

}
