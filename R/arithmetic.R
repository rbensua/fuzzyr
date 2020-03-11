#' @title Sum of fuzzy numbers
#'
#' @description Sum two fuzzy numbers of class \code{fzn} or one fuzzy number to a crisp
#' number.
#'
#' @usage fx1 + fx2
#' @param fx1 First fuzzy (or crisp) number.
#' @param fx2 Second fuzzy (or crisp) number.
#' @return The fuzzy sum (fx1 + fx2) as an object of class \code{fzn}.
#'
#' @examples
#'
#' @method `+` fzn
#' @export

`+.fzn` <- function(fx1, fx2)
{
  fx1 <- fuzzyfy(fx1)
  fx2 <- fuzzyfy(fx2)

  fx_list <- fzunion(fx1, fx2)
  fx1_union <- fx_list$fx1
  fx2_union <- fx_list$fx2

  if ((fx1$interp == "spline") || (fx2$interp == "spline")) {
    interp <- "spline"
  } else if ((fx1$interp == "linear") || (fx2$interp == "linear")) {
    interp <- "linear"
  } else {
    interp <- "step"
  }
  fz_sum <- fzn(alpha = fx1_union$alpha,
                l = fx1_union$l + fx2_union$l,
                u = fx1_union$u + fx2_union$u,
                interp = interp)
  return(fz_sum)
}

#' @title Substraction of fuzzy numbers
#'
#' @description Substracts two fuzzy numbers of class \code{fzn} or one fuzzy number to a
#' crisp number (or viceversa).
#'
#' @usage fx1 - fx2
#' @param fx1 First fuzzy (or crisp) number.
#' @param fx2 Second fuzzy (or crisp) number.
#' @return The fuzzy substraction (fx1 - fx2) as an object of class \code{fzn}.
#'
#' @examples
#'
#' @method `-` fzn
#' @export

`-.fzn` <- function(fx1 = NULL, fx2 = NULL)
{
  if(is.null(fx2)) {
    fx2 <- fx1
    fx1 <- 0
  }
  fx1 <- fuzzyfy(fx1)
  fx2 <- fuzzyfy(fx2)
  fx_list <- fzunion(fx1, fx2)
  fx1_union <- fx_list$fx1
  fx2_union <- fx_list$fx2
  lower_limits <- fx1_union$l - fx2_union$u
  upper_limits <- fx1_union$u - fx2_union$l
  if ((fx1$interp == "spline") || (fx2$interp == "spline")) {
    interp <- "spline"
  } else if ((fx1$interp == "linear") || (fx2$interp == "linear")) {
    interp <- "linear"
  } else {
    interp <- "step"
  }
  fz_subs <- fzn(alpha = fx1_union$alpha,
                 l = lower_limits,
                 u = upper_limits,
                 interp = interp)
  return(fz_subs)
}

#' @title Product of fuzzy numbers
#'
#' @description Multiplies two fuzzy numbers of class \code{fzn} or one fuzzy number to a
#' crisp number.
#'
#' @usage fx1 * fx2
#' @param fx1 First fuzzy (or crisp) number.
#' @param fx2 Second fuzzy (or crisp) number.
#' @return The fuzzy product (fx1 * fx2) as an object of class \code{fzn}.
#'
#' @examples
#'
#' @method `*` fzn
#' @export

`*.fzn` <- function(fx1, fx2)
{
  fx1 <- fuzzyfy(fx1)
  fx2 <- fuzzyfy(fx2)

  fx_list <- fzunion(fx1, fx2)
  fx1_union <- fx_list$fx1
  fx2_union <- fx_list$fx2
  if ((length(fx1_union$alpha) == 2) && (!is.crisp(fx1)) && (!is.crisp(fx2))) {
    fx1_union <- fzn(alphacut(fx1_union, alpha = c(0, 0.5, 1)))
    fx2_union <- fzn(alphacut(fx2_union, alpha = c(0, 0.5, 1)))
  }

  A <- rbind(fx1_union$l * fx2_union$l,
             fx1_union$l * fx2_union$u,
             fx1_union$u * fx2_union$l,
             fx1_union$u * fx2_union$u)
  res <- apply(A, MARGIN = 2, FUN = function(r) c(min(r), max(r)))
  if (is.crisp(fx1)) {
    interp <- fx2$interp
  } else if (is.crisp(fx2)) {
    interp <- fx1$interp
  } else {
    if ((fx1$interp == "step") && (fx2$interp == "step")){
      interp <- "step"
    } else {
      interp <- "spline"
    }
  }
  fz_prod <- fzn(rbind(fx1_union$alpha, res), interp = interp)
  return(fz_prod)
}

#' @title Product of fuzzy numbers vectors
#'
#' @description Multiplies two arrays of fuzzy numbers of class \code{fzn} element-wise.
#'
#' @usage fa1 * fa2
#' @param fa1 First fuzzy vector.
#' @param fa2 Second fuzzy vector.
#' @return The fuzzy product (fa1 * fa2) as an object of class \code{fzarray}.
#'
#' @examples
#'
# @method `*` fzarray
# @export

`*.fzarray` <- function(fa1, fa2)
{
  # multiply a fuzzy array by a number:
  if(class(fa1) != "fzarray" & ((is.numeric(fa1) & length(fa1) == 1) | is.fzn(fa1))) {
    fa1 <- structure(rep(list(fuzzyfy(fa1)), length(fa2)), class = "fzarray")
  }
  if(class(fa2) != "fzarray" & ((is.numeric(fa2) & length(fa2) == 1) | is.fzn(fa2))){
    fa2 <- structure(rep(list(fuzzyfy(fa2)), length(fa1)), class = "fzarray")
  }
  if(length(fa1) != length(fa2)) {
    stop("vectors should have the same length!")
  }
  res <- lapply(seq_along(fa1), function(i) { fa1[[i]] * fa2[[i]] } )
  fz_prod <- structure(res, class = "fzarray")
}

#' @title Division of fuzzy numbers
#'
#' @description Divides two fuzzy numbers of class \code{fzn} or one fuzzy number to a
#' crisp number.
#'
#' @usage fx1 / fx2
#' @param fx1 First fuzzy (or crisp) number.
#' @param fx2 Second fuzzy (or crisp) number (zero cannot be in any of the alpha-cuts).
#' @return The fuzzy quotient (fx1 / fx2) as an object of class \code{fzn}.
#'
#' @examples
#'
#' @method `/` fzn
#' @export

`/.fzn` <- function(fx1, fx2)
{
  fx1 <- fuzzyfy(fx1)
  fx2 <- fuzzyfy(fx2)
  if (fx2$l[1] * fx2$u[1] <= 0) {
    stop("Dividing by 0!")
  } else {
    fx_list <- fzunion(fx1, fx2)
    fx1_union <- fx_list$fx1
    fx2_union <- fx_list$fx2
    if ((length(fx1_union$alpha) == 2) && (!is.crisp(fx1)) && (!is.crisp(fx2))) {
      fx1_union <- fzn(alphacut(fx1_union, alpha = c(0, 0.5, 1)))
      fx2_union <- fzn(alphacut(fx2_union, alpha = c(0, 0.5, 1)))
    }
    A <- rbind(fx1_union$l / fx2_union$l,
               fx1_union$l / fx2_union$u,
               fx1_union$u / fx2_union$l,
               fx1_union$u / fx2_union$u)
    res <- apply(A, MARGIN = 2, FUN = function(r) c(min(r), max(r)))
    if (is.crisp(fx1)) {
      interp <- fx2$interp
    } else if (is.crisp(fx2)) {
      interp <- fx1$interp
    } else {
      if ((fx1$interp == "step") && (fx2$interp == "step")){
        interp <- "step"
      } else {
        interp <- "spline"
      }
    }
    fx_quot <- fzn(rbind(fx1_union$alpha, res), interp = interp)
    return(fx_quot)
  }
}
