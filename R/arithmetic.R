#' @title Sum of fuzzy numbers
#'
#' @description Sums two fuzzy numbers of class \code{fzn} or one fuzzy number to a crisp number.
#' Also works for fuzzy arrays of class \code{fzarray}.
#'
#' @usage fx1 + fx2
#'
#' @param fx1 First fuzzy (or crisp) number, or fuzzy array.
#' @param fx2 Second fuzzy (or crisp) number, or fuzzy array.
#'
#' @return The fuzzy sum (fx1 + fx2) as an object of class \code{fzn} or \code{fzarray}.
#'
#' @examples
#' x1 <- fzn(alpha = c(0, 0.1, 0.5, 0.8, 1),
#'           l = c(1, 3, 3, 4.5, 5),
#'           u = c(12, 10, 9, 6.5, 6))
#' x2 <- fzn(alpha = c(0, 0.25, 1),
#'           l = c(1, 3, 5),
#'           u = c(12, 10, 6))
#' par(mfrow = c(2, 2))
#' plotfzn(x1, main = "x1")
#' plotfzn(x2, main = "x2")
#' plotfzn(x1 + x2, main = "x1 + x2")
#'
#' @aliases +
#'
#' @method `+` fzn
#'
#' @export

`+.fzn` <- function(fx1, fx2) {

  fx1 <- fuzzyfy(fx1)
  fx2 <- fuzzyfy(fx2)

  a1 <- inherits(fx1, "fzarray")
  a2 <- inherits(fx2, "fzarray")

  if (!a1 && !a2) {

    if (!identical(fx1$alpha, fx2$alpha)) {
      alpha <- sort(unique(c(fx1$alpha, fx2$alpha)))
      fx1 <- fzn(alphacut(fx1, alpha), interp = fx1$interp)
      fx2 <- fzn(alphacut(fx2, alpha), interp = fx2$interp)
    } else {
      alpha <- fx1$alpha
    }
    if ((fx1$interp == "spline") || (fx2$interp == "spline")) {
      interp <- "spline"
    } else if ((fx1$interp == "approx") || (fx2$interp == "approx")) {
      interp <- "approx"
    } else if ((fx1$interp == "step") || (fx2$interp == "step")) {
      interp <- "step"
    } else {
      interp <- "step2"
    }
    res <- fzn(alpha = alpha,
               l = fx1$l + fx2$l,
               u = fx1$u + fx2$u,
               interp = interp)

  } else if (a1 && !a2) {

    fx2array <- structure(rep(list(fx2), length(fx1)), class = c("fzn", "fzarray"))
    res <- fx1 + fx2array

  } else if (!a1 && a2) {

    fx1array <- structure(rep(list(fx1), length(fx2)), class = c("fzn", "fzarray"))
    res <- fx1array + fx2

  } else { # a1 && a2

    if(length(fx1) != length(fx2)) {
      stop("Fuzzy arrays should have the same length.")
    }
    res <- structure(lapply(seq_along(fx1), function(i) { fx1[[i]] + fx2[[i]] } ),
                     class = c("fzn", "fzarray"))

  }

  return(res)

}

#' @title Substraction of fuzzy numbers
#'
#' @description Substracts two fuzzy numbers of class \code{fzn} or one fuzzy number to a crisp number (or viceversa).
#' Also works for fuzzy arrays of class \code{fzarray}.
#'
#' @usage fx1 - fx2
#'
#' @param fx1 First fuzzy (or crisp) number, or fuzzy array.
#' @param fx2 Second fuzzy (or crisp) number, or fuzzy array.
#'
#' @return The fuzzy substraction (fx1 - fx2) as an object of class \code{fzn} or \code{fzarray}.
#'
#' @examples
#' x1 <- fzn(alpha = c(0, 0.1, 0.5, 0.8, 1),
#'           l = c(1, 3, 3, 4.5, 5),
#'           u = c(12, 10, 9, 6.5, 6))
#' x2 <- fzn(alpha = c(0, 0.25, 1),
#'           l = c(1, 3, 5),
#'           u = c(12, 10, 6))
#' par(mfrow = c(2, 2))
#' plotfzn(x1, main = "x1")
#' plotfzn(x2, main = "x2")
#' plotfzn(x1 - x2, main = "x1 - x2")
#'
#' @aliases -
#'
#' @method `-` fzn
#'
#' @export

`-.fzn` <- function(fx1 = NULL, fx2 = NULL) {

  if(is.null(fx2)) {
    fx2 <- fx1
    fx1 <- 0
  }

  fx1 <- fuzzyfy(fx1)
  fx2 <- fuzzyfy(fx2)

  a1 <- inherits(fx1, "fzarray")
  a2 <- inherits(fx2, "fzarray")

  if (!a1 && !a2) {

    if (!identical(fx1$alpha, fx2$alpha)) {
      alpha <- sort(unique(c(fx1$alpha, fx2$alpha)))
      fx1 <- fzn(alphacut(fx1, alpha), interp = fx1$interp)
      fx2 <- fzn(alphacut(fx2, alpha), interp = fx2$interp)
    } else {
      alpha <- fx1$alpha
    }
    if ((fx1$interp == "spline") || (fx2$interp == "spline")) {
      interp <- "spline"
    } else if ((fx1$interp == "approx") || (fx2$interp == "approx")) {
      interp <- "approx"
    } else if ((fx1$interp == "step") || (fx2$interp == "step")) {
      interp <- "step"
    } else {
      interp <- "step2"
    }
    res <- (fzn(alpha = alpha,
                l = fx1$l - fx2$u,
                u = fx1$u - fx2$l,
                interp = interp))

  } else if (a1 && !a2) {

    fx2array <- structure(rep(list(fx2), length(fx1)), class = c("fzn", "fzarray"))
    res <- fx1 - fx2array

  } else if (!a1 && a2) {

    fx1array <- structure(rep(list(fx1), length(fx2)), class = c("fzn", "fzarray"))
    res <- fx1array - fx2

  } else { # a1 && a2

    if(length(fx1) != length(fx2)) {
      stop("Fuzzy arrays should have the same length.")
    }
    res <- structure(lapply(seq_along(fx1), function(i) { fx1[[i]] - fx2[[i]] } ),
                     class = c("fzn", "fzarray"))

  }

  return(res)

}

#' @title Product of fuzzy numbers
#'
#' @description Multiplies two fuzzy numbers of class \code{fzn} or one fuzzy number to a crisp number.
#' Also works for fuzzy arrays of class \code{fzarray}.
#'
#' @usage fx1 * fx2
#'
#' @param fx1 First fuzzy (or crisp) number, or fuzzy array.
#' @param fx2 Second fuzzy (or crisp) number, or fuzzy array.
#'
#' @return The fuzzy product (fx1 * fx2) as an object of class \code{fzn} or \code{fzarray}.
#'
#' @examples
#' x1 <- fzn(alpha = c(0, 0.1, 0.5, 0.8, 1),
#'           l = c(1, 3, 3, 4.5, 5),
#'           u = c(12, 10, 9, 6.5, 6))
#' x2 <- fzn(alpha = c(0, 0.25, 1),
#'           l = c(1, 3, 5),
#'           u = c(12, 10, 6))
#' par(mfrow = c(2, 2))
#' plotfzn(x1, main = "x1")
#' plotfzn(x2, main = "x2")
#' plotfzn(x1 * x2, main = "x1 * x2")
#'
#' @aliases *
#'
#' @method `*` fzn
#'
#' @export

`*.fzn` <- function(fx1, fx2) {

  fx1 <- fuzzyfy(fx1)
  fx2 <- fuzzyfy(fx2)

  a1 <- inherits(fx1, "fzarray")
  a2 <- inherits(fx2, "fzarray")

  if (!a1 && !a2) {

    if (!identical(fx1$alpha, fx2$alpha)) {
      alpha <- sort(unique(c(fx1$alpha, fx2$alpha)))
      fx1 <- fzn(alphacut(fx1, alpha), interp = fx1$interp)
      fx2 <- fzn(alphacut(fx2, alpha), interp = fx2$interp)
    } else {
      alpha <- fx1$alpha
    }
    A <- rbind(fx1$l * fx2$l,
               fx1$l * fx2$u,
               fx1$u * fx2$l,
               fx1$u * fx2$u)
    res <- apply(A, MARGIN = 2, FUN = function(r) c(min(r), max(r)))
    if ((fx1$interp == "spline") || (fx2$interp == "spline")) {
      interp <- "spline"
    } else if ((fx1$interp == "approx") || (fx2$interp == "approx")) {
      interp <- "approx"
    }  else if ((fx1$interp == "step") || (fx2$interp == "step")) {
      interp <- "step"
    } else {
      interp <- "step2"
    }
    #if (is.crisp(fx1)) {
    #  interp <- fx2$interp
    #} else if (is.crisp(fx2)) {
    #  interp <- fx1$interp
    #} else if (((fx1$interp == "step") || (fx1$interp == "step2")) &&
    #           ((fx2$interp == "step") || (fx2$interp == "step2"))) {
    #  if ((fx1$interp == "step") || (fx2$interp == "step")) {
    #    interp <- "step"
    #  } else {
    #    interp <- "step2"
    #  }
    #} else {
    #  interp <- "spline"
    #}
    res <- (fzn(rbind(alpha, res),
                interp = interp))

  } else if (a1 && !a2) {

    fx2array <- structure(rep(list(fx2), length(fx1)), class = c("fzn", "fzarray"))
    res <- fx1 * fx2array

  } else if (!a1 && a2) {

    fx1array <- structure(rep(list(fx1), length(fx2)), class = c("fzn", "fzarray"))
    res <- fx1array * fx2

  } else { # a1 && a2

    if(length(fx1) != length(fx2)) {
      stop("Fuzzy arrays should have the same length.")
    }
    res <- structure(lapply(seq_along(fx1), function(i) { fx1[[i]] * fx2[[i]] } ),
                     class = c("fzn", "fzarray"))

  }

  return(res)

}

#' @title Division of fuzzy numbers
#'
#' @description Divides two fuzzy numbers of class \code{fzn} or one fuzzy number to a crisp number.
#' Also works for fuzzy arrays of class \code{fzarray}.
#'
#' @usage fx1 / fx2
#'
#' @param fx1 First fuzzy (or crisp) number, or fuzzy array.
#' @param fx2 Second fuzzy (or crisp) number, or fuzzy array (zero cannot be in
#' any of the alpha cuts).
#'
#' @return The fuzzy quotient (fx1 / fx2) as an object of class \code{fzn} or \code{fzarray}.
#'
#' @examples
#' x1 <- fzn(alpha = c(0, 0.1, 0.5, 0.8, 1),
#'           l = c(1, 3, 3, 4.5, 5),
#'           u = c(12, 10, 9, 6.5, 6))
#' x2 <- fzn(alpha = c(0, 0.25, 1),
#'           l = c(1, 3, 5),
#'           u = c(12, 10, 6))
#' par(mfrow = c(2, 2))
#' plotfzn(x1, main = "x1")
#' plotfzn(x2, main = "x2")
#' plotfzn(x1 / x2, main = "x1 / x2")
#'
#' @aliases /
#'
#' @method `/` fzn
#'
#' @export

`/.fzn` <- function(fx1, fx2) {

  fx1 <- fuzzyfy(fx1)
  fx2 <- fuzzyfy(fx2)

  a1 <- inherits(fx1, "fzarray")
  a2 <- inherits(fx2, "fzarray")

  if (!a1 && !a2) {

    if (fx2$l[1] * fx2$u[1] <= 0) {
      stop("Dividing by 0!")
    } else {
      if (!identical(fx1$alpha, fx2$alpha)) {
        alpha <- sort(unique(c(fx1$alpha, fx2$alpha)))
        fx1 <- fzn(alphacut(fx1, alpha), interp = fx1$interp)
        fx2 <- fzn(alphacut(fx2, alpha), interp = fx2$interp)
      } else {
        alpha <- fx1$alpha
      }
      A <- rbind(fx1$l / fx2$l,
                 fx1$l / fx2$u,
                 fx1$u / fx2$l,
                 fx1$u / fx2$u)
      res <- apply(A, MARGIN = 2, FUN = function(r) c(min(r), max(r)))
      if ((fx1$interp == "spline") || (fx2$interp == "spline")) {
        interp <- "spline"
      } else if ((fx1$interp == "approx") || (fx2$interp == "approx")) {
        interp <- "approx"
      }  else if ((fx1$interp == "step") || (fx2$interp == "step")) {
        interp <- "step"
      } else {
        interp <- "step2"
      }
      #if (is.crisp(fx1)) {
      #  interp <- fx2$interp
      #} else if (is.crisp(fx2)) {
      #  interp <- fx1$interp
      #} else if (((fx1$interp == "step") || (fx1$interp == "step2")) &&
      #           ((fx2$interp == "step") || (fx2$interp == "step2"))) {
      #  if ((fx1$interp == "step") || (fx2$interp == "step")) {
      #    interp <- "step"
      #  } else {
      #    interp <- "step2"
      #  }
      #} else {
      #  interp <- "spline"
      #}
      res <- fzn(rbind(alpha, res),
                 interp = interp)
    }

  } else if (a1 && !a2) {

    fx2array <- structure(rep(list(fx2), length(fx1)), class = c("fzn", "fzarray"))
    res <- fx1 / fx2array

  } else if (!a1 && a2) {

    fx1array <- structure(rep(list(fx1), length(fx2)), class = c("fzn", "fzarray"))
    res <- fx1array / fx2

  } else { # a1 && a2

    if(length(fx1) != length(fx2)) {
      stop("Fuzzy arrays should have the same length.")
    }
    res <- structure(lapply(seq_along(fx1), function(i) { fx1[[i]] / fx2[[i]] } ),
                     class = c("fzn", "fzarray"))

  }

  return(res)

}
