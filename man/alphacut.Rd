% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alphacut.R
\name{alphacut}
\alias{alphacut}
\title{Alpha-cuts of a fuzzy number}
\usage{
alphacut(fx,
                alpha)
}
\arguments{
\item{fx}{A fuzzy number given by a \code{fzn} object.}

\item{alpha}{A numeric vector of numbers between 0 and 1 corresponding to the desired
possibility values.}
}
\value{
If \code{length(alpha)} == 1 it returns a vector of length 2 containing the range of
the alpha-cut (lower value, upper value). Otherwise it returns a matrix of size 3 x \code{length(alpha)}.
The first row will be the alphas considered and the rows 2 and 3 are the lower and upper values of
the corresponding alpha-cut, respectively.
}
\description{
This function gives the alpha-cuts of a fuzzy number given by an object
\code{fx} of class \code{fzn}, using the interpolation method given by the field
"interp" of \code{x}.
}
\examples{
fx <- fzn(alpha = c(0, 0.1, 0.5, 0.8, 1),
          l = c(1, 3, 3, 4.5, 5),
          u = c(12, 10, 9, 6.5, 6),
          interp = "spline")
# Computing some alpha-cuts using spline interpolation (since fx$interp == "spline")
alphacut(fx, alpha = seq(from = 0, to = 1, by = 0.1))
# Adding new alpha-cuts to fx using spline interpolation
fx <- fzn(alphacut(fx, alpha = seq(from = 0, to = 1, by = 0.1)))

}
