% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/undesirable_basic_fzn.R
\name{undesirable_basic_fzn}
\alias{undesirable_basic_fzn}
\title{Undesirable inputs and outputs for basic DEA model.}
\usage{
undesirable_basic(datadea,
                  vtrans_i = NULL,
                  vtrans_o = NULL)
}
\arguments{
\item{datadea}{The data, including DMUs, inputs and outputs.}

\item{vtrans_i}{Numeric vector of translation for undesirable inputs. If \code{vtrans_i[i]} is
\code{NA}, then it applies the "max + 1" translation to the i-th undesirable input.
If \code{vtrans_i} is a constant, then it applies the same translation to all
undesirable inputs. If \code{vtrans_i} is \code{NULL}, then it applies the
"max + 1" translation to all undesirable inputs.}

\item{vtrans_o}{Numeric vector of translation for undesirable outputs, analogous to
\code{vtrans_i}, but applied to outputs.}
}
\value{
An object of class \code{deadata} or \code{deadata_fuzzy}.
}
\description{
This function transforms, according to Seiford and Zhu (2002),
a deadata or deadata_fuzzy class with undesirable inputs/outputs into a
deadata or deadata_fuzzy class with no undesirable inputs/outputs.
Onwards, it is recommended to use a DEA model with variable returns to scale (vrs).
}
\examples{
data("Hua_Bian_2007")
# The third output is an undesirable output.
data_example <- read_data(Hua_Bian_2007,
                          ni = 2,
                          no = 3,
                          ud_outputs = 3)
# rts must be "vrs" for undesirable inputs/outputs:
# Translation parameter is set to (max + 1)
result <- model_basic(data_example,
                      orientation = "oo",
                      rts = "vrs")

}
\references{
Seiford, L.M.; Zhu, J. (2002). “Modeling undesirable factors in efficiency evaluation”,
European Journal of Operational Research 142, 16-20.

Hua Z.; Bian Y. (2007). DEA with Undesirable Factors. In: Zhu J., Cook W.D. (eds)
Modeling Data Irregularities and Structural Complexities in Data Envelopment Analysis.
Springer, Boston, MA.
}
\author{
\strong{Vicente Coll-Serrano} (\email{vicente.coll@uv.es}).
\emph{Quantitative Methods for Measuring Culture (MC2). Applied Economics.}

\strong{Vicente Bolós} (\email{vicente.bolos@uv.es}).
\emph{Department of Business Mathematics}

\strong{Rafael Benítez} (\email{rafael.suarez@uv.es}).
\emph{Department of Business Mathematics}

University of Valencia (Spain)
}
