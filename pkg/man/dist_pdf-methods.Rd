% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/def_conexp.R, R/def_conlorm.R, R/def_conpl.R, R/def_disexp.R, R/def_dislnorm.R, R/def_displ.R, R/def_dispois.R
\docType{methods}
\name{dist_pdf}
\alias{dist_pdf}
\alias{dist_pdf,conexp-method}
\alias{dist_pdf,conlnorm-method}
\alias{dist_pdf,conpl-method}
\alias{dist_pdf,disexp-method}
\alias{dist_pdf,dislnorm-method}
\alias{dist_pdf,displ-method}
\alias{dist_pdf,dispois-method}
\title{The probability density function (pdf)}
\usage{
dist_pdf(m, q = NULL, log = FALSE)

\S4method{dist_pdf}{conexp}(m, q = NULL, log = FALSE)

\S4method{dist_pdf}{conlnorm}(m, q = NULL, log = FALSE)

\S4method{dist_pdf}{conpl}(m, q = NULL, log = FALSE)

\S4method{dist_pdf}{disexp}(m, q = NULL, log = FALSE)

\S4method{dist_pdf}{dislnorm}(m, q = NULL, log = FALSE)

\S4method{dist_pdf}{displ}(m, q = NULL, log = FALSE)

\S4method{dist_pdf}{dispois}(m, q = NULL, log = FALSE)
}
\arguments{
\item{m}{The distribution reference object.}

\item{q}{a vector values where the function will be evaluated.
If \code{q} is \code{NULL} (default), then the data value will be used.}

\item{log}{default \code{FALSE}. If \code{TRUE}, probabilities are given as log(p).}
}
\value{
The probability density (or mass) function
}
\description{
This is generic function for distribution objects.
This function calculates the probability density function (pdf)
for the current parameters and xmin value.
}
\note{
This method does *not* alter the internal state of
the distribubtion objects.
}
\examples{
##########################################
#Create distribution object              #
##########################################
m = displ$new()
m$setXmin(7); m$setPars(2)

##########################################
#Calculate the pdf at particular values  #
##########################################
dist_pdf(m, 7:10)
}
\seealso{
\code{\link{dist_cdf}}, \code{\link{dist_ll}}
and \code{\link{dist_rand}}
}

