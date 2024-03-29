% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impliedCov.R
\name{impliedCov}
\alias{impliedCov}
\title{Implied Covariance Matrix from a Field Model}
\usage{
impliedCov(fieldmat, covmat)
}
\arguments{
\item{fieldmat}{field matrix - usually created from lavaan::sem}

\item{covmat}{specify the observe correlations or covariances between the features}
}
\value{
Will return \code{implied_cov}, with implied covariances from the specified
field model
}
\description{
This function will indicate the implied covariance matrix
from a set of forces.  (It seems like this should be done in lavaan through
 \code{lavInspect(fit, "implied")$cov}, but the covariances regularly seem
 to be over-accounted for by specified forces in a way that I have not
 figured out how to correct.)

 (Note that this function was adapted from code previously buried inside function
 \code{fieldResults}.
}
