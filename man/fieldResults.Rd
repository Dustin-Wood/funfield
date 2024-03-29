% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fieldResults.R
\name{fieldResults}
\alias{fieldResults}
\title{Main Functional Field Results}
\usage{
fieldResults(fModel, covMat, sampleN = 4)
}
\arguments{
\item{fModel}{field model, specifying which paths are freely estimated vs.
constrained to zero}

\item{covMat}{covariance matrix to be fit to a field model (can be either a correlation
or covariance matrix)}

\item{sampleN}{sample size associated with the covariance matrix (defaults to '4', which
will basically make sure everything is as \code{ns} as possible)}
}
\value{
list with estimated:
\item{eMatrix}{expectancy matrix}
\item{fMatrix}{field matrix}
\item{uMatrix}{'uniqueness' matrix (i.e., expectancy matrix not predicted by field matrix)}
\item{uMatrix2}{'uniqueness' matrix2 (a crude work-around for when lavaan does not want to estimate correctly; see https://github.com/yrosseel/lavaan/issues/44}
\item{parTable}{parameter estimates, with associated significance values}
}
\description{
Fit values of paths for a functional field, given an associated
\emph{expectancy} matrix.
}
\details{
Note that the \code{uMatrix2} is a servicable means of estimating
the unpredicted residuals in some of the models where lavaan does not want
to provide the path-analysis implied residuals. But it should only work when
the model is a DAG that has been placed to have rows in 'causal order'.
    There is also a problem with how \code{uMatrix2} is estimated (the final covariance
    is always misestimated) which I assume
    involves a bad interaction with how \code{expOutcomes} function closes out
}
