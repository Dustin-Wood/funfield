% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sweighteddata.R
\name{sweighteddata}
\alias{sweighteddata}
\title{Situation Weighted Data}
\usage{
sweighteddata(ESJT, voiset, voi)
}
\arguments{
\item{ESJT}{ESJT raw data; must be headed with three variables: \code{p, s, i} -
for 'Person', 'Situation', '[Initiated] action', respectively}

\item{voiset}{Set containing the variable of interest}

\item{voi}{Name of the specific variable within the voiset you wish to weight by}

\item{bySet}{columns you wish to match by (defaults to 'situation & action')}
}
\value{
The expected effects of the action, weighted by the action's situation-diagnosticity
}
\description{
In brief: weight situations by a variable of interest.  Mainly of value for eventually combining multiple ratings to a single score.  For instance: combining diverse ESJT ratings to a single score weighted by the situation's assertiveness-diagnosticity.
}
