% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modXY.R
\name{modXY}
\alias{modXY}
\title{Run Simple Test of how X->Y (or bYX) relationship is moderated by Z for Outcomes X}
\usage{
modXY(data, Z, xSet, Y)
}
\arguments{
\item{data}{dataset in a PSI long format, preferably using within-situation deviated scores}

\item{Z}{moderator variable (it will be standardized)}

\item{xSet}{location of the set of variables that will be explored as moderated by Z (one at a time)}

\item{Y}{the dependent variable (often "likelihood" - but not necessarily)}
}
\value{
Will return the main effects of the outcomes, and their z-moderated effect
}
\description{
This will run and extract single-mediator moderated-mediation models
('Model 1' in Wood, Harms, & Cho, 2023) for all measured potential mediators of the
[Action->Likelihood] association.
** continue editing here...**
}
\details{
This adjusts for degrees of freedom by using 'p' as a clustering variable in lavaan
}
