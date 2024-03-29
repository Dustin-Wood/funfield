% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepField.R
\name{prepField}
\alias{prepField}
\title{Prepare Functional Field}
\usage{
prepField(matLayout)
}
\arguments{
\item{matLayout}{Specified \emph{variable type} and \emph{variable name}}
}
\value{
Various objects that will be useful for creating fields from a prespecified matrix
}
\description{
This function will create the layout for the functional field
matrix, and help to maintain consistency between the field matrix and
graphical elements of the model.  It will also specify the type of each
variable in the graph - especially whether it should be represented as an \emph{action},
\emph{object}, \emph{variable}, \emph{choice point}, or \emph{appraisal}.

Variables in the functional field should be specified using a 'type_name' labeling convention.
Please see the \strong{**as yet unwritten help file**} for further details on preparing
a matrix layout.
}
\details{
Current (main) variable types:
    \code{d} = decision or 'do' nodes (indicate initiating some sort of action)
    \code{o} = object nodes - should only take values of 1 or 0
    \code{c} = choice point / chance nodes
    \code{x} = continuous variables
    \code{a} = appraisal nodes - indicate where a decision will be made through a maximization or threshold rule
}
