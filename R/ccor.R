#' Scale-Centered (or 'Cohen') Correlations
#'
#' @description
#' Estimate correlations between variables where the scale midpoint serves as the reflection point for estimating correlations.
#'
#' These are equivalent to Cohen's (1969) rc correlations, which are invariant across potentially arbitrary item- or variable-reversals.
#' @param x matrix to be used for correlations
#' @param smin Scale minimum
#' @param smax Scale maximum
#' @param margin Specify whether correlations are of rows(1) or columns(2) (default is columns)
#' @return The scale-centered correlations of matrix
#' @usage
#' ccor(x, smin, smax)
#' ccor(x, smin = 1, smax = 6) #e.g., standard 1-5 Likert scale
#'
#' @export

ccor <- function(x, smin, smax, margin=2) {
  cx <- (x - (smax + smin)/2)/(smax-smin)*2
  zcx <- apply(cx, margin, function(x) (x/sqrt((sum(x^2)/(length(x))))))
  ccor <- t(zcx) %*% zcx / nrow(zcx) #this last bit can be used to calculate matching pretty generally
  return(ccor)
}

