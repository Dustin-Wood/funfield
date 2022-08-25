#' Implied Covariance Matrix from a Field Model
#' @description This function will indicate the implied covariance matrix
#' from a set of forces.  (It seems like this should be done in lavaan through
#'  \code{lavInspect(fit, "implied")$cov}, but the covariances regularly seem
#'  to be over-accounted for by specified forces in a way that I have not
#'  figured out how to correct.)
#'
#'  (Note that this function was adapted from code previously buried inside function
#'  \code{fieldResults}.
#' @param fieldmat field matrix - usually created from lavaan::sem
#' @param covmat specify the observe correlations or covariances between the features
#' @return Will return \code{implied_cov}, with implied covariances from the specified
#' field model
#' @export

impliedCov <- function(fieldmat,covmat) {
  if(any(colnames(fieldmat) == colnames(covmat)) == F){
    print("Column order for field matrix and cov matrix does not seem to be the same. This means results may not be meaningful.")
  }
  implied_cov <- fieldmat * 0
  for(i in 1:nrow(fieldmat)){
    doi <- rep(0,ncol(fieldmat))
    doi[i] <-  sqrt(diag(covmat)[i] - diag(implied_cov)[i]) #activate node, but discount by the degree to which it has already been activated by other variables previously
    effect.i<-colSums(expOutcomes(doi,fieldmat))
    E.i<-as.matrix(effect.i)%*%t(as.matrix(effect.i)) #what are the total implied covariation from activating i?
    implied_cov <- implied_cov + E.i
  }
  return(implied_cov)
}
