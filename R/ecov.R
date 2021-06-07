#' Effect Covariance Matrix
#' @description Creates the effect covariance matrix from the long-format P*S effect file (usually estimated by function \code{ecov_big})
#' @param data Input matrix
#' @param fA Variable containing the X expected outcome feature names
#' @param fB Variable containing the Y expected outcome feature names
#' @param dAdB Variable containing the product of A and B expected effects (note: should be in quotation marks, as in "r" or "cov")
#' @return Expected effect covariance matrix
#' @usage ecov(data,fA,fB,dAdB="cov")
#' @details Note: this function depends on having loaded \code{reshape2} package
#'
#' @export

ecov <- function(data,fA=fA,fB=fB,dAdB=xAB) {

  ecov <- dcast(data, fA ~ fB, value.var = dAdB, sum, na.rm=TRUE)
  ecov <- ecov[-1]
  ecov <- ecov / ecov[1,1] #this effectively divides by (sum of pvoi*svoi)
  rownames(ecov) <- colnames(ecov)
  return (ecov)
}
