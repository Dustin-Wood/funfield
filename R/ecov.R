#' Effect Covariance Matrix
#' @description Creates the effect covariance matrix from the long-format
#' P*S*I effect file (usually estimated by function \code{ecov_long})
#' @param data effect covariance elements (should be generated from \code{ecov_long})
#' @param fX Variable containing the X expected outcome feature names
#' @param fY Variable containing the Y expected outcome feature names
#' @param dXdY Variable containing the product of X and Y expected effects
#' @param order specification of the order you would like the variables to be
#' ordered within the output (defaults to \code{NA} if none given)
#' @param byP Do a 'two-step aggregation' in which observations are averaged within-person first,
#' so that all participants observations are weighted equally (for instance: if one participant
#' rated 8 scenarios, and another only rated 1).
#' @return Expected effect covariance (and correlation) matrix; & associated sample sizes
#' @usage ecov(data,fX,fY,dXdY="cov")
#' @details This function utilizes functions from \code{plyr} and \code{tidyr}
#'
#' This will also place features in alphabetical order by default
#' - which is almost always suboptimal.
#' This can be altered by specifying an \code{order}, in the form of a
#' vector with the names of the variables in the preferred order.
#'
#' @export

ecov <- function(data,fX="fX",fY="fY",dXdY="dXdY", order=NA, byP = T) {
  if(byP == T){
  ecov1 <- plyr::ddply(data,.(p,fX,fY), function(x) colMeans(x["dXdY"]))
  ecov2 <- plyr::ddply(ecov1,.(fX,fY), function(x) colMeans(x["dXdY"]))
  }
  if(byP == F){
  ecov2 <- plyr::ddply(data,.(fX,fY), function(x) colSums(x["dXdY"]))
  }
  ecov3 <- tidyr::pivot_wider(ecov2,names_from = fY, values_from = dXdY)
  ecov <- ecov3[-1]
  ecov <- as.matrix(ecov)
  rownames(ecov) <- colnames(ecov)
  np <- length(table(data$p))
  npsi <- ecov["did_i","did_i"] #this is the total number of person-situation-action observations aggregated
  ecov <- ecov / ecov["did_i","did_i"] #this effectively divides by (sum of pvoi*svoi)
  r <- cov2cor(ecov)
  e <- list(ecov,r,npsi,np)
  #place in specified order, if given
  if(is.na(order[1]) == F) {
  e <- list(ecov[order,order],r[order,order],npsi)
  }
  names(e) <- c("ecov","r","npsi","np")
  return (e)
}
