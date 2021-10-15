#' Expected-Outcome Covariance Matrices for Low, Med, High Subgroups on a Variable of Interest
#' @description Estimate how expected outcomes covary for 'low', 'medium' and 'high'
#' levels of a 'person variable of interest' (\code{pvoi}).
#'
#' Note that this creates 'low' 'medium' and 'high' subgroups through
#' the strategy employed by the \code{zgroups} function, which attempts
#' to approximate how observations are weighted when calculating a
#' correlation coefficient.
#'
#' @param PSI the Person-Situation-Action expected effects relevant to the analysis
#' @param pVars file containing person variables
#' @param voi (person-)variable of interest (must provide in quotations)
#' @param p the variable containing the 'person' identifier (defaults to \code{"p"})
#' @param cuts detail how groups will be created; default to \code{z} to
#' indicate \code{zgroups} will be formed
#' @return Effect covariance matrices (and sample sizes) for \code{zL, zM, zH}
#' (low, medium, and high z-scores on the person variable of interest)
#' subgroups
#' @details
#'
#' @export

ecov.lmh <- function(PSI, pVars, voi, p = "p", cuts = "z", per = F){
  ##create a small file to merge variable of interest with PSI file
  pVOI<-data.frame(pVars[p],pVars[voi])
  colnames(pVOI)<-c("p","voi")

  #create specific covariance matrices for low, medium, & high levels of the variable of interest
  PSI2<- merge(PSI,pVOI, by = p)
  if(cuts[1] == "z"){group<-zgroups(as.matrix(PSI2$voi))}
  if(cuts[1] != "z"){group<-lmhgroups(PSI2$voi,cuts,per)}
  PSI.L <- data.frame(PSI2[1:3],PSI2[4:(ncol(PSI2)-1)]*sqrt(group$L))
  PSI.M <- data.frame(PSI2[1:3],PSI2[4:(ncol(PSI2)-1)]*sqrt(group$M))
  PSI.H <- data.frame(PSI2[1:3],PSI2[4:(ncol(PSI2)-1)]*sqrt(group$H))

  x <- list()
  x$L <- ecov(ecov_long(PSI.L))
  x$M <- ecov(ecov_long(PSI.M))
  x$H <- ecov(ecov_long(PSI.H))

  group2<-data.frame(PSI2[1],group)
  x$L$nP <- sum(ddply(group2, "p", function(x) mean(x$L, na.rm = T))[2], na.rm = T)
  x$M$nP <- sum(ddply(group2, "p", function(x) mean(x$M, na.rm = T))[2], na.rm = T)
  x$H$nP <- sum(ddply(group2, "p", function(x) mean(x$H, na.rm = T))[2], na.rm = T)

  return(x)
}
