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
#' @param VOI (person-)variable of interest (must provide in quotations)
#' @param p the variable containing the 'person' identifier (defaults to \code{"p"})
#' @return Effect covariance matrices (and sample sizes) for \code{zL, zM, zH}
#' (low, medium, and high z-scores on the person variable of interst)
#' subgroups
#' @details
#'
#' @export

ecov.lmhVOI <- function(PSI, pVars, VOI, p = "p"){
  ##create a small file to merge variable of interest with PSI file
  pVOI<-data.frame(pVars[p],pVars[VOI])
  colnames(pVOI)<-c("p","VOI")

  #create specific covariance matrices for low, medium, & high levels of the variable of interest
  PSI2<- merge(PSI,pVOI, by = p)
  zLevel<-zgroups(as.matrix(PSI2$VOI))
  PSI.zL <- data.frame(PSI2[1:3],PSI2[4:(ncol(PSI2)-1)]*sqrt(zLevel$zL))
  PSI.zM <- data.frame(PSI2[1:3],PSI2[4:(ncol(PSI2)-1)]*sqrt(zLevel$zM))
  PSI.zH <- data.frame(PSI2[1:3],PSI2[4:(ncol(PSI2)-1)]*sqrt(zLevel$zH))

  x <- list()
  x$zL <- ecov(ecov_long(PSI.zL))
  x$zM <- ecov(ecov_long(PSI.zM))
  x$zH <- ecov(ecov_long(PSI.zH))

  return(x)
}
