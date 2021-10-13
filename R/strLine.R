#' Percentage Straight-Lining Behavior Within Actions
#' @description
#' This will return the percentage of situation-action pairs for which the
#' respondent showed zero response variability.
#'
#'
#' @param ESJT ESJT matrix (should have 'p','s','i' as their first three features)
#' @param rmvLast remove the final variable (e.g., if it is Likelihood
#' ratings provided at different portion of survey)
#' @details It is recommended within Wood, Lowman, & Harms (202x) that
#' respondents showing 10% straightlining by this index be removed from all analyses
#' @return per0.p - the percentage of situation-action pairs in which
#' the respondent showed zero response variability whatsoever.
#'
#' @export


strLine <- function(ESJT, rmvLast=T) {
  if(rmvLast==T){
    varE.pi<-ddply(PSIdata, .(p,s,i), function(x) (sd(as.matrix(x[4:(ncol(x)-1)]),na.rm = T)))
  }  else {
    varE.pi<-ddply(PSIdata, .(p,s,i), function(x) (sd(as.matrix(x[4:ncol(x)]),na.rm = T)))
  }
  per0.p<-ddply(varE.pi, .(p), function(x) sum(x$V1 == 0)/nrow(x))
  return(per0.p)
}


