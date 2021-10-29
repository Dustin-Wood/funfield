#' Deviation Action Scoring of PSI matrix
#' @description
#' Will estimate how p's rating of outcomes of each action deviate within situation s
#' deviated from their average rating of outcomes of different actions in that situation
#'
#' @param PSIdata PSI-structured dataframe (should have 'p','s','i' as their first three features)
#' @param locX location of expected outcome ratings (note: default generously assumes
#' the prescribed 'c(p,s,i,...,Likelihood)' data structure)
#' ratings provided at different portion of survey)
#' @details This essentially forces 'deviation' scoring.  It also assumes that
#' 'i' is coded as a numerical variable, and if 'min_i' is selected will return only
#' the actions with the lowest value (so if coded as '1,2,3...' then will only return '1';
#' if '2.1,2.2,2.3,...' will only return '2.1', and so on). When nI = 2 for each
#' situation, deviation scoring will force i=1 and i=2 to be perfect reflections of one
#' another, so only one (i=1) is retained.
#'
#' Also note that in this case, if the deviations are doubled, you will obtain the
#' simple difference between i=1 and i=2 for each feature.
#' @return deviated ratings of effects of action i as rated by p within situation s
#' from the person's average rating of actions in that situation.
#' @export

devPSI <- function(PSIdata, locX=c(4:ncol(PSIdata)),min_i = T) {
  #make sure everything is sorted before doing dangerous merge later
  PSIdata <- with(PSIdata, PSIdata[order(p,s,i),])

  #kick out any cases that did not rate at least two actions
  PSIdata <- ddply(PSIdata, .(p,s), function(x) subset(x, sum(!is.na(x$i))>1))
  devPS <- ddply(PSIdata, .(p,s), function(x) sweep(x[locX],2, colMeans(x[locX], na.rm = T)))

  #(dangerous merge)
  devPSI <- data.frame(PSIdata[1:3],devPS[3:ncol(devPS)])

  #optional return only first action
  if(min_i == T) {
    devPSI<-ddply(devPSI, .(p,s), function(x) subset(x, i == min(i)))
  }
  return(devPSI)
}
