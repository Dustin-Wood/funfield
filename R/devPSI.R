#' Deviation Action Scoring of PSI matrix
#' @description
#' Will estimate how p's rating of outcomes of each action deviate within situation s
#' deviated from their average rating of outcomes of different actions in that situation
#'
#' @param PSIdata PSI-structured dataframe (should have 'p','s','i' as their first three features)
#' @param locX location of expected outcome ratings (note: default generously assumes
#' the prescribed 'c(p,s,i,...,Likelihood)' data structure)
#' ratings provided at different portion of survey)
#' @param max_i Return only the maximum value of i (often i=1)
#' @param dbl Double the deviation scores
#' (this will mainly make sense to do if nI = 2 and max_i = T,
#' in which case scores will be difference scores rather than deviation scores)
#' @examples (develop some example code)
#' @details This essentially forces 'deviation' scoring.  It also assumes that
#' 'i' is coded as a numerical variable, and if 'max_i' is selected, this will only return
#' the actions with the highest value.  It is recommended this is only used when
#' have been coded where i = 1 for the focal action, i = 0 for a contrasting action,
#' and other actions i have been excluded (maybe by subsetting), so that nI = 2.
#' Note that when nI = 2, the deviation scores will be reflections of one another.
#' In which case, keeping only the maximum i (which should be i=1)  can be useful
#' to help avoid artificial inflation of sample size
#' (although other strategies, like multilevel modeling in lavaan,
#' may be preferable to address this issue).
#'
#' Also note that in this case, if the deviations are doubled, you will obtain the
#' simple difference between i=1 and i=2 for each feature.
#' @return deviated ratings of effects of action i as rated by p within situation s
#' from the person's average rating of actions in that situation.
#' @export

devPSI <- function(PSIdata, locX=c(4:ncol(PSIdata)),max_i = F,dbl = F) {
  #make sure everything is sorted before doing dangerous merge later
  PSIdata <- with(PSIdata, PSIdata[order(p,s,i),])

  #kick out any cases that did not rate at least two actions
  PSIdata <- ddply(PSIdata, .(p,s), function(x) subset(x, sum(!is.na(x$i))>1))
  devPS <- ddply(PSIdata, .(p,s), function(x) sweep(x[locX],2, colMeans(x[locX], na.rm = T)))

  #(dangerous merge)
  devPSI <- data.frame(PSIdata[1:3],devPS[3:ncol(devPS)])

  #optional return only first action
  if(max_i == T) {
    devPSI<-ddply(devPSI, .(p,s), function(x) subset(x, i == max(i)))
  }

  if(dbl == T) {
    devPSI[4:ncol(devPSI)] <- devPSI[4:ncol(devPSI)]*2
  }
  return(devPSI)
}
