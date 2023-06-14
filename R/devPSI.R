#' Deviation Action Scoring of PSI matrix
#' @description
#' Will estimate how p's rating of outcomes of each action deviate within situation s
#' deviated from their average rating of outcomes of different actions in that situation
#'
#' @param PSIdata PSI-structured dataframe (should have 'p','s','i' as their first three features)
#' @param locX location of expected outcome ratings (note: default generously assumes
#' the prescribed 'c(p,s,i,...,Likelihood)' data structure)
#' ratings provided at different portion of survey)
#' @examples (develop some example code)
#' @details This essentially forces 'deviation' scoring.
#'
#' Note that when nI = 2, the deviation scores will be reflections of one another.
#' In which case, keeping only the maximum i (which should be i=1)  can be useful
#' to help avoid artificial inflation of sample size
#' (although other strategies, like multilevel modeling in lavaan,
#' may be preferable to address this issue).
#'
#' If we want the difference of the two actions (rather than deviation scoring),
#' we can pair the devPSI function with diffPSI.
#'
#' @return deviated ratings of effects of action i as rated by p within situation s
#' from the person's average rating of actions in that situation.
#' @export

devPSI <- function(PSIdata, locX=c(4:ncol(PSIdata))) {
  #make sure everything is sorted before doing dangerous merge later
  PSIdata <- with(PSIdata, PSIdata[order(p,s,i),])

  #kick out any cases that did not rate at least two actions
  PSIdata <- plyr::ddply(PSIdata, c("p","s"), function(x) subset(x, sum(!is.na(x$i))>1))
  devPS <- plyr::ddply(PSIdata, c("p","s"), function(x) sweep(x[locX],2, colMeans(x[locX], na.rm = T)))

  #(dangerous merge)
  devPSI <- data.frame(PSIdata[1:3],devPS[3:ncol(devPS)])

  return(devPSI)
}
