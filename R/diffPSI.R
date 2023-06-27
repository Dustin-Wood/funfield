#' Difference Scoring of nI=2 PSI matrix
#' @description
#' Will either return the difference Will estimate how p's rating of outcomes of each action deviate within situation s
#' deviated from their average rating of outcomes of different actions in that situation
#'
#' @param devPSI deviated PSI-structured dataframe (should have 'p','s','i' as their first three features)
#' @param locX location of expected outcome ratings (default generously assumes
#' the prescribed 'c(p,s,i,...,Likelihood)' data structure)
#' @param select_i Return the maximum or minimum of i?
#' @param dbl Double the deviation scores (default is T)
#' @details Note that when nI = 2, the dpsi scores will be reflections of one another.
#' In which case, keeping only the minimum or maximum i for each scenario can be useful
#' to help avoid artificial inflation of sample size
#' (although other strategies, like multilevel modeling in lavaan,
#' may be preferable to address this issue).
#'
#' When nI is greater than 2, a more sophisticated script should be used, which perhaps subtracts the
#' focal action (e.g., A = 1) from the average of the contrasting actions (e.g., A = 0)
#' @return expected effects (difference) of focal action(s) from contrasting action(s)
#' @export

diffPSI <- function(devPSI, locX=c(4:ncol(devPSI)),select_i = "max",dbl = T) {

  #optional return only first action
  if(select_i == "max") {
    diffpsi<-plyr::ddply(devPSI, c("p","s"), function(x) subset(x, i == max(i)))
  }

  #optional return only first action
  if(select_i == "min") {
    diffpsi<-plyr::ddply(devPSI, c("p","s"), function(x) subset(x, i == min(i)))
  }

  if(dbl == T) {
    diffpsi[locX] <- diffpsi[locX]*2
  }
  return(diffpsi)
}
