#' Percentage Straight-Lining Behavior Within Actions
#' @description
#' This will return the percentage of situation-action pairs for which the
#' respondent showed zero response variability.
#'
#' @param ESJT ESJT matrix (should have 'p','s','i' as their first three features)
#' @param locX location of expected outcome ratings (note: default generously assumes
#' the prescribed 'c(p,s,i,...,Likelihood)' data structure)
#' ratings provided at different portion of survey)
#' @details It is recommended within that respondents showing
#' 10\% straightlining by this index be removed from all analyses,
#' however less stringent cutoffs (e.g., 25\%) might be employed when
#' respondents rate very few features (e.g., 5 or less) or if all features
#' are desirable, both of which can make straight-lining behavior less suspect
#' @return per0.p - the percentage of situation-action pairs in which
#' the respondent showed zero response variability whatsoever.
#' @export
#' @examples
#' #combine this function and recommended screen into a single statement:
#' analysiscases <- subset(strLine(ESJT), per0.p > .10)

strLine <- function(ESJT, locX=c(4:(ncol(ESJT)-1))) {
    varE.pi<-ddply(PSIdata, .(p,s,i), function(x) (sd(as.matrix(x[locX]),na.rm = T)))
  per0.p<-ddply(varE.pi, .(p), function(x) sum(x$V1 == 0)/nrow(x))
  colnames(per0.p) <- c("p","per0.p")
  return(per0.p)
}


