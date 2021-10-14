#' Standard deviation of ESJT ratings
#' @description
#' This will index the standard deviation of the person's ratings across all of the
#' ESJT scenarios they rated.
#'
#' @param ESJT ESJT matrix (should have 'p','s','i' as their first three features)
#' @param rmvLast remove the final variable (e.g., if it is Likelihood
#' ratings provided at different portion of survey)
#' @details It is recommended within Wood, Lowman, & Harms (202x) that
#' respondents showing values of sd.p below 25\% of the scale maximum be removed from all analyses
#' @return sdE.p - the percentage of situation-action pairs in which
#' the respondent showed zero response variability whatsoever.
#' @export
#' @examples
#' #combine this function and recommended screen into a single statement:
#' analysiscases <- subset(sdESJT(ESJT), sdE.p > .25)

sdESJT <- function(PSIdata, rmvLast=T) {
  if(rmvLast==T){
    sdE.p<-plyr::ddply(PSIdata, "p", function(x) sd(as.matrix(x[4:(ncol(x)-1)],na.rm=T)))
  }  else {
    sdE.p<-plyr::ddply(PSIdata, "p", function(x) sd(as.matrix(x[4:ncol(x)],na.rm=T)))
  }
  colnames(sdE.p) <- c("p","sdE.p")
  return(sdE.p)
}


