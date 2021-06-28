#' Create High-Medium-Low Z-Score Groups
#' @description
#' This will return three variables detailing the weight that each individual should
#' receive for subset analyses to look at how relatively "high" or "low" scorers
#' on the measure
#'
#' @param voi variable of interest
#' @return weight for low, medium, and high z-score groups
#' @details Note that the sum of the three groups will equal the total sample
#' of non-missing values.  Can be used in combination  with \code{effectiveN}.
#'
#' @export

zgroups <- function(voi) {

pvoi <-scale(voi)

#force the VOI onto a [-1,1] scale.
pvoi <- pvoi / max(abs(pvoi),na.rm = TRUE)

#optional code to create high/med/low level groups.
zL <- ifelse(pvoi < 0, abs(pvoi),0)
zM <- 1 - abs(pvoi)
zH <- ifelse(pvoi > 0, pvoi,0)

voigroup<-data.frame(zL,zM,zH)
return(voigroup)
}
