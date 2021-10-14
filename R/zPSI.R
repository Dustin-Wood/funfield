#' Scale-Center-Standardized PSI Ratings
#' @description
#' Transforms ESJT ratings into a z-score-like metric indicating
#' the number of 'standard-deviations-from-zero' the person's original rating of the item was.
#'
#' @param PSIdata original PSI data (should have 'p','s','i' for first three columns)
#' @return Standardized scale-centered scores.
#' @details This syntax implicitly assumes that 0 is a meaningful point on the scale.
#' The original data collection and the prior processing of the data should reflect this.
#'
#' * The scale-centered scores do NOT have the usual interpretation of
#' [regular] z-scores as being translatable to approximate probabilities assuming
#' a normal distribution of responses.
#'
#' * If the person gives the same score to all items, all \code{zcx}
#' will equal \code{1}.
#'
#' @return Original matrix but with data now equating level of variability for each participant
#'
#' @export

zPSI <- function(PSIdata) {

  scPSI.p <- plyr::ddply(PSIdata, "p", function(x) sqrt(sum(x[4:ncol(x)]^2)/(ncol(x)*nrow(x))))
  colnames(scPSI.p)<- c("p","scPSI.p")

  zPSIdata<-merge(PSIdata, scPSI.p, by = "p")
  zPSIdata[4:ncol(zPSIdata)]<-zPSIdata[4:(ncol(zPSIdata)-1)]/zPSIdata$scPSI.p
  zPSIdata<-zPSIdata[-ncol(zPSIdata)] #get rid of last column from zPSIdata matrix
  export <- list(zPSIdata,scPSI.p)
  names(export) <- c("zPSIdata","scPSI.p")
  return(export)
}

