#' Expected Effects of Variable-of-Interest
#' @description  Give the average effects associated with this type of situation specified as the situational variable of interest (s_voi)
#'
#' Note: this function is currently defunct; it doesn't handle 'i' feature appropriately
#' I believe same functionality exists through combination
#' of \code{siWeightedData} and \code{ecov_long} and \code{ecov}
#' @param ESJT Expected effect data
#' @param voiset situational variable of interest
#' @param voi name of the variable of interest
#' @return expected effects for situational VOI
#'
#' @export


expEffects.voi <- function(ESJT,voiset,voi) {

  voiset$voi <- voi

  #append a particular type of additional attributes to the ESJT.
  x <- merge(ESJT,voiset,by = "s")
  x<-x[order(x$p,x$s),]

  wFF_act <- x[1:3]
  wFF_act$did_i <- 1*x$voi^2
  # weight the effect data by the square of the voi while preserving original sign of voi
  wFF_features <-  x[4:ncol(ESJT)]*x$voi^2*x$voi/abs(x$voi) #

  #here we have weighted data.
  wdata <- cbind(wFF_act,wFF_features)

  wdata<-wdata[,colnames(ESJT)] #this puts the variables back in the original order

  #finally, this provides a voi-weighted average of the original data.
  s.voi <-colSums(aggregate(wdata[3:ncol(wdata)], by=list(ESJT$s),
                            FUN=mean, na.rm=TRUE),na.rm=TRUE)/sum(voiset$voi^2)
  s.voi <- as.matrix(s.voi[-1])

  return(s.voi)

}
