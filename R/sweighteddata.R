#' Situation Weighted Data
#'
#' @description
#' In brief: weight situations by a variable of interest.  Mainly of value for eventually combining multiple ratings to a single score.  For instance: combining diverse ESJT ratings to a single score weighted by the situation's assertiveness-diagnosticity.
#' @usage
#' sweighteddata(FFdata, voiset, voi)
#' @param FFdata ESJT raw data; must be headed with three variables (**which are...?**)
#' @param voiset Set containing the variable of interest
#' @param voi Name of the specific variable within the voiset you wish to weight by
#' @return The expected effects of the action, weighted by the action's situation-diagnosticity
#'
#' @export

sweighteddata <- function(FFdata,voiset,voi) {

  #weight effect ratings by a variable of interest from S_ID set

  voiset$voi <- voi

  #apend a particular type of additional attributes to the file.
  x <- merge(FFdata,voiset,by = "S_ID")
  x<-x[order(x$P_ID,x$S_ID),]

  wFF_act <- x[1:3]
  wFF_act$DidA <-  x$DidA*x$voi
  wFF_features <-  x[4:ncol(FFdata)]*abs(x$voi)

  #here we have weighted data.
  wdata <- cbind(wFF_act,wFF_features)

  wdata<-wdata[,colnames(FFdata)] #this puts the variables back in the original order

  return(wdata)

}


