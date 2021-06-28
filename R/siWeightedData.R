#' Situation-Action Weighted Data
#'
#' @description
#' Weight situation-action expected effects by a variable of interest.
#' Of value for combining multiple situation-action pairs into a single situation,
#' weighted by the situation-action pair's trait diagnosticity.
#' (**This function is intended to replace "sweighteddata" soon.)
#' @usage
#' siWeightedData(ESJT, voiset, "voi")
#' @param ESJT ESJT raw data; must be headed with three variables: \code{p, s, i} -
#' for 'Person', 'Situation', '[Initiated] action', respectively
#' @param voiset Set containing the variable of interest
#' @param voi Name of the specific variable within the voiset you wish to weight by
#' - should be a variable name given in "quotes"
#' @param bySet columns you wish to match by (defaults to 'situation & action')
#' @return The expected effects of the action, weighted by the action's situation-diagnosticity
#'
#' @export

siWeightedData <- function(ESJT,voiset, voi, bySet = c("s","i")) {

  #apend a particular type of additional attributes to the file.
  x <- merge(ESJT,voiset,by = bySet)
  x<-x[order(x$p,x$s,x$i),]

  #weight by the variable of interest
  wESJT_act <- x[1:3]
  wESJT_act[,"did_i"] <-  1*as.vector(x[voi])
  wESJT_features <-  x[4:ncol(ESJT)]*abs(x[,voi])

  #here we have ESJT data, weighted by situation or action factor
  wESJT <- data.frame(wESJT_act,wESJT_features)

  #need to figure out where 'did_i' should be generated before running this..
#  wESJT<-wESJT[,colnames(ESJT)] #this puts the variables back in the original order

  return(wESJT)

}


