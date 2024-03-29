#' Situation Weighted Data
#'
#' @description
#' In brief: weight situations by a variable of interest.  Mainly of value for eventually combining multiple ratings to a single score.  For instance: combining diverse ESJT ratings to a single score weighted by the situation's assertiveness-diagnosticity.
#' @usage
#' sweighteddata(ESJT, voiset, voi)
#' @param ESJT ESJT raw data; must be headed with three variables: \code{p, s, i} -
#' for 'Person', 'Situation', '[Initiated] action', respectively
#' @param voiset Set containing the variable of interest
#' @param voi Name of the specific variable within the voiset you wish to weight by
#' @param bySet columns you wish to match by (defaults to 'situation & action')
#' @return The expected effects of the action, weighted by the action's situation-diagnosticity
#'
#' @export

sweighteddata <- function(ESJT,voiset, voi, bySet = c("s","i")) {

  #weight effect ratings by a variable of interest from S_ID set

  voiset$voi <- voi

  #apend a particular type of additional attributes to the file.
  x <- merge(ESJT,voiset,by = bySet)
  x<-x[order(x$p,x$s,x$i),]

  #weight by the variable of interest
  wESJT_act <- x[1:3]
  wESJT_act$did_i <-  x$did_i*x$voi
  wESJT_features <-  x[4:ncol(ESJT)]*abs(x$voi)

  #here we have ESJT data, weighted by situation or action factor
  wESJT <- cbind(wESJT_act,wESJT_features)

  wESJT<-wESJT[,colnames(ESJT)] #this puts the variables back in the original order

  return(wESJT)

}


