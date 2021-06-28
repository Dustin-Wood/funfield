#' Effective N from Weighted Data
#' @description
#' This will return a list of the effective number of items (effNitems) and
#' effective number of markers for X (effNXitems) for the different VOIs
#' contained in specified S matrix.
#'
#' Note that variables should be on a -1 to 1 scale, where values near -1 or 1
#' indicate highly loading items, and items near 0 indicate items that should
#' be weighted negligibly.
#'
#' (**This function currently has issues when there are missing values...**)
#'
#' @param vois variables of interest
#' @param id is there an identifying variable? Defaults to \code{TRUE}
#' @return effective N for different variables
#'
#' @export


effectiveN <- function(vois, id = F) {
  m <- vois
if(id == T) {m <- m[,-1]}   #drop identifying vector
  sq_m <- m^2
  z <- scale(m)
  low_m = ifelse(z<0,z,0)
  high_m = ifelse(z>0,z,0)
  effNXitems <- as.matrix(colSums(sq_m,na.rm=TRUE)) #effective number of markers of X
  effNXratio <- effNXitems/nrow(m) #percentage of markers of X contained within the whole set
  max_sq_m <- apply(sq_m,2,max)
  weight = cbind(t(t(sq_m)/apply(sq_m,2,max)))

  #effective number of unit-weighted items (i.e., receiving 100% weight of the item receiving the most weight)
  effNitems <- as.matrix(colSums(weight,na.rm=TRUE))
  effNratio <- effNitems/nrow(m) #
  effN <-cbind(nrow(m),effNXitems,effNXratio,effNitems,effNratio)
  colnames(effN)<-c("N","effNXitems","effNXratio","effNitems","effNratio")
  return(effN)
}


