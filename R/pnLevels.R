#' Positive and Negative Levels'
#' @description A small modification to qgraph that allows cells to be
#' colored as a gradient of the level of the variable, on a scale from range
#' \code{[-1:1]}
#' @param levels ?the level of the factor? Need to review this some more
#' @details ?Yeah, this function needs a little review to figure out how it is working within \code{qgraph}
#'
#' @export

pnLevels <- function(levels) {

  vout <- 0
  for(i in 1:length(levels)) {
    if(levels[i]<0) {
      vout <- rbind(vout,i)
    }
  }
  vneg <- vout[-1]
  vout <- 0
  for(i in 1:length(levels)) {
    if(levels[i]>=0) {
      vout <- rbind(vout,i)
    }
  }
  vpos <- vout[-1]
  pnGroups <- list(vpos,vneg)
  names(pnGroups) <- c("vpos","vneg")

  return(pnGroups)

}

