#' Expected Outcomes Matrix
#' @description Estimate expected outcomes of current situation
#' given current field
#' @param s situation vector or matrix, or 'situation states at a given time'
#' @param ff field matrix, or 'field at a given time'
#' @return expected situational states for next 'step' (increment of time)
#' @details Input \code{s} can be either a single situation or a range of
#' possible situations, with the situational states for each given on a separate row
#'
#' (Hopefully this will soon be completely outmoded and replaced with 'expOutcomesFF')
#'
#' @export

expOutcomes <- function(s,ff) {
  e_t <- matrix(0,length(s)+1,length(s))
  e_t[1,] <- s
  i=2
  for (i in 2:(length(s)+1)) {
    e_t[i,] <- e_t[i-1,]%*%ff
  }
  return(e_t)
}
