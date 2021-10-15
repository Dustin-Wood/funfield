#' Create Low-Medium-High Subgroups at Specified Levels
#' @description
#' This will return three variables detailing whether the participant
#' should be placed in a 'low', 'medium' or 'high' subgroup
#'
#' @param voi variable of interest
#' @param cuts Two numbers specifying (1) highest value for LOW membership,
#' and (2) lowest value for HIGH membership.
#' Remainder will be placed in MIDDLE group.
#' @param per Should VOI be converted to a 'percentage rank' variable? (Default to \code{F})
#' If \code{T} then \code{cuts} shift to becoming probabilities to cut at
#' (i.e., values should be between 0 and 1)
#' @return weight for low, medium, and high z-score groups
#' @details Note that the sum of the three groups will equal the total sample
#' of non-missing values.  Can be used in combination  with \code{effectiveN}.
#'
#' @export
#' @examples
#' #create about equal sized subgroups (~33%) if(!) there are no ties in data
#' xcuts<-lmhgroups(x, cuts = c(1/3,2/3), per = T)


lmhgroups <- function(voi, cuts, per=F) {
  if(per == T) {
    perRank <- function(x) {x2<-(rank(x)-.5)/length(x); return(x2)}
    voi <- perRank(voi)
  }

  L <- ifelse(voi <= cuts[1], 1,0)
  M <- ifelse(voi > cuts[1] & voi < cuts[2], 1,0)
  H <- ifelse(voi >= cuts[2], 1,0)

  lmhgroup<-data.frame(L,M,H)
  return(lmhgroup)
}
