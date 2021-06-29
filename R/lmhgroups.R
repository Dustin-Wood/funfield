#' Create Low-Medium-High Subgroups at Specified Levels
#' @description
#' This will return three variables detailing whether the participant
#' should be placed in a 'low', 'medium' or 'high' subgroup
#'
#' @param voi variable of interest
#' @param cuts Two numbers specifying (1) highest value for LOW membership,
#' and (2) lowest value for HIGH membership.
#' Remainder will be placed in MIDDLE group.
#' @return weight for low, medium, and high z-score groups
#' @details Note that the sum of the three groups will equal the total sample
#' of non-missing values.  Can be used in combination  with \code{effectiveN}.
#'
#' @export

lmhgroups <- function(voi, cuts) {
  L <- ifelse(voi <= cuts[1], 1,0)
  M <- ifelse(voi > cuts[1] & voi < cuts[2], 1,0)
  H <- ifelse(voi >= cuts[2], 1,0)

  lmhgroup<-data.frame(L,M,H)
  return(lmhgroup)
}
