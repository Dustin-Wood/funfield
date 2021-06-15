#' Estimate Action Expected-Outcome Covariance Elements
#' @description Compute all possible products of effects for all
#' features from weighted ESJT data, \code{wFFdata}.
#'      The average of the \code{dAdB} elements will ultimately
#' equal the covariances forming the expectancy matrix used to
#' estimate functional field models.
#'      Note #1: this function a more generalized version of function \code{ecov_big}, which 
#' provides flexibility of allowing multiple actions per scenario
#'      Note #2: this function assumes that the features have a "meaningful zero point."
#' That is a \code{0} should mean "no effect", and negative values should mean 
#' "negative effect", and positive values should mean "positive effect", with greater
#' deviations from zero meaning "stronger effect."  If data is not prepared this way,
#' the results are not very likely to be meaningful.
#'
#' Note: data can be weighted using \code{sweighteddata} function
#' @param wFFdata weighted functional field data
#' @param P_ID variable identifying 'person' (or respondent) - give in "quotemarks"
#' @param S_ID variable identifying 'situation' (or scenario) - give in "quotemarks"
#' @param A_ID variable identifying 'action' (or response) - give in "quotemarks"
#' @return Effect covariance matrix
#' @details
#' Usage notes: FFdata should have format of \code{P_ID}, \code{S_ID}, \code{A_ID}, as first three columns,
#' followed by effect ratings, and end in \code{Likelihood}
#'
#' @export

ecov_long <- function(wFFdata, P_ID = "P_ID", S_ID = "S_ID", A_ID = "A_ID") {
  library(dplyr)
  #1. Compute effectA*effectB elements.
  data_long <- melt(wFFdata, id=c(P_ID,S_ID,A_ID))
  data_long <- rename(data_long, fA=variable, xA=value)
  data_big <-merge(wFFdata,data_long,by.x = c(P_ID,S_ID,A_ID))
  FF_data_big <- melt(data_big, id=c(P_ID,S_ID,A_ID,"fA","xA")) 
  FF_data_big <- rename(FF_data_big, fB=variable,xB=value)
  FF_data_big$xAB<-FF_data_big$xA*FF_data_big$xB
  
  return(FF_data_big)
  
}
