#' Estimate Basic Covariance Elements
#' @description Compute all possible products of effects for all
#' features from weighted ESJT data, \code{wFFdata}.
#'      Note that the average of the \code{dAdB} elements will ultimately
#' equal the covariances forming the expectancy matrix used to
#' estimate functional field models.
#'
#' Note: data can be weighted using \code{sweighteddata} function
#' @param wFFdata weighted functional field data
#' @return Effect covariance matrix
#' @details
#' Usage notes: FFdata should have format of \code{P_ID}, \code{S_ID}, \code{Did_A}, as first three columns,
#' followed by effect ratings, and end in \code{Likelihood}
#'
#' @export


ecov_big <- function(wFFdata) {
  #1. Compute effectA*effectB elements.
  data_long <- reshape2::melt(wFFdata, id=c("P_ID","S_ID"))
  data_long <- dplyr::rename(data_long, fA=variable, xA=value)
  data_big <-merge(wFFdata,data_long,by.x = c("P_ID","S_ID"))
  FF_data_big <- reshape2::melt(data_big, id=c("P_ID","S_ID","fA","xA"))
  FF_data_big <- dplyr::rename(FF_data_big, fB=variable,xB=value)
  FF_data_big$xAB<-FF_data_big$xA*FF_data_big$xB

  return(FF_data_big)

}

