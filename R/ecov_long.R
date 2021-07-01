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
#' @param eData expected effect data
#' @param p variable identifying 'person' (or respondent) - give in "quotemarks"
#' @param s variable identifying 'situation' (or scenario) - give in "quotemarks"
#' @param i variable identifying 'action' (or response) - give in "quotemarks"
#' @param addDidi should you add the 'did_i' variable as element for effect covariance matrix,
#' if not already present? Defaults to \code{TRUE}.
#' @return Effect covariance elements
#' @details
#' Usage notes: eData should typically have format of \code{p}, \code{s}, \code{i}, as first three columns,
#' followed by effect ratings, and end in \code{Likelihood}.  If there is a preferred ordering
#' of columns, you should put eData into that ordering *before* running this code.
#'
#' @export

ecov_long <- function(eData, p = "p", s = "s", i = "i",addDidi = T) {

#0.  Add the 'did_i' column to the set before estimating covariance elements
      #note: requires both option set to 'TRUE' and 'did_i' is not already present
  if(addDidi == T & (('did_i' %in% colnames(eData)) == F)) {
  eData<-data.frame(eData[1:3],1,eData[4:ncol(eData)])
  colnames(eData)[4] <- "did_i"
}

  #1. Compute effectA*effectB elements.
  #data_long <- reshape2::melt(eData, id=c(p,s,i))
  data_long <- tidyr::pivot_longer(eData,cols = 4:ncol(eData),  names_to = "fX", values_to = "dX")
  data_longer <-merge(data_long,eData,by = c(p,s,i))
  eData_long <- tidyr::pivot_longer(data_longer,cols = 6:ncol(data_longer),  names_to = "fY", values_to = "dY")
  eData_long$dXdY<-eData_long$dX*eData_long$dY
  return(eData_long)

}
