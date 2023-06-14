#' Provide Rounded Parameter Estimates
#' @description This will return the parameter estimates from lavaan, but with values
#' rounded to a desired numer of decimal spaces.
#' @param fit standard output from lavaan
#' @param split locate where variables will start to be rounded
#' @param dec number of variables to round to
#' @return parameter estimates rounded down to desired number of decimal spaces
#'
#' @export

parView<-function(fit,split = "est",dec = 3){
  x <- lavaan::parameterestimates(fit)
  return(cbind(x[1:which(colnames(x)==split)-1],round(x[(which(colnames(x)==split)):(which(colnames(x)=="z"))],dec),round(x["pvalue"],dec+2)))
}


