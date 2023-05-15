#' Extract covariance and p Values from Lavaan Output
#' @description This function will extract covariance matrix information
#' (if present) in the parameter estimate output of lavaan, as well
#' as associated p-values.
#'
#' This function can be useful to getting appropriate p-values when working with
#' multilevel data, where ignoring the nested nature of the data can lead to artificially
#' 'statistically-significant' associations.
#' @param fit can provide as \code{summary(fit)$pe} statement in lavaan
#' @param vnames detail the variables you are associating
#' @return Will return covariances and associated pvalues in matrix form
#' @details There may be some fidgeting you will need to do to get the covariance
#' matrix to be spit out when there are no predictors.
#' If you are working with multilevel data, where you have mean-deviated all Level 1
#' variables (so mean = 0 for all in this set), you can predict those variables with a
#' Level 2 variable, which should necessarily have r(L1,L2)=0, to force generation of
#' covariances between everything.
#'
#' It will frequently be useful to quickly convert the covariances to correlations,
#' which can be done efficiently via \code{cov2cor(out$cov)}
#'
#' @export

covnps  <- function(fit, vnames) {

  fitPars<-lavaan::parameterestimates(fit)
  colnames(fitPars)[1:3]<-c("Y","op","X")

  #limit to rows that contain elements specified in 'vnames'
  fitPars <-  subset(fitPars, (Y %in% vnames))

  #limit params to correlation elements
  fitPars<-subset(fitPars, op == "~~")

  #function to blast separate parameters into separate field matrices
  #e.g., if in row i: X = "Speed", Y = "Ticket", and est = .20,
  #then create matrix element: f[Speed,Ticket] = .20
  matrify <- function(fmatrix,x,param){
    for(i in 1:nrow(x)) {
      fmatrix[x[i,"X"],x[i,"Y"]] <- x[i,param]
    }
    return(fmatrix)
  }
  #create empty matrix to store properties between 'vnames' variables
  fmatrix<-matrix(nrow=length(vnames),ncol=length(vnames),dimnames=list(vnames,vnames))

  #create matrix of covariances & associated p-values betwene 'vnames' variables
  cov <-  matrify(fmatrix,fitPars,"est")
  p <-  matrify(fmatrix,fitPars,"pvalue")

  out <- list(cov,p)
  names(out) <- c("cov","p")
  return(out)
}

