#' Main Functional Field Results
#' @description Fit values of paths for a functional field, given an associated
#' \emph{expectancy} matrix.
#' @param fModel field model, specifying which paths are freely estimated vs.
#' constrained to zero
#' @param covMat covariance matrix to be fit to a field model (can be either a correlation
#' or covariance matrix)
#' @param sampleN sample size associated with the covariance matrix (defaults to '4', which
#' will basically make sure everything is as \code{ns} as possible)
#' @return list with estimated:
#' \item{eMatrix}{expectancy matrix}
#' \item{fMatrix}{field matrix}
#' \item{uMatrix}{'uniqueness' matrix (i.e., expectancy matrix not predicted by field matrix)}
#' \item{parTable}{parameter estimates, with associated significance values}
#' @details **This package needs to be further developed to address the problem with
#' misestimating the uMatrix when 'did_i' predicts every other variable (as desirable on occasion)
#'
#' @export

fieldResults <- function(fModel,covMat, sampleN = 4) {

  fit_r1 <- lavaan::sem(fModel, sample.cov=covMat, sample.nobs = sampleN)

  # get the functional field matrices out of the lavaan output.
  fittedmatrix<-fit_r1@Model@GLIST$beta
  colnames(fittedmatrix)<-fit_r1@Model@dimNames[[1:2]]
  rownames(fittedmatrix)<-colnames(fittedmatrix)
  fieldmatrix_r1 <- t(fittedmatrix)[-1,-1]

  #this can be used to put the original matrices into the same order specified in the model (MO).
  MO<-colnames(fittedmatrix)[-1]
  r1_MO<-covMat[MO,MO]
  res_r1 <- as.matrix(lavaan::resid(fit_r1)$cov) #on many occasions, this does NOT seem to provide the results that it should.

  #Standard output if desired.
  #summary(fit_r1)
  #d_descriptives  <- cbind(r1_MO[,1],diag(as.matrix(r1_MO)^.5))

  #extract the parameters
  parTable_r1 <- lavaan::parTable(fit_r1)
  parTable_r1$t <- parTable_r1$est/parTable_r1$se
  parTable_r1$effNX <- sampleN
  parTable_r1$p <- 2*pt(-abs(parTable_r1$t),df=sampleN-1)

  outfit_r1 <- lavaan::parameterEstimates(fit_r1)
  outfit_r1$Neff <- sampleN


  #semPaths(fit_r1)

  #look at how well each feature in the model is predicted.
  R_fit_r1 <- as.matrix(lavaan::inspect(fit_r1, "rsquare")^.5)


  fieldResults<-list(r1_MO,fieldmatrix_r1,res_r1,parTable_r1)
  names(fieldResults) <- c("eMatrix","fMatrix","uMatrix","parTable")
  return(fieldResults)
}
