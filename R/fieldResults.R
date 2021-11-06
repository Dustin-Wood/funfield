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
#' \item{uMatrix2}{'uniqueness' matrix2 (a crude work-around for when lavaan does not want to estimate correctly; see https://github.com/yrosseel/lavaan/issues/44}
#' \item{parTable}{parameter estimates, with associated significance values}
#' @details Note that the \code{uMatrix2} is a servicable means of estimating
#' the unpredicted residuals in some of the models where lavaan does not want
#' to provide the path-analysis implied residuals. But it should only work when
#' the model is a DAG that has been placed to have rows in 'causal order'.
#'     There is also a problem with how \code{uMatrix2} is estimated (the final covariance
#'     is always misestimated) which I assume
#'     involves a bad interaction with how \code{expOutcomes} function closes out
#' @export

fieldResults <- function(fModel,covMat, sampleN = 4) {

  fit_cov1 <- lavaan::sem(fModel, sample.cov=covMat, sample.nobs = sampleN)

  # get the functional field matrices out of the lavaan output.
  fittedmatrix<-fit_cov1@Model@GLIST$beta
  colnames(fittedmatrix)<-fit_cov1@Model@dimNames[[1:2]]
  rownames(fittedmatrix)<-colnames(fittedmatrix)
  fieldmatrix_cov1 <- t(fittedmatrix)[-1,-1]

  #this can be used to put the original matrices into the same order specified in the model (MO).
  MO<-colnames(fittedmatrix)[-1]
  cov1_MO<-covMat[MO,MO]
  res_cov1 <- as.matrix(lavaan::resid(fit_cov1)$cov) #on many occasions, this does NOT seem to provide the results that it should.
  #summary(fit_cov1)  #Standard output if desired.

  #estimate total effects, crudely
  #this function should only work when the field matrix is truly a
  #DAG, with the field matrix in causal order
  totalE <- fieldmatrix_cov1 * 0
  for(i in 1:nrow(fieldmatrix_cov1)){
    doi <- rep(0,ncol(fieldmatrix_cov1))
    doi[i] <-  sqrt(diag(cov1_MO)[i] - diag(totalE)[i]) #activate node, but discount by the degree to which it has already been activated by other variables previously
    effect.i<-colSums(expOutcomes(doi,fieldmatrix_cov1))
    E.i<-as.matrix(effect.i)%*%t(as.matrix(effect.i)) #what are the total implied covariation from activating i?
    totalE <- totalE + E.i
  }
  res2 <- cov1_MO - totalE


  #extract the parameters
  parTable_cov1 <- lavaan::parTable(fit_cov1)
  parTable_cov1$t <- parTable_cov1$est/parTable_cov1$se
  parTable_cov1$effNX <- sampleN
  parTable_cov1$p <- round(2*pt(-abs(parTable_cov1$t),df=sampleN-1),6)

  outfit_cov1 <- lavaan::parameterEstimates(fit_cov1)
  outfit_cov1$Neff <- sampleN


  #semPaths(fit_cov1)

  #look at how well each feature in the model is predicted.
  R_fit_cov1 <- as.matrix(lavaan::inspect(fit_cov1, "rsquare")^.5)


  fieldResults<-list(cov1_MO,fieldmatrix_cov1,res_cov1,res2,parTable_cov1)
  names(fieldResults) <- c("eMatrix","fMatrix","uMatrix","uMatrix2","parTable")
  return(fieldResults)
}
