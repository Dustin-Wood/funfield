#' Main Functional Field Results
#' @description Fit values of paths for a functional field, given an associated
#' \emph{expectancy} matrix.
#' @param fModel field model, specifying which paths are freely estimated vs.
#' constrained to zero
#' @param covMat covariance matrix to be fit to a field model
#' @param sampleN sample size associated with the covariance matrix
#' @return list with estimated (1) expectancy matrix (as correlations), (2) field matrix, and (3) discrepancy matrix (i.e., expectancy matrix not predicted by field matrix)
#' @details **This package needs to be further developed to provide p values for paths and other matters
#'
#' @export

fieldResults <- function(fModel,covMat,sampleN) {

  fit_r1 <- sem(fModel, sample.cov=covMat, sample.nobs = sampleN)

  # get the functional field matrices out of the damn lavaan output. Somehow this seems to work...
  fittedmatrix<-fit_r1@Model@GLIST$beta
  colnames(fittedmatrix)<-fit_r1@Model@dimNames[[1:2]]
  rownames(fittedmatrix)<-colnames(fittedmatrix)
  fieldmatrix_r1 <- t(fittedmatrix)[-1,-1]

  #this can be used to put the original matrices into the same order specified in the model (MO).
  MO<-colnames(fittedmatrix)[-1]
  r1_MO<-covMat[MO,MO]
  d_descriptives  <- cbind(r1_MO[,1],diag(as.matrix(r1_MO)^.5))

  res_r1 <- as.matrix(round(resid(fit_r1)$cov,4)) #on many occasions, this does NOT seem to provide the results that it should.

  #Standard output if desired.
  #summary(fit_r1)

  #extract the parameters
  parTable_r1 <- parTable(fit_r1)
  parTable_r1$t <- parTable_r1$est/parTable_r1$se
  parTable_r1$effNX <- sampleN
  parTable_r1$p <- round(2*pt(-abs(parTable_r1$t),df=sampleN-1),4)

  outfit_r1 <- parameterEstimates(fit_r1)
  outfit_r1$Neff <- sampleN
  outfit_r1[4:10] <-round(outfit_r1[4:10],3)


  #semPaths(fit_r1)


  #select and run up to here.
  #need to manually run 'View(res_r1)' to look at the residuals


  #look at how well each feature in the model is predicted.
  R_fit_r1 <- as.matrix(inspect(fit_r1, "rsquare")^.5)

  #extract info re: the Ms and SDs of effects from the overall and low/high groups.
  #eCov_all_MO<-eCov_all[MO,MO]

  #  View(r1_MO)
  #  View(fieldmatrix_r1)
  #  View(res_r1)

  #   corPlot(fieldmatrix_r1,numbers=T)
  #   corPlot(res_r1,numbers=T)
  #   corPlot(r1_MO,numbers=T)

  fieldResults<-list(r1_MO,fieldmatrix_r1,res_r1)
  names(fieldResults) <- c("eMatrix","fMatrix","uMatrix")
  return(fieldResults)
}


