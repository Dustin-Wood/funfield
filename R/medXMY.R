#' Run Tests of Normative Simple Mediation Pathways of Total X->Y Relationship (no Moderators)
#' @description This will run and extract single-mediator mediation models
#' ('Model 1' in Wood, Harms, & Cho, 2023) for all measured potential mediators of the
#' [X->Y] association.
#' @param data dataset in a PSI long format
#' @param jSet location of the set of variables that will be explored as moderated by Z
#' @param X the initiating variable
#' @param Y the dependent variable (often "likelihood" - but not necessarily)
#' @param all include the full lavaan results for each variable (default to \code{F})
#' @return Will return the normative forces along the mediational pathway, separately
#' for each potential mediator.
#'
#' Will also return a summary that can be used to quickly glance the estimates of indirect
#' pathway and its statistical significance.
#' @details This adjusts for degrees of freedom by using 'p' as a clustering variable in lavaan
#'
#' Note that this model is referred to as the 'Minimal EV Model' in Wood et al., 2023,
#' and will produce a simple mediational model that is equivalent to the common
#' \code{c = a*b + c} formula found in mediational models, but using a different notation -
#' specifically \code{B0_YX = B1_MX*B1_YM + B1_YX}.
#'
#' The B0_YX is returned as a bit of a quality check.  If the estimates, p values, etc, are not
#' isn't the same across all cases, then this indicates that you did not get rid
#' of cases with missing values, which is generally recommended.
#'
#' @export
#'


medXMY <- function(data,X,Y,jSet,all=F) {

  #Minimal EV Model
  MedModel <- '
    M ~ 1 + B1_MX*X
    Y ~ 1 + B1_YX*X + B1_YM*M
    ind := B1_MX*B1_YM
    B0_YX := ind + B1_YX
  '

  MedModelTest <- function(x) {
    test <- data
    test["M"] <- x
    test["Y"] <- test[Y]
    test["X"] <- test[X]
    lavaan::sem(MedModel,test,cluster="p")
  }
  #run each mediator variable through Minimal EV Model, one at a time...
  x<-apply(data[jSet],2,MedModelTest)

  #...extract critical columns
  Expect<-plyr::ldply(x,function(x) parView(x)[parView(x)$label=="B1_MX",])
  Value<-plyr::ldply(x,function(x) parView(x)[parView(x)$label=="B1_YM",])
  EVInd<-plyr::ldply(x,function(x) parView(x)[parView(x)$label=="ind",])
  Total<-plyr::ldply(x,function(x) parView(x)[parView(x)$label=="B0_YX",])
  #format to paste into table
  allExps <- as.data.frame(paste0(Expect$est,"(",Expect$se,")",ifelse(Expect$pvalue<.05,"*","")))
  allVals <- as.data.frame(paste0(Value$est,"(",Value$se,")",ifelse(Value$pvalue<.05,"*","")))
  allEVInds <- as.data.frame(paste0(EVInd$est,"(",Value$se,")",ifelse(EVInd$pvalue<.05,"*",""),ifelse(EVInd$pvalue<.05 & EVInd$est>0,"(+)",ifelse(EVInd$pvalue<.05 & EVInd$est<0,"(-)",""))))

  summary <- data.frame(X,Expect[".id"],Y,allExps,allVals,allEVInds)
  colnames(summary) <- c("X","M","Y","B1_MX","B1_YM","B1_MX*B1_YM")

  if(all == F){
    out<-list(Expect,Value,EVInd,summary,Total)
    names(out)<-c("B1_MX","B1_YM","indXMY","summary","B0_YX")
  }
  if(all == T){
    out<-list(Expect,Value,EVInd,summary,Total,x)
    names(out)<-c("B1_MX","B1_YM","indXMY","summary","B0_YX","allResults")
  }
  return(out)

}
