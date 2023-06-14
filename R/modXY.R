#' Run Simple Test of how X->Y (or bYX) relationship is moderated by Z for Outcomes X
#' @description This will run and extract single-mediator moderated-mediation models
#' ('Model 1' in Wood, Harms, & Cho, 2023) for all measured potential mediators of the
#' [Action->Likelihood] association.
#' ** continue editing here...**
#' @param data dataset in a PSI long format
#' @param Z moderator variable (it will be standardized)
#' @param xSet location of the set of variables that will be explored as moderated by Z (one at a time)
#' @param Y the dependent variable (often "likelihood" - but not necessarily)
#' @return Will return the main effects of the outcomes, and their z-moderated effect
#' @details This adjusts for degrees of freedom by using 'p' as a clustering variable in lavaan
#'
#' @export

modXY <- function(data,Z,xSet,Y) {

ModModel <- 'Y ~ 1 + Z + B1_YX*X + BZ_YX*X:Z'

moderatorTest <- function(x) {
  test <- data
  test$X <- x
  test["Y"] <- test[Y]
  lavaan::sem(ModModel,test,cluster="p")
}
#run the thing through each mediator variable...
data["Z"] <- scale(data[Z]) #name of moderator
x<-apply(data[xSet],2,moderatorTest)

  #...and extract the overall and Z-moderated effect of X1 on Y information
  BZ_YX<-plyr::ldply(x,function(x) parView(x)[parView(x)$label=="BZ_YX",])
  B1_YX<-plyr::ldply(x,function(x) parView(x)[parView(x)$label=="B1_YX",])

  out<-list(BZ_YX,B1_YX)
  names(out)<-c("BZ_YX","B1_YX")
  return(out)
}


