#' Run Simple Test of how X->Y (or bYX) relationship is moderated by Z for Outcomes X
#' @description This will run and extract single-mediator moderated-mediation models
#' ('Model 1' in Wood, Harms, & Cho, 2023) for all measured potential mediators of the
#' [Action->Likelihood] association.
#' ** continue editing here...**
#' @param PSIdata dataset in a PSI long format
#' @param Z moderator variable (it will be standardized)
#' @param jSet location of the set of variables that will be explored as moderated by Z
#' @param Y the dependent variable (often "likelihood" - but not necessarily)
#' @return Will return the main effects of the outcomes, and their z-moderated effect
#' @details This adjusts for degrees of freedom by using 'p' as a clustering variable in lavaan
#'
#' @export

ModXY <- function(PSIdata,Z,jSet,Y) {

ModModel <- 'Y ~ 1 + Z + B1_Y1*X1 + BZ_Y1*X1:Z'

moderatorTest <- function(x) {
  test <- data
  test$X1 <- x
  test["Y"] <- test[Y]
  sem(ModModel,test,cluster="p")
}
#run the thing through each mediator variable...
data["Z"] <- scale(data[Z]) #name of moderator
x<-apply(data[jSet],2,moderatorTest)

  #...and extract the overall and Z-moderated effect of X1 on Y information
  BZ_Y1<-plyr::ldply(x,function(x) parView(x)[parView(x)$label=="BZ_Y1",])
  B1_Y1<-plyr::ldply(x,function(x) parView(x)[parView(x)$label=="B1_Y1",])

  out<-list(BZ_Y1,B1_Y1)
  names(out)<-c("BZ_Y1","B1_Y1")
  return(out)
}


