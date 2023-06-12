#' Run Tests of Moderated-Mediation Pathways of Total X->Y Relationship
#' @description This will run and extract single-mediator moderated-mediation models
#' ('Model 1' in Wood, Harms, & Cho, 2023) for all measured potential mediators of the
#' [X->Y] association.
#' @param data dataset in a PSI long format
#' @param Z moderator variable (it will be standardized)
#' @param jSet location of the set of variables that will be explored as moderated by Z
#' @param X the initiating variable
#' @param Y the dependent variable (often "likelihood" - but not necessarily)
#' @param all include the full lavaan results for each variable (default to \code{F})
#' @return Will return the main effects of forces along the mediational pathway,
#' and the z-moderation of each force.
#'
#' Will also print out some summary sheets ('paths' and 'paths2') as a
#' means of helping pull out key results
#' @details This adjusts for degrees of freedom by using 'p' as a clustering variable in lavaan
#'
#' The general decomposition of indirect pathway is informed by Muller et al., 2005.
#'
#' @export

modmedXMY <- function(data,X,Y,Z,jSet,all=F) {

ModMedModel <- '
Y ~ 1 + Z + B1_YX*X + BZ_YX*X:Z
Y ~ B1_YM*M + BZ_YM*M:Z
M ~ 1 + Z + B1_MX*X + BZ_MX*X:Z
indX1MZY := B1_MX*BZ_YM
indXZM1Y := BZ_MX*B1_YM
'

parView<-function(fit,split = "est",dec = 3){
  x <- parameterestimates(fit)
  return(cbind(x[1:which(colnames(x)==split)-1],round(x[(which(colnames(x)==split)):(which(colnames(x)=="z"))],dec),round(x["pvalue"],dec+2)))
}

moderatorTest <- function(x) {
  test <- data
  test["M"] <- x
  test["Y"] <- test[Y]
  test["X"] <- test[X]
  sem(ModMedModel,test,cluster="p")
}
#run the thing through each mediator variable...
data["Z"] <- scale(data[Z]) #name of moderator
x<-apply(data[jSet],2,moderatorTest)

  #...and extract the overall and Z-moderated effect of X1 on Y information
  BZ_YM<-plyr::ldply(x,function(x) parView(x)[parView(x)$label=="BZ_YM",])
  BZ_MX<-plyr::ldply(x,function(x) parView(x)[parView(x)$label=="BZ_MX",])
  B1_YM<-plyr::ldply(x,function(x) parView(x)[parView(x)$label=="B1_YM",])
  B1_MX<-plyr::ldply(x,function(x) parView(x)[parView(x)$label=="B1_MX",])
  indX1MZY<-plyr::ldply(x,function(x) parView(x)[parView(x)$label=="indX1MZY",])
  indXZM1Y<-plyr::ldply(x,function(x) parView(x)[parView(x)$label=="indXZM1Y",])

  paths<-cbind(X,B1_MX["est"],paste0(BZ_MX[,"est"],"*(Z_",Z,")"),BZ_MX[,".id"],B1_YM["est"],paste0(BZ_YM[,"est"],"*(Z_",Z,")"),Y,ifelse((indXZM1Y[,"est"]>0&indXZM1Y[,"pvalue"]<.05),"+",ifelse((indXZM1Y[,"est"]<0&indXZM1Y[,"pvalue"]<.05),"-"," ")),ifelse((indX1MZY[,"est"]>0&indX1MZY[,"pvalue"]<.05),"+",ifelse((indX1MZY[,"est"]<0&indX1MZY[,"pvalue"]<.05),"-"," ")))
  colnames(paths) <- c("X","b1_MX","bZ_MX","M","b1_YX","bZ_YX","Y","bZ_MX*b1_YM","b1_MX*bZ_YM")

  paths2<-cbind(X,BZ_MX[,".id"],Y,Z,BZ_MX[,"est"],B1_YM["est"],indXZM1Y[,"est"],indXZM1Y[,"pvalue"],B1_MX["est"],BZ_YM[,"est"],indX1MZY[,"est"],indX1MZY[,"pvalue"])
  colnames(paths2) <- c("X","M","Y","Z","BZ_MX","B1_LM","indbyZE","pindbyZE","B1_MX","BZ_LM","indbyZV","pindbyZV")

#format to paste into table
  allZExps <- as.data.frame(paste0(BZ_MX$est,"(",BZ_MX$se,")",
                            ifelse(BZ_MX$pvalue<.05,"*",""),
                            ifelse(indXZM1Y$pvalue<.05 & indXZM1Y$est>0,"(+)",
                            ifelse(indXZM1Y$pvalue<.05 & indXZM1Y$est<0,"(-)",""))))
  allZVals <- as.data.frame(paste0(BZ_YM$est,"(",BZ_YM$se,")",
                            ifelse(BZ_YM$pvalue<.05,"*",""),
                            ifelse(indX1MZY$pvalue<.05 & indX1MZY$est>0,"(+)",
                            ifelse(indX1MZY$pvalue<.05 & indX1MZY$est<0,"(-)",""))))


  summary <- data.frame(X,Expect[".id"],Y,allZExps,allZVals)
  colnames(summary) <- c("X","M","Y","BZ_MX","BZ_YM")

if(all == F){
  out<-list(BZ_YM,BZ_MX,B1_YM,B1_MX,indX1MZY,indXZM1Y,paths,paths2,summary)
  names(out)<-c("BZ_YM","BZ_MX","B1_YM","B1_MX","indX1MZY","indXZM1Y","paths","paths2","summary")
}
  if(all == T){
    out<-list(BZ_YM,BZ_MX,B1_YM,B1_MX,indX1MZY,indXZM1Y,paths,paths2,summary,x)
    names(out)<-c("BZ_YM","BZ_MX","B1_YM","B1_MX","indX1MZY","indXZM1Y","paths","paths2","summary","allResults")
  }
  return(out)

}
