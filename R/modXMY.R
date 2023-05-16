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
#' @return Will return the main effects of forces along the mediational pathway, and the z-moderation of each force
#' @details This adjusts for degrees of freedom by using 'p' as a clustering variable in lavaan
#'
#' @export

modXMY <- function(data,X,Y,Z,jSet,all=F) {

ModMedModel <- '
Y ~ 1 + Z + B1_YX*X + BZ_YX*X:Z
Y ~ B1_YM*M + BZ_YM*M:Z
M ~ 1 + Z + B1_MX*X + BZ_MX*X:Z
indX1MZY := B1_MX*BZ_YM
indXZM1Y := BZ_MX*B1_YM
'

parView<-function(fit,split = "est",dec = 2){
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

  paths<-cbind(X,B1_MX["est"],paste0(BZ_MX[,"est"],"*(Z_",Z,")"),BZ_MX[,".id"],B1_YM["est"],paste0(BZ_YM[,"est"],"*(Z_",Z,")"),Y,ifelse(indXZM1Y[,"pvalue"]<.05,"*"," "),ifelse(indX1MZY[,"pvalue"]<.05,"*"," "))
  colnames(paths) <- c("X","b1_MX","bZ_MX","M","b1_YX","bZ_YX","Y","bZ_MX*b1_YM","b1_MX*bZ_YM")
if(all == F){
  out<-list(BZ_YM,BZ_MX,B1_YM,B1_MX,indX1MZY,indXZM1Y,paths)
  names(out)<-c("BZ_YM","BZ_MX","B1_YM","B1_MX","indX1MZY","indXZM1Y","paths")
}
  if(all == T){
    out<-list(BZ_YM,BZ_MX,B1_YM,B1_MX,indX1MZY,indXZM1Y,paths,x)
    names(out)<-c("BZ_YM","BZ_MX","B1_YM","B1_MX","indX1MZY","indXZM1Y","paths","allResults")
  }
  return(out)

}
