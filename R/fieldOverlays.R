#' Field Overlays for Different Factors
#' @description This function will split force moderators
#' that have been specified and estimated within a lavaan model
#' (from a previous \code{fit <- sem(model,data)} statement) into separate
#' field matrices detailing what forces should be added to the base
#' diagram when the moderator variable has a level of '1' (often: 1=TRUE)
#' @param fitPars can provide as \code{summary(fit)$pe} statement in lavaan
#' @param fmatrix details the elements comprising the field matrix
#' @param sigOnly should only the significant associations be kept?  Defaults to \code{FALSE}
#' @return Will return \code{Fk} list of forces added by each moderator that was specified within lavaan model
#' @details The moderators should NOT be contained within the field matrix,
#' and within the lavaan model, moderators should be specified as y ~ x:mod rather
#' than as y ~ mod:x (as this function assigns what follows ':' as
#' the moderator variable).
#'
#' Generally, the moderator variables should be scaled in such a manner that
#' 0 is a meaningful point for all moderators, as this will make the base model
#' (where all moderators = 0) more meaningful.  This can be facilitated by having
#' moderator variables by dummy-coded (values of 0 or 1), effect coded (values of -1 or 1), or
#' standardized (M=0,SD=1).  See Cohen, Cohen, West, and Aiken (2003), Chapters 8 and 9.
#'
#' @export

fieldOverlays  <- function(fitPars, fmatrix, sigOnly = F) {
  colnames(fitPars)[1:3]<-c("Y","op","X")
  #limit params to regression elements
  fitPars<-subset(fitPars, op == "~")

  #split X:Moderator regression elements into separate X and Moderator columns
  modSet<-as.data.frame(stringr::str_split(fitPars$X, ":", n = 2, simplify = T))
  colnames(modSet)<-c("X","mod")

  #simpler parameter table
  fitPars2 <- data.frame(fitPars[1],modSet,round(fitPars[,c(7,8,10)],3))

  #can restrict to only significant parameters if you like
  if(sigOnly == T){
  fitPars2 <- subset(fitPars2, pvalue < .05)
  }

  #split different moderators into different lists
  params3<-plyr::dlply(fitPars2, "mod", function(x) x)

  #function to blast separate parameters into separate field matrices
  matrify <- function(fmatrix,x){
    for(i in 1:nrow(x)) {
      fmatrix[x[i,"X"],x[i,"Y"]] <- x[i,"est"]
    }
    return(fmatrix)
  }

  #create field matrices detailing forces added for each factor k
  Fk<-  plyr::llply(params3,function(x) matrify(fmatrix,x))
  names(Fk)[[1]]<-"m0"
  return(Fk)
  }
