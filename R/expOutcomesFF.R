#' Expected Outcomes from Situation Interacting with Functional Field
#' @description Estimate expected outcomes of current situation
#' given a particular functional field (which details how
#' situational factors determine the levels of forces in the environment)
#' @param s situation vector: 'situation states at a given time'
#' @param i action definition - specify the FEATURES you wish to set to value of 1 ("TRUE" or "present")
#' @param ffield functional - contains the 'belief system' or 'model' of how force levels are produced
#' @param plan specify forces to be added to the functional field
#' @param xS extra situational features that are NOT graphed that can add forces to the field
#' @return 'Situation log' - detailing \code{sit} level of situational forces,
#'  \code{dit} new disturbances, and \code{fit} structure of the field at time \code{t}
#'
#' @export
expOutcomesFF <- function(s,i=0,ffield=ff,plan=DoNothing){
  d<-s*0; d[i]<-1 #d = disturbance (in first round: usually from action)
  sit <- matrix(0,(length(s)+1),length(s)); colnames(sit)<-names(s)
  sit[1,] <- s+d
  dit <- matrix(0,(length(s)+1),length(s)); colnames(dit)<-names(s)
  dit[1,] <- d
  fit <- vector("list", (length(s)+1))
  fit[[1]]<-ffield(sit[1,],plan=plan)
  for(i in 1:(nrow(sit)-1)){
    fit[[i]]<-ffield(sit[i,],plan=plan)
    dit[i+1,] <- dit[i,]%*%fit[[i]]$ft
    sit[i+1,] <- sit[i,]+dit[i+1,]
    #if(sum(abs(sitlog$dit[i,]))==0) {i=nrow(sit)-1} #initial attempt to get loop to stop if nothing new is computed
  }
  fit[[length(s)]]<-ffield(sit[length(s),])

  sitlog <- list(sit,dit,fit)
  names(sitlog) <- list("sit","dit","fit")

  qF <- sitlog$fit[[i]]$ft
  #detail sources of forces
    #TBD: this is currently missing script for creating 'afforded' paths
  present <- qF; present[present==0]<-NA; present[present!=0]<-"afforded";
    qFactors<-surgery(qF,present)
  plani <-qF;   plani[,]<-NA; plani<-plan(plani); plani[plani==1]<-"plani";
    qFactors<-surgery(qFactors,plani)
  #color-code nodes by sources of forces
  graphColor <- qFactors;
    graphColor[graphColor=="potential"]<-"gray91";
    graphColor[graphColor=="afforded"]<-"black";
    graphColor[graphColor=="plani"]<-"limegreen"
  sitlog$graphColor <- graphColor
  return(sitlog)
}

