#' Variable Shapes
#' @description This is mainly used to run within the \code{prepField} function
#' @param vtype Variable type (short codes)
#'
#' @export

vshapes <- function(vtype) {
  vshapes <- vtype
#main types
  vshapes<-gsub("\\<d\\>","rfTriangle",vshapes) #do/decide
  vshapes<-gsub("\\<o\\>","diamond2",vshapes)   #object
  vshapes<-gsub("\\<x\\>","rectangle",vshapes)     #continuous variable
  vshapes<-gsub("\\<c\\>","circle",vshapes)     #choice/chance point
  vshapes<-gsub("\\<a\\>","lfTriangle",vshapes) #appraisal
#additional types
  vshapes<-gsub("\\<t\\>","lfTriCircle",vshapes) #appraisal+choice, all in one!
  vshapes<-gsub("\\<v\\>","mdiamond",vshapes) #verbal message diamond (representing communication)
  vshapes<-gsub("\\<V\\>","Msquare",vshapes) #verbal message diamond (representing communication)
  vshapes<-gsub("\\<e\\>","eSquare",vshapes) #message diamond (representing communication)
  vshapes<-gsub("\\<y\\>","yfTriangle",vshapes) #yes vote
  vshapes<-gsub("\\<n\\>","nfTriangle",vshapes) #no vote
  return(vshapes)
}

#' Variable Sizes
#' @description This is mainly used to run within the \code{prepField} function
#' @param vtype Variable type (short codes)
#'
#' @export

vsizes <- function(vtype) {
  vsizes <- vtype
#main types
  vsizes<-gsub("d",1.2,vsizes,fixed=T)
  vsizes<-gsub("o",1.2,vsizes,fixed=T)
  vsizes<-gsub("x",1,vsizes,fixed=T)
  vsizes<-gsub("c",1,vsizes,fixed=T)
  vsizes<-gsub("a",1.2,vsizes,fixed=T)
#additional types
  vsizes<-gsub("y",1.2,vsizes,fixed=T)
  vsizes<-gsub("n",1.2,vsizes,fixed=T)
  vsizes<-gsub("t",1.2,vsizes,fixed=T)
  vsizes<-gsub("v",1.2,vsizes,fixed=T)
  vsizes<-gsub("V",1.2,vsizes,fixed=T)
  vsizes<-gsub("e",1.2,vsizes,fixed=T)
  vsizes<-as.numeric(vsizes)
  return(vsizes)
}
