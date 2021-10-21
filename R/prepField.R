#' Prepare Functional Field
#' @description This function will create the layout for the functional field
#' matrix, and help to maintain consistency between the field matrix and
#' graphical elements of the model.  It will also specify the type of each
#' variable in the graph - especially whether it should be represented as an \emph{action},
#' \emph{object}, \emph{variable}, \emph{choice point}, or \emph{appraisal}.
#'
#' Variables in the functional field should be specified using a 'type_name' labeling convention.
#' Please see the \strong{**as yet unwritten help file**} for further details on preparing
#' a matrix layout.
#' @param matLayout Specified \emph{variable type} and \emph{variable name}
#' @details Current (main) variable types:
#'     \code{d} = decision or 'do' nodes (indicate initiating some sort of action)
#'     \code{o} = object nodes - should only take values of 1 or 0
#'     \code{c} = choice point / chance nodes
#'     \code{x} = continuous variables
#'     \code{a} = appraisal nodes - indicate where a decision will be made through a maximization or threshold rule
#' @return Various objects that will be useful for creating fields from a prespecified matrix
#' @export

prepField <- function(matLayout) {
  #prepares the functional field from a graphical layout which has the *nodeType*_*vname* layout

  ## renumber the nodes in their causal order as above
  numLayout <- matLayout

  #initialize some things
  vnames <- c()
  vtype <- c()
  nodeNum=1

  for (i in 1:ncol(matLayout)) {
    for (j in 1:nrow(matLayout)) {
      if(matLayout[j,i]!=0){
        numLayout[j,i]=nodeNum
        vnames<-rbind(vnames,substr(matLayout[j,i], 3, 10000))
        vtype<-rbind(vtype,substr(matLayout[j,i], 1, 1))
        matLayout[j,i]<-substr(matLayout[j,i], 3, 10000)
        nodeNum=nodeNum+1
      }
    }
  }

  flabels <- 1:(length(vnames))
  names(flabels)<-vnames

  vshapes <- vshapes(vtype) #this is now defined in a separate function

  #sizes are given in proportion to the sizing of x [standard square]
  vsizes <- vsizes(vtype)



  fmat <- matrix (0,length(vnames),length(vnames))
  rownames(fmat) <- vnames
  colnames(fmat) <- rownames(fmat)


  fprops <- list(matLayout,numLayout,vnames,vtype,vshapes,vsizes,flabels,fmat)
  names(fprops) <- c("matLayout","numLayout","vnames","vtype","vshapes","vsizes","flabels","fmat")

  return(fprops)
}

