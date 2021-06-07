#' Matrix Surgery
#' @description This function serves as a 'brute force' method for
#' replacing values in some matrix \code{x} with non-missing values of \code{y}.
#' @param x the original matrix is 'old' matrix representing function field model paths
#' @param y matrix with non-missing cells you wish to overwrite cells in x
#' @param suppressOverwrite do you wish reports of overwritten cells to be suppressed? Defaults to \code{TRUE}
#' @details Any cell in \code{y} given as a missing value (\code{NA}) will return the original cell in \code{x}
#'
#' Often, you will NOT want non-missing cells in \code{x} to be overwritten. (This might indicate some problem in the model.)
#' If so, you can specify \code{suppressOverwrite = F} to print when overwrites have occurred.
#' @return modified version of \code{x} overwritten with non-missing cells from \code{y}
#'
#' @export

surgery <- function(x, y, suppressOverwrite = T) {
  z <- x
  i=1
  j=1
  noverwrites=0
  for (i in 1:NROW(z)) {
    for (j in 1:NCOL(z)) {
      if(is.na(y[i,j])==F) {
        if(is.na(z[i,j])==F) {
          noverwrites = noverwrites + 1
        }
        z[i,j]<-y[i,j]
      }
    }
    j=1
  }
  if (suppressOverwrite == F) {
    if (noverwrites > 0) {
      print(paste("There were Overwrites: ",noverwrites)) # print the number of times a cell has been overwritten.  If > 0, then this could indicate a problem.
    }
  }
  return(z)
}

