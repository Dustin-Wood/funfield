#' Test of Condition Differences across Situation-Action (s,i) Pairs
#' @description Test whether a (dichotomous) condition produces mean-level
#' differences in how people rate situation-action expected outcomes
#' @param PSIdata.cond PSI-structured datafile, with "condition" as the final
#' @param hiCond exact name of 'high' condition (to be scored as '1')
#' @param loCond exact name of 'low' condition (to be scored as '-1')
#' @param locEE location of expected outcome ratings to test for condition
#' associations - usually something like \code{c(4:8)},
#' as first three columns should be \code{p,s,i}
#' @return Means, correlations and p-values linking expected outcome
#' ratings to high/low condition levels
#' \item{siMeans}{Situation-Action mean ratings, given in separate dataframe by condition}
#' \item{rXCond}{correlation between expected effect (X) and condition (L=-1,H=1)}
#' \item{pXCond}{statistical significance of these correlations (or mean difference, which is equivalent, btw)}
#' @details This assumes a very regimented c("p","s","i",...) data structure.
#' Additionally, the code is currently only for dichotomous condition levels.  If a
#' condition has more than 2 factor levels, then a different function will have to be
#' employed (perhaps using 'aov' routine)
#' @export



conditionTest <- function(PSIdata.cond, hiCond, loCond, locEE){
  #remove any other labels that might be in 'condition' variable
  PSIdata.HL <- subset(PSIdata.cond, (condition == hiCond | condition == loCond))

  #estimate means by (s,i,H/L)
  siMeans.HL <- ddply(PSIdata.HL, .(condition,s,i), function(x) colMeans(x[locEE], na.rm = T))
  #put (s,i) means for High and Low groups into separate dataframes
  siMeans <- dlply(siMeans.HL, .(condition), function(x) x[-1])

  #recode 'low' and 'high' conditions into -1 and 1, respectively, and make numeric variable
  PSIdata.HL$condition[PSIdata.HL$condition == hiCond] <- 1
  PSIdata.HL$condition[PSIdata.HL$condition == loCond] <- -1
  PSIdata.HL$condition <- as.numeric(PSIdata.HL$condition)

  #test all of the differences for significance all at once, within each (s,i) pair
  test.r<-ddply(PSIdata.HL, .(s,i), function(x) corr.test(x$condition,x[locEE])$r)
  test.p<-ddply(PSIdata.HL, .(s,i), function(x) corr.test(x$condition,x[locEE])$p)

  results <- list(siMeans, test.r, test.p)
  names(results) <- c("siMeans","rXCond","pXCond")
  return(results)

}
