#' Test of Condition Differences across Situation-Action (s,i) Pairs
#' @description Test whether a (dichotomous) condition produces mean-level
#' differences in how people rate situation-action expected outcomes
#' @param PSIdata.cond PSI-structured datafile, with "condition" as the final
#' @param hiCond exact name of 'high' condition (to be scored as '1')
#' @param loCond exact name of 'low' condition (to be scored as '-1')
#' @param locEE location of expected outcome ratings to test for condition
#' associations - usually something like \code{c(4:8)},
#' as first three columns should be \code{p,s,i}
#' @return Correlations and p-Values linking expected outcome
#' ratings to high/low condition levels
#' @usage ecov(data,fX,fY,dXdY="cov")
#' @details This assumes a very regimented c("p","s","i",...) data structure.
#' Additionally, the code is currently only for dichotomous condition levels.  If a
#' condition has more than 2 factor levels, then a different function will have to be
#' employed (perhaps using 'aov' routine)
#' @export


conditionTest <- function(PSIdata.cond, hiCond, loCond, locEE){
  PSIdata.HL <- subset(PSIdata.cond, (condition == hiCond | condition == loCond))

  #recode 'subordinate' and 'manager' into -1 and 1, respectively, and make numeric variable
  PSIdata.HL$condition[PSIdata.HL$condition == hiCond] <- 1
  PSIdata.HL$condition[PSIdata.HL$condition == loCond] <- -1
  PSIdata.HL$condition <- as.numeric(PSIdata.HL$condition)

  #test all of the differences for significance all at once
  test.r<-ddply(PSIdata.HL, .(s,i), function(x) corr.test(x$condition,x[locEE])$r)
  test.p<-ddply(PSIdata.HL, .(s,i), function(x) corr.test(x$condition,x[locEE])$p)

  results <- list(test.r, test.p)
  names(results) <- c("rXCond","pXCond")
  return(results)

}
