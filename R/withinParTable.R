#' Within-Person N Adjustment for ParTable
#' @description The standard 'parTable' in lavaan does not provide correct degrees of
#' freedom when working with a deviation-score data file; this corrects that
#' @param fit standard output from an sem(...) statement in lavaan
#' @param data original 'long' format data frame that was used in sem(...) statement
#' @return parTable in which se, z, and p statistics are adjusted to person level
#' @details (This may become outdated as procedures for doing multilevel analyses with field
#' models become further developed.  It should also substantially underestimate significance, I believe
#' if 3 or more observations are within-person)
#' @export

withinParTable <- function(fit, data) {
  #adjust sig information for artificial inflation (due to general sample-size doubling)
  fit@ParTable$se.adj <- fit@ParTable$se * sqrt(nrow(data)) /sqrt(length(table(data$p)))
  fit@ParTable$z.adj <- fit@ParTable$est/fit@ParTable$se.adj
  fit@ParTable$p.adj=2*pnorm(-abs(fit@ParTable$z.adj))

  #view reduced output, with correct p-values
  wParTable <-(cbind(as.data.frame(fit@ParTable[c(1:4)]),as.data.frame(round(as.data.frame(fit@ParTable[-c(1:13)]),3))))
  return(wParTable)
}
