#' Functional Field Builder
#'
#' @description Creates a functional field function from a concise rules
#'   function, handling the boilerplate of zero-matrix initialization and
#'   plan application.  The \code{ft[x, y] = expr(s)} assignment idiom is
#'   preserved entirely inside \code{rules} — \code{makeFF} only wraps the
#'   setup and teardown.
#'
#' @param rules A function \code{function(s, ft)} that populates the force
#'   matrix via assignment lines and returns \code{ft}.  Expressions in each
#'   cell can be arbitrarily complex: simple scalars, AND/OR products,
#'   regression terms, or any R expression involving \code{s}.
#'
#' @return A function \code{ff(s, plan = function(ft) ft)} with the standard
#'   interface expected by \code{expOutcomesFF}: takes the situation vector
#'   \code{s} and a plan function, returns \code{list(s = s, ft = ft)}.
#'
#' @examples
#' \dontrun{
#' ff <- makeFF(function(s, ft) {
#'   ft["Do(Power_CM)_p", "Power_CM"]     <- 1 * s["s1"]
#'   ft["Push(ON)_p",     "Have(Coffee)"] <- prod(s[c("Power_CM", "Water_CM")])
#'   ft
#' })
#' }
#'
#' @export
makeFF <- function(rules) {
  function(s, plan = function(ft) ft) {
    ft <- matrix(0, length(s), length(s))
    dimnames(ft) <- list(names(s), names(s))
    ft <- rules(s, ft)
    ft <- plan(ft)
    list(s = s, ft = ft)
  }
}
