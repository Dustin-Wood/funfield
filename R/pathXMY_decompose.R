#' Decompose a Z-moderated X-Y effect into expectation-, valuation-, and
#' direct-moderation components
#'
#' @description
#' For a moderator \code{Z} acting on the link from \code{X} to \code{Y},
#' two models are commonly of interest:
#'
#' \describe{
#'   \item{Model [1]}{The direct moderation \eqn{Y = b^1_{YX} X + b^Z_{YX} X
#'     \cdot Z + \dots}. \code{BZ_YX[1]} indexes the \emph{total} moderation
#'     effect — how much \eqn{Z} shifts the action-outcome link, without
#'     reference to any mediator.}
#'   \item{Model [2]}{The full mediated model \eqn{X \to M \to Y} with
#'     \eqn{Z} moderating all three paths.}
#' }
#'
#' These are related by the algebraic identity (applied to one mediator
#' at a time, in linear regression with cluster-robust SEs):
#'
#' \deqn{b^Z_{YX}[1] = b^Z_{MX} \cdot b^1_{YM} + b^1_{MX} \cdot b^Z_{YM} +
#' b^Z_{YX}[2]}
#'
#' \itemize{
#'   \item Term 1, \code{BZ_MX * B1_YM}: moderation that flows through
#'     \eqn{Z} changing the \emph{expectation} of \eqn{M} given \eqn{X}.
#'     This is what \code{pathXMY()} highlights by default.
#'   \item Term 2, \code{B1_MX * BZ_YM}: moderation that flows through
#'     \eqn{Z} changing the \emph{valuation} of \eqn{M} as a driver of
#'     \eqn{Y}, while leaving expectations untouched.
#'   \item Term 3, \code{BZ_YX[2]}: direct \eqn{Z}-moderation of the
#'     \eqn{X \to Y} link that does \emph{not} route through this mediator.
#' }
#'
#' Identifying which term carries most of \code{BZ_YX[1]} for a given
#' mediator answers a different psychological question than \code{pathXMY()}
#' alone. A person-trait moderator may leave expectations untouched
#' (\code{BZ_MX} near zero) yet substantially shift valuation
#' (\code{BZ_YM} large) — invisible in the \code{BZ_MX} table but central
#' to understanding why the trait moderates action.
#'
#' @param data A data frame in PSI long format with Level-1 columns already
#'   within-person deviated. See \code{\link{pathXMY}}.
#' @param X,Y,M,Z,Z.within,cluster,... Passed to \code{\link{pathXMY}}.
#'   \code{M} must be supplied (one or more mediator names); \code{Z} is
#'   required.
#'
#' @return A list with three elements:
#'   \describe{
#'     \item{total}{One-row data frame with the no-mediator (Model [1])
#'       estimates: \code{est}, \code{se}, \code{z}, \code{pvalue} for
#'       \code{BZ_YX[1]}.}
#'     \item{components}{Long tidy data frame, one row per (mediator,
#'       term). Terms: \code{"BZ_MX * B1_YM"} (expectation moderation),
#'       \code{"B1_MX * BZ_YM"} (valuation moderation), \code{"BZ_YX (direct)"}
#'       (residual direct moderation), and \code{"sum (1+2+3)"} (their
#'       algebraic sum; should approximate \code{BZ_YX[1]} for each
#'       single-mediator fit). The first three rows carry full SE/z/p
#'       columns; the sum row has \code{est} only.}
#'     \item{fits}{A list with \code{$direct} (Model [1]) and \code{$full}
#'       (the multi-mediator \code{pathXMY()} object).}
#'   }
#' @seealso \code{\link{pathXMY}}, \code{\link{plotPathXMY}}
#' @export
pathXMY_decompose <- function(data, X, Y, M, Z, Z.within = FALSE,
                              cluster = "p", ...) {
  if (is.null(M) || length(M) == 0L)
    stop("M is required: this function decomposes Z's moderation through M.")
  if (is.null(Z))
    stop("Z is required: this function is about decomposing Z-moderation.")

  ## Model [1]: direct X -> Y with Z moderation, no mediator
  fit1 <- pathXMY(data, X = X, Y = Y, M = NULL, Z = Z,
                  Z.within = Z.within, cluster = cluster, ...)
  ## Model [2]: full X -> M -> Y with Z on all paths
  fit2 <- pathXMY(data, X = X, Y = Y, M = M, Z = Z,
                  Z.within = Z.within, cluster = cluster, ...)

  t1 <- fit1$tidy_loop
  t2 <- fit2$tidy_loop

  total <- t1[t1$param == "BZ_YX",
              c("est","se","z","pvalue","ci.lower","ci.upper"), drop = FALSE]
  rownames(total) <- NULL

  ## Per mediator: pull the three components plus their algebraic sum
  one_med <- function(m) {
    s <- t2[t2$mediator == m, , drop = FALSE]
    pick <- function(p) {
      r <- s[s$param == p, , drop = FALSE]
      if (nrow(r) == 0L) data.frame(est = NA_real_, se = NA_real_,
                                    z = NA_real_, pvalue = NA_real_)
      else r[1, c("est","se","z","pvalue"), drop = FALSE]
    }
    t1_term <- pick("BZ_MX * B1_YM")
    t2_term <- pick("B1_MX * BZ_YM")
    t3_term <- pick("BZ_YX")
    sum_est <- t1_term$est + t2_term$est + t3_term$est

    data.frame(
      mediator = m,
      term     = c("BZ_MX * B1_YM", "B1_MX * BZ_YM",
                   "BZ_YX (direct)", "sum (1+2+3)"),
      est      = c(t1_term$est, t2_term$est, t3_term$est, sum_est),
      se       = c(t1_term$se,  t2_term$se,  t3_term$se,  NA_real_),
      z        = c(t1_term$z,   t2_term$z,   t3_term$z,   NA_real_),
      pvalue   = c(t1_term$pvalue, t2_term$pvalue, t3_term$pvalue, NA_real_),
      stringsAsFactors = FALSE
    )
  }
  components <- do.call(rbind, lapply(M, one_med))
  rownames(components) <- NULL

  list(
    total      = total,
    components = components,
    fits       = list(direct = fit1, full = fit2)
  )
}
