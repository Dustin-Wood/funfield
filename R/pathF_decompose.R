#' Decompose total and Z-moderated X-Y effects across a path model's routes
#'
#' @description
#' Generalizes \code{\link{pathXMY_decompose}} from the single-mediator
#' triangle to arbitrary \code{\link{pathF}} models. Two models are fit:
#'
#' \describe{
#'   \item{Total model}{The bare \eqn{X \to Y} regression (with \eqn{X
#'     \times Z} moderation when \code{Z} is supplied), giving the total
#'     effect \eqn{F_1^{*}[X,Y]} and total moderation \eqn{F_Z^{*}[X,Y]}.}
#'   \item{Path model}{The full \code{pathF()} model specified by
#'     \code{order} or \code{paths}, in which the total decomposes across
#'     every directed \eqn{X \to Y} route.}
#' }
#'
#' The baseline identity generalizes the Baron-Kenny sum: the total
#' \eqn{F_1^{*}[X,Y]} equals the sum over routes of each route's product of
#' \eqn{F_1} edges, plus the residual direct \eqn{F_1[X,Y]} (exact for the
#' saturated cascade, where the path model reproduces the OLS algebra).
#'
#' The moderation identity generalizes the Muller-Judd-Yzerbyt (2005)
#' decomposition: for each route, \eqn{Z}'s moderation "slides" through the
#' chain, contributing one first-order term per edge position --- the
#' moderated edge's \eqn{F_Z} times the \eqn{F_1} of every other edge on the
#' route. For a two-mediator chain \eqn{X \to M_1 \to M_2 \to Y}:
#'
#' \deqn{F_Z^{*}[X,Y] \approx F_Z[X,M_1] F_1[M_1,M_2] F_1[M_2,Y]
#'   + F_1[X,M_1] F_Z[M_1,M_2] F_1[M_2,Y]
#'   + F_1[X,M_1] F_1[M_1,M_2] F_Z[M_2,Y]
#'   + F_Z[X,Y]}
#'
#' summed over every route when the DAG has several. The identity is
#' first-order: substituting the moderated equations through the chain
#' produces \eqn{Z^2, Z^3, \dots} cross-terms (products of two or more
#' \eqn{F_Z} coefficients) that the decomposition drops, so the approximation
#' can loosen as chains lengthen. The \code{gap} rows in the output make the
#' approximation quality explicit.
#'
#' @param data A data frame in PSI long format (see \code{\link{pathF}}).
#' @param order,paths The path structure, passed to \code{\link{pathF}};
#'   supply exactly one.
#' @param X,Y Optional focal action / outcome (defaults resolved by
#'   \code{pathF()} from \code{order} or the DAG's unique source/sink).
#' @param Z Optional moderator. When \code{NULL} only the baseline
#'   (\eqn{F_1}) decomposition is returned.
#' @param Z.within,cluster,... Passed to \code{\link{pathF}}.
#'
#' @return A list with three elements:
#'   \describe{
#'     \item{total}{Data frame with the no-mediator total-model estimates:
#'       one row for \code{f1*} and (when \code{Z} is supplied) one for
#'       \code{fZ*}.}
#'     \item{components}{Long tidy data frame with a \code{block} column
#'       (\code{"f1"} or \code{"fZ"}) and a \code{term} column. Within each
#'       block: one row per route product (for \code{"fZ"}, one row per
#'       route x edge-position slide term), the residual direct path, their
#'       algebraic \code{sum}, the no-mediator \code{total}, and the
#'       \code{gap} (total - sum) indexing approximation quality. Route and
#'       direct rows carry full SE/z/p columns; sum/total/gap rows carry
#'       \code{est} only (total also carries its SE).}
#'     \item{fits}{A list with \code{$direct} (total model) and \code{$full}
#'       (the \code{pathF()} object).}
#'   }
#'
#' @seealso \code{\link{pathF}}, \code{\link{pathXMY_decompose}}
#' @export
pathF_decompose <- function(data, order = NULL, paths = NULL,
                            X = NULL, Y = NULL, Z = NULL,
                            Z.within = FALSE, cluster = "p", ...) {

  ## Model [2]: the full path model (fit first so X/Y resolve from the spec)
  full <- pathF(data, order = order, paths = paths, X = X, Y = Y,
                Z = Z, Z.within = Z.within, cluster = cluster, ...)
  Xv <- full$spec$X
  Yv <- full$spec$Y
  if (is.na(Xv) || is.na(Yv)) {
    stop("X and Y could not be resolved from the path structure; ",
         "supply them explicitly.", call. = FALSE)
  }

  ## Model [1]: bare X -> Y total model (single-edge pathF)
  tot <- pathF(data, paths = sprintf("%s ~ %s", Yv, Xv), X = Xv, Y = Yv,
               Z = Z, Z.within = Z.within, cluster = cluster, ...)

  f1_lab <- sprintf("f1_%s_%s", Xv, Yv)
  fZ_lab <- sprintf("fZ_%s_%s", Xv, Yv)
  has_Z  <- !is.null(Z)

  tt <- tot$tidy
  tcols <- c("est", "se", "z", "pvalue", "ci.lower", "ci.upper")
  total <- rbind(
    data.frame(param = paste0("f1*_", Xv, "_", Yv),
               tt[tt$param == f1_lab, tcols, drop = FALSE]),
    if (has_Z)
      data.frame(param = paste0("fZ*_", Xv, "_", Yv),
                 tt[tt$param == fZ_lab, tcols, drop = FALSE])
  )
  rownames(total) <- NULL

  ft <- full$tidy
  is_prod <- grepl(" * ", ft$param, fixed = TRUE)
  has_fZ  <- grepl("(^|\\* )fZ_", ft$param)

  pick <- function(rows) rows[, c("term", "est", "se", "z", "pvalue")]
  as_term <- function(d) { names(d)[names(d) == "param"] <- "term"; d }

  block <- function(blk, route_rows, direct_lab, total_est, total_se) {
    routes <- as_term(ft[route_rows,
                         c("param", "est", "se", "z", "pvalue"), drop = FALSE])
    direct <- as_term(ft[ft$param == direct_lab,
                         c("param", "est", "se", "z", "pvalue"), drop = FALSE])
    direct$term <- paste0(direct_lab, " (direct)")
    sum_est <- sum(routes$est, direct$est)
    extra <- data.frame(
      term   = c("sum (routes + direct)", "total (no-mediator)",
                 "gap (total - sum)"),
      est    = c(sum_est, total_est, total_est - sum_est),
      se     = c(NA_real_, total_se, NA_real_),
      z      = NA_real_,
      pvalue = NA_real_,
      stringsAsFactors = FALSE
    )
    out <- rbind(pick(routes), pick(direct), extra)
    data.frame(block = blk, out, row.names = NULL, stringsAsFactors = FALSE)
  }

  comp <- block("f1", which(is_prod & !has_fZ), f1_lab,
                total$est[1], total$se[1])
  if (has_Z) {
    comp <- rbind(comp,
                  block("fZ", which(is_prod & has_fZ), fZ_lab,
                        total$est[2], total$se[2]))
  }
  rownames(comp) <- NULL

  list(total      = total,
       components = comp,
       fits       = list(direct = tot, full = full))
}
