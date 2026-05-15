#' Field-style path diagram for an X-M-Y model
#'
#' @description
#' Renders the X to M to Y structure using the funfield visualization
#' conventions:
#'
#' \itemize{
#'   \item \strong{Nodes} are colored by their expected score when X is
#'     held at 1: positive scores in blue, negative scores in red, with
#'     gradient intensity proportional to magnitude. The X node is by
#'     definition 1 (full blue); each mediator's score is its path
#'     coefficient \eqn{b^1_{MX}}; Y's score is the sum of the direct and
#'     indirect contributions, \eqn{b^1_{YX} + \sum_j b^1_{YM_j} \cdot
#'     b^1_{MX_j}}.
#'   \item \strong{Shape} is rectangle for X and each mediator (continuous
#'     variables) and a left-facing triangle for Y (the focal action
#'     likelihood).
#'   \item \strong{Edges} are solid for positive paths and dashed for
#'     negative paths (\code{negDashed = TRUE}); edge thickness scales
#'     with path magnitude. Edge labels carry the \code{b1 + bZ(Z)}
#'     decomposition.
#' }
#'
#' Two view modes:
#' \itemize{
#'   \item \strong{Triangle} (single mediator) — X at bottom-left, M at
#'     top, Y at bottom-right. Shows the direct X to Y arrow and both
#'     mediated paths.
#'   \item \strong{Fan} (multiple mediators) — X at left, mediators
#'     stacked in the middle, Y at right. The direct X to Y arrow is
#'     omitted because each single-mediator fit gives a different residual
#'     direct effect.
#' }
#'
#' @param x A \code{\link{pathXMY}} or \code{\link{pathXMY_decompose}} return.
#' @param mediator Character vector. If \code{NULL} (default) and \code{x}
#'   has multiple mediators, a fan diagram of all of them is drawn. Pass
#'   a single name for the triangle view. Pass a subset for a smaller fan.
#' @param X_label,Y_label,Z_label Display labels for the X, Y, and Z nodes.
#' @param M_labels Optional character vector of labels for the mediators,
#'   parallel to \code{mediator} (or to the natural order in the fit).
#'   If \code{NULL}, the mediator variable names are used.
#' @param digits Decimal places for edge labels (default 2).
#' @param show_pvalues Logical; append p-values to edge labels.
#' @param scale_max qgraph \code{maximum} argument controlling edge
#'   thickness scaling. Default 0.8 (paths approaching this magnitude get
#'   maximum thickness).
#' @param score_intensity_max Numeric. The expected-score absolute value
#'   that maps to maximum color intensity. Default 1 (i.e. expected scores
#'   range over [-1, 1]).
#' @param title Optional plot title.
#' @param filename,filetype Optional; if \code{filename} is supplied,
#'   qgraph writes the plot to a file (e.g., \code{filetype = "png"}).
#' @param ... Additional arguments passed to \code{qgraph::qgraph()}.
#' @return Invisibly returns the qgraph object.
#' @seealso \code{\link{pathXMY}}, \code{\link{pathXMY_decompose}},
#'   \code{\link{vshapes}}, \code{\link{fieldPolygons}}, \code{\link{pnLevels}}
#' @export
plotPathXMY <- function(x,
                        mediator = NULL,
                        X_label = "X", Y_label = "Y", Z_label = "Z",
                        M_labels = NULL,
                        digits = 2,
                        show_pvalues = FALSE,
                        scale_max = 0.8,
                        score_intensity_max = 1,
                        title = NULL,
                        filename = NULL,
                        filetype = "png",
                        ...) {
  if (!requireNamespace("qgraph", quietly = TRUE))
    stop("plotPathXMY() requires the 'qgraph' package.")

  ## Accept either pathXMY or pathXMY_decompose
  tidy <- if (!is.null(x$fits) && !is.null(x$fits$full)) x$fits$full$tidy
          else if (!is.null(x$tidy))                      x$tidy
          else stop("`x` must be a pathXMY() or pathXMY_decompose() return.")

  all_meds <- unique(tidy$mediator)
  all_meds <- all_meds[!is.na(all_meds)]
  if (length(all_meds) == 0L) stop("No mediator found in the fit.")
  if (is.null(mediator)) mediator <- all_meds
  if (!all(mediator %in% all_meds))
    stop("Mediators not found in fit: ",
         paste(setdiff(mediator, all_meds), collapse = ", "))

  n_m       <- length(mediator)
  fan_view  <- n_m > 1L
  if (is.null(M_labels)) M_labels <- mediator

  ## Coefficient pulls
  pull <- function(med, param) {
    r <- tidy[tidy$mediator == med & tidy$param == param, , drop = FALSE]
    if (nrow(r) == 0L) list(est = NA_real_, pvalue = NA_real_)
    else list(est = r$est[1], pvalue = r$pvalue[1])
  }
  pull_est <- function(med, param) pull(med, param)$est

  B1_MX <- vapply(mediator, pull_est, numeric(1), param = "B1_MX")
  BZ_MX <- vapply(mediator, pull_est, numeric(1), param = "BZ_MX")
  B1_YM <- vapply(mediator, pull_est, numeric(1), param = "B1_YM")
  BZ_YM <- vapply(mediator, pull_est, numeric(1), param = "BZ_YM")

  ## Direct paths only meaningful in single-mediator fit
  has_direct <- !fan_view
  B1_YX <- if (has_direct) pull_est(mediator[1], "B1_YX") else NA_real_
  BZ_YX <- if (has_direct) pull_est(mediator[1], "BZ_YX") else NA_real_

  ## Expected scores at X = 1
  X_score   <- 1
  M_scores  <- B1_MX
  Y_score   <- sum(B1_YM * B1_MX, na.rm = TRUE) +
               (if (has_direct && !is.na(B1_YX)) B1_YX else 0)

  ## Node assembly
  node_names  <- c("X", mediator, "Y")
  node_labels <- c(X_label, M_labels, Y_label)
  n_nodes     <- length(node_names)
  scores_vec  <- c(X_score, M_scores, Y_score)

  ## Adjacency matrix of path weights
  adj <- matrix(0, n_nodes, n_nodes,
                dimnames = list(node_names, node_names))
  for (i in seq_along(mediator)) {
    adj["X", mediator[i]] <- B1_MX[i]
    adj[mediator[i], "Y"] <- B1_YM[i]
  }
  if (has_direct && !is.na(B1_YX)) adj["X", "Y"] <- B1_YX

  ## Edge-label matrix in b1 + bZ(Z) format
  fmt_path <- function(b1, bZ, pv = NA_real_) {
    if (is.na(b1)) return("")
    out <- sprintf(paste0("%+.", digits, "f"), b1)
    if (!is.na(bZ) && abs(bZ) > 0)
      out <- paste0(out, " ",
                    sprintf(paste0("%+.", digits, "f(%s)"), bZ, Z_label))
    if (show_pvalues && !is.na(pv))
      out <- paste0(out, "\np=", sprintf("%.3f", pv))
    out
  }

  elab <- matrix("", n_nodes, n_nodes,
                 dimnames = list(node_names, node_names))
  for (i in seq_along(mediator)) {
    p_MX <- pull(mediator[i], "B1_MX")$pvalue
    p_YM <- pull(mediator[i], "B1_YM")$pvalue
    elab["X", mediator[i]] <- fmt_path(B1_MX[i], BZ_MX[i], p_MX)
    elab[mediator[i], "Y"] <- fmt_path(B1_YM[i], BZ_YM[i], p_YM)
  }
  if (has_direct && !is.na(B1_YX)) {
    p_YX <- pull(mediator[1], "B1_YX")$pvalue
    elab["X", "Y"] <- fmt_path(B1_YX, BZ_YX, p_YX)
  }

  ## Layout
  layout_mat <- matrix(0, n_nodes, 2,
                       dimnames = list(node_names, NULL))
  if (fan_view) {
    layout_mat["X", ] <- c(-1.5, 0)
    layout_mat["Y", ] <- c( 1.5, 0)
    y_pos <- if (n_m == 1L) 0 else seq(1, -1, length.out = n_m)
    for (i in seq_along(mediator))
      layout_mat[mediator[i], ] <- c(0, y_pos[i])
  } else {
    layout_mat["X", ] <- c(-1.0, -0.6)
    layout_mat[mediator, ] <- c( 0.0,  0.8)
    layout_mat["Y", ] <- c( 1.0, -0.6)
  }

  ## Shapes: rectangles for X and Ms, left-facing triangle for Y
  shape_codes <- c("x", rep("x", n_m), "a")  # 'a' = appraisal -> lfTriangle
  shapes      <- vshapes(shape_codes)

  ## Color groupings and intensity scores
  groups     <- pnLevels(scores_vec)
  scores_int <- as.integer(round(pmin(abs(scores_vec) / score_intensity_max, 1) * 20))

  ## qgraph call
  qargs <- list(
    input          = adj,
    shape          = shapes,
    polygonList    = fieldPolygons(),
    groups         = groups,
    color          = c("dodgerblue", "red"),
    scores         = scores_int,
    scores.range   = c(0, 20),
    edge.color     = "black",
    edge.labels    = elab,
    edge.label.cex = 0.95,
    negDashed      = TRUE,
    maximum        = scale_max,
    layout         = layout_mat,
    labels         = node_labels,
    legend         = FALSE,
    fade           = FALSE,
    label.scale    = FALSE,
    label.cex      = 0.85,
    title          = title
  )
  if (!is.null(filename)) {
    qargs$filename <- filename
    qargs$filetype <- filetype
  }
  qargs <- c(qargs, list(...))

  q <- do.call(qgraph::qgraph, qargs)
  invisible(q)
}
