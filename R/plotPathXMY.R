#' Field-style path diagram for an X-M-Y model
#'
#' @description
#' Renders the X to M to Y structure as a \pkg{ggplot2} object using the
#' funfield visualization conventions:
#'
#' \itemize{
#'   \item \strong{Nodes} are colored by their expected score when X is
#'     held at 1, on a continuous diverging gradient (deep red to white
#'     to deep blue, midpoint at 0) mapped to the \code{[-1, 1]} range
#'     via \code{ggplot2::scale_fill_gradient2()}. The X node is by
#'     definition 1 (full blue); each mediator's score is its path
#'     coefficient \eqn{b^1_{MX}}; Y's score is the sum of the direct
#'     and indirect contributions,
#'     \eqn{b^1_{YX} + \sum_j b^1_{YM_j} \cdot b^1_{MX_j}}.
#'   \item \strong{Shape} defaults to a true square for X and each
#'     mediator (continuous variables) and a left-facing triangle for Y
#'     (the focal action likelihood). \code{X_shape} and \code{Y_shape}
#'     override this: use \code{X_shape = "rtTri"} when X is itself an
#'     imagined action (e.g. the speeding / overtime EXSJTs), and
#'     \code{Y_shape = "square"} when Y is a regular continuous variable
#'     rather than an action likelihood. Mediators are always squares.
#'   \item \strong{Labels} sit outside the shapes: above for any
#'     mediator (M), below for X and Y. This convention scales to the
#'     fan view.
#'   \item \strong{Edges} are solid for positive paths and dashed for
#'     negative paths. Linewidth and edge color both scale with path
#'     magnitude (capped at \code{scale_max}): the largest paths
#'     render thick and near-black, and paths with coefficients near
#'     zero fade to a thin, near-white line so they recede into the
#'     background. Arrowheads are clipped to the node perimeter via
#'     shape-aware intersection (L-infinity for squares, edge
#'     intersection for the triangles). Edge labels carry the
#'     \code{f1 + fZ(Z)} decomposition.
#'   \item \strong{Moderator (Z) coloring.} Anything that is purely a
#'     bZ coefficient renders in gold (\code{#F6BE00}), matching the
#'     condition-node color used in the deductive \code{plotField()}
#'     diagrams. In the default decomposed label, only the trailing
#'     \code{bZ(Z)} fragment is gold (the b1 portion and the edge itself
#'     describe the normative path). In the Z-overlay view
#'     (\code{Z_overlay = TRUE}), the entire label and the edge are gold,
#'     since the edge \emph{is} a bZ coefficient. A Z-collapsed view
#'     (\code{Z_value} supplied) renders normally: the label there is a
#'     single total slope \code{f1 + fZ * Z_value}, not a bZ coefficient
#'     on its own.
#' }
#'
#' Two view modes:
#' \itemize{
#'   \item \strong{Triangle} (single mediator) -- X at bottom-left, M at
#'     top, Y at bottom-right.
#'   \item \strong{Fan} (multiple mediators) -- X at left, mediators
#'     stacked in the middle, Y at right. With an odd number of
#'     mediators the stack is laid out asymmetrically so the straight
#'     X to Y arrow has a clean gap at the y = 0 axis.
#' }
#'
#' \code{from} controls which fit drives the diagram:
#' \itemize{
#'   \item \code{"loop"} (default): per-mediator loop overlay. In fan
#'     view the overlay is intentionally pure -- no residual direct
#'     X to Y arrow is drawn, since the only sensible direct path in
#'     that picture would have to come from a different (joint) fit.
#'     In single-mediator triangle view, the X to Y direct path is
#'     that one mediator's loop residual.
#'   \item \code{"joint"}: the joint multi-mediator simultaneous-fit
#'     picture -- each mediator's slopes are the partial slopes net of
#'     the other mediators, and the X to Y arrow is the single global
#'     \code{f1_XY_joint} direct path after controlling for all M.
#' }
#'
#' Passing \code{Z_overlay = TRUE} renders the same layout but with
#' each B1 coefficient swapped for its BZ counterpart -- a clean view
#' of the moderator's per-unit effect on path weights, isolated from
#' the normative field. Nodes are drawn white in that view since
#' expected scores have no meaning for a per-unit-Z slope.
#'
#' Passing \code{route = "expectation"} or \code{"valuation"} renders
#' a per-route view of the moderation. The \emph{expectation route}
#' shows \code{fZ_XM} on the X-to-M arm (gold) and \code{f1_MY} on the
#' M-to-Y arm (black); the \emph{valuation route} flips the arms
#' (\code{f1_XM} on the left, \code{fZ_MY} on the right). The visual
#' grammar of the coloring scheme makes the two route components of the
#' moderation decomposition read directly: if both arms of a mediator
#' carry visible weight in one of the two graphs, that gold--black or
#' black--gold chain identifies a \emph{reason} that the moderator
#' shifts the X-to-Y relationship. Mutually exclusive
#' with \code{Z_value} and \code{Z_overlay}; nodes are drawn white in
#' route views for the same reason as the Z overlay.
#'
#' @param x A \code{\link{pathXMY}} or \code{\link{pathXMY_decompose}} return.
#' @param mediator Character vector. If \code{NULL} (default) and \code{x}
#'   has multiple mediators, a fan diagram of all of them is drawn. Pass
#'   a single name for the triangle view. Pass a subset for a smaller fan.
#' @param X_label,Y_label,Z_label Display labels for the X, Y, and Z nodes.
#' @param M_labels Optional character vector of labels for the mediators,
#'   parallel to \code{mediator} (or to the natural order in the fit).
#'   If \code{NULL}, the mediator variable names are used.
#' @param X_shape Shape for the X node: \code{"square"} (default; X is a
#'   regular variable) or \code{"rtTri"} (right-facing triangle; X is an
#'   imagined action, as in the speeding and overtime EXSJTs).
#' @param Y_shape Shape for the Y node: \code{"lfTri"} (default; Y is an
#'   action likelihood, the typical focal outcome) or \code{"square"} (Y
#'   is a regular continuous variable). Mediators are always drawn as
#'   squares.
#' @param digits Decimal places for edge labels (default 2).
#' @param show_pvalues Logical; append p-values to edge labels.
#' @param strip0 Logical; if \code{TRUE} (default), edge-label
#'   coefficients render without their leading zero (\code{0.31 -> .31})
#'   in keeping with the funfield decimal house style. Set to
#'   \code{FALSE} to keep the leading zero.
#' @param scale_max Numeric. Path magnitude that maps to the maximum
#'   edge linewidth. Default \code{0.8}.
#' @param score_intensity_max Numeric. The expected-score absolute value
#'   that maps to maximum color intensity. Default \code{1} (the
#'   \code{[-1, 1]} range).
#' @param Z_value Optional numeric scalar. If supplied, the diagram is
#'   rendered \emph{conditional on Z at this value} -- every coefficient
#'   becomes the effective slope at that Z (\code{f1 + fZ * Z_value}),
#'   node colors reflect expected scores at that Z, and edge labels show
#'   the single effective coefficient rather than the
#'   \code{f1 + fZ(Z)} decomposition. See also
#'   \code{\link{plotPathXMY_ZLH}} for a paired low/high view.
#' @param Z_overlay Logical. If \code{TRUE}, render the \emph{Z-overlay}:
#'   the same graph layout as the normative model, but each B1
#'   coefficient is swapped for its BZ counterpart -- i.e. the
#'   per-unit-Z change in path weight. Nodes are drawn white since
#'   expected scores have no meaning in this view. Mutually exclusive
#'   with \code{Z_value} and \code{route}.
#' @param route One of \code{"none"} (default), \code{"expectation"}, or
#'   \code{"valuation"}. Selects a per-route moderation view rather than
#'   the decomposed default. \code{"expectation"} puts \code{fZ_XM} on
#'   the X-to-M arm (green) and \code{f1_MY} on the M-to-Y arm (black);
#'   \code{"valuation"} flips them (\code{f1_XM} green-less left,
#'   \code{fZ_MY} green right). No direct X-to-Y arrow is drawn since
#'   the route concept is about the indirect path through M. Mutually
#'   exclusive with \code{Z_value} and \code{Z_overlay}.
#' @param from One of \code{"loop"} (default) or \code{"joint"}: which
#'   tidy table on the \code{pathXMY()} return drives the diagram.
#'   See the description for behavior. The joint view requires that
#'   the fit was produced with \code{joint = TRUE} (the default) and
#'   \code{length(M) > 1}.
#' @param node_size Half-side of each square / half-bounding-box of the
#'   triangle, in data coordinates (default \code{0.07}).
#' @param label_pad Padding between a node's perimeter and its text
#'   label, in data coordinates (default \code{0.025}).
#' @param title Optional plot title.
#' @param filename,filetype Optional. If \code{filename} is supplied,
#'   the plot is also saved via \code{ggplot2::ggsave()} using
#'   \code{filetype} as the device (e.g., \code{"png"}, \code{"pdf"}).
#' @param ... Additional arguments passed to \code{ggplot2::ggsave()}
#'   when \code{filename} is supplied (ignored otherwise).
#' @return A \code{ggplot} object. Auto-prints in interactive sessions
#'   and knitr chunks; assign to a variable to suppress and reuse.
#' @seealso \code{\link{pathXMY}}, \code{\link{pathXMY_decompose}},
#'   \code{\link{plotPathXMY_ZLH}}, \code{\link{plotPathXMY_widget}}
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom grid arrow unit
#' @export
plotPathXMY <- function(x,
                        mediator = NULL,
                        X_label = "X", Y_label = "Y", Z_label = "Z",
                        M_labels = NULL,
                        X_shape = c("square", "rtTri"),
                        Y_shape = c("lfTri", "square"),
                        digits = 2,
                        show_pvalues = FALSE,
                        strip0 = TRUE,
                        scale_max = 0.8,
                        score_intensity_max = 1,
                        Z_value = NULL,
                        Z_overlay = FALSE,
                        route = c("none", "expectation", "valuation"),
                        from = c("loop", "joint"),
                        node_size = 0.07,
                        label_pad = 0.025,
                        title = NULL,
                        filename = NULL,
                        filetype = "png",
                        ...) {

  ## -- Extract tidy tables -----------------------------------------
  ## tidy_loop carries per-mediator loop-fit rows; tidy_joint (when
  ## present) carries the joint multi-mediator simultaneous-fit rows,
  ## including the global f1_XY_joint / fZ_XY_joint rows
  ## (mediator = NA). `from` selects which table drives the diagram:
  ##  - "loop" (default): per-mediator loop overlay; in fan view this
  ##    overlay is intentionally pure -- NO direct X to Y arrow is
  ##    drawn, because the only sensible direct path in that picture
  ##    would be from a different (joint) fit.
  ##  - "joint": the joint simultaneous-fit picture, including the
  ##    single global X to Y direct arrow.
  from <- match.arg(from)
  loop_tidy <- if (!is.null(x$fits) && !is.null(x$fits$full)) x$fits$full$tidy_loop
               else if (!is.null(x$tidy_loop))                 x$tidy_loop
               else NULL
  joint_tidy <- if (!is.null(x$fits) && !is.null(x$fits$full)) x$fits$full$tidy_joint
                else if (!is.null(x$tidy_joint))                x$tidy_joint
                else NULL
  if (from == "joint") {
    if (is.null(joint_tidy))
      stop("`from = \"joint\"` requires a joint multi-mediator fit ",
           "(pathXMY(joint = TRUE) with length(M) > 1).")
    tidy <- joint_tidy
    param_suffix <- "_joint"
  } else {
    if (is.null(loop_tidy))
      stop("`x` must be a pathXMY() or pathXMY_decompose() return.")
    tidy <- loop_tidy
    param_suffix <- ""
  }
  mk <- function(base) paste0(base, param_suffix)

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

  X_shape <- match.arg(X_shape)
  Y_shape <- match.arg(Y_shape)

  ## -- Coefficient pulls -------------------------------------------
  pull <- function(med, param) {
    r <- tidy[tidy$mediator == med & tidy$param == param, , drop = FALSE]
    if (nrow(r) == 0L) list(est = NA_real_, pvalue = NA_real_)
    else list(est = r$est[1], pvalue = r$pvalue[1])
  }
  pull_est <- function(med, param) pull(med, param)$est

  f1_XM <- vapply(mediator, pull_est, numeric(1), param = mk("f1_XM"))
  fZ_XM <- vapply(mediator, pull_est, numeric(1), param = mk("fZ_XM"))
  f1_MY <- vapply(mediator, pull_est, numeric(1), param = mk("f1_MY"))
  fZ_MY <- vapply(mediator, pull_est, numeric(1), param = mk("fZ_MY"))

  ## Direct X to Y path:
  ##  - from = "joint": pull the single global f*_XY_joint row
  ##    (mediator = NA) from the joint table.
  ##  - from = "loop", triangle: per-mediator residual f1_XY from
  ##    that one mediator's loop fit.
  ##  - from = "loop", fan: intentionally NO direct arrow, so the
  ##    loop overlay is pure.
  pull_global <- function(param) {
    r <- tidy[is.na(tidy$mediator) & tidy$param == param, , drop = FALSE]
    if (nrow(r) == 0L) list(est = NA_real_, pvalue = NA_real_)
    else list(est = r$est[1], pvalue = r$pvalue[1])
  }
  if (from == "joint") {
    has_global <- any(is.na(tidy$mediator) & tidy$param == mk("f1_XY"))
    if (has_global) {
      has_direct  <- TRUE
      f1_XY       <- pull_global(mk("f1_XY"))$est
      fZ_XY       <- pull_global(mk("fZ_XY"))$est
      p_YX_pvalue <- pull_global(mk("f1_XY"))$pvalue
    } else {
      has_direct <- FALSE
      f1_XY <- NA_real_; fZ_XY <- NA_real_; p_YX_pvalue <- NA_real_
    }
  } else if (fan_view) {
    has_direct <- FALSE
    f1_XY <- NA_real_; fZ_XY <- NA_real_; p_YX_pvalue <- NA_real_
  } else {
    has_direct  <- TRUE
    f1_XY       <- pull_est(mediator[1], "f1_XY")
    fZ_XY       <- pull_est(mediator[1], "fZ_XY")
    p_YX_pvalue <- pull(mediator[1], "f1_XY")$pvalue
  }

  ## -- Z collapse / Z overlay / route -----------------------------
  ## Three view-mode switches that all alter which coefficients drive
  ## the edges. They are mutually exclusive (each implies a different
  ## semantics) and together with the default decomposed view form four
  ## edge interpretations. Per-arm `MX_is_bZ` / `YM_is_bZ` flags drive
  ## both the label coloring and the edge color ramp downstream so the
  ## "anything purely bZ renders in gold (#F6BE00)" convention is
  ## consistent across all view modes.
  z_collapsed <- !is.null(Z_value)
  if (!is.logical(Z_overlay) || length(Z_overlay) != 1L || is.na(Z_overlay))
    stop("`Z_overlay` must be TRUE or FALSE.")
  route <- match.arg(route)
  if (sum(z_collapsed, Z_overlay, route != "none") > 1L)
    stop("Pass at most one of `Z_value`, `Z_overlay`, `route`.")
  naz <- function(v) { v[is.na(v)] <- 0; v }
  ## Per-arm bZ flag: TRUE when the arm displays a bare bZ coefficient.
  MX_is_bZ <- Z_overlay || route == "expectation"
  YM_is_bZ <- Z_overlay || route == "valuation"
  YX_is_bZ <- Z_overlay
  if (Z_overlay) {
    MX_eff <- naz(fZ_XM)
    YM_eff <- naz(fZ_MY)
    YX_eff <- if (has_direct) naz(fZ_XY) else NA_real_
    if (has_direct) {
      p_YX_pvalue <- if (from == "joint") pull_global(mk("fZ_XY"))$pvalue
                     else pull(mediator[1], "fZ_XY")$pvalue
    }
  } else if (z_collapsed) {
    if (!is.numeric(Z_value) || length(Z_value) != 1L)
      stop("`Z_value` must be a single numeric scalar.")
    MX_eff <- f1_XM + naz(fZ_XM) * Z_value
    YM_eff <- f1_MY + naz(fZ_MY) * Z_value
    YX_eff <- if (has_direct) f1_XY + naz(fZ_XY) * Z_value else NA_real_
  } else if (route == "expectation") {
    ## Expectation route: bZ on MX (green), b1 on YM (black). The
    ## direct X-to-Y arrow is suppressed because the route concept is
    ## about the indirect path through M, not the residual direct path.
    MX_eff <- naz(fZ_XM); YM_eff <- f1_MY
    YX_eff <- NA_real_; has_direct <- FALSE
  } else if (route == "valuation") {
    ## Valuation route: b1 on MX (black), bZ on YM (green).
    MX_eff <- f1_XM; YM_eff <- naz(fZ_MY)
    YX_eff <- NA_real_; has_direct <- FALSE
  } else {
    MX_eff <- f1_XM; YM_eff <- f1_MY; YX_eff <- f1_XY
  }
  ## Suppress node fill in any "purely bZ on some arm" presentation
  ## (Z_overlay collapses both arms; route views collapse one). Node
  ## scoring under those conditions mixes scales (bZ ~0.1 vs b1 ~0.5)
  ## and would mislead the diverging fill scale.
  suppress_scores <- Z_overlay || route != "none"

  ## -- Node scores at X = 1 ----------------------------------------
  ## Expected scores have no meaning when an arm is itself a bZ slope,
  ## so suppress fill in Z-overlay and route views. NA scores are
  ## rendered via `na.value = "white"` below.
  if (suppress_scores) {
    scores_clipped <- rep(NA_real_, n_m + 2L)
  } else {
    X_score  <- 1
    M_scores <- MX_eff
    Y_score  <- sum(YM_eff * MX_eff, na.rm = TRUE) +
                (if (has_direct && !is.na(YX_eff)) YX_eff else 0)
    scores_vec     <- c(X_score, M_scores, Y_score)
    scores_clipped <- pmax(-score_intensity_max,
                           pmin(score_intensity_max, scores_vec))
  }

  ## -- Layout ------------------------------------------------------
  if (fan_view) {
    ## Stretch the mediator y-range so adjacent box centers stay at
    ## least `min_spacing` apart in data coordinates. Required for the
    ## above-label convention to read unambiguously: at default
    ## node_size = 0.07 and label_pad = 0.025, a center-to-center
    ## spacing of about 0.33 keeps each label clearly closer to its
    ## own box than to the next box up. Even n_m uses n_m - 1 gaps;
    ## odd n_m drops the centermost slot of n_m + 1 slots, so the
    ## effective gap is 2 * y_extent / n_m.
    min_spacing <- (node_size * 2) + (label_pad * 2) + 0.14
    slots       <- if (n_m %% 2L == 0L) n_m - 1L else n_m
    y_extent    <- max(1, slots * min_spacing / 2)
    if (n_m %% 2L == 0L) {
      y_pos <- seq(y_extent, -y_extent, length.out = n_m)
    } else {
      y_all   <- seq(y_extent, -y_extent, length.out = n_m + 1L)
      drop_ix <- (n_m + 1L) %/% 2L + 1L
      y_pos   <- y_all[-drop_ix]
    }
    layout_x <- c(-1.5, rep(0, n_m), 1.5)
    layout_y <- c(0,    y_pos,        0)
  } else {
    layout_x <- c(-1.0, 0.0, 1.0)
    layout_y <- c(-0.25, 0.35, -0.25)
  }

  nodes <- data.frame(
    name  = c("X", mediator, "Y"),
    label = c(X_label, M_labels, Y_label),
    role  = c("X", rep("M", n_m), "Y"),
    x     = layout_x,
    y     = layout_y,
    score = scores_clipped,
    shape = c(X_shape, rep("square", n_m), Y_shape),
    stringsAsFactors = FALSE
  )
  ## Label convention: above for M, below for X and Y.
  nodes$label_y <- ifelse(
    nodes$role == "M",
    nodes$y + node_size + label_pad,
    nodes$y - node_size - label_pad
  )
  nodes$label_vjust <- ifelse(nodes$role == "M", 0, 1)

  ## -- Polygon coords (one shape per node) -------------------------
  build_polygon <- function(shape, s) {
    if (shape == "square") {
      list(px = c(-1,  1, 1, -1) * s,
           py = c(-1, -1, 1,  1) * s)
    } else if (shape == "lfTri") {
      list(px = c(-1,  1,  1) * s,
           py = c( 0,  1, -1) * s)
    } else if (shape == "rtTri") {
      list(px = c( 1, -1, -1) * s,
           py = c( 0,  1, -1) * s)
    } else stop("Unknown shape: ", shape)
  }
  poly_df <- do.call(rbind, lapply(seq_len(nrow(nodes)), function(k) {
    sh <- nodes$shape[k]; cx <- nodes$x[k]; cy <- nodes$y[k]
    p  <- build_polygon(sh, node_size)
    data.frame(name  = nodes$name[k],
               px    = p$px + cx,
               py    = p$py + cy,
               score = nodes$score[k],
               stringsAsFactors = FALSE)
  }))

  ## -- Edges -------------------------------------------------------
  ## Label coloring convention: any fragment that is purely a bZ
  ## coefficient renders in gold (#F6BE00), matching the condition-node
  ## color used in plotField(). There are two display modes:
  ##  - "decomposed" (default view): the label is "f1 + fZ(Z)"; only
  ##    the trailing bZ fragment is wrapped in gold.
  ##  - "single" (Z_overlay, Z_value, route): the label is one number.
  ##    If the arm itself is bZ (is_bZ_arm), the whole label is gold;
  ##    otherwise it stays black.
  z_gold      <- "#F6BE00"
  wrap_gold   <- function(s) paste0("<span style='color:", z_gold, "'>",
                                    s, "</span>")
  single_mode <- Z_overlay || z_collapsed || route != "none"
  ## Coefficient formatter: f0() strips the leading zero per the
  ## funfield house style; sprintf("%+.Nf") is the fallback when
  ## strip0 = FALSE. keep_sign = TRUE in f0() preserves the leading
  ## "+" on non-negative values so the dashed/solid + sign convention
  ## reads consistently.
  fmt_coef <- function(x) {
    if (strip0) f0(x, digits = digits, keep_sign = TRUE)
    else        sprintf(paste0("%+.", digits, "f"), x)
  }
  fmt_pv <- function(p) {
    if (strip0) f0(p, digits = 3, keep_sign = FALSE)
    else        sprintf("%.3f", p)
  }
  fmt_path <- function(coef, bZ, pv = NA_real_, is_bZ_arm = FALSE) {
    if (is.na(coef)) return("")
    main <- fmt_coef(coef)
    if (single_mode) {
      out <- main
      if (show_pvalues && !is.na(pv))
        out <- paste0(out, "<br>p=", fmt_pv(pv))
      return(if (is_bZ_arm) wrap_gold(out) else out)
    }
    ## Decomposed view: b1 head + gold bZ fragment.
    out <- main
    if (!is.na(bZ) && abs(bZ) > 0) {
      bz_part <- paste0(fmt_coef(bZ), "(", Z_label, ")")
      out <- paste0(out, " ", wrap_gold(bz_part))
    }
    if (show_pvalues && !is.na(pv))
      out <- paste0(out, "<br>p=", fmt_pv(pv))
    out
  }

  edge_rows <- list()
  for (i in seq_along(mediator)) {
    med  <- mediator[i]
    p_MX <- pull(med, mk(if (MX_is_bZ) "fZ_XM" else "f1_XM"))$pvalue
    p_YM <- pull(med, mk(if (YM_is_bZ) "fZ_MY" else "f1_MY"))$pvalue
    edge_rows[[length(edge_rows) + 1L]] <- data.frame(
      from = "X", to = med,
      coef = MX_eff[i], bZ = fZ_XM[i], pv = p_MX,
      is_bZ = MX_is_bZ,
      label = fmt_path(MX_eff[i], fZ_XM[i], p_MX, MX_is_bZ),
      stringsAsFactors = FALSE
    )
    edge_rows[[length(edge_rows) + 1L]] <- data.frame(
      from = med, to = "Y",
      coef = YM_eff[i], bZ = fZ_MY[i], pv = p_YM,
      is_bZ = YM_is_bZ,
      label = fmt_path(YM_eff[i], fZ_MY[i], p_YM, YM_is_bZ),
      stringsAsFactors = FALSE
    )
  }
  if (has_direct && !is.na(YX_eff)) {
    edge_rows[[length(edge_rows) + 1L]] <- data.frame(
      from = "X", to = "Y",
      coef = YX_eff, bZ = fZ_XY, pv = p_YX_pvalue,
      is_bZ = YX_is_bZ,
      label = fmt_path(YX_eff, fZ_XY, p_YX_pvalue, YX_is_bZ),
      stringsAsFactors = FALSE
    )
  }
  edges <- do.call(rbind, edge_rows)

  edges$from_x <- nodes$x[match(edges$from, nodes$name)]
  edges$from_y <- nodes$y[match(edges$from, nodes$name)]
  edges$to_x   <- nodes$x[match(edges$to,   nodes$name)]
  edges$to_y   <- nodes$y[match(edges$to,   nodes$name)]
  edges$mid_x  <- (edges$from_x + edges$to_x) / 2
  edges$mid_y  <- (edges$from_y + edges$to_y) / 2
  edges$lty    <- ifelse(!is.na(edges$coef) & edges$coef < 0,
                         "dashed", "solid")
  ## Magnitude -> linewidth (nonlinear; small coefs get very thin) and
  ## magnitude -> grayscale (power curve; largest |coef| renders
  ## near-black, near-zero |coef| fades to nearly white). Same formula
  ## for the normative loop view, the joint view, and the Z-overlay --
  ## scale_max is the per-call knob if a tighter or looser cap is
  ## wanted (e.g. scale_max = 0.3 for a typical bZ field, the default
  ## 0.8 for a B1 field).
  mag_ratio   <- pmin(abs(edges$coef) / scale_max, 1)
  edges$lw    <- mag_ratio^1.4 * 3.0 + 0.05
  ## Per-edge color ramp: gold ramp (#F6BE00 to near-white) for arms
  ## that ARE a bZ coefficient; grayscale ramp otherwise. This handles
  ## the Z-overlay (both arms gold) and route views (one arm gold, one
  ## black) with the same per-edge logic.
  fade        <- (1 - mag_ratio)^3
  gray_col    <- grDevices::gray(fade)
  g_rgb       <- c(246, 190, 0) / 255   # #F6BE00
  gold_col    <- grDevices::rgb(
    red   = g_rgb[1] + (1 - g_rgb[1]) * fade,
    green = g_rgb[2] + (1 - g_rgb[2]) * fade,
    blue  = g_rgb[3] + (1 - g_rgb[3]) * fade
  )
  edges$ecol  <- ifelse(edges$is_bZ, gold_col, gray_col)
  edges$from_shape <- nodes$shape[match(edges$from, nodes$name)]
  edges$to_shape   <- nodes$shape[match(edges$to,   nodes$name)]

  ## -- Shape-aware boundary clipping ------------------------------
  ## Bounding boxes are centered on each node's layout point. For a
  ## ray exiting the center along (ux, uy):
  ##   square -> L-infinity:  s / max(|ux|, |uy|)
  ##   lfTri  -> apex at (-s, 0), base corners (s, +/-s):
  ##     ux > 0          -> base edge:    s / ux
  ##     ux <= 0, uy > 0 -> top edge:     s / (2*uy - ux)
  ##     ux <= 0, uy < 0 -> bottom edge:  s / (-2*uy - ux)
  ##     ux < 0, uy == 0 -> apex:         s
  ##   rtTri  -> apex at (+s, 0), base corners (-s, +/-s)
  ##     (mirror of lfTri across x = 0):
  ##     ux < 0          -> base edge:    s / (-ux)
  ##     ux >= 0, uy > 0 -> top edge:     s / (ux + 2*uy)
  ##     ux >= 0, uy < 0 -> bottom edge:  s / (ux - 2*uy)
  ##     ux > 0, uy == 0 -> apex:         s
  node_exit <- function(shape, s, ux, uy) {
    if (shape == "square") {
      return(s / max(abs(ux), abs(uy), 1e-9))
    } else if (shape == "lfTri") {
      if (ux > 0) {
        return(s / ux)
      } else if (uy > 0) {
        return(s / (2 * uy - ux))
      } else if (uy < 0) {
        return(s / (-2 * uy - ux))
      } else {
        return(s)
      }
    } else if (shape == "rtTri") {
      if (ux < 0) {
        return(s / (-ux))
      } else if (uy > 0) {
        return(s / (ux + 2 * uy))
      } else if (uy < 0) {
        return(s / (ux - 2 * uy))
      } else {
        return(s)
      }
    }
    s
  }
  edge_shorten <- function(x0, y0, x1, y1, from_shape, to_shape, s) {
    dx <- x1 - x0; dy <- y1 - y0
    d  <- sqrt(dx^2 + dy^2)
    if (d == 0) return(c(x0, y0, x1, y1))
    ux <- dx / d; uy <- dy / d
    r0 <- node_exit(from_shape, s,  ux,  uy)
    r1 <- node_exit(to_shape,   s, -ux, -uy)
    c(x0 + r0 * ux, y0 + r0 * uy, x1 - r1 * ux, y1 - r1 * uy)
  }
  shortened <- t(mapply(edge_shorten,
                        edges$from_x, edges$from_y,
                        edges$to_x,   edges$to_y,
                        edges$from_shape, edges$to_shape,
                        MoreArgs = list(s = node_size)))
  edges$sx <- shortened[, 1]; edges$sy <- shortened[, 2]
  edges$ex <- shortened[, 3]; edges$ey <- shortened[, 4]

  ## -- Title / subtitle (fan / overlay / route caption) -----------
  ## Coefficient names in the auto-captions use the F-schema matrix-cell
  ## display form (FZ[X,M], F1[M,Y], ...). Edge labels themselves are
  ## bare numbers, so this is the only place a label name surfaces in the
  ## plot.
  subtitle <- NULL
  if (Z_overlay) {
    subtitle <- sprintf(
      "Z overlay: per-unit-%s change in path weight (FZ paths)", Z_label)
  } else if (route == "expectation") {
    subtitle <- sprintf(
      "Expectation route: FZ[X,M](%s) on the left arm, F1[M,Y] on the right",
      Z_label)
  } else if (route == "valuation") {
    subtitle <- sprintf(
      "Valuation route: F1[X,M] on the left arm, FZ[M,Y](%s) on the right",
      Z_label)
  } else if (fan_view) {
    subtitle <- if (from == "joint") {
      "Joint multi-mediator fit (one simultaneous regression with all M and direct X to Y)"
    } else {
      "Per-mediator loop fits (one X to M to Y regression per mediator, overlaid)"
    }
  }

  squish01 <- function(x, range) pmin(pmax(x, range[1]), range[2])

  ## -- Assemble ----------------------------------------------------
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data  = edges,
      ggplot2::aes(x = .data$sx, y = .data$sy,
                   xend = .data$ex, yend = .data$ey,
                   linetype  = .data$lty,
                   linewidth = .data$lw,
                   color     = .data$ecol),
      arrow = grid::arrow(length = grid::unit(0.14, "inches"),
                          type   = "closed"),
      lineend = "round") +
    ggtext::geom_richtext(
      data = edges,
      ggplot2::aes(x = .data$mid_x, y = .data$mid_y,
                   label = .data$label),
      size          = 3.2,
      label.padding = grid::unit(0.12, "lines"),
      label.size    = 0,
      fill          = "white") +
    ggplot2::geom_polygon(
      data = poly_df,
      ggplot2::aes(x = .data$px, y = .data$py,
                   group = .data$name, fill = .data$score),
      color = "black", linewidth = 0.5) +
    ggplot2::geom_text(
      data = nodes,
      ggplot2::aes(x = .data$x, y = .data$label_y,
                   label = .data$label, vjust = .data$label_vjust),
      size = 4) +
    ggplot2::scale_fill_gradient2(
      low      = "#b73712",
      mid      = "white",
      high     = "#1572da",
      midpoint = 0,
      limits   = c(-score_intensity_max, score_intensity_max),
      oob      = squish01,
      na.value = "white",
      name     = "score",
      ## Score-axis labels honor the house strip0 style. Exact zero
      ## renders as "0" (not ".0") since it's a category, not a
      ## rounded value.
      labels   = if (strip0) function(x) {
                   out <- f0(x, digits = 1, keep_sign = FALSE)
                   out[!is.na(x) & x == 0] <- "0"
                   out
                 } else ggplot2::waiver(),
      guide    = if (suppress_scores) "none" else ggplot2::guide_colorbar(
        barwidth  = grid::unit(2.2, "in"),
        barheight = grid::unit(0.18, "in"))) +
    ggplot2::scale_linetype_identity() +
    ggplot2::scale_linewidth_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin     = ggplot2::margin(8, 8, 8, 8),
      plot.title      = ggplot2::element_text(
        hjust = 0.5, size = 12,
        margin = ggplot2::margin(b = 6)),
      plot.subtitle   = ggplot2::element_text(
        hjust = 0.5, size = 9,
        margin = ggplot2::margin(b = 10)),
      legend.position = "bottom") +
    ggplot2::labs(title = title, subtitle = subtitle)

  if (!is.null(filename)) {
    ggplot2::ggsave(filename, p, device = filetype, ...)
  }
  p
}
