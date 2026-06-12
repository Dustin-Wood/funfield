#' Field-style path diagram for a pathF() cascade or DAG
#'
#' @description
#' Renders a \code{\link{pathF}} model as a \pkg{ggplot2} object in the
#' funfield visualization style, generalizing \code{\link{plotPathXMY}}
#' from the X-M-Y triangle/fan to causal cascades and arbitrary DAGs:
#'
#' \itemize{
#'   \item \strong{Layout by causal depth.} Each node's horizontal
#'     position is its longest-path depth from the model's source, so a
#'     serial cascade renders as a left-to-right chain and parallel
#'     mediators in a DAG stack vertically at the same depth (recovering
#'     the familiar fan).
#'   \item \strong{Skip edges arc.} Edges between adjacent depths are
#'     straight; edges that skip one or more depths (e.g. the residual
#'     direct \code{X -> Y} path in a cascade) bow upward as quadratic
#'     arcs, with height scaled by the number of depths skipped. Labels
#'     sit at the arc apex.
#'   \item \strong{Nodes} are colored by their implied expected score
#'     when the focal X is held at 1, propagated through the cascade
#'     (each node's score is the effective-coefficient-weighted sum of
#'     its predecessors' scores), on the same red-white-blue diverging
#'     gradient as \code{plotPathXMY()}. Nodes not downstream of X
#'     render white.
#'   \item \strong{Edges} follow the shared conventions: solid positive
#'     / dashed negative, linewidth and grayscale ramped by magnitude
#'     (capped at \code{scale_max}), arrowheads clipped to the node
#'     perimeter, and edge labels carrying the \code{f1 + fZ(Z)}
#'     decomposition with the fZ fragment in gold (\code{#F6BE00}).
#'   \item \strong{View modes.} \code{view} selects what the edges
#'     display: \code{"decomposed"} (default) shows the f1 field with
#'     the gold fZ fragment on each label; \code{"normative"} shows the
#'     same f1 field with the moderation hidden (black labels only);
#'     \code{"moderation"} swaps every f1 for its fZ counterpart (all
#'     edges and labels gold, node fill suppressed).
#'     \code{Z_value} instead collapses each edge to its effective
#'     slope at that Z. \code{Z_value} and a non-default \code{view}
#'     are mutually exclusive. \code{\link{plotPathF_widget}} stitches
#'     the three views into a Back/Forward widget.
#' }
#'
#' @param x A \code{\link{pathF}} return, or a \code{\link{pathF_decompose}}
#'   return (its \code{$fits$full} model is used).
#' @param labels Optional named character vector mapping variable names to
#'   display labels, e.g. \code{c(Speed = "Speed", HCoPot = "Hot coffee")}.
#'   Unnamed variables keep their own names.
#' @param X_shape Shape for the focal X node: \code{"rtTri"} (default;
#'   X is typically an imagined action in the EXSJTs) or \code{"square"}.
#' @param Y_shape Shape for the focal Y node: \code{"lfTri"} (default;
#'   an action likelihood) or \code{"square"}.
#' @param digits Decimal places for edge labels (default 2).
#' @param show_pvalues Logical; append p-values to edge labels.
#' @param strip0 Logical; strip leading zeros from edge-label decimals
#'   per the funfield house style (default \code{TRUE}).
#' @param scale_max Path magnitude mapping to maximum edge linewidth
#'   (default \code{0.8}; use a tighter cap such as \code{0.3} for
#'   the \code{"moderation"} view, where fZ magnitudes run smaller).
#' @param score_intensity_max Expected-score absolute value mapping to
#'   maximum node-fill intensity (default \code{1}).
#' @param view One of \code{"decomposed"} (default), \code{"normative"},
#'   or \code{"moderation"}; see the description. \code{"moderation"}
#'   requires a fit with Z.
#' @param Z_value Optional numeric scalar; render the field conditional
#'   on Z at this value (each edge becomes \code{f1 + fZ * Z_value}).
#'   Mutually exclusive with a non-default \code{view}.
#' @param Z_label Display label for the moderator in decomposed edge
#'   labels (default \code{"Z"}).
#' @param node_size Half-extent of each node shape in data coordinates
#'   (default \code{0.07}).
#' @param label_pad Padding between node perimeter and its text label
#'   (default \code{0.03}).
#' @param x_spacing Horizontal distance between adjacent causal depths
#'   (default \code{1.2}).
#' @param arc_height Arc rise per skipped depth for non-adjacent edges
#'   (default \code{0.55}).
#' @param title Optional plot title.
#' @param filename,filetype Optional; save the plot via
#'   \code{ggplot2::ggsave()}.
#' @param ... Passed to \code{ggplot2::ggsave()} when \code{filename}
#'   is supplied.
#' @return A \code{ggplot} object.
#' @seealso \code{\link{pathF}}, \code{\link{pathF_decompose}},
#'   \code{\link{plotPathXMY}}
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom grid arrow unit
#' @export
plotPathF <- function(x,
                      labels = NULL,
                      X_shape = c("rtTri", "square"),
                      Y_shape = c("lfTri", "square"),
                      digits = 2,
                      show_pvalues = FALSE,
                      strip0 = TRUE,
                      scale_max = 0.8,
                      score_intensity_max = 1,
                      view = c("decomposed", "normative", "moderation"),
                      Z_value = NULL,
                      Z_label = "Z",
                      node_size = 0.07,
                      label_pad = 0.03,
                      x_spacing = 1.2,
                      arc_height = 0.55,
                      title = NULL,
                      filename = NULL,
                      filetype = "png",
                      ...) {

  X_shape <- match.arg(X_shape)
  Y_shape <- match.arg(Y_shape)

  ## -- Accept pathF or pathF_decompose returns ---------------------
  if (!is.null(x$fits) && !is.null(x$fits$full) &&
      inherits(x$fits$full, "pathF")) {
    x <- x$fits$full
  }
  if (!inherits(x, "pathF"))
    stop("`x` must be a pathF() or pathF_decompose() return.")
  spec  <- x$spec
  tidy  <- x$tidy
  nodes_v <- spec$nodes
  edges <- spec$edges

  view <- match.arg(view)
  z_collapsed <- !is.null(Z_value)
  if (z_collapsed && view != "decomposed")
    stop("Pass at most one of `Z_value`, a non-default `view`.")
  if (z_collapsed && (!is.numeric(Z_value) || length(Z_value) != 1L))
    stop("`Z_value` must be a single numeric scalar.")
  is_mod <- view == "moderation"

  ## -- Pull edge coefficients --------------------------------------
  pull1 <- function(param) {
    r <- tidy[tidy$param == param, , drop = FALSE]
    if (nrow(r) == 0L) list(est = NA_real_, pvalue = NA_real_)
    else list(est = r$est[1], pvalue = r$pvalue[1])
  }
  edges$f1 <- NA_real_; edges$fZ <- NA_real_
  edges$p1 <- NA_real_; edges$pZ <- NA_real_
  for (r in seq_len(nrow(edges))) {
    e1 <- pull1(sprintf("f1_%s_%s", edges$src[r], edges$tgt[r]))
    eZ <- pull1(sprintf("fZ_%s_%s", edges$src[r], edges$tgt[r]))
    edges$f1[r] <- e1$est; edges$p1[r] <- e1$pvalue
    edges$fZ[r] <- eZ$est; edges$pZ[r] <- eZ$pvalue
  }
  has_Z <- any(!is.na(edges$fZ))
  if ((z_collapsed || is_mod) && !has_Z)
    stop("The fit has no fZ coefficients; refit pathF() with Z.")
  naz <- function(v) { v[is.na(v)] <- 0; v }

  ## Effective coefficient per view mode ("normative" displays the same
  ## f1 field as "decomposed"; only its labels differ, below)
  if (is_mod) {
    edges$coef <- naz(edges$fZ)
    edges$pv   <- edges$pZ
  } else if (z_collapsed) {
    edges$coef <- edges$f1 + naz(edges$fZ) * Z_value
    edges$pv   <- edges$p1
  } else {
    edges$coef <- edges$f1
    edges$pv   <- edges$p1
  }

  ## -- Layout: longest-path depth + vertical stacking --------------
  K <- length(nodes_v)
  idx <- stats::setNames(seq_len(K), nodes_v)
  depth <- rep(0L, K)
  ## longest path from any source (edges are a DAG by construction)
  for (pass in seq_len(K)) {
    changed <- FALSE
    for (r in seq_len(nrow(edges))) {
      si <- idx[[edges$src[r]]]; ti <- idx[[edges$tgt[r]]]
      if (depth[ti] < depth[si] + 1L) {
        depth[ti] <- depth[si] + 1L
        changed <- TRUE
      }
    }
    if (!changed) break
  }
  ## y positions: stack nodes sharing a depth symmetrically about 0
  ypos <- numeric(K)
  for (d in unique(depth)) {
    at <- which(depth == d)
    n  <- length(at)
    if (n > 1L) {
      ext <- 0.5 * (n - 1)
      ypos[at] <- seq(ext, -ext, length.out = n)
    }
  }
  layout_x <- depth * x_spacing
  layout_y <- ypos

  ## -- Node roles / shapes / scores --------------------------------
  Xv <- spec$X; Yv <- spec$Y
  role <- ifelse(nodes_v == Xv, "X", ifelse(nodes_v == Yv, "Y", "M"))
  shape <- ifelse(role == "X", X_shape,
                  ifelse(role == "Y", Y_shape, "square"))

  ## Implied expected scores at X = 1, propagated in depth order.
  ## Suppressed in the moderation view (a per-unit-Z slope field has no
  ## expected-score reading).
  if (is_mod) {
    scores <- rep(NA_real_, K)
  } else {
    scores <- rep(NA_real_, K)
    if (!is.na(Xv)) scores[idx[[Xv]]] <- 1
    for (d in sort(unique(depth))) {
      for (k in which(depth == d)) {
        inc <- edges[edges$tgt == nodes_v[k], , drop = FALSE]
        if (nrow(inc) == 0L) next
        pre <- scores[idx[inc$src]]
        if (all(is.na(pre))) next
        scores[k] <- sum(inc$coef * pre, na.rm = TRUE)
      }
    }
    scores <- pmax(-score_intensity_max,
                   pmin(score_intensity_max, scores))
  }

  disp <- nodes_v
  if (!is.null(labels)) {
    hit <- nodes_v %in% names(labels)
    disp[hit] <- labels[nodes_v[hit]]
  }

  nodes <- data.frame(
    name = nodes_v, label = disp, role = role,
    x = layout_x, y = layout_y, score = scores, shape = shape,
    stringsAsFactors = FALSE
  )
  ## Labels below each node (single-row chains keep the baseline clean;
  ## stacked nodes inherit the same convention).
  nodes$label_y <- nodes$y - node_size - label_pad

  ## -- Polygons -----------------------------------------------------
  build_polygon <- function(shape, s) {
    switch(shape,
      square = list(px = c(-1, 1, 1, -1) * s, py = c(-1, -1, 1, 1) * s),
      lfTri  = list(px = c(-1, 1, 1) * s,     py = c(0, 1, -1) * s),
      rtTri  = list(px = c(1, -1, -1) * s,    py = c(0, 1, -1) * s),
      stop("Unknown shape: ", shape))
  }
  poly_df <- do.call(rbind, lapply(seq_len(nrow(nodes)), function(k) {
    p <- build_polygon(nodes$shape[k], node_size)
    data.frame(name = nodes$name[k],
               px = p$px + nodes$x[k], py = p$py + nodes$y[k],
               score = nodes$score[k], stringsAsFactors = FALSE)
  }))

  ## -- Edge label formatting (shared f1 + gold fZ(Z) grammar) ------
  z_gold <- "#F6BE00"
  wrap_gold <- function(s) paste0("<span style='color:", z_gold, "'>",
                                  s, "</span>")
  fmt_coef <- function(v) {
    if (strip0) f0(v, digits = digits, keep_sign = TRUE)
    else        sprintf(paste0("%+.", digits, "f"), v)
  }
  fmt_pv <- function(p) {
    if (strip0) f0(p, digits = 3, keep_sign = FALSE)
    else        sprintf("%.3f", p)
  }
  single_mode <- is_mod || z_collapsed
  fmt_path <- function(coef, bZ, pv) {
    if (is.na(coef)) return("")
    out <- fmt_coef(coef)
    if (single_mode) {
      if (show_pvalues && !is.na(pv)) out <- paste0(out, "<br>p=", fmt_pv(pv))
      return(if (is_mod) wrap_gold(out) else out)
    }
    if (!is.na(bZ) && abs(bZ) > 0) {
      out <- paste0(out, " ", wrap_gold(paste0(fmt_coef(bZ), "(", Z_label, ")")))
    }
    if (show_pvalues && !is.na(pv)) out <- paste0(out, "<br>p=", fmt_pv(pv))
    out
  }
  ## The normative view shows the f1 field with the moderation hidden:
  ## suppress the gold fZ fragment by withholding fZ from the labeler.
  label_fZ <- if (view == "normative") rep(NA_real_, nrow(edges)) else edges$fZ
  edges$label <- mapply(fmt_path, edges$coef, label_fZ, edges$pv)

  ## -- Edge geometry: straight for adjacent depths, arcs for skips --
  edges$from_x <- nodes$x[idx[edges$src]]
  edges$from_y <- nodes$y[idx[edges$src]]
  edges$to_x   <- nodes$x[idx[edges$tgt]]
  edges$to_y   <- nodes$y[idx[edges$tgt]]
  edges$span   <- depth[idx[edges$tgt]] - depth[idx[edges$src]]
  ## Skip-arcs alternate sides by span parity (even spans bow up, odd
  ## spans bow down) so converging arrowheads split between the top and
  ## bottom of a shared target instead of piling onto one point.
  arc_dir      <- ifelse(edges$span %% 2L == 0L, 1, -1)
  edges$rise   <- pmax(edges$span - 1L, 0L) * arc_height * arc_dir

  edges$lty <- ifelse(!is.na(edges$coef) & edges$coef < 0, "dashed", "solid")
  mag_ratio <- pmin(abs(edges$coef) / scale_max, 1)
  mag_ratio[is.na(mag_ratio)] <- 0
  edges$lw  <- mag_ratio^1.4 * 3.0 + 0.05
  fade      <- (1 - mag_ratio)^3
  gray_col  <- grDevices::gray(fade)
  g_rgb     <- c(246, 190, 0) / 255
  gold_col  <- grDevices::rgb(
    red   = g_rgb[1] + (1 - g_rgb[1]) * fade,
    green = g_rgb[2] + (1 - g_rgb[2]) * fade,
    blue  = g_rgb[3] + (1 - g_rgb[3]) * fade
  )
  edges$ecol <- if (is_mod) gold_col else gray_col

  ## Shape containment tests for bezier clipping (centered shapes):
  ##   square: |dx| <= s and |dy| <= s
  ##   lfTri (apex -s,0; base corners s,+/-s): dx <= s and |dy| <= (dx+s)/2
  ##   rtTri (apex +s,0; base corners -s,+/-s): dx >= -s and |dy| <= (s-dx)/2
  inside_shape <- function(shape, cx, cy, s, px, py) {
    dx <- px - cx; dy <- py - cy
    switch(shape,
      square = abs(dx) <= s & abs(dy) <= s,
      lfTri  = dx <= s & abs(dy) <= (dx + s) / 2,
      rtTri  = dx >= -s & abs(dy) <= (s - dx) / 2)
  }

  ## Quadratic bezier per edge (control point lifted by `rise`); clip
  ## the sampled polyline to the first point outside the source shape
  ## and the last point outside the target shape, so arrowheads land on
  ## the node perimeter for straight and curved edges alike.
  n_samp <- 80L
  tt <- seq(0, 1, length.out = n_samp)
  path_rows <- vector("list", nrow(edges))
  apex <- matrix(NA_real_, nrow(edges), 2)
  for (r in seq_len(nrow(edges))) {
    p0 <- c(edges$from_x[r], edges$from_y[r])
    p1 <- c(edges$to_x[r],   edges$to_y[r])
    cp <- c((p0[1] + p1[1]) / 2, (p0[2] + p1[2]) / 2 + edges$rise[r])
    bx <- (1 - tt)^2 * p0[1] + 2 * tt * (1 - tt) * cp[1] + tt^2 * p1[1]
    by <- (1 - tt)^2 * p0[2] + 2 * tt * (1 - tt) * cp[2] + tt^2 * p1[2]
    s_sh <- nodes$shape[idx[[edges$src[r]]]]
    t_sh <- nodes$shape[idx[[edges$tgt[r]]]]
    in_s <- inside_shape(s_sh, p0[1], p0[2], node_size, bx, by)
    in_t <- inside_shape(t_sh, p1[1], p1[2], node_size, bx, by)
    keep <- which(!in_s & !in_t)
    if (length(keep) < 2L) keep <- seq_len(n_samp)
    keep <- min(keep):max(keep)
    path_rows[[r]] <- data.frame(
      edge_id = r, px = bx[keep], py = by[keep],
      lty = edges$lty[r], lw = edges$lw[r], ecol = edges$ecol[r],
      stringsAsFactors = FALSE
    )
    apex[r, 1] <- 0.25 * p0[1] + 0.5 * cp[1] + 0.25 * p1[1]
    apex[r, 2] <- 0.25 * p0[2] + 0.5 * cp[2] + 0.25 * p1[2]
  }
  path_df <- do.call(rbind, path_rows)
  edges$mid_x <- apex[, 1]
  ## Nudge arc labels off the curve (above an up-arc, below a down-arc);
  ## straight-edge labels stay centered on the segment, where their
  ## white richtext fill keeps them readable.
  edges$mid_y <- apex[, 2] + sign(edges$rise) * 0.085

  ## -- Subtitle ------------------------------------------------------
  subtitle <- NULL
  if (is_mod) {
    subtitle <- sprintf(
      "Moderation field: per-unit-%s change in path weight (FZ paths)",
      Z_label)
  } else if (view == "normative" && has_Z) {
    subtitle <- "Normative field: F1 paths only (moderation hidden)"
  } else if (z_collapsed) {
    zv_lab <- sprintf("%g", Z_value)
    if (strip0) zv_lab <- sub("^([+-]?)0\\.", "\\1.", zv_lab)
    subtitle <- sprintf("Field at %s = %s", Z_label, zv_lab)
  }

  squish01 <- function(v, range) pmin(pmax(v, range[1]), range[2])

  ## -- Assemble ------------------------------------------------------
  p <- ggplot2::ggplot() +
    ggplot2::geom_path(
      data = path_df,
      ggplot2::aes(x = .data$px, y = .data$py, group = .data$edge_id,
                   linetype = .data$lty, linewidth = .data$lw,
                   color = .data$ecol),
      arrow = grid::arrow(length = grid::unit(0.14, "inches"),
                          type = "closed"),
      lineend = "round") +
    ggtext::geom_richtext(
      data = edges[nzchar(edges$label), , drop = FALSE],
      ggplot2::aes(x = .data$mid_x, y = .data$mid_y, label = .data$label),
      size = 3.0,
      label.padding = grid::unit(0.1, "lines"),
      label.size = 0,
      fill = "white") +
    ggplot2::geom_polygon(
      data = poly_df,
      ggplot2::aes(x = .data$px, y = .data$py,
                   group = .data$name, fill = .data$score),
      color = "black", linewidth = 0.5) +
    ggplot2::geom_text(
      data = nodes,
      ggplot2::aes(x = .data$x, y = .data$label_y, label = .data$label),
      vjust = 1, size = 3.8) +
    ggplot2::scale_fill_gradient2(
      low = "#b73712", mid = "white", high = "#1572da",
      midpoint = 0,
      limits = c(-score_intensity_max, score_intensity_max),
      oob = squish01,
      na.value = "white",
      name = "score",
      labels = if (strip0) function(v) {
                 out <- f0(v, digits = 1, keep_sign = FALSE)
                 out[!is.na(v) & v == 0] <- "0"
                 out
               } else ggplot2::waiver(),
      guide = if (is_mod) "none" else ggplot2::guide_colorbar(
        barwidth = grid::unit(2.2, "in"),
        barheight = grid::unit(0.18, "in"))) +
    ggplot2::scale_linetype_identity() +
    ggplot2::scale_linewidth_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(8, 8, 8, 8),
      plot.title = ggplot2::element_text(
        hjust = 0.5, size = 12, margin = ggplot2::margin(b = 6)),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5, size = 9, margin = ggplot2::margin(b = 10)),
      legend.position = "bottom",
      legend.box.spacing = grid::unit(0.25, "in")) +
    ggplot2::labs(title = title, subtitle = subtitle)

  if (!is.null(filename)) {
    ggplot2::ggsave(filename, p, device = filetype, ...)
  }
  p
}
