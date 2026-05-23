#' Field-style path diagram for an X-M-Y model
#'
#' @description
#' Renders the X to M to Y structure as a \pkg{ggplot2} object using the
#' funfield visualization conventions:
#'
#' \itemize{
#'   \item \strong{Nodes} are colored by their expected score when X is
#'     held at 1, on a continuous diverging gradient
#'     (\code{red -> white -> dodgerblue}) mapped to the \code{[-1, 1]}
#'     range via \code{ggplot2::scale_fill_gradient2()}. The X node is
#'     by definition 1 (full blue); each mediator's score is its path
#'     coefficient \eqn{b^1_{MX}}; Y's score is the sum of the direct
#'     and indirect contributions,
#'     \eqn{b^1_{YX} + \sum_j b^1_{YM_j} \cdot b^1_{MX_j}}.
#'   \item \strong{Shape} is a true square for X and each mediator
#'     (continuous variables) and a left-facing triangle for Y (the
#'     focal action likelihood).
#'   \item \strong{Labels} sit outside the shapes: above for any
#'     mediator (M), below for X and Y. This convention scales to the
#'     fan view.
#'   \item \strong{Edges} are solid for positive paths and dashed for
#'     negative paths; edge linewidth scales with path magnitude.
#'     Arrowheads are clipped to the node perimeter via shape-aware
#'     intersection (L-infinity for squares, edge intersection for the
#'     left-facing triangle). Edge labels carry the
#'     \code{b1 + bZ(Z)} decomposition.
#' }
#'
#' Two view modes:
#' \itemize{
#'   \item \strong{Triangle} (single mediator) -- X at bottom-left, M at
#'     top, Y at bottom-right.
#'   \item \strong{Fan} (multiple mediators) -- X at left, mediators
#'     stacked in the middle, Y at right. The mediator arms are an
#'     overlay of the per-mediator loop fits; with joint multi-mediator
#'     results (\code{pathXMY(joint = TRUE)}, the default), the residual
#'     X to Y direct arrow is drawn from the joint fit. With an odd
#'     number of mediators the stack is laid out asymmetrically so the
#'     straight X to Y arrow has a clean gap at the y = 0 axis.
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
#' @param scale_max Numeric. Path magnitude that maps to the maximum
#'   edge linewidth. Default \code{0.8}.
#' @param score_intensity_max Numeric. The expected-score absolute value
#'   that maps to maximum color intensity. Default \code{1} (the
#'   \code{[-1, 1]} range).
#' @param Z_value Optional numeric scalar. If supplied, the diagram is
#'   rendered \emph{conditional on Z at this value} -- every coefficient
#'   becomes the effective slope at that Z (\code{b1 + bZ * Z_value}),
#'   node colors reflect expected scores at that Z, and edge labels show
#'   the single effective coefficient rather than the
#'   \code{b1 + bZ(Z)} decomposition. See also
#'   \code{\link{plotPathXMY_ZLH}} for a paired low/high view.
#' @param node_size Half-side of each square / half-bounding-box of the
#'   triangle, in data coordinates (default \code{0.07}).
#' @param label_pad Padding between a node's perimeter and its text
#'   label, in data coordinates (default \code{0.05}).
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
                        digits = 2,
                        show_pvalues = FALSE,
                        scale_max = 0.8,
                        score_intensity_max = 1,
                        Z_value = NULL,
                        node_size = 0.07,
                        label_pad = 0.05,
                        title = NULL,
                        filename = NULL,
                        filetype = "png",
                        ...) {

  ## -- Extract tidy table ------------------------------------------
  tidy <- if (!is.null(x$fits) && !is.null(x$fits$full)) x$fits$full$tidy_loop
          else if (!is.null(x$tidy_loop))                 x$tidy_loop
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

  ## -- Coefficient pulls -------------------------------------------
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

  ## Joint multi-mediator direct path (special row with mediator = NA)
  pull_joint <- function(param) {
    r <- tidy[is.na(tidy$mediator) & tidy$param == param, , drop = FALSE]
    if (nrow(r) == 0L) list(est = NA_real_, pvalue = NA_real_)
    else list(est = r$est[1], pvalue = r$pvalue[1])
  }
  has_joint_direct <- any(is.na(tidy$mediator) &
                          tidy$param == "B1_YX_joint")

  if (fan_view) {
    if (has_joint_direct) {
      has_direct  <- TRUE
      B1_YX       <- pull_joint("B1_YX_joint")$est
      BZ_YX       <- pull_joint("BZ_YX_joint")$est
      p_YX_pvalue <- pull_joint("B1_YX_joint")$pvalue
    } else {
      has_direct <- FALSE
      B1_YX <- NA_real_; BZ_YX <- NA_real_; p_YX_pvalue <- NA_real_
    }
  } else {
    has_direct  <- TRUE
    B1_YX       <- pull_est(mediator[1], "B1_YX")
    BZ_YX       <- pull_est(mediator[1], "BZ_YX")
    p_YX_pvalue <- pull(mediator[1], "B1_YX")$pvalue
  }

  ## -- Z collapse --------------------------------------------------
  z_collapsed <- !is.null(Z_value)
  if (z_collapsed) {
    if (!is.numeric(Z_value) || length(Z_value) != 1L)
      stop("`Z_value` must be a single numeric scalar.")
    naz <- function(v) { v[is.na(v)] <- 0; v }
    MX_eff <- B1_MX + naz(BZ_MX) * Z_value
    YM_eff <- B1_YM + naz(BZ_YM) * Z_value
    YX_eff <- if (has_direct) B1_YX + naz(BZ_YX) * Z_value else NA_real_
  } else {
    MX_eff <- B1_MX; YM_eff <- B1_YM; YX_eff <- B1_YX
  }

  ## -- Node scores at X = 1 ----------------------------------------
  X_score  <- 1
  M_scores <- MX_eff
  Y_score  <- sum(YM_eff * MX_eff, na.rm = TRUE) +
              (if (has_direct && !is.na(YX_eff)) YX_eff else 0)
  scores_vec     <- c(X_score, M_scores, Y_score)
  scores_clipped <- pmax(-score_intensity_max,
                         pmin(score_intensity_max, scores_vec))

  ## -- Layout ------------------------------------------------------
  if (fan_view) {
    if (n_m %% 2L == 0L) {
      y_pos <- seq(1, -1, length.out = n_m)
    } else {
      y_all   <- seq(1, -1, length.out = n_m + 1L)
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
    shape = c("square", rep("square", n_m), "lfTri"),
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
  fmt_path <- function(b1, bZ, pv = NA_real_) {
    if (is.na(b1)) return("")
    out <- sprintf(paste0("%+.", digits, "f"), b1)
    if (!z_collapsed && !is.na(bZ) && abs(bZ) > 0)
      out <- paste0(out, " ",
                    sprintf(paste0("%+.", digits, "f(%s)"), bZ, Z_label))
    if (show_pvalues && !is.na(pv))
      out <- paste0(out, "\np=", sprintf("%.3f", pv))
    out
  }

  edge_rows <- list()
  for (i in seq_along(mediator)) {
    med  <- mediator[i]
    p_MX <- pull(med, "B1_MX")$pvalue
    p_YM <- pull(med, "B1_YM")$pvalue
    edge_rows[[length(edge_rows) + 1L]] <- data.frame(
      from = "X", to = med,
      coef = MX_eff[i], bZ = BZ_MX[i], pv = p_MX,
      label = fmt_path(MX_eff[i], BZ_MX[i], p_MX),
      stringsAsFactors = FALSE
    )
    edge_rows[[length(edge_rows) + 1L]] <- data.frame(
      from = med, to = "Y",
      coef = YM_eff[i], bZ = BZ_YM[i], pv = p_YM,
      label = fmt_path(YM_eff[i], BZ_YM[i], p_YM),
      stringsAsFactors = FALSE
    )
  }
  if (has_direct && !is.na(YX_eff)) {
    edge_rows[[length(edge_rows) + 1L]] <- data.frame(
      from = "X", to = "Y",
      coef = YX_eff, bZ = BZ_YX, pv = p_YX_pvalue,
      label = fmt_path(YX_eff, BZ_YX, p_YX_pvalue),
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
  edges$lw     <- pmin(abs(edges$coef) / scale_max, 1) * 2.2 + 0.3
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

  ## -- Title / subtitle (fan caption) ------------------------------
  subtitle <- NULL
  if (fan_view) {
    subtitle <- if (has_joint_direct) {
      "Summary of per-mediator X to M to Y fits; X to Y from joint fit of all M"
    } else {
      "Summary of per-mediator X to M to Y fits across M"
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
                   linewidth = .data$lw),
      arrow = grid::arrow(length = grid::unit(0.14, "inches"),
                          type   = "closed"),
      color = "black", lineend = "round") +
    ggplot2::geom_label(
      data = edges,
      ggplot2::aes(x = .data$mid_x, y = .data$mid_y,
                   label = .data$label),
      size = 3.2,
      label.padding = grid::unit(0.12, "lines"),
      label.size    = 0,
      fill          = "#FFFFFFDA") +
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
      low      = "red",
      mid      = "white",
      high     = "dodgerblue",
      midpoint = 0,
      limits   = c(-score_intensity_max, score_intensity_max),
      oob      = squish01,
      name     = "score",
      guide    = ggplot2::guide_colorbar(
        barwidth  = grid::unit(2.2, "in"),
        barheight = grid::unit(0.18, "in"))) +
    ggplot2::scale_linetype_identity() +
    ggplot2::scale_linewidth_identity() +
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
