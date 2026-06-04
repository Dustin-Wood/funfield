#' Symbolic path-schema diagram (X to Y, optionally through one M)
#'
#' @description
#' Renders a clean pedagogical schematic of the X to Y path (with an
#' optional single mediator M) as a \pkg{ggplot2} object. Nodes are
#' drawn as white shapes (square by default; right- or left-facing
#' triangle if requested via \code{X_shape}/\code{Y_shape}), edges as
#' uniform solid black arrows. Edge labels are arbitrary user-supplied
#' strings (typically coefficient symbols) rather than fitted numbers,
#' so the schematic can sit alongside an algebraic identity
#' term-for-term.
#'
#' Two layouts:
#' \itemize{
#'   \item \strong{Total path} (\code{M_label = NULL}): X on the left,
#'     Y on the right, a single arrow between them carrying
#'     \code{XY_label}.
#'   \item \strong{Mediation triangle} (\code{M_label} set): X at
#'     bottom-left, M at the apex, Y at bottom-right. Edges
#'     \code{X to M} and \code{M to Y} carry \code{XM_label} and
#'     \code{MY_label}; if \code{XY_label} is also supplied, the
#'     direct \code{X to Y} residual arrow is drawn too.
#' }
#'
#' Edge labels are passed through \code{\link[ggtext]{geom_richtext}},
#' so HTML markup is supported -- e.g.
#' \code{"&beta;<sub>1<sub>MX</sub></sub>"} for proper Greek and
#' nested subscripts.
#'
#' @param X_label,Y_label Node labels for X and Y. Default \code{"X"},
#'   \code{"Y"}.
#' @param M_label Optional node label for the mediator. \code{NULL}
#'   (default) draws the total \code{X to Y} path only.
#' @param XY_label,XM_label,MY_label Edge labels (HTML supported). In
#'   the no-mediator layout, only \code{XY_label} is used. In the
#'   triangle layout, \code{XM_label} and \code{MY_label} carry the
#'   indirect arms; \code{XY_label} is optional and, when supplied,
#'   labels the direct \code{X to Y} residual arrow.
#' @param X_shape,Y_shape Shape for the X / Y node:
#'   \code{"square"} (default), \code{"rtTri"} (right-facing triangle,
#'   apex on the right -- conventional for an "action" / initiating
#'   node), or \code{"lfTri"} (left-facing triangle, apex on the left
#'   -- conventional for a likelihood / outcome node). Triangles are
#'   sized so their bounding box matches the square's, and edges are
#'   clipped against the shape's actual perimeter.
#' @param XM_color,MY_color,XY_color Per-edge arrow colors. All default
#'   to \code{"black"}. Use \code{"#F6BE00"} (gold, the bZ moderator
#'   convention matching \code{plotField()} condition nodes) when an arm
#'   represents a bZ moderation route, and a light gray (e.g.
#'   \code{"#BBBBBB"}) to fade arms that are not part of the term being
#'   highlighted in a panel. Ignored for any edge not drawn (e.g.
#'   \code{XY_color} when \code{XY_label} is NULL).
#' @param node_size Half-side of each node shape in data coordinates.
#'   Default \code{0.09}.
#' @param label_pad Padding between a node's perimeter and its text
#'   label, in data coordinates. Default \code{0.03}.
#' @param text_size Node-label text size. Default \code{4.5}.
#' @param edge_text_size Edge-label text size. Default \code{4.5}.
#' @param edge_linewidth Linewidth for edges. Default \code{0.9}.
#' @param title Optional plot title.
#' @param filename,filetype Optional. If \code{filename} is supplied,
#'   the plot is also saved via \code{ggplot2::ggsave()} using
#'   \code{filetype} as the device (e.g., \code{"png"}, \code{"pdf"}).
#' @param ... Additional arguments passed to \code{ggplot2::ggsave()}
#'   when \code{filename} is supplied (ignored otherwise).
#' @return A \code{ggplot} object.
#' @seealso \code{\link{plotPathXMY}} for the data-driven counterpart
#'   that renders fitted coefficients on the same layout.
#' @examples
#' \dontrun{
#' ## Total path (default squares)
#' plotPathSchema(XY_label = "&beta;*<sub>1<sub>YX</sub></sub>")
#'
#' ## Single-mediator decomposition (default squares)
#' plotPathSchema(
#'   M_label  = "M",
#'   XM_label = "&beta;<sub>1<sub>MX</sub></sub>",
#'   MY_label = "&beta;<sub>1<sub>YM</sub></sub>",
#'   XY_label = "&beta;<sub>1<sub>YX</sub></sub>"
#' )
#'
#' ## Expectancy x Value styling: X = action (right-facing triangle),
#' ## Y = likelihood (left-facing triangle).
#' plotPathSchema(
#'   M_label  = "M",
#'   XM_label = "&beta;<sub>1<sub>MX</sub></sub>",
#'   MY_label = "&beta;<sub>1<sub>YM</sub></sub>",
#'   XY_label = "&beta;<sub>1<sub>YX</sub></sub>",
#'   X_shape  = "rtTri",
#'   Y_shape  = "lfTri"
#' )
#' }
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom grid arrow unit
#' @export
plotPathSchema <- function(X_label = "X",
                           Y_label = "Y",
                           M_label = NULL,
                           XY_label = NULL,
                           XM_label = NULL,
                           MY_label = NULL,
                           X_shape = c("square", "rtTri", "lfTri"),
                           Y_shape = c("square", "rtTri", "lfTri"),
                           XM_color = "black",
                           MY_color = "black",
                           XY_color = "black",
                           node_size = 0.09,
                           label_pad = 0.03,
                           text_size = 4.5,
                           edge_text_size = 4.5,
                           edge_linewidth = 0.9,
                           title = NULL,
                           filename = NULL,
                           filetype = "png",
                           ...) {

  X_shape <- match.arg(X_shape)
  Y_shape <- match.arg(Y_shape)

  has_M <- !is.null(M_label)
  if (!has_M && is.null(XY_label))
    stop("`XY_label` is required when no mediator is given.")
  if (has_M && is.null(XM_label) && is.null(MY_label))
    stop("Provide at least one of `XM_label` / `MY_label` for the ",
         "triangle layout.")

  ## -- Layout ------------------------------------------------------
  if (has_M) {
    nodes <- data.frame(
      name  = c("X", "M", "Y"),
      label = c(X_label, M_label, Y_label),
      role  = c("X", "M", "Y"),
      shape = c(X_shape, "square", Y_shape),
      x     = c(-1.0, 0.0, 1.0),
      y     = c(-0.25, 0.35, -0.25),
      stringsAsFactors = FALSE
    )
  } else {
    nodes <- data.frame(
      name  = c("X", "Y"),
      label = c(X_label, Y_label),
      role  = c("X", "Y"),
      shape = c(X_shape, Y_shape),
      x     = c(-1.0, 1.0),
      y     = c(0, 0),
      stringsAsFactors = FALSE
    )
  }
  ## Label convention (same as plotPathXMY): above for M, below for X/Y.
  nodes$label_y <- ifelse(
    nodes$role == "M",
    nodes$y + node_size + label_pad,
    nodes$y - node_size - label_pad
  )
  nodes$label_vjust <- ifelse(nodes$role == "M", 0, 1)

  ## -- Polygon corners (white shapes) ------------------------------
  ## Bounding box for every shape is the +/- node_size square, so
  ## switching shape does not change the slot a node occupies.
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
    data.frame(name = nodes$name[k],
               px   = p$px + cx,
               py   = p$py + cy,
               stringsAsFactors = FALSE)
  }))

  ## -- Edges -------------------------------------------------------
  edge_rows <- list()
  if (has_M) {
    if (!is.null(XM_label))
      edge_rows[[length(edge_rows) + 1L]] <-
        data.frame(from = "X", to = "M", label = XM_label,
                   ecol = XM_color, stringsAsFactors = FALSE)
    if (!is.null(MY_label))
      edge_rows[[length(edge_rows) + 1L]] <-
        data.frame(from = "M", to = "Y", label = MY_label,
                   ecol = MY_color, stringsAsFactors = FALSE)
    if (!is.null(XY_label))
      edge_rows[[length(edge_rows) + 1L]] <-
        data.frame(from = "X", to = "Y", label = XY_label,
                   ecol = XY_color, stringsAsFactors = FALSE)
  } else {
    edge_rows[[1L]] <-
      data.frame(from = "X", to = "Y", label = XY_label,
                 ecol = XY_color, stringsAsFactors = FALSE)
  }
  edges <- do.call(rbind, edge_rows)

  edges$from_x <- nodes$x[match(edges$from, nodes$name)]
  edges$from_y <- nodes$y[match(edges$from, nodes$name)]
  edges$to_x   <- nodes$x[match(edges$to,   nodes$name)]
  edges$to_y   <- nodes$y[match(edges$to,   nodes$name)]
  edges$mid_x  <- (edges$from_x + edges$to_x) / 2
  edges$mid_y  <- (edges$from_y + edges$to_y) / 2
  edges$from_shape <- nodes$shape[match(edges$from, nodes$name)]
  edges$to_shape   <- nodes$shape[match(edges$to,   nodes$name)]

  ## -- Shape-aware boundary clipping ------------------------------
  ## Same intersection geometry as plotPathXMY(): for a ray exiting
  ## the center along (ux, uy), return the distance to the shape
  ## perimeter so the arrowhead lands on the edge, not the centroid.
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

  ## -- Assemble ----------------------------------------------------
  ## Per-edge stroke color via aes(color = ecol) + scale_color_identity.
  ## Arrowhead inherits stroke color so faded arms get fully faded
  ## heads. Labels are rendered as ggtext richtext, so per-fragment
  ## coloring is the caller's responsibility (via <span style='color:'>).
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data  = edges,
      ggplot2::aes(x = .data$sx, y = .data$sy,
                   xend = .data$ex, yend = .data$ey,
                   color = .data$ecol),
      arrow     = grid::arrow(length = grid::unit(0.14, "inches"),
                              type   = "closed"),
      linewidth = edge_linewidth,
      lineend   = "round") +
    ggplot2::scale_color_identity() +
    ggtext::geom_richtext(
      data = edges,
      ggplot2::aes(x = .data$mid_x, y = .data$mid_y,
                   label = .data$label),
      size          = edge_text_size,
      label.padding = grid::unit(0.18, "lines"),
      label.size    = 0,
      fill          = "white") +
    ggplot2::geom_polygon(
      data = poly_df,
      ggplot2::aes(x = .data$px, y = .data$py, group = .data$name),
      fill = "white", color = "black", linewidth = 0.5) +
    ggplot2::geom_text(
      data = nodes,
      ggplot2::aes(x = .data$x, y = .data$label_y,
                   label = .data$label, vjust = .data$label_vjust),
      size = text_size) +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(8, 8, 8, 8),
      plot.title  = ggplot2::element_text(
        hjust = 0.5, size = 12,
        margin = ggplot2::margin(b = 6))) +
    ggplot2::labs(title = title)

  if (!is.null(filename)) {
    ggplot2::ggsave(filename, p, device = filetype, ...)
  }
  p
}
