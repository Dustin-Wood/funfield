## ---------------------------------------------------------------------
## Shared shape geometry for field diagrams. Same polygon corners and
## ray/perimeter intersection used by plotPathSchema(), generalised to
## arbitrary node positions and a diamond (object) shape.
## ---------------------------------------------------------------------

## Polygon corners for a shape whose bounding box is the +/- s square.
.field_polygon <- function(shape, s) {
  switch(
    shape,
    square  = list(px = c(-1,  1, 1, -1) * s, py = c(-1, -1, 1,  1) * s),
    diamond = list(px = c( 0,  1, 0, -1) * s, py = c(-1,  0, 1,  0) * s),
    lfTri   = list(px = c(-1,  1,  1) * s,    py = c( 0,  1, -1) * s),
    rtTri   = list(px = c( 1, -1, -1) * s,    py = c( 0,  1, -1) * s),
    stop("Unknown shape: ", shape)
  )
}

## Distance from a node centre to its perimeter along unit ray (ux, uy).
.field_exit <- function(shape, s, ux, uy) {
  if (shape == "square") {
    s / max(abs(ux), abs(uy), 1e-9)
  } else if (shape == "diamond") {
    s / max(abs(ux) + abs(uy), 1e-9)
  } else if (shape == "lfTri") {
    if (ux > 0)       s / ux
    else if (uy > 0)  s / (2 * uy - ux)
    else if (uy < 0)  s / (-2 * uy - ux)
    else              s
  } else if (shape == "rtTri") {
    if (ux < 0)       s / (-ux)
    else if (uy > 0)  s / (ux + 2 * uy)
    else if (uy < 0)  s / (ux - 2 * uy)
    else              s
  } else s
}

## Trim a centre-to-centre segment back to both node perimeters.
.field_shorten <- function(x0, y0, x1, y1, from_shape, to_shape, s) {
  dx <- x1 - x0; dy <- y1 - y0
  d  <- sqrt(dx^2 + dy^2)
  if (d == 0) return(c(x0, y0, x1, y1))
  ux <- dx / d; uy <- dy / d
  r0 <- .field_exit(from_shape, s,  ux,  uy)
  r1 <- .field_exit(to_shape,   s, -ux, -uy)
  c(x0 + r0 * ux, y0 + r0 * uy, x1 - r1 * ux, y1 - r1 * uy)
}

## Parse an fZ_X.Y label into list(mod, source, target). Returns NULL
## if the string does not match the schema.
.field_parse_label <- function(lbl) {
  if (length(lbl) != 1L || is.na(lbl) || !nzchar(lbl)) return(NULL)
  if (!grepl("^f.+_.+\\..+$", lbl)) return(NULL)
  body <- sub("^f", "", lbl)
  us   <- regexpr("_", body, fixed = TRUE)
  mod  <- substr(body, 1L, us - 1L)
  st   <- substr(body, us + 1L, nchar(body))
  list(mod    = mod,
       source = sub("\\..*$", "", st),
       target = sub("^.*\\.", "", st))
}

## Structural (source -> target) edge identities of a model string,
## keyed for set membership. Source comes from the fZ_X.Y label when
## present, otherwise the first right-hand-side variable.
.field_pairs <- function(model) {
  pt <- lavaan::lavaanify(model, fixed.x = FALSE)
  pt <- pt[pt$op == "~", , drop = FALSE]
  if (nrow(pt) == 0L) return(character(0))
  vapply(seq_len(nrow(pt)), function(i) {
    rhs_vars <- strsplit(pt$rhs[i], ":", fixed = TRUE)[[1]]
    parsed   <- .field_parse_label(pt$label[i])
    src <- if (!is.null(parsed)) parsed$source else rhs_vars[length(rhs_vars)]
    paste(src, pt$lhs[i], sep = "\r")
  }, character(1))
}

#' Draw one frame of a functional field at a given state
#'
#' @description
#' Renders a functional field `F` evaluated at a single state `s` as a
#' \pkg{ggplot2} node-link diagram. Nodes are placed at caller-supplied
#' coordinates and drawn as shapes coloured by their current state value
#' (white at the low limit, blue at the high limit). An edge is drawn for
#' every force that is currently **afforded** --- i.e., whose resolved
#' field coefficient is non-zero given `s`. Stepping `plotField()` across
#' the rows of a [runF()] trajectory and stitching the frames with
#' [plotsAsWidget()] produces a back/forward animation of forces being
#' introduced as gates open and state propagating through the field.
#'
#' @details
#' Each regression row of `model` (`Y ~ label * X` or
#' `Y ~ label * X:Z`) is read through its `fZ_X.Y` label (see
#' `vignette("notation", package = "funfield")`): the label names the
#' source `X` and target `Y`, and any remaining variable in the
#' right-hand-side interaction is the **gate** `Z`. The resolved field
#' coefficient is `params[label] * prod(s[gate])`, so an afforded force
#' (no baseline term) contributes nothing until its gate flips on. The
#' edge `X -> Y` is drawn whenever the magnitude of that resolved
#' coefficient exceeds `edge_min`; the source node `X` itself is coloured
#' by `s[X]`, so an afforded-but-unfired force shows as an edge leaving a
#' still-white node. Terms with a fixed coefficient (e.g. `1 * X`, as in
#' a conditional action plan) are valued from that fixed coefficient
#' rather than `params`.
#'
#' When a `plan` sub-model is supplied, the edges it contributes are
#' drawn in `plan_color` (gold by convention) -- a visual marker for the
#' forces a conditional action plan adds on top of the situation's own
#' affordance structure.
#'
#' @param model A `lavaan`-syntax model string with `fZ_X.Y` labels (and
#'   optionally fixed `1 * X` terms), as consumed by [evalF()] / [runF()].
#'   When a `plan` is layered onto a situation model, pass the full
#'   stitched string here.
#' @param params Named numeric vector of parameter values.
#' @param s Named numeric vector: the state at which to draw the field.
#' @param layout A data frame describing node placement, with columns
#'   `name` (matching the model's variables), `x`, `y`, and `shape` (one
#'   of `"square"`, `"diamond"`, `"rtTri"`, `"lfTri"`). An optional
#'   `label` column supplies display text (defaults to `name`).
#' @param plan Optional `lavaan`-syntax sub-model string. Edges whose
#'   source -> target identity appears in `plan` are drawn in
#'   `plan_color`. Typically the action-plan half of a stitched model.
#'   Default `NULL` (no gold edges).
#' @param edge_color,plan_color Colours for situation edges and
#'   plan-contributed edges. Defaults `"grey25"` and `"#DAA520"` (gold).
#' @param fill_limits Numeric length-2 range mapped to the colour
#'   gradient. Default `c(0, 1)`; values outside are squished to the
#'   ends.
#' @param fill_low,fill_high Gradient endpoint colours. Default white to
#'   `"#1572da"`.
#' @param na_fill Fill for layout nodes absent from `s`. Default
#'   `"grey90"`.
#' @param edge_min Minimum absolute resolved coefficient for an edge to
#'   be drawn. Default `1e-9` (draw any afforded force).
#' @param node_size Half-extent of each node shape in data coordinates.
#'   Default `0.32`.
#' @param label_pad Gap between a node's perimeter and its label. Default
#'   `0.12`.
#' @param label_size,edge_label_size Text sizes for node and edge
#'   labels. Defaults `3.2` and `3`.
#' @param edge_linewidth Edge linewidth. Default `0.8`.
#' @param arrow_inches Arrowhead length in inches. Default `0.11`.
#' @param edge_labels Logical: label each drawn edge with its resolved
#'   coefficient. Default `FALSE`.
#' @param edge_label_fmt Function formatting the resolved coefficient for
#'   the edge label. Default two significant digits.
#' @param title Optional plot title.
#'
#' @return A `ggplot` object.
#' @seealso [runF()] for the trajectory, [plotsAsWidget()] to stitch
#'   frames, [plotPathSchema()] for the fixed X-M-Y schematic.
#' @examples
#' \dontrun{
#' model <- "s2 ~ fS1_make.s2 * make:s1
#'           L  ~ f1_s2.L      * s2"
#' params <- c(fS1_make.s2 = 1, f1_s2.L = 1)
#' layout <- data.frame(
#'   name  = c("s1", "make", "s2", "L"),
#'   x     = c(0, 1, 2, 3),
#'   y     = c(0, 0, 1, 1),
#'   shape = c("diamond", "rtTri", "diamond", "lfTri")
#' )
#' plotField(model, params, c(s1 = 1, make = 1, s2 = 1, L = 0), layout)
#' }
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom grid arrow unit
#' @export
plotField <- function(model, params, s, layout,
                      plan            = NULL,
                      edge_color      = "grey25",
                      plan_color      = "#DAA520",
                      fill_limits     = c(0, 1),
                      fill_low        = "white",
                      fill_high       = "#1572da",
                      na_fill         = "grey90",
                      edge_min        = 1e-9,
                      node_size       = 0.32,
                      label_pad       = 0.12,
                      label_size      = 3.2,
                      edge_label_size = 3,
                      edge_linewidth  = 0.8,
                      arrow_inches    = 0.11,
                      edge_labels     = FALSE,
                      edge_label_fmt  = function(x)
                        formatC(x, digits = 2, format = "g"),
                      title = NULL) {

  if (is.null(names(s)) || any(names(s) == ""))
    stop("`s` must be a fully named numeric vector.")
  req <- c("name", "x", "y", "shape")
  if (!is.data.frame(layout) || !all(req %in% names(layout)))
    stop("`layout` must be a data frame with columns: ",
         paste(req, collapse = ", "), ".")
  ok_shapes <- c("square", "diamond", "rtTri", "lfTri")
  bad <- setdiff(unique(layout$shape), ok_shapes)
  if (length(bad))
    stop("Unknown shape(s): ", paste(bad, collapse = ", "),
         ". Use one of: ", paste(ok_shapes, collapse = ", "), ".")
  if (is.null(layout$label)) layout$label <- layout$name

  ## -- Node polygons + fill values --------------------------------
  layout$value <- s[layout$name]   # NA where a layout node isn't in s
  poly_df <- do.call(rbind, lapply(seq_len(nrow(layout)), function(k) {
    p <- .field_polygon(layout$shape[k], node_size)
    data.frame(name  = layout$name[k],
               px    = p$px + layout$x[k],
               py    = p$py + layout$y[k],
               value = layout$value[k],
               stringsAsFactors = FALSE)
  }))

  ## -- Afforded edges ---------------------------------------------
  ## Structural (from, to) pairs contributed by the action plan, if any,
  ## so their edges can be drawn in `plan_color` (gold).
  plan_pairs <- if (!is.null(plan)) .field_pairs(plan) else character(0)

  pt <- lavaan::lavaanify(model, fixed.x = FALSE)
  pt <- pt[pt$op == "~", , drop = FALSE]

  edge_rows <- list()
  for (i in seq_len(nrow(pt))) {
    lbl       <- pt$label[i]
    is_lab    <- !is.na(lbl) && nzchar(lbl)
    if (is_lab) {
      if (!lbl %in% names(params)) next
      coef <- params[[lbl]]
    } else if (pt$free[i] == 0 && !is.na(pt$ustart[i])) {
      coef <- pt$ustart[i]                       # fixed structural coef
    } else next
    rhs_vars <- strsplit(pt$rhs[i], ":", fixed = TRUE)[[1]]
    parsed   <- .field_parse_label(lbl)
    ## Source comes from the label; failing that, the convention places
    ## the action/source last (`Z:X`), so fall back to the final rhs var.
    src <- if (!is.null(parsed)) parsed$source else rhs_vars[length(rhs_vars)]
    gate <- setdiff(rhs_vars, src)
    gate_val <- if (length(gate)) prod(s[gate]) else 1
    resolved <- coef * gate_val
    if (is.na(resolved) || abs(resolved) <= edge_min) next
    pair <- paste(src, pt$lhs[i], sep = "\r")
    edge_rows[[length(edge_rows) + 1L]] <-
      data.frame(from = src, to = pt$lhs[i], resolved = resolved,
                 ecol = if (pair %in% plan_pairs) plan_color else edge_color,
                 stringsAsFactors = FALSE)
  }

  have_edges <- length(edge_rows) > 0L
  if (have_edges) {
    edges <- do.call(rbind, edge_rows)
    miss  <- setdiff(c(edges$from, edges$to), layout$name)
    if (length(miss))
      stop("Edge endpoints missing from `layout`: ",
           paste(unique(miss), collapse = ", "), ".")
    idx <- function(v) match(v, layout$name)
    edges$from_x     <- layout$x[idx(edges$from)]
    edges$from_y     <- layout$y[idx(edges$from)]
    edges$to_x       <- layout$x[idx(edges$to)]
    edges$to_y       <- layout$y[idx(edges$to)]
    edges$from_shape <- layout$shape[idx(edges$from)]
    edges$to_shape   <- layout$shape[idx(edges$to)]
    sh <- t(mapply(.field_shorten,
                   edges$from_x, edges$from_y, edges$to_x, edges$to_y,
                   edges$from_shape, edges$to_shape,
                   MoreArgs = list(s = node_size)))
    edges$sx <- sh[, 1]; edges$sy <- sh[, 2]
    edges$ex <- sh[, 3]; edges$ey <- sh[, 4]
    edges$mid_x <- (edges$sx + edges$ex) / 2
    edges$mid_y <- (edges$sy + edges$ey) / 2
    edges$elab  <- edge_label_fmt(edges$resolved)
  }

  ## -- Assemble ----------------------------------------------------
  p <- ggplot2::ggplot()
  if (have_edges) {
    p <- p + ggplot2::geom_segment(
      data = edges,
      ggplot2::aes(x = .data$sx, y = .data$sy,
                   xend = .data$ex, yend = .data$ey,
                   colour = .data$ecol),
      arrow     = grid::arrow(length = grid::unit(arrow_inches, "inches"),
                              type = "closed"),
      linewidth = edge_linewidth, lineend = "round") +
      ggplot2::scale_colour_identity()
  }
  p <- p +
    ggplot2::geom_polygon(
      data = poly_df,
      ggplot2::aes(x = .data$px, y = .data$py, group = .data$name,
                   fill = .data$value),
      colour = "black", linewidth = 0.5) +
    ggplot2::scale_fill_gradient(
      low = fill_low, high = fill_high, limits = fill_limits,
      oob = scales::squish, na.value = na_fill, name = "state") +
    ggplot2::geom_text(
      data = layout,
      ggplot2::aes(x = .data$x, y = .data$y - node_size - label_pad,
                   label = .data$label),
      vjust = 1, size = label_size)
  if (have_edges && edge_labels) {
    p <- p + ggtext::geom_richtext(
      data = edges,
      ggplot2::aes(x = .data$mid_x, y = .data$mid_y, label = .data$elab),
      size = edge_label_size, label.size = 0,
      label.padding = grid::unit(0.12, "lines"), fill = "white")
  }
  p +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(10, 10, 16, 10),
      plot.title  = ggplot2::element_text(hjust = 0.5, size = 12,
                                          margin = ggplot2::margin(b = 6))) +
    ggplot2::labs(title = title)
}
