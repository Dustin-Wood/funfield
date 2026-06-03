## ---------------------------------------------------------------------
## Shared shape geometry for field diagrams. Same polygon corners and
## ray/perimeter intersection used by plotPathSchema(), generalised to
## arbitrary node positions and a diamond (object) shape.
## ---------------------------------------------------------------------

## Polygon corners for a shape whose bounding box is the +/- s square.
## A circle (choice / chance node) is approximated by a fine polygon of
## radius s.
.field_polygon <- function(shape, s) {
  switch(
    shape,
    square  = list(px = c(-1,  1, 1, -1) * s, py = c(-1, -1, 1,  1) * s),
    diamond = list(px = c( 0,  1, 0, -1) * s, py = c(-1,  0, 1,  0) * s),
    lfTri   = list(px = c(-1,  1,  1) * s,    py = c( 0,  1, -1) * s),
    rtTri   = list(px = c( 1, -1, -1) * s,    py = c( 0,  1, -1) * s),
    circle  = {
      th <- seq(0, 2 * pi, length.out = 49L)[-49L]
      list(px = cos(th) * s, py = sin(th) * s)
    },
    stop("Unknown shape: ", shape)
  )
}

## A lightning-bolt polygon centred at (cx, cy), sized to a node of
## half-extent `s`. Drawn as a filled glyph (more reliable than a ⚡ emoji,
## which most graphics devices render off-centre). The silhouette echoes the
## "high voltage" emoji: a flag at top-left tapering to a point bottom-right.
.bolt_polygon <- function(cx, cy, s) {
  bx <- c( 0.32, -0.40, -0.04, -0.32,  0.40,  0.04)
  by <- c( 1.00,   0.08,  0.08, -1.00, -0.08, -0.08)
  bx <- bx - mean(range(bx)); by <- by - mean(range(by))   # centre exactly
  list(px = cx + bx * s, py = cy + by * s)
}

## Sample an edge as a path of points plus its unit end-tangent. A straight
## edge is the two endpoints; a curved edge is a quadratic Bezier whose
## control point is offset perpendicular to the chord by `curvature`, which
## gives an exact end tangent for placing a sharp arrowhead.
.edge_geom <- function(sx, sy, ex, ey, curvature = 0, n = 32L) {
  dx <- ex - sx; dy <- ey - sy
  len <- sqrt(dx^2 + dy^2)
  if (len < 1e-9)
    return(list(x = c(sx, ex), y = c(sy, ey), dirx = 1, diry = 0))
  if (abs(curvature) < 1e-9)
    return(list(x = c(sx, ex), y = c(sy, ey), dirx = dx / len, diry = dy / len))
  px <- dy / len; py <- -dx / len                 # right-hand normal
  off <- curvature * len * 0.5
  cxp <- (sx + ex) / 2 + px * off
  cyp <- (sy + ey) / 2 + py * off
  t  <- seq(0, 1, length.out = n)
  bx <- (1 - t)^2 * sx + 2 * (1 - t) * t * cxp + t^2 * ex
  by <- (1 - t)^2 * sy + 2 * (1 - t) * t * cyp + t^2 * ey
  tx <- ex - cxp; ty <- ey - cyp                  # tangent at t = 1
  tl <- sqrt(tx^2 + ty^2); if (tl < 1e-9) tl <- 1
  list(x = bx, y = by, dirx = tx / tl, diry = ty / tl)
}

## Distance from a node centre to its perimeter along unit ray (ux, uy).
.field_exit <- function(shape, s, ux, uy) {
  if (shape == "square") {
    s / max(abs(ux), abs(uy), 1e-9)
  } else if (shape == "circle") {
    s
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
#' the rows of a [simulateF()] trajectory and stitching the frames with
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
#' **Encoding the force.** Each edge's resolved coefficient is read off its
#' appearance: its **magnitude** sets the linewidth and colour intensity
#' (the edge's base colour fading toward white as `|f| -> 0`, capped at
#' `edge_scale_max`), and its **sign** sets the linetype (solid for a
#' positive force, dashed for a negative one). So a force of `1` is a strong
#' solid line and a force of `-0.1` a faint dashed one, and (with
#' `edge_labels`) the level is printed on the shaft. The shaft is drawn with
#' butt-ended dashes so they stay legible on a thick line, and the
#' **arrowhead** is a sharp, solid mitred triangle --- identical for
#' positive and negative forces --- carried on a short solid tip segment so
#' a dashed shaft never breaks up the head. **Node bodies** are shaded on a
#' diverging red-white-blue scale (negative red, zero white, positive blue),
#' so a depleting `Energy` stock reddens as it is spent.
#'
#' When a `plan` sub-model is supplied, the edges it contributes take
#' `plan_color` (gold by convention) as their base colour -- a visual marker
#' for the forces a conditional action plan adds on top of the situation's
#' own affordance structure.
#'
#' @param model A `lavaan`-syntax model string with `fZ_X.Y` labels (and
#'   optionally fixed `1 * X` terms), as consumed by [evalF()] / [simulateF()].
#'   When a `plan` is layered onto a situation model, pass the full
#'   stitched string here.
#' @param params Named numeric vector of parameter values.
#' @param s Named numeric vector: the state at which to draw the field.
#' @param layout A data frame describing node placement, with columns
#'   `name` (matching the model's variables), `x`, `y`, and `shape` (one
#'   of `"square"`, `"diamond"`, `"rtTri"`, `"lfTri"`, `"circle"`). By
#'   convention objects are diamonds, actions right-facing triangles, the
#'   likelihood readout a left-facing triangle, and a choice / chance node
#'   a circle. An optional `label` column supplies display text (defaults
#'   to `name`). An optional `glyph` column marks a node as a **glyph node**:
#'   where it holds a non-empty string, that string is drawn centred inside
#'   the shape and no caption is placed beneath it. The body is still
#'   value-shaded like any other node --- e.g. an `Energy` node drawn as a
#'   square holding a lightning bolt that reddens as energy is spent.
#' @param plan Optional `lavaan`-syntax sub-model string. Edges whose
#'   source -> target identity appears in `plan` take `plan_color` as their
#'   base colour. Typically the action-plan half of a stitched model.
#'   Default `NULL` (no gold edges).
#' @param edge_color,plan_color Base colours for situation edges and
#'   plan-contributed edges (each fades toward white as `|f| -> 0`).
#'   Defaults `"black"` and `"#DAA520"` (gold).
#' @param fill_limits Numeric length-2 range mapped to the diverging fill
#'   scale. Default `c(-1, 1)`; values outside are squished to the ends.
#' @param fill_low,fill_mid,fill_high Diverging fill colours at the low,
#'   zero, and high ends. Default red `"#b73712"` / white / blue
#'   `"#1572da"`, midpoint `0`.
#' @param na_fill Fill for layout nodes absent from `s`. Default
#'   `"grey90"`.
#' @param edge_min Minimum absolute resolved coefficient for an edge to
#'   be drawn. Default `1e-9` (draw any afforded force).
#' @param edge_scale_max Absolute resolved coefficient mapped to a
#'   full-strength edge (linewidth and colour saturate here). Default `1`.
#' @param curvature Arc curvature for backward consumption edges (the
#'   negative member of an anti-parallel pair), drawn as a quadratic Bezier
#'   so they bow clear of the straight forward edge they would otherwise
#'   overlay. Negative bows the arc below the forward edge (a leftward
#'   "mirrored smile"); the dip is deep enough to keep the two edges'
#'   mid-shaft labels from colliding. Default `-1`.
#' @param node_size Half-extent of each node shape in data coordinates.
#'   Default `0.32`.
#' @param label_pad Gap between a node's perimeter and its label. Default
#'   `0.12`.
#' @param label_size,edge_label_size Text sizes for node and edge
#'   labels. Defaults `3.2` and `3`.
#' @param glyph_size Text size for centred glyph-node symbols. Default `5`.
#' @param glyph_color Fill colour for the lightning-bolt glyph (`glyph` of
#'   `"bolt"` or `"⚡"`). Default blue `"#1572da"` (the positive-activation
#'   colour).
#' @param edge_linewidth Linewidth of a full-strength edge (`|f| >=
#'   edge_scale_max`); fainter forces scale below it. Default `2.1`.
#' @param arrow_inches Arrowhead length in inches. Default `0.15`. The head
#'   is a sharp, solid (mitred) triangle, the same size for every edge and
#'   for positive and negative forces alike.
#' @param arrow_size Length (data units) of the solid tip segment that
#'   carries each arrowhead, kept solid so the head reads cleanly even on a
#'   dashed shaft. Default `0.26`.
#' @param arrow_stroke Linewidth of the arrowhead's own stroke (constant,
#'   not the shaft's `lw`). Kept thin so the sharp mitred tip lands exactly
#'   on the node outline instead of overshooting into it. Default `0.8`.
#' @param arrow_gap Distance (data units) the head end is pulled back from
#'   the node perimeter, so the arrowhead tip rests on the outline rather
#'   than poking inside. Default `0.06`.
#' @param edge_labels Logical: label each drawn edge with its resolved
#'   force level. Default `TRUE`.
#' @param edge_label_fmt Function formatting the resolved coefficient for
#'   the edge label. Default the no-leading-zero house style with trailing
#'   zeros trimmed (`1`, `.9`, `-.1`).
#' @param title Optional plot title.
#'
#' @return A `ggplot` object.
#' @seealso [simulateF()] for the trajectory, [plotsAsWidget()] to stitch
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
                      edge_color      = "black",
                      plan_color      = "#DAA520",
                      fill_limits     = c(-1, 1),
                      fill_low        = "#b73712",
                      fill_mid        = "white",
                      fill_high       = "#1572da",
                      na_fill         = "grey90",
                      edge_min        = 1e-9,
                      edge_scale_max  = 1,
                      curvature       = -1.0,
                      node_size       = 0.32,
                      label_pad       = 0.12,
                      label_size      = 3.2,
                      glyph_size      = 5,
                      glyph_color     = "#1572da",
                      edge_label_size = 3,
                      edge_linewidth  = 2.1,
                      arrow_inches    = 0.15,
                      arrow_size      = 0.26,
                      arrow_stroke    = 0.8,
                      arrow_gap       = 0.06,
                      edge_labels     = TRUE,
                      edge_label_fmt  = function(x) {
                        s <- f0(x, digits = 2)
                        s <- sub("0+$", "", s)
                        sub("\\.$", "", s)
                      },
                      title = NULL) {

  if (is.null(names(s)) || any(names(s) == ""))
    stop("`s` must be a fully named numeric vector.")
  req <- c("name", "x", "y", "shape")
  if (!is.data.frame(layout) || !all(req %in% names(layout)))
    stop("`layout` must be a data frame with columns: ",
         paste(req, collapse = ", "), ".")
  ok_shapes <- c("square", "diamond", "rtTri", "lfTri", "circle")
  bad <- setdiff(unique(layout$shape), ok_shapes)
  if (length(bad))
    stop("Unknown shape(s): ", paste(bad, collapse = ", "),
         ". Use one of: ", paste(ok_shapes, collapse = ", "), ".")
  if (is.null(layout$label)) layout$label <- layout$name
  ## A `glyph` node carries a centred symbol instead of a value-shaded body:
  ## it is drawn with a transparent fill (a fixed annotation, not read off
  ## the state) and its glyph sits inside the shape, so no caption is placed
  ## beneath it. Used e.g. for an Energy node drawn as a clear square with a
  ## lightning bolt.
  if (is.null(layout$glyph)) layout$glyph <- NA_character_
  is_glyph <- !is.na(layout$glyph) & nzchar(layout$glyph)

  ## -- Node polygons + fill values --------------------------------
  layout$value <- s[layout$name]   # NA where a layout node isn't in s
  poly_df <- do.call(rbind, lapply(seq_len(nrow(layout)), function(k) {
    p <- .field_polygon(layout$shape[k], node_size)
    data.frame(name  = layout$name[k],
               px    = p$px + layout$x[k],
               py    = p$py + layout$y[k],
               value = layout$value[k],
               glyph = is_glyph[k],
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
                 base = if (pair %in% plan_pairs) plan_color else edge_color,
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

    ## Encode each force's resolved coefficient: magnitude -> linewidth and
    ## colour intensity (its base colour fading toward white as |f| -> 0),
    ## sign -> linetype (solid for a positive force, dashed for negative).
    ## So f = 1 is a strong solid line, f = -0.1 a faint dashed one.
    mag_ratio   <- pmin(abs(edges$resolved) / edge_scale_max, 1)
    ## Floor the linewidth so a faint force still reads as a proper dashed
    ## line rather than a dotted one (grid scales the dash pattern with the
    ## linewidth, so a hairline dashed edge degenerates into dots).
    edges$lw    <- mag_ratio^1.4 * (edge_linewidth - 0.45) + 0.45
    edges$lty   <- ifelse(edges$resolved < 0, "dashed", "solid")
    fade        <- (1 - mag_ratio)^3
    base_rgb    <- t(grDevices::col2rgb(edges$base) / 255)
    faded       <- base_rgb + (1 - base_rgb) * fade
    edges$ecol  <- grDevices::rgb(faded[, 1], faded[, 2], faded[, 3])

    ## Backward consumption ('use') edges run anti-parallel to a forward
    ## (plan) edge between the same two nodes and would overlay it. Draw
    ## those --- the negative member of an anti-parallel pair --- as curved
    ## arcs that bow clear of the straight forward edge.
    e_key <- paste(edges$from, edges$to, sep = "\r")
    e_rev <- paste(edges$to,   edges$from, sep = "\r")
    edges$curved <- edges$resolved < 0 & e_rev %in% e_key
  }

  ## -- Assemble ----------------------------------------------------
  ## Each edge is drawn as two pieces so the arrowhead is always a clean,
  ## sharp, solid triangle regardless of the line's dash pattern or sign:
  ##   (1) the shaft --- a path (Bezier when curved) carrying the linetype,
  ##       butt-ended so dashes read crisply even on a thick line; and
  ##   (2) a short solid tip segment at the head, whose closed mitred
  ##       arrowhead points along the edge's exact end tangent.
  p <- ggplot2::ggplot()
  if (have_edges) {
    ## Pull each edge's head end back from the node perimeter by `arrow_gap`
    ## along its end tangent, so the sharp (slightly overshooting) mitred
    ## arrowhead tip lands on the outline rather than poking inside. A first
    ## pass gets the tangent; the second builds the path to the pulled point.
    geoms <- lapply(seq_len(nrow(edges)), function(i) {
      cv <- if (edges$curved[i]) curvature else 0
      d0 <- .edge_geom(edges$sx[i], edges$sy[i], edges$ex[i], edges$ey[i], cv)
      .edge_geom(edges$sx[i], edges$sy[i],
                 edges$ex[i] - d0$dirx * arrow_gap,
                 edges$ey[i] - d0$diry * arrow_gap, cv)
    })

    shaft_df <- do.call(rbind, lapply(seq_len(nrow(edges)), function(i) {
      g <- geoms[[i]]
      data.frame(id = i, x = g$x, y = g$y, ecol = edges$ecol[i],
                 lw = edges$lw[i], lty = edges$lty[i],
                 stringsAsFactors = FALSE)
    }))
    tip_df <- do.call(rbind, lapply(seq_len(nrow(edges)), function(i) {
      g  <- geoms[[i]]; n <- length(g$x)
      ex <- g$x[n]; ey <- g$y[n]
      data.frame(id = i,
                 x  = ex - g$dirx * arrow_size,
                 y  = ey - g$diry * arrow_size,
                 xe = ex, ye = ey,
                 ecol = edges$ecol[i], lw = edges$lw[i],
                 stringsAsFactors = FALSE)
    }))
    ## Force-level label sits on the shaft midpoint --- the mean of the two
    ## endpoints for a straight edge, the arc's mid-sample for a curved one
    ## --- formatted in the house no-leading-zero style.
    lab_df <- do.call(rbind, lapply(seq_len(nrow(edges)), function(i) {
      g <- geoms[[i]]; n <- length(g$x)
      if (n <= 2L) { lx <- mean(g$x); ly <- mean(g$y) }
      else        { m <- (n + 1L) %/% 2L; lx <- g$x[m]; ly <- g$y[m] }
      data.frame(x = lx, y = ly, elab = edges$elab[i], stringsAsFactors = FALSE)
    }))

    p <- p +
      ggplot2::geom_path(
        data = shaft_df,
        ggplot2::aes(x = .data$x, y = .data$y, group = .data$id,
                     colour = .data$ecol, linewidth = .data$lw,
                     linetype = .data$lty),
        lineend = "butt", linejoin = "round") +
      ggplot2::geom_segment(
        data = tip_df,
        ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xe, yend = .data$ye,
                     colour = .data$ecol),
        linewidth = arrow_stroke,
        arrow = grid::arrow(length = grid::unit(arrow_inches, "inches"),
                            type = "closed"),
        lineend = "butt", linejoin = "mitre") +
      ggplot2::scale_colour_identity() +
      ggplot2::scale_linewidth_identity() +
      ggplot2::scale_linetype_identity()
  }
  ## Node bodies, value-shaded on a diverging red-white-blue scale
  ## (negative red, zero white, positive blue) --- the same convention used
  ## by the pathXMY diagrams. Glyph nodes are shaded the same way; only
  ## their label differs (a centred symbol, no caption beneath).
  p <- p +
    ggplot2::geom_polygon(
      data = poly_df,
      ggplot2::aes(x = .data$px, y = .data$py, group = .data$name,
                   fill = .data$value),
      colour = "black", linewidth = 0.5) +
    ggplot2::scale_fill_gradient2(
      low = fill_low, mid = fill_mid, high = fill_high, midpoint = 0,
      limits = fill_limits, oob = scales::squish, na.value = na_fill,
      name = "state")
  ## Glyph nodes carry a centred symbol in place of a caption beneath. A
  ## lightning bolt ("bolt" or the emoji) is drawn as a filled polygon so it
  ## sits exactly centred; any other glyph string is drawn as centred text.
  if (any(is_glyph)) {
    gl       <- layout[is_glyph, , drop = FALSE]
    is_bolt  <- gl$glyph %in% c("bolt", "⚡")
    if (any(is_bolt)) {
      gb <- gl[is_bolt, , drop = FALSE]
      bolt_df <- do.call(rbind, lapply(seq_len(nrow(gb)), function(k) {
        b <- .bolt_polygon(gb$x[k], gb$y[k], node_size * 0.92)
        data.frame(name = gb$name[k], px = b$px, py = b$py,
                   stringsAsFactors = FALSE)
      }))
      p <- p + ggplot2::geom_polygon(
        data = bolt_df,
        ggplot2::aes(x = .data$px, y = .data$py, group = .data$name),
        fill = glyph_color, colour = "black", linewidth = 0.4,
        linejoin = "mitre", inherit.aes = FALSE)
    }
    if (any(!is_bolt)) {
      p <- p + ggplot2::geom_text(
        data = gl[!is_bolt, , drop = FALSE],
        ggplot2::aes(x = .data$x, y = .data$y, label = .data$glyph),
        size = glyph_size)
    }
  }
  ## Captions beneath the non-glyph nodes.
  p <- p +
    ggplot2::geom_text(
      data = layout[!is_glyph, , drop = FALSE],
      ggplot2::aes(x = .data$x, y = .data$y - node_size - label_pad,
                   label = .data$label),
      vjust = 1, size = label_size)
  if (have_edges && edge_labels) {
    p <- p + ggtext::geom_richtext(
      data = lab_df,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$elab),
      size = edge_label_size, label.size = 0,
      label.padding = grid::unit(0.1, "lines"), fill = "white")
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
