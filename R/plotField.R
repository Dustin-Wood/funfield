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

## A hollow "block arrow" silhouette traced along a sampled path: a thin
## rectangular shaft of half-width `hw` that opens into a triangular head
## (half-width `hh`, length `hl`) at the tip. Returned as one closed polygon
## (`px`, `py`) so it can be drawn with a grey outline and white fill to mark a
## *potential* (not-yet-active) path. `NULL` for a degenerate path.
.block_arrow <- function(px, py, hw, hh, hl) {
  n <- length(px)
  if (n < 2L) return(NULL)
  dx <- diff(px); dy <- diff(py)
  seg <- sqrt(dx^2 + dy^2)
  L   <- sum(seg)
  if (L < 1e-9) return(NULL)
  hl <- min(hl, L * 0.85)                 # head no longer than the whole arrow
  cs <- c(0, cumsum(seg))
  base_s <- L - hl                        # arc-length where the head begins
  keep <- which(cs <= base_s); if (!length(keep)) keep <- 1L
  j <- max(keep)
  if (j < n) {
    t  <- (base_s - cs[j]) / seg[j]
    bx <- px[j] + t * dx[j]; by <- py[j] + t * dy[j]
  } else { bx <- px[n]; by <- py[n] }
  shx <- c(px[keep], bx); shy <- c(py[keep], by)   # shaft path to the head base
  m <- length(shx)
  nx <- ny <- numeric(m)                  # left-hand unit normal per shaft point
  for (k in seq_len(m)) {
    if      (k == 1L) { tx <- shx[2] - shx[1];     ty <- shy[2] - shy[1] }
    else if (k == m)  { tx <- shx[m] - shx[m - 1]; ty <- shy[m] - shy[m - 1] }
    else              { tx <- shx[k+1] - shx[k-1]; ty <- shy[k+1] - shy[k-1] }
    tl <- sqrt(tx^2 + ty^2); if (tl < 1e-9) tl <- 1
    nx[k] <- -ty / tl; ny[k] <- tx / tl
  }
  bnx <- nx[m]; bny <- ny[m]              # normal at the head base (for wings)
  tipx <- px[n]; tipy <- py[n]
  left_x  <- shx + nx * hw; left_y  <- shy + ny * hw
  right_x <- shx - nx * hw; right_y <- shy - ny * hw
  list(px = c(left_x, bx + bnx * hh, tipx, bx - bnx * hh, rev(right_x)),
       py = c(left_y, by + bny * hh, tipy, by - bny * hh, rev(right_y)))
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
#' **every structural force** in the model (any non-zero coefficient), and
#' coloured by whether its condition is currently met: a **potential** path,
#' whose gate is not yet on, is light grey; it darkens to its full colour as
#' the gate goes on. Stepping `plotField()` across the rows of a
#' [simulateF()] trajectory and stitching the frames with [plotsAsWidget()]
#' produces a back/forward animation in which the whole field is visible
#' throughout and the active sub-path lights up as conditions are met.
#'
#' @details
#' Each regression row of `model` (`Y ~ label * X` or
#' `Y ~ label * X:Z`) is read through its `fZ_X.Y` label (see
#' `vignette("notation", package = "funfield")`): the label names the
#' source `X` and target `Y`, and any remaining variable in the
#' right-hand-side interaction is the **gate** `Z`. The edge `X -> Y` is
#' drawn whenever the coefficient is non-zero, and its **activation** ---
#' `prod(s[gate])`, or `1` for an ungated standing force --- sets its colour:
#' light grey (`potential_color`) when 0, the base colour when 1. The source
#' node `X` is coloured by `s[X]`, so an open-but-unfired path shows as a
#' black edge leaving a still-white node. Terms with a fixed coefficient
#' (e.g. `1 * X`) are valued from that coefficient rather than `params`.
#'
#' **Encoding the force.** Each edge's coefficient is read off its
#' appearance: its **magnitude** sets the linewidth (and how far an *active*
#' edge fades toward white as `|f| -> 0`, capped at `edge_scale_max`), and
#' its **sign** sets the linetype (solid for a positive force, dashed for a
#' negative one). So a force of `1` is a strong solid line and a force of
#' `-0.1` a faint dashed one, and (with `edge_labels`) the level --- or, for
#' a gated path, its condition's name --- is printed on the shaft. The shaft
#' is drawn with
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
#' @param plan Optional `lavaan`-syntax sub-model string, or a [planF()]
#'   object. Edges whose source -> target identity appears in the plan take
#'   `plan_color` as their base colour. Typically the action-plan half of a
#'   stitched model. When a `planF` is supplied, its `$policy` supplies those
#'   edges, its `$prefix` defaults `plan_label`, and its step descriptions
#'   drive the corner **plan-list** (see `plan_legend`). Default `NULL` (no
#'   gold edges).
#' @param plan_label Optional single-character prefix naming the plan (e.g.
#'   `"a"` for "Plan A"). When supplied, each plan edge is labelled by its
#'   **step** in the plan -- the order its `action ~ condition` rule appears
#'   in `plan` -- as `<plan_label><step>` (so Plan A's four rules read
#'   `a1`, `a2`, `a3`, `a4`). Default `NULL` (plan edges keep their
#'   coefficient / condition label).
#' @param plan_ramp Logical; when `TRUE`, the **action plan's forces** are
#'   coloured on a green ramp (the "green means go" cue): each plan edge and its
#'   `<plan_label><step>` label take a colour stepped along `plan_colors` by
#'   plan order (first step = the ramp's first colour), shown when the edge's
#'   condition is met and `potential_color` (grey) when merely potential. Only
#'   the **forces** are recoloured --- action *nodes* keep the value scale
#'   (white when idle, blue on the turn they fire), so the plan adds no new node
#'   colour. When `FALSE`, plan edges take flat `plan_color` (gold). Default
#'   `FALSE`.
#' @param plan_colors Anchor colours for the `plan_ramp`, interpolated to one
#'   colour per plan step. Default `c("#00FF00", "#32CD32", "#41A317")`
#'   (lime -> limegreen -> dark lime green).
#' @param condition_ramp Logical; when `TRUE`, the **condition states** ---
#'   the moderators `Z` that gate the afforded forces (`Y ~ Z:X`) --- are
#'   coloured on a gold -> orange ramp instead of the red-white-blue value
#'   scale. Each condition node, the gated force it opens, and that force's
#'   label take a colour stepped along `condition_colors` by the condition's
#'   order of appearance (earliest = the ramp's first colour). The node fills
#'   with its colour when present and is white when absent; the gated force and
#'   its label show the colour when the condition is met and `potential_color`
#'   (grey) when not. Actions and non-condition states keep the diverging
#'   value scale. Default `FALSE`. (With `plan_ramp` this gives two parallel
#'   staircases: a green action ramp and a gold condition ramp, kept clear of
#'   each other and of the red-white-blue value scale.)
#' @param condition_colors Anchor colours for the `condition_ramp`,
#'   interpolated to one colour per condition. Default
#'   `c("#F6BE00", "#9A6000")` (gold -> deep amber), kept clear of the
#'   value-red and the green action ramp.
#' @param conjunctive Character vector of target node names whose incoming
#'   edges are **conjunctive** --- the node fires only when *all* of them are
#'   on (a product, not a sum). Those edges are drawn **dotted** and carry no
#'   numeric label, so they are not mistaken for additive regression weights.
#'   Default `NULL`.
#' @param condition_labels Logical; when `TRUE` (default), a **gated** edge
#'   (from an interaction `Y ~ Z:X`, where `Z` is the condition that opens
#'   the `X -> Y` force) is labelled by its condition's displayed name ---
#'   e.g. `TurnOn -> HCoPot`, gated on `s2`, reads "rig set" --- rather than
#'   by the coefficient. Ungated paths keep their numeric weight. Set `FALSE`
#'   to label every edge by its coefficient.
#' @param edge_color,plan_color Base (fully active) colours for situation
#'   edges and plan-contributed edges --- the colour a path takes when its
#'   condition is fully met. Defaults `"black"` and `"#DAA520"` (gold).
#' @param potential_color Colour of a **potential** path --- one drawn from
#'   the model structure but whose condition is not yet met. An edge
#'   interpolates from this toward its base colour as its condition's
#'   activation goes 0 -> 1. Default `"grey80"` (light but legible).
#' @param fill_limits Numeric length-2 range mapped to the diverging fill
#'   scale. Default `c(-1, 1)`; values outside are squished to the ends.
#' @param fill_low,fill_mid,fill_high Diverging fill colours at the low,
#'   zero, and high ends. Default red `"#b73712"` / white / blue
#'   `"#1572da"`, midpoint `0`.
#' @param na_fill Fill for layout nodes absent from `s`. Default
#'   `"grey90"`.
#' @param edge_min Minimum absolute coefficient for a path to be drawn at
#'   all. Default `1e-9` (draw any structural force).
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
#' @param hollow_potential Logical; when `TRUE` (default), a **potential**
#'   path --- one whose condition is less than half met --- is drawn as a
#'   **hollow block arrow**: a thin grey perimeter outline with a white
#'   interior (a rectangular shaft opening into an arrowhead), rather than a
#'   solid line. This keeps a merely available path from blending with a faint
#'   small-magnitude *active* force, which keeps its solid colour-filled line.
#'   Conjunctive (dotted) edges are exempt. Set `FALSE` to draw every edge as a
#'   solid line.
#' @param edge_labels Logical: label each drawn edge with its resolved
#'   force level. Default `TRUE`.
#' @param edge_label_fmt Function formatting the resolved coefficient for
#'   the edge label. Default the no-leading-zero house style with trailing
#'   zeros trimmed (`1`, `.9`, `-.1`).
#' @param plan_legend Logical; when `TRUE` (default) and `plan` is a [planF()]
#'   object, draw the **plan-list** in the upper-left corner: a "Plan <label>"
#'   heading then one line per step (`a1: prepare coffee machine`, ...), each in
#'   the green ramp colour of its arrow, with the step whose policy arrow is
#'   **active** in this frame --- its condition is met, so it is the action the
#'   agent intends next --- shown in **bold**. Has no effect for a plain-string
#'   `plan`.
#' @param plan_legend_x,plan_legend_y Data coordinates of the plan-list's
#'   top-left anchor. Default `NULL` --- the layout's upper-left
#'   (`min(x)`, `max(y)`), which the staircase layouts leave empty.
#' @param plan_legend_size Text size for the plan-list. Default `3.2`.
#' @param plan_legend_title Heading for the plan-list. Default `NULL` ---
#'   `"Plan <label>"` from the `planF` object.
#' @param active_min Minimum activation of a plan step's condition
#'   (`prod(s[condition])`) for it to count as the **active** step --- the one
#'   bolded in the plan-list and lit green in the diagram. Default `0.5`.
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
                      plan             = NULL,
                      plan_label       = NULL,
                      plan_ramp        = FALSE,
                      plan_colors      = c("#00FF00", "#32CD32", "#41A317"),
                      conjunctive      = NULL,
                      condition_labels = TRUE,
                      condition_ramp   = FALSE,
                      condition_colors = c("#F6BE00", "#9A6000"),
                      edge_color       = "black",
                      plan_color       = "#DAA520",
                      potential_color  = "grey80",
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
                      hollow_potential = TRUE,
                      edge_labels     = TRUE,
                      edge_label_fmt  = function(x) {
                        s <- f0(x, digits = 2)
                        s <- sub("0+$", "", s)
                        sub("\\.$", "", s)
                      },
                      plan_legend       = TRUE,
                      plan_legend_x     = NULL,
                      plan_legend_y     = NULL,
                      plan_legend_size  = 3.2,
                      plan_legend_title = NULL,
                      active_min        = 0.5,
                      title = NULL) {

  ## A `planF` plan object carries its own policy string, step prefix, and the
  ## per-step descriptions/colours used to draw the corner plan-list. Unpack it
  ## here so the rest of the function sees the policy as a plain string.
  plan_obj <- NULL
  if (inherits(plan, "planF")) {
    plan_obj <- plan
    plan     <- plan_obj$policy
    if (is.null(plan_label)) plan_label <- plan_obj$prefix
  }

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
    rhs_vars  <- strsplit(pt$rhs[i], ":", fixed = TRUE)[[1]]
    parsed    <- .field_parse_label(lbl)
    ## Source comes from the label; failing that, the convention places
    ## the action/source last (`Z:X`), so fall back to the final rhs var.
    src     <- if (!is.null(parsed)) parsed$source else rhs_vars[length(rhs_vars)]
    gate    <- setdiff(rhs_vars, src)
    pair    <- paste(src, pt$lhs[i], sep = "\r")
    is_plan <- pair %in% plan_pairs
    step    <- if (is_plan) match(pair, plan_pairs) else NA_integer_
    ## Resolve the edge's coefficient: from `params` for a labelled term, from
    ## the fixed start value for a `c * X` term, and --- so a bare policy edge
    ## (`Do ~ Ready`, no coefficient) still draws --- a nominal `1` for a
    ## recognised plan edge. Any other free/unfixed term has no force to show.
    if (is_lab) {
      if (!lbl %in% names(params)) next
      coef <- params[[lbl]]
    } else if (pt$free[i] == 0 && !is.na(pt$ustart[i])) {
      coef <- pt$ustart[i]                       # fixed structural coef
    } else if (is_plan) {
      coef <- 1                                  # bare policy edge: nominal 1
    } else next
    if (is.na(coef) || abs(coef) <= edge_min) next   # no structural force
    ## Activation: how "on" the path's condition is. A gated situation force
    ## (`Y ~ Z:X`) is afforded to the degree its gate `Z` is present; a plan
    ## rule (`action ~ condition`) to the degree its condition (the source)
    ## is; an ungated standing force is always on. The edge is drawn always
    ## (its structure), coloured by this --- light grey when 0 (a *potential*
    ## path), full colour at 1.
    act <- if (length(gate)) prod(s[gate]) else if (is_plan) s[[src]] else 1
    edge_rows[[length(edge_rows) + 1L]] <-
      data.frame(from = src, to = pt$lhs[i], coef = coef,
                 act  = if (is.na(act)) 0 else act,
                 gate = if (length(gate)) paste(gate, collapse = ":") else "",
                 base = if (is_plan) plan_color else edge_color,
                 is_plan = is_plan, step = step,
                 stringsAsFactors = FALSE)
  }

  cond_color <- character(0)   # condition -> gold-ramp colour (set if ramp on)
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

    ## Edge label: by default a *gated* path (an interaction `Y ~ Z:X`, where
    ## Z is a known condition) is labelled by its condition --- its
    ## displayed-name --- rather than by the coefficient, since for these
    ## paths the informative thing is "fires when Z is on", not the (usually
    ## fixed) weight. Ungated paths keep their numeric coefficient.
    cond_label <- function(g) {
      vars <- strsplit(g, ":", fixed = TRUE)[[1]]
      labs <- vapply(vars, function(v) {
        k <- match(v, layout$name)
        gsub("\n", " ", if (!is.na(k)) layout$label[k] else v)
      }, character(1))
      paste(labs, collapse = " & ")
    }
    num_lab  <- edge_label_fmt(edges$coef)
    gated    <- condition_labels & nzchar(edges$gate)
    edges$elab <- num_lab
    if (any(gated))
      edges$elab[gated] <- vapply(edges$gate[gated], cond_label, character(1))

    ## Plan-step labels: each plan edge is named by its step in the plan ---
    ## the order its `action ~ condition` rule appears --- as
    ## `<plan_label><step>` (Plan A -> a1, a2, ...).
    if (!is.null(plan_label) && any(edges$is_plan))
      edges$elab[edges$is_plan] <-
        paste0(plan_label, edges$step[edges$is_plan])

    ## Action staircase (green ramp). "Green means go": when `plan_ramp` is on,
    ## each plan edge and its step label take a colour stepped along
    ## `plan_colors` (lime -> dark green) by plan order. Only the FORCES are
    ## recoloured --- the action *nodes* stay on the value scale (white when
    ## idle, blue on the turn they fire), so green marks what the plan *does*
    ## without adding another node colour.
    if (plan_ramp && any(edges$is_plan)) {
      pramp <- grDevices::colorRampPalette(plan_colors)(length(plan_pairs))
      ip <- which(edges$is_plan)
      edges$base[ip] <- pramp[edges$step[ip]]
    }

    ## Condition ramp. The moderators Z that gate the afforded forces
    ## (`Y ~ Z:X`) carry a gold -> orange colour, ordered by appearance, shared
    ## by the condition node, the gated force it opens, and that force's label.
    ## Recolour the gated (non-plan) edges here; node fills are held in
    ## `cond_color` and applied when the nodes are drawn.
    if (condition_ramp) {
      gidx <- which(nzchar(edges$gate) & !edges$is_plan)
      cond_order <- unique(unlist(
        strsplit(edges$gate[gidx], ":", fixed = TRUE)))
      if (length(cond_order)) {
        cramp <- grDevices::colorRampPalette(condition_colors)(length(cond_order))
        cond_color <- stats::setNames(cramp, cond_order)
        for (i in gidx) {
          gv <- strsplit(edges$gate[i], ":", fixed = TRUE)[[1]]
          gv <- gv[gv %in% names(cond_color)]
          if (length(gv)) edges$base[i] <- cond_color[[gv[length(gv)]]]
        }
      }
    }

    ## Structure vs. activation. The edge's *structure* is fixed: magnitude
    ## |coef| sets the linewidth, sign sets the linetype. Its *colour* tracks
    ## activation `act` (the gate value): a fully active path (act = 1) takes
    ## its base colour (faded toward white only by small magnitude), a
    ## potential path (act = 0) is drawn in light grey, and partial gates
    ## interpolate. So a path appears as soon as it exists in the model and
    ## darkens to black exactly when its condition is met.
    mag_ratio   <- pmin(abs(edges$coef) / edge_scale_max, 1)
    edges$mag_ratio <- mag_ratio
    edges$lw    <- mag_ratio^1.4 * (edge_linewidth - 0.45) + 0.45
    edges$lty   <- ifelse(edges$coef < 0, "dashed", "solid")

    edges$conj  <- edges$to %in% conjunctive   # see below; affects activation
    fade        <- (1 - mag_ratio)^3
    base_rgb    <- t(grDevices::col2rgb(edges$base) / 255)
    active_rgb  <- base_rgb + (1 - base_rgb) * fade           # act == 1 colour
    pot_rgb     <- matrix(grDevices::col2rgb(potential_color) / 255,
                          nrow = nrow(edges), ncol = 3, byrow = TRUE)
    ## A conjunctive input lights with the *source's* own presence (each
    ## ingredient of the AND), not a separate gate.
    act <- edges$act
    if (any(edges$conj)) act[edges$conj] <- s[edges$from[edges$conj]]
    act <- pmin(pmax(ifelse(is.na(act), 0, act), 0), 1)
    mix <- pot_rgb + (active_rgb - pot_rgb) * act
    edges$ecol <- grDevices::rgb(mix[, 1], mix[, 2], mix[, 3])

    ## Potential (not-yet-active) paths are drawn as a *hollow block arrow* --- a
    ## thin grey outline with a white interior (a rectangular shaft opening into
    ## an arrowhead) --- rather than a solid line. This keeps a merely available
    ## path from blending with a faint small-magnitude *active* force, which
    ## keeps its solid, colour-filled line. A path counts as potential when its
    ## condition is less than half on. Conjunctive (dotted) edges keep their
    ## product styling rather than becoming a block arrow.
    edges$hollow <- hollow_potential & act < 0.5 & !edges$conj

    ## Conjunctive ("joint") inputs: edges into a node that fires only when
    ## *all* of them are on are drawn dotted (a product, not an additive
    ## coefficient) and carry no numeric label --- the dotted style says it.
    if (any(edges$conj)) {
      edges$lty[edges$conj]  <- "dotted"
      edges$elab[edges$conj] <- NA_character_
      edges$lw[edges$conj]   <- 1.1   # structural, not magnitude-scaled
    }

    ## Consumption ('use') edges are drawn as curved arcs, always --- so a
    ## consumption path reads the same whether or not anything else runs
    ## alongside it. A consumption term is `Y ~ use * Y:trigger`: the target
    ## node `Y` is consumed by its own outflow, so it appears in its own gate.
    ## (Also curve any other negative edge that runs anti-parallel to a forward
    ## edge, so the two do not overlay.)
    consumes <- mapply(function(to, g)
      nzchar(g) && to %in% strsplit(g, ":", fixed = TRUE)[[1]],
      edges$to, edges$gate)
    e_key <- paste(edges$from, edges$to, sep = "\r")
    e_rev <- paste(edges$to,   edges$from, sep = "\r")
    edges$curved <- consumes | (edges$coef < 0 & e_rev %in% e_key)
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

    ## Active / structural forces (`solid`) are drawn as a line shaft plus a
    ## solid closed arrowhead; potential forces (`hollow`) are drawn as an
    ## outlined block-arrow polygon (grey perimeter, white fill). Split them.
    solid_idx  <- which(!edges$hollow)
    hollow_idx <- which( edges$hollow)

    shaft_df <- if (length(solid_idx))
      do.call(rbind, lapply(solid_idx, function(i) {
        g <- geoms[[i]]
        data.frame(id = i, x = g$x, y = g$y, ecol = edges$ecol[i],
                   lw = edges$lw[i], lty = edges$lty[i],
                   stringsAsFactors = FALSE)
      })) else NULL
    tip_df <- if (length(solid_idx))
      do.call(rbind, lapply(solid_idx, function(i) {
        g  <- geoms[[i]]; n <- length(g$x)
        ex <- g$x[n]; ey <- g$y[n]
        data.frame(id = i,
                   x  = ex - g$dirx * arrow_size,
                   y  = ey - g$diry * arrow_size,
                   xe = ex, ye = ey,
                   ecol = edges$ecol[i], lw = edges$lw[i],
                   stringsAsFactors = FALSE)
      })) else NULL

    ## Hollow block-arrow polygons. The shaft half-width grows a little with the
    ## force magnitude (so a strong potential force is a wider hollow arrow), and
    ## the head fans out past it; all in data units, scaling with `node_size`.
    block_df <- if (length(hollow_idx))
      do.call(rbind, lapply(hollow_idx, function(i) {
        g  <- geoms[[i]]
        hw <- node_size * (0.05 + 0.10 * edges$mag_ratio[i])
        ba <- .block_arrow(g$x, g$y, hw,
                           hh = node_size * 0.30, hl = node_size * 0.55)
        if (is.null(ba)) return(NULL)
        data.frame(id = i, x = ba$px, y = ba$py, ecol = edges$ecol[i],
                   stringsAsFactors = FALSE)
      })) else NULL

    ## Force-level label sits on the shaft midpoint --- the mean of the two
    ## endpoints for a straight edge, the arc's mid-sample for a curved one
    ## --- formatted in the house no-leading-zero style.
    lab_df <- do.call(rbind, lapply(seq_len(nrow(edges)), function(i) {
      g <- geoms[[i]]; n <- length(g$x)
      if (n <= 2L) { lx <- mean(g$x); ly <- mean(g$y) }
      else        { m <- (n + 1L) %/% 2L; lx <- g$x[m]; ly <- g$y[m] }
      data.frame(x = lx, y = ly, elab = edges$elab[i], ecol = edges$ecol[i],
                 stringsAsFactors = FALSE)
    }))
    lab_df <- lab_df[!is.na(lab_df$elab) & nzchar(lab_df$elab), , drop = FALSE]

    head_closed <- grid::arrow(length = grid::unit(arrow_inches, "inches"),
                               type = "closed")
    ## Hollow block arrows first (so any active edge sits on top of them).
    if (!is.null(block_df) && nrow(block_df))
      p <- p + ggplot2::geom_polygon(
        data = block_df,
        ggplot2::aes(x = .data$x, y = .data$y, group = .data$id,
                     colour = .data$ecol),
        fill = "white", linewidth = 0.5)
    if (!is.null(shaft_df))
      p <- p + ggplot2::geom_path(
        data = shaft_df,
        ggplot2::aes(x = .data$x, y = .data$y, group = .data$id,
                     colour = .data$ecol, linewidth = .data$lw,
                     linetype = .data$lty),
        lineend = "butt", linejoin = "round")
    if (!is.null(tip_df))
      p <- p + ggplot2::geom_segment(
        data = tip_df,
        ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xe, yend = .data$ye,
                     colour = .data$ecol),
        linewidth = arrow_stroke, arrow = head_closed,
        lineend = "butt", linejoin = "mitre")
    p <- p +
      ggplot2::scale_colour_identity() +
      ggplot2::scale_linewidth_identity() +
      ggplot2::scale_linetype_identity()
  }
  ## Only CONDITION nodes leave the diverging value scale (one extra node
  ## colour, to avoid a proliferation): a condition fills white -> its gold-ramp
  ## colour by how present it is. Actions and ordinary states keep the value
  ## scale. Each condition node is drawn with a constant per-node fill (no fill
  ## aesthetic) so it sits outside the gradient scale, and is removed from the
  ## gradient layer below.
  special_fill <- character(0)
  for (nm in intersect(names(cond_color), layout$name)) {
    v  <- unname(s[nm]); if (is.na(v)) v <- 0
    v  <- min(max(v, 0), 1)
    cr <- grDevices::col2rgb(cond_color[[nm]]) / 255
    mx <- c(1, 1, 1) + (cr[, 1] - c(1, 1, 1)) * v
    special_fill[nm] <- grDevices::rgb(mx[1], mx[2], mx[3])
  }
  ## Node bodies, value-shaded on a diverging red-white-blue scale
  ## (negative red, zero white, positive blue) --- the same convention used
  ## by the pathXMY diagrams. Glyph nodes are shaded the same way; only
  ## their label differs (a centred symbol, no caption beneath).
  grad_df <- poly_df[!poly_df$name %in% names(special_fill), , drop = FALSE]
  p <- p +
    ggplot2::geom_polygon(
      data = grad_df,
      ggplot2::aes(x = .data$px, y = .data$py, group = .data$name,
                   fill = .data$value),
      colour = "black", linewidth = 0.5) +
    ggplot2::scale_fill_gradient2(
      low = fill_low, mid = fill_mid, high = fill_high, midpoint = 0,
      limits = fill_limits, oob = scales::squish, na.value = na_fill,
      name = "state")
  for (nm in names(special_fill)) {
    p <- p + ggplot2::geom_polygon(
      data = poly_df[poly_df$name == nm, , drop = FALSE],
      ggplot2::aes(x = .data$px, y = .data$py, group = .data$name),
      fill = special_fill[[nm]], colour = "black", linewidth = 0.5,
      inherit.aes = FALSE)
  }
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
  if (have_edges && edge_labels && nrow(lab_df)) {
    p <- p + ggtext::geom_richtext(
      data = lab_df,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$elab,
                   colour = .data$ecol),
      size = edge_label_size, label.size = 0,
      label.padding = grid::unit(0.1, "lines"), fill = "white")
  }
  ## Corner plan-list. When a `planF` object is supplied, write the whole plan
  ## out in the upper-left: a "Plan A" heading then one line per step, each in
  ## its arrow's ramp colour, with the *active* step (the plan action whose
  ## state is on in this frame) bolded. Built as a single multi-line
  ## `ggtext::geom_richtext` so the per-line colours and bolding render as HTML.
  if (!is.null(plan_obj) && plan_legend) {
    st     <- plan_obj$steps
    ## Bold the step whose policy arrow is *active* --- its condition is
    ## currently met --- which is the action the agent intends to do next, and
    ## keeps the bolded step in lock-step with the green arrow that lights for
    ## it. (This is the same activation `prod(s[condition])` that colours the
    ## edge, not whether the action has already fired.)
    act_of <- function(cond) {
      cv <- trimws(sub("^.*\\*", "", strsplit(cond, ":", fixed = TRUE)[[1]]))
      v  <- s[cv]
      if (any(is.na(v))) 0 else prod(v)
    }
    active <- vapply(st$condition, act_of, numeric(1)) >= active_min
    desc   <- ifelse(is.na(st$desc) | !nzchar(st$desc),
                     paste(st$action, "~", st$condition), st$desc)
    body   <- sprintf("<span style='color:%s'>%s%s: %s%s</span>",
                      st$color,
                      ifelse(active, "<b>", ""),
                      st$step, desc,
                      ifelse(active, "</b>", ""))
    ttl    <- if (is.null(plan_legend_title)) paste("Plan", plan_obj$label)
              else plan_legend_title
    ## Heading in the green of the ramp's midpoint, so "Plan A" reads as the
    ## plan's own colour rather than a neutral black.
    mid_col <- grDevices::colorRampPalette(plan_obj$plan_colors)(3)[2]
    ttl_html <- sprintf("<span style='color:%s'><b>%s</b></span>", mid_col, ttl)
    html   <- paste(c(ttl_html, body), collapse = "<br>")
    lx     <- if (is.null(plan_legend_x)) min(layout$x) else plan_legend_x
    ly     <- if (is.null(plan_legend_y)) max(layout$y) else plan_legend_y
    leg_df <- data.frame(x = lx, y = ly, label = html, stringsAsFactors = FALSE)
    p <- p + ggtext::geom_richtext(
      data = leg_df,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      hjust = 0, vjust = 1, size = plan_legend_size, label.size = 0,
      label.padding = grid::unit(0.2, "lines"), fill = NA,
      inherit.aes = FALSE)
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
