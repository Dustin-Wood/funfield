# Field-style path diagram for an X-M-Y model

Renders the X to M to Y structure as a ggplot2 object using the funfield
visualization conventions:

- **Nodes** are colored by their expected score when X is held at 1, on
  a continuous diverging gradient (deep red to white to deep blue,
  midpoint at 0) mapped to the `[-1, 1]` range via
  [`ggplot2::scale_fill_gradient2()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html).
  The X node is by definition 1 (full blue); each mediator's score is
  its path coefficient \\b^1\_{MX}\\; Y's score is the sum of the direct
  and indirect contributions, \\b^1\_{YX} + \sum_j b^1\_{YM_j} \cdot
  b^1\_{MX_j}\\.

- **Shape** defaults to a true square for X and each mediator
  (continuous variables) and a left-facing triangle for Y (the focal
  action likelihood). `X_shape` and `Y_shape` override this: use
  `X_shape = "rtTri"` when X is itself an imagined action (e.g. the
  speeding / overtime EXSJTs), and `Y_shape = "square"` when Y is a
  regular continuous variable rather than an action likelihood.
  Mediators are always squares.

- **Labels** sit outside the shapes: above for any mediator (M), below
  for X and Y. This convention scales to the fan view.

- **Edges** are solid for positive paths and dashed for negative paths.
  Linewidth and edge color both scale with path magnitude (capped at
  `scale_max`): the largest paths render thick and near-black, and paths
  with coefficients near zero fade to a thin, near-white line so they
  recede into the background. Arrowheads are clipped to the node
  perimeter via shape-aware intersection (L-infinity for squares, edge
  intersection for the triangles). Edge labels carry the `f1 + fZ(Z)`
  decomposition.

- **Moderator (Z) coloring.** Anything that is purely a bZ coefficient
  renders in limegreen. In the default decomposed label, only the
  trailing `bZ(Z)` fragment is green (the b1 portion and the edge itself
  describe the normative path). In the Z-overlay view
  (`Z_overlay = TRUE`), the entire label and the edge are green, since
  the edge *is* a bZ coefficient. A Z-collapsed view (`Z_value`
  supplied) renders normally: the label there is a single total slope
  `f1 + fZ * Z_value`, not a bZ coefficient on its own.

Two view modes:

- **Triangle** (single mediator) – X at bottom-left, M at top, Y at
  bottom-right.

- **Fan** (multiple mediators) – X at left, mediators stacked in the
  middle, Y at right. With an odd number of mediators the stack is laid
  out asymmetrically so the straight X to Y arrow has a clean gap at the
  y = 0 axis.

`from` controls which fit drives the diagram:

- `"loop"` (default): per-mediator loop overlay. In fan view the overlay
  is intentionally pure – no residual direct X to Y arrow is drawn,
  since the only sensible direct path in that picture would have to come
  from a different (joint) fit. In single-mediator triangle view, the X
  to Y direct path is that one mediator's loop residual.

- `"joint"`: the joint multi-mediator simultaneous-fit picture – each
  mediator's slopes are the partial slopes net of the other mediators,
  and the X to Y arrow is the single global `f1_XY_joint` direct path
  after controlling for all M.

Passing `Z_overlay = TRUE` renders the same layout but with each B1
coefficient swapped for its BZ counterpart – a clean view of the
moderator's per-unit effect on path weights, isolated from the normative
field. Nodes are drawn white in that view since expected scores have no
meaning for a per-unit-Z slope.

Passing `route = "expectation"` or `"valuation"` renders a per-route
view of the moderation. The *expectation route* shows `fZ_XM` on the
X-to-M arm (green) and `f1_MY` on the M-to-Y arm (black); the *valuation
route* flips the arms (`f1_XM` on the left, `fZ_MY` on the right). The
visual grammar of the new coloring scheme makes the two route components
of the moderation decomposition read directly: if both arms of a
mediator carry visible weight in one of the two graphs, that green–black
or black–green chain identifies a *reason* that the moderator shifts the
X-to-Y relationship. Mutually exclusive with `Z_value` and `Z_overlay`;
nodes are drawn white in route views for the same reason as the Z
overlay.

## Usage

``` r
plotPathXMY(
  x,
  mediator = NULL,
  X_label = "X",
  Y_label = "Y",
  Z_label = "Z",
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
  ...
)
```

## Arguments

- x:

  A
  [`pathXMY`](https://dustin-wood.github.io/funfield/reference/pathXMY.md)
  or
  [`pathXMY_decompose`](https://dustin-wood.github.io/funfield/reference/pathXMY_decompose.md)
  return.

- mediator:

  Character vector. If `NULL` (default) and `x` has multiple mediators,
  a fan diagram of all of them is drawn. Pass a single name for the
  triangle view. Pass a subset for a smaller fan.

- X_label, Y_label, Z_label:

  Display labels for the X, Y, and Z nodes.

- M_labels:

  Optional character vector of labels for the mediators, parallel to
  `mediator` (or to the natural order in the fit). If `NULL`, the
  mediator variable names are used.

- X_shape:

  Shape for the X node: `"square"` (default; X is a regular variable) or
  `"rtTri"` (right-facing triangle; X is an imagined action, as in the
  speeding and overtime EXSJTs).

- Y_shape:

  Shape for the Y node: `"lfTri"` (default; Y is an action likelihood,
  the typical focal outcome) or `"square"` (Y is a regular continuous
  variable). Mediators are always drawn as squares.

- digits:

  Decimal places for edge labels (default 2).

- show_pvalues:

  Logical; append p-values to edge labels.

- strip0:

  Logical; if `TRUE` (default), edge-label coefficients render without
  their leading zero (`0.31 -> .31`) in keeping with the funfield
  decimal house style. Set to `FALSE` to keep the leading zero.

- scale_max:

  Numeric. Path magnitude that maps to the maximum edge linewidth.
  Default `0.8`.

- score_intensity_max:

  Numeric. The expected-score absolute value that maps to maximum color
  intensity. Default `1` (the `[-1, 1]` range).

- Z_value:

  Optional numeric scalar. If supplied, the diagram is rendered
  *conditional on Z at this value* – every coefficient becomes the
  effective slope at that Z (`f1 + fZ * Z_value`), node colors reflect
  expected scores at that Z, and edge labels show the single effective
  coefficient rather than the `f1 + fZ(Z)` decomposition. See also
  [`plotPathXMY_ZLH`](https://dustin-wood.github.io/funfield/reference/plotPathXMY_ZLH.md)
  for a paired low/high view.

- Z_overlay:

  Logical. If `TRUE`, render the *Z-overlay*: the same graph layout as
  the normative model, but each B1 coefficient is swapped for its BZ
  counterpart – i.e. the per-unit-Z change in path weight. Nodes are
  drawn white since expected scores have no meaning in this view.
  Mutually exclusive with `Z_value` and `route`.

- route:

  One of `"none"` (default), `"expectation"`, or `"valuation"`. Selects
  a per-route moderation view rather than the decomposed default.
  `"expectation"` puts `fZ_XM` on the X-to-M arm (green) and `f1_MY` on
  the M-to-Y arm (black); `"valuation"` flips them (`f1_XM` green-less
  left, `fZ_MY` green right). No direct X-to-Y arrow is drawn since the
  route concept is about the indirect path through M. Mutually exclusive
  with `Z_value` and `Z_overlay`.

- from:

  One of `"loop"` (default) or `"joint"`: which tidy table on the
  [`pathXMY()`](https://dustin-wood.github.io/funfield/reference/pathXMY.md)
  return drives the diagram. See the description for behavior. The joint
  view requires that the fit was produced with `joint = TRUE` (the
  default) and `length(M) > 1`.

- node_size:

  Half-side of each square / half-bounding-box of the triangle, in data
  coordinates (default `0.07`).

- label_pad:

  Padding between a node's perimeter and its text label, in data
  coordinates (default `0.025`).

- title:

  Optional plot title.

- filename, filetype:

  Optional. If `filename` is supplied, the plot is also saved via
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  using `filetype` as the device (e.g., `"png"`, `"pdf"`).

- ...:

  Additional arguments passed to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  when `filename` is supplied (ignored otherwise).

## Value

A `ggplot` object. Auto-prints in interactive sessions and knitr chunks;
assign to a variable to suppress and reuse.

## See also

[`pathXMY`](https://dustin-wood.github.io/funfield/reference/pathXMY.md),
[`pathXMY_decompose`](https://dustin-wood.github.io/funfield/reference/pathXMY_decompose.md),
[`plotPathXMY_ZLH`](https://dustin-wood.github.io/funfield/reference/plotPathXMY_ZLH.md),
[`plotPathXMY_widget`](https://dustin-wood.github.io/funfield/reference/plotPathXMY_widget.md)
