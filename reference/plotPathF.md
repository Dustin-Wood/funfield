# Field-style path diagram for a pathF() cascade or DAG

Renders a
[`pathF`](https://dustin-wood.github.io/funfield/reference/pathF.md)
model as a ggplot2 object in the funfield visualization style,
generalizing
[`plotPathXMY`](https://dustin-wood.github.io/funfield/reference/plotPathXMY.md)
from the X-M-Y triangle/fan to causal cascades and arbitrary DAGs:

- **Layout by causal depth.** Each node's horizontal position is its
  longest-path depth from the model's source, so a serial cascade
  renders as a left-to-right chain and parallel mediators in a DAG stack
  vertically at the same depth (recovering the familiar fan).

- **Skip edges arc.** Edges between adjacent depths are straight; edges
  that skip one or more depths (e.g. the residual direct `X -> Y` path
  in a cascade) bow upward as quadratic arcs, with height scaled by the
  number of depths skipped. Labels sit at the arc apex.

- **Nodes** are colored by their implied expected score when the focal X
  is held at 1, propagated through the cascade (each node's score is the
  effective-coefficient-weighted sum of its predecessors' scores), on
  the same red-white-blue diverging gradient as
  [`plotPathXMY()`](https://dustin-wood.github.io/funfield/reference/plotPathXMY.md).
  Nodes not downstream of X render white.

- **Edges** follow the shared conventions: solid positive / dashed
  negative, linewidth and grayscale ramped by magnitude (capped at
  `scale_max`), arrowheads clipped to the node perimeter, and edge
  labels carrying the `f1 + fZ(Z)` decomposition with the fZ fragment in
  gold (`#F6BE00`).

- **View modes.** `Z_value` collapses each edge to its effective slope
  at that Z; `Z_overlay = TRUE` swaps every f1 for its fZ counterpart
  (all edges and labels gold, node fill suppressed). Mutually exclusive.

## Usage

``` r
plotPathF(
  x,
  labels = NULL,
  X_shape = c("rtTri", "square"),
  Y_shape = c("lfTri", "square"),
  digits = 2,
  show_pvalues = FALSE,
  strip0 = TRUE,
  scale_max = 0.8,
  score_intensity_max = 1,
  Z_value = NULL,
  Z_overlay = FALSE,
  Z_label = "Z",
  node_size = 0.07,
  label_pad = 0.03,
  x_spacing = 1.2,
  arc_height = 0.55,
  title = NULL,
  filename = NULL,
  filetype = "png",
  ...
)
```

## Arguments

- x:

  A [`pathF`](https://dustin-wood.github.io/funfield/reference/pathF.md)
  return, or a
  [`pathF_decompose`](https://dustin-wood.github.io/funfield/reference/pathF_decompose.md)
  return (its `$fits$full` model is used).

- labels:

  Optional named character vector mapping variable names to display
  labels, e.g. `c(Speed = "Speed", HCoPot = "Hot coffee")`. Unnamed
  variables keep their own names.

- X_shape:

  Shape for the focal X node: `"rtTri"` (default; X is typically an
  imagined action in the EXSJTs) or `"square"`.

- Y_shape:

  Shape for the focal Y node: `"lfTri"` (default; an action likelihood)
  or `"square"`.

- digits:

  Decimal places for edge labels (default 2).

- show_pvalues:

  Logical; append p-values to edge labels.

- strip0:

  Logical; strip leading zeros from edge-label decimals per the funfield
  house style (default `TRUE`).

- scale_max:

  Path magnitude mapping to maximum edge linewidth (default `0.8`; use a
  tighter cap such as `0.3` for `Z_overlay` views, where fZ magnitudes
  run smaller).

- score_intensity_max:

  Expected-score absolute value mapping to maximum node-fill intensity
  (default `1`).

- Z_value:

  Optional numeric scalar; render the field conditional on Z at this
  value (each edge becomes `f1 + fZ * Z_value`).

- Z_overlay:

  Logical; render the fZ field (per-unit-Z change in every path weight).
  Node fill is suppressed.

- Z_label:

  Display label for the moderator in decomposed edge labels (default
  `"Z"`).

- node_size:

  Half-extent of each node shape in data coordinates (default `0.07`).

- label_pad:

  Padding between node perimeter and its text label (default `0.03`).

- x_spacing:

  Horizontal distance between adjacent causal depths (default `1.2`).

- arc_height:

  Arc rise per skipped depth for non-adjacent edges (default `0.55`).

- title:

  Optional plot title.

- filename, filetype:

  Optional; save the plot via
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

- ...:

  Passed to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  when `filename` is supplied.

## Value

A `ggplot` object.

## See also

[`pathF`](https://dustin-wood.github.io/funfield/reference/pathF.md),
[`pathF_decompose`](https://dustin-wood.github.io/funfield/reference/pathF_decompose.md),
[`plotPathXMY`](https://dustin-wood.github.io/funfield/reference/plotPathXMY.md)
