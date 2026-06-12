# Symbolic path-schema diagram (X to Y, optionally through one M)

Renders a clean pedagogical schematic of the X to Y path (with an
optional single mediator M) as a ggplot2 object. Nodes are drawn as
white shapes (square by default; right- or left-facing triangle if
requested via `X_shape`/`Y_shape`), edges as uniform solid black arrows.
Edge labels are arbitrary user-supplied strings (typically coefficient
symbols) rather than fitted numbers, so the schematic can sit alongside
an algebraic identity term-for-term.

Two layouts:

- **Total path** (`M_label = NULL`): X on the left, Y on the right, a
  single arrow between them carrying `XY_label`.

- **Mediation triangle** (`M_label` set): X at bottom-left, M at the
  apex, Y at bottom-right. Edges `X to M` and `M to Y` carry `XM_label`
  and `MY_label`; if `XY_label` is also supplied, the direct `X to Y`
  residual arrow is drawn too.

Edge labels are passed through
[`geom_richtext`](https://wilkelab.org/ggtext/reference/geom_richtext.html),
so HTML markup is supported – e.g. `"&beta;<sub>1<sub>MX</sub></sub>"`
for proper Greek and nested subscripts.

## Usage

``` r
plotPathSchema(
  X_label = "X",
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
  ...
)
```

## Arguments

- X_label, Y_label:

  Node labels for X and Y. Default `"X"`, `"Y"`.

- M_label:

  Optional node label for the mediator. `NULL` (default) draws the total
  `X to Y` path only.

- XY_label, XM_label, MY_label:

  Edge labels (HTML supported). In the no-mediator layout, only
  `XY_label` is used. In the triangle layout, `XM_label` and `MY_label`
  carry the indirect arms; `XY_label` is optional and, when supplied,
  labels the direct `X to Y` residual arrow.

- X_shape, Y_shape:

  Shape for the X / Y node: `"square"` (default), `"rtTri"`
  (right-facing triangle, apex on the right – conventional for an
  "action" / initiating node), or `"lfTri"` (left-facing triangle, apex
  on the left – conventional for a likelihood / outcome node). Triangles
  are sized so their bounding box matches the square's, and edges are
  clipped against the shape's actual perimeter.

- XM_color, MY_color, XY_color:

  Per-edge arrow colors. All default to `"black"`. Use `"#F6BE00"`
  (gold, the bZ moderator convention matching
  [`plotField()`](https://dustin-wood.github.io/funfield/reference/plotField.md)
  condition nodes) when an arm represents a bZ moderation route, and a
  light gray (e.g. `"#BBBBBB"`) to fade arms that are not part of the
  term being highlighted in a panel. Ignored for any edge not drawn
  (e.g. `XY_color` when `XY_label` is NULL).

- node_size:

  Half-side of each node shape in data coordinates. Default `0.09`.

- label_pad:

  Padding between a node's perimeter and its text label, in data
  coordinates. Default `0.03`.

- text_size:

  Node-label text size. Default `4.5`.

- edge_text_size:

  Edge-label text size. Default `4.5`.

- edge_linewidth:

  Linewidth for edges. Default `0.9`.

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

A `ggplot` object.

## See also

[`plotPathXMY`](https://dustin-wood.github.io/funfield/reference/plotPathXMY.md)
for the data-driven counterpart that renders fitted coefficients on the
same layout.

## Examples

``` r
if (FALSE) { # \dontrun{
## Total path (default squares)
plotPathSchema(XY_label = "&beta;*<sub>1<sub>YX</sub></sub>")

## Single-mediator decomposition (default squares)
plotPathSchema(
  M_label  = "M",
  XM_label = "&beta;<sub>1<sub>MX</sub></sub>",
  MY_label = "&beta;<sub>1<sub>YM</sub></sub>",
  XY_label = "&beta;<sub>1<sub>YX</sub></sub>"
)

## Expectancy x Value styling: X = action (right-facing triangle),
## Y = likelihood (left-facing triangle).
plotPathSchema(
  M_label  = "M",
  XM_label = "&beta;<sub>1<sub>MX</sub></sub>",
  MY_label = "&beta;<sub>1<sub>YM</sub></sub>",
  XY_label = "&beta;<sub>1<sub>YX</sub></sub>",
  X_shape  = "rtTri",
  Y_shape  = "lfTri"
)
} # }
```
