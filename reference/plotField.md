# Draw one frame of a functional field at a given state

Renders a functional field \`F\` evaluated at a single state \`s\` as a
ggplot2 node-link diagram. Nodes are placed at caller-supplied
coordinates and drawn as shapes coloured by their current state value
(white at the low limit, blue at the high limit). An edge is drawn for
every force that is currently \*\*afforded\*\* — i.e., whose resolved
field coefficient is non-zero given \`s\`. Stepping \`plotField()\`
across the rows of a \[runF()\] trajectory and stitching the frames with
\[plotsAsWidget()\] produces a back/forward animation of forces being
introduced as gates open and state propagating through the field.

## Usage

``` r
plotField(
  model,
  params,
  s,
  layout,
  plan = NULL,
  edge_color = "grey25",
  plan_color = "#DAA520",
  fill_limits = c(0, 1),
  fill_low = "white",
  fill_high = "#1572da",
  na_fill = "grey90",
  edge_min = 1e-09,
  node_size = 0.32,
  label_pad = 0.12,
  label_size = 3.2,
  edge_label_size = 3,
  edge_linewidth = 0.8,
  arrow_inches = 0.11,
  edge_labels = FALSE,
  edge_label_fmt = function(x) formatC(x, digits = 2, format = "g"),
  title = NULL
)
```

## Arguments

- model:

  A \`lavaan\`-syntax model string with \`fZ_X.Y\` labels (and
  optionally fixed \`1 \* X\` terms), as consumed by \[evalF()\] /
  \[runF()\]. When a \`plan\` is layered onto a situation model, pass
  the full stitched string here.

- params:

  Named numeric vector of parameter values.

- s:

  Named numeric vector: the state at which to draw the field.

- layout:

  A data frame describing node placement, with columns \`name\`
  (matching the model's variables), \`x\`, \`y\`, and \`shape\` (one of
  \`"square"\`, \`"diamond"\`, \`"rtTri"\`, \`"lfTri"\`). An optional
  \`label\` column supplies display text (defaults to \`name\`).

- plan:

  Optional \`lavaan\`-syntax sub-model string. Edges whose source -\>
  target identity appears in \`plan\` are drawn in \`plan_color\`.
  Typically the action-plan half of a stitched model. Default \`NULL\`
  (no gold edges).

- edge_color, plan_color:

  Colours for situation edges and plan-contributed edges. Defaults
  \`"grey25"\` and \`"#DAA520"\` (gold).

- fill_limits:

  Numeric length-2 range mapped to the colour gradient. Default \`c(0,
  1)\`; values outside are squished to the ends.

- fill_low, fill_high:

  Gradient endpoint colours. Default white to \`"#1572da"\`.

- na_fill:

  Fill for layout nodes absent from \`s\`. Default \`"grey90"\`.

- edge_min:

  Minimum absolute resolved coefficient for an edge to be drawn. Default
  \`1e-9\` (draw any afforded force).

- node_size:

  Half-extent of each node shape in data coordinates. Default \`0.32\`.

- label_pad:

  Gap between a node's perimeter and its label. Default \`0.12\`.

- label_size, edge_label_size:

  Text sizes for node and edge labels. Defaults \`3.2\` and \`3\`.

- edge_linewidth:

  Edge linewidth. Default \`0.8\`.

- arrow_inches:

  Arrowhead length in inches. Default \`0.11\`.

- edge_labels:

  Logical: label each drawn edge with its resolved coefficient. Default
  \`FALSE\`.

- edge_label_fmt:

  Function formatting the resolved coefficient for the edge label.
  Default two significant digits.

- title:

  Optional plot title.

## Value

A \`ggplot\` object.

## Details

Each regression row of \`model\` (\`Y ~ label \* X\` or \`Y ~ label \*
X:Z\`) is read through its \`fZ_X.Y\` label (see \`vignette("notation",
package = "funfield")\`): the label names the source \`X\` and target
\`Y\`, and any remaining variable in the right-hand-side interaction is
the \*\*gate\*\* \`Z\`. The resolved field coefficient is
\`params\[label\] \* prod(s\[gate\])\`, so an afforded force (no
baseline term) contributes nothing until its gate flips on. The edge \`X
-\> Y\` is drawn whenever the magnitude of that resolved coefficient
exceeds \`edge_min\`; the source node \`X\` itself is coloured by
\`s\[X\]\`, so an afforded-but-unfired force shows as an edge leaving a
still-white node. Terms with a fixed coefficient (e.g. \`1 \* X\`, as in
a conditional action plan) are valued from that fixed coefficient rather
than \`params\`.

When a \`plan\` sub-model is supplied, the edges it contributes are
drawn in \`plan_color\` (gold by convention) – a visual marker for the
forces a conditional action plan adds on top of the situation's own
affordance structure.

## See also

\[runF()\] for the trajectory, \[plotsAsWidget()\] to stitch frames,
\[plotPathSchema()\] for the fixed X-M-Y schematic.

## Examples

``` r
if (FALSE) { # \dontrun{
model <- "s2 ~ fS1_make.s2 * make:s1
          L  ~ f1_s2.L      * s2"
params <- c(fS1_make.s2 = 1, f1_s2.L = 1)
layout <- data.frame(
  name  = c("s1", "make", "s2", "L"),
  x     = c(0, 1, 2, 3),
  y     = c(0, 0, 1, 1),
  shape = c("diamond", "rtTri", "diamond", "lfTri")
)
plotField(model, params, c(s1 = 1, make = 1, s2 = 1, L = 0), layout)
} # }
```
