# Draw one frame of a functional field at a given state

Renders a functional field \`F\` evaluated at a single state \`s\` as a
ggplot2 node-link diagram. Nodes are placed at caller-supplied
coordinates and drawn as shapes coloured by their current state value
(white at the low limit, blue at the high limit). An edge is drawn for
\*\*every structural force\*\* in the model (any non-zero coefficient),
and coloured by whether its condition is currently met: a
\*\*potential\*\* path, whose gate is not yet on, is light grey; it
darkens to its full colour as the gate goes on. Stepping \`plotField()\`
across the rows of a \[simulateF()\] trajectory and stitching the frames
with \[plotsAsWidget()\] produces a back/forward animation in which the
whole field is visible throughout and the active sub-path lights up as
conditions are met.

## Usage

``` r
plotField(
  model,
  params,
  s,
  layout,
  plan = NULL,
  plan_label = NULL,
  plan_ramp = FALSE,
  plan_colors = c("#00FF00", "#32CD32", "#41A317"),
  conjunctive = NULL,
  condition_labels = TRUE,
  condition_ramp = FALSE,
  condition_colors = c("#F6BE00", "#9A6000"),
  edge_color = "black",
  plan_color = "#DAA520",
  potential_color = "grey80",
  fill_limits = c(-1, 1),
  fill_low = "#b73712",
  fill_mid = "white",
  fill_high = "#1572da",
  na_fill = "grey90",
  edge_min = 1e-09,
  edge_scale_max = 1,
  curvature = -1,
  node_size = 0.32,
  label_pad = 0.12,
  label_size = 3.2,
  glyph_size = 5,
  glyph_color = "#1572da",
  edge_label_size = 3,
  edge_linewidth = 2.1,
  arrow_inches = 0.15,
  arrow_size = 0.26,
  arrow_stroke = 0.8,
  arrow_gap = 0.06,
  edge_labels = TRUE,
  edge_label_fmt = function(x) {
s <- f0(x, digits = 2)
s <- sub("0+$", "", s)

        sub("\\.$", "", s)
 },
  title = NULL
)
```

## Arguments

- model:

  A \`lavaan\`-syntax model string with \`fZ_X.Y\` labels (and
  optionally fixed \`1 \* X\` terms), as consumed by \[evalF()\] /
  \[simulateF()\]. When a \`plan\` is layered onto a situation model,
  pass the full stitched string here.

- params:

  Named numeric vector of parameter values.

- s:

  Named numeric vector: the state at which to draw the field.

- layout:

  A data frame describing node placement, with columns \`name\`
  (matching the model's variables), \`x\`, \`y\`, and \`shape\` (one of
  \`"square"\`, \`"diamond"\`, \`"rtTri"\`, \`"lfTri"\`, \`"circle"\`).
  By convention objects are diamonds, actions right-facing triangles,
  the likelihood readout a left-facing triangle, and a choice / chance
  node a circle. An optional \`label\` column supplies display text
  (defaults to \`name\`). An optional \`glyph\` column marks a node as a
  \*\*glyph node\*\*: where it holds a non-empty string, that string is
  drawn centred inside the shape and no caption is placed beneath it.
  The body is still value-shaded like any other node — e.g. an
  \`Energy\` node drawn as a square holding a lightning bolt that
  reddens as energy is spent.

- plan:

  Optional \`lavaan\`-syntax sub-model string. Edges whose source -\>
  target identity appears in \`plan\` take \`plan_color\` as their base
  colour. Typically the action-plan half of a stitched model. Default
  \`NULL\` (no gold edges).

- plan_label:

  Optional single-character prefix naming the plan (e.g. \`"a"\` for
  "Plan A"). When supplied, each plan edge is labelled by its
  \*\*step\*\* in the plan – the order its \`action ~ condition\` rule
  appears in \`plan\` – as \`\<plan_label\>\<step\>\` (so Plan A's four
  rules read \`a1\`, \`a2\`, \`a3\`, \`a4\`). Default \`NULL\` (plan
  edges keep their coefficient / condition label).

- plan_ramp:

  Logical; when \`TRUE\`, the \*\*action plan's forces\*\* are coloured
  on a green ramp (the "green means go" cue): each plan edge and its
  \`\<plan_label\>\<step\>\` label take a colour stepped along
  \`plan_colors\` by plan order (first step = the ramp's first colour),
  shown when the edge's condition is met and \`potential_color\` (grey)
  when merely potential. Only the \*\*forces\*\* are recoloured — action
  \*nodes\* keep the value scale (white when idle, blue on the turn they
  fire), so the plan adds no new node colour. When \`FALSE\`, plan edges
  take flat \`plan_color\` (gold). Default \`FALSE\`.

- plan_colors:

  Anchor colours for the \`plan_ramp\`, interpolated to one colour per
  plan step. Default \`c("#00FF00", "#32CD32", "#41A317")\` (lime -\>
  limegreen -\> dark lime green).

- conjunctive:

  Character vector of target node names whose incoming edges are
  \*\*conjunctive\*\* — the node fires only when \*all\* of them are on
  (a product, not a sum). Those edges are drawn \*\*dotted\*\* and carry
  no numeric label, so they are not mistaken for additive regression
  weights. Default \`NULL\`.

- condition_labels:

  Logical; when \`TRUE\` (default), a \*\*gated\*\* edge (from an
  interaction \`Y ~ Z:X\`, where \`Z\` is the condition that opens the
  \`X -\> Y\` force) is labelled by its condition's displayed name —
  e.g. \`TurnOn -\> HCoPot\`, gated on \`s2\`, reads "rig set" — rather
  than by the coefficient. Ungated paths keep their numeric weight. Set
  \`FALSE\` to label every edge by its coefficient.

- condition_ramp:

  Logical; when \`TRUE\`, the \*\*condition states\*\* — the moderators
  \`Z\` that gate the afforded forces (\`Y ~ Z:X\`) — are coloured on a
  gold -\> orange ramp instead of the red-white-blue value scale. Each
  condition node, the gated force it opens, and that force's label take
  a colour stepped along \`condition_colors\` by the condition's order
  of appearance (earliest = the ramp's first colour). The node fills
  with its colour when present and is white when absent; the gated force
  and its label show the colour when the condition is met and
  \`potential_color\` (grey) when not. Actions and non-condition states
  keep the diverging value scale. Default \`FALSE\`. (With \`plan_ramp\`
  this gives two parallel staircases: a green action ramp and a gold
  condition ramp, kept clear of each other and of the red-white-blue
  value scale.)

- condition_colors:

  Anchor colours for the \`condition_ramp\`, interpolated to one colour
  per condition. Default \`c("#F6BE00", "#9A6000")\` (gold -\> deep
  amber), kept clear of the value-red and the green action ramp.

- edge_color, plan_color:

  Base (fully active) colours for situation edges and plan-contributed
  edges — the colour a path takes when its condition is fully met.
  Defaults \`"black"\` and \`"#DAA520"\` (gold).

- potential_color:

  Colour of a \*\*potential\*\* path — one drawn from the model
  structure but whose condition is not yet met. An edge interpolates
  from this toward its base colour as its condition's activation goes 0
  -\> 1. Default \`"grey80"\` (light but legible).

- fill_limits:

  Numeric length-2 range mapped to the diverging fill scale. Default
  \`c(-1, 1)\`; values outside are squished to the ends.

- fill_low, fill_mid, fill_high:

  Diverging fill colours at the low, zero, and high ends. Default red
  \`"#b73712"\` / white / blue \`"#1572da"\`, midpoint \`0\`.

- na_fill:

  Fill for layout nodes absent from \`s\`. Default \`"grey90"\`.

- edge_min:

  Minimum absolute coefficient for a path to be drawn at all. Default
  \`1e-9\` (draw any structural force).

- edge_scale_max:

  Absolute resolved coefficient mapped to a full-strength edge
  (linewidth and colour saturate here). Default \`1\`.

- curvature:

  Arc curvature for backward consumption edges (the negative member of
  an anti-parallel pair), drawn as a quadratic Bezier so they bow clear
  of the straight forward edge they would otherwise overlay. Negative
  bows the arc below the forward edge (a leftward "mirrored smile"); the
  dip is deep enough to keep the two edges' mid-shaft labels from
  colliding. Default \`-1\`.

- node_size:

  Half-extent of each node shape in data coordinates. Default \`0.32\`.

- label_pad:

  Gap between a node's perimeter and its label. Default \`0.12\`.

- label_size, edge_label_size:

  Text sizes for node and edge labels. Defaults \`3.2\` and \`3\`.

- glyph_size:

  Text size for centred glyph-node symbols. Default \`5\`.

- glyph_color:

  Fill colour for the lightning-bolt glyph (\`glyph\` of \`"bolt"\` or
  \`"⚡"\`). Default blue \`"#1572da"\` (the positive-activation
  colour).

- edge_linewidth:

  Linewidth of a full-strength edge (\`\|f\| \>= edge_scale_max\`);
  fainter forces scale below it. Default \`2.1\`.

- arrow_inches:

  Arrowhead length in inches. Default \`0.15\`. The head is a sharp,
  solid (mitred) triangle, the same size for every edge and for positive
  and negative forces alike.

- arrow_size:

  Length (data units) of the solid tip segment that carries each
  arrowhead, kept solid so the head reads cleanly even on a dashed
  shaft. Default \`0.26\`.

- arrow_stroke:

  Linewidth of the arrowhead's own stroke (constant, not the shaft's
  \`lw\`). Kept thin so the sharp mitred tip lands exactly on the node
  outline instead of overshooting into it. Default \`0.8\`.

- arrow_gap:

  Distance (data units) the head end is pulled back from the node
  perimeter, so the arrowhead tip rests on the outline rather than
  poking inside. Default \`0.06\`.

- edge_labels:

  Logical: label each drawn edge with its resolved force level. Default
  \`TRUE\`.

- edge_label_fmt:

  Function formatting the resolved coefficient for the edge label.
  Default the no-leading-zero house style with trailing zeros trimmed
  (\`1\`, \`.9\`, \`-.1\`).

- title:

  Optional plot title.

## Value

A \`ggplot\` object.

## Details

Each regression row of \`model\` (\`Y ~ label \* X\` or \`Y ~ label \*
X:Z\`) is read through its \`fZ_X.Y\` label (see \`vignette("notation",
package = "funfield")\`): the label names the source \`X\` and target
\`Y\`, and any remaining variable in the right-hand-side interaction is
the \*\*gate\*\* \`Z\`. The edge \`X -\> Y\` is drawn whenever the
coefficient is non-zero, and its \*\*activation\*\* —
\`prod(s\[gate\])\`, or \`1\` for an ungated standing force — sets its
colour: light grey (\`potential_color\`) when 0, the base colour when 1.
The source node \`X\` is coloured by \`s\[X\]\`, so an open-but-unfired
path shows as a black edge leaving a still-white node. Terms with a
fixed coefficient (e.g. \`1 \* X\`) are valued from that coefficient
rather than \`params\`.

\*\*Encoding the force.\*\* Each edge's coefficient is read off its
appearance: its \*\*magnitude\*\* sets the linewidth (and how far an
\*active\* edge fades toward white as \`\|f\| -\> 0\`, capped at
\`edge_scale_max\`), and its \*\*sign\*\* sets the linetype (solid for a
positive force, dashed for a negative one). So a force of \`1\` is a
strong solid line and a force of \`-0.1\` a faint dashed one, and (with
\`edge_labels\`) the level — or, for a gated path, its condition's name
— is printed on the shaft. The shaft is drawn with butt-ended dashes so
they stay legible on a thick line, and the \*\*arrowhead\*\* is a sharp,
solid mitred triangle — identical for positive and negative forces —
carried on a short solid tip segment so a dashed shaft never breaks up
the head. \*\*Node bodies\*\* are shaded on a diverging red-white-blue
scale (negative red, zero white, positive blue), so a depleting
\`Energy\` stock reddens as it is spent.

When a \`plan\` sub-model is supplied, the edges it contributes take
\`plan_color\` (gold by convention) as their base colour – a visual
marker for the forces a conditional action plan adds on top of the
situation's own affordance structure.

## See also

\[simulateF()\] for the trajectory, \[plotsAsWidget()\] to stitch
frames, \[plotPathSchema()\] for the fixed X-M-Y schematic.

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
