# Interactive Step-by-Step Field Sequence Widget

Renders each time step of a functional field simulation as a
high-resolution PNG image and wraps them in a self-contained HTML widget
with **Back** and **Forward** navigation buttons. The widget is suitable
for use in R Markdown / knitr HTML documents (including package
vignettes), RStudio's Viewer pane, and Shiny apps. No server is
required: all images are embedded as base-64 data URIs so the HTML file
is fully self-contained.

Images are rendered at twice the CSS display width (controlled by `res`)
so they appear sharp on high-DPI / retina displays.

## Usage

``` r
fieldSequenceWidget(
  sitlog,
  fprops,
  steps = NULL,
  titles = NULL,
  node_labels = NULL,
  label.cex = 0.65,
  width = 9,
  height = 5.5,
  res = 192,
  ...
)
```

## Arguments

- sitlog:

  Output of
  [`expOutcomesFF`](https://dustin-wood.github.io/funfield/reference/expOutcomesFF.md):
  a list with elements `sit` (situation matrix), `dit` (disturbance
  matrix), and `fit` (list of field states, one per time step).

- fprops:

  Output of
  [`prepField`](https://dustin-wood.github.io/funfield/reference/prepField.md):
  a list containing `matLayout`, `vnames`, `vshapes`, `vsizes`, and
  `vtype`.

- steps:

  Integer vector of time-step indices to include. Defaults to all valid
  steps (those with a non-`NULL` `$ft` element in `sitlog$fit`).

- titles:

  Character vector of step titles displayed *above* each image, one per
  element of `steps`. Defaults to `"Step 1"`, `"Step 2"`, etc.

- node_labels:

  Optional named character vector mapping internal node names to display
  labels (e.g.
  `c("s1" = "Ready to make coffee", "make(s2).p" = "Set up coffeemaker")`).
  When supplied, labels are drawn *outside* each node (above action /
  appraisal nodes, below object / state nodes) rather than inside. Names
  not found in `fprops$vnames` are silently ignored; nodes without an
  entry fall back to their internal name.

- label.cex:

  Character expansion for external node labels. Default `0.65`. Only
  used when `node_labels` is supplied.

- width:

  Plot width in inches. Default `9`.

- height:

  Plot height in inches. Default `5.5`.

- res:

  PNG resolution in pixels per inch. Default `192`, which renders the
  image at 2× the CSS display size for retina sharpness.

- ...:

  Additional arguments passed through to
  [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

## Value

An
[`htmltools::tagList`](https://rstudio.github.io/htmltools/reference/tagList.html)
object. In a knitr / R Markdown chunk this renders automatically as an
interactive HTML widget. Call
`htmltools::browsable(fieldSequenceWidget(...))` to preview it in the
RStudio Viewer.

## See also

[`expOutcomesFF`](https://dustin-wood.github.io/funfield/reference/expOutcomesFF.md),
[`prepField`](https://dustin-wood.github.io/funfield/reference/prepField.md),
[`plotFieldSequence`](https://dustin-wood.github.io/funfield/reference/plotFieldSequence.md),
[`pnLevels`](https://dustin-wood.github.io/funfield/reference/pnLevels.md),
[`fieldPolygons`](https://dustin-wood.github.io/funfield/reference/fieldPolygons.md)
