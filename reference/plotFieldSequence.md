# Plot a Sequence of Functional Field Time Steps

Visualize the time-step evolution of a functional field simulation by
plotting the field at each specified step. This function replaces the
manual pattern of incrementing `i` by hand and re-running a `qgraph`
call after
[`expOutcomesFF`](https://dustin-wood.github.io/funfield/reference/expOutcomesFF.md).

## Usage

``` r
plotFieldSequence(
  sitlog,
  fprops,
  steps = NULL,
  titles = NULL,
  node_labels = NULL,
  label.cex = 0.65,
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
  a list containing `matLayout`, `vnames`, `vshapes`, and `vsizes`.

- steps:

  Integer vector of time-step indices to plot. Defaults to all valid
  steps (those with a non-`NULL` `$ft` element in `sitlog$fit`).

- titles:

  Character vector of plot titles, one per element of `steps`. Defaults
  to `"Step 1"`, `"Step 2"`, etc.

- node_labels:

  Optional named character vector mapping internal node names to display
  labels (e.g.
  `c("s1" = "Ready to make coffee", "make(s2).p" = "Set up coffeemaker")`).
  When supplied, labels are drawn *outside* each node (above action
  nodes, below state/object nodes) rather than inside. Names not found
  in `fprops$vnames` are silently ignored; nodes without an entry fall
  back to their internal name.

- label.cex:

  Character expansion for external node labels. Default `0.65`. Only
  used when `node_labels` is supplied.

- ...:

  Additional arguments passed through to
  [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

## Value

The `sitlog` list, invisibly, so the call can be used in a pipe.

## See also

[`expOutcomesFF`](https://dustin-wood.github.io/funfield/reference/expOutcomesFF.md),
[`prepField`](https://dustin-wood.github.io/funfield/reference/prepField.md),
[`pnLevels`](https://dustin-wood.github.io/funfield/reference/pnLevels.md),
[`fieldPolygons`](https://dustin-wood.github.io/funfield/reference/fieldPolygons.md)
