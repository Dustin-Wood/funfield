# Interactive field diagram editor

Launches a local Shiny app for interactively repositioning nodes in a
functional field diagram. Drag nodes to desired positions, then click
"Render qgraph" to produce a high-quality static plot with those
coordinates.

## Usage

``` r
fieldEditor(fprops, fieldMatrix = NULL)
```

## Arguments

- fprops:

  A field properties list returned by
  [`prepField`](https://dustin-wood.github.io/funfield/reference/prepField.md).

- fieldMatrix:

  Optional path-coefficient matrix (from
  [`fieldResults`](https://dustin-wood.github.io/funfield/reference/fieldResults.md))
  used to draw weighted, signed edges. If NULL, edges are drawn
  unweighted.

## Value

Launches a Shiny app; does not return a value.
