# Toggleable widget for expectation vs valuation route diagrams

Renders the two
[`plotPathXMY_routes`](https://dustin-wood.github.io/funfield/reference/plotPathXMY_routes.md)
panels as a self-contained HTML widget with **Back** and **Forward**
buttons that swap them in place at pixel-identical layout, so the green
arm slides from left (expectation) to right (valuation) and the eye
picks up which mediator carries which route. Sibling of
[`plotPathXMY_widget`](https://dustin-wood.github.io/funfield/reference/plotPathXMY_widget.md);
both share the same toggle plumbing.

## Usage

``` r
plotPathXMY_widget_routes(
  x,
  mediator = NULL,
  Z_label = "Z",
  panel_titles = NULL,
  width = 9,
  height = NULL,
  res = 192,
  format = c("png", "svg"),
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

- mediator, X_label, Y_label, Z_label, M_labels, X_shape, Y_shape,
  digits, scale_max, score_intensity_max, ...:

  Passed through to
  [`plotPathXMY`](https://dustin-wood.github.io/funfield/reference/plotPathXMY.md).

- panel_titles:

  Optional length-2 character vector. If `NULL`, titles are
  auto-generated from `Z_label` in the F-schema display form.

- width, height:

  Frame width and height in inches. `height` defaults to `3.5` for the
  single-mediator triangle view and to `3 + 0.5 * n_mediators` for the
  fan view.

- res:

  PNG resolution in pixels per inch. Default `192`. Ignored when
  `format = "svg"`.

- format:

  One of `"png"` (default) or `"svg"`. SVG is recommended for HTML
  vignette output.

## Value

An
[`htmltools::tagList`](https://rstudio.github.io/htmltools/reference/tagList.html)
object that auto-renders as an interactive HTML widget in knitr / R
Markdown.

## See also

[`plotPathXMY_routes`](https://dustin-wood.github.io/funfield/reference/plotPathXMY_routes.md),
[`plotPathXMY_widget`](https://dustin-wood.github.io/funfield/reference/plotPathXMY_widget.md)
