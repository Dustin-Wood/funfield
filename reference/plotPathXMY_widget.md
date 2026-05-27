# Interactive toggleable widget for an X-M-Y field across Z levels

Renders
[`plotPathXMY`](https://dustin-wood.github.io/funfield/reference/plotPathXMY.md)
once per requested `Z_value`, embeds the resulting PNGs as base-64 data
URIs, and wraps them in a self-contained HTML widget with **Back** and
**Forward** navigation buttons. Each click swaps the field at
pixel-identical layout, so edges thicken / thin and node colors
transition between Z levels in place — making the moderation visually
striking compared to a side-by-side static view.

Suitable for knitr/R-Markdown HTML output, RStudio's Viewer pane, and
Shiny apps. No server is required: all images are embedded as data URIs,
so the HTML file is fully self-contained.

## Usage

``` r
plotPathXMY_widget(
  x,
  mediator = NULL,
  Z_levels = c(-1, 0, 1),
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
  See there for details. In particular, `X_shape = "rtTri"` marks X as
  itself an imagined action (speeding / overtime EXSJTs) and
  `Y_shape = "square"` marks Y as a regular continuous outcome rather
  than an action likelihood.

- Z_levels:

  Numeric vector of Z values to render, one frame each. Default
  `c(-1, 0, 1)` — one SD below the mean, the mean, and one SD above, for
  a z-standardized between-person moderator. Linear models make path
  weights between these endpoints trivially interpolatable; values at
  \\\|Z\| \> 1\\ are often out of the well-supported range of the data
  so are not in the default.

- panel_titles:

  Optional character vector of titles, one per frame. If `NULL`, titles
  are auto-generated using `Z_label` and the `Z_levels` values.

- width, height:

  Frame width and height in inches. `height` defaults to `3.5` for the
  single-mediator triangle view and to `3 + 0.5 * n_mediators` for the
  fan view.

- res:

  PNG resolution in pixels per inch. Default `192` (2x retina factor for
  sharpness on high-DPI displays). Ignored when `format = "svg"`.

- format:

  One of `"png"` (default) or `"svg"`. SVG frames are vector and scale
  crisply at any zoom — recommended for HTML vignette output. PNG frames
  are embedded as `image/png;base64` data URIs at the requested `res`;
  SVG frames are embedded as `image/svg+xml;base64` data URIs.

## Value

An
[`htmltools::tagList`](https://rstudio.github.io/htmltools/reference/tagList.html)
object that auto-renders as an interactive HTML widget in knitr / R
Markdown. Call `htmltools::browsable(plotPathXMY_widget(...))` to
preview it in the RStudio Viewer.

## See also

[`plotPathXMY`](https://dustin-wood.github.io/funfield/reference/plotPathXMY.md),
[`plotPathXMY_ZLH`](https://dustin-wood.github.io/funfield/reference/plotPathXMY_ZLH.md),
[`fieldSequenceWidget`](https://dustin-wood.github.io/funfield/reference/fieldSequenceWidget.md)
