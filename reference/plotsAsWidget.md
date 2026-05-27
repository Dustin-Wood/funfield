# Wrap a list of ggplots in a navigable Back / Forward HTML widget

Pixel-identical-layout image carousel: each ggplot in `plots` becomes
one frame, and the widget swaps frames in place via **Back** /
**Forward** buttons. This is the same machinery that powers
[`plotPathXMY_widget`](https://dustin-wood.github.io/funfield/reference/plotPathXMY_widget.md)
and
[`plotPathXMY_widget_routes`](https://dustin-wood.github.io/funfield/reference/plotPathXMY_widget_routes.md),
exposed so callers can stitch custom panel sequences from any plot
family – e.g. the three term-decomposition panels from
[`plotPathSchema`](https://dustin-wood.github.io/funfield/reference/plotPathSchema.md)
overlaid in place rather than shown side by side.

All frames are rendered at the same width / height so they swap in place
without reflowing the page; build each constituent plot with the same
canvas size.

## Usage

``` r
plotsAsWidget(
  plots,
  panel_titles = NULL,
  width = 9,
  height = 3.5,
  res = 192,
  format = c("png", "svg")
)
```

## Arguments

- plots:

  A list of `ggplot` objects, one per frame.

- panel_titles:

  Character vector of captions, one per frame, shown in the widget's
  counter strip and used as image `alt` text. If `NULL` (default),
  frames are auto-labeled “Frame k of n”.

- width, height:

  Frame width and height in inches. Default `9` x `3.5`.

- res:

  PNG resolution in pixels per inch (ignored for SVG). Default `192` (2x
  retina for sharpness on high-DPI displays).

- format:

  One of `"png"` (default) or `"svg"`. SVG scales crisply at any zoom;
  PNG renders faster and matches the default for the other widget
  functions in the package.

## Value

An
[`htmltools::tagList`](https://rstudio.github.io/htmltools/reference/tagList.html)
that auto-renders as an interactive widget in knitr / R Markdown HTML
output. The HTML is fully self-contained – all images are embedded as
base64 data URIs, no server required.

## See also

[`plotPathXMY_widget`](https://dustin-wood.github.io/funfield/reference/plotPathXMY_widget.md),
[`plotPathXMY_widget_routes`](https://dustin-wood.github.io/funfield/reference/plotPathXMY_widget_routes.md)
