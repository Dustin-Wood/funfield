# Back/Forward widget stepping through the views of a moderated pathF field

Renders a moderated
[`pathF`](https://dustin-wood.github.io/funfield/reference/pathF.md)
model as a navigable HTML widget whose frames are the
[`plotPathF`](https://dustin-wood.github.io/funfield/reference/plotPathF.md)
view modes at pixel-identical layout: by default the **decomposed**
field (f1 labels with their gold fZ companions), the **normative** field
(the same f1 paths with the moderation hidden), and the **moderation**
field (every edge swapped for its fZ counterpart, all gold). Clicking
**Forward** therefore peels the one diagram apart into its black and
gold layers in place.

To guarantee the frames overlay perfectly — same node and arc positions
to the pixel — the widget strips the per-view subtitles and the score
colorbar from every frame (their presence varies by view and would
otherwise shift the panel). Frame identity is carried by the title
suffix and the widget's caption strip.

This is a thin convenience over
[`plotsAsWidget`](https://dustin-wood.github.io/funfield/reference/plotsAsWidget.md);
for custom frame sequences (e.g. mixing `Z_value` collapses with view
modes), build the frames with
[`plotPathF()`](https://dustin-wood.github.io/funfield/reference/plotPathF.md)
directly and stitch them yourself.

## Usage

``` r
plotPathF_widget(
  x,
  views = c("decomposed", "normative", "moderation"),
  title = NULL,
  panel_titles = NULL,
  scale_max = 0.8,
  scale_max_moderation = NULL,
  width = 9,
  height = 4.5,
  res = 192,
  format = c("png", "svg"),
  ...
)
```

## Arguments

- x:

  A [`pathF`](https://dustin-wood.github.io/funfield/reference/pathF.md)
  or
  [`pathF_decompose`](https://dustin-wood.github.io/funfield/reference/pathF_decompose.md)
  return; the fit must include a moderator Z.

- views:

  Character vector of
  [`plotPathF()`](https://dustin-wood.github.io/funfield/reference/plotPathF.md)
  view modes, one per frame, in display order. Default
  `c("decomposed", "normative", "moderation")`.

- title:

  Optional base title. Frames after the first are suffixed
  `"[normative effects]"` / `"[moderation effects]"` per their view; the
  decomposed frame carries the bare title.

- panel_titles:

  Optional character vector overriding the auto-built frame captions,
  parallel to `views`.

- scale_max:

  Path-magnitude cap for the f1-scaled frames (decomposed, normative);
  default `0.8`.

- scale_max_moderation:

  Cap for the moderation frame, where fZ magnitudes run smaller. Default
  `NULL` uses `scale_max` (edge thicknesses then compare honestly across
  frames); pass a tighter value such as `0.3` to make the gold field
  legible on its own terms.

- width, height:

  Frame size in inches (default `9` x `4.5`).

- res:

  PNG resolution in pixels per inch (ignored for SVG).

- format:

  One of `"png"` (default) or `"svg"`.

- ...:

  Additional arguments passed to every
  [`plotPathF`](https://dustin-wood.github.io/funfield/reference/plotPathF.md)
  call (e.g. `Z_label`, `labels`, `show_pvalues`).

## Value

An
[`htmltools::tagList`](https://rstudio.github.io/htmltools/reference/tagList.html)
that auto-renders as an interactive widget in knitr / R Markdown HTML
output; fully self-contained (images embedded as data URIs).

## See also

[`plotPathF`](https://dustin-wood.github.io/funfield/reference/plotPathF.md),
[`plotsAsWidget`](https://dustin-wood.github.io/funfield/reference/plotsAsWidget.md),
[`plotPathXMY_widget`](https://dustin-wood.github.io/funfield/reference/plotPathXMY_widget.md)
