# Two-panel low/high-Z field diagrams for a moderated X-M-Y model

Returns a patchwork composition of two
[`plotPathXMY`](https://dustin-wood.github.io/funfield/reference/plotPathXMY.md)
panels, one at a low value of `Z` and one at a high value. Each panel
renders the field with coefficients *collapsed* at the chosen `Z` value
(`b1 + bZ * Z_value`), so the moderation becomes visually concrete:
edges thicken/thin, nodes shift color, and the implied total effect on Y
shifts between panels. The two panels share a single color legend at the
bottom.

## Usage

``` r
plotPathXMY_ZLH(
  x,
  mediator = NULL,
  Z_levels = c(-1, 1),
  Z_label = "Z",
  panel_titles = NULL,
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

  Numeric length-2 vector of low and high Z values (default `c(-1, 1)`,
  i.e. one SD below and above the mean for a z-standardized
  between-person moderator).

- panel_titles:

  Optional length-2 character vector of panel titles. If `NULL`, panel
  titles are auto-generated using `Z_label` and the `Z_levels` values.

## Value

A `patchwork` object combining two `ggplot` panels with a shared bottom
legend. Auto-prints in interactive sessions and knitr chunks.

## See also

[`plotPathXMY`](https://dustin-wood.github.io/funfield/reference/plotPathXMY.md)
