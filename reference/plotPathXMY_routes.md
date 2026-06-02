# Two-panel expectation vs valuation route diagrams

Returns a patchwork composition of two
[`plotPathXMY`](https://dustin-wood.github.io/funfield/reference/plotPathXMY.md)
panels, one per moderation route. The **expectation** panel shows
`FZ[X,M]` on the X-to-M arm (limegreen) and `F1[M,Y]` on the M-to-Y arm
(black); the **valuation** panel shows the converse pairing.

Reading rule: a mediator whose two arms BOTH carry visible weight in the
same panel identifies a *reason* that the moderator shifts the X-to-Y
relationship. A green-black chain in the expectation panel says the
moderator shifts what the actor expects from the action AND those
expectations matter for action. A black-green chain in the valuation
panel says the moderator shifts how those expectations are weighted
while leaving the expectations themselves unchanged. The
indirect-product columns of
[`pathXMY_pairtable`](https://dustin-wood.github.io/funfield/reference/pathXMY_pairtable.md)
are the formal counterpart.

## Usage

``` r
plotPathXMY_routes(x, mediator = NULL, Z_label = "Z", panel_titles = NULL, ...)
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
  See there for details.

- panel_titles:

  Optional length-2 character vector of panel titles. If `NULL`, panel
  titles are auto-generated from `Z_label` in the F-schema display form.

## Value

A `patchwork` object combining the two `ggplot` panels. Auto-prints in
interactive sessions and knitr chunks.

## See also

[`plotPathXMY`](https://dustin-wood.github.io/funfield/reference/plotPathXMY.md),
[`plotPathXMY_ZLH`](https://dustin-wood.github.io/funfield/reference/plotPathXMY_ZLH.md),
[`plotPathXMY_widget_routes`](https://dustin-wood.github.io/funfield/reference/plotPathXMY_widget_routes.md)
