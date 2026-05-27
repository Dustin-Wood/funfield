# Decompose a Z-moderated X-Y effect into expectation-, valuation-, and direct-moderation components

For a moderator `Z` acting on the link from `X` to `Y`, two models are
commonly of interest:

- Model \[1\]:

  The direct moderation \\Y = b^1\_{YX} X + b^Z\_{YX} X \cdot Z +
  \dots\\. `BZ_YX[1]` indexes the *total* moderation effect — how much
  \\Z\\ shifts the action-outcome link, without reference to any
  mediator.

- Model \[2\]:

  The full mediated model \\X \to M \to Y\\ with \\Z\\ moderating all
  three paths.

These are related by the algebraic identity (applied to one mediator at
a time, in linear regression with cluster-robust SEs):

\$\$b^Z\_{YX}\[1\] = b^Z\_{MX} \cdot b^1\_{YM} + b^1\_{MX} \cdot
b^Z\_{YM} + b^Z\_{YX}\[2\]\$\$

- Term 1, `BZ_MX * B1_YM`: moderation that flows through \\Z\\ changing
  the *expectation* of \\M\\ given \\X\\. This is what
  [`pathXMY()`](https://dustin-wood.github.io/funfield/reference/pathXMY.md)
  highlights by default.

- Term 2, `B1_MX * BZ_YM`: moderation that flows through \\Z\\ changing
  the *valuation* of \\M\\ as a driver of \\Y\\, while leaving
  expectations untouched.

- Term 3, `BZ_YX[2]`: direct \\Z\\-moderation of the \\X \to Y\\ link
  that does *not* route through this mediator.

Identifying which term carries most of `BZ_YX[1]` for a given mediator
answers a different psychological question than
[`pathXMY()`](https://dustin-wood.github.io/funfield/reference/pathXMY.md)
alone. A person-trait moderator may leave expectations untouched
(`BZ_MX` near zero) yet substantially shift valuation (`BZ_YM` large) —
invisible in the `BZ_MX` table but central to understanding why the
trait moderates action.

## Usage

``` r
pathXMY_decompose(data, X, Y, M, Z, Z.within = FALSE, cluster = "p", ...)
```

## Arguments

- data:

  A data frame in PSI long format. Level-1 columns are within-(person,
  situation) deviated automatically by
  [`pathXMY`](https://dustin-wood.github.io/funfield/reference/pathXMY.md)
  (see its `deviate` argument).

- X, Y, M, Z, Z.within, cluster, ...:

  Passed to
  [`pathXMY`](https://dustin-wood.github.io/funfield/reference/pathXMY.md).
  `M` must be supplied (one or more mediator names); `Z` is required.

## Value

A list with three elements:

- total:

  One-row data frame with the no-mediator (Model \[1\]) estimates:
  `est`, `se`, `z`, `pvalue` for `BZ_YX[1]`.

- components:

  Long tidy data frame, one row per (mediator, term). Terms:
  `"BZ_MX * B1_YM"` (expectation moderation), `"B1_MX * BZ_YM"`
  (valuation moderation), `"BZ_YX (direct)"` (residual direct
  moderation), and `"sum (1+2+3)"` (their algebraic sum; should
  approximate `BZ_YX[1]` for each single-mediator fit). The first three
  rows carry full SE/z/p columns; the sum row has `est` only.

- fits:

  A list with `$direct` (Model \[1\]) and `$full` (the multi-mediator
  [`pathXMY()`](https://dustin-wood.github.io/funfield/reference/pathXMY.md)
  object).

## See also

[`pathXMY`](https://dustin-wood.github.io/funfield/reference/pathXMY.md),
[`plotPathXMY`](https://dustin-wood.github.io/funfield/reference/plotPathXMY.md)
