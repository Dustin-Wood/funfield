# Decompose a Z-moderated X-Y effect into expectation-, valuation-, and direct-moderation components

For a moderator `Z` acting on the link from `X` to `Y`, two models are
commonly of interest:

- Total model:

  The direct moderation \\Y = F_1\[X,Y\] X + F_Z\[X,Y\] X \cdot Z +
  \dots\\. The total `fZ_XY` (\\F_Z^{\*}\[X,Y\]\\) indexes the *total*
  moderation effect — how much \\Z\\ shifts the action-outcome link,
  without reference to any mediator.

- Mediated model:

  The full mediated model \\X \to M \to Y\\ with \\Z\\ moderating all
  three paths.

These are related by the algebraic identity (applied to one mediator at
a time, in linear regression with cluster-robust SEs):

\$\$F_Z^{\*}\[X,Y\] = F_Z\[X,M\] \cdot F_1\[M,Y\] + F_1\[X,M\] \cdot
F_Z\[M,Y\] + F_Z\[X,Y\]\$\$

- Term 1, `fZ_XM * f1_MY`: moderation that flows through \\Z\\ changing
  the *expectation* of \\M\\ given \\X\\. This is what
  [`pathXMY()`](https://dustin-wood.github.io/funfield/reference/pathXMY.md)
  highlights by default.

- Term 2, `f1_XM * fZ_MY`: moderation that flows through \\Z\\ changing
  the *valuation* of \\M\\ as a driver of \\Y\\, while leaving
  expectations untouched.

- Term 3, residual `fZ_XY`: direct \\Z\\-moderation of the \\X \to Y\\
  link that does *not* route through this mediator.

Identifying which term carries most of the total `fZ_XY` for a given
mediator answers a different psychological question than
[`pathXMY()`](https://dustin-wood.github.io/funfield/reference/pathXMY.md)
alone. A person-trait moderator may leave expectations untouched
(`fZ_XM` near zero) yet substantially shift valuation (`fZ_MY` large) —
invisible in the `fZ_XM` table but central to understanding why the
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

  One-row data frame with the no-mediator (total-model) estimates:
  `est`, `se`, `z`, `pvalue` for the total `fZ_XY`
  (\\F_Z^{\*}\[X,Y\]\\).

- components:

  Long tidy data frame, one row per (mediator, term). Terms:
  `"fZ_XM * f1_MY"` (expectation moderation), `"f1_XM * fZ_MY"`
  (valuation moderation), `"fZ_XY (direct)"` (residual direct
  moderation), and `"sum (1+2+3)"` (their algebraic sum; should
  approximate the total `fZ_XY` for each single-mediator fit). The first
  three rows carry full SE/z/p columns; the sum row has `est` only.

- fits:

  A list with `$direct` (total model) and `$full` (the multi-mediator
  [`pathXMY()`](https://dustin-wood.github.io/funfield/reference/pathXMY.md)
  object).

## See also

[`pathXMY`](https://dustin-wood.github.io/funfield/reference/pathXMY.md),
[`plotPathXMY`](https://dustin-wood.github.io/funfield/reference/plotPathXMY.md)
