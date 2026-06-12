# Decompose total and Z-moderated X-Y effects across a path model's routes

Generalizes
[`pathXMY_decompose`](https://dustin-wood.github.io/funfield/reference/pathXMY_decompose.md)
from the single-mediator triangle to arbitrary
[`pathF`](https://dustin-wood.github.io/funfield/reference/pathF.md)
models. Two models are fit:

- Total model:

  The bare \\X \to Y\\ regression (with \\X \times Z\\ moderation when
  `Z` is supplied), giving the total effect \\F_1^{\*}\[X,Y\]\\ and
  total moderation \\F_Z^{\*}\[X,Y\]\\.

- Path model:

  The full
  [`pathF()`](https://dustin-wood.github.io/funfield/reference/pathF.md)
  model specified by `order` or `paths`, in which the total decomposes
  across every directed \\X \to Y\\ route.

The baseline identity generalizes the Baron-Kenny sum: the total
\\F_1^{\*}\[X,Y\]\\ equals the sum over routes of each route's product
of \\F_1\\ edges, plus the residual direct \\F_1\[X,Y\]\\ (exact for the
saturated cascade, where the path model reproduces the OLS algebra).

The moderation identity generalizes the Muller-Judd-Yzerbyt (2005)
decomposition: for each route, \\Z\\'s moderation "slides" through the
chain, contributing one first-order term per edge position — the
moderated edge's \\F_Z\\ times the \\F_1\\ of every other edge on the
route. For a two-mediator chain \\X \to M_1 \to M_2 \to Y\\:

\$\$F_Z^{\*}\[X,Y\] \approx F_Z\[X,M_1\] F_1\[M_1,M_2\] F_1\[M_2,Y\] +
F_1\[X,M_1\] F_Z\[M_1,M_2\] F_1\[M_2,Y\] + F_1\[X,M_1\] F_1\[M_1,M_2\]
F_Z\[M_2,Y\] + F_Z\[X,Y\]\$\$

summed over every route when the DAG has several. The identity is
first-order: substituting the moderated equations through the chain
produces \\Z^2, Z^3, \dots\\ cross-terms (products of two or more
\\F_Z\\ coefficients) that the decomposition drops, so the approximation
can loosen as chains lengthen. The `gap` rows in the output make the
approximation quality explicit.

## Usage

``` r
pathF_decompose(
  data,
  order = NULL,
  paths = NULL,
  X = NULL,
  Y = NULL,
  Z = NULL,
  Z.within = FALSE,
  cluster = "p",
  ...
)
```

## Arguments

- data:

  A data frame in PSI long format (see
  [`pathF`](https://dustin-wood.github.io/funfield/reference/pathF.md)).

- order, paths:

  The path structure, passed to
  [`pathF`](https://dustin-wood.github.io/funfield/reference/pathF.md);
  supply exactly one.

- X, Y:

  Optional focal action / outcome (defaults resolved by
  [`pathF()`](https://dustin-wood.github.io/funfield/reference/pathF.md)
  from `order` or the DAG's unique source/sink).

- Z:

  Optional moderator. When `NULL` only the baseline (\\F_1\\)
  decomposition is returned.

- Z.within, cluster, ...:

  Passed to
  [`pathF`](https://dustin-wood.github.io/funfield/reference/pathF.md).

## Value

A list with three elements:

- total:

  Data frame with the no-mediator total-model estimates: one row for
  `f1*` and (when `Z` is supplied) one for `fZ*`.

- components:

  Long tidy data frame with a `block` column (`"f1"` or `"fZ"`) and a
  `term` column. Within each block: one row per route product (for
  `"fZ"`, one row per route x edge-position slide term), the residual
  direct path, their algebraic `sum`, the no-mediator `total`, and the
  `gap` (total - sum) indexing approximation quality. Route and direct
  rows carry full SE/z/p columns; sum/total/gap rows carry `est` only
  (total also carries its SE).

- fits:

  A list with `$direct` (total model) and `$full` (the
  [`pathF()`](https://dustin-wood.github.io/funfield/reference/pathF.md)
  object).

## See also

[`pathF`](https://dustin-wood.github.io/funfield/reference/pathF.md),
[`pathXMY_decompose`](https://dustin-wood.github.io/funfield/reference/pathXMY_decompose.md)
