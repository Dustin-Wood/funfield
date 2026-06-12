# General Path / SEM Field Models with Optional Moderation

Fits an arbitrary recursive path (SEM) model to PSI long-format data
with cluster-robust standard errors, generalizing
[`pathXMY`](https://dustin-wood.github.io/funfield/reference/pathXMY.md)
from the single \\X \to M \to Y\\ triangle to cascades \\X \to M_1 \to
M_2 \to \dots \to Y\\ and to arbitrary directed acyclic path structures.

The structural part can be supplied two ways:

- `order`: a character vector of variables in presumed **causal order**.
  This auto-expands to the *saturated lower-triangular cascade* in which
  every causally upstream variable predicts every downstream variable
  (`M1 ~ X`; `M2 ~ X + M1`; `M3 ~ X + M1 + M2`; `Y ~ X + M1 + M2 + M3`).

- `paths`: a character vector of lavaan-style regression formulas (e.g.
  `c("Y ~ M2", "M2 ~ X + M1", "M1 ~ X")`) for an arbitrary DAG.

Supply exactly one of `order` or `paths`.

When a moderator `Z` is given, every structural edge \\b\\A\\ is
expanded to \\f_1 A + f_Z (A \times Z)\\ and a `Z` main effect is added
to each endogenous equation, exactly as in
[`pathXMY()`](https://dustin-wood.github.io/funfield/reference/pathXMY.md).
Indirect effects are enumerated along every directed \\X \to Y\\ path of
two or more edges, each as a product of edge coefficients, plus (under
moderation) the first-order route-moderation terms.

## Usage

``` r
pathF(
  data,
  order = NULL,
  paths = NULL,
  X = NULL,
  Y = NULL,
  Z = NULL,
  Z.within = FALSE,
  cluster = "p",
  situation = "s",
  controls = NULL,
  deviate = c("auto", "none"),
  conf.level = 0.95,
  check.deviation = TRUE,
  suppress.warnings = TRUE,
  max_paths = 256L
)
```

## Arguments

- data:

  A data frame in PSI long format (`p`, `s`, `i` as the first columns).
  The cascade/path variables are within-(person, situation) deviated
  automatically (see `deviate`); `Z` is handled separately.

- order:

  Optional character vector of variable names in presumed causal order.
  Expands to the saturated lower-triangular cascade. Supply this *or*
  `paths`, not both.

- paths:

  Optional character vector of lavaan-style regression formulas
  (`"lhs ~ rhs1 + rhs2 + ..."`) describing an arbitrary DAG. Each
  right-hand-side term `r` contributes a directed edge `r -> lhs`.

- X, Y:

  Optional names of the focal initiating action and outcome used to
  enumerate indirect-effect routes. Default to the first and last
  element of `order`; with `paths`, default to the unique source / sink
  node when one exists (otherwise indirect enumeration is skipped unless
  both are supplied).

- Z:

  Optional moderator (length-1 character). When `NULL`, the unmoderated
  model is fit.

- Z.within:

  Logical (default `FALSE`). When `TRUE`, `Z` is within-cluster deviated
  (situation-level moderator); when `FALSE` it is z-standardized
  globally (between-person trait).

- cluster:

  Name of the clustering variable (default `"p"`); set to `NULL` to fit
  without cluster-robust SEs (not recommended).

- situation:

  Name of the situation column (default `"s"`), paired with `cluster`
  for automatic within-cell deviation.

- controls:

  Optional character vector of control variables added to every
  endogenous equation.

- deviate:

  One of `"auto"` (default) or `"none"`; see
  [`pathXMY`](https://dustin-wood.github.io/funfield/reference/pathXMY.md).

- conf.level:

  Confidence level for CI columns (default 0.95).

- check.deviation:

  Logical (default `TRUE`); warn if the cascade variables do not look
  within-person deviated.

- suppress.warnings:

  Logical (default `TRUE`); muffle the cosmetic non-positive-definite
  vcov warning lavaan emits at machine precision.

- max_paths:

  Integer (default 256). Cap on the number of directed \\X \to Y\\ paths
  enumerated for indirect effects. A saturated \\K\\-node cascade has
  \\2^{K-2}\\ such paths; above the cap, indirect definitions are
  skipped with a warning (the structural edges are still returned).

## Value

A list of class `"pathF"` with:

- tidy:

  A tidy data frame, one row per structural edge and per indirect-effect
  term. Columns: `param`, `src`, `tgt`, `est`, `se`, `z`, `pvalue`,
  `ci.lower`, `ci.upper`. Edge `param`s use the F-schema source-target
  form `f1_<src>_<tgt>` / `fZ_<src>_<tgt>`; indirect rows use a
  ` * `-joined product string (e.g. `f1_X_M * f1_M_Y`,
  `fZ_X_M * f1_M_Y`).

- fit:

  The fitted `lavaan` object.

- spec:

  The parsed specification (`nodes`, `edges`, `X`, `Y`).

- model:

  The generated lavaan model string (useful for debugging).

## Details

A saturated recursive path model is just-identified and algebraically
equivalent to a sequence of OLS regressions, so lavaan fits it without
iteration. Under moderation the interaction terms across many edges can
be highly collinear, inflating standard errors; inspect `$model` and the
SEs when the cascade is long.

The 3-variable saturated cascade `order = c(X, M, Y)` reproduces the
single-mediator
[`pathXMY()`](https://dustin-wood.github.io/funfield/reference/pathXMY.md)
fit exactly: `f1_X_M` \\=\\ `f1_XM`, `f1_X_Y` \\=\\ `f1_XY`, `f1_M_Y`
\\=\\ `f1_MY`, and the indirect/route-moderation products match.

## See also

[`pathXMY`](https://dustin-wood.github.io/funfield/reference/pathXMY.md)
for the X-M-Y special case (loop vs joint mediator handling, bootstrap
SEs).

## Examples

``` r
if (FALSE) { # \dontrun{
data(speedingESJT)
PSI <- speedingESJT$PSI

## Saturated causal cascade Speed -> OnTime -> Appropriate -> Likelihood
fitc <- pathF(PSI, order = c("Speed", "OnTime", "Appropriate", "Likelihood"))
fitc$tidy

## Same, moderated by a between-person trait
sr <- speedingESJT$traits[, c("p", "SR_30")]
names(sr)[2] <- "SRFast"
dd <- merge(PSI, sr, by = "p")
fitm <- pathF(dd, order = c("Speed", "OnTime", "Appropriate", "Likelihood"),
              Z = "SRFast")
fitm$tidy

## Arbitrary DAG via formulas
pathF(PSI, paths = c("Likelihood ~ Appropriate + OnTime",
                     "Appropriate ~ Speed",
                     "OnTime ~ Speed"),
      X = "Speed", Y = "Likelihood")
} # }
```
