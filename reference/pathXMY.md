# Path Analysis for X -\> M -\> Y Structures with Optional Moderation

Fits the path model \$\$M = \beta^1\_{MX} X + \beta^Z\_{MX}(X \times
Z) + \beta^Z_M Z + e_M\$\$ \$\$Y = \beta^1\_{YX} X + \beta^Z\_{YX}(X
\times Z) + \beta^1\_{YM} M + \beta^Z\_{YM}(M \times Z) + \beta^Z_Y Z +
e_Y\$\$ via `lavaan` with cluster-robust standard errors (clustering on
`cluster`, typically the person ID `"p"` in ESJT data).

Omitting `M` collapses to the direct \\X \to Y\\ model (optionally
moderated). Omitting `Z` collapses to the unmoderated version. Supplying
multiple variable names in `M` loops the model over each mediator one at
a time and returns a single stacked tidy table.

## Usage

``` r
pathXMY(
  data,
  X,
  Y,
  M = NULL,
  Z = NULL,
  Z.within = FALSE,
  cluster = "p",
  situation = "s",
  controls = NULL,
  deviate = c("auto", "none"),
  se = c("cluster", "boot"),
  nboot = 500,
  conf.level = 0.95,
  check.deviation = TRUE,
  suppress.warnings = TRUE,
  joint = TRUE
)
```

## Arguments

- data:

  A data frame in PSI long format (`p`, `s`, `i` as the first columns).
  By default the Level-1 columns `X`, `M`, `Y` are within-(person,
  situation) deviated automatically (see `deviate`); `Z` is handled
  separately.

- X:

  Name of the initiating action / predictor variable (length-1
  character).

- Y:

  Name of the outcome variable (length-1 character; typically `"L"` for
  likelihood).

- M:

  Optional. Name(s) of candidate mediator variables (character vector).
  When `NULL`, the direct \\X \to Y\\ model is fit (no mediator). When
  length \> 1, the X-M-Y model is fit once per mediator.

- Z:

  Optional. Name of the moderator variable (length-1 character). When
  `NULL`, the unmoderated model is fit. By default (`Z.within = FALSE`),
  `Z` is treated as a between-person trait and z-standardized globally.
  Set `Z.within = TRUE` for a situation-level moderator that varies
  within persons (e.g., an experimentally varied condition); in that
  case `Z` is within-cluster deviated instead.

- Z.within:

  Logical (default `FALSE`). When `TRUE`, `Z` is treated as a
  within-cluster (situation-level) variable and is within-person
  deviated before fitting, exactly like the Level-1 predictors. Use this
  when the moderator varies across situations within a person (e.g., a
  randomly assigned scenario condition). When `FALSE` (default), `Z` is
  z-standardized globally (between-person trait interpretation).

- cluster:

  Name of the clustering variable (default `"p"`). Set to `NULL` to fit
  without cluster-robust SEs (not recommended for ESJT data).

- situation:

  Name of the situation/scenario column (default `"s"`), paired with
  `cluster` to define the (person, situation) cells used by automatic
  deviation.

- controls:

  Optional character vector of control variables added to both
  equations.

- deviate:

  One of `"auto"` (default) or `"none"`. With `"auto"`, `X`, `M`, and
  `Y` are within-(person, situation) deviated before fitting — each
  value centered on the mean of the actions rated in the same
  `(cluster, situation)` cell — provided a `situation` column is present
  and every cell holds at least two rows. When some cells hold a single
  action the data is left untouched (deviate it yourself first, e.g.
  with
  [`devPSI`](https://dustin-wood.github.io/funfield/reference/devPSI.md)).
  With `"none"`, no deviation is applied and pre-deviated data is
  assumed.

- se:

  One of `"cluster"` (default; sandwich SEs from lavaan) or `"boot"`
  (cluster bootstrap of `nboot` replicates).

- nboot:

  Number of bootstrap replicates when `se = "boot"`.

- conf.level:

  Confidence level for CI columns (default 0.95).

- check.deviation:

  Logical. When `TRUE` (default), warn if `X`, `M`, or `Y` still do not
  look within-person deviated after the `deviate` step — relevant mainly
  when `deviate = "none"` and the data was not pre-deviated.

- suppress.warnings:

  Logical. When `TRUE` (default), suppress the cosmetic non-PD vcov
  warning that lavaan emits at machine precision.

- joint:

  Logical (default `TRUE`). When `length(M) > 1`, additionally fit a
  single *joint* multi-mediator model in which all mediators appear
  simultaneously in the Y equation. The joint fit contributes a residual
  \\X \to Y\\ path (after controlling for the full mediator set), and
  parallel per-mediator coefficients with the suffix `_joint`. See
  *Details*.

## Value

A list with three elements:

- tidy_loop:

  A tidy data frame with one row per (mediator x parameter), assembled
  from **separate single-mediator regressions** — one X-M-Y fit per
  mediator, fit independently. The Y equation in each row's source model
  contains exactly one mediator. *These coefficients are not from a
  simultaneous regression and should not be read as partial effects net
  of the other mediators.* Columns: `mediator`, `param`, `est`, `se`,
  `z`, `pvalue`, `ci.lower`, `ci.upper`. Parameter labels follow the XMY
  convention: `B1_MX`, `BZ_MX`, `B1_YX`, `BZ_YX`, `B1_YM`, `BZ_YM`, plus
  the indirect effects `B1_MX * B1_YM` (unmoderated only),
  `BZ_MX * B1_YM` (\\= \beta^Z\_{MX} \cdot \beta^1\_{YM}\\), and
  `B1_MX * BZ_YM` (\\= \beta^1\_{MX} \cdot \beta^Z\_{YM}\\). When
  `M = NULL`, this slot holds the rows from the single Y-on-X direct
  regression (`mediator = NA`).

- tidy_joint:

  A tidy data frame from the **single simultaneous multi-mediator
  regression** in which all mediators appear together in the Y equation.
  `NULL` when `joint = FALSE` or `length(M) <= 1`. Per-mediator rows
  (`B1_MX_joint`, `BZ_MX_joint`, `B1_YM_joint`, `BZ_YM_joint`) are
  indexed by `mediator`; global rows (`B1_YX_joint`, `BZ_YX_joint`)
  carry `mediator = NA`. The `B*_YM_joint` coefficients are partial
  slopes net of the other mediators and are *not* comparable to the
  single-mediator `B*_YM` values in `tidy_loop`.

- fits:

  A named list of the lavaan fit objects (one per mediator, named by the
  mediator variable name; `"_direct"` when `M = NULL`; `"_joint"` for
  the joint multi-mediator fit when present).

## Details

**Deviation.** `cluster = "p"` in lavaan provides cluster-robust
sandwich standard errors on a single-level SEM; it is *not* a multilevel
model. Correct within-person path coefficients therefore require the
Level-1 variables (`X`, `M`, `Y`) to be deviated within each
`(cluster, situation)` cell. By default (`deviate = "auto"`) `pathXMY()`
does this internally; pass `deviate = "none"` for data that is already
deviated. When more than one action is rated per (person, situation),
within-cell deviation isolates the within-situation action contrast the
field model is about; with a single situation per person it reduces to
ordinary within-person deviation. `Z` is handled separately: a
between-person trait moderator (`Z.within = FALSE`) is z-standardized, a
situation-level moderator (`Z.within = TRUE`) is within-person deviated.

**Small cluster counts.** The cluster-robust z-tests assume the number
of clusters \\G\\ is large. With \\G \< 50\\, results may be
anti-conservative; consider `se = "boot"`.

**Naming convention.** Parameters use the X-M-Y suffix convention from
Wood, Harms, & Cho (2023): `B1_*` are main-effect coefficients, `BZ_*`
are Z-moderated coefficients; `B*_MX` parameters regress M on X, `B*_YM`
regress Y on M, `B*_YX` are the (direct) Y on X coefficients.

**Loop vs joint fits — two different regressions, two different
tables.** With multiple mediators, `pathXMY()` produces two separate
tidy tables, deliberately kept apart because they answer different
questions and the coefficients are *not* interchangeable:

- `$tidy_loop` comes from a "loop" pass: one independent X-M-Y model is
  fit *per mediator*, each with only that single mediator in the Y
  equation. A row labeled `B1_YM` for mediator \\m_k\\ is the slope of Y
  on \\m_k\\ controlling for X — nothing else. The loop pass is the
  inferential workhorse and supports the stable expectation-route
  summary `BZ_MX * B1_YM`.

- `$tidy_joint` comes from a single simultaneous regression where *all*
  mediators appear together in the Y equation. A row labeled
  `B1_YM_joint` for mediator \\m_k\\ is the partial slope of Y on
  \\m_k\\ *net of every other mediator in M*. These are not comparable
  to the loop `B1_YM` values and will routinely differ in magnitude (and
  sometimes sign) when mediators are correlated.

`B1_YX_joint` and `BZ_YX_joint` are the most useful joint outputs: they
index the residual direct \\X \to Y\\ (and its Z-moderation) after
controlling for the entire mediator set, and serve as a diagnostic of
whether the measured mediators absorb the total `BZ_YX` moderation.
*Per-mediator* joint coefficients (especially `BZ_YM_joint`) are
typically less stable than their loop counterparts when mediators are
numerous or correlated, because the M-by-Z product terms are highly
collinear (Wood, Adanu, & Harms, 2025). For inference about a single
mediator's role, prefer the loop coefficient in `$tidy_loop`; treat the
joint coefficients in `$tidy_joint` as a system-level diagnostic.

## Examples

``` r
if (FALSE) { # \dontrun{
data(speedingESJT)
## X, M, Y are within-(person, situation) deviated automatically.
PSI <- speedingESJT$PSI

## Unmoderated mediation, one mediator
res1 <- pathXMY(PSI, X = "Speed", Y = "Likelihood", M = "Crash")
res1$tidy_loop

## Mediation across all eight outcome features
mediators <- c("Crash","Injured","Ticket","MoneyCost","OnTime",
               "IntQuality","FunDrive","Appropriate")
res2 <- pathXMY(PSI, X = "Speed", Y = "Likelihood", M = mediators)
## Loop pass: per-mediator single-mediator regressions
res2$tidy_loop
## Joint pass: simultaneous multi-mediator regression
res2$tidy_joint
} # }
```
