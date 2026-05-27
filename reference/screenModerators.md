# Fast moderator screen for the three X-M-Y path coefficients

For each candidate trait in a battery, estimate its moderation of (a)
the total \\X \to Y\\ path (`BZ_YX[1]`, one number per trait), (b) the
per-mediator \\X \to M_k\\ paths (`BZ_MX`, one per trait per mediator),
and (c) the per-mediator \\M_k \to Y\\ paths (`BZ_YM`, one per trait per
mediator). A few [`cor.test()`](https://rdrr.io/r/stats/cor.test.html)
and [`lm()`](https://rdrr.io/r/stats/lm.html) calls in place of dozens
of SEM fits.

**The shortcuts.**

- **YX arm**:

  Per person, compute the within-person slope of `Y` on `X` (call it
  \\\Delta Y\\). Across persons, the Pearson correlation \\r(T, \Delta
  Y)\\ is algebraically related to `BZ_YX[1]` from a no-mediator
  [`pathXMY()`](https://dustin-wood.github.io/funfield/reference/pathXMY.md)
  fit by \$\$\mathrm{BZ\\YX}\[1\] = r(T, \Delta Y) \cdot
  \mathrm{SD}(\Delta Y),\$\$ because
  [`pathXMY()`](https://dustin-wood.github.io/funfield/reference/pathXMY.md)
  z-standardizes the between-person moderator internally (so the
  \\\mathrm{SD}(T)\\ factor that would otherwise appear cancels).

- **MX arm**:

  The same shortcut, with the mediator in place of `Y`: per person
  compute the within-person slope of each `M_k` on `X` (\\\Delta M_k\\)
  and correlate with `T`. One row per (trait, mediator).

- **YM arm**:

  Per-person \\Y\\-on-\\M\\ slopes are too sparse at typical EXSJT row
  counts to be useful (3 obs per person leaves 0 residual df once `X` is
  partialled out), so the YM shortcut is computed at the population
  level: a single OLS regression of within-person-deviated `Y` on
  within-person-deviated `X` and `M_k` with a `T_z * M_dev` interaction.
  The `M_dev:T_z` coefficient is the `BZ_YM` estimate (matches
  [`pathXMY()`](https://dustin-wood.github.io/funfield/reference/pathXMY.md)
  to ~0.1 percent in the speeding fit). The reported p-value comes from
  OLS and does **not** account for within-person clustering, so it is
  optimistic compared to
  [`pathXMY()`](https://dustin-wood.github.io/funfield/reference/pathXMY.md)'s
  cluster-robust p. Use the ranking for screening; use
  [`pathXMY_decompose()`](https://dustin-wood.github.io/funfield/reference/pathXMY_decompose.md)
  for inference.

## Usage

``` r
screenModerators(
  psi,
  X,
  Y,
  M = NULL,
  traits,
  trait_cols = NULL,
  p_col = "p",
  arms = c("YX", "MX", "YM")
)
```

## Arguments

- psi:

  A data frame with within-person observations of `X`, `Y`, and (for
  `MX`/`YM` arms) the mediators. Must contain a person ID column
  matching `p_col`.

- X, Y:

  Column names (length-1 character) for the focal X and Y.

- M:

  Character vector of mediator column names. Required when `arms`
  includes `"MX"` or `"YM"`.

- traits:

  A data frame with one row per person, a person ID column matching
  `p_col`, and one column per candidate trait.

- trait_cols:

  Character vector of trait column names in `traits` to screen. If
  `NULL` (default), all columns other than `p_col` are used.

- p_col:

  Person ID column name; default `"p"`. Must be present in both `psi`
  and `traits`.

- arms:

  Character vector subset of `c("YX", "MX", "YM")`; default all three.

## Value

A tidy data frame with one row per (trait \\\times\\ target \\\times\\
mediator) cell. Columns:

- `trait`:

  Trait column name.

- `target`:

  One of `"YX"`, `"MX"`, `"YM"`.

- `mediator`:

  Mediator name (NA for the YX target).

- `r`:

  Pearson correlation between the trait and the per-person slope (YX and
  MX targets only; NA for YM).

- `beta`:

  Estimate on the `BZ` scale. For YX/MX this is \\r \cdot
  \mathrm{SD}(\mathrm{slope})\\; for YM it is the `M_dev:T_z` OLS
  coefficient.

- `p`:

  Significance test *p*-value. For YX/MX this is the Pearson
  [`cor.test()`](https://rdrr.io/r/stats/cor.test.html) *p*; for YM it
  is the OLS t-test *p* (uncorrected for clustering — see Details).

- `n`:

  Sample size used.

Rows are sorted by descending `|beta|` within `target`.

## See also

[`pathXMY`](https://dustin-wood.github.io/funfield/reference/pathXMY.md),
[`pathXMY_decompose`](https://dustin-wood.github.io/funfield/reference/pathXMY_decompose.md)
