# \`knitr::kable()\` wrapper with stripped leading zeros

Thin wrapper that pre-processes a data frame via
[`df_f0`](https://dustin-wood.github.io/funfield/reference/df_f0.md)
before handing it to
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html). Used in
funfield vignettes to keep tabular output consistent with the house
style: decimals less than one render as `.123` rather than `0.123`.

## Usage

``` r
kable0(x, digits = 3, keep_sign = FALSE, align = NULL, ...)
```

## Arguments

- x:

  A data frame or anything
  [`df_f0`](https://dustin-wood.github.io/funfield/reference/df_f0.md)
  can format (numeric columns become character with stripped leading
  zeros).

- digits:

  Number of decimal places (default 3). Passed to
  [`df_f0()`](https://dustin-wood.github.io/funfield/reference/df_f0.md),
  not to [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html).

- keep_sign:

  Logical; if `TRUE`, prepend `"+"` for non-negative non-zero values.
  Default `FALSE`.

- ...:

  Additional arguments passed to
  [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) (e.g.,
  `caption`, `col.names`, `format`).

## Value

A `knitr_kable` object.
