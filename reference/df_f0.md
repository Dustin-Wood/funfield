# Apply f0() to numeric columns of a data frame

Convenience wrapper: returns a copy of `df` where every numeric column
has been formatted via
[`f0`](https://dustin-wood.github.io/funfield/reference/f0.md). Useful
as a pre-processing step before passing a coefficient table to
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) or
[`group_kable`](https://dustin-wood.github.io/funfield/reference/group_kable.md)
when the no-leading-zero style is in effect.

## Usage

``` r
df_f0(df, digits = 3, keep_sign = FALSE, cols = NULL)
```

## Arguments

- df:

  A data frame.

- digits, keep_sign:

  Passed through to
  [`f0`](https://dustin-wood.github.io/funfield/reference/f0.md).

- cols:

  Optional character vector of column names to format. If `NULL`
  (default), all numeric columns are formatted.

## Value

A data frame the same shape as `df`; numeric columns are now character.
