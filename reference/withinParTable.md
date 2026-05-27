# Within-Person N Adjustment for ParTable

The standard 'parTable' in lavaan does not provide correct degrees of
freedom when working with a deviation-score data file; this corrects
that

## Usage

``` r
withinParTable(fit, data)
```

## Arguments

- fit:

  standard output from an sem(...) statement in lavaan

- data:

  original 'long' format data frame that was used in sem(...) statement

## Value

parTable in which se, z, and p statistics are adjusted to person level

## Details

(This may become outdated as procedures for doing multilevel analyses
with field models become further developed. It should also substantially
underestimate significance, I believe if 3 or more observations are
within-person)
