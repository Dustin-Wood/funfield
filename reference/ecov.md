# Effect Covariance Matrix

Creates the effect covariance matrix from the long-format P\*S\*I effect
file (usually estimated by function `ecov_long`)

## Usage

``` r
ecov(data,fX,fY,dXdY="cov")
```

## Arguments

- data:

  effect covariance elements (should be generated from `ecov_long`)

- fX:

  Variable containing the X expected outcome feature names

- fY:

  Variable containing the Y expected outcome feature names

- dXdY:

  Variable containing the product of X and Y expected effects

- order:

  specification of the order you would like the variables to be ordered
  within the output (defaults to `NA` if none given)

- byP:

  Do a 'two-step aggregation' in which observations are averaged
  within-person first, so that all participants observations are
  weighted equally (for instance: if one participant rated 8 scenarios,
  and another only rated 1).

## Value

Expected effect covariance (and correlation) matrix; & associated sample
sizes

## Details

This function utilizes functions from `plyr` and `tidyr`

This will also place features in alphabetical order by default - which
is almost always suboptimal. This can be altered by specifying an
`order`, in the form of a vector with the names of the variables in the
preferred order.
