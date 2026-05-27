# Implied Covariance Matrix from a Field Model

This function will indicate the implied covariance matrix from a set of
forces. (It seems like this should be done in lavaan through
`lavInspect(fit, "implied")$cov`, but the covariances regularly seem to
be over-accounted for by specified forces in a way that I have not
figured out how to correct.)

(Note that this function was adapted from code previously buried inside
function `fieldResults`.

## Usage

``` r
impliedCov(fieldmat, covmat)
```

## Arguments

- fieldmat:

  field matrix - usually created from lavaan::sem

- covmat:

  specify the observe correlations or covariances between the features

## Value

Will return `implied_cov`, with implied covariances from the specified
field model
