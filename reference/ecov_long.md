# Estimate Action Expected-Outcome Covariance Elements

Compute all possible products of effects for all features from weighted
ESJT data, `wFFdata`. The average of the `dAdB` elements will ultimately
equal the covariances forming the expectancy matrix used to estimate
functional field models. Note \#1: this function a more generalized
version of function `ecov_big`, which provides flexibility of allowing
multiple actions per scenario Note \#2: this function assumes that the
features have a "meaningful zero point." That is a `0` should mean "no
effect", and negative values should mean "negative effect", and positive
values should mean "positive effect", with greater deviations from zero
meaning "stronger effect." If data is not prepared this way, the results
are not very likely to be meaningful.

Note: data can be weighted using `sweighteddata` function

## Usage

``` r
ecov_long(
  eData,
  p = "p",
  s = "s",
  i = "i",
  fcols = c(4:ncol(eData)),
  addDidi = T
)
```

## Arguments

- eData:

  expected effect data

- p:

  variable identifying 'person' (or respondent) - give in "quotemarks"

- s:

  variable identifying 'situation' (or scenario) - give in "quotemarks"

- i:

  variable identifying 'action' (or response) - give in "quotemarks"

- fcols:

  columns with variables for field matrix - defaults to c(4:ncol(eData))
  assuming a c(p,s,i,(etc)) data structure

- addDidi:

  should you add the 'did_i' variable as element for effect covariance
  matrix, if not already present? Defaults to `TRUE`.

## Value

Effect covariance elements

## Details

Usage notes: eData should typically have format of `p`, `s`, `i`, as
first three columns, followed by effect ratings, and end in
`Likelihood`. If there is a preferred ordering of columns, you should
put eData into that ordering \*before\* running this code.
