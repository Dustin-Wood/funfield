# Field Overlays for Different Factors

This function will split force moderators that have been specified and
estimated within a lavaan model (from a previous
`fit <- sem(model,data)` statement) into separate field matrices
detailing what forces should be added to the base diagram when the
moderator variable has a level of '1' (often: 1=TRUE)

## Usage

``` r
fieldOverlays(fitPars, fmatrix, sigOnly = F)
```

## Arguments

- fitPars:

  can provide as `summary(fit)$pe` statement in lavaan

- fmatrix:

  details the elements comprising the field matrix

- sigOnly:

  should only the significant associations be kept? Defaults to `FALSE`

## Value

Will return `Fk` list of forces added by each moderator that was
specified within lavaan model

## Details

The moderators should NOT be contained within the field matrix, and
within the lavaan model, moderators should be specified as y ~ x:mod
rather than as y ~ mod:x (as this function assigns what follows ':' as
the moderator variable).

Generally, the moderator variables should be scaled in such a manner
that 0 is a meaningful point for all moderators, as this will make the
base model (where all moderators = 0) more meaningful. This can be
facilitated by having moderator variables by dummy-coded (values of 0 or
1), effect coded (values of -1 or 1), or standardized (M=0,SD=1). See
Cohen, Cohen, West, and Aiken (2003), Chapters 8 and 9.
