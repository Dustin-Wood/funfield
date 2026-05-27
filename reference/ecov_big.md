# Estimate Basic Covariance Elements

Compute all possible products of effects for all features from weighted
ESJT data, `wFFdata`. Note that the average of the `dAdB` elements will
ultimately equal the covariances forming the expectancy matrix used to
estimate functional field models. Note that this action can be
particularly useful when there is a SINGLE action (or TWO MAIN actions)
that are being considered for a given scenario (such as 'Cooperate vs.
Defect?' in a "Prisoner's Dilemma" situation; or 'Go straight vs.
Swerve?' in a "Chicken" situation). If there are MORE than two actions
being represented for a single situation, you may want to use the
`ecov_long` function instead.

Note: data can be weighted using `sweighteddata` function

## Usage

``` r
ecov_big(wFFdata, P_ID = "P_ID", S_ID = "S_ID")
```

## Arguments

- wFFdata:

  weighted functional field data

## Value

Effect covariance matrix

## Details

Usage notes: FFdata should have format of `P_ID`, `S_ID`, `Did_A`, as
first three columns, followed by effect ratings, and end in `Likelihood`
