# Scale-Center-Standardized PSI Ratings

Transforms ESJT ratings into a z-score-like metric indicating the number
of 'standard-deviations-from-zero' the person's original rating of the
item was.

## Usage

``` r
zPSI(PSIdata)
```

## Arguments

- PSIdata:

  original PSI data (should have 'p','s','i' for first three columns)

## Value

Standardized scale-centered scores.

Original matrix but with data now equating level of variability for each
participant

## Details

This syntax implicitly assumes that 0 is a meaningful point on the
scale. The original data collection and the prior processing of the data
should reflect this.

\* The scale-centered scores do NOT have the usual interpretation of
\[regular\] z-scores as being translatable to approximate probabilities
assuming a normal distribution of responses.

\* If the person gives the same score to all items, all `zcx` will equal
`1`.
