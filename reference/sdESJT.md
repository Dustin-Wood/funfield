# Standard deviation of ESJT ratings

This will index the standard deviation of the person's ratings across
all of the ESJT scenarios they rated.

## Usage

``` r
sdESJT(PSIdata, rmvLast = T)
```

## Arguments

- rmvLast:

  remove the final variable (e.g., if it is Likelihood ratings provided
  at different portion of survey)

- ESJT:

  ESJT matrix (should have 'p','s','i' as their first three features)

## Value

sdE.p - the percentage of situation-action pairs in which the respondent
showed zero response variability whatsoever.

## Details

It is recommended within Wood, Lowman, & Harms (202x) that respondents
showing values of sd.p below 25% of the scale maximum be removed from
all analyses

## Examples

``` r
#combine this function and recommended screen into a single statement:
analysiscases <- subset(sdESJT(ESJT), sdE.p > .25)
#> Error: object 'ESJT' not found
```
