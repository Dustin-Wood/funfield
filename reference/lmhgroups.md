# Create Low-Medium-High Subgroups at Specified Levels

This will return three variables detailing whether the participant
should be placed in a 'low', 'medium' or 'high' subgroup

## Usage

``` r
lmhgroups(voi, cuts, per = F)
```

## Arguments

- voi:

  variable of interest

- cuts:

  Two numbers specifying (1) highest value for LOW membership, and (2)
  lowest value for HIGH membership. Remainder will be placed in MIDDLE
  group.

- per:

  Should VOI be converted to a 'percentage rank' variable? (Default to
  `F`) If `T` then `cuts` shift to becoming probabilities to cut at
  (i.e., values should be between 0 and 1)

## Value

weight for low, medium, and high z-score groups

## Details

Note that the sum of the three groups will equal the total sample of
non-missing values. Can be used in combination with `effectiveN`.

## Examples

``` r
#create about equal sized subgroups (~33%) if(!) there are no ties in data
xcuts<-lmhgroups(x, cuts = c(1/3,2/3), per = T)
#> Error: object 'x' not found
```
