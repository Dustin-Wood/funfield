# Create High-Medium-Low Z-Score Groups

This will return three variables detailing the weight that each
individual should receive for subset analyses to look at how relatively
"high" or "low" scorers on the measure

## Usage

``` r
zgroups(voi)
```

## Arguments

- voi:

  variable of interest

## Value

weight for low, medium, and high z-score groups

## Details

Note that the sum of the three groups will equal the total sample of
non-missing values. Can be used in combination with `effectiveN`.
