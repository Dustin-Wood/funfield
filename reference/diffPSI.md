# Difference Scoring of nI=2 PSI matrix

Will either return the difference Will estimate how p's rating of
outcomes of each action deviate within situation s deviated from their
average rating of outcomes of different actions in that situation

## Usage

``` r
diffPSI(devPSI, locX = c(4:ncol(devPSI)), select_i = "max", dbl = T)
```

## Arguments

- devPSI:

  deviated PSI-structured dataframe (should have 'p','s','i' as their
  first three features)

- locX:

  location of expected outcome ratings (default generously assumes the
  prescribed 'c(p,s,i,...,Likelihood)' data structure)

- select_i:

  Return the maximum or minimum of i?

- dbl:

  Double the deviation scores (default is T)

## Value

expected effects (difference) of focal action(s) from contrasting
action(s)

## Details

Note that when nI = 2, the dpsi scores will be reflections of one
another. In which case, keeping only the minimum or maximum i for each
scenario can be useful to help avoid artificial inflation of sample size
(although other strategies, like multilevel modeling in lavaan, may be
preferable to address this issue).

When nI is greater than 2, a more sophisticated script should be used,
which perhaps subtracts the focal action (e.g., A = 1) from the average
of the contrasting actions (e.g., A = 0)
