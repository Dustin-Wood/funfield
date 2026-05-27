# Test of Condition Differences across Situation-Action (s,i) Pairs

Test whether a (dichotomous) condition produces mean-level differences
in how people rate situation-action expected outcomes

## Usage

``` r
conditionTest(PSIdata.cond, hiCond, loCond, locEE)
```

## Arguments

- PSIdata.cond:

  PSI-structured datafile, with "condition" as the final

- hiCond:

  exact name of 'high' condition (to be scored as '1')

- loCond:

  exact name of 'low' condition (to be scored as '-1')

- locEE:

  location of expected outcome ratings to test for condition
  associations - usually something like `c(4:8)`, as first three columns
  should be `p,s,i`

## Value

Means, correlations and p-values linking expected outcome ratings to
high/low condition levels

- siMeans:

  Situation-Action mean ratings, given in separate dataframe by
  condition

- rXCond:

  correlation between expected effect (X) and condition (L=-1,H=1)

- pXCond:

  statistical significance of these correlations (or mean difference,
  which is equivalent, btw)

## Details

This assumes a very regimented c("p","s","i",...) data structure.
Additionally, the code is currently only for dichotomous condition
levels. If a condition has more than 2 factor levels, then a different
function will have to be employed (perhaps using 'aov' routine)
