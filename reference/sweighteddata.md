# Situation Weighted Data

In brief: weight situations by a variable of interest. Mainly of value
for eventually combining multiple ratings to a single score. For
instance: combining diverse ESJT ratings to a single score weighted by
the situation's assertiveness-diagnosticity.

## Usage

``` r
sweighteddata(ESJT, voiset, voi)
```

## Arguments

- ESJT:

  ESJT raw data; must be headed with three variables: `p, s, i` - for
  'Person', 'Situation', '\[Initiated\] action', respectively

- voiset:

  Set containing the variable of interest

- voi:

  Name of the specific variable within the voiset you wish to weight by

- bySet:

  columns you wish to match by (defaults to 'situation & action')

## Value

The expected effects of the action, weighted by the action's
situation-diagnosticity
