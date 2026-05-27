# Situation-Action Weighted Data

Weight situation-action expected effects by a variable of interest. Of
value for combining multiple situation-action pairs into a single
situation, weighted by the situation-action pair's trait diagnosticity.
(\*\*This function is intended to replace "sweighteddata" soon.)

## Usage

``` r
siWeightedData(ESJT, voiset, "voi")
```

## Arguments

- ESJT:

  ESJT raw data; must be headed with three variables: `p, s, i` - for
  'Person', 'Situation', '\[Initiated\] action', respectively

- voiset:

  Set containing the variable of interest; must be headed with ONLY
  variables `s, i`.

- voi:

  Name of the specific variable within the voiset you wish to weight
  by - should be a variable name given in "quotes"

- bySet:

  columns you wish to match by (defaults to 'situation & action')

## Value

The expected effects of the action, weighted by the action's
situation-diagnosticity
