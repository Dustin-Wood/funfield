# Weight Expectancy Data

In brief: weight situations by a situational and/or action variable of
interest. Mainly of value for eventually combining multiple scenarios
into a single score. For instance: combining diverse ESJT ratings to a
single score weighted by the situation's assertiveness-diagnosticity.

## Usage

``` r
sweighteddata(FFdata, voiset, voi)
```

## Arguments

- Ex:

  ESJT raw data; must be headed with three variables, which should
  generally be `p, s, i` (for person, situation, and action,
  respectively)

- voiset:

  Set containing the variable of interest

- voi:

  Name of the specific variable within the voiset you wish to weight by

- si_ids:

  = Name of situation and action identifiers, common to both files

## Value

The expected effects of the action, weighted by the action's
situation-diagnosticity
