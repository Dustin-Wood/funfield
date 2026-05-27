# Expected Outcomes Matrix

Estimate expected outcomes of current situation given current field

## Usage

``` r
expOutcomes(s, ff)
```

## Arguments

- s:

  situation vector or matrix, or 'situation states at a given time'

- ff:

  field matrix, or 'field at a given time'

## Value

expected situational states for next 'step' (increment of time)

## Details

Input `s` can be either a single situation or a range of possible
situations, with the situational states for each given on a separate row

(Hopefully this will soon be completely outmoded and replaced with
'expOutcomesFF')
