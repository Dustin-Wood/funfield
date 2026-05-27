# Main Functional Field Results

Fit values of paths for a functional field, given an associated
*expectancy* matrix.

## Usage

``` r
fieldResults(fModel, covMat, sampleN = 4)
```

## Arguments

- fModel:

  field model, specifying which paths are freely estimated vs.
  constrained to zero

- covMat:

  covariance matrix to be fit to a field model (can be either a
  correlation or covariance matrix)

- sampleN:

  sample size associated with the covariance matrix (defaults to '4',
  which will basically make sure everything is as `ns` as possible)

## Value

list with estimated:

- eMatrix:

  expectancy matrix

- fMatrix:

  field matrix

- uMatrix:

  'uniqueness' matrix (i.e., expectancy matrix not predicted by field
  matrix)

- uMatrix2:

  'uniqueness' matrix2 (a crude work-around for when lavaan does not
  want to estimate correctly; see
  https://github.com/yrosseel/lavaan/issues/44

- parTable:

  parameter estimates, with associated significance values

## Details

Note that the `uMatrix2` is a servicable means of estimating the
unpredicted residuals in some of the models where lavaan does not want
to provide the path-analysis implied residuals. But it should only work
when the model is a DAG that has been placed to have rows in 'causal
order'. There is also a problem with how `uMatrix2` is estimated (the
final covariance is always misestimated) which I assume involves a bad
interaction with how `expOutcomes` function closes out
