# Expected-Outcome Covariance Matrices for Low, Med, High Subgroups on a Variable of Interest

Estimate how expected outcomes covary for 'low', 'medium' and 'high'
levels of a 'person variable of interest' (`pvoi`).

Note that this creates 'low' 'medium' and 'high' subgroups through the
strategy employed by the `zgroups` function, which attempts to
approximate how observations are weighted when calculating a correlation
coefficient.

## Usage

``` r
ecov.lmh(PSI, pVars, voi, p = "p", cuts = "z", per = F)
```

## Arguments

- PSI:

  the Person-Situation-Action expected effects relevant to the analysis

- pVars:

  file containing person variables

- voi:

  (person-)variable of interest (must provide in quotations)

- p:

  the variable containing the 'person' identifier (defaults to `"p"`)

- cuts:

  detail how groups will be created; default to `z` to indicate
  `zgroups` will be formed

## Value

Effect covariance matrices (and sample sizes) for `zL, zM, zH` (low,
medium, and high z-scores on the person variable of interest) subgroups
