# Matrix Surgery

This function serves as a 'brute force' method for replacing values in
some matrix `x` with non-missing values of `y`.

## Usage

``` r
surgery(x, y, suppressOverwrite = T)
```

## Arguments

- x:

  the original matrix is 'old' matrix representing function field model
  paths

- y:

  matrix with non-missing cells you wish to overwrite cells in x

- suppressOverwrite:

  do you wish reports of overwritten cells to be suppressed? Defaults to
  `TRUE`

## Value

modified version of `x` overwritten with non-missing cells from `y`

## Details

Any cell in `y` given as a missing value (`NA`) will return the original
cell in `x`

Often, you will NOT want non-missing cells in `x` to be overwritten.
(This might indicate some problem in the model.) If so, you can specify
`suppressOverwrite = F` to print when overwrites have occurred.
