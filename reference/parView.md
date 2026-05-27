# Provide Rounded Parameter Estimates

This will return the parameter estimates from lavaan, but with values
rounded to a desired numer of decimal spaces.

## Usage

``` r
parView(fit, split = "est", dec = 3)
```

## Arguments

- fit:

  standard output from lavaan

- split:

  locate where variables will start to be rounded

- dec:

  number of variables to round to

## Value

parameter estimates rounded down to desired number of decimal spaces
