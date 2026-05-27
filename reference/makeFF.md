# Functional Field Builder

Creates a functional field function from a concise rules function,
handling the boilerplate of zero-matrix initialization and plan
application. The `ft[x, y] = expr(s)` assignment idiom is preserved
entirely inside `rules` — `makeFF` only wraps the setup and teardown.

## Usage

``` r
makeFF(rules)
```

## Arguments

- rules:

  A function `function(s, ft)` that populates the force matrix via
  assignment lines and returns `ft`. Expressions in each cell can be
  arbitrarily complex: simple scalars, AND/OR products, regression
  terms, or any R expression involving `s`.

## Value

A function `ff(s, plan = function(ft) ft)` with the standard interface
expected by `expOutcomesFF`: takes the situation vector `s` and a plan
function, returns `list(s = s, ft = ft)`.

## Examples

``` r
if (FALSE) { # \dontrun{
ff <- makeFF(function(s, ft) {
  ft["Do(Power_CM)_p", "Power_CM"]     <- 1 * s["s1"]
  ft["Push(ON)_p",     "Have(Coffee)"] <- prod(s[c("Power_CM", "Water_CM")])
  ft
})
} # }
```
