# Side-by-Side Table of Two (or More) pathXMY Parameters

Reshapes a
[`pathXMY`](https://dustin-wood.github.io/funfield/reference/pathXMY.md)
tidy table from long form (one row per mediator x parameter) into a wide
form that places two or more named parameters next to each other, keyed
by mediator. This is the natural layout for reading an expectation path
beside its companion valuation path — `f1_XM` next to `f1_MY` for the
normative model, or the two moderation arms `fZ_XM` next to `fZ_MY` — in
a single table, without the `mediator` column repeating.

## Usage

``` r
pathXMY_pairtable(
  x,
  params,
  from = c("loop", "joint"),
  cols = c("est", "se", "z", "pvalue"),
  mediators = NULL
)
```

## Arguments

- x:

  A `pathXMY` object (the list returned by
  [`pathXMY()`](https://dustin-wood.github.io/funfield/reference/pathXMY.md)),
  or a tidy data frame carrying `mediator`, `param`, and the requested
  `cols` (e.g. `x$tidy_loop`).

- params:

  Character vector (length \>= 2) of parameter labels to place side by
  side, in display order. Labels follow the XMY convention used by
  [`pathXMY()`](https://dustin-wood.github.io/funfield/reference/pathXMY.md):
  e.g. `"f1_XM"`, `"f1_MY"`, `"fZ_XM"`, `"fZ_MY"`.

- from:

  One of `"loop"` (default) or `"joint"` — which tidy table to draw from
  when `x` is a `pathXMY` object. Ignored when `x` is already a data
  frame.

- cols:

  Character vector of statistic columns carried for each parameter
  (default `c("est", "se", "z", "pvalue")`).

- mediators:

  Optional character vector giving an explicit row order. When `NULL`
  (default), mediators appear in the order they occur for the first
  parameter.

## Value

A data frame with one row per mediator: a leading `mediator` column
followed by one block of `cols` per parameter. Block columns are named
`<param>.<col>` (runs of non-alphanumeric characters in `param`
collapsed to `"_"`), so `c("f1_XM", "f1_MY")` yields `f1_XM.est`,
`f1_XM.se`, `...`, `f1_MY.est`, `f1_MY.se`, `...`. Rows with a missing
(`NA`) mediator — the global `f*_XY` direct paths — are dropped, since
the table is keyed by mediator.

## See also

[`pathXMY`](https://dustin-wood.github.io/funfield/reference/pathXMY.md),
[`pathXMY_decompose`](https://dustin-wood.github.io/funfield/reference/pathXMY_decompose.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data(speedingESJT)
mediators <- c("Crash","Injured","Ticket","MoneyCost","OnTime",
               "IntQuality","FunDrive","Appropriate")
res <- pathXMY(speedingESJT$PSI, X = "Speed", Y = "Likelihood",
               M = mediators)
## expectation paths beside valuation paths
pathXMY_pairtable(res, c("f1_XM", "f1_MY"))

## the two moderation arms beside each other (model fit with a Z)
modp <- pathXMY(speedingESJT$PSI, X = "Speed", Y = "Likelihood",
                M = mediators, Z = "someTrait", Z.within = FALSE)
pathXMY_pairtable(modp, c("fZ_XM", "fZ_MY"))
} # }
```
