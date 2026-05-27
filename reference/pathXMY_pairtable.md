# Side-by-Side Table of Two (or More) pathXMY Parameters

Reshapes a
[`pathXMY`](https://dustin-wood.github.io/funfield/reference/pathXMY.md)
tidy table from long form (one row per mediator x parameter) into a wide
form that places two or more named parameters next to each other, keyed
by mediator. This is the natural layout for reading an expectation path
beside its companion valuation path — `B1_MX` next to `B1_YM` for the
normative model, or the two moderation arms `BZ_MX` next to `BZ_YM` — in
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
  e.g. `"B1_MX"`, `"B1_YM"`, `"BZ_MX"`, `"BZ_YM"`.

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
collapsed to `"_"`), so `c("B1_MX", "B1_YM")` yields `B1_MX.est`,
`B1_MX.se`, `...`, `B1_YM.est`, `B1_YM.se`, `...`. Rows with a missing
(`NA`) mediator — the global `B*_YX` direct paths — are dropped, since
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
pathXMY_pairtable(res, c("B1_MX", "B1_YM"))

## the two moderation arms beside each other (model fit with a Z)
modp <- pathXMY(speedingESJT$PSI, X = "Speed", Y = "Likelihood",
                M = mediators, Z = "someTrait", Z.within = FALSE)
pathXMY_pairtable(modp, c("BZ_MX", "BZ_YM"))
} # }
```
