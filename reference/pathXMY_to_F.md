# Render pathXMY F-schema keys as matrix-cell display labels

[`pathXMY()`](https://dustin-wood.github.io/funfield/reference/pathXMY.md)
and its companions
([`pathXMY_decompose`](https://dustin-wood.github.io/funfield/reference/pathXMY_decompose.md),
[`pathXMY_pairtable`](https://dustin-wood.github.io/funfield/reference/pathXMY_pairtable.md))
label parameters with the F-schema source-target keys `f1_XM` / `fZ_XY`
— prefix `f`, a scope digit (`1` for the baseline, `Z` for a moderator),
and a two-letter *source-target* suffix. These are lavaan-safe
identifiers, ideal as keys but terse to read. `pathXMY_to_F()` rewrites
them into the package's matrix-cell *display* form, `FZ[X,Y]` — the
`(X, Y)` cell (source `X`, target `Y`) of the `Z`-overlay matrix `F_Z`.
See
[`vignette("notation")`](https://dustin-wood.github.io/funfield/articles/notation.md)
for the schema.

This is a *display* transform only. The terse `fZ_XY` keys are what
index the tidy tables and what helper arguments expect; matrix-bracket
forms like `FZ[X,Y]` are not valid lavaan identifiers and are intended
for prose, captions, and printed table cells. The intended idiom is to
do all subsetting and arithmetic on the keys, then wrap the *final*
object handed to a table:


    kable0(pathXMY_to_F(subset(norm$tidy_loop, param == "f1_XM * f1_MY")))

## Usage

``` r
pathXMY_to_F(x)
```

## Arguments

- x:

  One of: a character vector of parameter keys; a data frame carrying a
  `param` and/or `term` column (relabelled in place); or a
  [`pathXMY`](https://dustin-wood.github.io/funfield/reference/pathXMY.md)
  object (its `tidy_loop` and `tidy_joint` tables are relabelled). Lists
  carrying `components` (a
  [`pathXMY_decompose`](https://dustin-wood.github.io/funfield/reference/pathXMY_decompose.md)
  return) have their `components$term` relabelled.

## Value

The same kind of object as `x`, with coefficient keys rewritten into the
matrix-cell display form. For character input, a character vector of the
same length.

## Details

A single coefficient `f<scope>_<src><tgt>` (with optional `_joint` /
`_total` model suffix) becomes `F<scope>[<src>,<tgt>]`. The model suffix
`_total` adds a `*` superscript (the overall / no-mediator model);
`_joint` maps to the same cell (the joint multi-mediator model is
identified by the table it sits in). Products (`"A * B"`) are mapped
factor-by-factor and rejoined with a middot. A trailing parenthetical
annotation (e.g. `" (direct)"`) is preserved, and any token that does
not parse as a coefficient (e.g. `"sum (1+2+3)"`) passes through
unchanged.

|                  |                    |                                |
|------------------|--------------------|--------------------------------|
| **Key**          | **Display**        | **Reads as**                   |
| `f1_XM`          | `F1[X,M]`          | baseline X -\> M force         |
| `fZ_XM`          | `FZ[X,M]`          | Z-moderation of X -\> M        |
| `f1_MY`          | `F1[M,Y]`          | baseline M -\> Y force         |
| `fZ_MY`          | `FZ[M,Y]`          | Z-moderation of M -\> Y        |
| `f1_XY`          | `F1[X,Y]`          | direct X -\> Y                 |
| `fZ_XY`          | `FZ[X,Y]`          | Z-moderation of direct X -\> Y |
| `f1_XY_total`    | `F1*[X,Y]`         | total (no-mediator) X -\> Y    |
| `fZ_XM * f1_MY`  | `FZ[X,M]·F1[M,Y]`  | expectation route              |
| `fZ_XY (direct)` | `FZ[X,Y] (direct)` | residual direct moderation     |

## See also

[`pathXMY`](https://dustin-wood.github.io/funfield/reference/pathXMY.md),
[`pathXMY_decompose`](https://dustin-wood.github.io/funfield/reference/pathXMY_decompose.md),
[`pathXMY_pairtable`](https://dustin-wood.github.io/funfield/reference/pathXMY_pairtable.md);
[`vignette("notation")`](https://dustin-wood.github.io/funfield/articles/notation.md).

## Examples

``` r
pathXMY_to_F(c("f1_XM", "fZ_MY", "fZ_XM * f1_MY", "fZ_XY (direct)"))
#> [1] "F1[X,M]"          "FZ[M,Y]"          "FZ[X,M]·F1[M,Y]"  "FZ[X,Y] (direct)"
#> "F1[X,M]" "FZ[M,Y]" "FZ[X,M]·F1[M,Y]" "FZ[X,Y] (direct)"
```
