# Render a Wide Data Frame as an HTML Table with Grouped Column Headers

Renders a wide data frame — typically the output of
[`pathXMY_pairtable`](https://dustin-wood.github.io/funfield/reference/pathXMY_pairtable.md)
— as a self-contained HTML table in which the columns are gathered under
spanning group headers. Each group after the first is set off by a
divider rule, so two parameter blocks placed side by side (e.g. `f1_XM`
beside `f1_MY`) read as distinct, titled panels rather than one
undifferentiated block of numbers.

Intended for use inside R Markdown / knitr HTML documents (the package
vignettes use it to display
[`pathXMY_pairtable`](https://dustin-wood.github.io/funfield/reference/pathXMY_pairtable.md)
results). The returned object renders as raw HTML; styling is carried in
an inline `<style>` block, so no external CSS is required.

## Usage

``` r
group_kable(
  df,
  groups,
  col_labels = NULL,
  digits = 3,
  caption = NULL,
  strip0 = TRUE
)
```

## Arguments

- df:

  A data frame to render. Numeric columns are formatted to `digits`
  decimal places; other columns are coerced to character.

- groups:

  A named, integer-valued vector describing the column groups in order.
  Names are the spanning header labels (may contain HTML, e.g. an HTML
  entity such as `"&times;"`); values are the number of columns each
  group spans and must sum to `ncol(df)`. Use a one-column group named
  `" "` for a leading key column (e.g. the mediator name).

- col_labels:

  Character vector of length `ncol(df)` giving the second (per-column)
  header row. When `NULL` (default), the data frame's own column names
  are used.

- digits:

  Number of decimal places for numeric columns (default 3).

- caption:

  Optional table caption (character; may contain HTML).

- strip0:

  Logical; if `TRUE`, numeric columns are formatted via
  [`f0`](https://dustin-wood.github.io/funfield/reference/f0.md) so the
  leading zero is stripped from any decimal whose absolute value is less
  than 1 (`0.123 -> .123`). Default `TRUE` matches the funfield vignette
  house style.

## Value

An [`HTML`](https://rstudio.github.io/htmltools/reference/HTML.html)
object: a complete `<table>` preceded by an inline `<style>` block,
which knitr emits verbatim into HTML output.

## See also

[`pathXMY_pairtable`](https://dustin-wood.github.io/funfield/reference/pathXMY_pairtable.md),
[`pathXMY`](https://dustin-wood.github.io/funfield/reference/pathXMY.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data(speedingESJT)
mediators <- c("Crash","Injured","Ticket","MoneyCost","OnTime",
               "IntQuality","FunDrive","Appropriate")
res <- pathXMY(speedingESJT$PSI, X = "Speed", Y = "Likelihood",
               M = mediators)
tab <- pathXMY_pairtable(res, c("f1_XM", "f1_MY"))
group_kable(
  tab,
  groups = c(" " = 1,
             "Expectation paths (F1[X,M])" = 4,
             "Valuation paths (F1[M,Y])"   = 4),
  col_labels = c("Mediator", "est", "se", "z", "p",
                             "est", "se", "z", "p"))
} # }
```
