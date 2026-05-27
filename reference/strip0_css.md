# Emit the funfield decimal-house-style CSS once per HTML document

Outputs a small `<style>` block that turns on
`font-variant-numeric: tabular-nums` for any right-aligned table cell.
Combined with right-alignment (which
[`kable0`](https://dustin-wood.github.io/funfield/reference/kable0.md)
enables for originally-numeric columns and
[`group_kable`](https://dustin-wood.github.io/funfield/reference/group_kable.md)
enables via its own CSS), this makes the decimal points line up under
each other even when integer parts differ in width (e.g., `.664` versus
`-4.654`).

Intended to be called *once* per vignette, in a chunk with
`echo = FALSE, results = "asis"`. Re-emitting it is harmless but
wasteful.

## Usage

``` r
strip0_css()
```

## Value

Side effect: prints raw HTML. Invisibly returns the CSS string.

## Examples

``` r
if (FALSE) { # \dontrun{
# In a vignette setup chunk: ```{r echo=FALSE, results="asis"}
#                            strip0_css()
#                            ```
strip0_css()
} # }
```
