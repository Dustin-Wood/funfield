# Cluster-Robust Covariance and Correlation Matrix for ESJT Data

Estimates pairwise covariances (and correlations) among a set of
variables in ESJT long-format data, using a saturated lavaan model with
cluster-robust standard errors. Returns a structured object with
covariance, correlation, and p-value matrices, plus a tidy long-form
table.

## Usage

``` r
esjtCov(
  data,
  vars,
  cluster = "p",
  check.deviation = TRUE,
  suppress.warnings = TRUE
)
```

## Arguments

- data:

  A data frame in PSI long format. Level-1 (within-person) variables
  should be within-person deviated before being passed in. See
  *Details*.

- vars:

  Character vector of variable names to include (length \>= 2).

- cluster:

  Name of the clustering variable (default `"p"`). Set to `NULL` to fit
  without cluster-robust SEs (not recommended for ESJT data).

- check.deviation:

  Logical. When `TRUE` (default), warn if the within-cluster mean of any
  variable in `vars` suggests data has not been within-person deviated.

- suppress.warnings:

  Logical. When `TRUE` (default), suppress the cosmetic
  non-positive-definite vcov warning that lavaan emits at machine
  precision.

## Value

An object of class `"esjtCov"`, a list with:

- cov:

  Covariance matrix (k \\\times\\ k).

- cor:

  Correlation matrix (k \\\times\\ k); `cov2cor(cov)`.

- p:

  Matrix of two-sided p-values for each covariance/correlation (k
  \\\times\\ k). Diagonal is `NA`.

- tidy:

  Long-form data frame with one row per unique variable pair (lower
  triangle): columns `var1`, `var2`, `cov`, `r`, `p`.

- n:

  Number of clusters (persons).

- fit:

  The lavaan fit object (for advanced diagnostics).

## Details

**Deviation assumption.** `cluster = "p"` in lavaan provides
cluster-robust sandwich standard errors on a single-level model; it is
*not* a multilevel model. To get correct within-person covariances,
Level-1 variables should be within-person deviated before being passed
in.

**Model.** A saturated lavaan covariance model is fit internally: all
pairwise `~~` statements are generated for `vars`, and lavaan estimates
each covariance with cluster-robust sandwich standard errors when
`cluster` is non-`NULL`. p-values are two-sided Wald tests on the
covariance (equivalently, on the correlation).

**Small cluster counts.** The cluster-robust z-tests assume the number
of clusters \\G\\ is large. With \\G \< 50\\, results may be
anti-conservative.

## See also

[`covnps`](https://dustin-wood.github.io/funfield/reference/covnps.md),
[`pathXMY`](https://dustin-wood.github.io/funfield/reference/pathXMY.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data(speedingESJT)
L1 <- c("Speed","Crash","Injured","Ticket","MoneyCost","OnTime",
        "IntQuality","FunDrive","Appropriate","L")
dev <- speedingESJT
for (v in L1) dev[[v]] <- ave(dev[[v]], dev$p,
                              FUN = function(x) x - mean(x))

ec <- esjtCov(dev, vars = L1)
ec                   # correlation matrix with significance stars
ec$tidy              # long-form table, one row per pair
cov2cor(ec$cov)      # raw correlation matrix
} # }
```
