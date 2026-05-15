#' Cluster-Robust Covariance and Correlation Matrix for ESJT Data
#'
#' @description
#' Estimates pairwise covariances (and correlations) among a set of variables
#' in ESJT long-format data, using a saturated lavaan model with
#' cluster-robust standard errors. Returns a structured object with
#' covariance, correlation, and p-value matrices, plus a tidy long-form table.
#'
#' @param data A data frame in PSI long format. Level-1 (within-person)
#'   variables should be within-person deviated before being passed in.
#'   See \emph{Details}.
#' @param vars Character vector of variable names to include (length >= 2).
#' @param cluster Name of the clustering variable (default \code{"p"}).
#'   Set to \code{NULL} to fit without cluster-robust SEs (not recommended
#'   for ESJT data).
#' @param check.deviation Logical. When \code{TRUE} (default), warn if the
#'   within-cluster mean of any variable in \code{vars} suggests data has not
#'   been within-person deviated.
#' @param suppress.warnings Logical. When \code{TRUE} (default), suppress
#'   the cosmetic non-positive-definite vcov warning that lavaan emits at
#'   machine precision.
#'
#' @return An object of class \code{"esjtCov"}, a list with:
#'   \describe{
#'     \item{cov}{Covariance matrix (k \eqn{\times} k).}
#'     \item{cor}{Correlation matrix (k \eqn{\times} k); \code{cov2cor(cov)}.}
#'     \item{p}{Matrix of two-sided p-values for each covariance/correlation
#'       (k \eqn{\times} k). Diagonal is \code{NA}.}
#'     \item{tidy}{Long-form data frame with one row per unique variable pair
#'       (lower triangle): columns \code{var1}, \code{var2}, \code{cov},
#'       \code{r}, \code{p}.}
#'     \item{n}{Number of clusters (persons).}
#'     \item{fit}{The lavaan fit object (for advanced diagnostics).}
#'   }
#'
#' @details
#' \strong{Deviation assumption.} \code{cluster = "p"} in lavaan provides
#' cluster-robust sandwich standard errors on a single-level model; it is
#' \emph{not} a multilevel model. To get correct within-person covariances,
#' Level-1 variables should be within-person deviated before being passed in.
#'
#' \strong{Model.} A saturated lavaan covariance model is fit internally:
#' all pairwise \code{~~} statements are generated for \code{vars}, and
#' lavaan estimates each covariance with cluster-robust sandwich standard
#' errors when \code{cluster} is non-\code{NULL}. p-values are two-sided
#' Wald tests on the covariance (equivalently, on the correlation).
#'
#' \strong{Small cluster counts.} The cluster-robust z-tests assume the
#' number of clusters \eqn{G} is large. With \eqn{G < 50}, results may be
#' anti-conservative.
#'
#' @examples
#' \dontrun{
#' data(speedingESJT)
#' L1 <- c("Speed","Crash","Injured","Ticket","MoneyCost","OnTime",
#'         "IntQuality","FunDrive","Appropriate","L")
#' dev <- speedingESJT
#' for (v in L1) dev[[v]] <- ave(dev[[v]], dev$p,
#'                               FUN = function(x) x - mean(x))
#'
#' ec <- esjtCov(dev, vars = L1)
#' ec                   # correlation matrix with significance stars
#' ec$tidy              # long-form table, one row per pair
#' cov2cor(ec$cov)      # raw correlation matrix
#' }
#'
#' @seealso \code{\link{covnps}}, \code{\link{pathXMY}}
#' @export
esjtCov <- function(data, vars, cluster = "p",
                    check.deviation = TRUE,
                    suppress.warnings = TRUE) {

  stopifnot(is.data.frame(data),
            is.character(vars), length(vars) >= 2,
            is.null(cluster) || (is.character(cluster) && length(cluster) == 1))

  needed <- c(vars, cluster)
  missing_cols <- setdiff(needed, colnames(data))
  if (length(missing_cols) > 0)
    stop("Columns not found in data: ", paste(missing_cols, collapse = ", "))

  if (!is.null(cluster)) {
    G <- length(unique(data[[cluster]]))
    if (G < 50)
      warning(sprintf(
        "Only %d clusters in '%s'. Cluster-robust z-tests assume G is large;\n  consider interpreting p-values cautiously with small G.",
        G, cluster))
  }

  if (check.deviation)
    .check_deviation(data, vars, cluster)

  ## Fit saturated covariance model
  mod <- .esjtCov_model(vars)
  fit <- .esjtCov_lavaan(data, mod, cluster, suppress.warnings)

  ## Extract and symmetrize (lavaan outputs each pair once)
  raw     <- covnps(fit, vars)
  cov_mat <- .esjtCov_symmetrize(raw$cov)
  p_mat   <- .esjtCov_symmetrize(raw$p)
  diag(p_mat) <- NA_real_
  cor_mat <- cov2cor(cov_mat)

  ## Tidy long-form (lower triangle only)
  idx  <- which(lower.tri(cor_mat), arr.ind = TRUE)
  tidy <- data.frame(
    var1 = rownames(cor_mat)[idx[, 1]],
    var2 = colnames(cor_mat)[idx[, 2]],
    cov  = cov_mat[idx],
    r    = cor_mat[idx],
    p    = p_mat[idx],
    row.names = NULL, stringsAsFactors = FALSE
  )

  n_clusters <- if (!is.null(cluster)) length(unique(data[[cluster]])) else nrow(data)

  structure(
    list(cov = cov_mat, cor = cor_mat, p = p_mat,
         tidy = tidy, n = n_clusters, fit = fit),
    class = c("esjtCov", "list")
  )
}

## ---------- print method ----------

#' @export
print.esjtCov <- function(x, digits = 3, ...) {
  k <- ncol(x$cor)
  r <- x$cor
  p <- x$p

  ## Lower-triangle character matrix; upper blank; diagonal —
  out <- matrix("", k, k, dimnames = dimnames(r))
  for (i in seq_len(k)) {
    for (j in seq_len(k)) {
      if (i == j) {
        out[i, j] <- "—"          # em dash
      } else if (i > j) {
        rv <- formatC(r[i, j], digits = digits, format = "f", flag = "+")
        pv <- p[i, j]
        st <- if (is.na(pv))  "   " else
              if (pv < .001)  "***" else
              if (pv < .01)   "** " else
              if (pv < .05)   "*  " else
                              "   "
        out[i, j] <- paste0(rv, st)
      }
    }
  }

  cat(sprintf("esjtCov  (%d variables, n = %d clusters)\n\n", k, x$n))
  print(noquote(out), right = TRUE)
  cat("\nSignif. codes: *** p<.001  ** p<.01  * p<.05\n")
  invisible(x)
}

## ---------- internal helpers ----------

## Build a saturated lavaan covariance model for 'vars'.
.esjtCov_model <- function(vars) {
  variances <- paste(vars, "~~", vars)
  pairs     <- utils::combn(vars, 2,
                            FUN = function(x) paste(x[1], "~~", x[2]))
  paste(c(variances, pairs), collapse = "\n")
}

## Fit the lavaan model with optional cluster-robust SEs.
.esjtCov_lavaan <- function(data, mod, cluster, suppress.warnings) {
  fn <- function() {
    if (is.null(cluster))
      lavaan::sem(mod, data)
    else
      lavaan::sem(mod, data, cluster = cluster)
  }
  if (suppress.warnings) {
    withCallingHandlers(fn(), warning = function(w) {
      if (grepl("positive definite", w$message, fixed = TRUE))
        invokeRestart("muffleWarning")
    })
  } else {
    fn()
  }
}

## Symmetrize a matrix by copying lower triangle to upper where upper is NA.
.esjtCov_symmetrize <- function(m) {
  for (i in seq_len(nrow(m)))
    for (j in seq_len(ncol(m)))
      if (is.na(m[i, j]) && !is.na(m[j, i]))
        m[i, j] <- m[j, i]
  m
}
