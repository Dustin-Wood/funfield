#' Path Analysis for X -> M -> Y Structures with Optional Moderation
#'
#' @description
#' Fits the path model
#' \deqn{M = \beta^1_{MX} X + \beta^Z_{MX}(X \times Z) + \beta^Z_M Z + e_M}
#' \deqn{Y = \beta^1_{YX} X + \beta^Z_{YX}(X \times Z) + \beta^1_{YM} M + \beta^Z_{YM}(M \times Z) + \beta^Z_Y Z + e_Y}
#' via \code{lavaan} with cluster-robust standard errors (clustering on
#' \code{cluster}, typically the person ID \code{"p"} in ESJT data).
#'
#' Omitting \code{M} collapses to the direct \eqn{X \to Y} model (optionally
#' moderated). Omitting \code{Z} collapses to the unmoderated version.
#' Supplying multiple variable names in \code{M} loops the model over each
#' mediator one at a time and returns a single stacked tidy table.
#'
#' @param data A data frame in PSI long format. Level-1 (within-person)
#'   columns should be within-person deviated; \code{Z} is treated as a
#'   between-person trait moderator and is z-standardized inside the
#'   function. See \emph{Details} for the deviation assumption.
#' @param X Name of the initiating action / predictor variable (length-1
#'   character).
#' @param Y Name of the outcome variable (length-1 character; typically
#'   \code{"L"} for likelihood).
#' @param M Optional. Name(s) of candidate mediator variables (character
#'   vector). When \code{NULL}, the direct \eqn{X \to Y} model is fit (no
#'   mediator). When length > 1, the X-M-Y model is fit once per mediator.
#' @param Z Optional. Name of the moderator variable (length-1 character).
#'   When \code{NULL}, the unmoderated model is fit. Will be z-standardized
#'   inside the function.
#' @param cluster Name of the clustering variable (default \code{"p"}).
#'   Set to \code{NULL} to fit without cluster-robust SEs (not recommended
#'   for ESJT data).
#' @param controls Optional character vector of control variables added to
#'   both equations.
#' @param se One of \code{"cluster"} (default; sandwich SEs from lavaan) or
#'   \code{"boot"} (cluster bootstrap of \code{nboot} replicates).
#' @param nboot Number of bootstrap replicates when \code{se = "boot"}.
#' @param conf.level Confidence level for CI columns (default 0.95).
#' @param check.deviation Logical. When \code{TRUE} (default), warn if the
#'   within-cluster mean of \code{X}, \code{M}, or \code{Y} suggests data
#'   has not been within-person deviated.
#' @param suppress.warnings Logical. When \code{TRUE} (default), suppress
#'   the cosmetic non-PD vcov warning that lavaan emits at machine precision.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{tidy}{A tidy data frame with one row per (mediator x parameter).
#'       Columns: \code{mediator}, \code{param}, \code{est}, \code{se},
#'       \code{z}, \code{pvalue}, \code{ci.lower}, \code{ci.upper}. Parameter
#'       labels follow the XMY convention: \code{B1_MX}, \code{BZ_MX},
#'       \code{B1_YX}, \code{BZ_YX}, \code{B1_YM}, \code{BZ_YM}, plus the
#'       indirect effects \code{ind} (unmoderated only), \code{indZ_X}
#'       (\eqn{= \beta^Z_{MX} \cdot \beta^1_{YM}}), and \code{indZ_Y}
#'       (\eqn{= \beta^1_{MX} \cdot \beta^Z_{YM}}).}
#'     \item{fits}{A named list of the lavaan fit objects (one per mediator,
#'       named by the mediator variable name; \code{"_direct"} when
#'       \code{M = NULL}).}
#'   }
#'
#' @details
#' \strong{Deviation assumption.} \code{cluster = "p"} in lavaan provides
#' cluster-robust sandwich standard errors on a single-level SEM; it is
#' \emph{not} a multilevel model. To get correct within-person path
#' coefficients, Level-1 variables (\code{X}, \code{M}, \code{Y}) should
#' be within-person deviated before being passed in. \code{Z} is treated
#' as a between-person trait (no within-person variance expected) and is
#' z-standardized inside the function.
#'
#' \strong{Small cluster counts.} The cluster-robust z-tests assume the
#' number of clusters \eqn{G} is large. With \eqn{G < 50}, results may be
#' anti-conservative; consider \code{se = "boot"}.
#'
#' \strong{Naming convention.} Parameters use the X-M-Y suffix convention
#' from Wood, Harms, & Cho (2023): \code{B1_*} are main-effect coefficients,
#' \code{BZ_*} are Z-moderated coefficients; \code{B*_MX} parameters
#' regress M on X, \code{B*_YM} regress Y on M, \code{B*_YX} are the
#' (direct) Y on X coefficients.
#'
#' @examples
#' \dontrun{
#' data(speedingESJT)
#' ## Within-person deviate the Level 1 columns
#' L1 <- c("Speed","Crash","Injured","Ticket","MoneyCost","OnTime",
#'         "IntQuality","FunDrive","Appropriate","L")
#' dev <- speedingESJT
#' for (v in L1) dev[[v]] <- ave(dev[[v]], dev$p,
#'                               FUN = function(x) x - mean(x))
#'
#' ## Unmoderated mediation, one mediator
#' res1 <- pathXMY(dev, X = "Speed", Y = "L", M = "Crash")
#' res1$tidy
#'
#' ## Moderated mediation across all eight outcome features
#' mediators <- c("Crash","Injured","Ticket","MoneyCost","OnTime",
#'                "IntQuality","FunDrive","Appropriate")
#' res2 <- pathXMY(dev, X = "Speed", Y = "L", M = mediators,
#'                 Z = "SRFastDriver")
#' subset(res2$tidy, param %in% c("BZ_MX","indZ_X"))
#' }
#'
#' @export
pathXMY <- function(data, X, Y, M = NULL, Z = NULL,
                    cluster = "p", controls = NULL,
                    se = c("cluster", "boot"),
                    nboot = 500, conf.level = 0.95,
                    check.deviation = TRUE,
                    suppress.warnings = TRUE) {

  se <- match.arg(se)
  stopifnot(is.data.frame(data),
            is.character(X), length(X) == 1,
            is.character(Y), length(Y) == 1,
            is.null(M) || is.character(M),
            is.null(Z) || (is.character(Z) && length(Z) == 1),
            is.null(cluster) || (is.character(cluster) && length(cluster) == 1),
            is.numeric(conf.level), length(conf.level) == 1,
            conf.level > 0, conf.level < 1)

  needed <- c(X, Y, M, Z, cluster, controls)
  missing_cols <- setdiff(needed, colnames(data))
  if (length(missing_cols) > 0) {
    stop("Columns not found in data: ",
         paste(missing_cols, collapse = ", "))
  }

  if (!is.null(cluster)) {
    G <- length(unique(data[[cluster]]))
    if (G < 50) {
      warning(sprintf(
        "Only %d clusters in '%s'. Cluster-robust z-tests assume G is large;\n  consider se = \"boot\" for small-G inference.", G, cluster))
    }
  }

  if (check.deviation) {
    .check_deviation(data, c(X, M, Y), cluster)
  }

  ## prep Z (between-person trait): standardize once, attach as "_Z"
  dat <- data
  if (!is.null(Z)) {
    dat$.Z <- as.numeric(scale(dat[[Z]]))
  }
  ## controls: standardize numerics so coefs are interpretable & comparable
  ctrl_terms <- character(0)
  if (length(controls) > 0) {
    for (cc in controls) {
      if (is.numeric(dat[[cc]])) {
        dat[[paste0(".c_", cc)]] <- as.numeric(scale(dat[[cc]]))
      } else {
        dat[[paste0(".c_", cc)]] <- dat[[cc]]
      }
      ctrl_terms <- c(ctrl_terms, paste0(".c_", cc))
    }
  }

  ## What model are we fitting?
  has_M <- !is.null(M) && length(M) > 0
  has_Z <- !is.null(Z)

  if (!has_M) {
    fit <- .pathXMY_fit_direct(dat, X = X, Y = Y, Z = Z, has_Z = has_Z,
                               ctrl_terms = ctrl_terms,
                               cluster = cluster,
                               se = se, nboot = nboot,
                               conf.level = conf.level,
                               suppress.warnings = suppress.warnings)
    tidy <- fit$tidy
    tidy <- data.frame(mediator = NA_character_, tidy,
                       row.names = NULL, stringsAsFactors = FALSE)
    fits <- list(`_direct` = fit$fit)
    return(structure(list(tidy = tidy, fits = fits),
                     class = c("pathXMY", "list")))
  }

  ## With mediator(s) — loop over M
  tidy_list <- vector("list", length(M))
  fits      <- vector("list", length(M))
  names(fits) <- M
  for (k in seq_along(M)) {
    mvar <- M[k]
    fit_k <- .pathXMY_fit_med(dat, X = X, Y = Y, M = mvar,
                              Z = Z, has_Z = has_Z,
                              ctrl_terms = ctrl_terms,
                              cluster = cluster,
                              se = se, nboot = nboot,
                              conf.level = conf.level,
                              suppress.warnings = suppress.warnings)
    fits[[mvar]]      <- fit_k$fit
    t_k <- fit_k$tidy
    t_k <- data.frame(mediator = mvar, t_k,
                      row.names = NULL, stringsAsFactors = FALSE)
    tidy_list[[k]] <- t_k
  }
  tidy <- do.call(rbind, tidy_list)
  rownames(tidy) <- NULL

  structure(list(tidy = tidy, fits = fits),
            class = c("pathXMY", "list"))
}

## ---------- internal helpers ----------

## Build the lavaan model string for the X -> M -> Y case
.pathXMY_model_med <- function(has_Z, ctrl_terms) {
  ctrl_M <- if (length(ctrl_terms) > 0)
    paste0(" + ", paste(ctrl_terms, collapse = " + ")) else ""
  ctrl_Y <- ctrl_M
  if (has_Z) {
    m_eq <- sprintf(
      "M ~ 1 + B1_MX*X + .Z + BZ_MX*X:.Z%s", ctrl_M)
    y_eq <- sprintf(
      "Y ~ 1 + B1_YX*X + .Z + BZ_YX*X:.Z + B1_YM*M + BZ_YM*M:.Z%s",
      ctrl_Y)
    defs <- c(
      "ind   := B1_MX*B1_YM",
      "indZ_X := BZ_MX*B1_YM",
      "indZ_Y := B1_MX*BZ_YM"
    )
  } else {
    m_eq <- sprintf("M ~ 1 + B1_MX*X%s", ctrl_M)
    y_eq <- sprintf("Y ~ 1 + B1_YX*X + B1_YM*M%s", ctrl_Y)
    defs <- "ind := B1_MX*B1_YM"
  }
  paste(c(m_eq, y_eq, defs), collapse = "\n")
}

.pathXMY_model_direct <- function(has_Z, ctrl_terms) {
  ctrl_Y <- if (length(ctrl_terms) > 0)
    paste0(" + ", paste(ctrl_terms, collapse = " + ")) else ""
  if (has_Z) {
    sprintf("Y ~ 1 + B1_YX*X + .Z + BZ_YX*X:.Z%s", ctrl_Y)
  } else {
    sprintf("Y ~ 1 + B1_YX*X%s", ctrl_Y)
  }
}

.pathXMY_fit_med <- function(dat, X, Y, M, Z, has_Z, ctrl_terms,
                             cluster, se, nboot, conf.level,
                             suppress.warnings) {
  d <- dat
  d$X <- d[[X]]
  d$Y <- d[[Y]]
  d$M <- d[[M]]
  mod <- .pathXMY_model_med(has_Z, ctrl_terms)
  fit <- .pathXMY_lavaan(d, mod, cluster = cluster,
                         suppress.warnings = suppress.warnings)
  tidy <- .pathXMY_tidy(fit, conf.level = conf.level)
  if (se == "boot") {
    boot_se <- .pathXMY_boot(mod, d, cluster = cluster, R = nboot,
                             conf.level = conf.level,
                             suppress.warnings = suppress.warnings)
    tidy <- .merge_boot(tidy, boot_se)
  }
  list(fit = fit, tidy = tidy)
}

.pathXMY_fit_direct <- function(dat, X, Y, Z, has_Z, ctrl_terms,
                                cluster, se, nboot, conf.level,
                                suppress.warnings) {
  d <- dat
  d$X <- d[[X]]
  d$Y <- d[[Y]]
  mod <- .pathXMY_model_direct(has_Z, ctrl_terms)
  fit <- .pathXMY_lavaan(d, mod, cluster = cluster,
                         suppress.warnings = suppress.warnings)
  tidy <- .pathXMY_tidy(fit, conf.level = conf.level)
  if (se == "boot") {
    boot_se <- .pathXMY_boot(mod, d, cluster = cluster, R = nboot,
                             conf.level = conf.level,
                             suppress.warnings = suppress.warnings)
    tidy <- .merge_boot(tidy, boot_se)
  }
  list(fit = fit, tidy = tidy)
}

.pathXMY_lavaan <- function(d, mod, cluster, suppress.warnings) {
  fn <- function() {
    if (is.null(cluster)) {
      lavaan::sem(mod, d)
    } else {
      lavaan::sem(mod, d, cluster = cluster)
    }
  }
  if (suppress.warnings) {
    withCallingHandlers(fn(),
      warning = function(w) {
        if (grepl("does not appear to be positive definite", w$message,
                  fixed = TRUE)) {
          invokeRestart("muffleWarning")
        }
      })
  } else {
    fn()
  }
}

## Build the tidy table from a lavaan fit. Suppresses the cosmetic noise
## rows (the Z-on-outcome row that's structurally zero, the M:Z variance
## row that lavaan emits for product terms).
.pathXMY_tidy <- function(fit, conf.level) {
  pe <- lavaan::parameterestimates(fit, level = conf.level)
  keep_lab <- c("B1_MX","BZ_MX","B1_YX","BZ_YX","B1_YM","BZ_YM",
                "ind","indZ_X","indZ_Y")
  pe <- pe[pe$label %in% keep_lab, , drop = FALSE]
  ## Order parameters consistently
  pe$param <- factor(pe$label, levels = keep_lab)
  pe <- pe[order(pe$param), , drop = FALSE]
  out <- data.frame(
    param    = as.character(pe$param),
    est      = pe$est,
    se       = pe$se,
    z        = pe$z,
    pvalue   = pe$pvalue,
    ci.lower = pe$ci.lower,
    ci.upper = pe$ci.upper,
    row.names = NULL, stringsAsFactors = FALSE
  )
  out
}

## Cluster bootstrap: resample clusters with replacement and refit
.pathXMY_boot <- function(mod, d, cluster, R, conf.level,
                          suppress.warnings) {
  if (is.null(cluster)) {
    stop("Cluster bootstrap requires a cluster variable.", call. = FALSE)
  }
  cl_ids <- unique(d[[cluster]])
  par_mat <- NULL
  for (b in seq_len(R)) {
    samp <- sample(cl_ids, length(cl_ids), replace = TRUE)
    db <- do.call(rbind, lapply(seq_along(samp), function(j) {
      sub <- d[d[[cluster]] == samp[j], , drop = FALSE]
      sub[[cluster]] <- paste0("b", j)
      sub
    }))
    fb <- tryCatch(
      .pathXMY_lavaan(db, mod, cluster = cluster,
                      suppress.warnings = suppress.warnings),
      error = function(e) NULL)
    if (is.null(fb)) next
    pb <- lavaan::parameterestimates(fb)
    pb <- pb[pb$label %in% c("B1_MX","BZ_MX","B1_YX","BZ_YX",
                             "B1_YM","BZ_YM","ind","indZ_X","indZ_Y"),
             , drop = FALSE]
    if (is.null(par_mat)) {
      par_mat <- matrix(NA_real_, R, length(pb$label),
                        dimnames = list(NULL, pb$label))
    }
    par_mat[b, pb$label] <- pb$est
  }
  alpha <- (1 - conf.level) / 2
  data.frame(
    param    = colnames(par_mat),
    se_boot  = apply(par_mat, 2, sd,        na.rm = TRUE),
    ci_lo_boot = apply(par_mat, 2, quantile, alpha,         na.rm = TRUE),
    ci_hi_boot = apply(par_mat, 2, quantile, 1 - alpha,     na.rm = TRUE),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

.merge_boot <- function(tidy, boot_se) {
  m <- merge(tidy, boot_se, by = "param", all.x = TRUE, sort = FALSE)
  m$se       <- m$se_boot
  m$ci.lower <- m$ci_lo_boot
  m$ci.upper <- m$ci_hi_boot
  m$z        <- m$est / m$se
  m$pvalue   <- 2 * stats::pnorm(-abs(m$z))
  m$se_boot <- m$ci_lo_boot <- m$ci_hi_boot <- NULL
  m
}

## Warn if Level-1 variables don't look within-person deviated.
.check_deviation <- function(data, vars, cluster) {
  if (is.null(cluster) || !(cluster %in% colnames(data))) return(invisible())
  bad <- character(0)
  for (v in vars) {
    if (!(v %in% colnames(data))) next
    if (!is.numeric(data[[v]])) next
    pmean <- stats::ave(data[[v]], data[[cluster]], FUN = function(x)
      mean(x, na.rm = TRUE))
    ## "looks deviated" if cluster means are essentially zero
    if (max(abs(pmean), na.rm = TRUE) > 0.05 * stats::sd(data[[v]], na.rm = TRUE)) {
      bad <- c(bad, v)
    }
  }
  if (length(bad) > 0) {
    warning(sprintf(
      "Variables %s do not appear within-cluster deviated.\n  pathXMY() fits a single-level SEM with cluster-robust SEs;\n  Level-1 variables should typically be within-person deviated first.\n  Pass check.deviation = FALSE to silence this warning.",
      paste(sprintf("'%s'", bad), collapse = ", ")), call. = FALSE)
  }
  invisible()
}
