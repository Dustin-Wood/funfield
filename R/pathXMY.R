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
#'   When \code{NULL}, the unmoderated model is fit. By default
#'   (\code{Z.within = FALSE}), \code{Z} is treated as a between-person trait
#'   and z-standardized globally. Set \code{Z.within = TRUE} for a
#'   situation-level moderator that varies within persons (e.g., an
#'   experimentally varied condition); in that case \code{Z} is
#'   within-cluster deviated instead.
#' @param Z.within Logical (default \code{FALSE}). When \code{TRUE}, \code{Z}
#'   is treated as a within-cluster (situation-level) variable and is
#'   within-person deviated before fitting, exactly like the Level-1 predictors.
#'   Use this when the moderator varies across situations within a person
#'   (e.g., a randomly assigned scenario condition). When \code{FALSE}
#'   (default), \code{Z} is z-standardized globally (between-person trait
#'   interpretation).
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
#' @param joint Logical (default \code{TRUE}). When \code{length(M) > 1},
#'   additionally fit a single \emph{joint} multi-mediator model in which
#'   all mediators appear simultaneously in the Y equation. The joint fit
#'   contributes a residual \eqn{X \to Y} path (after controlling for the
#'   full mediator set), and parallel per-mediator coefficients with the
#'   suffix \code{_joint}. See \emph{Details}.
#'
#' @return A list with three elements:
#'   \describe{
#'     \item{tidy_loop}{A tidy data frame with one row per (mediator x
#'       parameter), assembled from \strong{separate single-mediator
#'       regressions} --- one X-M-Y fit per mediator, fit independently.
#'       The Y equation in each row's source model contains exactly one
#'       mediator. \emph{These coefficients are not from a simultaneous
#'       regression and should not be read as partial effects net of the
#'       other mediators.} Columns: \code{mediator}, \code{param},
#'       \code{est}, \code{se}, \code{z}, \code{pvalue}, \code{ci.lower},
#'       \code{ci.upper}. Parameter labels follow the XMY convention:
#'       \code{B1_MX}, \code{BZ_MX}, \code{B1_YX}, \code{BZ_YX},
#'       \code{B1_YM}, \code{BZ_YM}, plus the indirect effects
#'       \code{B1_MX * B1_YM} (unmoderated only),
#'       \code{BZ_MX * B1_YM} (\eqn{= \beta^Z_{MX} \cdot \beta^1_{YM}}),
#'       and \code{B1_MX * BZ_YM} (\eqn{= \beta^1_{MX} \cdot \beta^Z_{YM}}).
#'       When \code{M = NULL}, this slot holds the rows from the single
#'       Y-on-X direct regression (\code{mediator = NA}).}
#'     \item{tidy_joint}{A tidy data frame from the \strong{single
#'       simultaneous multi-mediator regression} in which all mediators
#'       appear together in the Y equation. \code{NULL} when
#'       \code{joint = FALSE} or \code{length(M) <= 1}. Per-mediator rows
#'       (\code{B1_MX_joint}, \code{BZ_MX_joint}, \code{B1_YM_joint},
#'       \code{BZ_YM_joint}) are indexed by \code{mediator}; global rows
#'       (\code{B1_YX_joint}, \code{BZ_YX_joint}) carry \code{mediator = NA}.
#'       The \code{B*_YM_joint} coefficients are partial slopes net of the
#'       other mediators and are \emph{not} comparable to the
#'       single-mediator \code{B*_YM} values in \code{tidy_loop}.}
#'     \item{fits}{A named list of the lavaan fit objects (one per mediator,
#'       named by the mediator variable name; \code{"_direct"} when
#'       \code{M = NULL}; \code{"_joint"} for the joint multi-mediator fit
#'       when present).}
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
#' \strong{Loop vs joint fits --- two different regressions, two different
#' tables.} With multiple mediators, \code{pathXMY()} produces two separate
#' tidy tables, deliberately kept apart because they answer different
#' questions and the coefficients are \emph{not} interchangeable:
#' \itemize{
#'   \item \code{$tidy_loop} comes from a "loop" pass: one independent
#'         X-M-Y model is fit \emph{per mediator}, each with only that
#'         single mediator in the Y equation. A row labeled \code{B1_YM}
#'         for mediator \eqn{m_k} is the slope of Y on \eqn{m_k}
#'         controlling for X --- nothing else. The loop pass is the
#'         inferential workhorse and supports the stable
#'         expectation-route summary \code{BZ_MX * B1_YM}.
#'   \item \code{$tidy_joint} comes from a single simultaneous regression
#'         where \emph{all} mediators appear together in the Y equation.
#'         A row labeled \code{B1_YM_joint} for mediator \eqn{m_k} is the
#'         partial slope of Y on \eqn{m_k} \emph{net of every other
#'         mediator in M}. These are not comparable to the loop
#'         \code{B1_YM} values and will routinely differ in magnitude
#'         (and sometimes sign) when mediators are correlated.
#' }
#' \code{B1_YX_joint} and \code{BZ_YX_joint} are the most useful joint
#' outputs: they index the residual direct \eqn{X \to Y} (and its
#' Z-moderation) after controlling for the entire mediator set, and serve
#' as a diagnostic of whether the measured mediators absorb the total
#' \code{BZ_YX} moderation. \emph{Per-mediator} joint coefficients
#' (especially \code{BZ_YM_joint}) are typically less stable than their
#' loop counterparts when mediators are numerous or correlated, because
#' the M-by-Z product terms are highly collinear (Wood, Adanu, & Harms,
#' 2025). For inference about a single mediator's role, prefer the loop
#' coefficient in \code{$tidy_loop}; treat the joint coefficients in
#' \code{$tidy_joint} as a system-level diagnostic.
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
#' res1$tidy_loop
#'
#' ## Moderated mediation across all eight outcome features
#' mediators <- c("Crash","Injured","Ticket","MoneyCost","OnTime",
#'                "IntQuality","FunDrive","Appropriate")
#' res2 <- pathXMY(dev, X = "Speed", Y = "L", M = mediators,
#'                 Z = "SRFastDriver")
#' ## Loop pass: per-mediator single-mediator regressions
#' subset(res2$tidy_loop, param %in% c("BZ_MX","BZ_MX * B1_YM"))
#' ## Joint pass: simultaneous multi-mediator regression
#' res2$tidy_joint
#' }
#'
#' @export
pathXMY <- function(data, X, Y, M = NULL, Z = NULL, Z.within = FALSE,
                    cluster = "p", controls = NULL,
                    se = c("cluster", "boot"),
                    nboot = 500, conf.level = 0.95,
                    check.deviation = TRUE,
                    suppress.warnings = TRUE,
                    joint = TRUE) {

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

  ## deviation check: X, M, Y always; Z only when between-person (not Z.within),
  ## because Z.within = TRUE means pathXMY deviates Z internally.
  if (check.deviation) {
    .check_deviation(data, c(X, M, Y), cluster)
  }

  ## prep Z: within-deviate (situation-level) or z-standardize (between-person trait)
  dat <- data
  if (!is.null(Z)) {
    if (Z.within) {
      if (!is.null(cluster)) {
        dat$.Z <- ave(dat[[Z]], dat[[cluster]],
                      FUN = function(x) x - mean(x, na.rm = TRUE))
      } else {
        dat$.Z <- dat[[Z]] - mean(dat[[Z]], na.rm = TRUE)
      }
    } else {
      dat$.Z <- as.numeric(scale(dat[[Z]]))
    }
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
    tidy_loop <- fit$tidy
    tidy_loop <- data.frame(mediator = NA_character_, tidy_loop,
                            row.names = NULL, stringsAsFactors = FALSE)
    fits <- list(`_direct` = fit$fit)
    return(structure(list(tidy_loop = tidy_loop,
                          tidy_joint = NULL,
                          fits = fits),
                     class = c("pathXMY", "list")))
  }

  ## With mediator(s) — loop over M (one X-M-Y fit per mediator, independently)
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
  tidy_loop <- do.call(rbind, tidy_list)
  rownames(tidy_loop) <- NULL

  ## Optional joint multi-mediator fit (length(M) > 1) — kept in its own
  ## tidy table so users cannot accidentally read joint partial slopes as
  ## if they were the single-mediator loop coefficients.
  tidy_joint <- NULL
  if (isTRUE(joint) && length(M) > 1L) {
    if (se == "boot") {
      warning("Bootstrap SEs are not yet implemented for the joint multi-mediator fit; ",
              "cluster-robust SEs are used for joint parameters.",
              call. = FALSE)
    }
    jfit <- .pathXMY_fit_joint(dat, X = X, Y = Y, M = M,
                                Z = Z, has_Z = has_Z,
                                ctrl_terms = ctrl_terms,
                                cluster = cluster,
                                conf.level = conf.level,
                                suppress.warnings = suppress.warnings)
    fits[["_joint"]] <- jfit$fit
    tidy_joint <- jfit$tidy[, colnames(tidy_loop), drop = FALSE]
    rownames(tidy_joint) <- NULL
  }

  structure(list(tidy_loop = tidy_loop,
                 tidy_joint = tidy_joint,
                 fits = fits),
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
        if (grepl("positive definite", w$message, fixed = TRUE)) {
          invokeRestart("muffleWarning")
        }
      })
  } else {
    fn()
  }
}

## Rename internal lavaan defined-parameter labels to display labels.
## (Lavaan cannot use "*" or spaces in defined-parameter names, so we keep
## the simple identifiers inside the model string and rename on the way out.)
.relabel_ind <- function(x) {
  m <- c(ind    = "B1_MX * B1_YM",
         indZ_X = "BZ_MX * B1_YM",
         indZ_Y = "B1_MX * BZ_YM")
  ifelse(x %in% names(m), m[x], x)
}

## Build the tidy table from a lavaan fit. Suppresses the cosmetic noise
## rows (the Z-on-outcome row that's structurally zero, the M:Z variance
## row that lavaan emits for product terms).
.pathXMY_tidy <- function(fit, conf.level) {
  pe <- lavaan::parameterestimates(fit, level = conf.level)
  keep_lab <- c("B1_MX","BZ_MX","B1_YX","BZ_YX","B1_YM","BZ_YM",
                "ind","indZ_X","indZ_Y")
  pe <- pe[pe$label %in% keep_lab, , drop = FALSE]
  ## Order parameters consistently, then apply display labels
  pe$param <- factor(pe$label, levels = keep_lab)
  pe <- pe[order(pe$param), , drop = FALSE]
  out <- data.frame(
    param    = .relabel_ind(as.character(pe$param)),
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
    param    = .relabel_ind(colnames(par_mat)),
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

## ---------- joint multi-mediator fit ----------

## Build the lavaan model string for the joint multi-mediator case.
## Interactions are passed as explicit product columns (X_Z, M_1_Z, ...)
## rather than the `X:.Z` syntax so the rhs in parameterestimates() is
## predictable across lavaan versions and across long equations.
.pathXMY_model_joint <- function(K, has_Z, ctrl_terms) {
  ctrl_M <- if (length(ctrl_terms) > 0)
    paste0(" + ", paste(ctrl_terms, collapse = " + ")) else ""
  ctrl_Y <- ctrl_M

  m_eqs <- vapply(seq_len(K), function(k) {
    if (has_Z) {
      sprintf("M_%d ~ 1 + B1_MX_J_%d*X + .Z + BZ_MX_J_%d*X_Z%s",
              k, k, k, ctrl_M)
    } else {
      sprintf("M_%d ~ 1 + B1_MX_J_%d*X%s", k, k, ctrl_M)
    }
  }, character(1))

  ym_terms <- if (has_Z) {
    paste(vapply(seq_len(K), function(k)
      sprintf("B1_YM_J_%d*M_%d + BZ_YM_J_%d*M_%d_Z", k, k, k, k),
      character(1)), collapse = " + ")
  } else {
    paste(vapply(seq_len(K), function(k)
      sprintf("B1_YM_J_%d*M_%d", k, k), character(1)),
      collapse = " + ")
  }

  y_eq <- if (has_Z) {
    sprintf("Y ~ 1 + B1_YX_J*X + .Z + BZ_YX_J*X_Z + %s%s",
            ym_terms, ctrl_Y)
  } else {
    sprintf("Y ~ 1 + B1_YX_J*X + %s%s", ym_terms, ctrl_Y)
  }

  paste(c(m_eqs, y_eq), collapse = "\n")
}

.pathXMY_fit_joint <- function(dat, X, Y, M, Z, has_Z, ctrl_terms,
                                cluster, conf.level, suppress.warnings) {
  d <- dat
  d$X <- d[[X]]
  d$Y <- d[[Y]]
  K <- length(M)
  Mn <- paste0("M_", seq_len(K))
  for (k in seq_len(K)) d[[Mn[k]]] <- d[[M[k]]]

  ## Explicit product columns for the interactions. d$.Z was set in the
  ## main pathXMY() preflight (within-deviated or z-standardized).
  if (has_Z) {
    d$X_Z <- d$X * d$.Z
    for (k in seq_len(K)) d[[paste0(Mn[k], "_Z")]] <- d[[Mn[k]]] * d$.Z
  }

  mod <- .pathXMY_model_joint(K, has_Z, ctrl_terms)
  fit <- .pathXMY_lavaan(d, mod, cluster = cluster,
                         suppress.warnings = suppress.warnings)
  tidy <- .pathXMY_tidy_joint(fit, M, has_Z = has_Z,
                              conf.level = conf.level)
  list(fit = fit, tidy = tidy)
}

## Tidy the joint-fit lavaan output by pulling parameters structurally
## from (lhs, op, rhs) triples in parameterestimates(). Product columns
## are explicit (X_Z, M_k_Z) so the rhs matches are exact strings —
## no reliance on lavaan's handling of `:` syntax for interactions.
.pathXMY_tidy_joint <- function(fit, M, has_Z, conf.level) {
  pe <- lavaan::parameterestimates(fit, level = conf.level)
  K  <- length(M)
  Mn <- paste0("M_", seq_len(K))

  one_row <- function(mediator, param, lhs, op, rhs) {
    r <- pe[pe$lhs == lhs & pe$op == op & pe$rhs == rhs, , drop = FALSE]
    if (nrow(r) == 0L) return(NULL)
    data.frame(
      mediator = mediator,
      param    = param,
      est      = r$est[1],
      se       = r$se[1],
      z        = r$z[1],
      pvalue   = r$pvalue[1],
      ci.lower = r$ci.lower[1],
      ci.upper = r$ci.upper[1],
      row.names = NULL, stringsAsFactors = FALSE
    )
  }

  rows <- list()
  for (k in seq_len(K)) {
    rows[[length(rows) + 1L]] <- one_row(M[k], "B1_MX_joint",
                                         Mn[k], "~", "X")
    if (has_Z)
      rows[[length(rows) + 1L]] <- one_row(M[k], "BZ_MX_joint",
                                           Mn[k], "~", "X_Z")
    rows[[length(rows) + 1L]] <- one_row(M[k], "B1_YM_joint",
                                         "Y", "~", Mn[k])
    if (has_Z)
      rows[[length(rows) + 1L]] <- one_row(M[k], "BZ_YM_joint",
                                           "Y", "~", paste0(Mn[k], "_Z"))
  }
  rows[[length(rows) + 1L]] <- one_row(NA_character_, "B1_YX_joint",
                                       "Y", "~", "X")
  if (has_Z)
    rows[[length(rows) + 1L]] <- one_row(NA_character_, "BZ_YX_joint",
                                         "Y", "~", "X_Z")

  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    warning("Joint multi-mediator fit produced no recognizable parameters; ",
            "skipping joint output. Inspect ",
            "lavaan::parameterestimates(<fit>$fits[['_joint']]) to debug.",
            call. = FALSE)
    return(data.frame(
      mediator = character(0), param = character(0),
      est = numeric(0), se = numeric(0), z = numeric(0),
      pvalue = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
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
