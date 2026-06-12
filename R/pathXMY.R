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
#' @param data A data frame in PSI long format (\code{p}, \code{s}, \code{i}
#'   as the first columns). By default the Level-1 columns \code{X},
#'   \code{M}, \code{Y} are within-(person, situation) deviated
#'   automatically (see \code{deviate}); \code{Z} is handled separately.
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
#' @param situation Name of the situation/scenario column (default
#'   \code{"s"}), paired with \code{cluster} to define the (person,
#'   situation) cells used by automatic deviation.
#' @param deviate One of \code{"auto"} (default) or \code{"none"}. With
#'   \code{"auto"}, \code{X}, \code{M}, and \code{Y} are within-(person,
#'   situation) deviated before fitting --- each value centered on the mean
#'   of the actions rated in the same \code{(cluster, situation)} cell ---
#'   provided a \code{situation} column is present and every cell holds at
#'   least two rows. When some cells hold a single action the data is left
#'   untouched (deviate it yourself first, e.g. with \code{\link{devPSI}}).
#'   With \code{"none"}, no deviation is applied and pre-deviated data is
#'   assumed.
#' @param controls Optional character vector of control variables added to
#'   both equations.
#' @param se One of \code{"cluster"} (default; sandwich SEs from lavaan) or
#'   \code{"boot"} (cluster bootstrap of \code{nboot} replicates).
#' @param nboot Number of bootstrap replicates when \code{se = "boot"}.
#' @param conf.level Confidence level for CI columns (default 0.95).
#' @param check.deviation Logical. When \code{TRUE} (default), warn if
#'   \code{X}, \code{M}, or \code{Y} still do not look within-person
#'   deviated after the \code{deviate} step --- relevant mainly when
#'   \code{deviate = "none"} and the data was not pre-deviated.
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
#'       \code{ci.upper}. Parameter labels use the F-schema source-target
#'       convention: \code{f1_XM}, \code{fZ_XM}, \code{f1_XY},
#'       \code{fZ_XY}, \code{f1_MY}, \code{fZ_MY}, plus the indirect
#'       effects \code{f1_XM * f1_MY} (unmoderated only),
#'       \code{fZ_XM * f1_MY} (\eqn{= F_Z[X,M] \cdot F_1[M,Y]}),
#'       and \code{f1_XM * fZ_MY} (\eqn{= F_1[X,M] \cdot F_Z[M,Y]}).
#'       The display form \code{FZ[X,Y]} is produced by
#'       \code{\link{pathXMY_to_F}}. When \code{M = NULL}, this slot holds
#'       the rows from the single Y-on-X direct regression
#'       (\code{mediator = NA}).}
#'     \item{tidy_joint}{A tidy data frame from the \strong{single
#'       simultaneous multi-mediator regression} in which all mediators
#'       appear together in the Y equation. \code{NULL} when
#'       \code{joint = FALSE} or \code{length(M) <= 1}. Per-mediator rows
#'       (\code{f1_XM_joint}, \code{fZ_XM_joint}, \code{f1_MY_joint},
#'       \code{fZ_MY_joint}) are indexed by \code{mediator}; global rows
#'       (\code{f1_XY_joint}, \code{fZ_XY_joint}) carry \code{mediator = NA}.
#'       The \code{f*_MY_joint} coefficients are partial slopes net of the
#'       other mediators and are \emph{not} comparable to the
#'       single-mediator \code{f*_MY} values in \code{tidy_loop}.}
#'     \item{fits}{A named list of the lavaan fit objects (one per mediator,
#'       named by the mediator variable name; \code{"_direct"} when
#'       \code{M = NULL}; \code{"_joint"} for the joint multi-mediator fit
#'       when present). Every fit is estimated through the shared
#'       \code{\link{pathF}} engine, so the lavaan objects internally use
#'       the engine's token variable names (\code{v1, v2, ...}) and
#'       source-target labels (\code{f1_1_2, ...}); the tidy tables
#'       translate these to the canonical \code{f1_XM} vocabulary.}
#'   }
#'
#' @details
#' \strong{Deviation.} \code{cluster = "p"} in lavaan provides
#' cluster-robust sandwich standard errors on a single-level SEM; it is
#' \emph{not} a multilevel model. Correct within-person path coefficients
#' therefore require the Level-1 variables (\code{X}, \code{M}, \code{Y})
#' to be deviated within each \code{(cluster, situation)} cell. By default
#' (\code{deviate = "auto"}) \code{pathXMY()} does this internally; pass
#' \code{deviate = "none"} for data that is already deviated. When more than
#' one action is rated per (person, situation), within-cell deviation
#' isolates the within-situation action contrast the field model is about;
#' with a single situation per person it reduces to ordinary within-person
#' deviation. \code{Z} is handled separately: a between-person trait
#' moderator (\code{Z.within = FALSE}) is z-standardized, a situation-level
#' moderator (\code{Z.within = TRUE}) is within-person deviated.
#'
#' \strong{Small cluster counts.} The cluster-robust z-tests assume the
#' number of clusters \eqn{G} is large. With \eqn{G < 50}, results may be
#' anti-conservative; consider \code{se = "boot"}.
#'
#' \strong{Naming convention.} Parameters use the F-schema source-target
#' convention (see \code{vignette("notation")}): \code{f1_*} are baseline
#' coefficients, \code{fZ_*} are Z-moderated coefficients; \code{f*_XM}
#' parameters regress M on X (the \eqn{X \to M} force), \code{f*_MY}
#' regress Y on M (\eqn{M \to Y}), \code{f*_XY} are the (direct)
#' \eqn{X \to Y} coefficients. The two trailing letters read source then
#' target, matching the matrix-cell display form \code{F[src,tgt]}.
#'
#' \strong{Loop vs joint fits --- two different regressions, two different
#' tables.} With multiple mediators, \code{pathXMY()} produces two separate
#' tidy tables, deliberately kept apart because they answer different
#' questions and the coefficients are \emph{not} interchangeable:
#' \itemize{
#'   \item \code{$tidy_loop} comes from a "loop" pass: one independent
#'         X-M-Y model is fit \emph{per mediator}, each with only that
#'         single mediator in the Y equation. A row labeled \code{f1_MY}
#'         for mediator \eqn{m_k} is the slope of Y on \eqn{m_k}
#'         controlling for X --- nothing else. The loop pass is the
#'         inferential workhorse and supports the stable
#'         expectation-route summary \code{fZ_XM * f1_MY}.
#'   \item \code{$tidy_joint} comes from a single simultaneous regression
#'         where \emph{all} mediators appear together in the Y equation.
#'         A row labeled \code{f1_MY_joint} for mediator \eqn{m_k} is the
#'         partial slope of Y on \eqn{m_k} \emph{net of every other
#'         mediator in M}. These are not comparable to the loop
#'         \code{f1_MY} values and will routinely differ in magnitude
#'         (and sometimes sign) when mediators are correlated.
#' }
#' \code{f1_XY_joint} and \code{fZ_XY_joint} are the most useful joint
#' outputs: they index the residual direct \eqn{X \to Y} (and its
#' Z-moderation) after controlling for the entire mediator set, and serve
#' as a diagnostic of whether the measured mediators absorb the total
#' \code{fZ_XY} moderation. \emph{Per-mediator} joint coefficients
#' (especially \code{fZ_MY_joint}) are typically less stable than their
#' loop counterparts when mediators are numerous or correlated, because
#' the M-by-Z product terms are highly collinear (Wood, Adanu, & Harms,
#' 2025). For inference about a single mediator's role, prefer the loop
#' coefficient in \code{$tidy_loop}; treat the joint coefficients in
#' \code{$tidy_joint} as a system-level diagnostic.
#'
#' @examples
#' \dontrun{
#' data(speedingESJT)
#' ## X, M, Y are within-(person, situation) deviated automatically.
#' PSI <- speedingESJT$PSI
#'
#' ## Unmoderated mediation, one mediator
#' res1 <- pathXMY(PSI, X = "Speed", Y = "Likelihood", M = "Crash")
#' res1$tidy_loop
#'
#' ## Mediation across all eight outcome features
#' mediators <- c("Crash","Injured","Ticket","MoneyCost","OnTime",
#'                "IntQuality","FunDrive","Appropriate")
#' res2 <- pathXMY(PSI, X = "Speed", Y = "Likelihood", M = mediators)
#' ## Loop pass: per-mediator single-mediator regressions
#' res2$tidy_loop
#' ## Joint pass: simultaneous multi-mediator regression
#' res2$tidy_joint
#' }
#'
#' @export
pathXMY <- function(data, X, Y, M = NULL, Z = NULL, Z.within = FALSE,
                    cluster = "p", situation = "s", controls = NULL,
                    deviate = c("auto", "none"),
                    se = c("cluster", "boot"),
                    nboot = 500, conf.level = 0.95,
                    check.deviation = TRUE,
                    suppress.warnings = TRUE,
                    joint = TRUE) {

  se <- match.arg(se)
  deviate <- match.arg(deviate)
  stopifnot(is.data.frame(data),
            is.character(X), length(X) == 1,
            is.character(Y), length(Y) == 1,
            is.null(M) || is.character(M),
            is.null(Z) || (is.character(Z) && length(Z) == 1),
            is.null(cluster) || (is.character(cluster) && length(cluster) == 1),
            is.null(situation) || (is.character(situation) && length(situation) == 1),
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

  ## within-(person, situation) deviate the Level-1 columns X, M, Y
  dat <- .pathXMY_deviate(data, vars = unique(c(X, M, Y)),
                          cluster = cluster, situation = situation,
                          deviate = deviate)

  ## deviation check on the (possibly auto-deviated) data: warn if X, M, or
  ## Y still do not look within-person deviated. Z is excluded -- it is
  ## handled separately just below.
  if (check.deviation) {
    .check_deviation(dat, c(X, M, Y), cluster)
  }

  ## prep Z: within-deviate (situation-level) or z-standardize (between-person trait)
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
    fit <- .pathXMY_fit_direct(dat, X = X, Y = Y, has_Z = has_Z,
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
                              has_Z = has_Z,
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
                                has_Z = has_Z,
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

## All estimation routes through the shared pathF engine
## (.pathF_engine in pathF.R): pathXMY's triangle, direct, and joint
## fits are pathF specs whose engine params (f1_<src>_<tgt>) are
## relabeled to the canonical X/M/Y vocabulary below. pathXMY is the
## X -> M -> Y special case of pathF at the estimation level; what it
## adds is the per-mediator loop protocol, the canonical labels, and
## the bootstrap.

## Canonical parameter vocabulary, in display order
.pathXMY_canon_levels <- c(
  "f1_XM", "fZ_XM", "f1_XY", "fZ_XY", "f1_MY", "fZ_MY",
  "f1_XM * f1_MY", "fZ_XM * f1_MY", "f1_XM * fZ_MY"
)

## Engine-param -> canonical-param map for one X -> m -> Y triangle
.pathXMY_canon_map <- function(X, m, Y) {
  e1 <- function(a, b) sprintf("f1_%s_%s", a, b)
  eZ <- function(a, b) sprintf("fZ_%s_%s", a, b)
  stats::setNames(
    .pathXMY_canon_levels,
    c(e1(X, m), eZ(X, m), e1(X, Y), eZ(X, Y), e1(m, Y), eZ(m, Y),
      paste(e1(X, m), "*", e1(m, Y)),
      paste(eZ(X, m), "*", e1(m, Y)),
      paste(e1(X, m), "*", eZ(m, Y))))
}

## Relabel an engine tidy to canonical params, drop unmapped rows,
## order canonically. Keeps the lavaan `label` column for the boot map.
.pathXMY_relabel <- function(eng_tidy, map, levels) {
  td <- eng_tidy
  td$param <- unname(map[td$param])
  td <- td[!is.na(td$param), , drop = FALSE]
  td <- td[order(factor(td$param, levels = levels)), , drop = FALSE]
  rownames(td) <- NULL
  td
}

.pathXMY_tidy_cols <- c("param", "est", "se", "z", "pvalue",
                        "ci.lower", "ci.upper")

.pathXMY_fit_med <- function(dat, X, Y, M, has_Z, ctrl_terms,
                             cluster, se, nboot, conf.level,
                             suppress.warnings) {
  spec <- .pathF_spec_make(c(X, M, Y),
                           src = c(X, X, M), tgt = c(M, Y, Y),
                           X = X, Y = Y)
  eng <- .pathF_engine(dat, spec, has_Z = has_Z, ctrl_terms = ctrl_terms,
                       cluster = cluster, conf.level = conf.level,
                       suppress.warnings = suppress.warnings)
  td <- .pathXMY_relabel(eng$tidy, .pathXMY_canon_map(X, M, Y),
                         .pathXMY_canon_levels)
  tidy <- td[, .pathXMY_tidy_cols]
  if (se == "boot") {
    boot_se <- .pathXMY_boot(eng$model, eng$data, cluster = cluster,
                             R = nboot, conf.level = conf.level,
                             suppress.warnings = suppress.warnings,
                             label_map = stats::setNames(td$param, td$label))
    tidy <- .merge_boot(tidy, boot_se)
  }
  list(fit = eng$fit, tidy = tidy)
}

.pathXMY_fit_direct <- function(dat, X, Y, has_Z, ctrl_terms,
                                cluster, se, nboot, conf.level,
                                suppress.warnings) {
  spec <- .pathF_spec_make(c(X, Y), src = X, tgt = Y, X = X, Y = Y)
  eng <- .pathF_engine(dat, spec, has_Z = has_Z, ctrl_terms = ctrl_terms,
                       cluster = cluster, conf.level = conf.level,
                       suppress.warnings = suppress.warnings)
  map <- stats::setNames(c("f1_XY", "fZ_XY"),
                         c(sprintf("f1_%s_%s", X, Y),
                           sprintf("fZ_%s_%s", X, Y)))
  td <- .pathXMY_relabel(eng$tidy, map, c("f1_XY", "fZ_XY"))
  tidy <- td[, .pathXMY_tidy_cols]
  if (se == "boot") {
    boot_se <- .pathXMY_boot(eng$model, eng$data, cluster = cluster,
                             R = nboot, conf.level = conf.level,
                             suppress.warnings = suppress.warnings,
                             label_map = stats::setNames(td$param, td$label))
    tidy <- .merge_boot(tidy, boot_se)
  }
  list(fit = eng$fit, tidy = tidy)
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

## Cluster bootstrap: resample clusters with replacement and refit the
## engine model string on the engine-prepared data. `label_map` maps the
## fit's internal lavaan labels (f1_1_2, ind_1, ...) to the canonical
## display params.
.pathXMY_boot <- function(mod, d, cluster, R, conf.level,
                          suppress.warnings, label_map) {
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
    pb <- pb[pb$label %in% names(label_map), , drop = FALSE]
    if (is.null(par_mat)) {
      par_mat <- matrix(NA_real_, R, length(pb$label),
                        dimnames = list(NULL, pb$label))
    }
    par_mat[b, pb$label] <- pb$est
  }
  alpha <- (1 - conf.level) / 2
  data.frame(
    param    = unname(label_map[colnames(par_mat)]),
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

## The joint fit is the pathF DAG X -> {M_1..M_K} -> Y (plus the direct
## X -> Y edge) fit through the shared engine; route enumeration is
## skipped (max_paths = 0) since the joint table never carried indirect
## rows. Engine params are relabeled to *_joint and reordered to the
## canonical per-mediator interleaving.
.pathXMY_fit_joint <- function(dat, X, Y, M, has_Z, ctrl_terms,
                                cluster, conf.level, suppress.warnings) {
  K <- length(M)
  spec <- .pathF_spec_make(c(X, M, Y),
                           src = c(rep(X, K), X, M),
                           tgt = c(M, Y, rep(Y, K)),
                           X = X, Y = Y)
  eng <- .pathF_engine(dat, spec, has_Z = has_Z, ctrl_terms = ctrl_terms,
                       cluster = cluster, conf.level = conf.level,
                       suppress.warnings = suppress.warnings,
                       max_paths = 0L)
  pe <- eng$tidy

  grab <- function(mediator, canon, pf_param) {
    r <- pe[pe$param == pf_param, , drop = FALSE]
    if (nrow(r) == 0L) return(NULL)
    data.frame(
      mediator = mediator, param = canon,
      est = r$est[1], se = r$se[1], z = r$z[1], pvalue = r$pvalue[1],
      ci.lower = r$ci.lower[1], ci.upper = r$ci.upper[1],
      row.names = NULL, stringsAsFactors = FALSE
    )
  }

  rows <- list()
  for (k in seq_len(K)) {
    rows[[length(rows) + 1L]] <-
      grab(M[k], "f1_XM_joint", sprintf("f1_%s_%s", X, M[k]))
    if (has_Z)
      rows[[length(rows) + 1L]] <-
        grab(M[k], "fZ_XM_joint", sprintf("fZ_%s_%s", X, M[k]))
    rows[[length(rows) + 1L]] <-
      grab(M[k], "f1_MY_joint", sprintf("f1_%s_%s", M[k], Y))
    if (has_Z)
      rows[[length(rows) + 1L]] <-
        grab(M[k], "fZ_MY_joint", sprintf("fZ_%s_%s", M[k], Y))
  }
  rows[[length(rows) + 1L]] <-
    grab(NA_character_, "f1_XY_joint", sprintf("f1_%s_%s", X, Y))
  if (has_Z)
    rows[[length(rows) + 1L]] <-
      grab(NA_character_, "fZ_XY_joint", sprintf("fZ_%s_%s", X, Y))

  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    warning("Joint multi-mediator fit produced no recognizable parameters; ",
            "skipping joint output. Inspect ",
            "lavaan::parameterestimates(<fit>$fits[['_joint']]) to debug.",
            call. = FALSE)
    return(list(fit = eng$fit, tidy = data.frame(
      mediator = character(0), param = character(0),
      est = numeric(0), se = numeric(0), z = numeric(0),
      pvalue = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0),
      stringsAsFactors = FALSE
    )))
  }
  list(fit = eng$fit, tidy = do.call(rbind, rows))
}

## Within-(person, situation) deviate Level-1 columns (X, M, Y).
## deviate = "auto": deviate iff a situation column is present and every
##   (cluster, situation) cell holds >= 2 rows -- so no single-action cell
##   is collapsed to a constant 0. Otherwise the data is returned unchanged
##   (and .check_deviation() then warns if it is not already deviated).
## deviate = "none": return the data untouched.
.pathXMY_deviate <- function(data, vars, cluster, situation, deviate) {
  if (identical(deviate, "none")) return(data)
  if (is.null(cluster)   || !(cluster   %in% colnames(data))) return(data)
  if (is.null(situation) || !(situation %in% colnames(data))) return(data)
  g <- interaction(data[[cluster]], data[[situation]], drop = TRUE)
  if (any(table(g) < 2L)) return(data)   # singleton cell: leave to the user
  for (v in vars) {
    if (is.numeric(data[[v]])) {
      data[[v]] <- stats::ave(data[[v]], g,
                              FUN = function(x) x - mean(x, na.rm = TRUE))
    }
  }
  data
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
