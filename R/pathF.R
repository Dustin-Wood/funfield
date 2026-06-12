#' General Path / SEM Field Models with Optional Moderation
#'
#' @description
#' Fits an arbitrary recursive path (SEM) model to PSI long-format data with
#' cluster-robust standard errors, generalizing \code{\link{pathXMY}} from the
#' single \eqn{X \to M \to Y} triangle to cascades
#' \eqn{X \to M_1 \to M_2 \to \dots \to Y} and to arbitrary directed acyclic
#' path structures.
#'
#' The structural part can be supplied two ways:
#' \itemize{
#'   \item \code{order}: a character vector of variables in presumed
#'         \strong{causal order}. This auto-expands to the \emph{saturated
#'         lower-triangular cascade} in which every causally upstream variable
#'         predicts every downstream variable
#'         (\code{M1 ~ X}; \code{M2 ~ X + M1}; \code{M3 ~ X + M1 + M2};
#'         \code{Y ~ X + M1 + M2 + M3}).
#'   \item \code{paths}: a character vector of \pkg{lavaan}-style regression
#'         formulas (e.g. \code{c("Y ~ M2", "M2 ~ X + M1", "M1 ~ X")}) for an
#'         arbitrary DAG.
#' }
#' Supply exactly one of \code{order} or \code{paths}.
#'
#' When a moderator \code{Z} is given, every structural edge \eqn{b\,A} is
#' expanded to \eqn{f_1 A + f_Z (A \times Z)} and a \code{Z} main effect is
#' added to each endogenous equation, exactly as in \code{pathXMY()}. Indirect
#' effects are enumerated along every directed \eqn{X \to Y} path of two or
#' more edges, each as a product of edge coefficients, plus (under moderation)
#' the first-order route-moderation terms.
#'
#' @param data A data frame in PSI long format (\code{p}, \code{s}, \code{i}
#'   as the first columns). The cascade/path variables are within-(person,
#'   situation) deviated automatically (see \code{deviate}); \code{Z} is
#'   handled separately.
#' @param order Optional character vector of variable names in presumed causal
#'   order. Expands to the saturated lower-triangular cascade. Supply this
#'   \emph{or} \code{paths}, not both.
#' @param paths Optional character vector of lavaan-style regression formulas
#'   (\code{"lhs ~ rhs1 + rhs2 + ..."}) describing an arbitrary DAG. Each
#'   right-hand-side term \code{r} contributes a directed edge \code{r -> lhs}.
#' @param X,Y Optional names of the focal initiating action and outcome used to
#'   enumerate indirect-effect routes. Default to the first and last element of
#'   \code{order}; with \code{paths}, default to the unique source / sink node
#'   when one exists (otherwise indirect enumeration is skipped unless both are
#'   supplied).
#' @param Z Optional moderator (length-1 character). When \code{NULL}, the
#'   unmoderated model is fit.
#' @param Z.within Logical (default \code{FALSE}). When \code{TRUE}, \code{Z} is
#'   within-cluster deviated (situation-level moderator); when \code{FALSE} it
#'   is z-standardized globally (between-person trait).
#' @param cluster Name of the clustering variable (default \code{"p"}); set to
#'   \code{NULL} to fit without cluster-robust SEs (not recommended).
#' @param situation Name of the situation column (default \code{"s"}), paired
#'   with \code{cluster} for automatic within-cell deviation.
#' @param controls Optional character vector of control variables added to
#'   every endogenous equation.
#' @param deviate One of \code{"auto"} (default) or \code{"none"}; see
#'   \code{\link{pathXMY}}.
#' @param conf.level Confidence level for CI columns (default 0.95).
#' @param check.deviation Logical (default \code{TRUE}); warn if the cascade
#'   variables do not look within-person deviated.
#' @param suppress.warnings Logical (default \code{TRUE}); muffle the cosmetic
#'   non-positive-definite vcov warning lavaan emits at machine precision.
#' @param max_paths Integer (default 256). Cap on the number of directed
#'   \eqn{X \to Y} paths enumerated for indirect effects. A saturated
#'   \eqn{K}-node cascade has \eqn{2^{K-2}} such paths; above the cap, indirect
#'   definitions are skipped with a warning (the structural edges are still
#'   returned).
#'
#' @return A list of class \code{"pathF"} with:
#'   \describe{
#'     \item{tidy}{A tidy data frame, one row per structural edge and per
#'       indirect-effect term. Columns: \code{param}, \code{src}, \code{tgt},
#'       \code{label} (the internal lavaan parameter label, for debugging
#'       against \code{$fit}), \code{est}, \code{se}, \code{z},
#'       \code{pvalue}, \code{ci.lower}, \code{ci.upper}. Edge
#'       \code{param}s use the F-schema source-target form
#'       \code{f1_<src>_<tgt>} / \code{fZ_<src>_<tgt>}; indirect rows use
#'       a \code{ * }-joined product string (e.g.
#'       \code{f1_X_M * f1_M_Y}, \code{fZ_X_M * f1_M_Y}).}
#'     \item{fit}{The fitted \code{lavaan} object.}
#'     \item{spec}{The parsed specification (\code{nodes}, \code{edges},
#'       \code{X}, \code{Y}).}
#'     \item{model}{The generated lavaan model string (useful for debugging).}
#'     \item{data}{The prepared data frame the model was fit to (deviated,
#'       with the \code{.Z} moderator, token alias columns \code{v1..vK},
#'       and interaction product columns). Used by resampling wrappers.}
#'   }
#'
#' @details
#' A saturated recursive path model is just-identified and algebraically
#' equivalent to a sequence of OLS regressions, so lavaan fits it without
#' iteration. Under moderation the interaction terms across many edges can be
#' highly collinear, inflating standard errors; inspect \code{$model} and the
#' SEs when the cascade is long.
#'
#' The 3-variable saturated cascade \code{order = c(X, M, Y)} reproduces the
#' single-mediator \code{pathXMY()} fit exactly: \code{f1_X_M} \eqn{=}
#' \code{f1_XM}, \code{f1_X_Y} \eqn{=} \code{f1_XY}, \code{f1_M_Y} \eqn{=}
#' \code{f1_MY}, and the indirect/route-moderation products match.
#'
#' @seealso \code{\link{pathXMY}} for the X-M-Y special case (loop vs joint
#'   mediator handling, bootstrap SEs).
#'
#' @examples
#' \dontrun{
#' data(speedingESJT)
#' PSI <- speedingESJT$PSI
#'
#' ## Saturated causal cascade Speed -> OnTime -> Appropriate -> Likelihood
#' fitc <- pathF(PSI, order = c("Speed", "OnTime", "Appropriate", "Likelihood"))
#' fitc$tidy
#'
#' ## Same, moderated by a between-person trait
#' sr <- speedingESJT$traits[, c("p", "SR_30")]
#' names(sr)[2] <- "SRFast"
#' dd <- merge(PSI, sr, by = "p")
#' fitm <- pathF(dd, order = c("Speed", "OnTime", "Appropriate", "Likelihood"),
#'               Z = "SRFast")
#' fitm$tidy
#'
#' ## Arbitrary DAG via formulas
#' pathF(PSI, paths = c("Likelihood ~ Appropriate + OnTime",
#'                      "Appropriate ~ Speed",
#'                      "OnTime ~ Speed"),
#'       X = "Speed", Y = "Likelihood")
#' }
#'
#' @export
pathF <- function(data, order = NULL, paths = NULL, X = NULL, Y = NULL,
                  Z = NULL, Z.within = FALSE,
                  cluster = "p", situation = "s", controls = NULL,
                  deviate = c("auto", "none"),
                  conf.level = 0.95,
                  check.deviation = TRUE, suppress.warnings = TRUE,
                  max_paths = 256L) {

  deviate <- match.arg(deviate)
  stopifnot(is.data.frame(data),
            is.null(Z) || (is.character(Z) && length(Z) == 1),
            is.null(cluster) || (is.character(cluster) && length(cluster) == 1),
            is.null(situation) || (is.character(situation) && length(situation) == 1),
            is.numeric(conf.level), length(conf.level) == 1,
            conf.level > 0, conf.level < 1)

  spec <- .pathF_spec(order, paths, X, Y)

  needed <- c(spec$nodes, Z, cluster, controls)
  missing_cols <- setdiff(needed, colnames(data))
  if (length(missing_cols) > 0) {
    stop("Columns not found in data: ", paste(missing_cols, collapse = ", "))
  }

  if (!is.null(cluster)) {
    G <- length(unique(data[[cluster]]))
    if (G < 50) {
      warning(sprintf(
        "Only %d clusters in '%s'. Cluster-robust z-tests assume G is large.",
        G, cluster))
    }
  }

  ## within-(person, situation) deviate the cascade variables
  dat <- .pathXMY_deviate(data, vars = spec$nodes,
                          cluster = cluster, situation = situation,
                          deviate = deviate)
  if (check.deviation) .check_deviation(dat, spec$nodes, cluster)

  ## prep Z: within-deviate (situation-level) or z-standardize (trait)
  has_Z <- !is.null(Z)
  if (has_Z) {
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

  ## controls: standardize numerics
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

  eng <- .pathF_engine(dat, spec, has_Z = has_Z, ctrl_terms = ctrl_terms,
                       cluster = cluster, conf.level = conf.level,
                       suppress.warnings = suppress.warnings,
                       max_paths = max_paths)

  structure(list(tidy = eng$tidy, fit = eng$fit, spec = spec,
                 model = eng$model, data = eng$data),
            class = c("pathF", "list"))
}

## ---------- internal helpers ----------

## The shared estimation core: alias the spec's variables to token columns
## (v1, v2, ...) so the model string is independent of possibly-messy real
## variable names, build interaction product columns, generate the lavaan
## model string, fit, and tidy. Assumes `dat` is fully prepared (deviation
## done, `.Z` and `.c_*` columns in place) -- both pathF() and pathXMY()
## route every fit through here so there is a single source of truth for
## model construction and extraction.
.pathF_engine <- function(dat, spec, has_Z, ctrl_terms, cluster,
                          conf.level, suppress.warnings,
                          max_paths = 256L) {
  d <- dat
  K <- length(spec$nodes)
  for (k in seq_len(K)) d[[paste0("v", k)]] <- d[[spec$nodes[k]]]
  if (has_Z) {
    for (k in seq_len(K)) d[[paste0("v", k, "_Z")]] <- d[[paste0("v", k)]] * d$.Z
  }
  mb  <- .pathF_build_model(spec, has_Z, ctrl_terms, max_paths)
  fit <- .pathXMY_lavaan(d, mb$model, cluster = cluster,
                         suppress.warnings = suppress.warnings)
  tidy <- .pathF_tidy(fit, mb, spec, conf.level)
  list(tidy = tidy, fit = fit, model = mb$model, data = d, mb = mb)
}

## Build a pathF spec directly from a node list and edge vectors, without
## formula parsing -- robust to arbitrary column names. Used by pathXMY()
## to construct its triangle / direct / joint specs.
.pathF_spec_make <- function(nodes, src, tgt, X, Y) {
  list(nodes = nodes,
       edges = data.frame(src = src, tgt = tgt, stringsAsFactors = FALSE),
       X = X, Y = Y,
       tok = stats::setNames(seq_along(nodes), nodes))
}

## Parse `order` / `paths` into a node set, an edge list (src -> tgt), and the
## focal X / Y used for indirect-effect enumeration.
.pathF_spec <- function(order, paths, X, Y) {
  if (is.null(order) == is.null(paths)) {
    stop("Supply exactly one of `order` or `paths`.", call. = FALSE)
  }

  if (!is.null(order)) {
    stopifnot(is.character(order), length(order) >= 2)
    if (anyDuplicated(order)) stop("`order` contains duplicate names.", call. = FALSE)
    nodes <- order
    ## saturated lower-triangular cascade: every i precedes j gives edge i -> j
    edges <- do.call(rbind, lapply(seq_along(order)[-1], function(j) {
      data.frame(src = order[seq_len(j - 1)], tgt = order[j],
                 stringsAsFactors = FALSE)
    }))
    if (is.null(X)) X <- order[1]
    if (is.null(Y)) Y <- order[length(order)]
  } else {
    stopifnot(is.character(paths), length(paths) >= 1)
    ed <- list()
    for (f in paths) {
      parts <- strsplit(f, "~", fixed = TRUE)[[1]]
      if (length(parts) != 2) {
        stop("Each `paths` element must be a single 'lhs ~ rhs' formula: ", f,
             call. = FALSE)
      }
      lhs <- trimws(parts[1])
      rhs <- trimws(strsplit(parts[2], "+", fixed = TRUE)[[1]])
      rhs <- rhs[nzchar(rhs)]
      for (r in rhs) {
        ed[[length(ed) + 1L]] <- data.frame(src = r, tgt = lhs,
                                             stringsAsFactors = FALSE)
      }
    }
    edges <- unique(do.call(rbind, ed))
    nodes <- unique(c(edges$src, edges$tgt))
    if (is.null(X)) {
      src_only <- setdiff(edges$src, edges$tgt)
      X <- if (length(src_only) == 1L) src_only else NA_character_
    }
    if (is.null(Y)) {
      sink_only <- setdiff(edges$tgt, edges$src)
      Y <- if (length(sink_only) == 1L) sink_only else NA_character_
    }
  }

  edges <- unique(edges)
  list(nodes = nodes, edges = edges, X = X, Y = Y,
       tok = stats::setNames(seq_along(nodes), nodes))
}

## Build the lavaan model string (equations + indirect-effect definitions),
## returning a label map for tidy extraction.
.pathF_build_model <- function(spec, has_Z, ctrl_terms, max_paths) {
  tok   <- spec$tok
  nodes <- spec$nodes
  edges <- spec$edges
  edges$si <- as.integer(tok[edges$src])
  edges$ti <- as.integer(tok[edges$tgt])

  endo <- unique(edges$tgt)
  ctrl <- if (length(ctrl_terms))
    paste0(" + ", paste(ctrl_terms, collapse = " + ")) else ""

  eqs <- character(0)
  edge_lab <- list()
  for (t in endo) {
    ti <- as.integer(tok[t])
    preds <- edges[edges$tgt == t, , drop = FALSE]
    terms <- character(0)
    for (r in seq_len(nrow(preds))) {
      si <- preds$si[r]
      f1lab <- sprintf("f1_%d_%d", si, ti)
      terms <- c(terms, sprintf("%s*v%d", f1lab, si))
      edge_lab[[length(edge_lab) + 1L]] <-
        data.frame(label = f1lab, si = si, ti = ti, coef = "f1",
                   stringsAsFactors = FALSE)
      if (has_Z) {
        fZlab <- sprintf("fZ_%d_%d", si, ti)
        terms <- c(terms, sprintf("%s*v%d_Z", fZlab, si))
        edge_lab[[length(edge_lab) + 1L]] <-
          data.frame(label = fZlab, si = si, ti = ti, coef = "fZ",
                     stringsAsFactors = FALSE)
      }
    }
    rhs <- paste(terms, collapse = " + ")
    if (has_Z) rhs <- paste0(rhs, " + .Z")
    eqs <- c(eqs, sprintf("v%d ~ 1 + %s%s", ti, rhs, ctrl))
  }
  edge_lab <- do.call(rbind, edge_lab)

  ## indirect-effect definitions along directed X -> Y paths (>= 2 edges).
  ## max_paths <= 0 skips enumeration silently (used by pathXMY's joint
  ## fit, which never carried indirect rows).
  defs <- character(0)
  def_map <- list()
  if (max_paths > 0L && !is.na(spec$X) && !is.na(spec$Y) &&
      spec$X %in% nodes && spec$Y %in% nodes) {
    adj <- lapply(seq_along(nodes), function(i) edges$ti[edges$si == i])
    ptoks <- .pathF_enum_paths(adj, as.integer(tok[spec$X]),
                               as.integer(tok[spec$Y]), max_paths)
    if (is.null(ptoks)) {
      warning(sprintf(
        "More than %d directed %s -> %s paths; skipping indirect-effect definitions.",
        max_paths, spec$X, spec$Y), call. = FALSE)
    } else {
      di <- 0L
      for (p in ptoks) {
        L <- length(p) - 1L
        if (L < 2L) next                 # single-edge = the direct path
        f1s <- sprintf("f1_%d_%d", p[-length(p)], p[-1])
        di <- di + 1L
        ilab <- sprintf("ind_%d", di)
        defs <- c(defs, sprintf("%s := %s", ilab, paste(f1s, collapse = "*")))
        def_map[[ilab]] <- list(kind = "ind", path = p, edge = NA_integer_)
        if (has_Z) {
          for (e in seq_len(L)) {
            comp    <- f1s
            comp[e] <- sprintf("fZ_%d_%d", p[e], p[e + 1L])
            zlab <- sprintf("indZ_%d_%d", di, e)
            defs <- c(defs, sprintf("%s := %s", zlab, paste(comp, collapse = "*")))
            def_map[[zlab]] <- list(kind = "indZ", path = p, edge = e)
          }
        }
      }
    }
  }

  list(model = paste(c(eqs, defs), collapse = "\n"),
       edge_lab = edge_lab, def_map = def_map)
}

## Enumerate all directed paths from token `from` to token `to`.
## Returns a list of integer token vectors, or NULL if the count exceeds
## `max_paths` (caller then skips indirect enumeration).
.pathF_enum_paths <- function(adj, from, to, max_paths) {
  out  <- list()
  over <- FALSE
  rec <- function(node, path) {
    if (over) return(invisible())
    if (node == to) {
      out[[length(out) + 1L]] <<- c(path, node)
      if (length(out) > max_paths) over <<- TRUE
      return(invisible())
    }
    for (nx in adj[[node]]) rec(nx, c(path, node))
  }
  rec(from, integer(0))
  if (over) return(NULL)
  out
}

## Build the tidy table from a fitted pathF lavaan object, keyed on the
## user-supplied parameter labels (edges) and defined-parameter labels
## (indirect terms).
.pathF_tidy <- function(fit, mb, spec, conf.level) {
  pe <- lavaan::parameterestimates(fit, level = conf.level)
  nodes <- spec$nodes
  rows <- list()

  pf_row <- function(param, src, tgt, label, pr) {
    data.frame(param = param, src = src, tgt = tgt, label = label,
               est = pr$est[1], se = pr$se[1], z = pr$z[1],
               pvalue = pr$pvalue[1],
               ci.lower = pr$ci.lower[1], ci.upper = pr$ci.upper[1],
               row.names = NULL, stringsAsFactors = FALSE)
  }

  ## structural edges
  el <- mb$edge_lab
  for (r in seq_len(nrow(el))) {
    pr <- pe[pe$label == el$label[r] & pe$op == "~", , drop = FALSE]
    if (!nrow(pr)) next
    src <- nodes[el$si[r]]; tgt <- nodes[el$ti[r]]
    rows[[length(rows) + 1L]] <-
      pf_row(sprintf("%s_%s_%s", el$coef[r], src, tgt), src, tgt,
             el$label[r], pr)
  }

  ## indirect / route-moderation products
  for (lab in names(mb$def_map)) {
    pr <- pe[pe$label == lab & pe$op == ":=", , drop = FALSE]
    if (!nrow(pr)) next
    info <- mb$def_map[[lab]]
    p <- info$path
    esrc <- nodes[p[-length(p)]]; etgt <- nodes[p[-1]]
    comps <- sprintf("f1_%s_%s", esrc, etgt)
    if (identical(info$kind, "indZ")) {
      comps[info$edge] <- sprintf("fZ_%s_%s", esrc[info$edge], etgt[info$edge])
    }
    rows[[length(rows) + 1L]] <-
      pf_row(paste(comps, collapse = " * "),
             nodes[p[1]], nodes[p[length(p)]], lab, pr)
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
