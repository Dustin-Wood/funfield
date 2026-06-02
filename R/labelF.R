#' Auto-generate `fZ_X.Y` parameter labels for a field model
#'
#' @description
#' Takes a `lavaan`-syntax field model whose regression terms may be
#' unlabelled (e.g. `Y ~ X:Z`) or carry fixed coefficients (e.g.
#' `Y ~ 1 * X:Z`) and rewrites every term with its canonical `fZ_X.Y`
#' label, so models can be specified without typing the labels by hand.
#' Returns both the relabelled model string and a `params` vector
#' pre-populated with any coefficients the input fixed.
#'
#' @details
#' Labelling is **order-dependent**, and the order is read straight off
#' the right-hand side: `Y ~ Z:X` becomes `fZ_X.Y`. The interaction is
#' written **condition first, action last** --- the leading variable(s)
#' are the gating **condition** `Z`, the final variable is the **action**
#' / source `X`, and `Y` is the **target**. So the generic form is
#' `Y ~ Condition:Action`, generating `fCondition_Action.Y`. A term with
#' a single right-hand variable (`Y ~ X`) carries no condition and becomes
#' the baseline `f1_X.Y`; multiple leading conditions are concatenated
#' (matching the compound `fab_X.Y` convention). Variable names are used
#' verbatim (no case change), so the labels round-trip cleanly.
#'
#' Because the order is load-bearing, conditions must precede actions. If
#' `actions` is supplied, `labelF()` warns when one of those action
#' variables turns up in a condition (non-final) position --- a sign the
#' term was written `Y ~ Action:Condition` by mistake.
#'
#' A fixed coefficient (`1 * Z:X`, or any `c * Z:X`) is *not* an obstacle:
#' the label is generated exactly as for a free term, and the fixed value
#' is captured as that parameter's entry in `params`. For example, a row
#' `s2 ~ 1 * s1:make` becomes `s2 ~ fs1_make.s2 * s1:make` with
#' `params["fs1_make.s2"] = 1`. Terms left free and unfixed get a
#' `params` value of `default` (`NA` by default), to be filled in before
#' the model is run. Terms that already carry a label keep it.
#'
#' @param model A `lavaan`-syntax model string of regression (`~`) rows.
#' @param actions Optional character vector of action (source) variable
#'   names. When supplied, a warning is raised for any term where an
#'   action appears in a condition position, since the convention is
#'   `Y ~ Condition:Action`. Default `NULL` (no check).
#' @param default Value assigned in `params` to terms that neither carry
#'   a fixed coefficient nor an existing label-with-value. Default `NA`.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{`model`}{The relabelled model string: every regression term
#'       rewritten as `label * rhs`.}
#'     \item{`params`}{A named numeric vector, one entry per generated /
#'       retained label, holding the fixed value where the input
#'       supplied one and `default` otherwise.}
#'   }
#'   Suitable for passing straight to [runF()] / [evalF()] (as
#'   `out$model`, `out$params`), or for stitching two halves with
#'   `paste(a$model, b$model)` and `c(a$params, b$params)`.
#'
#' @seealso [evalF()], [runF()], and
#'   `vignette("notation", package = "funfield")` for the label schema.
#' @examples
#' \dontrun{
#' situation <- "
#'   s2     ~ 1 * s1:make
#'   HCoPot ~ 1 * s2:TurnOn
#'   L      ~ 1 * HCo
#' "
#' lab <- labelF(situation)
#' lab$model    # terms now carry fs1_make.s2, fs2_TurnOn.HCoPot, f1_HCo.L
#' lab$params   # all = 1, taken from the fixed coefficients
#' }
#' @export
labelF <- function(model, actions = NULL, default = NA_real_) {

  pt <- lavaan::lavaanify(model, fixed.x = FALSE)
  pt <- pt[pt$op == "~", , drop = FALSE]
  if (nrow(pt) == 0L)
    stop("`model` contains no regression (`~`) terms to label.")

  ## Per-term: generated/retained label. Convention: rhs is read
  ## condition-first, action-last (`Y ~ Z:X` -> `fZ_X.Y`), so the final
  ## rhs variable is the action/source and any leading ones the
  ## condition(s).
  pt$.label <- character(nrow(pt))
  for (i in seq_len(nrow(pt))) {
    if (!is.na(pt$label[i]) && nzchar(pt$label[i])) {
      pt$.label[i] <- pt$label[i]
      next
    }
    rhs_vars <- strsplit(pt$rhs[i], ":", fixed = TRUE)[[1]]
    n        <- length(rhs_vars)
    source   <- rhs_vars[n]
    conds    <- if (n > 1L) rhs_vars[-n] else character(0)
    if (!is.null(actions) && length(conds)) {
      misplaced <- intersect(conds, actions)
      if (length(misplaced))
        warning("In '", pt$lhs[i], " ~ ", pt$rhs[i], "': action(s) '",
                paste(misplaced, collapse = "', '"), "' appear in a ",
                "condition position. The convention is ",
                "`Y ~ Condition:Action`, so actions should be listed ",
                "last -- it looks like this term may be written ",
                "back-to-front.", call. = FALSE)
    }
    Z <- if (length(conds)) paste(conds, collapse = "") else "1"
    pt$.label[i] <- sprintf("f%s_%s.%s", Z, source, pt$lhs[i])
  }

  pt$.value <- pt$ustart            # fixed coef or start value; NA if free
  pt$.value[is.na(pt$.value)] <- default

  ## Reconstruct the model, grouping terms by target in row order.
  lhs_order <- unique(pt$lhs)
  lines <- vapply(lhs_order, function(y) {
    rows  <- pt[pt$lhs == y, , drop = FALSE]
    terms <- paste0(rows$.label, " * ", rows$rhs)
    paste0(y, " ~ ", paste(terms, collapse = " + "))
  }, character(1))
  new_model <- paste(lines, collapse = "\n")

  ## params: one entry per label. Guard against a label colliding with
  ## conflicting values.
  params <- pt$.value
  names(params) <- pt$.label
  dup <- duplicated(names(params))
  if (any(dup)) {
    for (lab in unique(names(params)[dup])) {
      vals <- unique(stats::na.omit(params[names(params) == lab]))
      if (length(vals) > 1L)
        stop("Label '", lab, "' generated with conflicting values: ",
             paste(vals, collapse = ", "), ".")
    }
    params <- params[!dup]
  }

  list(model = new_model, params = params)
}
