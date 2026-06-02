#' Evaluate One Markov Step of a Functional Field
#'
#' @description
#' Takes a `lavaan`-syntax model string `F` with `fZ_X.Y`-labelled
#' parameters, a vector of parameter values, and a current state `s_t`,
#' and returns the next state `s_(t+1)` produced by one step of the
#' Markov dynamics `s_(t+1) = s_t %*% F_t`, where `F_t = F(s_t)`.
#'
#' @details
#' Each regression row in `model` of the form
#'
#'   `Y ~ label_1 * rhs_1 + label_2 * rhs_2 + ...`
#'
#' contributes to the new value of `Y` as
#'
#'   `s_(t+1)[Y] = sum_k params[label_k] * prod(s_t[vars(rhs_k)])`
#'
#' where `rhs_k` is either a single state variable or an interaction
#' term `X:Z:...` parsed as the product of the named state variables.
#' Variables that never appear on the left of `~` (exogenous nodes,
#' typically actions or persistent contextual states) are carried
#' forward unchanged. See `vignette("notation", package = "funfield")`
#' for the full schema.
#'
#' Every regression term must resolve a coefficient one of two ways: a
#' **labelled** term (`fZ_X.Y * X`) takes its value from `params`, and a
#' **fixed** term (`1 * X`, lavaan's fixed-coefficient syntax) takes its
#' value from the coefficient itself --- useful for structural forces
#' such as a conditional action plan's triggers, which are committed
#' rather than estimated and so need no `params` entry. A term that is
#' neither labelled nor fixed cannot be valued and raises an error, as do
#' a label missing from `params` or a variable missing from `s_t`.
#'
#' @param model A `lavaan`-syntax model string. Only regression rows
#'   (`~`) are interpreted; other operators are ignored.
#' @param params Named numeric vector mapping parameter labels (e.g.
#'   `"f1_X.Y"`, `"fS2_TurnOn.HCoPot"`) to their values.
#' @param s_t Named numeric vector of state values at time `t`.
#'
#' @return Named numeric vector `s_(t+1)` of the same length and names
#'   as `s_t`.
#'
#' @seealso [runF()] for the iteration driver.
#'
#' @export
evalF <- function(model, params, s_t) {

  if (is.null(names(s_t)) || any(names(s_t) == "")) {
    stop("`s_t` must be a fully named numeric vector.")
  }
  if (is.null(names(params)) || any(names(params) == "")) {
    stop("`params` must be a fully named numeric vector.")
  }

  pt <- lavaan::lavaanify(model, fixed.x = FALSE)
  pt <- pt[pt$op == "~", , drop = FALSE]

  if (nrow(pt) == 0L) {
    return(s_t)
  }

  ## A term resolves its coefficient one of two ways: from `params` via
  ## a label (estimated forces, `fZ_X.Y * X`), or from a fixed value
  ## (structural coefficients, e.g. `1 * X` in a conditional action
  ## plan). lavaanify marks the latter with free == 0 and a non-NA
  ## ustart. A term that is neither labelled nor fixed cannot be valued.
  labelled <- !is.na(pt$label) & pt$label != ""
  fixed    <- (pt$free == 0) & !is.na(pt$ustart)
  unresolved <- !labelled & !fixed
  if (any(unresolved)) {
    bad <- paste0(pt$lhs[unresolved], " ~ ", pt$rhs[unresolved])
    stop("Every regression term must carry a label or a fixed ",
         "coefficient (e.g. `1 * x`). Unresolved: ",
         paste(bad, collapse = "; "))
  }

  missing_params <- setdiff(pt$label[labelled], names(params))
  if (length(missing_params)) {
    stop("Parameter values not supplied: ",
         paste(missing_params, collapse = ", "))
  }

  rhs_vars <- lapply(pt$rhs, function(x) strsplit(x, ":", fixed = TRUE)[[1]])
  all_vars <- unique(c(pt$lhs, unlist(rhs_vars)))
  missing_state <- setdiff(all_vars, names(s_t))
  if (length(missing_state)) {
    stop("State variables not in `s_t`: ",
         paste(missing_state, collapse = ", "))
  }

  s_next <- s_t
  targets <- unique(pt$lhs)
  s_next[targets] <- 0

  for (i in seq_len(nrow(pt))) {
    coef       <- if (labelled[i]) params[[pt$label[i]]] else pt$ustart[i]
    state_prod <- prod(s_t[rhs_vars[[i]]])
    s_next[pt$lhs[i]] <- s_next[pt$lhs[i]] + coef * state_prod
  }

  s_next
}
