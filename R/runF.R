#' Iterate a Functional Field Forward
#'
#' @description
#' Drives the Markov iteration `s_(t+1) = s_t %*% F_t` for a fixed
#' number of steps, optionally applying scheduled action perturbations
#' to the state at the start of each step. Returns the full trajectory
#' of states.
#'
#' @details
#' At each step `t` in `1:steps`, the function:
#' 1. Applies any actions scheduled for step `t` --- each named state
#'    variable is set to its scheduled value (default `1`) *before*
#'    propagation.
#' 2. Calls [evalF()] to compute `s_(t+1)` from the post-action `s_t`.
#' 3. Optionally clamps the result to `[-1, 1]`.
#'
#' The `actions` argument is a list whose names are step numbers (as
#' character strings, e.g. `"1"`, `"2"`) and whose values are either:
#' * a character vector of state-variable names to set to `1`, or
#' * a named numeric vector specifying the value to set each variable to.
#'
#' Variables that never appear on the LHS of a `~` in `model` (exogenous
#' nodes, typically actions or persistent context) are carried forward
#' by `evalF()` unchanged --- once an action is set to `1` it stays at
#' `1` for the rest of the run unless clamping or a later equation
#' overwrites it.
#'
#' @param model A `lavaan`-syntax model string with `fZ_X.Y`-labelled
#'   parameters.
#' @param params Named numeric vector mapping parameter labels to values.
#' @param s_0 Named numeric vector of initial state values.
#' @param actions Optional list of action schedules. Names are step
#'   numbers (character); values are character or named numeric vectors.
#'   See Details. Default empty list (no scheduled actions; perturb
#'   `s_0` directly instead).
#' @param steps Integer number of propagation steps. Default 4 (matches
#'   the four-stage coffee example).
#' @param clamp Logical. When `TRUE` (default), clamp state values to
#'   `[-1, 1]` after each propagation step.
#'
#' @return A numeric matrix with `steps + 1` rows (indexed `t=0` through
#'   `t=steps`) and one column per state variable. Row `t=0` is `s_0`
#'   *before* any actions are applied; subsequent rows are post-action,
#'   post-propagation states.
#'
#' @seealso [evalF()] for the single-step propagator.
#'
#' @export
runF <- function(model, params, s_0, actions = list(), steps = 4L,
                 clamp = TRUE) {

  if (is.null(names(s_0)) || any(names(s_0) == "")) {
    stop("`s_0` must be a fully named numeric vector.")
  }
  steps <- as.integer(steps)
  if (length(steps) != 1L || is.na(steps) || steps < 0L) {
    stop("`steps` must be a single non-negative integer.")
  }

  trajectory <- matrix(NA_real_, nrow = steps + 1L, ncol = length(s_0),
                       dimnames = list(paste0("t=", 0:steps), names(s_0)))
  trajectory[1L, ] <- s_0

  s <- s_0
  for (t in seq_len(steps)) {

    sched <- actions[[as.character(t)]]
    if (!is.null(sched)) {
      if (is.character(sched)) {
        missing_vars <- setdiff(sched, names(s))
        if (length(missing_vars)) {
          stop("Action variables not in state: ",
               paste(missing_vars, collapse = ", "))
        }
        s[sched] <- 1
      } else if (is.numeric(sched) && !is.null(names(sched))) {
        missing_vars <- setdiff(names(sched), names(s))
        if (length(missing_vars)) {
          stop("Action variables not in state: ",
               paste(missing_vars, collapse = ", "))
        }
        s[names(sched)] <- sched
      } else {
        stop("Each `actions` entry must be a character vector or a ",
             "named numeric vector.")
      }
    }

    s <- evalF(model, params, s)

    if (clamp) {
      s[s >  1] <-  1
      s[s < -1] <- -1
    }

    trajectory[t + 1L, ] <- s
  }

  trajectory
}
