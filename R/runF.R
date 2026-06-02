#' Resolve the Set of Latching (Stock) Nodes
#'
#' Internal helper. Given the endogenous targets of a field, work out
#' which ones latch, following the priority in [runF()]'s documentation:
#' an **auto** rule keyed on the action set, an **explicit** `stocks=` /
#' `flows=` set, or **memoryless** (nothing latches) when no hint is
#' given. Returns a character vector of node names to latch.
#'
#' @param targets Character vector of endogenous nodes (model LHS).
#' @param actions,readouts,stocks,flows See [runF()].
#' @return Character vector (subset of `targets`) of nodes that latch.
#' @keywords internal
#' @noRd
.resolveStocks <- function(targets, actions = NULL, readouts = "L",
                           stocks = NULL, flows = NULL) {

  if (!is.null(stocks) && !is.null(flows)) {
    clash <- intersect(stocks, flows)
    if (length(clash)) {
      stop("Nodes listed as both `stocks` and `flows`: ",
           paste(clash, collapse = ", "), ".")
    }
  }

  if (!is.null(actions)) {                         # AUTO mode
    flow0  <- intersect(union(actions, readouts), targets)
    stock0 <- setdiff(targets, flow0)
    res    <- setdiff(union(stock0, intersect(stocks, targets)), flows)
  } else if (!is.null(stocks)) {                   # EXPLICIT (stocks)
    res <- intersect(stocks, targets)
  } else if (!is.null(flows)) {                    # EXPLICIT (flows)
    res <- setdiff(targets, flows)
  } else {                                         # MEMORYLESS (default)
    res <- character(0)
  }
  res
}

#' Iterate a Functional Field Forward
#'
#' @description
#' Drives the field iteration for a fixed number of steps and returns the
#' full trajectory of states, calling [evalF()] once per step. Supports
#' both propagation modes and gives the field memory by latching a set of
#' **stock** nodes --- which it can derive automatically from the action
#' nodes, so the situation/plan structure of the model *is* the interface.
#'
#' @details
#' **Choosing a mode.** `mode = "sweep"` (default) resolves the whole
#' causal chain each step --- read off the settled outcome (e.g. the
#' likelihood node `L`) for "what does this plan ultimately afford?".
#' `mode = "sync"` advances one edge per step and is required for cyclic
#' / dynamical fields, including stock-and-flow consumption pipelines (a
#' consumed state and the action that consumes it form a cycle, which
#' `"sweep"` rejects).
#'
#' **When to specify stocks.** A *stock* latches (remembers its value);
#' a *flow* is recomputed from the current state each step. Which to use
#' is governed, in priority order, by:
#' \enumerate{
#'   \item **Auto** --- pass `actions` (the action nodes, the same set you
#'     name in [labelF()]). Every other endogenous node becomes a stock,
#'     except `readouts` (the likelihood / appraisal nodes, default `L`),
#'     which recompute. In the canonical situation/plan model this makes
#'     the situation's object states latch and the plan's actions fire as
#'     transient flows. Fine-tune individual nodes with `stocks` / `flows`.
#'   \item **Explicit** --- pass `stocks` (or `flows`) and no `actions`;
#'     exactly those nodes latch (or all but `flows` do). The low-level
#'     escape hatch for non-canonical fields.
#'   \item **Memoryless** --- pass none of the three (the default). Nothing
#'     latches; every node recomputes. This is the field's
#'     afforded-equilibrium behaviour and the right choice for a model
#'     written *without* consumption terms. (Latching an
#'     inflow-only model makes its stocks accumulate without bound ---
#'     see the bounds warning below.)
#' }
#' Marking a node as both a stock and a flow is an error.
#'
#' **Bounds.** No clamping is performed. Functional-field state is
#' expected to stay within `[-bound, bound]` (default `[-1, 1]`) when the
#' model is well specified; a value outside it signals a specification
#' error (e.g. a latched stock with inflow but no consumption, or forces
#' that compound past 1) rather than something to silently truncate. By
#' default `runF()` warns, naming the offending nodes and step, and
#' returns the true (un-clamped) values so the overshoot is visible.
#'
#' @param model A `lavaan`-syntax model string with `fZ_X.Y`-labelled or
#'   fixed (`1 * X`) parameters.
#' @param params Named numeric vector mapping parameter labels to values.
#' @param s_0 Named numeric vector of initial state values.
#' @param actions Optional character vector of action-node names. Supplied
#'   to derive the stock set automatically (see Details). Default `NULL`.
#' @param readouts Character vector of nodes that recompute rather than
#'   latch even under auto-derivation --- the likelihood / appraisal
#'   readouts. Default `"L"`.
#' @param stocks,flows Optional character vectors to force individual
#'   nodes to latch (`stocks`) or recompute (`flows`). Override the
#'   auto-derivation; without `actions` they set the stock set explicitly.
#'   A node in both is an error. Default `NULL`. An **exogenous** node
#'   (never on a left-hand side) named in `flows` becomes a one-shot pulse
#'   --- seeded then zeroed rather than carried forward, the way a choice /
#'   chance node fires once and is spent; see [evalF()].
#' @param mode Propagation mode passed to [evalF()]: `"sweep"` (default)
#'   or `"sync"`. See Details.
#' @param steps Integer number of propagation steps. Default 4.
#' @param warn_bounds Logical; when `TRUE` (default) warn if any state
#'   value exceeds `bound` in magnitude after a step. See Details.
#' @param bound Numeric magnitude bound for the warning. Default 1.
#'
#' @return A numeric matrix with `steps + 1` rows (`t=0` through
#'   `t=steps`) and one column per state variable. Row `t=0` is `s_0`.
#'
#' @seealso [evalF()] for the single-step propagator, [labelF()] for
#'   building the model and naming the actions.
#'
#' @export
runF <- function(model, params, s_0,
                 actions     = NULL,
                 readouts    = "L",
                 stocks      = NULL,
                 flows       = NULL,
                 mode        = c("sweep", "sync"),
                 steps       = 4L,
                 warn_bounds = TRUE,
                 bound       = 1) {

  mode <- match.arg(mode)

  if (is.null(names(s_0)) || any(names(s_0) == "")) {
    stop("`s_0` must be a fully named numeric vector.")
  }
  steps <- as.integer(steps)
  if (length(steps) != 1L || is.na(steps) || steps < 0L) {
    stop("`steps` must be a single non-negative integer.")
  }

  ## Derive the latching set once from the model's endogenous targets.
  pt      <- lavaan::lavaanify(model, fixed.x = FALSE)
  targets <- unique(pt$lhs[pt$op == "~"])
  stock_set <- .resolveStocks(targets, actions = actions, readouts = readouts,
                              stocks = stocks, flows = flows)

  trajectory <- matrix(NA_real_, nrow = steps + 1L, ncol = length(s_0),
                       dimnames = list(paste0("t=", 0:steps), names(s_0)))
  trajectory[1L, ] <- s_0

  ## Exogenous nodes flagged as flows fire once and deplete; pass them to
  ## evalF, which zeroes them after each step (endogenous nodes are handled
  ## by `stock_set`). Coerce the optional `flows` to a character vector.
  flow_set <- if (is.null(flows)) character(0) else flows

  s <- s_0
  for (t in seq_len(steps)) {
    s <- evalF(model, params, s, stocks = stock_set, flows = flow_set,
               mode = mode)

    if (warn_bounds) {
      bad <- names(s)[abs(s) > bound]
      if (length(bad)) {
        warning("runF: |state| > ", bound, " at t = ", t,
                " (possible model misspecification): ",
                paste0(bad, "=", round(s[bad], 3), collapse = ", "),
                call. = FALSE)
      }
    }

    trajectory[t + 1L, ] <- s
  }

  trajectory
}
