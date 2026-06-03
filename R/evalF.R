## Kahn topological sort of `items` given a named list `deps` mapping each
## item to the items it must follow. Returns the order; stops if a cycle
## remains. Used to sequence rate flows within a sync step.
.topo_order <- function(items, deps) {
  order_out <- character(0)
  remaining <- items
  while (length(remaining)) {
    ready <- remaining[vapply(remaining, function(tg)
      length(intersect(deps[[tg]], remaining)) == 0L, logical(1))]
    if (!length(ready))
      stop("Field is not acyclic: cycle among rate flows: ",
           paste(remaining, collapse = ", "), ".")
    order_out <- c(order_out, ready)
    remaining <- setdiff(remaining, ready)
  }
  order_out
}

#' Evaluate One Step of a Functional Field
#'
#' @description
#' Takes a `lavaan`-syntax model string `F` with `fZ_X.Y`-labelled
#' parameters, a vector of parameter values, and a current state `s_t`,
#' and returns the next state `s_(t+1)` produced by one step of the
#' field dynamics. Two propagation modes are available --- a
#' **topological sweep** (the default) that resolves the whole causal
#' chain in one step, and a **synchronous** update that advances the
#' state one edge at a time --- and individual nodes can be marked as
#' latching **stocks** to give the field memory.
#'
#' @details
#' Each regression row in `model` of the form
#'
#'   `Y ~ label_1 * rhs_1 + label_2 * rhs_2 + ...`
#'
#' contributes `params[label_k] * prod(s[vars(rhs_k)])` to the new value
#' of `Y`, where `rhs_k` is a single state variable or an interaction
#' term `X:Z:...` parsed as the product of the named state variables.
#' A **labelled** term (`fZ_X.Y * X`) takes its value from `params`; a
#' **fixed** term (`1 * X`, lavaan's fixed-coefficient syntax) takes its
#' value from the coefficient itself. A term that is neither labelled nor
#' fixed cannot be valued and raises an error, as do a label missing from
#' `params` or a variable missing from `s_t`.
#'
#' **Propagation mode.**
#' \describe{
#'   \item{`"sweep"` (default)}{Endogenous nodes are evaluated in
#'     topological (causal) order, each reading the *freshly updated*
#'     values of its inputs, so an entire causal chain settles in a
#'     single call. A gated force `Y ~ Z:X` resolves correctly because
#'     the moderator `Z` is treated as a dependency and is updated before
#'     `Y`. Requires the field to be a DAG; a feedback cycle raises an
#'     error (use `"sync"` instead).}
#'   \item{`"sync"`}{One discrete time step of a stock-and-flow system,
#'     evaluated in the standard rate-then-integrate order so each tick
#'     advances the process one stage. Three phases run in sequence:
#'     (1) **rate flows** --- the non-stock, non-readout nodes (typically
#'     *actions*) --- are recomputed from the start-of-step **stock
#'     levels**; (2) **stocks** integrate, `new = old + inflow - outflow`,
#'     reading the *fresh* rate flows just computed and the *old* stock
#'     levels; (3) **readouts** (named in `readouts`) recompute from the
#'     *freshly updated* stocks. Evaluating rates before integrating means
#'     a stock is consumed in the very tick the action that draws on it
#'     fires, so an affording state lives exactly one step and each action
#'     fires exactly **once** before its trigger is spent. This is the
#'     natural mode for cyclic / dynamical fields --- in particular
#'     stock-and-flow consumption pipelines, which are inherently cyclic
#'     and cannot sweep.}
#' }
#'
#' **Stocks vs. flows.** By default every endogenous node is recomputed
#' from scratch each step (a *flow*: its value is a pure function of the
#' current state, so it does not persist on its own). A node named in
#' `stocks` instead *latches*: it carries its previous value forward and
#' the model's terms add to or subtract from it, i.e.
#' `s_(t+1)[Y] = s_t[Y] + sum_k coef_k * prod(...)`. Stocks are how a
#' field remembers --- an object state stays on once attained, and a
#' negative (consumption) term such as `c * Y:trigger` draws it back
#' down. lavaan forbids the self-edge `Y ~ Y`, so the carry-forward
#' cannot be written in the model string; marking `Y` a stock supplies it.
#' See [simulateF()], which drives this one-step propagator as a turn-based
#' process and derives the stock set automatically, and
#' `vignette("coffee_field_model", package = "funfield")` for a worked
#' stock/flow example.
#'
#' Nodes that never appear on the left of `~` (exogenous nodes, typically
#' persistent context) are by default carried forward unchanged, which is
#' itself stock-like behaviour. Naming such a node in `flows` overrides
#' that default and makes it a **one-shot pulse**: it is seeded by `s_t`,
#' read by its consumers this step, and then zeroed, so it is "on" only at
#' the moment it fires and then depletes. This is the transient dual of
#' the carried-forward default, and is how a **choice / chance node** that
#' fires once and is spent is expressed (it triggers the first action and
#' then empties, rather than persisting as a stock). `flows` has no effect
#' on endogenous nodes, which already recompute unless listed in `stocks`.
#'
#' @param model A `lavaan`-syntax model string. Only regression rows
#'   (`~`) are interpreted; other operators are ignored.
#' @param params Named numeric vector mapping parameter labels (e.g.
#'   `"f1_X.Y"`, `"fs2_TurnOn.HCoPot"`) to their values.
#' @param s_t Named numeric vector of state values at time `t`.
#' @param stocks Character vector of node names that **latch** (carry
#'   their value forward). Names not on a left-hand side are ignored.
#'   Default `character(0)` --- every node recomputes (memoryless),
#'   matching the field's afforded-equilibrium behaviour.
#' @param flows Character vector of **exogenous** node names (those never
#'   on a left-hand side) to treat as one-shot pulses: seeded, read, then
#'   zeroed each step instead of carried forward. Endogenous nodes already
#'   recompute and are unaffected. Default `character(0)`. See Details.
#' @param readouts Character vector of endogenous node names that recompute
#'   from the *freshly updated* stock levels at the end of a `"sync"` step
#'   (phase 3) --- the likelihood / appraisal nodes, which should reflect
#'   the current state rather than feed back into it. Ignored under
#'   `"sweep"`. Default `character(0)`.
#' @param mode Propagation mode: `"sweep"` (default, topological,
#'   whole-chain) or `"sync"` (one stock/flow time step). See Details.
#'
#' @return Named numeric vector `s_(t+1)` of the same length and names
#'   as `s_t`.
#'
#' @seealso [simulateF()], the turn-based driver that calls this propagator
#'   one action at a time, and [labelF()] for building the model string.
#'
#' @export
evalF <- function(model, params, s_t, stocks = character(0),
                  flows = character(0), readouts = character(0),
                  mode = c("sweep", "sync")) {

  mode <- match.arg(mode)

  if (is.null(names(s_t)) || any(names(s_t) == "")) {
    stop("`s_t` must be a fully named numeric vector.")
  }
  if (length(params) && (is.null(names(params)) || any(names(params) == ""))) {
    stop("`params` must be a fully named numeric vector.")
  }

  pt <- lavaan::lavaanify(model, fixed.x = FALSE)
  pt <- pt[pt$op == "~", , drop = FALSE]

  if (nrow(pt) == 0L) {
    return(s_t)
  }

  ## A term resolves its coefficient one of two ways: from `params` via a
  ## label (estimated forces, `fZ_X.Y * X`), or from a fixed value
  ## (structural coefficients, e.g. `1 * X`). lavaanify marks the latter
  ## with free == 0 and a non-NA ustart. A term that is neither cannot be
  ## valued.
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

  targets      <- unique(pt$lhs)
  flow_targets <- setdiff(targets, stocks)        # latching nodes keep s_t[Y]
  ## Exogenous nodes flagged as flows are one-shot pulses: zeroed after the
  ## step rather than carried forward. They are never on a LHS, so nothing
  ## re-feeds them and they survive only at their seed value.
  exo_flows    <- intersect(setdiff(names(s_t), targets), flows)

  coef_i <- function(i) if (labelled[i]) params[[pt$label[i]]] else pt$ustart[i]
  rows_of <- function(tg) which(pt$lhs == tg)

  if (mode == "sync") {
    ## One stock/flow time step, evaluated rate-then-integrate so each
    ## tick advances the process exactly one stage (see Details).
    stock_t <- intersect(targets, stocks)             # latch
    rate_t  <- setdiff(flow_targets, readouts)        # action / rate flows
    read_t  <- intersect(flow_targets, readouts)      # appraisal readouts

    w <- s_t

    ## Phase 1 -- rate flows, recomputed from start-of-step stock levels.
    ## Ordered so an upstream rate feeds a downstream one within the step;
    ## stocks are still at their old (carried) values in `w` here.
    if (length(rate_t)) {
      rdeps <- lapply(rate_t, function(tg)
        intersect(unique(unlist(rhs_vars[rows_of(tg)])), rate_t))
      names(rdeps) <- rate_t
      for (tg in .topo_order(rate_t, rdeps)) {
        val <- 0
        for (i in rows_of(tg)) val <- val + coef_i(i) * prod(w[rhs_vars[[i]]])
        w[tg] <- val
      }
    }

    ## Phase 2 -- stocks integrate: carried value + inflows - outflows,
    ## reading the fresh rate flows and the old stock levels. Computed into
    ## a buffer so stocks do not see each other's updates within the step.
    if (length(stock_t)) {
      upd <- stats::setNames(numeric(length(stock_t)), stock_t)
      for (tg in stock_t) {
        val <- s_t[[tg]]
        for (i in rows_of(tg)) val <- val + coef_i(i) * prod(w[rhs_vars[[i]]])
        upd[[tg]] <- val
      }
      w[stock_t] <- upd
    }

    ## Phase 3 -- readouts / derived nodes reflect the freshly updated stock
    ## levels. Evaluated in topological order among themselves, so a derived
    ## node built from other derived nodes (e.g. a conjunction chain
    ## `p1 ~ a:b`, `p2 ~ p1:c`) settles to its total value within the step.
    if (length(read_t)) {
      r_dep <- lapply(read_t, function(tg)
        intersect(unique(unlist(rhs_vars[rows_of(tg)])), read_t))
      names(r_dep) <- read_t
      for (tg in .topo_order(read_t, r_dep)) {
        val <- 0
        for (i in rows_of(tg)) val <- val + coef_i(i) * prod(w[rhs_vars[[i]]])
        w[tg] <- val
      }
    }

    ## One-shot exogenous flows have now been read; empty them.
    if (length(exo_flows)) w[exo_flows] <- 0
    return(w)
  }

  ## mode == "sweep": evaluate targets in topological order, each reading
  ## the freshly updated state. A node depends on every endogenous
  ## component of its right-hand sides (the gate/moderator counts), minus
  ## itself --- a stock's own value in a latch/consumption term is taken
  ## from the carried-forward seed, not treated as an ordering constraint.
  deps <- lapply(targets, function(tg) {
    rows <- which(pt$lhs == tg)
    setdiff(intersect(unique(unlist(rhs_vars[rows])), targets), tg)
  })
  names(deps) <- targets

  order_out <- character(0)
  remaining <- targets
  repeat {
    ready <- remaining[vapply(remaining, function(tg)
      length(intersect(deps[[tg]], remaining)) == 0L, logical(1))]
    if (!length(ready)) {
      stop("Field is not acyclic: topological-sweep mode requires a DAG. ",
           "Cycle among: ", paste(remaining, collapse = ", "),
           ". Use mode = \"sync\" for cyclic fields (e.g. stock/flow ",
           "consumption pipelines).")
    }
    order_out <- c(order_out, ready)
    remaining <- setdiff(remaining, ready)
    if (!length(remaining)) break
  }

  s <- s_t
  for (tg in order_out) {
    rows <- which(pt$lhs == tg)
    val  <- if (tg %in% stocks) s_t[[tg]] else 0   # latch seed vs. recompute
    for (i in rows) val <- val + coef_i(i) * prod(s[rhs_vars[[i]]])
    s[tg] <- val
  }
  ## One-shot exogenous flows have now been read by their consumers; empty
  ## them so they do not persist into the next step's seed.
  if (length(exo_flows)) s[exo_flows] <- 0
  s
}
