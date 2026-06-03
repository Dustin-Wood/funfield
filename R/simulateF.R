#' Simulate a Functional Field as a Turn-Based Process
#'
#' @description
#' Runs a functional field forward as a sequence of **turns**, one action at
#' a time, under a **policy**. This is the deductive engine in its unified
#' form: a single `field` model (the physics --- how actions and states move
#' states and outcomes, including consumption) is driven by a separate
#' `policy` (which action is eligible when). Each turn the engine finds the
#' eligible actions, fires **one** of them, lets its total effects settle,
#' and records the result; the run ends when no action is eligible
#' (quiescence) or after `max_turns`.
#'
#' Because actions are injected **exogenously**, one per turn, the field over
#' the *states* is acyclic and settles cleanly --- so the same run answers
#' both "what does this plan ultimately afford?" (read the final `readout`)
#' and "how does it unfold in time?" (read the whole trajectory). There is
#' no separate equilibrium vs. dynamical mode.
#'
#' @details
#' **The turn.** Given the current state, an action is **eligible** when the
#' state variables named as its condition in `policy` are jointly present
#' (their product exceeds `tol`). Of the eligible actions, one fires:
#' \itemize{
#'   \item if exactly one is eligible, it fires;
#'   \item if several are, each is **appraised** --- simulated one turn ahead
#'     --- and the one yielding the highest `readout` fires (the deductive
#'     role of a choice / chance node, resolved here as a one-step argmax).
#' }
#' The chosen action is injected (set to `1`) and the field takes one step
#' via [evalF()] in `mode = "sync"`: stocks transition by `new = old +
#' inflow - outflow` reading the *old* stock levels (so a production term
#' uses the un-consumed upstream value), then the `readout` settles against
#' the new state. The action is then spent (reset to `0`), as are any
#' **one-shot triggers** among its conditions named in `flows` (e.g. an
#' intention `choice` that is used up once it launches the first action).
#' Field-level consumption makes the remaining actions self-limiting: once an
#' action consumes the state that afforded it, it is no longer eligible.
#'
#' **Why one action per turn.** Firing a single action and settling its total
#' effects --- rather than placing a whole plan in the model at once ---
#' advances the process exactly one stage: downstream forces gated on
#' *other* actions stay shut until those actions take their own turn. A plan
#' that omits a step simply runs out of eligible actions there and halts,
#' which is the structural signature of a broken plan.
#'
#' @param field A `lavaan`-syntax model string of the field's forces
#'   (production, consumption, cost, value), with `fZ_X.Y`-labelled or fixed
#'   (`1 * X`) terms, as consumed by [evalF()]. Action nodes appear only on
#'   right-hand sides (they are exogenous); the engine injects them.
#' @param params Named numeric vector of parameter values for `field`.
#' @param s_0 Named numeric vector of the initial state, including every
#'   action node (typically `0`) and any trigger such as `choice`.
#' @param policy A `lavaan`-syntax string of `action ~ condition` rows
#'   (the plan): each names an action and the state variable(s) whose joint
#'   presence makes it eligible. Coefficients are ignored --- only the
#'   condition variables matter. A single bare variable (`make ~ choice`) is
#'   a one-variable condition; an interaction (`Pour ~ HCoPot`) likewise.
#' @param readout Name of the node read as the value to appraise against and
#'   to report. Default `"L"`.
#' @param aux Character vector of **derived** field nodes that recompute each
#'   turn rather than latch --- pure functions of the stocks, such as a
#'   conjunction `Prepped ~ a:b:c` that is on only while all of `a`, `b`, `c`
#'   are. They may be used as policy conditions. Default `NULL`. (The
#'   `readout` is always non-latching; `aux` adds to it.)
#' @param stocks Character vector of field nodes that **latch**. Default
#'   `NULL` --- derive automatically as every field target except `readout`
#'   and `aux` (object/resource states persist; readouts and derived nodes
#'   recompute).
#' @param flows Character vector of exogenous **one-shot triggers** that are
#'   spent when the action they gate fires (seeded in `s_0`, depleted on use)
#'   rather than persisting. Default `NULL`.
#' @param max_turns Maximum number of turns. Default `32`.
#' @param tol Threshold on a condition's product for an action to count as
#'   eligible. Default `1e-9`.
#' @param warn_bounds,bound When `warn_bounds` is `TRUE`, warn if any state
#'   value exceeds `bound` in magnitude after a turn --- functional-field
#'   state is expected to stay within `[-bound, bound]`, so an overshoot
#'   signals a misspecification (e.g. an unconsumed stock). Defaults `TRUE`,
#'   `1`.
#'
#' @return A list with:
#'   \describe{
#'     \item{`trajectory`}{Numeric matrix, one row per recorded state
#'       (`t=0` through `t=N`, `N` the number of turns run) and one column
#'       per state variable. Row `t=0` is `s_0`; each later row is the state
#'       after that turn, with the action that fired marked `1` for display
#'       (it is otherwise spent).}
#'     \item{`fired`}{Character vector, the action fired on each turn.}
#'     \item{`halted`}{`"quiescent"` (no eligible action) or `"max_turns"`.}
#'   }
#'
#' @seealso [evalF()] for the within-turn step, [labelF()] / [costF()] for
#'   building the field, [plotField()] to draw a trajectory row, and
#'   `vignette("coffee_field_model", package = "funfield")`.
#' @examples
#' \dontrun{
#' field <- "
#'   s2     ~ 1 * make        + use * s2:TurnOn
#'   HCoPot ~ 1 * s2:TurnOn   + use * HCoPot:Pour
#'   HCoCup ~ 1 * HCoPot:Pour + use * HCoCup:Sip
#'   HCo    ~ 1 * HCoCup:Sip
#'   Energy ~ -0.1*make + -0.1*TurnOn + -0.1*Pour + -0.1*Sip
#'   L      ~ 0.9 * HCo + 0.1 * Energy
#' "
#' policy <- "make ~ choice
#'            TurnOn ~ s2
#'            Pour ~ HCoPot
#'            Sip ~ HCoCup"
#' s_0 <- c(choice = 1, make = 0, s2 = 0, TurnOn = 0, HCoPot = 0,
#'          Pour = 0, HCoCup = 0, Sip = 0, HCo = 0, Energy = 0, L = 0)
#' out <- simulateF(field, c(use = -1), s_0, policy, flows = "choice")
#' out$trajectory; out$fired   # make -> TurnOn -> Pour -> Sip, final L = .86
#' }
#' @export
simulateF <- function(field, params, s_0, policy,
                      readout     = "L",
                      aux         = NULL,
                      stocks      = NULL,
                      flows       = NULL,
                      max_turns   = 32L,
                      tol         = 1e-9,
                      warn_bounds = TRUE,
                      bound       = 1) {

  if (is.null(names(s_0)) || any(names(s_0) == ""))
    stop("`s_0` must be a fully named numeric vector.")
  if (!is.character(field)  || length(field)  != 1L)
    stop("`field` must be a single model string.")
  if (!is.character(policy) || length(policy) != 1L)
    stop("`policy` must be a single model string.")

  ## -- Parse the policy: each `action ~ condition...` row gives an action
  ## and the state variables whose joint presence makes it eligible.
  ppt <- lavaan::lavaanify(policy, fixed.x = FALSE)
  ppt <- ppt[ppt$op == "~", , drop = FALSE]
  if (nrow(ppt) == 0L)
    stop("`policy` must contain at least one `action ~ condition` row.")
  pact <- unique(ppt$lhs)
  cond <- lapply(pact, function(a)
    unique(unlist(strsplit(ppt$rhs[ppt$lhs == a], ":", fixed = TRUE))))
  names(cond) <- pact

  miss <- setdiff(unique(c(pact, unlist(cond))), names(s_0))
  if (length(miss))
    stop("Policy variables not in `s_0`: ", paste(miss, collapse = ", "), ".")

  ## -- Non-latching nodes: the appraisal `readout` plus any derived `aux`
  ## nodes (pure functions of the stocks, recomputed each turn, e.g. an AND
  ## of several prerequisites). Everything else among the field's targets is
  ## a latching stock.
  nonlatch  <- union(readout, if (is.null(aux)) character(0) else aux)
  ft        <- lavaan::lavaanify(field, fixed.x = FALSE)
  targets   <- unique(ft$lhs[ft$op == "~"])
  stock_set <- if (is.null(stocks)) setdiff(targets, nonlatch) else stocks
  flow_set  <- if (is.null(flows))  character(0)              else flows

  ## -- One turn: inject one action, take a single field step (stock
  ## transition + derived/readout settle), then spend the action and any
  ## one-shot triggers among its conditions.
  one_turn <- function(state, a) {
    st <- state; st[[a]] <- 1
    nx <- evalF(field, params, st, stocks = stock_set,
                readouts = nonlatch, mode = "sync")
    nx[[a]] <- 0
    spent <- intersect(cond[[a]], flow_set)
    if (length(spent)) nx[spent] <- 0
    nx
  }

  state  <- s_0
  traj   <- list(`t=0` = s_0)
  fired  <- character(0)
  halted <- "max_turns"

  for (turn in seq_len(as.integer(max_turns))) {
    elig <- pact[vapply(pact, function(a)
      isTRUE(prod(state[cond[[a]]]) > tol), logical(1))]
    if (!length(elig)) { halted <- "quiescent"; break }

    chosen <- if (length(elig) == 1L) elig else
      elig[which.max(vapply(elig, function(a)
        one_turn(state, a)[[readout]], numeric(1)))]

    state <- one_turn(state, chosen)

    if (warn_bounds) {
      bad <- names(state)[abs(state) > bound]
      if (length(bad))
        warning("simulateF: |state| > ", bound, " at turn ", turn,
                " (possible misspecification): ",
                paste0(bad, "=", round(state[bad], 3), collapse = ", "),
                call. = FALSE)
    }

    disp <- state; disp[[chosen]] <- 1            # mark the action that fired
    traj[[paste0("t=", turn)]] <- disp
    fired <- c(fired, chosen)
  }

  list(trajectory = do.call(rbind, traj), fired = fired, halted = halted)
}
