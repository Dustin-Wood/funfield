#' Choose Among Action Plans by Expected Outcome
#'
#' @description
#' The deductive counterpart of a **choice / chance node**: given a
#' situation field and a set of candidate action plans, simulate each plan
#' forward and report what it affords, then flag the plan with the highest
#' value on a readout node (by convention the likelihood node `L`, playing
#' the role of subjective expected utility). This is how a choice node
#' "considers multiple responses to the situation and selects the action
#' with the highest expected utility" --- the selection is an argmax over
#' counterfactual field runs, which no single linear field step can
#' express, so it is computed here across runs.
#'
#' @details
#' Each plan is stitched onto `situation` (`paste(situation, plan)`) and
#' run through [runF()] in `mode = "sweep"`, which resolves the whole
#' causal chain in one step, so the returned trajectory's final row is the
#' afforded equilibrium. The value of each `report` node at that
#' equilibrium becomes one cell of the output; the `readout` node's value
#' is the plan's expected utility and drives the `chosen` flag.
#'
#' A plan that omits a necessary step affords nothing downstream of the
#' gap, because the situation's forces are gated: e.g. dropping the pour
#' from a coffee plan leaves the cup empty, so the sip force --- gated on
#' there being coffee in the cup --- never fires and the readout stays at
#' zero. Comparing the readout column across plans is what lets the choice
#' node reject such a plan in favour of a complete one.
#'
#' With `null_plan = TRUE` a do-nothing baseline (an empty plan, firing no
#' actions) is appended, giving the floor an inaction affords --- the
#' minimal "do something vs. do nothing" contrast.
#'
#' @param situation A `lavaan`-syntax model string for the situation
#'   (affordance structure), with `fZ_X.Y`-labelled or fixed (`1 * X`)
#'   terms, as consumed by [runF()]. The plans are pasted onto this.
#' @param params Named numeric vector of parameter values for the labelled
#'   terms in `situation` (and in any plan supplied as a [labelF()] list).
#' @param s_0 Named numeric vector of the initial state, including the
#'   choice-point seed (e.g. `choice = 1`).
#' @param plans A **named** list of candidate plans. Each element is either
#'   a `lavaan`-syntax plan string or a [labelF()]-style list with
#'   `$model` (and optionally `$params`, which are merged into `params`).
#' @param readout Name of the node read as expected utility and used to
#'   pick the chosen plan. Default `"L"`.
#' @param report Character vector of node names to tabulate at equilibrium.
#'   Default `NULL` --- use the situation's endogenous targets (the object
#'   states and the readout).
#' @param null_plan Logical; append a do-nothing baseline plan. Default
#'   `TRUE`.
#' @param null_name Row label for the do-nothing baseline. Default
#'   `"(do nothing)"`.
#' @param tol Numeric tolerance for the argmax tie-break on `readout`; any
#'   plan within `tol` of the maximum is flagged `chosen`. Default `1e-9`.
#'
#' @return A data frame with one row per plan (candidate plans in the order
#'   given, then the do-nothing baseline if requested), columns: `plan`
#'   (the list names), one numeric column per `report` node holding its
#'   afforded value, and `chosen` (logical) marking the maximum-`readout`
#'   plan(s).
#'
#' @seealso [runF()] for the underlying simulation, [labelF()] for building
#'   the situation and plans, and
#'   `vignette("coffee_field_model", package = "funfield")` for a worked
#'   choice-point example.
#' @examples
#' \dontrun{
#' situation <- labelF("
#'   s2     ~ 1 * make
#'   HCoPot ~ 1 * s2:TurnOn
#'   HCoCup ~ 1 * HCoPot:Pour
#'   HCo    ~ 1 * HCoCup:Sip
#'   L      ~ 1 * HCo
#' ", actions = c("make", "TurnOn", "Pour", "Sip"))
#'
#' full    <- "make ~ 1*choice
#'             TurnOn ~ 1*s2
#'             Pour ~ 1*HCoPot
#'             Sip ~ 1*HCoCup"
#' no_pour <- "make ~ 1*choice
#'             TurnOn ~ 1*s2
#'             Sip ~ 1*HCoCup"
#'
#' s_0 <- c(choice = 1, make = 0, s2 = 0, TurnOn = 0, HCoPot = 0,
#'          Pour = 0, HCoCup = 0, Sip = 0, HCo = 0, L = 0)
#'
#' chooseF(situation$model, situation$params, s_0,
#'         plans = list(full = full, no_pour = no_pour))
#' }
#' @export
chooseF <- function(situation, params, s_0, plans,
                    readout   = "L",
                    report    = NULL,
                    null_plan = TRUE,
                    null_name = "(do nothing)",
                    tol       = 1e-9) {

  if (!is.character(situation) || length(situation) != 1L)
    stop("`situation` must be a single model string.")
  if (is.null(names(s_0)) || any(names(s_0) == ""))
    stop("`s_0` must be a fully named numeric vector.")
  if (!is.list(plans) || !length(plans) ||
      is.null(names(plans)) || any(names(plans) == ""))
    stop("`plans` must be a non-empty *named* list of plans.")

  ## Default report = the situation's endogenous targets, readout last.
  spt         <- lavaan::lavaanify(situation, fixed.x = FALSE)
  sit_targets <- unique(spt$lhs[spt$op == "~"])
  if (is.null(report)) report <- sit_targets
  report <- unique(c(setdiff(report, readout), readout))

  miss <- setdiff(report, names(s_0))
  if (length(miss))
    stop("`report` / `readout` node(s) not in `s_0`: ",
         paste(miss, collapse = ", "), ".")

  ## Normalise plans: a plan may be a model string or a labelF()-style
  ## list(model=, params=); merge any plan params into the shared set.
  all_params  <- params
  plan_models <- vector("list", length(plans))
  names(plan_models) <- names(plans)
  for (nm in names(plans)) {
    pl <- plans[[nm]]
    if (is.list(pl)) {
      plan_models[[nm]] <- pl$model
      if (!is.null(pl$params)) all_params <- c(all_params, pl$params)
    } else if (is.character(pl) && length(pl) == 1L) {
      plan_models[[nm]] <- pl
    } else {
      stop("Plan '", nm, "' must be a model string or a list(model=, params=).")
    }
  }
  if (null_plan) plan_models[[null_name]] <- ""    # fires no actions

  ## Simulate each plan to its afforded equilibrium (one sweep settles it).
  vals <- t(vapply(names(plan_models), function(nm) {
    model <- paste(situation, plan_models[[nm]], sep = "\n")
    tr    <- runF(model, all_params, s_0, mode = "sweep",
                  steps = 1L, warn_bounds = FALSE)
    tr[nrow(tr), report]
  }, numeric(length(report))))

  out <- data.frame(plan = names(plan_models), vals,
                    check.names = FALSE, row.names = NULL,
                    stringsAsFactors = FALSE)
  rv  <- out[[readout]]
  out$chosen <- !is.na(rv) & rv >= max(rv, na.rm = TRUE) - tol
  out
}
