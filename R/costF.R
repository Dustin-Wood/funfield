#' Build an Action-Cost Force for a Functional Field
#'
#' @description
#' Generates a single regression row that charges a small **cost** to a
#' resource node (by convention `Energy`) for every action --- the idea
#' that *performing an action is never free*. The charge lands on the bare
#' action node, so it is paid whenever the action fires, **whether or not
#' the action accomplishes anything** (pouring an empty pot still costs
#' effort). Pasting the result onto a field and running it through
#' [simulateF()] makes the resource deplete one decrement per action as the
#' process unfolds.
#'
#' @details
#' For each name in `actions` the function emits one term `cost * action`
#' on the cost node, with no gating condition. In a [simulateF()] run each
#' action fires exactly once (one per turn, and an action's affording state
#' is consumed when it fires), so a flat `-0.1` per action accumulates to
#' the expected total --- e.g. four actions deplete `Energy` to `-0.4`. The
#' cost node latches (a **stock**, the default for any non-readout field
#' target), so the charges accumulate into a running tally.
#'
#' The coefficient is written as a fixed value into the returned string, so
#' a subsequent [labelF()] pass captures it into `params` under the
#' generated `fZ_X.Y` label (here the baseline `f1_action.node`, e.g.
#' `f1_TurnOn.Energy`), to be re-tuned later if wanted.
#'
#' @param actions Character vector of action node names to charge.
#' @param node Name of the resource node charged. Default `"Energy"`.
#' @param cost Numeric cost charged per action (typically a small negative
#'   number, a decrement). Recycled if length 1; otherwise must name one
#'   value per action. Default `-0.1`.
#'
#' @return A length-1 character string holding one regression row,
#'   `node ~ cost * action1 + cost * action2 + ...`, ready to `paste()`
#'   onto the situation before a [labelF()] pass.
#'
#' @seealso [labelF()] to label the result, [simulateF()] to run it, and
#'   `vignette("coffee_field_model", package = "funfield")` for a worked
#'   energy-cost example.
#' @examples
#' \dontrun{
#' costF(actions = c("make", "TurnOn", "Pour", "Sip"))
#' #> "Energy ~ -0.1 * make + -0.1 * TurnOn + -0.1 * Pour + -0.1 * Sip"
#'
#' ## Per-action costs:
#' costF(c("make", "Sip"), cost = c(make = -0.05, Sip = -0.2))
#' }
#' @export
costF <- function(actions, node = "Energy", cost = -0.1) {

  if (!is.character(actions) || !length(actions))
    stop("`actions` must be a non-empty character vector.")
  if (!is.numeric(cost) || !length(cost))
    stop("`cost` must be a numeric vector.")
  if (length(cost) != 1L) {
    if (is.null(names(cost)) || !all(actions %in% names(cost)))
      stop("A length-", length(cost), " `cost` must be named for every ",
           "action in `actions`.")
    coefs <- cost[actions]
  } else {
    coefs <- rep(cost, length(actions))
  }

  terms <- paste0(formatC(coefs, format = "g"), " * ", actions)
  paste0(node, " ~ ", paste(terms, collapse = " + "))
}
