#' Expected Outcomes from Situation Interacting with Functional Field
#'
#' @description Propagates a situation vector \code{s} forward through a
#'   functional field until equilibrium is reached. At each time step, an
#'   optional \code{appraise} function elaborates the raw situation into
#'   derived/summary nodes, then \code{ffield} computes the force matrix
#'   \code{ft} from that elaborated situation.  The situation updates as
#'   \code{s_new = s_eff + delta \%*\% ft}, where \code{delta = s_eff - s_eff_prev}
#'   is only the \emph{change} in the elaborated situation since the last step.
#'   This ensures each activation fires exactly once: already-active nodes do
#'   not keep generating downstream force.  Iteration stops when
#'   \code{max(|s_new - s|) < tol} (equilibrium).
#'
#' @param s Named numeric vector: situation states at a given time.
#' @param i Node name(s) or index(es) to set to 1 in \code{s} before the loop
#'   (the initial action / disturbance). \code{NULL} means no initial action.
#' @param ffield The functional field function: \code{function(s, plan)} that
#'   returns \code{list(s = s, ft = ft)}.  Defaults to \code{ff} in the
#'   calling environment.
#' @param plan Function that injects plan-driven forces into \code{ft}.
#'   Defaults to the identity (no plan).
#' @param appraise Optional \code{function(s) -> s} that recomputes derived or
#'   summary nodes from the raw situation before each force-propagation step.
#'   This is where OR-sets, goal-completion nodes, and arbitrary inference
#'   functions live.  \code{NULL} (default) means no elaboration.
#' @param tol Convergence tolerance: stop when \code{max(|s_new - s|) < tol}.
#'   Default \code{1e-8}.
#' @param max_steps Maximum number of propagation steps (safety cap).
#'   Defaults to \code{2 * length(s)}.
#' @param clamp Logical.  If \code{TRUE} (default), clamp all situational
#'   levels to \code{[-1, 1]} after each step.
#'
#' @return A named list (\code{sitlog}) with:
#'   \describe{
#'     \item{\code{sit}}{Matrix of situation states, one row per time step.}
#'     \item{\code{fit}}{List of field snapshots.  Each element is
#'       \code{list(s = s_eff, ft = ft)} for that step.}
#'     \item{\code{graphColor}}{Edge-color matrix for \code{qgraph}, encoding
#'       afforded (black) vs. plan-driven (limegreen) forces.}
#'   }
#'
#' @export
expOutcomesFF <- function(s, i = NULL, ffield = ff, plan = function(ft) ft * 0,
                           appraise = NULL, tol = 1e-8,
                           max_steps = NULL, clamp = TRUE) {

  # --- Initial action -------------------------------------------------------
  if (!is.null(i)) s[i] <- s[i] + 1
  if (clamp) { s[s >  1] <-  1; s[s < -1] <- -1 }

  if (is.null(max_steps)) max_steps <- 2L * length(s)

  # --- Storage --------------------------------------------------------------
  sit <- matrix(NA_real_, max_steps + 1L, length(s))
  colnames(sit) <- names(s)
  fit <- vector("list", max_steps)
  sit[1L, ] <- s

  # --- Propagation loop -----------------------------------------------------
  # s_eff_prev tracks the elaborated situation from the previous step so that
  # only the *delta* (newly activated nodes) drives downstream forces.
  s_eff_prev <- setNames(rep(0, length(s)), names(s))

  n_steps <- 0L
  for (t in seq_len(max_steps)) {

    s_eff    <- if (!is.null(appraise)) appraise(sit[t, ]) else sit[t, ]
    fft      <- ffield(s_eff, plan = plan)
    ft       <- fft$ft
    fit[[t]] <- list(s = s_eff, ft = ft)

    # Propagate only what is *new* at this step — prevents repeated activation
    delta    <- s_eff - s_eff_prev
    s_new    <- s_eff + as.vector(delta %*% ft)
    names(s_new) <- names(s_eff)
    if (clamp) { s_new[s_new >  1] <-  1; s_new[s_new < -1] <- -1 }
    sit[t + 1L, ] <- s_new
    n_steps    <- t
    s_eff_prev <- s_eff

    if (max(abs(s_new - sit[t, ])) < tol) break
  }

  # --- Trim to actual length ------------------------------------------------
  sit <- sit[seq_len(n_steps + 1L), , drop = FALSE]
  fit <- fit[seq_len(n_steps)]

  sitlog <- list(sit = sit, fit = fit)

  # --- graphColor (based on final field) ------------------------------------
  qF       <- fit[[n_steps]]$ft
  present  <- qF; present[present == 0] <- NA; present[present != 0] <- "afforded"
  qFactors <- surgery(qF, present)
  plani    <- qF * 0; plani <- plan(plani); plani[plani == 1] <- "plani"
  qFactors <- surgery(qFactors, plani)
  graphColor <- qFactors
  graphColor[graphColor == "potential"] <- "gray91"
  graphColor[graphColor == "afforded"]  <- "black"
  graphColor[graphColor == "plani"]     <- "limegreen"
  sitlog$graphColor <- graphColor

  return(sitlog)
}
