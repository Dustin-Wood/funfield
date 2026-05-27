# Expected Outcomes from Situation Interacting with Functional Field

Propagates a situation vector `s` forward through a functional field
until equilibrium is reached. At each time step, an optional `appraise`
function elaborates the raw situation into derived/summary nodes, then
`ffield` computes the force matrix `ft` from that elaborated situation.
The situation updates as `s_new = s_eff + delta %*% ft`, where
`delta = s_eff - s_eff_prev` is only the *change* in the elaborated
situation since the last step. This ensures each activation fires
exactly once: already-active nodes do not keep generating downstream
force. Iteration stops when `max(|s_new - s|) < tol` (equilibrium).

## Usage

``` r
expOutcomesFF(
  s,
  i = NULL,
  ffield = ff,
  plan = function(ft) ft * 0,
  appraise = NULL,
  tol = 1e-08,
  max_steps = NULL,
  clamp = TRUE
)
```

## Arguments

- s:

  Named numeric vector: situation states at a given time.

- i:

  Node name(s) or index(es) to set to 1 in `s` before the loop (the
  initial action / disturbance). `NULL` means no initial action.

- ffield:

  The functional field function: `function(s, plan)` that returns
  `list(s = s, ft = ft)`. Defaults to `ff` in the calling environment.

- plan:

  Function that injects plan-driven forces into `ft`. Defaults to the
  identity (no plan).

- appraise:

  Optional `function(s) -> s` that recomputes derived or summary nodes
  from the raw situation before each force-propagation step. This is
  where OR-sets, goal-completion nodes, and arbitrary inference
  functions live. `NULL` (default) means no elaboration.

- tol:

  Convergence tolerance: stop when `max(|s_new - s|) < tol`. Default
  `1e-8`.

- max_steps:

  Maximum number of propagation steps (safety cap). Defaults to
  `2 * length(s)`.

- clamp:

  Logical. If `TRUE` (default), clamp all situational levels to
  `[-1, 1]` after each step.

## Value

A named list (`sitlog`) with:

- `sit`:

  Matrix of situation states, one row per time step.

- `fit`:

  List of field snapshots. Each element is `list(s = s_eff, ft = ft)`
  for that step.

- `graphColor`:

  Edge-color matrix for `qgraph`, encoding afforded (black) vs.
  plan-driven (limegreen) forces.
