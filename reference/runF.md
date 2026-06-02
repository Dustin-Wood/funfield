# Iterate a Functional Field Forward

Drives the Markov iteration \`s\_(t+1) = s_t number of steps, optionally
applying scheduled action perturbations to the state at the start of
each step. Returns the full trajectory of states.

## Usage

``` r
runF(model, params, s_0, actions = list(), steps = 4L, clamp = TRUE)
```

## Arguments

- model:

  A \`lavaan\`-syntax model string with \`fZ_X.Y\`-labelled parameters.

- params:

  Named numeric vector mapping parameter labels to values.

- s_0:

  Named numeric vector of initial state values.

- actions:

  Optional list of action schedules. Names are step numbers (character);
  values are character or named numeric vectors. See Details. Default
  empty list (no scheduled actions; perturb \`s_0\` directly instead).

- steps:

  Integer number of propagation steps. Default 4 (matches the four-stage
  coffee example).

- clamp:

  Logical. When \`TRUE\` (default), clamp state values to \`\[-1, 1\]\`
  after each propagation step.

## Value

A numeric matrix with \`steps + 1\` rows (indexed \`t=0\` through
\`t=steps\`) and one column per state variable. Row \`t=0\` is \`s_0\`
\*before\* any actions are applied; subsequent rows are post-action,
post-propagation states.

## Details

At each step \`t\` in \`1:steps\`, the function: 1. Applies any actions
scheduled for step \`t\` — each named state variable is set to its
scheduled value (default \`1\`) \*before\* propagation. 2. Calls
\[evalF()\] to compute \`s\_(t+1)\` from the post-action \`s_t\`. 3.
Optionally clamps the result to \`\[-1, 1\]\`.

The \`actions\` argument is a list whose names are step numbers (as
character strings, e.g. \`"1"\`, \`"2"\`) and whose values are either:
\* a character vector of state-variable names to set to \`1\`, or \* a
named numeric vector specifying the value to set each variable to.

Variables that never appear on the LHS of a \`~\` in \`model\`
(exogenous nodes, typically actions or persistent context) are carried
forward by \`evalF()\` unchanged — once an action is set to \`1\` it
stays at \`1\` for the rest of the run unless clamping or a later
equation overwrites it.

## See also

\[evalF()\] for the single-step propagator.
