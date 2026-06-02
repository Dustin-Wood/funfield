# Evaluate One Step of a Functional Field

Takes a \`lavaan\`-syntax model string \`F\` with \`fZ_X.Y\`-labelled
parameters, a vector of parameter values, and a current state \`s_t\`,
and returns the next state \`s\_(t+1)\` produced by one step of the
field dynamics. Two propagation modes are available — a \*\*topological
sweep\*\* (the default) that resolves the whole causal chain in one
step, and a \*\*synchronous\*\* update that advances the state one edge
at a time — and individual nodes can be marked as latching
\*\*stocks\*\* to give the field memory.

## Usage

``` r
evalF(model, params, s_t, stocks = character(0), mode = c("sweep", "sync"))
```

## Arguments

- model:

  A \`lavaan\`-syntax model string. Only regression rows (\`~\`) are
  interpreted; other operators are ignored.

- params:

  Named numeric vector mapping parameter labels (e.g. \`"f1_X.Y"\`,
  \`"fs2_TurnOn.HCoPot"\`) to their values.

- s_t:

  Named numeric vector of state values at time \`t\`.

- stocks:

  Character vector of node names that \*\*latch\*\* (carry their value
  forward). Names not on a left-hand side are ignored. Default
  \`character(0)\` — every node recomputes (memoryless), matching the
  field's afforded-equilibrium behaviour.

- mode:

  Propagation mode: \`"sweep"\` (default, topological, whole-chain) or
  \`"sync"\` (one edge per step). See Details.

## Value

Named numeric vector \`s\_(t+1)\` of the same length and names as
\`s_t\`.

## Details

Each regression row in \`model\` of the form

\`Y ~ label_1 \* rhs_1 + label_2 \* rhs_2 + ...\`

contributes \`params\[label_k\] \* prod(s\[vars(rhs_k)\])\` to the new
value of \`Y\`, where \`rhs_k\` is a single state variable or an
interaction term \`X:Z:...\` parsed as the product of the named state
variables. A \*\*labelled\*\* term (\`fZ_X.Y \* X\`) takes its value
from \`params\`; a \*\*fixed\*\* term (\`1 \* X\`, lavaan's
fixed-coefficient syntax) takes its value from the coefficient itself. A
term that is neither labelled nor fixed cannot be valued and raises an
error, as do a label missing from \`params\` or a variable missing from
\`s_t\`.

\*\*Propagation mode.\*\*

- \`"sweep"\` (default):

  Endogenous nodes are evaluated in topological (causal) order, each
  reading the \*freshly updated\* values of its inputs, so an entire
  causal chain settles in a single call. A gated force \`Y ~ Z:X\`
  resolves correctly because the moderator \`Z\` is treated as a
  dependency and is updated before \`Y\`. Requires the field to be a
  DAG; a feedback cycle raises an error (use \`"sync"\` instead).

- \`"sync"\`:

  Every right-hand side is read from \`s_t\` (the state at the \*start\*
  of the call), so the signal advances exactly one edge per call. This
  is the natural mode for cyclic / dynamical fields — in particular
  stock-and-flow consumption pipelines, which are inherently cyclic and
  cannot sweep.

\*\*Stocks vs. flows.\*\* By default every endogenous node is recomputed
from scratch each step (a \*flow\*: its value is a pure function of the
current state, so it does not persist on its own). A node named in
\`stocks\` instead \*latches\*: it carries its previous value forward
and the model's terms add to or subtract from it, i.e. \`s\_(t+1)\[Y\] =
s_t\[Y\] + sum_k coef_k \* prod(...)\`. Stocks are how a field remembers
— an object state stays on once attained, and a negative (consumption)
term such as \`c \* Y:trigger\` draws it back down. lavaan forbids the
self-edge \`Y ~ Y\`, so the carry-forward cannot be written in the model
string; marking \`Y\` a stock supplies it. See \[runF()\] to have the
stock set derived automatically from the action nodes, and
\`vignette("coffee_field_model", package = "funfield")\` for a worked
stock/flow example.

Nodes that never appear on the left of \`~\` (exogenous nodes, typically
persistent context) are always carried forward unchanged, which is
itself stock-like behaviour and needs no declaration.

## See also

\[runF()\] for the iteration driver (which derives \`stocks\` from the
action set), \[labelF()\] for building the model string.
