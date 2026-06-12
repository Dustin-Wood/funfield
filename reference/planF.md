# Build a Staircase Plan Object from a Policy with Step Descriptions

Bundles a \[simulateF()\] \*\*policy\*\* with the bookkeeping a
staircase diagram needs to write the plan out in words: an ordered
\*\*step label\*\* per rule (\`a1, a2, ...\`), a human-readable
\*\*description\*\*, and the green ramp \*\*colour\*\* each step takes
as an arrow. The result is a single object that is the source of truth
for the plan — pass it straight to \[simulateF()\], \[chooseF()\], or
\[plotField()\], where it drives both the run and the upper-left
\*\*plan-list\*\* the diagram draws (each step in its arrow's colour,
the active step bolded as the run reaches it).

## Usage

``` r
planF(rules, label = "A", plan_colors = c("#00FF00", "#32CD32", "#41A317"))
```

## Arguments

- rules:

  A \`lavaan\`-syntax policy block: one \`action ~ condition\` row per
  line, each optionally ending in a \`# description\` comment. Blank and
  pure-comment lines are ignored.

- label:

  Single-token plan name shown as "Plan \<label\>" and lower-cased for
  the step prefix (\`"A"\` -\> steps \`a1, a2, ...\`). Default \`"A"\`.

- plan_colors:

  Anchor colours for the per-step ramp, interpolated to one colour per
  step. Default the green action ramp \`c("#00FF00", "#32CD32",
  "#41A317")\` (lime -\> dark lime green), matching \[plotField()\]'s
  \`plan_colors\`.

## Value

A list of class \`"planF"\` with:

- \`policy\`:

  The clean \`lavaan\` policy string (descriptions stripped), for
  \[simulateF()\] / \[chooseF()\] / \[plotField()\].

- \`label\`:

  The plan name (\`"A"\`).

- \`prefix\`:

  The lower-cased step prefix (\`"a"\`).

- \`plan_colors\`:

  The ramp anchors, retained.

- \`steps\`:

  A tidy data frame, one row per rule, with columns \`step\` (\`a1,
  ...\`), \`action\`, \`condition\`, \`desc\`, and \`color\`.

## Details

Each row of \`rules\` is an \`action ~ condition\` policy rule (see
\[simulateF()\]) carrying a trailing \`#\` \*\*description\*\* comment:

      make   ~ choice    # prepare coffee machine
      TurnOn ~ s2        # once prepared, turn on
      Pour   ~ HCoPot    # when pot full, pour over cup
      Sip    ~ HCoCup    # when coffee in pot, take sip

Rules are numbered in the order written — the same order \[plotField()\]
walks the policy to colour the action ramp — so the plan-list and the
arrows line up by construction. Step labels are \`paste0(tolower(label),
seq_along(rules))\`, so \`label = "A"\` gives \`a1, a2, ...\`. Step
colours come from \`colorRampPalette(plan_colors)\`, the identical ramp
\[plotField()\] applies to the plan edges.

The stored \`policy\` is the rules with the \`#\` descriptions stripped,
ready to hand to the engine. (\`lavaan\` itself treats \`#\` as a
comment, so an un-stripped string would also run; \`planF()\` parses the
descriptions out explicitly so the step-to-text mapping stays
order-locked.)

## See also

\[simulateF()\] / \[chooseF()\] (which accept a \`planF\` as the
policy), \[plotField()\] (which draws its plan-list), and
\`vignette("coffee_field_model", package = "funfield")\`.

## Examples

``` r
planA <- planF("
  make   ~ choice    # prepare coffee machine
  TurnOn ~ s2        # once prepared, turn on
  Pour   ~ HCoPot    # when pot full, pour over cup
  Sip    ~ HCoCup    # when coffee in pot, take sip
", label = "A")
planA
#> Plan A
#>  step rule          description                 
#>  a1   make ~ choice prepare coffee machine      
#>  a2   TurnOn ~ s2   once prepared, turn on      
#>  a3   Pour ~ HCoPot when pot full, pour over cup
#>  a4   Sip ~ HCoCup  when coffee in pot, take sip
planA$policy        # the clean string for simulateF()
#> [1] "make ~ choice\nTurnOn ~ s2\nPour ~ HCoPot\nSip ~ HCoCup"
```
