# Build a Conjunction (logical AND) as a Pairwise Chain

Express a node that is on only when \*\*all\*\* of several states are on
— a logical AND / product of the inputs — as a field fragment. Because
\`lavaan\` syntax allows only \*pairwise\* interactions (\`a:b\`, not
\`a:b:c:...\`), an n-way conjunction is built as a chain of 2-way
products through intermediate derived nodes: \`p1 ~ a:b\`, \`p2 ~
p1:c\`, ..., \`node ~ p\_(n-2):z\`. Every node in the chain is
\*\*derived\*\* (a pure function of the inputs, recomputed each turn),
so the returned \`aux\` names are meant to be handed to
\[simulateF()\]'s \`aux\` argument and seeded at \`0\` in the initial
state.

## Usage

``` r
andF(node, terms, prefix = node)
```

## Arguments

- node:

  Name of the conjunction node (e.g. \`"Prepped"\`).

- terms:

  Character vector of two or more input state names.

- prefix:

  Base name for the intermediate nodes. Default \`node\`, giving
  \`node1\`, \`node2\`, ... Avoid \`\_\` and \`.\` in names: both are
  reserved by the \`fZ_X.Y\` label scheme (the \`\_\` mod/source
  separator and the \`.\` source/target separator), so an underscore in
  a node name breaks label parsing.

## Value

A list with:

- \`model\`:

  The \`lavaan\`-syntax fragment (the chain of \`~\` rows).

- \`aux\`:

  The derived node names — the intermediates plus \`node\` — to pass to
  \[simulateF()\] as \`aux =\` and to seed at \`0\`.

## Details

Each term is a fixed \`1 \*\` product, so the conjunction is \`1\`
exactly when all inputs are \`1\` and \`0\` otherwise (for binary object
states). The chain is order-insensitive — any ordering of \`terms\`
yields the same value — but the intermediate node names follow the given
order. Pass the result's \`aux\` to \[simulateF()\] so the chain
recomputes (rather than latching), and \[evalF()\]'s topological settle
resolves the whole chain within one turn.

## See also

\[simulateF()\] (the \`aux\` argument), \[costF()\], \[labelF()\].

## Examples

``` r
a <- andF("Prepped",
          c("HasWater", "HasGrounds", "HasFilter", "HasPower", "HasPot"))
cat(a$model)
#> Prepped1 ~ 1 * HasWater:HasGrounds
#> Prepped2 ~ 1 * Prepped1:HasFilter
#> Prepped3 ~ 1 * Prepped2:HasPower
#> Prepped ~ 1 * Prepped3:HasPot
a$aux
#> [1] "Prepped1" "Prepped2" "Prepped3" "Prepped" 
```
