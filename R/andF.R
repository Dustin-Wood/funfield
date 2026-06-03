#' Build a Conjunction (logical AND) as a Pairwise Chain
#'
#' @description
#' Express a node that is on only when **all** of several states are on --- a
#' logical AND / product of the inputs --- as a field fragment. Because
#' `lavaan` syntax allows only *pairwise* interactions (`a:b`, not
#' `a:b:c:...`), an n-way conjunction is built as a chain of 2-way products
#' through intermediate derived nodes: `p1 ~ a:b`, `p2 ~ p1:c`, ...,
#' `node ~ p_(n-2):z`. Every node in the chain is **derived** (a pure
#' function of the inputs, recomputed each turn), so the returned `aux` names
#' are meant to be handed to [simulateF()]'s `aux` argument and seeded at `0`
#' in the initial state.
#'
#' @details
#' Each term is a fixed `1 *` product, so the conjunction is `1` exactly when
#' all inputs are `1` and `0` otherwise (for binary object states). The chain
#' is order-insensitive --- any ordering of `terms` yields the same value ---
#' but the intermediate node names follow the given order. Pass the result's
#' `aux` to [simulateF()] so the chain recomputes (rather than latching), and
#' [evalF()]'s topological settle resolves the whole chain within one turn.
#'
#' @param node Name of the conjunction node (e.g. `"Prepped"`).
#' @param terms Character vector of two or more input state names.
#' @param prefix Base name for the intermediate nodes. Default `node`, giving
#'   `node1`, `node2`, ... Avoid `_` and `.` in names: both are reserved by
#'   the `fZ_X.Y` label scheme (the `_` mod/source separator and the `.`
#'   source/target separator), so an underscore in a node name breaks label
#'   parsing.
#'
#' @return A list with:
#'   \describe{
#'     \item{`model`}{The `lavaan`-syntax fragment (the chain of `~` rows).}
#'     \item{`aux`}{The derived node names --- the intermediates plus `node`
#'       --- to pass to [simulateF()] as `aux =` and to seed at `0`.}
#'   }
#' @seealso [simulateF()] (the `aux` argument), [costF()], [labelF()].
#' @examples
#' a <- andF("Prepped",
#'           c("HasWater", "HasGrounds", "HasFilter", "HasPower", "HasPot"))
#' cat(a$model)
#' a$aux
#' @export
andF <- function(node, terms, prefix = node) {
  if (!is.character(node) || length(node) != 1L || !nzchar(node))
    stop("`node` must be a single non-empty name.")
  terms <- as.character(terms)
  n <- length(terms)
  if (n < 2L)
    stop("`terms` must name at least two states.")
  if (n == 2L)
    return(list(model = paste0(node, " ~ 1 * ", terms[1], ":", terms[2]),
                aux   = node))

  inter <- paste0(prefix, seq_len(n - 2L))   # intermediate node names
  chain <- c(inter, node)                    # full left-hand-side sequence
  lines <- character(n - 1L)
  lines[1] <- paste0(chain[1], " ~ 1 * ", terms[1], ":", terms[2])
  for (k in 2:(n - 1L))
    lines[k] <- paste0(chain[k], " ~ 1 * ", chain[k - 1L], ":", terms[k + 1L])

  list(model = paste(lines, collapse = "\n"), aux = chain)
}
