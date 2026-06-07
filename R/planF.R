#' Build a Staircase Plan Object from a Policy with Step Descriptions
#'
#' @description
#' Bundles a [simulateF()] **policy** with the bookkeeping a staircase diagram
#' needs to write the plan out in words: an ordered **step label** per rule
#' (`a1, a2, ...`), a human-readable **description**, and the green ramp
#' **colour** each step takes as an arrow. The result is a single object that
#' is the source of truth for the plan --- pass it straight to [simulateF()],
#' [chooseF()], or [plotField()], where it drives both the run and the
#' upper-left **plan-list** the diagram draws (each step in its arrow's colour,
#' the active step bolded as the run reaches it).
#'
#' @details
#' Each row of `rules` is an `action ~ condition` policy rule (see
#' [simulateF()]) carrying a trailing `#` **description** comment:
#'
#' \preformatted{  make   ~ choice    # prepare coffee machine
#'   TurnOn ~ s2        # once prepared, turn on
#'   Pour   ~ HCoPot    # when pot full, pour over cup
#'   Sip    ~ HCoCup    # when coffee in pot, take sip}
#'
#' Rules are numbered in the order written --- the same order [plotField()]
#' walks the policy to colour the action ramp --- so the plan-list and the
#' arrows line up by construction. Step labels are `paste0(tolower(label),
#' seq_along(rules))`, so `label = "A"` gives `a1, a2, ...`. Step colours come
#' from `colorRampPalette(plan_colors)`, the identical ramp [plotField()]
#' applies to the plan edges.
#'
#' The stored `policy` is the rules with the `#` descriptions stripped, ready
#' to hand to the engine. (`lavaan` itself treats `#` as a comment, so an
#' un-stripped string would also run; `planF()` parses the descriptions out
#' explicitly so the step-to-text mapping stays order-locked.)
#'
#' @param rules A `lavaan`-syntax policy block: one `action ~ condition` row
#'   per line, each optionally ending in a `# description` comment. Blank and
#'   pure-comment lines are ignored.
#' @param label Single-token plan name shown as "Plan <label>" and lower-cased
#'   for the step prefix (`"A"` -> steps `a1, a2, ...`). Default `"A"`.
#' @param plan_colors Anchor colours for the per-step ramp, interpolated to one
#'   colour per step. Default the green action ramp
#'   `c("#00FF00", "#32CD32", "#41A317")` (lime -> dark lime green), matching
#'   [plotField()]'s `plan_colors`.
#'
#' @return A list of class `"planF"` with:
#'   \describe{
#'     \item{`policy`}{The clean `lavaan` policy string (descriptions
#'       stripped), for [simulateF()] / [chooseF()] / [plotField()].}
#'     \item{`label`}{The plan name (`"A"`).}
#'     \item{`prefix`}{The lower-cased step prefix (`"a"`).}
#'     \item{`plan_colors`}{The ramp anchors, retained.}
#'     \item{`steps`}{A tidy data frame, one row per rule, with columns
#'       `step` (`a1, ...`), `action`, `condition`, `desc`, and `color`.}
#'   }
#'
#' @seealso [simulateF()] / [chooseF()] (which accept a `planF` as the policy),
#'   [plotField()] (which draws its plan-list), and
#'   `vignette("coffee_field_model", package = "funfield")`.
#' @examples
#' planA <- planF("
#'   make   ~ choice    # prepare coffee machine
#'   TurnOn ~ s2        # once prepared, turn on
#'   Pour   ~ HCoPot    # when pot full, pour over cup
#'   Sip    ~ HCoCup    # when coffee in pot, take sip
#' ", label = "A")
#' planA
#' planA$policy        # the clean string for simulateF()
#' @export
planF <- function(rules, label = "A",
                  plan_colors = c("#00FF00", "#32CD32", "#41A317")) {

  if (!is.character(rules) || length(rules) != 1L || !nzchar(trimws(rules)))
    stop("`rules` must be a single non-empty policy string.")
  if (!is.character(label) || length(label) != 1L || !nzchar(label))
    stop("`label` must be a single non-empty plan name.")

  ## Split into lines; for each, peel off the trailing `# description`, keep
  ## the `action ~ condition` rule-part, and drop blank / pure-comment lines.
  lines <- strsplit(rules, "\n", fixed = TRUE)[[1]]
  rows  <- list()
  for (ln in lines) {
    hash <- regexpr("#", ln, fixed = TRUE)
    desc <- if (hash > 0L) trimws(substring(ln, hash + 1L)) else NA_character_
    rule <- trimws(if (hash > 0L) substring(ln, 1L, hash - 1L) else ln)
    if (!nzchar(rule)) next                       # blank or pure-comment line
    if (!grepl("~", rule, fixed = TRUE))
      stop("Policy rule has no `~`: '", rule, "'.")
    parts  <- strsplit(rule, "~", fixed = TRUE)[[1]]
    action <- trimws(parts[1])
    cond   <- trimws(paste(parts[-1], collapse = "~"))
    rows[[length(rows) + 1L]] <-
      data.frame(action = action, condition = cond, desc = desc,
                 stringsAsFactors = FALSE)
  }
  if (!length(rows))
    stop("`rules` contains no `action ~ condition` rows.")
  steps <- do.call(rbind, rows)

  n      <- nrow(steps)
  prefix <- tolower(label)
  steps$step  <- paste0(prefix, seq_len(n))
  steps$color <- grDevices::colorRampPalette(plan_colors)(n)
  steps <- steps[c("step", "action", "condition", "desc", "color")]

  policy <- paste(paste(steps$action, "~", steps$condition), collapse = "\n")

  structure(list(policy      = policy,
                 label       = label,
                 prefix      = prefix,
                 plan_colors = plan_colors,
                 steps       = steps),
            class = "planF")
}

#' @export
#' @method print planF
print.planF <- function(x, ...) {
  cat("Plan ", x$label, "\n", sep = "")
  tab <- data.frame(step = x$steps$step,
                    rule = paste(x$steps$action, "~", x$steps$condition),
                    description = ifelse(is.na(x$steps$desc), "",
                                         x$steps$desc),
                    stringsAsFactors = FALSE)
  print(tab, row.names = FALSE, right = FALSE)
  invisible(x)
}
