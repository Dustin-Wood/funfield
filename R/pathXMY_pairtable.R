#' Side-by-Side Table of Two (or More) pathXMY Parameters
#'
#' @description
#' Reshapes a \code{\link{pathXMY}} tidy table from long form (one row per
#' mediator x parameter) into a wide form that places two or more named
#' parameters next to each other, keyed by mediator. This is the natural
#' layout for reading an expectation path beside its companion valuation
#' path --- \code{B1_MX} next to \code{B1_YM} for the normative model, or
#' the two moderation arms \code{BZ_MX} next to \code{BZ_YM} --- in a single
#' table, without the \code{mediator} column repeating.
#'
#' @param x A \code{pathXMY} object (the list returned by \code{pathXMY()}),
#'   or a tidy data frame carrying \code{mediator}, \code{param}, and the
#'   requested \code{cols} (e.g. \code{x$tidy_loop}).
#' @param params Character vector (length >= 2) of parameter labels to place
#'   side by side, in display order. Labels follow the XMY convention used by
#'   \code{pathXMY()}: e.g. \code{"B1_MX"}, \code{"B1_YM"}, \code{"BZ_MX"},
#'   \code{"BZ_YM"}.
#' @param from One of \code{"loop"} (default) or \code{"joint"} --- which
#'   tidy table to draw from when \code{x} is a \code{pathXMY} object.
#'   Ignored when \code{x} is already a data frame.
#' @param cols Character vector of statistic columns carried for each
#'   parameter (default \code{c("est", "se", "z", "pvalue")}).
#' @param mediators Optional character vector giving an explicit row order.
#'   When \code{NULL} (default), mediators appear in the order they occur
#'   for the first parameter.
#'
#' @return A data frame with one row per mediator: a leading \code{mediator}
#'   column followed by one block of \code{cols} per parameter. Block columns
#'   are named \code{<param>.<col>} (runs of non-alphanumeric characters in
#'   \code{param} collapsed to \code{"_"}), so \code{c("B1_MX", "B1_YM")}
#'   yields \code{B1_MX.est}, \code{B1_MX.se}, \code{...}, \code{B1_YM.est},
#'   \code{B1_YM.se}, \code{...}. Rows with a missing (\code{NA}) mediator
#'   --- the global \code{B*_YX} direct paths --- are dropped, since the
#'   table is keyed by mediator.
#'
#' @seealso \code{\link{pathXMY}}, \code{\link{pathXMY_decompose}}
#'
#' @examples
#' \dontrun{
#' data(speedingESJT)
#' mediators <- c("Crash","Injured","Ticket","MoneyCost","OnTime",
#'                "IntQuality","FunDrive","Appropriate")
#' res <- pathXMY(speedingESJT$PSI, X = "Speed", Y = "Likelihood",
#'                M = mediators)
#' ## expectation paths beside valuation paths
#' pathXMY_pairtable(res, c("B1_MX", "B1_YM"))
#'
#' ## the two moderation arms beside each other (model fit with a Z)
#' modp <- pathXMY(speedingESJT$PSI, X = "Speed", Y = "Likelihood",
#'                 M = mediators, Z = "someTrait", Z.within = FALSE)
#' pathXMY_pairtable(modp, c("BZ_MX", "BZ_YM"))
#' }
#'
#' @export
pathXMY_pairtable <- function(x, params,
                              from = c("loop", "joint"),
                              cols = c("est", "se", "z", "pvalue"),
                              mediators = NULL) {
  from <- match.arg(from)
  stopifnot(is.character(params), length(params) >= 2,
            is.character(cols), length(cols) >= 1,
            is.null(mediators) || is.character(mediators))

  ## resolve the tidy table
  if (inherits(x, "pathXMY")) {
    tab <- if (from == "joint") x$tidy_joint else x$tidy_loop
    if (is.null(tab)) {
      stop("No '", from, "' tidy table on this pathXMY object",
           if (from == "joint")
             " (the joint fit requires length(M) > 1 and joint = TRUE)."
           else ".", call. = FALSE)
    }
  } else if (is.data.frame(x)) {
    tab <- x
  } else {
    stop("'x' must be a pathXMY object or a tidy data frame.",
         call. = FALSE)
  }

  miss <- setdiff(c("mediator", "param", cols), colnames(tab))
  if (length(miss) > 0) {
    stop("Tidy table is missing column(s): ",
         paste(miss, collapse = ", "), call. = FALSE)
  }

  bad <- setdiff(params, unique(tab$param))
  if (length(bad) > 0) {
    stop("Parameter(s) not found in the tidy table: ",
         paste(bad, collapse = ", "),
         ".\n  Available: ", paste(unique(tab$param), collapse = ", "),
         call. = FALSE)
  }

  ## one block of statistic columns per parameter, keyed by mediator
  block <- function(p) {
    sub <- tab[tab$param == p & !is.na(tab$mediator),
               c("mediator", cols), drop = FALSE]
    pre <- gsub("[^A-Za-z0-9]+", "_", p)
    names(sub)[-1] <- paste0(pre, ".", cols)
    sub
  }
  blocks <- lapply(params, block)

  out <- blocks[[1]]
  for (k in 2:length(blocks)) {
    out <- merge(out, blocks[[k]], by = "mediator",
                 all = TRUE, sort = FALSE)
  }

  ## row order: explicit, else the order seen for the first parameter
  ord <- if (!is.null(mediators)) mediators else blocks[[1]]$mediator
  out <- out[order(match(out$mediator, ord)), , drop = FALSE]
  rownames(out) <- NULL
  out
}
