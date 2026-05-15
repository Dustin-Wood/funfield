#' Plot a Sequence of Functional Field Time Steps
#'
#' @description Visualize the time-step evolution of a functional field
#' simulation by plotting the field at each specified step. This function
#' replaces the manual pattern of incrementing \code{i} by hand and
#' re-running a \code{qgraph} call after \code{\link{expOutcomesFF}}.
#'
#' @param sitlog Output of \code{\link{expOutcomesFF}}: a list with elements
#'   \code{sit} (situation matrix), \code{dit} (disturbance matrix), and
#'   \code{fit} (list of field states, one per time step).
#' @param fprops Output of \code{\link{prepField}}: a list containing
#'   \code{matLayout}, \code{vnames}, \code{vshapes}, and \code{vsizes}.
#' @param steps Integer vector of time-step indices to plot. Defaults to all
#'   valid steps (those with a non-\code{NULL} \code{$ft} element in
#'   \code{sitlog$fit}).
#' @param titles Character vector of plot titles, one per element of
#'   \code{steps}. Defaults to \code{"Step 1"}, \code{"Step 2"}, etc.
#' @param node_labels Optional named character vector mapping internal node
#'   names to display labels (e.g.
#'   \code{c("s1" = "Ready to make coffee", "make(s2).p" = "Set up coffeemaker")}).
#'   When supplied, labels are drawn \emph{outside} each node (above action
#'   nodes, below state/object nodes) rather than inside. Names not found in
#'   \code{fprops$vnames} are silently ignored; nodes without an entry fall
#'   back to their internal name.
#' @param label.cex Character expansion for external node labels. Default
#'   \code{0.65}. Only used when \code{node_labels} is supplied.
#' @param ... Additional arguments passed through to \code{qgraph::qgraph()}.
#' @return The \code{sitlog} list, invisibly, so the call can be used in a
#'   pipe.
#' @seealso \code{\link{expOutcomesFF}}, \code{\link{prepField}},
#'   \code{\link{pnLevels}}, \code{\link{fieldPolygons}}
#' @export

plotFieldSequence <- function(sitlog, fprops, steps = NULL, titles = NULL,
                               node_labels = NULL, label.cex = 0.65, ...) {

  if (!requireNamespace("qgraph", quietly = TRUE)) {
    stop("Package 'qgraph' is needed for plotFieldSequence(). Please install it.")
  }

  vnames  <- as.vector(fprops$vnames)
  vtype   <- as.vector(fprops$vtype)
  n_nodes <- length(vnames)
  n_fit   <- length(sitlog$fit)

  # Default: every step that has a non-NULL $ft
  if (is.null(steps)) {
    steps <- which(vapply(
      sitlog$fit,
      function(f) !is.null(f) && !is.null(f$ft),
      logical(1)
    ))
  }

  if (is.null(titles)) titles <- paste("Step", seq_along(steps))

  # Resolve display labels -----------------------------------------------
  # When node_labels is supplied it can be:
  #   - a named vector: names map internal vnames to display strings
  #   - an unnamed vector of length n_nodes: used positionally
  use_external <- !is.null(node_labels)
  if (use_external) {
    if (!is.null(names(node_labels))) {
      display_labels <- vnames          # start with internal names as fallback
      matched        <- names(node_labels) %in% vnames
      display_labels[match(names(node_labels)[matched], vnames)] <-
        node_labels[matched]
    } else {
      stopifnot(length(node_labels) == n_nodes)
      display_labels <- node_labels
    }
  } else {
    display_labels <- vnames
  }

  # Build x,y coordinate matrix from matLayout (col = x, -row = y) ------
  ml     <- fprops$matLayout
  coords <- matrix(0, nrow = n_nodes, ncol = 2)
  for (vi in seq_len(n_nodes)) {
    idx <- which(ml == vnames[vi], arr.ind = TRUE)
    if (nrow(idx) > 0) {
      coords[vi, 1] <-  idx[1, 2]
      coords[vi, 2] <- -idx[1, 1]
    }
  }

  fp <- fieldPolygons()

  for (k in seq_along(steps)) {
    i <- steps[k]
    if (i < 1 || i > n_fit)          next
    if (is.null(sitlog$fit[[i]]))     next
    if (is.null(sitlog$fit[[i]]$ft))  next

    ft     <- sitlog$fit[[i]]$ft
    sit_i  <- sitlog$sit[i, ]
    groups <- pnLevels(sit_i)

    # When external labels are used, suppress qgraph's internal labels
    # so there is room for the text added below.
    internal_labels <- if (use_external) rep("", n_nodes) else display_labels

    g <- qgraph::qgraph(
      ft,
      layout      = coords,
      shape       = fprops$vshapes,
      vsize       = fprops$vsizes,
      groups      = groups,
      color       = c("lightblue", "lightyellow"),
      polygonList = fp,
      title       = titles[k],
      labels      = internal_labels,
      ...
    )

    # Add external labels above action nodes (d, a) and below state nodes (o)
    if (use_external) {
      # Compute y-offset as 30% of the smallest nonzero row gap in the
      # normalised layout returned by qgraph.
      y_vals <- sort(unique(round(g$layout[, 2], 8)))
      y_gaps <- diff(y_vals)
      y_step <- if (any(y_gaps > 0)) min(y_gaps[y_gaps > 0]) else 0.4
      y_off  <- y_step * 0.30

      # Action / appraisal nodes: label above; state / object nodes: below
      y_offsets <- ifelse(vtype %in% c("d", "a"), y_off, -y_off)

      graphics::text(
        g$layout[, 1],
        g$layout[, 2] + y_offsets,
        labels = display_labels,
        cex    = label.cex,
        xpd    = NA        # allow drawing outside the plot region
      )
    }
  }

  invisible(sitlog)
}
