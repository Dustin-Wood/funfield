#' Triangle path diagram for one mediator in a moderated X-M-Y model
#'
#' @description
#' Renders the classic X-M-Y mediation triangle with path labels carrying
#' both the baseline coefficient and the Z-moderation slope:
#' \code{b1 + bZ(Z)}. Designed for a single mediator at a time; when the
#' input has multiple mediators, pick one via the \code{mediator} argument.
#'
#' \itemize{
#'   \item \eqn{X \to M}: \eqn{b^1_{MX} + b^Z_{MX}(Z)} — expectation arm
#'   \item \eqn{M \to Y}: \eqn{b^1_{YM} + b^Z_{YM}(Z)} — valuation arm
#'   \item \eqn{X \to Y}: \eqn{b^1_{YX} + b^Z_{YX}(Z)} — direct arm
#' }
#'
#' @param x Either a \code{\link{pathXMY}} or \code{\link{pathXMY_decompose}}
#'   object.
#' @param mediator Character (length 1). Required when \code{x} has more
#'   than one mediator; ignored when only one mediator is present.
#' @param X_label,Y_label,Z_label Display labels for the three constructs.
#'   Default \code{"X"}, \code{"Y"}, \code{"Z"}.
#' @param M_label Display label for the mediator. If \code{NULL} (default),
#'   uses the mediator variable name from the fit.
#' @param digits Integer; decimal places for path coefficients (default 2).
#' @param show_pvalues Logical; append p-values to edge labels.
#' @param highlight Optional. One of \code{"BZ_MX"}, \code{"BZ_YM"},
#'   \code{"BZ_YX"}, or \code{NULL}. If set, draws the corresponding
#'   Z-moderation term in red to emphasize where the moderation routes.
#' @param title Optional plot title.
#' @return Invisibly returns the \code{qgraph} object.
#' @seealso \code{\link{pathXMY}}, \code{\link{pathXMY_decompose}}
#' @export
plotPathXMY <- function(x, mediator = NULL,
                        X_label = "X", Y_label = "Y", Z_label = "Z",
                        M_label = NULL,
                        digits = 2,
                        show_pvalues = FALSE,
                        highlight = NULL,
                        title = NULL) {
  if (!requireNamespace("qgraph", quietly = TRUE))
    stop("plotPathXMY() requires the 'qgraph' package.")

  ## Accept either pathXMY or pathXMY_decompose
  if (!is.null(x$fits) && !is.null(x$fits$full)) {
    tidy <- x$fits$full$tidy
  } else if (!is.null(x$tidy)) {
    tidy <- x$tidy
  } else {
    stop("`x` must be a pathXMY() or pathXMY_decompose() return value.")
  }

  meds <- unique(tidy$mediator)
  meds <- meds[!is.na(meds)]
  if (length(meds) == 0L) stop("No mediator found in the fit.")
  if (length(meds) > 1L && is.null(mediator))
    stop("Multiple mediators in fit; specify `mediator =` to pick one.\n",
         "Available: ", paste(meds, collapse = ", "))
  if (is.null(mediator)) mediator <- meds[1]
  if (!mediator %in% meds)
    stop("Mediator '", mediator, "' not in fit. Available: ",
         paste(meds, collapse = ", "))

  s <- tidy[tidy$mediator == mediator, , drop = FALSE]
  if (is.null(M_label)) M_label <- mediator

  pick <- function(p) {
    r <- s[s$param == p, , drop = FALSE]
    if (nrow(r) == 0L) return(list(est = NA_real_, pvalue = NA_real_))
    list(est = r$est[1], pvalue = r$pvalue[1])
  }

  b1_MX <- pick("B1_MX"); bZ_MX <- pick("BZ_MX")
  b1_YM <- pick("B1_YM"); bZ_YM <- pick("BZ_YM")
  b1_YX <- pick("B1_YX"); bZ_YX <- pick("BZ_YX")

  fmt_term <- function(b1, bZ, which_BZ) {
    base <- if (!is.na(b1$est)) sprintf(paste0("%.", digits, "f"), b1$est) else "?"
    out <- base
    if (!is.na(bZ$est))
      out <- paste0(base, " ",
                    sprintf(paste0("%+.", digits, "f(%s)"), bZ$est, Z_label))
    if (show_pvalues && !is.na(b1$pvalue))
      out <- paste0(out, "\np=", sprintf("%.3f", b1$pvalue))
    out
  }

  lab_MX <- fmt_term(b1_MX, bZ_MX, "BZ_MX")
  lab_YM <- fmt_term(b1_YM, bZ_YM, "BZ_YM")
  lab_YX <- fmt_term(b1_YX, bZ_YX, "BZ_YX")

  ## Three-node layout: X bottom-left, M top-center, Y bottom-right
  node_names  <- c("X", "M", "Y")
  node_labels <- c(X_label, M_label, Y_label)
  layout_mat <- rbind(c(-1.0, -0.6),
                      c( 0.0,  0.8),
                      c( 1.0, -0.6))

  edges <- rbind(c(1, 2),   # X -> M
                 c(2, 3),   # M -> Y
                 c(1, 3))   # X -> Y
  elabels <- c(lab_MX, lab_YM, lab_YX)

  ## Color edges: highlighted Z-term in red, others gray
  edge_cols <- c("grey25","grey25","grey25")
  if (!is.null(highlight)) {
    if (highlight == "BZ_MX") edge_cols[1] <- "red3"
    if (highlight == "BZ_YM") edge_cols[2] <- "red3"
    if (highlight == "BZ_YX") edge_cols[3] <- "red3"
  }

  q <- qgraph::qgraph(
    input          = edges,
    directed       = TRUE,
    layout         = layout_mat,
    labels         = node_labels,
    shape          = "rectangle",
    edge.labels    = elabels,
    edge.label.cex = 0.95,
    edge.color     = edge_cols,
    node.width     = 1.6,
    node.height    = 0.7,
    mar            = c(6, 6, 6, 6),
    title          = title,
    DoNotPlot      = FALSE
  )

  invisible(q)
}
