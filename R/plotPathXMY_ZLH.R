#' Two-panel low/high-Z field diagrams for a moderated X-M-Y model
#'
#' @description
#' Draws two \code{\link{plotPathXMY}} field diagrams side-by-side, one at
#' a low value of \code{Z} and one at a high value. Each panel shows the
#' field with coefficients \emph{collapsed} at the chosen \code{Z} value
#' (\code{b1 + bZ \times Z_value}), so the moderation becomes visually
#' concrete: edges thicken/thin, nodes shift color, and the implied total
#' effect on Y shifts between panels.
#'
#' @param x A \code{\link{pathXMY}} or \code{\link{pathXMY_decompose}} return.
#' @param mediator,X_label,Y_label,Z_label,M_labels,digits,scale_max,score_intensity_max,...
#'   Passed through to \code{\link{plotPathXMY}}. See there for details.
#' @param Z_levels Numeric length-2 vector of low and high Z values
#'   (default \code{c(-1, 1)}, i.e. one SD below and above the mean for a
#'   z-standardized between-person moderator).
#' @param panel_titles Optional length-2 character vector of panel
#'   titles. If \code{NULL}, panel titles are auto-generated using
#'   \code{Z_label} and the \code{Z_levels} values.
#' @return Invisibly returns \code{NULL}.
#' @seealso \code{\link{plotPathXMY}}
#' @export
plotPathXMY_ZLH <- function(x,
                            mediator = NULL,
                            Z_levels = c(-1, 1),
                            Z_label = "Z",
                            panel_titles = NULL,
                            ...) {
  if (length(Z_levels) != 2L || !is.numeric(Z_levels))
    stop("`Z_levels` must be a numeric vector of length 2.")
  if (is.null(panel_titles)) {
    panel_titles <- sprintf("%s = %+g", Z_label, Z_levels)
  } else if (length(panel_titles) != 2L) {
    stop("`panel_titles` must be a character vector of length 2.")
  }

  op <- graphics::par(mfrow = c(1, 2))
  on.exit(graphics::par(op), add = TRUE)

  for (i in seq_along(Z_levels)) {
    plotPathXMY(x,
                mediator = mediator,
                Z_value  = Z_levels[i],
                Z_label  = Z_label,
                title    = panel_titles[i],
                ...)
  }
  invisible(NULL)
}
