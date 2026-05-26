#' Two-panel expectation vs valuation route diagrams
#'
#' @description
#' Returns a \pkg{patchwork} composition of two
#' \code{\link{plotPathXMY}} panels, one per moderation route. The
#' \strong{expectation} panel shows \code{BZ_MX} on the X-to-M arm
#' (limegreen) and \code{B1_YM} on the M-to-Y arm (black); the
#' \strong{valuation} panel shows the converse pairing.
#'
#' Reading rule: a mediator whose two arms BOTH carry visible weight in
#' the same panel identifies a \emph{reason} that the moderator shifts
#' the X-to-Y relationship. A green-black chain in the expectation
#' panel says the moderator shifts what the actor expects from the
#' action AND those expectations matter for action. A black-green chain
#' in the valuation panel says the moderator shifts how those
#' expectations are weighted while leaving the expectations themselves
#' unchanged. The indirect-product columns of
#' \code{\link{pathXMY_pairtable}} are the formal counterpart.
#'
#' @param x A \code{\link{pathXMY}} or \code{\link{pathXMY_decompose}} return.
#' @param mediator,X_label,Y_label,Z_label,M_labels,X_shape,Y_shape,digits,scale_max,score_intensity_max,...
#'   Passed through to \code{\link{plotPathXMY}}. See there for details.
#' @param panel_titles Optional length-2 character vector of panel
#'   titles. If \code{NULL}, panel titles are auto-generated from
#'   \code{Z_label}.
#' @return A \code{patchwork} object combining the two \code{ggplot}
#'   panels. Auto-prints in interactive sessions and knitr chunks.
#' @seealso \code{\link{plotPathXMY}}, \code{\link{plotPathXMY_ZLH}},
#'   \code{\link{plotPathXMY_widget_routes}}
#' @export
plotPathXMY_routes <- function(x,
                               mediator = NULL,
                               Z_label  = "Z",
                               panel_titles = NULL,
                               ...) {
  if (is.null(panel_titles)) {
    panel_titles <- c(
      sprintf("Expectation route (BZ_MX(%s), B1_YM)", Z_label),
      sprintf("Valuation route (B1_MX, BZ_YM(%s))",   Z_label)
    )
  } else if (length(panel_titles) != 2L) {
    stop("`panel_titles` must be a character vector of length 2.")
  }
  if (!requireNamespace("patchwork", quietly = TRUE))
    stop("plotPathXMY_routes() requires the 'patchwork' package.")

  p_exp <- plotPathXMY(x, mediator = mediator,
                       Z_label  = Z_label,
                       route    = "expectation",
                       title    = panel_titles[1], ...)
  p_val <- plotPathXMY(x, mediator = mediator,
                       Z_label  = Z_label,
                       route    = "valuation",
                       title    = panel_titles[2], ...)

  patchwork::wrap_plots(p_exp, p_val, ncol = 2)
}


#' Toggleable widget for expectation vs valuation route diagrams
#'
#' @description Renders the two \code{\link{plotPathXMY_routes}} panels
#'   as a self-contained HTML widget with \strong{Back} and
#'   \strong{Forward} buttons that swap them in place at pixel-identical
#'   layout, so the green arm slides from left (expectation) to right
#'   (valuation) and the eye picks up which mediator carries which
#'   route. Sibling of \code{\link{plotPathXMY_widget}}; both share the
#'   same toggle plumbing.
#'
#' @param x A \code{\link{pathXMY}} or \code{\link{pathXMY_decompose}} return.
#' @param mediator,X_label,Y_label,Z_label,M_labels,X_shape,Y_shape,digits,scale_max,score_intensity_max,...
#'   Passed through to \code{\link{plotPathXMY}}.
#' @param panel_titles Optional length-2 character vector. If
#'   \code{NULL}, titles are auto-generated from \code{Z_label}.
#' @param width,height Frame width and height in inches. \code{height}
#'   defaults to \code{3.5} for the single-mediator triangle view and to
#'   \code{3 + 0.5 * n_mediators} for the fan view.
#' @param res PNG resolution in pixels per inch. Default \code{192}.
#'   Ignored when \code{format = "svg"}.
#' @param format One of \code{"png"} (default) or \code{"svg"}. SVG is
#'   recommended for HTML vignette output.
#' @return An \code{htmltools::tagList} object that auto-renders as an
#'   interactive HTML widget in knitr / R Markdown.
#' @seealso \code{\link{plotPathXMY_routes}}, \code{\link{plotPathXMY_widget}}
#' @export
plotPathXMY_widget_routes <- function(x,
                                      mediator = NULL,
                                      Z_label = "Z",
                                      panel_titles = NULL,
                                      width = 9,
                                      height = NULL,
                                      res = 192,
                                      format = c("png", "svg"),
                                      ...) {
  format <- match.arg(format)
  if (!requireNamespace("base64enc", quietly = TRUE))
    stop("Package 'base64enc' is needed for plotPathXMY_widget_routes(). Please install it.")
  if (!requireNamespace("htmltools", quietly = TRUE))
    stop("Package 'htmltools' is needed for plotPathXMY_widget_routes(). Please install it.")

  ## Default height: compact for single-mediator triangle, scales with
  ## n of mediators for the fan view. Matches plotPathXMY_widget().
  tidy <- if (!is.null(x$fits) && !is.null(x$fits$full)) x$fits$full$tidy_loop
          else if (!is.null(x$tidy_loop))                 x$tidy_loop
          else stop("`x` must be a pathXMY() or pathXMY_decompose() return.")
  meds_in_fit <- unique(tidy$mediator)
  meds_in_fit <- meds_in_fit[!is.na(meds_in_fit)]
  n_m <- if (is.null(mediator)) length(meds_in_fit) else length(mediator)
  if (is.null(height)) height <- if (n_m == 1L) 3.5 else 3 + 0.5 * n_m

  if (is.null(panel_titles)) {
    panel_titles <- c(
      sprintf("Expectation route (BZ_MX(%s), B1_YM)", Z_label),
      sprintf("Valuation route (B1_MX, BZ_YM(%s))",   Z_label)
    )
  } else if (length(panel_titles) != 2L) {
    stop("`panel_titles` must be a character vector of length 2.")
  }

  plots <- list(
    plotPathXMY(x, mediator = mediator,
                Z_label = Z_label, route = "expectation",
                title   = panel_titles[1], ...),
    plotPathXMY(x, mediator = mediator,
                Z_label = Z_label, route = "valuation",
                title   = panel_titles[2], ...)
  )

  pxmy_widget_html(plots, panel_titles, width, height, res, format)
}
