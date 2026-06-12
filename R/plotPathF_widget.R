#' Back/Forward widget stepping through the views of a moderated pathF field
#'
#' @description
#' Renders a moderated \code{\link{pathF}} model as a navigable HTML
#' widget whose frames are the \code{\link{plotPathF}} view modes at
#' pixel-identical layout: by default the \strong{decomposed} field
#' (f1 labels with their gold fZ companions), the \strong{normative}
#' field (the same f1 paths with the moderation hidden), and the
#' \strong{moderation} field (every edge swapped for its fZ
#' counterpart, all gold). Clicking \strong{Forward} therefore peels
#' the one diagram apart into its black and gold layers in place.
#'
#' This is a thin convenience over \code{\link{plotsAsWidget}}; for
#' custom frame sequences (e.g. mixing \code{Z_value} collapses with
#' view modes), build the frames with \code{plotPathF()} directly and
#' stitch them yourself.
#'
#' @param x A \code{\link{pathF}} or \code{\link{pathF_decompose}}
#'   return; the fit must include a moderator Z.
#' @param views Character vector of \code{plotPathF()} view modes, one
#'   per frame, in display order. Default
#'   \code{c("decomposed", "normative", "moderation")}.
#' @param title Optional base title. Frames after the first are
#'   suffixed \code{"[normative effects]"} / \code{"[moderation
#'   effects]"} per their view; the decomposed frame carries the bare
#'   title.
#' @param panel_titles Optional character vector overriding the
#'   auto-built frame captions, parallel to \code{views}.
#' @param scale_max Path-magnitude cap for the f1-scaled frames
#'   (decomposed, normative); default \code{0.8}.
#' @param scale_max_moderation Cap for the moderation frame, where fZ
#'   magnitudes run smaller. Default \code{NULL} uses \code{scale_max}
#'   (edge thicknesses then compare honestly across frames); pass a
#'   tighter value such as \code{0.3} to make the gold field legible
#'   on its own terms.
#' @param width,height Frame size in inches (default \code{9} x
#'   \code{4.5}).
#' @param res PNG resolution in pixels per inch (ignored for SVG).
#' @param format One of \code{"png"} (default) or \code{"svg"}.
#' @param ... Additional arguments passed to every
#'   \code{\link{plotPathF}} call (e.g. \code{Z_label},
#'   \code{labels}, \code{show_pvalues}).
#'
#' @return An \code{htmltools::tagList} that auto-renders as an
#'   interactive widget in knitr / R Markdown HTML output; fully
#'   self-contained (images embedded as data URIs).
#' @seealso \code{\link{plotPathF}}, \code{\link{plotsAsWidget}},
#'   \code{\link{plotPathXMY_widget}}
#' @export
plotPathF_widget <- function(x,
                             views = c("decomposed", "normative",
                                       "moderation"),
                             title = NULL,
                             panel_titles = NULL,
                             scale_max = 0.8,
                             scale_max_moderation = NULL,
                             width = 9,
                             height = 4.5,
                             res = 192,
                             format = c("png", "svg"),
                             ...) {
  format <- match.arg(format)
  ok_views <- c("decomposed", "normative", "moderation")
  if (length(views) < 1L || !all(views %in% ok_views))
    stop("`views` must be drawn from: ",
         paste(ok_views, collapse = ", "))

  suffix <- c(decomposed = "",
              normative  = " [normative effects]",
              moderation = " [moderation effects]")
  if (is.null(panel_titles)) {
    panel_titles <- if (is.null(title)) {
      c(decomposed = "Decomposed field (F1 + FZ)",
        normative  = "Normative effects (F1 paths)",
        moderation = "Moderation effects (FZ paths)")[views]
    } else {
      paste0(title, suffix[views])
    }
    panel_titles <- unname(panel_titles)
  } else if (length(panel_titles) != length(views)) {
    stop("`panel_titles` must have one entry per view.")
  }

  plots <- lapply(seq_along(views), function(k) {
    v <- views[k]
    sm <- if (v == "moderation" && !is.null(scale_max_moderation))
      scale_max_moderation else scale_max
    plotPathF(x, view = v, scale_max = sm,
              title = panel_titles[k], ...)
  })
  pxmy_widget_html(plots, panel_titles, width, height, res, format)
}
