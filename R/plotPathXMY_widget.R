#' Interactive toggleable widget for an X-M-Y field across Z levels
#'
#' @description Renders \code{\link{plotPathXMY}} once per requested
#'   \code{Z_value}, embeds the resulting PNGs as base-64 data URIs, and
#'   wraps them in a self-contained HTML widget with \strong{Back} and
#'   \strong{Forward} navigation buttons. Each click swaps the field at
#'   pixel-identical layout, so edges thicken / thin and node colors
#'   transition between Z levels in place — making the moderation
#'   visually striking compared to a side-by-side static view.
#'
#'   Suitable for knitr/R-Markdown HTML output, RStudio's Viewer pane,
#'   and Shiny apps. No server is required: all images are embedded as
#'   data URIs, so the HTML file is fully self-contained.
#'
#' @param x A \code{\link{pathXMY}} or \code{\link{pathXMY_decompose}} return.
#' @param mediator,X_label,Y_label,Z_label,M_labels,digits,scale_max,score_intensity_max,...
#'   Passed through to \code{\link{plotPathXMY}}. See there for details.
#' @param Z_levels Numeric vector of Z values to render, one frame each.
#'   Default \code{c(-1, 0, 1)} — one SD below the mean, the mean, and
#'   one SD above, for a z-standardized between-person moderator. Linear
#'   models make path weights between these endpoints trivially
#'   interpolatable; values at \eqn{|Z| > 1} are often out of the
#'   well-supported range of the data so are not in the default.
#' @param panel_titles Optional character vector of titles, one per
#'   frame. If \code{NULL}, titles are auto-generated using \code{Z_label}
#'   and the \code{Z_levels} values.
#' @param width,height Frame width and height in inches. \code{height}
#'   defaults to \code{3.5} for the single-mediator triangle view and to
#'   \code{3 + 0.5 * n_mediators} for the fan view.
#' @param res PNG resolution in pixels per inch. Default \code{192}
#'   (2x retina factor for sharpness on high-DPI displays). Ignored when
#'   \code{format = "svg"}.
#' @param format One of \code{"png"} (default) or \code{"svg"}. SVG
#'   frames are vector and scale crisply at any zoom — recommended for
#'   HTML vignette output. PNG frames are embedded as
#'   \code{image/png;base64} data URIs at the requested \code{res};
#'   SVG frames are embedded as \code{image/svg+xml;base64} data URIs.
#'
#' @return An \code{htmltools::tagList} object that auto-renders as an
#'   interactive HTML widget in knitr / R Markdown. Call
#'   \code{htmltools::browsable(plotPathXMY_widget(...))} to preview it
#'   in the RStudio Viewer.
#' @seealso \code{\link{plotPathXMY}}, \code{\link{plotPathXMY_ZLH}},
#'   \code{\link{fieldSequenceWidget}}
#' @export
plotPathXMY_widget <- function(x,
                               mediator = NULL,
                               Z_levels = c(-1, 0, 1),
                               Z_label = "Z",
                               panel_titles = NULL,
                               width = 9,
                               height = NULL,
                               res = 192,
                               format = c("png", "svg"),
                               ...) {
  format <- match.arg(format)
  if (!requireNamespace("base64enc", quietly = TRUE))
    stop("Package 'base64enc' is needed for plotPathXMY_widget(). Please install it.")
  if (!requireNamespace("htmltools", quietly = TRUE))
    stop("Package 'htmltools' is needed for plotPathXMY_widget(). Please install it.")

  if (!is.numeric(Z_levels) || length(Z_levels) < 2L)
    stop("`Z_levels` must be a numeric vector of length >= 2.")

  ## Default height: compact for single-mediator triangle, scale with n
  ## of mediators for the fan view.
  tidy <- if (!is.null(x$fits) && !is.null(x$fits$full)) x$fits$full$tidy_loop
          else if (!is.null(x$tidy_loop))                 x$tidy_loop
          else stop("`x` must be a pathXMY() or pathXMY_decompose() return.")
  meds_in_fit <- unique(tidy$mediator)
  meds_in_fit <- meds_in_fit[!is.na(meds_in_fit)]
  n_m <- if (is.null(mediator)) length(meds_in_fit) else length(mediator)
  if (is.null(height)) height <- if (n_m == 1L) 3.5 else 3 + 0.5 * n_m

  n_frames <- length(Z_levels)
  if (is.null(panel_titles)) {
    panel_titles <- sprintf("%s = %+g", Z_label, Z_levels)
  } else if (length(panel_titles) != n_frames) {
    stop("`panel_titles` must have length equal to `Z_levels` when supplied.")
  }

  ## Render one frame per Z value at the requested size and format.
  uris <- character(n_frames)
  for (k in seq_along(Z_levels)) {
    if (format == "svg") {
      tmp <- tempfile(fileext = ".svg")
      grDevices::svg(tmp, width = width, height = height, bg = "white")
    } else {
      tmp <- tempfile(fileext = ".png")
      grDevices::png(tmp, width = width, height = height,
                     units = "in", res = res, bg = "white")
    }
    on.exit(unlink(tmp), add = TRUE)
    tryCatch({
      plotPathXMY(x,
                  mediator = mediator,
                  Z_value  = Z_levels[k],
                  Z_label  = Z_label,
                  title    = panel_titles[k],
                  ...)
    }, finally = grDevices::dev.off())
    mime <- if (format == "svg") "image/svg+xml" else "image/png"
    uris[k] <- base64enc::dataURI(file = tmp, mime = mime)
  }

  ## CSS display width: for PNG, half the rendered pixel width (2x retina);
  ## for SVG, the requested width in CSS px (96 dpi reference).
  display_px <- if (format == "svg") round(width * 96)
                else                  round(width * res / 2L)

  ## Unique widget id (safe as JS identifier and CSS class)
  wid <- paste0("pxw", paste(sample(c(letters, 0:9), 12L, replace = TRUE),
                              collapse = ""))

  img_tags <- lapply(seq_len(n_frames), function(k) {
    htmltools::tags$img(
      class = paste0(wid, "-frame"),
      src   = uris[k],
      alt   = panel_titles[k],
      style = paste0(
        "display:", if (k == 1L) "block" else "none", ";",
        "width:100%;",
        "max-width:", display_px, "px;"
      )
    )
  })

  nav_bar <- htmltools::tags$div(
    style = paste0(
      "display:flex;",
      "align-items:center;",
      "gap:12px;",
      "margin-top:8px;",
      "font-family:sans-serif;"
    ),
    htmltools::tags$button(
      id      = paste0(wid, "-back"),
      onclick = paste0(wid, "_nav(-1)"),
      "← Back"
    ),
    htmltools::tags$span(
      id    = paste0(wid, "-counter"),
      style = "min-width:11em; text-align:center;",
      paste("Frame 1 of", n_frames)
    ),
    htmltools::tags$button(
      id      = paste0(wid, "-forward"),
      onclick = paste0(wid, "_nav(1)"),
      "Forward →"
    )
  )

  js_code <- sprintf(
    '(function(){
  var id  = "%s";
  var n   = %d;
  var cur = 0;
  function show(k) {
    var imgs = document.querySelectorAll("." + id + "-frame");
    var j;
    for (j = 0; j < imgs.length; j++) imgs[j].style.display = "none";
    imgs[k].style.display = "block";
    document.getElementById(id + "-counter").textContent =
      "Frame " + (k + 1) + " of " + n;
    document.getElementById(id + "-back").disabled    = (k === 0);
    document.getElementById(id + "-forward").disabled = (k === n - 1);
    cur = k;
  }
  window[id + "_nav"] = function(dir) {
    show(Math.max(0, Math.min(n - 1, cur + dir)));
  };
  show(0);
})();',
    wid, n_frames
  )

  htmltools::tagList(
    htmltools::tags$div(
      style = paste0("max-width:", display_px, "px; margin:0 auto;"),
      do.call(htmltools::tagList, img_tags),
      nav_bar,
      htmltools::tags$script(htmltools::HTML(js_code))
    )
  )
}
