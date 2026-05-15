#' Interactive Step-by-Step Field Sequence Widget
#'
#' @description Renders each time step of a functional field simulation as a
#'   high-resolution PNG image and wraps them in a self-contained HTML widget
#'   with \strong{Back} and \strong{Forward} navigation buttons.  The widget
#'   is suitable for use in R Markdown / knitr HTML documents (including
#'   package vignettes), RStudio's Viewer pane, and Shiny apps.  No server is
#'   required: all images are embedded as base-64 data URIs so the HTML file
#'   is fully self-contained.
#'
#'   Images are rendered at twice the CSS display width (controlled by
#'   \code{res}) so they appear sharp on high-DPI / retina displays.
#'
#' @param sitlog Output of \code{\link{expOutcomesFF}}: a list with elements
#'   \code{sit} (situation matrix), \code{dit} (disturbance matrix), and
#'   \code{fit} (list of field states, one per time step).
#' @param fprops Output of \code{\link{prepField}}: a list containing
#'   \code{matLayout}, \code{vnames}, \code{vshapes}, \code{vsizes}, and
#'   \code{vtype}.
#' @param steps Integer vector of time-step indices to include.  Defaults to
#'   all valid steps (those with a non-\code{NULL} \code{$ft} element in
#'   \code{sitlog$fit}).
#' @param titles Character vector of step titles displayed \emph{above} each
#'   image, one per element of \code{steps}.  Defaults to
#'   \code{"Step 1"}, \code{"Step 2"}, etc.
#' @param node_labels Optional named character vector mapping internal node
#'   names to display labels (e.g.
#'   \code{c("s1" = "Ready to make coffee", "make(s2).p" = "Set up coffeemaker")}).
#'   When supplied, labels are drawn \emph{outside} each node (above action /
#'   appraisal nodes, below object / state nodes) rather than inside.  Names
#'   not found in \code{fprops$vnames} are silently ignored; nodes without an
#'   entry fall back to their internal name.
#' @param label.cex Character expansion for external node labels.  Default
#'   \code{0.65}.  Only used when \code{node_labels} is supplied.
#' @param width Plot width in inches.  Default \code{9}.
#' @param height Plot height in inches.  Default \code{5.5}.
#' @param res PNG resolution in pixels per inch.  Default \code{192}, which
#'   renders the image at 2× the CSS display size for retina sharpness.
#' @param ... Additional arguments passed through to \code{qgraph::qgraph()}.
#' @return An \code{htmltools::tagList} object.  In a knitr / R Markdown
#'   chunk this renders automatically as an interactive HTML widget.  Call
#'   \code{htmltools::browsable(fieldSequenceWidget(...))} to preview it in
#'   the RStudio Viewer.
#' @seealso \code{\link{expOutcomesFF}}, \code{\link{prepField}},
#'   \code{\link{plotFieldSequence}}, \code{\link{pnLevels}},
#'   \code{\link{fieldPolygons}}
#' @export

fieldSequenceWidget <- function(sitlog, fprops,
                                steps       = NULL,
                                titles      = NULL,
                                node_labels = NULL,
                                label.cex   = 0.65,
                                width       = 9,
                                height      = 5.5,
                                res         = 192,
                                ...) {

  if (!requireNamespace("qgraph",    quietly = TRUE))
    stop("Package 'qgraph' is needed for fieldSequenceWidget(). Please install it.")
  if (!requireNamespace("base64enc", quietly = TRUE))
    stop("Package 'base64enc' is needed for fieldSequenceWidget(). Please install it.")
  if (!requireNamespace("htmltools", quietly = TRUE))
    stop("Package 'htmltools' is needed for fieldSequenceWidget(). Please install it.")

  vnames  <- as.vector(fprops$vnames)
  vtype   <- as.vector(fprops$vtype)
  n_nodes <- length(vnames)
  n_fit   <- length(sitlog$fit)

  # ---- Resolve steps --------------------------------------------------------
  if (is.null(steps)) {
    steps <- which(vapply(
      sitlog$fit,
      function(f) !is.null(f) && !is.null(f$ft),
      logical(1)
    ))
  }
  n_steps <- length(steps)
  if (n_steps == 0L) stop("No valid steps found in sitlog$fit.")

  if (is.null(titles)) titles <- paste("Step", seq_along(steps))

  # ---- Resolve display labels -----------------------------------------------
  use_external <- !is.null(node_labels)
  if (use_external) {
    if (!is.null(names(node_labels))) {
      display_labels <- vnames
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

  # ---- Coordinate matrix from matLayout (col = x, -row = y) ----------------
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

  # ---- Render each step to a base64 data URI --------------------------------
  uris <- character(n_steps)

  for (k in seq_along(steps)) {
    i   <- steps[k]
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)

    # Blank placeholder for any invalid step index
    if (i < 1L || i > n_fit ||
        is.null(sitlog$fit[[i]]) ||
        is.null(sitlog$fit[[i]]$ft)) {
      grDevices::png(tmp, width = width, height = height,
                     units = "in", res = res, bg = "white")
      graphics::plot.new()
      grDevices::dev.off()
      uris[k] <- base64enc::dataURI(file = tmp, mime = "image/png")
      next
    }

    ft     <- sitlog$fit[[i]]$ft
    sit_i  <- sitlog$sit[i, ]
    groups <- pnLevels(sit_i)

    internal_labels <- if (use_external) rep("", n_nodes) else display_labels

    grDevices::png(tmp, width = width, height = height,
                   units = "in", res = res, bg = "white")

    g <- qgraph::qgraph(
      ft,
      layout      = coords,
      shape       = fprops$vshapes,
      vsize       = fprops$vsizes,
      groups      = groups,
      color       = c("lightblue", "lightyellow"),
      polygonList = fp,
      labels      = internal_labels,
      ...
    )

    if (use_external) {
      y_vals <- sort(unique(round(g$layout[, 2], 8)))
      y_gaps <- diff(y_vals)
      y_step <- if (any(y_gaps > 0)) min(y_gaps[y_gaps > 0]) else 0.4
      y_off  <- y_step * 0.30
      y_offsets <- ifelse(vtype %in% c("d", "a"), y_off, -y_off)
      graphics::text(
        g$layout[, 1],
        g$layout[, 2] + y_offsets,
        labels = display_labels,
        cex    = label.cex,
        xpd    = NA
      )
    }

    grDevices::dev.off()
    uris[k] <- base64enc::dataURI(file = tmp, mime = "image/png")
  }

  # ---- Build HTML -----------------------------------------------------------
  # Unique alphanumeric ID — safe as both a JS identifier and a CSS class name.
  # Using 12 random chars gives ~4e18 combinations; collision probability in
  # a single document is negligible.
  wid <- paste0("fsw", paste(sample(c(letters, 0:9), 12L, replace = TRUE),
                              collapse = ""))

  # CSS display width is half the rendered pixel width (2x retina factor).
  display_px <- round(width * res / 2L)

  # Title divs — only the first visible initially; JS manages the rest.
  title_divs <- lapply(seq_len(n_steps), function(k) {
    htmltools::tags$div(
      class = paste0(wid, "-title"),
      style = paste0(
        "display:",      if (k == 1L) "block" else "none", ";",
        "font-family:sans-serif;",
        "font-size:1.05em;",
        "font-weight:bold;",
        "margin-bottom:4px;"
      ),
      titles[k]
    )
  })

  # Image tags — only the first visible initially.
  img_tags <- lapply(seq_len(n_steps), function(k) {
    htmltools::tags$img(
      class = paste0(wid, "-step"),
      src   = uris[k],
      alt   = titles[k],
      style = paste0(
        "display:", if (k == 1L) "block" else "none", ";",
        "width:100%;",
        "max-width:", display_px, "px;"
      )
    )
  })

  # Navigation bar.
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
      "\u2190 Back"
    ),
    htmltools::tags$span(
      id    = paste0(wid, "-counter"),
      style = "min-width:9em; text-align:center;",
      paste("Step 1 of", n_steps)
    ),
    htmltools::tags$button(
      id      = paste0(wid, "-forward"),
      onclick = paste0(wid, "_nav(1)"),
      "Forward \u2192"
    )
  )

  # Self-contained IIFE: owns show(k) and exposes nav(dir) on window.
  # Runs synchronously — all DOM elements above the script tag are already
  # present, so show(0) correctly initialises button states without needing
  # to wait for DOMContentLoaded.
  js_code <- sprintf(
    '(function(){
  var id  = "%s";
  var n   = %d;
  var cur = 0;
  function show(k) {
    var imgs   = document.querySelectorAll("." + id + "-step");
    var ttls   = document.querySelectorAll("." + id + "-title");
    var j;
    for (j = 0; j < imgs.length; j++) imgs[j].style.display = "none";
    for (j = 0; j < ttls.length; j++) ttls[j].style.display = "none";
    imgs[k].style.display = "block";
    ttls[k].style.display = "block";
    document.getElementById(id + "-counter").textContent =
      "Step " + (k + 1) + " of " + n;
    document.getElementById(id + "-back").disabled    = (k === 0);
    document.getElementById(id + "-forward").disabled = (k === n - 1);
    cur = k;
  }
  window[id + "_nav"] = function(dir) {
    show(Math.max(0, Math.min(n - 1, cur + dir)));
  };
  show(0);
})();',
    wid, n_steps
  )

  htmltools::tagList(
    htmltools::tags$div(
      style = paste0("max-width:", display_px, "px; margin:0 auto;"),
      do.call(htmltools::tagList, title_divs),
      do.call(htmltools::tagList, img_tags),
      nav_bar,
      htmltools::tags$script(htmltools::HTML(js_code))
    )
  )
}
