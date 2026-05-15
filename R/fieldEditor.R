#' Interactive field diagram editor
#'
#' Launches a local Shiny app for interactively repositioning nodes in a
#' functional field diagram. Drag nodes to desired positions, then click
#' "Render qgraph" to produce a high-quality static plot with those coordinates.
#'
#' @param fprops A field properties list returned by \code{\link{prepField}}.
#' @param fieldMatrix Optional path-coefficient matrix (from \code{\link{fieldResults}})
#'   used to draw weighted, signed edges. If NULL, edges are drawn unweighted.
#' @return Launches a Shiny app; does not return a value.
#' @export
fieldEditor <- function(fprops, fieldMatrix = NULL) {

  # ---- Build visNetwork nodes ------------------------------------------------
  vn  <- fprops$vnames
  vt  <- fprops$vtype
  mat <- fprops$matLayout

  # Map type codes to visNetwork shapes (approximating qgraph equivalents)
  # d = rfTriangle (right-facing) -> triangle (up, distinct from a)
  # a = lfTriangle (left-facing)  -> triangleDown (down, distinct from d)
  # c = circle in qgraph          -> circle in visNetwork
  shape_map <- c(d = "triangle", o = "diamond", x = "square", c = "circle",
                 a = "triangleDown", t = "circle", v = "diamond", V = "square",
                 e = "square", y = "triangle", n = "triangleDown")

  shapes <- vapply(vt, function(t) {
    s <- shape_map[t]
    if (is.na(s)) "ellipse" else s
  }, character(1))

  # Derive initial grid positions from layout matrix (col -> x, row -> y, inverted)
  nr   <- nrow(mat)
  nc   <- ncol(mat)
  nums <- fprops$numLayout
  pos  <- data.frame(id = seq_along(vn), x = NA_real_, y = NA_real_)
  for (r in seq_len(nr)) {
    for (co in seq_len(nc)) {
      idx <- suppressWarnings(as.integer(nums[r, co]))
      if (!is.na(idx) && idx > 0) {
        pos$x[idx] <- (co - 1) * 150
        pos$y[idx] <- (r - 1) * 150
      }
    }
  }

  nodes <- data.frame(
    id      = seq_along(vn),
    label   = vn,
    shape   = shapes,
    x       = pos$x,
    y       = pos$y,
    physics = FALSE,
    stringsAsFactors = FALSE
  )

  # ---- Build visNetwork edges ------------------------------------------------
  edges <- data.frame(from = integer(0), to = integer(0),
                      value = numeric(0), color = character(0),
                      stringsAsFactors = FALSE)

  if (!is.null(fieldMatrix)) {
    n <- nrow(fieldMatrix)
    for (r in seq_len(n)) {
      for (co in seq_len(n)) {
        w <- fieldMatrix[r, co]
        if (!is.na(w) && w != 0) {
          edges <- rbind(edges, data.frame(
            from  = r,
            to    = co,
            value = abs(w),
            color = if (w > 0) "#2196F3" else "#F44336",
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }

  # ---- Shiny UI --------------------------------------------------------------
  ui <- shiny::fluidPage(
    shiny::titlePanel("Functional Field Editor"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 2,
        shiny::helpText("Drag nodes to reposition them."),
        shiny::actionButton("render_btn", "Render qgraph", class = "btn-primary"),
        shiny::hr(),
        shiny::helpText("Node types:"),
        shiny::tags$ul(
          shiny::tags$li("triangle = do (d)"),
          shiny::tags$li("triangleDown = appraisal (a)"),
          shiny::tags$li("diamond = object (o) / verbal (v)"),
          shiny::tags$li("square = continuous (x) / message (e/V)"),
          shiny::tags$li("ellipse = choice (c) / appraisal+choice (t)")
        )
      ),
      shiny::mainPanel(
        width = 10,
        visNetwork::visNetworkOutput("net", height = "500px"),
        shiny::plotOutput("qgraph_out", height = "500px")
      )
    )
  )

  # ---- Shiny Server ----------------------------------------------------------
  server <- function(input, output, session) {

    output$net <- visNetwork::renderVisNetwork({
      visNetwork::visNetwork(nodes, edges) |>
        visNetwork::visEdges(arrows = "to", scaling = list(min = 1, max = 4)) |>
        visNetwork::visInteraction(dragNodes = TRUE, dragView = TRUE,
                                   zoomView = TRUE) |>
        visNetwork::visPhysics(enabled = FALSE)
    })

    shiny::observeEvent(input$render_btn, {
      visNetwork::visNetworkProxy("net") |>
        visNetwork::visGetPositions()
    })

    shiny::observeEvent(input$net_positions, {
      pos_list <- input$net_positions
      ids <- as.integer(names(pos_list))
      xs  <- vapply(pos_list, `[[`, numeric(1), "x")
      ys  <- vapply(pos_list, `[[`, numeric(1), "y")

      # Normalize to [-1, 1] range for qgraph
      xr <- range(xs)
      yr <- range(ys)
      xn <- if (diff(xr) == 0) xs * 0 else 2 * (xs - xr[1]) / diff(xr) - 1
      yn <- if (diff(yr) == 0) ys * 0 else -(2 * (ys - yr[1]) / diff(yr) - 1)

      layout_mat <- matrix(0, nrow = length(ids), ncol = 2)
      layout_mat[ids, 1] <- xn
      layout_mat[ids, 2] <- yn

      output$qgraph_out <- shiny::renderPlot({
        polys <- fieldPolygons()
        qgraph::qgraph(
          if (!is.null(fieldMatrix)) fieldMatrix else fprops$fmat,
          layout      = layout_mat,
          labels      = fprops$vnames,
          shape       = fprops$vshapes,
          vsize       = fprops$vsizes * 8,
          polygonList = polys,
          edge.color  = if (!is.null(fieldMatrix)) {
            ifelse(fieldMatrix[fieldMatrix != 0] > 0, "#2196F3", "#F44336")
          } else "black"
        )
      })
    })
  }

  shiny::runApp(shiny::shinyApp(ui, server))
}
