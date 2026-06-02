#' Render a Wide Data Frame as an HTML Table with Grouped Column Headers
#'
#' @description
#' Renders a wide data frame --- typically the output of
#' \code{\link{pathXMY_pairtable}} --- as a self-contained HTML table in
#' which the columns are gathered under spanning group headers. Each group
#' after the first is set off by a divider rule, so two parameter blocks
#' placed side by side (e.g. \code{f1_XM} beside \code{f1_MY}) read as
#' distinct, titled panels rather than one undifferentiated block of
#' numbers.
#'
#' Intended for use inside R Markdown / knitr HTML documents (the package
#' vignettes use it to display \code{\link{pathXMY_pairtable}} results).
#' The returned object renders as raw HTML; styling is carried in an inline
#' \code{<style>} block, so no external CSS is required.
#'
#' @param df A data frame to render. Numeric columns are formatted to
#'   \code{digits} decimal places; other columns are coerced to character.
#' @param groups A named, integer-valued vector describing the column
#'   groups in order. Names are the spanning header labels (may contain
#'   HTML, e.g. an HTML entity such as \code{"&times;"}); values are the
#'   number of columns each group spans and must sum to \code{ncol(df)}.
#'   Use a one-column group named \code{" "} for a leading key column
#'   (e.g. the mediator name).
#' @param col_labels Character vector of length \code{ncol(df)} giving the
#'   second (per-column) header row. When \code{NULL} (default), the data
#'   frame's own column names are used.
#' @param digits Number of decimal places for numeric columns (default 3).
#' @param caption Optional table caption (character; may contain HTML).
#' @param strip0 Logical; if \code{TRUE}, numeric columns are formatted
#'   via \code{\link{f0}} so the leading zero is stripped from any
#'   decimal whose absolute value is less than 1 (\code{0.123 -> .123}).
#'   Default \code{TRUE} matches the funfield vignette house style.
#'
#' @return An \code{\link[htmltools]{HTML}} object: a complete
#'   \code{<table>} preceded by an inline \code{<style>} block, which knitr
#'   emits verbatim into HTML output.
#'
#' @seealso \code{\link{pathXMY_pairtable}}, \code{\link{pathXMY}}
#'
#' @examples
#' \dontrun{
#' data(speedingESJT)
#' mediators <- c("Crash","Injured","Ticket","MoneyCost","OnTime",
#'                "IntQuality","FunDrive","Appropriate")
#' res <- pathXMY(speedingESJT$PSI, X = "Speed", Y = "Likelihood",
#'                M = mediators)
#' tab <- pathXMY_pairtable(res, c("f1_XM", "f1_MY"))
#' group_kable(
#'   tab,
#'   groups = c(" " = 1,
#'              "Expectation paths (F1[X,M])" = 4,
#'              "Valuation paths (F1[M,Y])"   = 4),
#'   col_labels = c("Mediator", "est", "se", "z", "p",
#'                              "est", "se", "z", "p"))
#' }
#'
#' @export
group_kable <- function(df, groups, col_labels = NULL, digits = 3,
                        caption = NULL, strip0 = TRUE) {
  stopifnot(is.data.frame(df), ncol(df) >= 1L,
            length(groups) >= 1L, !is.null(names(groups)))
  n <- ncol(df)
  gspan <- as.integer(groups)
  if (anyNA(gspan) || any(gspan < 1L)) {
    stop("'groups' values must be positive whole numbers.", call. = FALSE)
  }
  if (sum(gspan) != n) {
    stop(sprintf("'groups' values sum to %d but 'df' has %d columns.",
                 sum(gspan), n), call. = FALSE)
  }
  if (is.null(col_labels)) col_labels <- names(df)
  if (length(col_labels) != n) {
    stop(sprintf("'col_labels' has length %d but 'df' has %d columns.",
                 length(col_labels), n), call. = FALSE)
  }

  fmt <- function(x) {
    if (!is.numeric(x)) return(as.character(x))
    if (strip0) f0(x, digits = digits)
    else        formatC(x, format = "f", digits = digits)
  }
  B <- as.data.frame(lapply(df, fmt), stringsAsFactors = FALSE)

  gstart <- cumsum(c(1L, gspan))[seq_along(gspan)]
  divid  <- gstart[-1]                       # first col of each later group
  dv <- function(j) if (j %in% divid) ' class="grpdiv"' else ''

  h1 <- paste0(mapply(function(nm, sp, st)
      sprintf('<th colspan="%d"%s>%s</th>', sp, dv(st), nm),
      names(groups), gspan, gstart), collapse = "")
  h2 <- paste0(vapply(seq_len(n), function(j)
      sprintf('<th%s>%s</th>', dv(j), col_labels[j]), character(1)),
      collapse = "")
  body <- paste0(vapply(seq_len(nrow(B)), function(i)
      paste0('<tr>', paste0(vapply(seq_len(n), function(j)
        sprintf('<td%s>%s</td>', dv(j), B[i, j]), character(1)),
        collapse = ''), '</tr>'), character(1)), collapse = "")

  cap <- if (!is.null(caption))
    sprintf('<caption>%s</caption>', caption) else ''

  css <- paste0(
    '<style>',
    'table.grouptbl{border-collapse:collapse;margin:0 auto 1.2em;',
      ## Tabular (fixed-width) digits make decimal points line up
      ## under right-aligned numeric cells even when integer parts
      ## differ in width (e.g., ".664" vs "-4.654").
      'font-variant-numeric:tabular-nums;}',
    'table.grouptbl caption{caption-side:top;font-style:italic;',
      'padding-bottom:4px;}',
    'table.grouptbl th,table.grouptbl td{padding:3px 10px;',
      'text-align:right;}',
    'table.grouptbl thead th{border-bottom:1px solid #555;}',
    'table.grouptbl thead tr:first-child th{text-align:center;',
      'font-weight:600;border-bottom:1px solid #bbb;}',
    'table.grouptbl td:first-child,table.grouptbl th:first-child{',
      'text-align:left;}',
    'table.grouptbl tbody tr:nth-child(even){background:#f6f6f6;}',
    'table.grouptbl .grpdiv{border-left:2px solid #bbb;',
      'padding-left:18px;}',
    '</style>')

  htmltools::HTML(paste0(
    css, '<table class="grouptbl">', cap,
    '<thead><tr>', h1, '</tr><tr>', h2, '</tr></thead><tbody>',
    body, '</tbody></table>'))
}
