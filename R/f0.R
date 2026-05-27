#' Format numbers without the leading zero
#'
#' @description
#' Format a numeric vector to character, stripping the leading zero from
#' values whose absolute value is less than 1. \code{0.123 -> ".123"},
#' \code{-0.045 -> "-.045"}, \code{1.234 -> "1.234"}.
#'
#' Used as a vignette / output style helper: funfield's vignettes
#' default to no leading zeros on decimals (aggressive: applies to all
#' decimals, not just bounded-by-1 quantities).
#'
#' @param x Numeric vector (or anything coercible to numeric). \code{NA}
#'   passes through as \code{NA_character_}.
#' @param digits Number of decimal places. Default 3.
#' @param keep_sign Logical; if \code{TRUE}, prepend an explicit
#'   \code{"+"} for non-negative non-zero values. Default \code{FALSE}.
#' @return A character vector the same length as \code{x}.
#' @examples
#' f0(c(0.123, -0.045, 1.234, NA))         # ".123", "-.045", "1.234", NA
#' f0(c(0.1, -0.001), digits = 2)          # ".10", "-.00"
#' f0(0.5, keep_sign = TRUE)               # "+.500"
#' @export
f0 <- function(x, digits = 3, keep_sign = FALSE) {
  if (!is.numeric(x)) x <- suppressWarnings(as.numeric(x))
  out <- formatC(x, digits = digits, format = "f")
  ## Strip leading "0" before decimal point. Handles both positive and
  ## negative cases. Numbers with |x| >= 1 have a non-zero integer
  ## part, so the regex naturally leaves them alone.
  out <- sub("^(-?)0\\.", "\\1.", out)
  if (keep_sign) {
    out[!is.na(x) & x >= 0] <- paste0("+", out[!is.na(x) & x >= 0])
  }
  out[is.na(x)] <- NA_character_
  out
}


#' Apply f0() to numeric columns of a data frame
#'
#' @description
#' Convenience wrapper: returns a copy of \code{df} where every numeric
#' column has been formatted via \code{\link{f0}}. Useful as a
#' pre-processing step before passing a coefficient table to
#' \code{knitr::kable()} or \code{\link{group_kable}} when the
#' no-leading-zero style is in effect.
#'
#' @param df A data frame.
#' @param digits,keep_sign Passed through to \code{\link{f0}}.
#' @param cols Optional character vector of column names to format. If
#'   \code{NULL} (default), all numeric columns are formatted.
#' @return A data frame the same shape as \code{df}; numeric columns
#'   are now character.
#' @export
df_f0 <- function(df, digits = 3, keep_sign = FALSE, cols = NULL) {
  stopifnot(is.data.frame(df))
  if (is.null(cols))
    cols <- names(df)[vapply(df, is.numeric, logical(1))]
  for (cn in cols) df[[cn]] <- f0(df[[cn]], digits = digits,
                                  keep_sign = keep_sign)
  df
}


#' `knitr::kable()` wrapper with stripped leading zeros
#'
#' @description
#' Thin wrapper that pre-processes a data frame via \code{\link{df_f0}}
#' before handing it to \code{knitr::kable()}. Used in funfield
#' vignettes to keep tabular output consistent with the house style:
#' decimals less than one render as \code{.123} rather than \code{0.123}.
#'
#' @param x A data frame or anything \code{\link{df_f0}} can format
#'   (numeric columns become character with stripped leading zeros).
#' @param digits Number of decimal places (default 3). Passed to
#'   \code{df_f0()}, not to \code{knitr::kable()}.
#' @param keep_sign Logical; if \code{TRUE}, prepend \code{"+"} for
#'   non-negative non-zero values. Default \code{FALSE}.
#' @param ... Additional arguments passed to \code{knitr::kable()}
#'   (e.g., \code{caption}, \code{col.names}, \code{format}).
#' @return A \code{knitr_kable} object.
#' @export
kable0 <- function(x, digits = 3, keep_sign = FALSE, align = NULL, ...) {
  if (!requireNamespace("knitr", quietly = TRUE))
    stop("kable0() requires the 'knitr' package.")
  if (is.data.frame(x)) {
    ## Detect originally-numeric columns BEFORE df_f0() converts them
    ## to character, and force right-alignment for those columns.
    ## Without this, knitr::kable() would left-align them (its default
    ## for character cols) and the f0-stripped decimals wouldn't line
    ## up over their decimal points.
    if (is.null(align)) {
      align <- ifelse(vapply(x, is.numeric, logical(1)), "r", "l")
    }
    x <- df_f0(x, digits = digits, keep_sign = keep_sign)
  }
  knitr::kable(x, align = align, ...)
}


#' Emit the funfield decimal-house-style CSS once per HTML document
#'
#' @description
#' Outputs a small \code{<style>} block that turns on
#' \code{font-variant-numeric: tabular-nums} for any right-aligned
#' table cell. Combined with right-alignment (which \code{\link{kable0}}
#' enables for originally-numeric columns and \code{\link{group_kable}}
#' enables via its own CSS), this makes the decimal points line up
#' under each other even when integer parts differ in width
#' (e.g., \code{.664} versus \code{-4.654}).
#'
#' Intended to be called \emph{once} per vignette, in a chunk with
#' \code{echo = FALSE, results = "asis"}. Re-emitting it is harmless
#' but wasteful.
#'
#' @return Side effect: prints raw HTML. Invisibly returns the CSS
#'   string.
#' @examples
#' \dontrun{
#' # In a vignette setup chunk: ```{r echo=FALSE, results="asis"}
#' #                            strip0_css()
#' #                            ```
#' strip0_css()
#' }
#' @export
strip0_css <- function() {
  css <- paste0(
    '<style>',
    'table td[align="right"],table th[align="right"]',
    '{font-variant-numeric:tabular-nums;}',
    '</style>')
  cat(css)
  invisible(css)
}
