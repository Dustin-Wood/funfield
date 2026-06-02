#' Render pathXMY F-schema keys as matrix-cell display labels
#'
#' @description
#' \code{pathXMY()} and its companions (\code{\link{pathXMY_decompose}},
#' \code{\link{pathXMY_pairtable}}) label parameters with the F-schema
#' source-target keys \code{f1_XM} / \code{fZ_XY} --- prefix \code{f}, a
#' scope digit (\code{1} for the baseline, \code{Z} for a moderator), and
#' a two-letter \emph{source-target} suffix. These are lavaan-safe
#' identifiers, ideal as keys but terse to read.
#' \code{pathXMY_to_F()} rewrites them into the package's matrix-cell
#' \emph{display} form, \code{FZ[X,Y]} --- the \code{(X, Y)} cell (source
#' \code{X}, target \code{Y}) of the \code{Z}-overlay matrix \code{F_Z}.
#' See \code{vignette("notation")} for the schema.
#'
#' This is a \emph{display} transform only. The terse \code{fZ_XY} keys
#' are what index the tidy tables and what helper arguments expect;
#' matrix-bracket forms like \code{FZ[X,Y]} are not valid lavaan
#' identifiers and are intended for prose, captions, and printed table
#' cells. The intended idiom is to do all subsetting and arithmetic on
#' the keys, then wrap the \emph{final} object handed to a table:
#'
#' \preformatted{
#' kable0(pathXMY_to_F(subset(norm$tidy_loop, param == "f1_XM * f1_MY")))
#' }
#'
#' @details
#' A single coefficient \code{f<scope>_<src><tgt>} (with optional
#' \code{_joint} / \code{_total} model suffix) becomes
#' \code{F<scope>[<src>,<tgt>]}. The model suffix \code{_total} adds a
#' \code{*} superscript (the overall / no-mediator model); \code{_joint}
#' maps to the same cell (the joint multi-mediator model is identified by
#' the table it sits in). Products (\code{"A * B"}) are mapped
#' factor-by-factor and rejoined with a middot. A trailing parenthetical
#' annotation (e.g. \code{" (direct)"}) is preserved, and any token that
#' does not parse as a coefficient (e.g. \code{"sum (1+2+3)"}) passes
#' through unchanged.
#'
#' \tabular{lll}{
#'   \strong{Key} \tab \strong{Display} \tab \strong{Reads as} \cr
#'   \code{f1_XM} \tab \code{F1[X,M]} \tab baseline X -> M force \cr
#'   \code{fZ_XM} \tab \code{FZ[X,M]} \tab Z-moderation of X -> M \cr
#'   \code{f1_MY} \tab \code{F1[M,Y]} \tab baseline M -> Y force \cr
#'   \code{fZ_MY} \tab \code{FZ[M,Y]} \tab Z-moderation of M -> Y \cr
#'   \code{f1_XY} \tab \code{F1[X,Y]} \tab direct X -> Y \cr
#'   \code{fZ_XY} \tab \code{FZ[X,Y]} \tab Z-moderation of direct X -> Y \cr
#'   \code{f1_XY_total} \tab \code{F1*[X,Y]} \tab total (no-mediator) X -> Y \cr
#'   \code{fZ_XM * f1_MY} \tab \code{FZ[X,M]·F1[M,Y]} \tab expectation route \cr
#'   \code{fZ_XY (direct)} \tab \code{FZ[X,Y] (direct)} \tab residual direct moderation
#' }
#'
#' @param x One of: a character vector of parameter keys; a data frame
#'   carrying a \code{param} and/or \code{term} column (relabelled in
#'   place); or a \code{\link{pathXMY}} object (its \code{tidy_loop} and
#'   \code{tidy_joint} tables are relabelled). Lists carrying
#'   \code{components} (a \code{\link{pathXMY_decompose}} return) have
#'   their \code{components$term} relabelled.
#'
#' @return The same kind of object as \code{x}, with coefficient keys
#'   rewritten into the matrix-cell display form. For character input, a
#'   character vector of the same length.
#'
#' @seealso \code{\link{pathXMY}}, \code{\link{pathXMY_decompose}},
#'   \code{\link{pathXMY_pairtable}}; \code{vignette("notation")}.
#'
#' @examples
#' pathXMY_to_F(c("f1_XM", "fZ_MY", "fZ_XM * f1_MY", "fZ_XY (direct)"))
#' #> "F1[X,M]" "FZ[M,Y]" "FZ[X,M]·F1[M,Y]" "FZ[X,Y] (direct)"
#'
#' @export
pathXMY_to_F <- function(x) {
  if (is.character(x)) {
    return(vapply(x, .f_label, character(1), USE.NAMES = FALSE))
  }
  if (is.data.frame(x)) {
    return(.f_relabel_df(x))
  }
  if (is.list(x)) {
    ## pathXMY object: relabel the tidy tables.
    if (!is.null(x$tidy_loop))  x$tidy_loop  <- .f_relabel_df(x$tidy_loop)
    if (!is.null(x$tidy_joint)) x$tidy_joint <- .f_relabel_df(x$tidy_joint)
    ## pathXMY_decompose return: relabel the components' term column.
    if (!is.null(x$components)) x$components <- .f_relabel_df(x$components)
    return(x)
  }
  stop("pathXMY_to_F(): `x` must be a character vector, a data frame, ",
       "or a pathXMY / pathXMY_decompose object.", call. = FALSE)
}

## Relabel the param and/or term column of a tidy data frame in place.
.f_relabel_df <- function(d) {
  for (col in c("param", "term")) {
    if (col %in% names(d) && is.character(d[[col]])) {
      d[[col]] <- vapply(d[[col]], .f_label, character(1), USE.NAMES = FALSE)
    }
  }
  d
}

## Map one key (possibly a product, possibly annotated) to the matrix-cell
## display form. Unrecognized tokens pass through unchanged.
.f_label <- function(s) {
  if (is.na(s)) return(s)
  ## Products: split on "*", map each factor, rejoin with a middot.
  if (grepl("\\*", s)) {
    parts  <- strsplit(s, "\\s*\\*\\s*")[[1]]
    mapped <- vapply(parts, .f_label, character(1), USE.NAMES = FALSE)
    return(paste(mapped, collapse = intToUtf8(183L)))   # 183 = middot
  }
  ## Split a trailing parenthetical annotation off the bare token.
  m <- regmatches(s, regexec("^\\s*(\\S+)\\s*(\\(.*\\))?\\s*$", s))[[1]]
  if (length(m) == 0L) return(s)
  tok <- m[2]
  ann <- if (length(m) >= 3L && nzchar(m[3])) paste0(" ", m[3]) else ""
  f <- .f_token(tok)
  if (is.na(f)) s else paste0(f, ann)
}

## Map one bare F-schema key, e.g. "f1_XM", "fZ_XY_joint", "f1_XY_total".
## The two trailing letters are source then target. Returns NA for
## anything that is not a coefficient key.
.f_token <- function(tok) {
  m <- regmatches(
    tok,
    regexec("^f([1Zz])_([A-Za-z])([A-Za-z])(_joint|_total)?$", tok)
  )[[1]]
  if (length(m) == 0L) return(NA_character_)
  scope <- toupper(m[2])              # "1" (baseline) or "Z" (moderator)
  src   <- m[3]                       # first letter = source
  tgt   <- m[4]                       # second letter = target
  star  <- if (identical(m[5], "_total")) "*" else ""
  paste0("F", scope, star, "[", src, ",", tgt, "]")
}
