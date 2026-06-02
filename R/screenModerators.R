#' Fast moderator screen for the three X-M-Y path coefficients
#'
#' @description
#' For each candidate trait in a battery, estimate its moderation of
#' (a) the total \eqn{X \to Y} path (the total \code{fZ_XY}, one number
#' per trait), (b) the per-mediator \eqn{X \to M_k} paths (\code{fZ_XM},
#' one per trait per mediator), and (c) the per-mediator
#' \eqn{M_k \to Y} paths (\code{fZ_MY}, one per trait per mediator).
#' A few \code{cor.test()} and \code{lm()} calls in place of dozens of
#' SEM fits.
#'
#' \strong{The shortcuts.}
#' \describe{
#'   \item{\strong{YX arm}}{Per person, compute the within-person
#'     slope of \code{Y} on \code{X} (call it \eqn{\Delta Y}). Across
#'     persons, the Pearson correlation \eqn{r(T, \Delta Y)} is
#'     algebraically related to the total \code{fZ_XY} from a no-mediator
#'     \code{pathXMY()} fit by
#'     \deqn{F_Z^{*}[X,Y] = r(T, \Delta Y) \cdot \mathrm{SD}(\Delta Y),}
#'     because \code{pathXMY()} z-standardizes the between-person
#'     moderator internally (so the \eqn{\mathrm{SD}(T)} factor that
#'     would otherwise appear cancels).}
#'   \item{\strong{MX arm}}{The same shortcut, with the mediator in
#'     place of \code{Y}: per person compute the within-person slope
#'     of each \code{M_k} on \code{X} (\eqn{\Delta M_k}) and correlate
#'     with \code{T}. One row per (trait, mediator).}
#'   \item{\strong{YM arm}}{Per-person \eqn{Y}-on-\eqn{M} slopes are
#'     too sparse at typical EXSJT row counts to be useful (3 obs per
#'     person leaves 0 residual df once \code{X} is partialled out),
#'     so the YM shortcut is computed at the population level: a
#'     single OLS regression of within-person-deviated \code{Y} on
#'     within-person-deviated \code{X} and \code{M_k} with a
#'     \code{T_z * M_dev} interaction. The \code{M_dev:T_z}
#'     coefficient is the \code{fZ_MY} estimate (matches
#'     \code{pathXMY()} to ~0.1 percent in the speeding fit). The
#'     reported p-value comes from OLS and does \strong{not} account
#'     for within-person clustering, so it is optimistic compared to
#'     \code{pathXMY()}'s cluster-robust p. Use the ranking for
#'     screening; use \code{pathXMY_decompose()} for inference.}
#' }
#'
#' @param psi A data frame with within-person observations of \code{X},
#'   \code{Y}, and (for \code{MX}/\code{YM} arms) the mediators. Must
#'   contain a person ID column matching \code{p_col}.
#' @param X,Y Column names (length-1 character) for the focal X and Y.
#' @param M Character vector of mediator column names. Required when
#'   \code{arms} includes \code{"MX"} or \code{"YM"}.
#' @param traits A data frame with one row per person, a person ID
#'   column matching \code{p_col}, and one column per candidate trait.
#' @param trait_cols Character vector of trait column names in
#'   \code{traits} to screen. If \code{NULL} (default), all columns
#'   other than \code{p_col} are used.
#' @param p_col Person ID column name; default \code{"p"}. Must be
#'   present in both \code{psi} and \code{traits}.
#' @param arms Character vector subset of \code{c("YX", "MX", "YM")};
#'   default all three.
#' @return A tidy data frame with one row per (trait \eqn{\times}
#'   target \eqn{\times} mediator) cell. Columns:
#'   \describe{
#'     \item{\code{trait}}{Trait column name.}
#'     \item{\code{target}}{One of \code{"YX"}, \code{"MX"}, \code{"YM"}.}
#'     \item{\code{mediator}}{Mediator name (NA for the YX target).}
#'     \item{\code{r}}{Pearson correlation between the trait and the
#'       per-person slope (YX and MX targets only; NA for YM).}
#'     \item{\code{beta}}{Estimate on the \code{BZ} scale. For YX/MX
#'       this is \eqn{r \cdot \mathrm{SD}(\mathrm{slope})}; for YM
#'       it is the \code{M_dev:T_z} OLS coefficient.}
#'     \item{\code{p}}{Significance test \emph{p}-value. For YX/MX
#'       this is the Pearson \code{cor.test()} \emph{p}; for YM it is
#'       the OLS t-test \emph{p} (uncorrected for clustering — see
#'       Details).}
#'     \item{\code{n}}{Sample size used.}
#'   }
#'   Rows are sorted by descending \code{|beta|} within \code{target}.
#' @seealso \code{\link{pathXMY}}, \code{\link{pathXMY_decompose}}
#' @export
screenModerators <- function(psi, X, Y, M = NULL, traits,
                             trait_cols = NULL, p_col = "p",
                             arms = c("YX", "MX", "YM")) {

  ## ---- validate ------------------------------------------------
  arms <- match.arg(arms, several.ok = TRUE)
  if (!is.data.frame(psi))    stop("`psi` must be a data frame.")
  if (!is.data.frame(traits)) stop("`traits` must be a data frame.")
  if (!p_col %in% names(psi))    stop(sprintf("`psi` lacks `%s` column.", p_col))
  if (!p_col %in% names(traits)) stop(sprintf("`traits` lacks `%s` column.", p_col))
  for (cn in c(X, Y)) {
    if (!cn %in% names(psi)) stop(sprintf("`psi` lacks column `%s`.", cn))
  }
  need_M <- any(c("MX", "YM") %in% arms)
  if (need_M) {
    if (is.null(M) || length(M) == 0L)
      stop("`M` (mediator names) is required when `arms` includes \"MX\" or \"YM\".")
    miss_M <- setdiff(M, names(psi))
    if (length(miss_M))
      stop("`psi` lacks mediator columns: ",
           paste(miss_M, collapse = ", "))
  }
  if (is.null(trait_cols)) {
    cand <- setdiff(names(traits), p_col)
    is_num <- vapply(cand, function(cn) is.numeric(traits[[cn]]),
                     logical(1))
    skipped <- cand[!is_num]
    trait_cols <- cand[is_num]
    if (length(skipped))
      message("screenModerators: skipping non-numeric trait columns: ",
              paste(skipped, collapse = ", "))
  }
  miss_T <- setdiff(trait_cols, names(traits))
  if (length(miss_T))
    stop("`traits` lacks columns: ", paste(miss_T, collapse = ", "))
  non_num <- trait_cols[!vapply(trait_cols,
                                function(cn) is.numeric(traits[[cn]]),
                                logical(1))]
  if (length(non_num))
    stop("`trait_cols` includes non-numeric columns: ",
         paste(non_num, collapse = ", "))

  ## ---- helpers ------------------------------------------------
  slope_per_person <- function(d, pred, out) {
    d <- d[!is.na(d[[out]]) & !is.na(d[[pred]]), , drop = FALSE]
    if (nrow(d) < 2L || length(unique(d[[pred]])) < 2L) return(NA_real_)
    unname(stats::coef(stats::lm(stats::reformulate(pred, out),
                                 data = d))[pred])
  }
  compute_slopes <- function(pred, out) {
    spl <- split(psi[, c(pred, out), drop = FALSE], psi[[p_col]])
    sl  <- vapply(spl, slope_per_person, pred = pred, out = out,
                  FUN.VALUE = numeric(1))
    data.frame(p = names(sl), slope = unname(sl),
               stringsAsFactors = FALSE)
  }
  cor_screen <- function(slope_df, target, mediator_name = NA_character_) {
    m <- merge(slope_df, traits[, c(p_col, trait_cols)],
               by.x = "p", by.y = p_col, sort = FALSE)
    rows <- lapply(trait_cols, function(it) {
      ok <- !is.na(m$slope) & !is.na(m[[it]])
      if (sum(ok) < 10L) return(NULL)
      ct  <- stats::cor.test(m$slope[ok], m[[it]][ok])
      sd_slope <- stats::sd(m$slope[ok])
      data.frame(trait    = it,
                 target   = target,
                 mediator = mediator_name,
                 r        = unname(ct$estimate),
                 beta     = unname(ct$estimate) * sd_slope,
                 p        = ct$p.value,
                 n        = sum(ok),
                 stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
  }

  ## Within-person mean deviation. Used by the YM arm.
  dev_within <- function(x, p) x - ave(x, p, FUN = function(v)
                                       mean(v, na.rm = TRUE))

  out_pieces <- list()

  ## ---- YX arm -------------------------------------------------
  if ("YX" %in% arms) {
    dY <- compute_slopes(X, Y)
    out_pieces[[length(out_pieces) + 1L]] <- cor_screen(dY, "YX")
  }

  ## ---- MX arm -------------------------------------------------
  if ("MX" %in% arms) {
    for (Mk in M) {
      dM <- compute_slopes(X, Mk)
      out_pieces[[length(out_pieces) + 1L]] <- cor_screen(dM, "MX", Mk)
    }
  }

  ## ---- YM arm -------------------------------------------------
  ## Population-level OLS interaction on within-person-deviated data:
  ##   Y_dev ~ X_dev * T_z + M_dev * T_z
  ## The M_dev:T_z coefficient matches pathXMY()'s fZ_MY to ~0.1%
  ## (the residual diff is from how pathXMY handles person-mean
  ## centering vs the simple ave()-based deviation here).
  if ("YM" %in% arms) {
    psi_w <- psi[, c(p_col, X, Y, M), drop = FALSE]
    psi_w$.X_dev <- dev_within(psi_w[[X]], psi_w[[p_col]])
    psi_w$.Y_dev <- dev_within(psi_w[[Y]], psi_w[[p_col]])
    for (Mk in M)
      psi_w[[paste0(".M_dev_", Mk)]] <- dev_within(psi_w[[Mk]],
                                                   psi_w[[p_col]])

    ym_rows <- list()
    for (it in trait_cols) {
      tr <- traits[, c(p_col, it)]
      tr <- tr[!is.na(tr[[it]]), , drop = FALSE]
      if (nrow(tr) < 10L) next
      tr$.T_z <- as.numeric(scale(tr[[it]]))
      dat <- merge(psi_w, tr[, c(p_col, ".T_z")],
                   by = p_col, sort = FALSE)
      for (Mk in M) {
        mcol <- paste0(".M_dev_", Mk)
        fm <- stats::as.formula(sprintf(
          ".Y_dev ~ .X_dev * .T_z + %s * .T_z", mcol))
        f  <- tryCatch(stats::lm(fm, data = dat), error = function(e) NULL)
        if (is.null(f)) next
        cs <- summary(f)$coefficients
        rn <- rownames(cs)
        ix <- which(rn %in% c(paste0(mcol, ":.T_z"),
                              paste0(".T_z:", mcol)))
        if (length(ix) != 1L) next
        ym_rows[[length(ym_rows) + 1L]] <- data.frame(
          trait    = it,
          target   = "YM",
          mediator = Mk,
          r        = NA_real_,
          beta     = unname(cs[ix, "Estimate"]),
          p        = unname(cs[ix, "Pr(>|t|)"]),
          n        = nrow(dat),
          stringsAsFactors = FALSE
        )
      }
    }
    if (length(ym_rows))
      out_pieces[[length(out_pieces) + 1L]] <- do.call(rbind, ym_rows)
  }

  out <- do.call(rbind, out_pieces)
  rownames(out) <- NULL
  ## Sort by descending |beta| within target, preserving target order.
  target_order <- match(out$target, c("YX", "MX", "YM"))
  out <- out[order(target_order, -abs(out$beta)), ]
  rownames(out) <- NULL
  out
}
