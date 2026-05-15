#' Build an ESJT/EXSJT dataset from raw Qualtrics data
#'
#' @description
#' Convert raw Qualtrics survey output (data + column map) into the
#' funfield-canonical named-list ESJT/EXSJT structure: \code{$PSI},
#' \code{$cond}, \code{$sit}, \code{$traits}, \code{$codebook}. All Likert
#' ratings are rescaled to a [0, 1] proportion-of-maximum-possible (POMP)
#' metric via \code{fancyr::pomp()}. Codebook elements (variable wording,
#' scale anchors, factor levels) are populated automatically from the column
#' map.
#'
#' Designed to accept the output of \code{fancyr::fetch_survey_plus()}, which
#' returns a list with \code{$data} (the response data) and \code{$colmap}
#' (a tibble of \code{qname}, \code{main}, \code{sub}, \code{value_labels}).
#'
#' @param data Response data frame (typically \code{xsjt$data}). One row per
#'   respondent.
#' @param colmap Column map data frame with columns \code{qname}, \code{main},
#'   \code{sub}, \code{value_labels} (typically \code{xsjt$colmap}).
#' @param scenario Short tag for the scenario; used as the \code{s} value in
#'   \code{$PSI} (e.g., \code{"SL"}, \code{"WO"}, \code{"DH"}).
#' @param action_text Character vector of qnames pointing to the columns that
#'   carry the action display text (one qname per action).
#' @param likelihood Character vector of qnames for the per-action likelihood
#'   ratings, parallel to \code{action_text}.
#' @param outcomes List of character vectors. \code{outcomes[[i]]} holds the
#'   qnames of the outcome ratings for action \code{i}. All elements should
#'   have the same length (= \code{length(outcome_names)}).
#' @param outcome_names Names to use in \code{$PSI} for the outcome columns,
#'   parallel to each element of \code{outcomes}.
#' @param action_code Optional numeric coding for the actions on [0, 1]; if
#'   \code{NULL}, uses \code{seq(0, 1, length.out = length(action_text))}.
#'   Stored in \code{$PSI} as a column named per \code{action_code_name}.
#' @param action_code_name Column name for the numeric action coding in
#'   \code{$PSI} (default \code{"Speed"} for backwards compatibility with
#'   the speeding example; pass a domain-relevant name for other scenarios).
#' @param scenario_text Qname of the assembled scenario text column (e.g.,
#'   \code{"SL.scenario"}, \code{"overtimeScenario"}); if \code{NULL}, the
#'   function tries \code{paste0(scenario, ".scenario")}.
#' @param scenario_template Optional character. The canonical scenario
#'   template (typically the Qualtrics text with \code{${e://Field/...}}
#'   placeholders intact). Stored verbatim in
#'   \code{$codebook$scenario$template}. If \code{NULL}, the function falls
#'   back to a sample value from \code{data[[scenario_text]]}, which will be
#'   one specific respondent's fully-interpolated scenario.
#' @param conditions Character vector of qnames for situation-level factor
#'   columns; if \code{NULL}, auto-detected as all colmap qnames matching
#'   \code{^\\Q{scenario}\\E\\.}, excluding \code{scenario_text}.
#' @param perceptions Named character vector mapping perception names to
#'   qnames (e.g., \code{c(Realistic = "Speeding.Real_1", Relatable = "Speeding.Real_2")}).
#' @param traits Character vector of qnames for person-level rated items.
#' @param trait_names Optional names for trait columns in \code{$traits}; if
#'   \code{NULL}, derived by replacing spaces with underscores in the qnames.
#' @param demographics Named character vector mapping demographic variable
#'   names to qnames (e.g., \code{c(age = "Q13", gender = "Q14")}). Values
#'   are preserved as raw Qualtrics codes.
#' @param timing_blocks Optional list of lists, each with \code{first},
#'   \code{last}, and \code{n} (number of items in the timed block). Combined
#'   total time / total transitions becomes the SPI input.
#' @param smin,smax Lower and upper bounds of the original Likert metric for
#'   all rated columns (default 1 and 5).
#' @param strLine_max,prMaxSD_min,spi_min Screen thresholds. Set any to
#'   \code{NULL} to skip that screen. NA-in-screen is treated as "no evidence
#'   to drop" — respondents with missing screen values are retained.
#' @param id Qname of the respondent ID column (default \code{"ResponseId"}).
#'
#' @return A named list with elements \code{$PSI}, \code{$cond}, \code{$sit},
#'   \code{$traits}, and \code{$codebook}.
#' @export
esjtFromQualtrics <- function(
  data, colmap,
  scenario,
  action_text, likelihood, outcomes, outcome_names,
  action_code = NULL, action_code_name = "Speed",
  scenario_text = NULL,
  scenario_template = NULL,
  conditions = NULL,
  perceptions = NULL,
  traits = NULL, trait_names = NULL,
  demographics = NULL,
  timing_blocks = NULL,
  smin = 1, smax = 5,
  strLine_max = 0.10, prMaxSD_min = 0.25, spi_min = 1,
  id = "ResponseId"
) {
  ## ---- Validation -----------------------------------------------------
  n_actions <- length(action_text)
  if (length(likelihood) != n_actions)
    stop("length(likelihood) must equal length(action_text).")
  if (!is.list(outcomes) || length(outcomes) != n_actions)
    stop("'outcomes' must be a list of length(action_text) character vectors.")
  if (length(unique(lengths(outcomes))) != 1L)
    stop("Each element of 'outcomes' must have the same length.")
  if (length(outcome_names) != length(outcomes[[1]]))
    stop("length(outcome_names) must match length of each element of 'outcomes'.")
  if (is.null(action_code))
    action_code <- if (n_actions > 1L) seq(0, 1, length.out = n_actions) else 0
  if (length(action_code) != n_actions)
    stop("length(action_code) must equal length(action_text).")
  if (is.null(scenario_text))
    scenario_text <- paste0(scenario, ".scenario")
  if (is.null(conditions)) {
    pat <- paste0("^", gsub("([.\\\\+*?[^$()|])", "\\\\\\1", scenario), "\\.")
    conditions <- setdiff(grep(pat, colmap$qname, value = TRUE), scenario_text)
  }

  ## ---- Helpers ---------------------------------------------------------
  .numeric <- function(x) as.numeric(unclass(x))
  .chr     <- function(x) as.character(unclass(x))
  poMP     <- function(x) fancyr::pomp(.numeric(x), smin = smin, smax = smax)
  pull_lab <- function(qname, field = "sub") {
    val <- colmap[[field]][match(qname, colmap$qname)]
    if (is.null(val) || length(val) == 0L) NA_character_ else val
  }

  data$.p <- .chr(data[[id]])
  data <- data[!is.na(data$.p) & nzchar(data$.p), , drop = FALSE]

  ## ---- Build $PSI ------------------------------------------------------
  build_action <- function(a) {
    raw <- data[outcomes[[a]]]
    out <- as.data.frame(lapply(raw, poMP), check.names = FALSE)
    names(out) <- outcome_names
    df <- data.frame(
      p = data$.p,
      s = scenario,
      i = .chr(data[[action_text[a]]]),
      .code = action_code[a],
      out,
      Likelihood = poMP(data[[likelihood[a]]]),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    names(df)[names(df) == ".code"] <- action_code_name
    df
  }
  PSI <- do.call(rbind, lapply(seq_len(n_actions), build_action))
  PSI <- PSI[order(PSI$p, PSI[[action_code_name]]), , drop = FALSE]
  rownames(PSI) <- NULL

  ## ---- Build $cond -----------------------------------------------------
  cond_cols <- list(p = data$.p, s = scenario)
  for (q in conditions) cond_cols[[sub(paste0("^", scenario, "\\."), "", q)]] <- .chr(data[[q]])
  if (!is.null(scenario_text) && scenario_text %in% names(data))
    cond_cols$scenario <- .chr(data[[scenario_text]])
  cond <- data.frame(cond_cols, stringsAsFactors = FALSE, check.names = FALSE)

  ## ---- Build $sit ------------------------------------------------------
  if (!is.null(perceptions) && length(perceptions)) {
    sit_cols <- list(p = data$.p, s = scenario)
    for (nm in names(perceptions)) sit_cols[[nm]] <- poMP(data[[perceptions[[nm]]]])
    sit <- data.frame(sit_cols, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    sit <- NULL
  }

  ## ---- Build $traits ---------------------------------------------------
  traits_df <- NULL
  if (!is.null(traits) && length(traits)) {
    tr <- as.data.frame(lapply(data[traits], poMP), check.names = FALSE)
    if (is.null(trait_names)) trait_names <- gsub("\\s+", "_", traits)
    names(tr) <- trait_names
    traits_df <- cbind(data.frame(p = data$.p, stringsAsFactors = FALSE), tr)
  }
  if (!is.null(demographics) && length(demographics)) {
    dem <- lapply(demographics, function(q) .chr(data[[q]]))
    names(dem) <- names(demographics)
    dem_df <- as.data.frame(dem, stringsAsFactors = FALSE, check.names = FALSE)
    if (is.null(traits_df)) {
      traits_df <- cbind(data.frame(p = data$.p, stringsAsFactors = FALSE), dem_df)
    } else {
      traits_df <- cbind(traits_df, dem_df)
    }
  }

  ## ---- Data-quality screens -------------------------------------------
  rate_cols <- c(outcome_names, "Likelihood")
  keep <- rep(TRUE, length(data$.p))
  names(keep) <- data$.p

  if (!is.null(strLine_max)) {
    str_p <- fancyr::strLine(PSI[, rate_cols, drop = FALSE], group = PSI$p)
    flag  <- str_p[data$.p] > strLine_max
    flag[is.na(flag)] <- FALSE
    keep <- keep & !flag
  }
  if (!is.null(prMaxSD_min)) {
    psi_wide <- do.call(rbind, lapply(split(PSI[, rate_cols, drop = FALSE], PSI$p),
                                       function(d) as.numeric(as.matrix(d))))
    prmax_p <- fancyr::prMaxSD(psi_wide, smin = 0, smax = 1)
    flag <- prmax_p[data$.p] < prMaxSD_min
    flag[is.na(flag)] <- FALSE
    keep <- keep & !flag
  }
  if (!is.null(spi_min) && !is.null(timing_blocks) && length(timing_blocks)) {
    total_time <- rowSums(do.call(cbind, lapply(timing_blocks, function(b) {
      .numeric(data[[b$last]]) - .numeric(data[[b$first]])
    })))
    total_trans <- sum(vapply(timing_blocks, function(b) as.numeric(b$n) - 1, numeric(1)))
    spi_c <- total_time / total_trans
    flag <- spi_c < spi_min
    flag[is.na(flag)] <- FALSE
    keep <- keep & !flag
  }

  keep_p <- data$.p[keep]
  PSI <- PSI[PSI$p %in% keep_p, , drop = FALSE]
  cond <- cond[cond$p %in% keep_p, , drop = FALSE]
  if (!is.null(sit)) sit <- sit[sit$p %in% keep_p, , drop = FALSE]
  if (!is.null(traits_df)) traits_df <- traits_df[traits_df$p %in% keep_p, , drop = FALSE]

  ## ---- Build $codebook -------------------------------------------------
  codebook <- .esjt_codebook(
    colmap = colmap, data = data,
    scenario = scenario,
    action_text = action_text, likelihood = likelihood,
    outcomes = outcomes, outcome_names = outcome_names,
    action_code = action_code, action_code_name = action_code_name,
    scenario_text = scenario_text, scenario_template = scenario_template,
    conditions = conditions,
    perceptions = perceptions,
    traits = traits, trait_names = trait_names,
    demographics = demographics,
    smin = smin, smax = smax
  )

  ## ---- Assemble --------------------------------------------------------
  out <- list(PSI = PSI, cond = cond)
  if (!is.null(sit))       out$sit    <- sit
  if (!is.null(traits_df)) out$traits <- traits_df
  out$codebook <- codebook
  out
}

## Internal: rescale a "1=A | 2=B | ..." value_labels string to [0,1].
.rescale_anchors <- function(s, smin, smax) {
  if (is.null(s) || is.na(s) || !nzchar(s)) return(NA_character_)
  parts <- strsplit(s, "\\s*\\|\\s*")[[1]]
  rebuilt <- vapply(parts, function(p) {
    m <- regmatches(p, regexec("^\\s*([0-9.]+)\\s*=\\s*(.*)$", p))[[1]]
    if (length(m) != 3L) return(p)
    k <- suppressWarnings(as.numeric(m[2]))
    if (is.na(k)) return(p)
    new_k <- (k - smin) / (smax - smin)
    sprintf("%g = %s", new_k, m[3])
  }, character(1))
  paste(unname(rebuilt), collapse = " | ")
}

## Internal: build the codebook from colmap + spec
.esjt_codebook <- function(colmap, data, scenario,
                           action_text, likelihood, outcomes, outcome_names,
                           action_code, action_code_name,
                           scenario_text, scenario_template, conditions,
                           perceptions, traits, trait_names,
                           demographics, smin, smax) {
  pull <- function(q, field) {
    v <- colmap[[field]][match(q, colmap$qname)]
    if (length(v) == 0L) NA_character_ else v
  }

  cb <- list()

  ## outcomes: use the first action's outcome qnames to pull labels
  cb$outcomes <- data.frame(
    var   = outcome_names,
    label = vapply(outcomes[[1]], pull, character(1), field = "sub"),
    scale = .rescale_anchors(pull(outcomes[[1]][1], "value_labels"), smin, smax),
    stem  = pull(outcomes[[1]][1], "main"),
    stringsAsFactors = FALSE
  )

  ## Likelihood
  cb$Likelihood <- data.frame(
    var   = "Likelihood",
    label = "Reported likelihood of taking each action in this situation",
    scale = .rescale_anchors(pull(likelihood[1], "value_labels"), smin, smax),
    stem  = pull(likelihood[1], "main"),
    stringsAsFactors = FALSE
  )

  ## actions: pull one example text per action from the data
  cb$actions <- data.frame(
    action   = seq_along(action_text),
    code     = action_code,
    text     = vapply(action_text, function(q) {
      v <- as.character(unclass(data[[q]]))
      v <- v[!is.na(v) & nzchar(v)]
      if (length(v) == 0L) NA_character_ else v[1]
    }, character(1)),
    stringsAsFactors = FALSE
  )
  names(cb$actions)[names(cb$actions) == "code"] <- action_code_name

  ## scenario: template (first non-NA scenario_text value, with conditions
  ## bracketed) + factors table
  scenario_factors <- data.frame(
    var = sub(paste0("^", scenario, "\\."), "", conditions),
    levels = vapply(conditions, function(q) {
      v <- unique(as.character(unclass(data[[q]])))
      v <- v[!is.na(v) & nzchar(v)]
      paste(v, collapse = "; ")
    }, character(1)),
    stringsAsFactors = FALSE
  )
  template <- NA_character_
  if (!is.null(scenario_template) && nzchar(scenario_template)) {
    template <- scenario_template
  } else if (!is.null(scenario_text) && scenario_text %in% names(data)) {
    v <- unique(as.character(unclass(data[[scenario_text]])))
    v <- v[!is.na(v) & nzchar(v)]
    if (length(v)) template <- v[1]
  }
  cb$scenario <- list(template = template, factors = scenario_factors)

  ## sit_items
  if (!is.null(perceptions) && length(perceptions)) {
    cb$sit_items <- data.frame(
      var   = names(perceptions),
      label = vapply(perceptions, pull, character(1), field = "sub"),
      scale = .rescale_anchors(pull(perceptions[[1]], "value_labels"), smin, smax),
      stem  = pull(perceptions[[1]], "main"),
      stringsAsFactors = FALSE
    )
  }

  ## trait_items
  if (!is.null(traits) && length(traits)) {
    if (is.null(trait_names)) trait_names <- gsub("\\s+", "_", traits)
    cb$trait_items <- data.frame(
      var   = trait_names,
      label = vapply(traits, pull, character(1), field = "sub"),
      scale = .rescale_anchors(pull(traits[1], "value_labels"), smin, smax),
      stem  = pull(traits[1], "main"),
      stringsAsFactors = FALSE
    )
  }

  ## demographics
  if (!is.null(demographics) && length(demographics)) {
    cb$demographics <- data.frame(
      var   = names(demographics),
      label = vapply(demographics, pull, character(1), field = "main"),
      qualtrics = unname(demographics),
      note  = "Values preserved as raw Qualtrics codes; see colmap for level labels.",
      stringsAsFactors = FALSE
    )
  }

  cb
}
