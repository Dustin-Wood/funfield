#' Speeding ESJT Dataset
#'
#' @description
#' Long-format Elaborated & eXperimental Situational Judgment Test (EXSJT) data
#' on speeding decisions, bundled as a named list with five linked elements
#' (four data frames plus a codebook). Used in Wood, Harms, & Cho (2023) and
#' as a canonical example in funfield documentation.
#'
#' Each participant (N = 333 after quality screening) rated three actions
#' (drive at the speed limit, +5 mph, +10+ mph) on eight expected outcomes
#' and on the likelihood that they would take the action. Six features of
#' the scenario (vehicle, weather, traffic, time pressure, social context,
#' and police visibility) were randomly varied across participants, so that
#' each respondent saw exactly one fully crossed instance of the speeding
#' scenario. This differs from \code{\link{coworkerESJT}}, in which 12
#' discrete situations were rated within each participant.
#'
#' @format A named list with five elements:
#'
#' \strong{\code{$PSI}} — person × action ratings (999 rows = 333 persons ×
#' 3 actions). All Likert-rated columns are rescaled to [0, 1]:
#' \describe{
#'   \item{p}{Person ID (Qualtrics response ID string).}
#'   \item{s}{Situation ID (constant: \code{"SL"} — the single speeding scenario).}
#'   \item{i}{Action text as displayed to the respondent.}
#'   \item{Speed}{Action coded on [0, 1]: \code{0} = at the speed limit,
#'     \code{0.5} = +5 mph, \code{1} = +10+ mph.}
#'   \item{MoneyCost}{\emph{The total financial cost of your trip.} [0, 1]}
#'   \item{FunDrive}{\emph{The amount of enjoyment experienced from driving on
#'     this trip.} [0, 1]}
#'   \item{IntQuality}{\emph{The quality of your next meeting with your
#'     [context].} [0, 1]}
#'   \item{Appropriate}{\emph{Your sense that you acted appropriately.} [0, 1]}
#'   \item{OnTime}{\emph{Likelihood of getting to your meeting on time.} [0, 1]}
#'   \item{Crash}{\emph{Likelihood of getting into a car crash.} [0, 1]}
#'   \item{Injured}{\emph{Likelihood of having a serious injury.} [0, 1]}
#'   \item{Ticket}{\emph{Likelihood of getting a speeding ticket.} [0, 1]}
#'   \item{Likelihood}{\emph{How likely are you to do each action in this
#'     situation?} [0, 1]}
#' }
#'
#' \strong{\code{$cond}} — situation-level experimental factors (333 rows;
#' one per person). The six factors were independently and randomly varied
#' across participants:
#' \describe{
#'   \item{p}{Person ID.}
#'   \item{s}{Situation ID (\code{"SL"}).}
#'   \item{context}{Who the respondent is meeting with: \code{"boss"},
#'     \code{"coworkers"}, or \code{"friends"}.}
#'   \item{vehicle}{Vehicle driven: \code{"a midsized SUV"},
#'     \code{"a small compact car"}, or \code{"a large truck"}.}
#'   \item{weather}{Weather: \code{"dry and sunny"}, \code{"gray and rainy"},
#'     or \code{"very foggy"}.}
#'   \item{traffic}{Traffic level: \code{"almost no other cars"} or
#'     \code{"many other cars"}.}
#'   \item{timePressure}{Schedule pressure: \code{"quite late"},
#'     \code{"a bit late"}, or \code{"comfortably ahead of schedule"}.}
#'   \item{policeFreq}{Police visibility history on the road:
#'     \code{"have regularly"}, \code{"have occasionally"}, or
#'     \code{"don't recall having ever"} seen police.}
#'   \item{scenario}{Full assembled scenario text as displayed to the
#'     respondent (the six factors interpolated into the template; see
#'     \code{$codebook$scenario$template}).}
#' }
#'
#' \strong{\code{$sit}} — situation-level perceptions (333 rows). Items
#' rated on the stem \emph{"How do you perceive this situation?"} on [0, 1]
#' (0 = Strongly Disagree, 1 = Strongly Agree):
#' \describe{
#'   \item{p}{Person ID.}
#'   \item{s}{Situation ID.}
#'   \item{Realistic}{\emph{The scenario is realistic.}}
#'   \item{Relatable}{\emph{A similar scenario could happen to me.}}
#' }
#'
#' \strong{\code{$traits}} — person-level self-report ratings and demographics
#' (333 rows). The 30 SR items use the stem \emph{"Describe how much you agree
#' with each of the statements below."} on [0, 1] (0 = Strongly disagree,
#' 1 = Strongly agree). See \code{$codebook$trait_items} for full wording,
#' and \code{$codebook$trait_scales} for the composite recipes that reproduce
#' the named scales (Big Five, value orientations, self-report traits) used
#' in Wood, Harms, & Cho (2023):
#' \describe{
#'   \item{p}{Person ID.}
#'   \item{SR_01, \ldots, SR_30}{Raw self-report items, each rescaled to
#'     [0, 1]. See \code{$codebook$trait_items}.}
#'   \item{employment, education, income, ethnicity, age, gender}{Demographics
#'     as raw Qualtrics codes; see \code{$codebook$demographics} for the
#'     mapping.}
#' }
#'
#' \strong{\code{$codebook}} — survey wording, scale anchors, factor levels,
#' and composite recipes. A named list with eight elements:
#' \describe{
#'   \item{outcomes}{Data frame (8 rows): variable name, full survey wording,
#'     [0, 1] scale anchor text, and shared stem for the eight expected-outcome
#'     items.}
#'   \item{Likelihood}{Data frame (1 row): wording and scale for the
#'     action-likelihood rating.}
#'   \item{actions}{Data frame (3 rows): action number, Speed code, and full
#'     action text as displayed.}
#'   \item{scenario}{List with two elements: \code{template} (the scenario
#'     text with bracketed factor placeholders) and \code{factors} (a data
#'     frame listing each factor and its observed levels).}
#'   \item{sit_items}{Data frame (2 rows): wording, scale, and stem for the
#'     situation-perception items.}
#'   \item{trait_items}{Data frame (30 rows): wording, scale, and stem for
#'     each of the 30 SR items.}
#'   \item{trait_scales}{Data frame (20 rows): for each named composite scale,
#'     the items it draws on (with \code{(1-SR_NN)} marking reverse-keyed
#'     items) and the number of items.}
#'   \item{demographics}{Data frame (6 rows): variable name, original survey
#'     wording, source Qualtrics qname, and a note that values are preserved
#'     as raw Qualtrics codes.}
#' }
#'
#' @details
#' \strong{Data cleaning.} The raw dataset contained 347 participants. Three
#' sequential data-quality screens were applied:
#' \enumerate{
#'   \item \strong{Straightlining} (\code{fancyr::strLine()}, cut at > 10\% of
#'     (person, action) rows showing zero variance across the 9 rated columns).
#'   \item \strong{Low overall variability} (\code{fancyr::prMaxSD()}, cut at
#'     < 0.25 of the maximum possible SD across all 27 rated cells).
#'   \item \strong{Response speed} (\code{fancyr::spi()} combined across the
#'     four speeding rating blocks, cut at < 1 second per item per Wood,
#'     Harms, Lowman, & DeSimone, 2017).
#' }
#' Respondents with missing timing data (a known intermittent Qualtrics
#' recording failure) are \emph{retained} — missingness on a screen is
#' treated as no evidence to drop. Final analytic sample:
#' \strong{N = 333} participants.
#'
#' \strong{Rescaling.} All Likert items are stored on a [0, 1] proportion-of-
#' maximum-possible (POMP) scale (\code{fancyr::pomp(x, smin = 1, smax = 5)}).
#' Differences between two actions on a [0, 1] outcome therefore fall on
#' [-1, 1], which is the natural metric for expectancy-value reasoning.
#'
#' \strong{Typical workflow.} Within-person deviate the Level-1 columns
#' before fitting path models:
#'
#' \preformatted{
#' data(speedingESJT)
#' L1 <- c("Speed","MoneyCost","FunDrive","IntQuality","Appropriate",
#'         "OnTime","Crash","Injured","Ticket","Likelihood")
#' dev <- speedingESJT$PSI
#' for (v in L1) dev[[v]] <- ave(dev[[v]], dev$p,
#'                               FUN = function(x) x - mean(x))
#' }
#'
#' \strong{Scoring named trait composites.} The composite recipes in
#' \code{$codebook$trait_scales} reproduce the named scales used in
#' Wood, Harms, & Cho (2023):
#'
#' \preformatted{
#' tr <- speedingESJT$traits
#' tr$Extra <- rowMeans(cbind(1 - tr$SR_15, tr$SR_16, tr$SR_17), na.rm = TRUE)
#' tr$Open  <- rowMeans(cbind(tr$SR_27, 1 - tr$SR_28, tr$SR_29), na.rm = TRUE)
#' tr$SRFastDriver <- tr$SR_30
#' }
#'
#' @source Wood, D., Harms, P. D., & Cho, S. (2023). Speeding study OSF
#'   materials. Raw data captured via Qualtrics; reproduced with permission
#'   for use as a canonical example in the \pkg{funfield} package.
#'
#' @references Wood, D., Harms, P., Lowman, G. H., & DeSimone, J. A. (2017).
#'   Response speed and response consistency as mutually validating indicators
#'   of data quality in online samples. \emph{Social Psychological and
#'   Personality Science, 8}(4), 454-464.
#'
#' @examples
#' data(speedingESJT)
#' ## Basic structure
#' lapply(speedingESJT, function(x) if (is.data.frame(x)) dim(x) else length(x))
#' head(speedingESJT$PSI)
#' table(speedingESJT$cond$context)
#'
#' ## Outcome variable wording
#' speedingESJT$codebook$outcomes[, c("var","label")]
#'
#' ## The scenario template and observed factor levels
#' speedingESJT$codebook$scenario
"speedingESJT"
