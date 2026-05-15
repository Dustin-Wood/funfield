#' Work-Overtime ESJT Dataset
#'
#' @description
#' Long-format Elaborated & eXperimental Situational Judgment Test (EXSJT)
#' data on work-overtime decisions. Bundled as a named list with five
#' linked elements (four data frames plus a codebook).
#'
#' The same Prolific respondents who completed the speeding and data-home
#' scenarios also rated the work-overtime scenario described here; the
#' three datasets (\code{\link{speedingESJT}}, \code{overtimeESJT},
#' \code{\link{dataHomeESJT}}) share a participant pool and can be linked
#' by \code{p} to examine within-person consistency across domains.
#'
#' Each participant (N = 329 after quality screening) rated two actions
#' (agree to work overtime; decline) on nine expected outcomes and on the
#' likelihood of taking each action. Eight features of the situation were
#' randomly varied at the person level (one fully crossed scenario per
#' respondent).
#'
#' @format A named list with five elements:
#'
#' \strong{\code{$PSI}} — person × action ratings (658 rows = 329 persons ×
#' 2 actions). All Likert-rated columns are rescaled to [0, 1]:
#' \describe{
#'   \item{p}{Person ID (Qualtrics response ID string).}
#'   \item{s}{Situation ID (constant: \code{"WO"}).}
#'   \item{i}{Action text as displayed (\code{"agree to work overtime"} or
#'     \code{"decline to work overtime"}).}
#'   \item{Overtime}{Action coded on [0, 1]: \code{0} = decline,
#'     \code{1} = agree.}
#'   \item{RelationshipQuality}{\emph{The quality of your relationship with
#'     [coworker/boss].}}
#'   \item{EmployerValuation}{\emph{The company's understanding of your
#'     value as an employee.}}
#'   \item{OutsideRelationships}{\emph{The quality of your relationships
#'     with people outside of work.}}
#'   \item{Drained}{\emph{Your feeling of being drained/tired from work.}}
#'   \item{CompanyHealth}{\emph{The health of the company.}}
#'   \item{Finances}{\emph{Your overall financial well-being.}}
#'   \item{Appropriate}{\emph{Your sense that you had acted appropriately.}}
#'   \item{RaisePromotion}{\emph{Likelihood of company giving you a raise
#'     or promotion in the near future.}}
#'   \item{Punished}{\emph{Likelihood of company firing/punishing you in
#'     the near future.}}
#'   \item{Likelihood}{\emph{How likely you would be to do each action.}}
#' }
#'
#' \strong{\code{$cond}} — situation-level experimental factors (329 rows):
#' \describe{
#'   \item{p, s}{Person and situation IDs.}
#'   \item{CoType}{Who is asking: \code{"A coworker"} or \code{"Your boss"}.}
#'   \item{CoType2, CoType3}{Linked variants of \code{CoType} used at later
#'     positions in the scenario template (e.g., \code{"The coworker"},
#'     \code{"your boss"}). Same underlying assignment as \code{CoType}.}
#'   \item{weekday}{\code{"Friday evening"} or \code{"Wednesday evening"}.}
#'   \item{priorPlans}{\code{"don't have any plans"} or
#'     \code{"have previously made plans to meet with friends"}.}
#'   \item{workDifficulty}{\code{"difficult"} or \code{"fairly easy"}.}
#'   \item{cashReward}{Whether overtime pay is offered.}
#'   \item{projectImpact}{Whether the project's value to the company is
#'     clear.}
#'   \item{scenario}{Full assembled scenario text as displayed to the
#'     respondent; see \code{$codebook$scenario$template} for the canonical
#'     Qualtrics template with placeholders.}
#' }
#'
#' \strong{\code{$sit}} — situation-level perceptions (329 rows):
#' \describe{
#'   \item{p, s}{Person and situation IDs.}
#'   \item{Realistic}{\emph{The scenario is realistic.}}
#'   \item{Relatable}{\emph{A similar scenario could happen to me.}}
#' }
#'
#' \strong{\code{$traits}} — person-level self-report items + demographics
#' (329 rows). \code{SR_01}–\code{SR_30} are the 30 raw self-report items
#' (shared across all three EXSJT scenarios); see
#' \code{$codebook$trait_items} for wording and \code{$codebook$trait_scales}
#' for the composite recipes used in Wood, Harms, & Cho (2023).
#'
#' \strong{\code{$codebook}} — survey wording, scale anchors, factor levels,
#' and trait composite recipes. See \code{\link{speedingESJT}} for full
#' structural detail.
#'
#' @details
#' \strong{Data cleaning.} Three sequential data-quality screens (same as
#' \code{speedingESJT}): \code{fancyr::strLine()} > 0.10,
#' \code{fancyr::prMaxSD()} < 0.25, and combined \code{fancyr::spi()} < 1
#' across the three rating blocks (likelihood + two outcome blocks).
#' Respondents with missing timing data are retained. Final N = 329.
#'
#' \strong{Rescaling.} All Likert items rescaled from the original 1-5
#' Qualtrics codes to [0, 1] via \code{fancyr::pomp()}.
#'
#' @source Wood, D., Harms, P. D., & Cho, S. (2023). EXSJT survey (Prolific
#'   panel); same respondents as \code{speedingESJT} and \code{dataHomeESJT}.
#'
#' @examples
#' data(overtimeESJT)
#' lapply(overtimeESJT, function(x) if (is.data.frame(x)) dim(x) else length(x))
#' table(overtimeESJT$cond$CoType)
#' overtimeESJT$codebook$scenario$template
"overtimeESJT"
