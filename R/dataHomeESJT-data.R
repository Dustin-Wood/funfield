#' Data-Home ESJT Dataset
#'
#' @description
#' Long-format Elaborated & eXperimental Situational Judgment Test (EXSJT)
#' data on the decision to skip or follow a security procedure when taking
#' company data home. Bundled as a named list mirroring
#' \code{\link{speedingESJT}} and \code{\link{overtimeESJT}}.
#'
#' The same Prolific respondents who completed the speeding and overtime
#' scenarios also rated this data-home scenario; the three datasets share a
#' participant pool and can be linked by \code{p}.
#'
#' Each participant (N = 332 after quality screening) rated two actions
#' (skip the password procedure; complete it) on seven expected outcomes
#' and on the likelihood of taking each action. Three features of the
#' situation were randomly varied at the person level.
#'
#' @format A named list with five elements:
#'
#' \strong{\code{$PSI}} — person × action ratings (664 rows = 332 persons ×
#' 2 actions). All Likert-rated columns are rescaled to [0, 1]:
#' \describe{
#'   \item{p}{Person ID.}
#'   \item{s}{Situation ID (constant: \code{"DH"}).}
#'   \item{i}{Action text as displayed.}
#'   \item{SkipProcedure}{Action coded on [0, 1]: \code{1} = skip the
#'     password procedure (insecure), \code{0} = complete it.}
#'   \item{Drained}{\emph{Your feeling of being drained/tired from work.}}
#'   \item{CustomerWellbeing}{\emph{The well-being of your company's
#'     customers.}}
#'   \item{CompanyWellbeing}{\emph{Your company's well-being.}}
#'   \item{Punished}{\emph{Likelihood of company firing/punishing you in
#'     the near future.}}
#'   \item{Appropriate}{\emph{Your sense that you acted appropriately.}}
#'   \item{PolicyViolation}{\emph{Likelihood of company seeing you as
#'     violating company rules/policies.}}
#'   \item{DataExposure}{\emph{Likelihood of non-authorized people getting
#'     access to the data.}}
#'   \item{Likelihood}{\emph{How likely you would be to do each action.}}
#' }
#'
#' \strong{\code{$cond}} — situation-level experimental factors (332 rows):
#' \describe{
#'   \item{p, s}{Person and situation IDs.}
#'   \item{selfResponseEfficacy}{Description of the password procedure (e.g.,
#'     \code{"fairly straightforward and easy, and very effective at making
#'     the data more secure"}).}
#'   \item{threatVulSev}{Vulnerability and severity of harm if data is
#'     exposed.}
#'   \item{sanctionCertainSev}{Certainty and severity of punishment if
#'     caught.}
#'   \item{scenario}{Full assembled scenario text as displayed.}
#' }
#'
#' \strong{\code{$sit}} — situation-level perceptions (332 rows): same
#' \code{Realistic} and \code{Relatable} items as the other two EXSJT
#' datasets.
#'
#' \strong{\code{$traits}} — person-level self-report items + demographics
#' (332 rows). Same 30 \code{SR_*} items and composite recipes as
#' \code{speedingESJT} and \code{overtimeESJT}.
#'
#' \strong{\code{$codebook}} — survey wording, scale anchors, factor levels,
#' and trait composite recipes.
#'
#' @details
#' \strong{Data cleaning.} Three sequential screens (same as the other two
#' EXSJT datasets). Final N = 332.
#'
#' \strong{Rescaling.} All Likert items rescaled from 1-5 to [0, 1] via
#' \code{fancyr::pomp()}.
#'
#' @source Wood, D., Harms, P. D., & Cho, S. (2023). EXSJT survey (Prolific
#'   panel); same respondents as \code{speedingESJT} and \code{overtimeESJT}.
#'
#' @examples
#' data(dataHomeESJT)
#' lapply(dataHomeESJT, function(x) if (is.data.frame(x)) dim(x) else length(x))
#' table(dataHomeESJT$cond$sanctionCertainSev)
#' dataHomeESJT$codebook$scenario$template
"dataHomeESJT"
