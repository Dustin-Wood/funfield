#' Coworker Assertiveness ESJT Dataset
#'
#' @description
#' Long-format Elaborated Situational Judgment Test (ESJT) data on assertiveness
#' decisions in workplace coworker interactions, bundled as a named list with
#' five linked elements (four data frames plus a codebook).
#'
#' Participants (N = 249 after quality screening) each rated 12 workplace
#' scenarios involving a coworker making an inappropriate request. For each
#' scenario, two possible responses (assertive vs. non-assertive action) were
#' rated on their expected outcomes and likelihood of being taken. The rank of
#' the coworker (subordinate, coworker, or manager) was randomly varied across
#' scenarios within each participant.
#'
#' @format A named list with five elements:
#'
#' \strong{\code{$PSI}} — person × situation × action ratings (5,846 rows).
#' Outcome columns use the stem
#' \emph{"If you do this... [action] ...how likely is this outcome to result?"}
#' on a −1 to 1 scale:
#' \describe{
#'   \item{p}{Person ID (Qualtrics response ID string).}
#'   \item{s}{Situation ID (integer 1–12).}
#'   \item{i}{Action ID, coded as \code{"s.a"} (e.g., \code{"1.1"} = scenario 1
#'     action 1, \code{"1.2"} = scenario 1 action 2).}
#'   \item{Appropriate}{\emph{Having acted appropriately within your role in the
#'     company.} (−1 to 1)}
#'   \item{ExpDisapproval}{\emph{Having directly/forcefully expressed disapproval
#'     with your [coworker]'s behavior.} (−1 to 1)}
#'   \item{MajorArg}{\emph{Having a major argument/confrontation with your
#'     [coworker].} (−1 to 1)}
#'   \item{GoodRelationP}{\emph{Having a good working relationship with your
#'     [coworker] in the long run.} (−1 to 1)}
#'   \item{Punished}{\emph{You being formally punished in some way (example:
#'     reprimanded or fired).} (−1 to 1)}
#'   \item{CompleteJob}{\emph{You completing your job responsibilities/work
#'     effectively in the long run.} (−1 to 1)}
#'   \item{P_Punished}{\emph{Your [coworker] being formally punished in some
#'     way (example: reprimanded or fired).} (−1 to 1)}
#'   \item{CompanySuccess}{\emph{The overall success of the company.} (−1 to 1)}
#'   \item{Likelihood}{\emph{How likely are you to do the following actions in
#'     response to this situation?} (−1 to 1)}
#' }
#'
#' \strong{\code{$cond}} — situational condition per person-scenario (2,988 rows):
#' \describe{
#'   \item{p}{Person ID.}
#'   \item{s}{Situation ID.}
#'   \item{condition}{Rank of the coworker in the scenario: \code{"subordinate"},
#'     \code{"coworker"}, or \code{"manager"}. Randomly varied across scenarios
#'     within each participant (approximately 4 scenarios per condition).}
#' }
#'
#' \strong{\code{$sit}} — situation-level perceptions, action-independent
#' (2,465 rows; ~82.5\% coverage of person × situation combinations).
#' Items use the stem \emph{"At this point in the situation, to what extent has
#' your [coworker] done something that…"} on a −1 to 1 scale
#' (−1 = Greatly Decreased, 0 = Negligible Effect, 1 = Greatly Increased):
#' \describe{
#'   \item{p}{Person ID.}
#'   \item{s}{Situation ID.}
#'   \item{HurtCoStanding}{\emph{Hurt your standing or status/reputation in
#'     the group/company.}}
#'   \item{WasUnethical}{\emph{Should be regarded as unethical, morally
#'     inappropriate, or "wrong".}}
#'   \item{Insulted}{\emph{Has insulted you or shown disrespect to you.}}
#'   \item{HurtCo}{\emph{Has or will hurt the efficiency or performance of
#'     the group/company.}}
#' }
#'
#' \strong{\code{$IIDL}} — person-level IIDL trait ratings (206 persons;
#' not all participants in \code{$PSI} completed this measure).
#' Items use the stem \emph{"How much does each term describe the way you
#' typically act, feel, or see yourself at work?"} on a −1 to 1 scale
#' (−1 = Very Uncharacteristic, 1 = Very Characteristic):
#' \describe{
#'   \item{p}{Person ID.}
#'   \item{IIDL_outgoing, IIDL_bold, \ldots}{40 IIDL (Interpersonal Identity
#'     Description List) trait adjective pairs (e.g., \emph{"bold, assertive"};
#'     \emph{"outgoing, sociable"}). See \code{$codebook$IIDL_items} for the
#'     full list.}
#' }
#'
#' \strong{\code{$codebook}} — survey wording and scale anchors, a named list
#' with four elements:
#' \describe{
#'   \item{outcomes}{Data frame (9 rows): \code{var}, \code{label} (full
#'     survey wording), \code{scale} (anchor text), \code{stem} (shared
#'     question stem for all outcome items).}
#'   \item{scenarios}{Data frame (12 rows): \code{s} (scenario number),
#'     \code{scenario} (vignette text, with \code{[X]} as the rank placeholder),
#'     \code{option_A} (assertive action), \code{option_B} (non-assertive
#'     action).}
#'   \item{sit_items}{Data frame (4 rows): \code{var}, \code{label},
#'     \code{scale}, \code{stem} for the situation-perception items.}
#'   \item{IIDL_items}{Data frame (40 rows): \code{var}, \code{item}
#'     (adjective pair wording), \code{scale}, \code{stem} for the IIDL
#'     personality items.}
#' }
#'
#' @details
#' \strong{Data cleaning.} The raw dataset contained 296 participants. Three
#' sequential screens were applied:
#' \enumerate{
#'   \item \strong{Straightlining} (\code{fancyr::strLine()}): 42 participants
#'     removed for zero-variance expected-outcome ratings on more than 10\%
#'     of their action ratings.
#'   \item \strong{Low variability} (\code{sdESJT()}): 2 additional
#'     participants removed for a within-person SD below 0.25 across the
#'     response scale (total removed by screens 1–2: 44).
#'   \item \strong{Condition balance}: 3 additional participants removed for
#'     not having rated at least one \emph{manager} and one \emph{subordinate}
#'     scenario, or for not having all 12 scenarios represented.
#' }
#' Final analytic sample: \strong{N = 249} participants.
#'
#' \strong{Typical workflow.} Within-person deviate the Level-1 columns
#' before fitting path models:
#'
#' \preformatted{
#' data(coworkerESJT)
#' L1cols <- c("Appropriate","ExpDisapproval","MajorArg","GoodRelationP",
#'             "Punished","CompleteJob","P_Punished","CompanySuccess","Likelihood")
#' PSI <- coworkerESJT$PSI
#' for (v in L1cols) PSI[[v]] <- ave(PSI[[v]], PSI$p,
#'                                   FUN = function(x) x - mean(x))
#' }
#'
#' \strong{Using the condition moderator.} Merge \code{$cond} first, then
#' deviate, so that within-person means stay at zero. The \code{pathXMY}
#' call uses \code{Z.within = TRUE} to within-person deviate the moderator
#' before fitting:
#'
#' \preformatted{
#' L1cols <- c("Appropriate","ExpDisapproval","MajorArg","GoodRelationP",
#'             "Punished","CompleteJob","P_Punished","CompanySuccess","Likelihood")
#' dat <- merge(coworkerESJT$PSI, coworkerESJT$cond, by = c("p","s"))
#' dat$manager <- as.numeric(dat$condition == "manager")
#' for (v in L1cols) dat[[v]] <- ave(dat[[v]], dat$p, FUN = function(x) x - mean(x))
#' ## Z = "manager" will be within-deviated automatically by pathXMY
#' res <- pathXMY(dat, X = "ExpDisapproval", Y = "Likelihood",
#'                M = "Appropriate", Z = "manager", Z.within = TRUE)
#' }
#'
#' @source Wood, D. (unpublished). Assertiveness and power in workplace
#'   interactions: An ESJT approach. Data collected via Qualtrics; reproduced
#'   for use as a canonical example in the \pkg{funfield} package.
#'
#' @examples
#' data(coworkerESJT)
#' ## Basic structure
#' lapply(coworkerESJT, dim)
#' head(coworkerESJT$PSI)
#' table(coworkerESJT$cond$condition)
#'
#' ## Survey wording for outcome variables
#' coworkerESJT$codebook$outcomes[, c("var", "label")]
#'
#' ## First scenario text and action options
#' coworkerESJT$codebook$scenarios[1, ]
"coworkerESJT"
