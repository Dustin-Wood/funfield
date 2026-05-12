#' Speeding ESJT Dataset
#'
#' @description
#' Long-format Elaborated Situational Judgment Test (ESJT) data on speeding
#' decisions, used in Wood, Harms, & Cho (2023) and as the canonical working
#' example in funfield documentation and tests.
#'
#' Each participant rated three actions (drive at the speed limit, drive 5 mph
#' over, drive 10+ mph over) on their expected outcomes (Crash, Injured,
#' Ticket, MoneyCost, OnTime, IntQuality, FunDrive, Appropriate) and the
#' likelihood that they would take the action.
#'
#' @format A data frame with 906 rows (302 persons x 3 actions) and 37 columns:
#' \describe{
#'   \item{p}{Person/respondent ID.}
#'   \item{s}{Situation ID (constant: \code{"SL"} for the speeding scenario).}
#'   \item{i}{Initiating action (text description of the speed chosen).}
#'   \item{Speed}{Action coded as continuous (-0.5 = at limit, 0 = +5 mph, 0.5 = +10 mph).}
#'   \item{Crash, Injured, Ticket, MoneyCost, OnTime, IntQuality, FunDrive, Appropriate}{
#'     Rated expected outcomes of the action (on a -1 to 1 scale).}
#'   \item{L}{Likelihood the respondent would take the action.}
#'   \item{gender2, age2, Income}{Demographic measures (between-person).}
#'   \item{Extra, Agree, Consc, Neuro, Open}{Big Five personality scales.}
#'   \item{vRules, vRelations, vPunctual, vStimulation, vDanger, vCommitments, vEmployed, vTime}{
#'     Value orientations (between-person).}
#'   \item{Thrifty, DrivingSkill, Worry, Lazy, TireEasily, Helpful, SRFastDriver}{
#'     Self-report trait measures (between-person).}
#'   \item{perSD}{Person-level standard deviation across the action ratings (data quality).}
#' }
#'
#' @details
#' This is the \strong{raw} long-format dataset. To fit within-person models
#' (the typical workflow), within-person deviate the Level 1 columns first:
#'
#' \preformatted{
#'   data(speedingESJT)
#'   L1cols <- c("Speed","Crash","Injured","Ticket","MoneyCost","OnTime",
#'               "IntQuality","FunDrive","Appropriate","L")
#'   dev <- speedingESJT
#'   for (v in L1cols) dev[[v]] <- ave(dev[[v]], dev$p, FUN = function(x) x - mean(x))
#' }
#'
#' @source Wood, D., Harms, P. D., & Cho, S. (2023). Speeding study OSF
#'   materials. Reproduced with permission for use as a canonical example
#'   in the \pkg{funfield} package.
#'
#' @examples
#' data(speedingESJT)
#' head(speedingESJT[, 1:13])
"speedingESJT"
