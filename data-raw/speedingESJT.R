## Build speedingESJT from the raw EXSJT data + column map.
##
## Source: xsjt$data and xsjt$colmap, produced by fancyr::fetch_survey_plus
##         and saved at:
##           "C:/Users/dusti/Dropbox/Work/functional system model/
##            creating fields from ESJT data/experimental vignettes/
##            EXSJT data & code/xsjt data and column map.Rdata"
##
## Output: data/speedingESJT.rda — a named list mirroring coworkerESJT's shape:
##   $PSI       — action-level ratings (one row per person-action)
##   $cond      — situation-level experimental factors (one row per person)
##   $sit       — situation-level perceptions (one row per person)
##   $traits    — person-level raw SR items + demographics
##   $codebook  — variable wording, scale anchors, factor levels, composite recipes
##
## All Likert-rated columns are rescaled from the original 1-5 Qualtrics codes
## to a [0, 1] proportion-of-maximum-possible (POMP) scale via fancyr::pomp().
## The codebook stores anchor text on the rescaled [0, 1] metric.

## Load dev fancyr (for spi(), strLine(), prMaxSD(), pomp())
suppressMessages(devtools::load_all("C:/Users/dusti/Dropbox/R/fancyr", quiet = TRUE))

## ---- 1. Load source -------------------------------------------------------
src <- file.path(
  "C:/Users/dusti/Dropbox/Work/functional system model",
  "creating fields from ESJT data/experimental vignettes/EXSJT data & code",
  "xsjt data and column map.Rdata"
)
load(src)
stopifnot(exists("xsjt"), is.list(xsjt), all(c("data","colmap") %in% names(xsjt)))
xd <- xsjt$data
cm <- xsjt$colmap

## ---- 2. Person ID ---------------------------------------------------------
xd$p <- xd$ResponseId
xd <- xd[!is.na(xd$p) & nzchar(xd$p), ]
cat("Raw respondents:", nrow(xd), "\n")

## Helpers: strip haven_labelled/SPSS attrs before coercion
.numeric <- function(x) as.numeric(unclass(x))
.chr     <- function(x) as.character(unclass(x))
poMP     <- function(x) fancyr::pomp(.numeric(x), smin = 1, smax = 5)
xd$p     <- .chr(xd$p)

## ---- 3. Build $PSI (long: 3 rows per person) ------------------------------
out_map <- c(
  Out_1 = "MoneyCost",
  Out_2 = "FunDrive",
  Out_3 = "IntQuality",
  Out_4 = "Appropriate",
  Out_5 = "OnTime",
  Out_6 = "Crash",
  Out_7 = "Injured",
  Out_8 = "Ticket"
)
## Action speed coding on [0,1]: at-limit=0, +5 mph=0.5, +10+ mph=1
speed_code <- c(`1` = 0, `2` = 0.5, `3` = 1)

build_action <- function(a) {
  raw <- xd[paste0("iSpeed", a, ".Out_", 1:8)]
  out_pomp <- as.data.frame(lapply(raw, poMP), check.names = FALSE)
  names(out_pomp) <- unname(out_map)
  data.frame(
    p          = xd$p,
    s          = "SL",
    i          = .chr(xd[[paste0("iSpeed", a)]]),
    Speed      = unname(speed_code[as.character(a)]),
    out_pomp,
    Likelihood = poMP(xd[[paste0("iSpeeding_", a)]]),
    stringsAsFactors = FALSE
  )
}
PSI <- do.call(rbind, lapply(1:3, build_action))
PSI <- PSI[order(PSI$p, PSI$Speed), ]
rownames(PSI) <- NULL

## ---- 4. Build $cond (one row per person) ----------------------------------
cond <- data.frame(
  p            = xd$p,
  s            = "SL",
  context      = .chr(xd$SL.context),
  vehicle      = .chr(xd$SL.vehicle),
  weather      = .chr(xd$SL.weather),
  traffic      = .chr(xd$SL.traffic),
  timePressure = .chr(xd$SL.timePressure),
  policeFreq   = .chr(xd$SL.policeFreq),
  scenario     = .chr(xd$SL.scenario),
  stringsAsFactors = FALSE
)

## ---- 5. Build $sit (one row per person) -----------------------------------
sit <- data.frame(
  p         = xd$p,
  s         = "SL",
  Realistic = poMP(xd$Speeding.Real_1),
  Relatable = poMP(xd$Speeding.Real_2),
  stringsAsFactors = FALSE
)

## ---- 6. Build $traits (raw 30 SR items + demographics) --------------------
sr_cols  <- paste0("General SR_", 1:30)
sr_pomp  <- as.data.frame(lapply(xd[sr_cols], poMP), check.names = FALSE)
names(sr_pomp) <- sprintf("SR_%02d", 1:30)

traits <- data.frame(
  p          = xd$p,
  sr_pomp,
  employment = .chr(xd$Q4),
  education  = .chr(xd$Q6),
  income     = .chr(xd$Q7),
  ethnicity  = .chr(xd$Q12),
  age        = .numeric(xd$Q13),
  gender     = .chr(xd$Q14),
  stringsAsFactors = FALSE
)

## ---- 7. Data-quality screens ----------------------------------------------
ratecols <- c("MoneyCost","FunDrive","IntQuality","Appropriate",
              "OnTime","Crash","Injured","Ticket","Likelihood")

## (a) Straight-lining: proportion of (person,action) rows with zero variance
##     across the 9 rated columns. Wood et al. (2017) suggested cut: > 0.10
str_p <- fancyr::strLine(PSI[, ratecols], group = PSI$p)

## (b) Low within-person variability across all 27 rated cells (3 actions ×
##     9 items). prMaxSD < 0.25 is the suggested cut.
psi_wide <- do.call(rbind, lapply(split(PSI[, ratecols], PSI$p), function(d) {
  as.numeric(as.matrix(d))
}))
prmax_p <- fancyr::prMaxSD(psi_wide, smin = 0, smax = 1)

## (c) Response speed across the four speeding rating blocks.
##     iSpeedingTimer_* = 3-item likelihood; Q108/Q112/Q115_* = 8-item outcome
##     blocks for iSpeed1/2/3. Combined SPI = total elapsed / total transitions.
blocks <- list(
  list(first = "iSpeedingTimer_First Click", last = "iSpeedingTimer_Last Click", n = 3),
  list(first = "Q108_First Click",           last = "Q108_Last Click",           n = 8),
  list(first = "Q112_First Click",           last = "Q112_Last Click",           n = 8),
  list(first = "Q115_First Click",           last = "Q115_Last Click",           n = 8)
)
total_time  <- rowSums(do.call(cbind, lapply(blocks, function(b) {
  .numeric(xd[[b$last]]) - .numeric(xd[[b$first]])
})), na.rm = FALSE)
total_trans <- sum(sapply(blocks, function(b) b$n - 1))  # 2 + 7 + 7 + 7 = 23
spi_combined <- total_time / total_trans
names(spi_combined) <- xd$p

## Combine flags. Treat NA in any flag as 'no evidence to drop' (keep).
## Qualtrics occasionally fails to record page-timing data, and we don't want
## those respondents penalized for a recording failure.
flag_str  <- str_p[xd$p]   > 0.10
flag_var  <- prmax_p[xd$p] < 0.25
flag_spi  <- spi_combined  < 1
flag_str[is.na(flag_str)] <- FALSE
flag_var[is.na(flag_var)] <- FALSE
flag_spi[is.na(flag_spi)] <- FALSE
keep <- !(flag_str | flag_var | flag_spi)

cat(sprintf(
  "Screens: strLine>0.10 fails=%d | prMaxSD<0.25 fails=%d | SPI<1 fails=%d | any-fail=%d\n",
  sum(flag_str), sum(flag_var), sum(flag_spi), sum(!keep)
))
cat(sprintf("Retained: %d of %d respondents\n", sum(keep), length(keep)))

keep_p <- xd$p[keep]
PSI    <- PSI[PSI$p %in% keep_p, ]
cond   <- cond[cond$p %in% keep_p, ]
sit    <- sit[sit$p %in% keep_p, ]
traits <- traits[traits$p %in% keep_p, ]

## ---- 8. Build $codebook ---------------------------------------------------
## Anchor strings on the rescaled [0, 1] metric.
ag5  <- "0 = Strongly disagree | 0.25 = Somewhat disagree | 0.50 = Equally agree/disagree | 0.75 = Somewhat agree | 1.00 = Strongly agree"
real5 <- "0 = Strongly Disagree | 0.25 = Disagree | 0.50 = Equally Agree/Disagree | 0.75 = Agree | 1.00 = Strongly Agree"
lik5 <- "0 = Almost definitely would not | 0.25 = Probably would not | 0.50 = About equally likely/unlikely | 0.75 = Probably would | 1.00 = Almost definitely would"
out5 <- "0 = Very Low | 0.25 = Low | 0.50 = Moderate | 0.75 = High | 1.00 = Very High"

## Pull SR item wording straight from colmap
sr_label <- vapply(sr_cols, function(q) cm$sub[cm$qname == q], character(1))

codebook <- list(
  outcomes = data.frame(
    var   = unname(out_map),
    label = c(
      "The total financial cost of your trip",
      "The amount of enjoyment experienced from driving on this trip",
      "The quality of your next meeting with your [SL.context]",
      "Your sense that you acted appropriately",
      "Likelihood of getting to your meeting on time",
      "Likelihood of getting into a car crash",
      "Likelihood of having a serious injury",
      "Likelihood of getting a speeding ticket"
    ),
    scale = out5,
    stem  = "Describe the level of these outcomes that you expect would result from deciding to [action] in this situation:",
    stringsAsFactors = FALSE
  ),
  Likelihood = data.frame(
    var   = "Likelihood",
    label = "How likely you would be to do each action in this situation",
    scale = lik5,
    stem  = "Below are a couple different ways you could respond to this situation. Please describe how likely you would be to do each of the actions below if in this situation:",
    stringsAsFactors = FALSE
  ),
  actions = data.frame(
    action = 1:3,
    Speed  = c(0, 0.5, 1),
    text   = c(
      "drive no more than the speed limit (65 mph)",
      "drive as much as 5 miles per hour over the speed limit (70 miles per hour)",
      "drive more than 10 miles per hour over the speed limit (75+ miles per hour)"
    ),
    stringsAsFactors = FALSE
  ),
  scenario = list(
    template = paste0(
      "You are driving on the freeway in [vehicle]. It is a [weather] day with [traffic] ",
      "on the road. You are running [timePressure] for a meeting with your [context]. ",
      "The posted speed limit of 65 miles-per-hour, and you [policeFreq] seen police on this road."
    ),
    factors = data.frame(
      var = c("vehicle","weather","traffic","timePressure","context","policeFreq"),
      levels = c(
        "a midsized SUV; a small compact car; a large truck",
        "dry and sunny; gray and rainy; very foggy",
        "almost no other cars; many other cars",
        "quite late; a bit late; comfortably ahead of schedule",
        "boss; coworkers; friends",
        "have regularly; have occasionally; don't recall having ever"
      ),
      stringsAsFactors = FALSE
    )
  ),
  sit_items = data.frame(
    var   = c("Realistic","Relatable"),
    label = c("The scenario is realistic.",
              "A similar scenario could happen to me."),
    scale = real5,
    stem  = "How do you perceive this situation?",
    stringsAsFactors = FALSE
  ),
  trait_items = data.frame(
    var   = sprintf("SR_%02d", 1:30),
    label = unname(sr_label),
    scale = ag5,
    stem  = "Describe how much you agree with each of the statements below.",
    stringsAsFactors = FALSE
  ),
  ## A recipe table for the named composites used in Wood/Harms/Cho 2023.
  ## "items" is an expression you can mean() to score (where 1 - x reverses).
  trait_scales = data.frame(
    scale = c("vRules","vRelations","vPunctual","vStimulation","vDanger",
              "Thrifty","DrivingSkill","Worry","vCommitments","vEmployed",
              "Lazy","vTime","TireEasily","Helpful",
              "Extra","Agree","Consc","Neuro","Open","SRFastDriver"),
    items = c("SR_01","SR_02","SR_03","SR_04","SR_05",
              "SR_06","SR_07","SR_08","SR_09","SR_10",
              "SR_11","SR_12","SR_13","SR_14",
              "(1-SR_15) + SR_16 + SR_17",
              "SR_18 + (1-SR_19) + SR_20",
              "(1-SR_21) + (1-SR_22) + SR_23",
              "SR_24 + SR_25 + (1-SR_26)",
              "SR_27 + (1-SR_28) + SR_29",
              "SR_30"),
    n_items = c(rep(1, 14), rep(3, 5), 1),
    stringsAsFactors = FALSE
  ),
  demographics = data.frame(
    var   = c("employment","education","income","ethnicity","age","gender"),
    label = c(
      "What is your current employment status?",
      "Please indicate the highest level of education completed.",
      "What is your personal income?",
      "How would you best describe your ethnic or racial background?",
      "What is your age?",
      "What is your gender?"
    ),
    qualtrics = c("Q4","Q6","Q7","Q12","Q13","Q14"),
    note = "Values preserved as raw Qualtrics codes; see colmap for level labels.",
    stringsAsFactors = FALSE
  )
)

## ---- 9. Assemble and save -------------------------------------------------
speedingESJT <- list(
  PSI      = PSI,
  cond     = cond,
  sit      = sit,
  traits   = traits,
  codebook = codebook
)

cat("\nFinal structure:\n")
cat(sprintf("  $PSI:    %d rows x %d cols (%d persons x 3 actions)\n",
            nrow(PSI), ncol(PSI), length(unique(PSI$p))))
cat(sprintf("  $cond:   %d rows x %d cols\n", nrow(cond), ncol(cond)))
cat(sprintf("  $sit:    %d rows x %d cols\n", nrow(sit), ncol(sit)))
cat(sprintf("  $traits: %d rows x %d cols\n", nrow(traits), ncol(traits)))
cat(sprintf("  $codebook: %d elements (%s)\n",
            length(codebook), paste(names(codebook), collapse = ", ")))

if (!dir.exists("data")) dir.create("data")
save(speedingESJT, file = "data/speedingESJT.rda", compress = "xz")
cat("\nSaved data/speedingESJT.rda\n")
