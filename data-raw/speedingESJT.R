## Build speedingESJT via the general esjtFromQualtrics() builder.
##
## Source: xsjt$data + xsjt$colmap (from fancyr::fetch_survey_plus)
## Output: data/speedingESJT.rda
##
## See also: R/esjtFromQualtrics.R, R/speedingESJT-data.R.

suppressMessages({
  devtools::load_all("C:/Users/dusti/Dropbox/R/fancyr", quiet = TRUE)
  devtools::load_all(".", quiet = TRUE)
})

src <- file.path(
  "C:/Users/dusti/Dropbox/Work/functional system model",
  "creating fields from ESJT data/experimental vignettes/EXSJT data & code",
  "xsjt data and column map.Rdata"
)
load(src)

speedingESJT <- esjtFromQualtrics(
  data    = xsjt$data,
  colmap  = xsjt$colmap,
  scenario = "SL",

  action_text   = c("iSpeed1", "iSpeed2", "iSpeed3"),
  likelihood    = c("iSpeeding_1", "iSpeeding_2", "iSpeeding_3"),
  outcomes      = list(
    paste0("iSpeed1.Out_", 1:8),
    paste0("iSpeed2.Out_", 1:8),
    paste0("iSpeed3.Out_", 1:8)
  ),
  outcome_names = c("MoneyCost", "FunDrive", "IntQuality", "Appropriate",
                    "OnTime",    "Crash",    "Injured",    "Ticket"),
  action_code      = c(0, 0.5, 1),
  action_code_name = "Speed",

  scenario_text = "SL.scenario",
  scenario_template = paste0(
    "You are driving on the freeway in ${e://Field/SL.vehicle}. ",
    "It is a ${e://Field/SL.weather} day with ${e://Field/SL.traffic} on the road. ",
    "You are running ${e://Field/SL.timePressure} for a meeting with your ",
    "${e://Field/SL.context}. The posted speed limit of 65 miles-per-hour, ",
    "and you ${e://Field/SL.policeFreq} seen police on this road."
  ),

  perceptions = c(Realistic = "Speeding.Real_1",
                  Relatable = "Speeding.Real_2"),

  traits      = paste0("General SR_", 1:30),
  trait_names = sprintf("SR_%02d", 1:30),

  demographics = c(employment = "Q4",
                   education  = "Q6",
                   income     = "Q7",
                   ethnicity  = "Q12",
                   age        = "Q13",
                   gender     = "Q14"),

  timing_blocks = list(
    list(first = "iSpeedingTimer_First Click", last = "iSpeedingTimer_Last Click", n = 3),
    list(first = "Q108_First Click",           last = "Q108_Last Click",           n = 8),
    list(first = "Q112_First Click",           last = "Q112_Last Click",           n = 8),
    list(first = "Q115_First Click",           last = "Q115_Last Click",           n = 8)
  )
)

## Coerce age to numeric (the demographics block returns it as raw chars).
speedingESJT$traits$age <- as.numeric(speedingESJT$traits$age)

## Speeding has named trait composites used in Wood, Harms, & Cho (2023).
## Append the scoring recipe to the codebook; users score with these.
speedingESJT$codebook$trait_scales <- data.frame(
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
)

cat(sprintf("speedingESJT built: %d persons, %d action rows.\n",
            length(unique(speedingESJT$PSI$p)), nrow(speedingESJT$PSI)))
cat("Elements:", paste(names(speedingESJT), collapse = ", "), "\n")

if (!dir.exists("data")) dir.create("data")
save(speedingESJT, file = "data/speedingESJT.rda", compress = "xz")
cat("Saved data/speedingESJT.rda\n")
