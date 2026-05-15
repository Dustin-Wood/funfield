## Build overtimeESJT via the general esjtFromQualtrics() builder.
##
## Source: xsjt$data + xsjt$colmap (from fancyr::fetch_survey_plus) — same
## raw survey as speedingESJT and dataHomeESJT (same respondents, different
## scenario block).
## Output: data/overtimeESJT.rda
##
## The work-overtime scenario randomly varied 8 features: who is asking
## (boss vs coworker, repeated through three template positions),
## weekday, prior plans, work difficulty, cash reward, and project impact.
## Two actions (iOvertime1, iOvertime2) and 9 expected outcomes per action.

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

overtimeESJT <- esjtFromQualtrics(
  data    = xsjt$data,
  colmap  = xsjt$colmap,
  scenario = "WO",

  action_text   = c("iOvertime1", "iOvertime2"),
  likelihood    = c("iOvertime_1", "iOvertime_2"),
  outcomes      = list(
    paste0("iOvertime1.Out_", 1:9),
    paste0("iOvertime2.Out_", 1:9)
  ),
  outcome_names = c("RelationshipQuality", "EmployerValuation",
                    "OutsideRelationships", "Drained", "CompanyHealth",
                    "Finances", "Appropriate", "RaisePromotion", "Punished"),
  action_code      = c(1, 0),   # iOvertime1 = agree (1), iOvertime2 = decline (0)
  action_code_name = "Overtime",

  scenario_text = "overtimeScenario",
  scenario_template = paste0(
    "${e://Field/WO.CoType} asks if you can work overtime on a project for ",
    "a couple hours tomorrow, a ${e://Field/WO.weekday}. ",
    "You ${e://Field/WO.priorPlans} at that time. ",
    "${e://Field/WO.CoType2} says that ${e://Field/WO.cashReward}. ",
    "It seems like the work would be ${e://Field/WO.workDifficulty}. ",
    "It ${e://Field/WO.projectImpact}."
  ),

  perceptions = c(Realistic = "Overtime.Real_1",
                  Relatable = "Overtime.Real_2"),

  traits      = paste0("General SR_", 1:30),
  trait_names = sprintf("SR_%02d", 1:30),

  demographics = c(employment = "Q4",
                   education  = "Q6",
                   income     = "Q7",
                   ethnicity  = "Q12",
                   age        = "Q13",
                   gender     = "Q14"),

  timing_blocks = list(
    list(first = "Q99_First Click", last = "Q99_Last Click", n = 2),  # likelihood
    list(first = "Q92_First Click", last = "Q92_Last Click", n = 9),  # action 1 outcomes
    list(first = "Q96_First Click", last = "Q96_Last Click", n = 9)   # action 2 outcomes
  )
)

overtimeESJT$traits$age <- as.numeric(overtimeESJT$traits$age)

## Trait scoring recipes (same 30 SR items shared across all three EXSJT
## scenarios; see Wood, Harms, & Cho, 2023).
overtimeESJT$codebook$trait_scales <- data.frame(
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

cat(sprintf("overtimeESJT built: %d persons, %d action rows.\n",
            length(unique(overtimeESJT$PSI$p)), nrow(overtimeESJT$PSI)))
cat("Elements:", paste(names(overtimeESJT), collapse = ", "), "\n")

if (!dir.exists("data")) dir.create("data")
save(overtimeESJT, file = "data/overtimeESJT.rda", compress = "xz")
cat("Saved data/overtimeESJT.rda\n")
