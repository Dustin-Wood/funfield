## Build dataHomeESJT via the general esjtFromQualtrics() builder.
##
## Source: xsjt$data + xsjt$colmap (from fancyr::fetch_survey_plus) — same
## raw survey as speedingESJT and overtimeESJT.
## Output: data/dataHomeESJT.rda
##
## The data-home scenario randomly varied 3 features: response efficacy of
## the password procedure (selfResponseEfficacy), threat vulnerability and
## severity if data leaks (threatVulSev), and sanction certainty and severity
## if caught (sanctionCertainSev). Two actions (iDataHome1, iDataHome2) and
## 7 expected outcomes per action.

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

dataHomeESJT <- esjtFromQualtrics(
  data    = xsjt$data,
  colmap  = xsjt$colmap,
  scenario = "DH",

  action_text   = c("iDataHome1", "iDataHome2"),
  likelihood    = c("iDataHome_1", "iDataHome_2"),
  outcomes      = list(
    paste0("iDataHome1.Out_", 1:7),
    paste0("iDataHome2.Out_", 1:7)
  ),
  outcome_names = c("Drained", "CustomerWellbeing", "CompanyWellbeing",
                    "Punished", "Appropriate", "PolicyViolation",
                    "DataExposure"),
  action_code      = c(1, 0),   # iDataHome1 = skip procedure (1), iDataHome2 = follow (0)
  action_code_name = "SkipProcedure",

  scenario_text = "DH.scenario",
  scenario_template = paste0(
    "You are working with data about your company's customers, and want to ",
    "continue your work from home. Your company requires you to go through ",
    "a procedure to apply a password to data before taking it out of the ",
    "office. You find this procedure is ${e://Field/DH.selfResponseEfficacy}. ",
    "You believe that if you *don't* complete the password procedure, ",
    "unauthorized people will ${e://Field/DH.threatVulSev}. You believe the ",
    "chances of the company knowing if you skipped the procedure are ",
    "${e://Field/DH.sanctionCertainSev}."
  ),

  perceptions = c(Realistic = "DataHome.Real_1",
                  Relatable = "DataHome.Real_2"),

  traits      = paste0("General SR_", 1:30),
  trait_names = sprintf("SR_%02d", 1:30),

  demographics = c(employment = "Q4",
                   education  = "Q6",
                   income     = "Q7",
                   ethnicity  = "Q12",
                   age        = "Q13",
                   gender     = "Q14"),

  timing_blocks = list(
    list(first = "Q88_First Click", last = "Q88_Last Click", n = 2),  # likelihood
    list(first = "Q80_First Click", last = "Q80_Last Click", n = 7),  # action 1 outcomes
    list(first = "Q85_First Click", last = "Q85_Last Click", n = 7)   # action 2 outcomes
  )
)

dataHomeESJT$traits$age <- as.numeric(dataHomeESJT$traits$age)

## Trait scoring recipes (same 30 SR items shared across all three EXSJT
## scenarios; see Wood, Harms, & Cho, 2023).
dataHomeESJT$codebook$trait_scales <- data.frame(
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

cat(sprintf("dataHomeESJT built: %d persons, %d action rows.\n",
            length(unique(dataHomeESJT$PSI$p)), nrow(dataHomeESJT$PSI)))
cat("Elements:", paste(names(dataHomeESJT), collapse = ", "), "\n")

if (!dir.exists("data")) dir.create("data")
save(dataHomeESJT, file = "data/dataHomeESJT.rda", compress = "xz")
cat("Saved data/dataHomeESJT.rda\n")
