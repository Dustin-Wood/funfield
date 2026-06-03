# chooseF runs each policy on one field to quiescence and ranks them by the
# readout, with cost making a wasteful failed plan lose to inaction.

field <- labelF(paste("
  s2     ~ 1 * make        + use * s2:TurnOn
  HCoPot ~ 1 * s2:TurnOn   + use * HCoPot:Pour
  HCoCup ~ 1 * HCoPot:Pour + use * HCoCup:Sip
  HCo    ~ 1 * HCoCup:Sip",
  costF(c("make", "TurnOn", "Pour", "Sip")),
  "L ~ 0.9 * HCo + 0.1 * Energy", sep = "\n"),
  actions = c("make", "TurnOn", "Pour", "Sip"))
field$params["use"] <- -1

s_0 <- c(choice = 1, make = 0, s2 = 0, TurnOn = 0, HCoPot = 0,
         Pour = 0, HCoCup = 0, Sip = 0, HCo = 0, Energy = 0, L = 0)
full   <- "make ~ choice
           TurnOn ~ s2
           Pour ~ HCoPot
           Sip ~ HCoCup"
faulty <- "make ~ choice
           TurnOn ~ s2
           Sip ~ HCoCup"

test_that("chooseF ranks full > do-nothing > faulty and flags the winner", {
  tab <- chooseF(field$model, field$params, s_0,
                 policies = list(full = full, faulty = faulty), flows = "choice")

  expect_equal(tab$plan, c("full", "faulty", "(do nothing)"))
  expect_equal(tab$L, c(0.86, -0.02, 0))
  expect_equal(tab$chosen, c(TRUE, FALSE, FALSE))

  # full nets coffee; faulty leaves it in the pot; nothing reaches nothing
  expect_equal(tab$HCo,    c(1, 0, 0))
  expect_equal(tab$HCoPot, c(0, 1, 0))
})
