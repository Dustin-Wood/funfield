# Unified turn-based engine reproduces the coffee field's two answers from a
# single model: the temporal staircase AND the final afforded value, for both
# a complete plan and one that omits the pour.

field <- "
  s2     ~ 1 * make        + use * s2:TurnOn
  HCoPot ~ 1 * s2:TurnOn   + use * HCoPot:Pour
  HCoCup ~ 1 * HCoPot:Pour + use * HCoCup:Sip
  HCo    ~ 1 * HCoCup:Sip
  Energy ~ -0.1*make + -0.1*TurnOn + -0.1*Pour + -0.1*Sip
  L      ~ 0.9 * HCo + 0.1 * Energy
"
params <- c(use = -1)
s_0 <- c(choice = 1, make = 0, s2 = 0, TurnOn = 0, HCoPot = 0,
         Pour = 0, HCoCup = 0, Sip = 0, HCo = 0, Energy = 0, L = 0)

full   <- "make ~ choice
           TurnOn ~ s2
           Pour ~ HCoPot
           Sip ~ HCoCup"
faulty <- "make ~ choice
           TurnOn ~ s2
           Sip ~ HCoCup"

test_that("the full plan brews coffee, one action per turn, ending at L = .86", {
  out <- simulateF(field, params, s_0, full, flows = "choice")

  expect_equal(out$fired, c("make", "TurnOn", "Pour", "Sip"))
  expect_equal(out$halted, "quiescent")

  fin <- out$trajectory[nrow(out$trajectory), ]
  expect_equal(unname(fin["HCo"]),    1)
  expect_equal(unname(fin["Energy"]), -0.4)
  expect_equal(unname(fin["L"]),      0.86)

  # every object state is consumed as the next is produced; nothing piles up
  expect_equal(unname(fin[c("s2", "HCoPot", "HCoCup")]), c(0, 0, 0))
})

test_that("the faulty plan halts at the pot with negative value", {
  out <- simulateF(field, params, s_0, faulty, flows = "choice")

  expect_equal(out$fired, c("make", "TurnOn"))
  expect_equal(out$halted, "quiescent")

  fin <- out$trajectory[nrow(out$trajectory), ]
  expect_equal(unname(fin["HCoPot"]), 1)   # coffee stuck in the pot
  expect_equal(unname(fin["HCo"]),    0)   # never reaches the cup / hand
  expect_equal(unname(fin["Energy"]), -0.2)
  expect_equal(unname(fin["L"]),      -0.02)  # worse than doing nothing
})

test_that("each action fires exactly once", {
  out <- simulateF(field, params, s_0, full, flows = "choice")
  expect_true(all(table(out$fired) == 1))
})
