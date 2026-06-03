# andF() builds an n-way conjunction as a pairwise chain, and simulateF()
# resolves it (via aux + the topological derived-node settle) so five
# arbitrary-order prep actions gate a downstream brew.

test_that("andF builds the pairwise chain and names the derived nodes", {
  a <- andF("Prepped", c("HasWater", "HasGrounds", "HasFilter"))
  expect_equal(a$aux, c("Prepped1", "Prepped"))
  expect_match(a$model, "Prepped1 ~ 1 \\* HasWater:HasGrounds")
  expect_match(a$model, "Prepped ~ 1 \\* Prepped1:HasFilter")

  # two-way conjunction needs no intermediate
  b <- andF("Both", c("x", "y"))
  expect_equal(b$aux, "Both")
  expect_equal(b$model, "Both ~ 1 * x:y")

  expect_error(andF("Z", "x"))                 # needs >= 2 terms
})

test_that("blown-out prep: 5 parallel preps gate the brew, final L = .82", {
  preps    <- c("AddWater","AddGrounds","AddFilter","PlugIn","AddPot")
  haves    <- c("HasWater","HasGrounds","HasFilter","HasPower","HasPot")
  actions2 <- c(preps, "TurnOnCM", "Pour", "Sip")
  prepped  <- andF("Prepped", haves)

  field2 <- labelF(paste("
    HasWater   ~ 1 * AddWater + use * HasWater:TurnOnCM
    HasGrounds ~ 1 * AddGrounds
    HasFilter  ~ 1 * AddFilter
    HasPower   ~ 1 * PlugIn
    HasPot     ~ 1 * AddPot",
    prepped$model,
    "HCoPot ~ 1 * Prepped:TurnOnCM + use * HCoPot:Pour
     HCoCup ~ 1 * HCoPot:Pour      + use * HCoCup:Sip
     HCo    ~ 1 * HCoCup:Sip",
    costF(actions2, cost = -0.1),
    "L ~ 0.9 * HCo + 0.1 * Energy", sep = "\n"),
    actions = actions2)
  field2$params["use"] <- -1

  policy2 <- "AddWater ~ toWater
              AddGrounds ~ toGrounds
              AddFilter ~ toFilter
              PlugIn ~ toPower
              AddPot ~ toPot
              TurnOnCM ~ Prepped
              Pour ~ HCoPot
              Sip ~ HCoCup"
  todo <- c(toWater=1, toGrounds=1, toFilter=1, toPower=1, toPot=1)
  s_0 <- c(todo,
           setNames(rep(0, length(haves)), haves),
           setNames(rep(0, length(prepped$aux)), prepped$aux),
           setNames(rep(0, length(actions2)), actions2),
           HCoPot=0, HCoCup=0, HCo=0, Energy=0, L=0)

  out <- simulateF(field2$model, field2$params, s_0, policy2,
                   aux = prepped$aux, flows = names(todo), max_turns = 20)

  # all five preps fire (once each) before the brew
  expect_setequal(out$fired[1:5], preps)
  expect_equal(out$fired[6:8], c("TurnOnCM", "Pour", "Sip"))
  expect_equal(out$halted, "quiescent")

  fin <- out$trajectory[nrow(out$trajectory), ]
  expect_equal(unname(fin["HCo"]),    1)
  expect_equal(unname(fin["Energy"]), -0.8)   # eight actions
  expect_equal(unname(fin["L"]),      0.82)

  # Prepped is on once all five states held (just before the brew)
  expect_equal(unname(out$trajectory["t=5", "Prepped"]), 1)
})
