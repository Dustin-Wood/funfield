# plotField draws one field frame; here we lock in the two parallel ramps:
# a GREEN action staircase (plan edges + action nodes, labelled a1, a2, ...)
# and a GOLD->ORANGE condition staircase (gating moderators + the forces they
# open), kept clear of the red-white-blue value scale.

field <- labelF(paste("
  s2     ~ 1 * make        + use * s2:TurnOn
  HCoPot ~ 1 * s2:TurnOn   + use * HCoPot:Pour
  HCoCup ~ 1 * HCoPot:Pour + use * HCoCup:Sip
  HCo    ~ 1 * HCoCup:Sip",
  costF(c("make", "TurnOn", "Pour", "Sip")),
  "L ~ 0.9 * HCo + 0.1 * Energy", sep = "\n"),
  actions = c("make", "TurnOn", "Pour", "Sip"))
field$params["use"] <- -1

full <- "make ~ 1*choice
         TurnOn ~ 1*s2
         Pour ~ 1*HCoPot
         Sip ~ 1*HCoCup"
model_full <- paste(field$model, full, sep = "\n")

layout <- data.frame(
  name  = c("choice","make","s2","TurnOn","HCoPot","Pour","HCoCup","Sip",
            "HCo","Energy","L"),
  x     = c(0,2,3,5,6,8,9,11,12,12,14), y = c(0,0,2,2,4,4,6,6,8,0,0),
  shape = c("circle","rtTri","diamond","rtTri","diamond","rtTri","diamond",
            "rtTri","diamond","square","lfTri"),
  glyph = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,"bolt",NA),
  stringsAsFactors = FALSE)

# make has just fired (the first action, fully on) and rig set (s2, the first
# condition) is present: the action node should carry the green ramp's first
# (lime) colour, and the condition node the gold ramp's first colour.
s <- c(choice = 0, make = 1, s2 = 1, TurnOn = 0, HCoPot = 0, Pour = 0,
       HCoCup = 0, Sip = 0, HCo = 0, Energy = -0.1, L = 0)

test_that("plotField returns a buildable ggplot with the new role args", {
  skip_if_not_installed("ggplot2")
  g <- plotField(model_full, field$params, s, layout,
                 plan = full, plan_label = "a", plan_ramp = TRUE,
                 condition_ramp = TRUE)
  expect_s3_class(g, "ggplot")
  expect_silent(ggplot2::ggplot_build(g))
})

test_that("plan edges step-labelled; actions green, conditions gold/orange", {
  skip_if_not_installed("ggtext")
  g  <- plotField(model_full, field$params, s, layout,
                  plan = full, plan_label = "a", plan_ramp = TRUE,
                  condition_ramp = TRUE)
  bd <- ggplot2::ggplot_build(g)

  # The richtext layer holds the edge labels; the four plan rules should
  # appear as a1..a4.
  labs <- unlist(lapply(bd$data, function(d)
    if ("label" %in% names(d)) as.character(d$label)))
  expect_true(all(c("a1", "a2", "a3", "a4") %in% labs))

  # The just-fired first action (make) fills with the green ramp's lime end
  # (white -> #00FF00 at value 1); the active first condition (s2) fills with
  # the gold ramp's first colour (#FFDF00). Both are drawn as their own
  # constant-fill polygons, outside the value gradient.
  fills <- toupper(unlist(lapply(bd$data, function(d)
    if ("fill" %in% names(d)) as.character(d$fill))))
  expect_true("#00FF00" %in% fills)           # lime, make just fired
  expect_true("#F6BE00" %in% fills)           # gold, s2 present (ramp start)
})
