## Reproduction test: pathXMY() against the hand-written speeding script
## Confirms the new umbrella function recovers the same numbers we already
## stress-tested in stress_test/01_*.log.

## Use embedded dataset (raw); deviate inside.
suppressPackageStartupMessages({
  library(lavaan)
})

## Source the new function (avoid devtools::load_all and its rlang version dep)
source("R/pathXMY.R")
load("data/speedingESJT.rda")
L1 <- c("Speed","Crash","Injured","Ticket","MoneyCost","OnTime",
        "IntQuality","FunDrive","Appropriate","L")
dev <- speedingESJT
for (v in L1) dev[[v]] <- ave(dev[[v]], dev$p, FUN = function(x) x - mean(x))

cat("\n==== TEST 1: Direct model, no moderator (M = NULL, Z = NULL) ====\n")
res <- pathXMY(dev, X = "Speed", Y = "L")
print(res$tidy)

cat("\n==== TEST 2: Direct model, moderated (M = NULL, Z given) ====\n")
res <- pathXMY(dev, X = "Speed", Y = "L", Z = "SRFastDriver")
print(res$tidy)
## This is the TOTAL effect of X on Y (and Z*X on Y) when no mediator is in
## the model. It should equal direct + indirect effects from the mediation
## model. Sanity check: total Z-moderation ~ BZ_YX + indZ_X + indZ_Y from
## the full mediation model (see TEST 4 numbers): 0.228 + 0.036 - 0.023 = 0.241
cat("[sanity check] Total Z-moderation should ~= BZ_YX + indZ_X + indZ_Y\n")
cat("  from TEST 4: 0.228 + 0.036 - 0.023 = 0.241 (compare to BZ_YX above)\n")

cat("\n==== TEST 3: Unmoderated mediation (M = 'Crash', Z = NULL) ====\n")
res <- pathXMY(dev, X = "Speed", Y = "L", M = "Crash")
print(res$tidy)
## Expected from speeding script (NoModMedModel with M = Crash):
##   B1_MX (B1_1A): 0.314  SE .016  (script reports it under cluster=p)
##   B1_YX (B1_LA): -0.075 SE .040
##   B1_YM (B1_L1): -0.662 SE .076
##   ind:           -0.208 SE .026
cat("[expected from script (NoModMedModel, M=Crash):]\n")
cat("  B1_MX:  0.314 (.016)\n")
cat("  B1_YX: -0.075 (.040)\n")
cat("  B1_YM: -0.662 (.076)\n")
cat("  ind:   -0.208 (.026)\n")

cat("\n==== TEST 4: Moderated mediation, single mediator ====\n")
res <- pathXMY(dev, X = "Speed", Y = "L", M = "Crash", Z = "SRFastDriver")
print(res$tidy)
## Expected from stress test / speeding script:
##   B1_MX:  0.314 (.015)
##   BZ_MX: -0.065 (.015)
##   B1_YX: -0.117 (.035)
##   BZ_YX:  0.228 (.034)
##   B1_YM: -0.546 (.072)
##   BZ_YM: -0.074 (.070)
##   indZ_X (was indByZE):  0.036 (.010)
##   indZ_Y (was indByZV): -0.023 (.022)
cat("[expected from speeding script / stress test 01:]\n")
cat("  B1_MX:  0.314 (.015)\n")
cat("  BZ_MX: -0.065 (.015)\n")
cat("  B1_YX: -0.117 (.035)\n")
cat("  BZ_YX:  0.228 (.034)\n")
cat("  B1_YM: -0.546 (.072)\n")
cat("  BZ_YM: -0.074 (.070)\n")
cat("  ind:   ~ -0.171 (the unmoderated indirect at Z = 0)\n")
cat("  indZ_X: 0.036 (.010)\n")
cat("  indZ_Y: -0.023 (.022)\n")

cat("\n==== TEST 5: Moderated mediation looped over 8 mediators ====\n")
mediators <- c("Crash","Injured","Ticket","MoneyCost","OnTime",
               "IntQuality","FunDrive","Appropriate")
res <- pathXMY(dev, X = "Speed", Y = "L", M = mediators, Z = "SRFastDriver")
cat("\n-- BZ_MX across mediators --\n")
print(subset(res$tidy, param == "BZ_MX")[, c("mediator","est","se","z","pvalue")],
      row.names = FALSE)
cat("\n-- indZ_X (Z-moderation of indirect via expectation path) --\n")
print(subset(res$tidy, param == "indZ_X")[, c("mediator","est","se","z","pvalue")],
      row.names = FALSE)
cat("\n-- indZ_Y (Z-moderation of indirect via valuation path) --\n")
print(subset(res$tidy, param == "indZ_Y")[, c("mediator","est","se","z","pvalue")],
      row.names = FALSE)

cat("\n==== TEST 6: Bootstrap SE mode ====\n")
res <- pathXMY(dev, X = "Speed", Y = "L", M = "Crash", Z = "SRFastDriver",
               se = "boot", nboot = 200)
print(res$tidy)

cat("\n==== TEST 7: Low-cluster warning ====\n")
sub <- dev[dev$p %in% sample(unique(dev$p), 20), ]
res <- tryCatch(
  pathXMY(sub, X = "Speed", Y = "L", M = "Crash", Z = "SRFastDriver"),
  warning = function(w) { cat("warning fired: ", w$message, "\n"); NULL }
)

cat("\n==== TEST 8: Deviation-check warning ====\n")
res <- tryCatch(
  pathXMY(speedingESJT, X = "Speed", Y = "L", M = "Crash", Z = "SRFastDriver"),
  warning = function(w) { cat("warning fired: ", w$message, "\n"); NULL }
)

cat("\nDONE\n")
