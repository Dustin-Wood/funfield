## Tier 1 Item 1 — Stress test of moderated EV/mediation machinery
## Step 0: Load data, verify structure, sanity-check baseline model
## Output: data dimensions, cluster size, variance decomposition,
##         baseline NoModMedModel and ModMedModel coefficients

speeding_dir <- "C:/Users/dusti/Dropbox/Work/functional system model/creating fields from ESJT data/experimental vignettes/EXSJT data & code/speeding OSF"
load(file.path(speeding_dir, "SL prepared dataset.Rdata"))

suppressPackageStartupMessages({
  library(lavaan)
  library(funfield)
})

cat("\n==== OBJECTS LOADED ====\n")
print(ls())

cat("\n==== devSL3 dims ====\n")
print(dim(devSL3))
cat("\n==== devSL3 first 12 col names ====\n")
print(colnames(devSL3)[1:12])
cat("\n==== devSL3 head (first 12 cols) ====\n")
print(head(devSL3[, 1:12]))

cat("\n==== Cluster structure ====\n")
np <- length(unique(devSL3$p))
ns <- length(unique(devSL3$s))
ni <- length(unique(devSL3$i))
cat(sprintf("n persons = %d\n", np))
cat(sprintf("n situations = %d\n", ns))
cat(sprintf("n actions (i) = %d\n", ni))
cat(sprintf("n rows = %d\n", nrow(devSL3)))
cat(sprintf("rows per person (mean) = %.2f\n", nrow(devSL3) / np))
cat("\ntable(rows per person):\n")
print(table(table(devSL3$p)))

## Variance decomposition: how much of Speed:Z, Z, Speed is within vs between?
devSL3$Z <- scale(devSL3$SRFastDriver)[, 1]
devSL3$SpeedZ <- devSL3$Speed * devSL3$Z

cat("\n==== Variance decomposition (within vs between person) ====\n")
vdec <- function(x, p) {
  d <- data.frame(x = x, p = p)
  d <- d[!is.na(d$x), ]
  pmean <- ave(d$x, d$p, FUN = mean)
  total <- var(d$x)
  bw    <- var(pmean)
  wn    <- var(d$x - pmean)
  c(total = total, between = bw, within = wn,
    icc_like = bw / total)
}
for (v in c("Speed", "Z", "SpeedZ", "Crash", "L", "SRFastDriver")) {
  cat(sprintf("\n%s:\n", v))
  print(round(vdec(devSL3[[v]], devSL3$p), 4))
}

cat("\n==== Baseline: NoModMedModel for M = Crash ====\n")
NoModMedModel <- '
M ~ 1 + B1_1A*Speed
L ~ 1 + B1_LA*Speed + B1_L1*M
ind := B1_1A*B1_L1
'
test <- devSL3
test$M <- test$Crash
fit_nomod <- sem(NoModMedModel, test, cluster = "p")
print(summary(fit_nomod))

cat("\n==== Moderated: ModMedModel for M = Crash, Z = scale(SRFastDriver) ====\n")
ModMedModel <- '
M ~ 1 + B1_1A*Speed + Z + BZ_1A*Speed:Z
L ~ 1 + B1_LA*Speed + Z + BZ_LA*Speed:Z + B1_L1*M + BZ_L1*M:Z
indByZE := BZ_1A*B1_L1
indByZV := B1_1A*BZ_L1
'
fit_mod <- sem(ModMedModel, test, cluster = "p")
print(summary(fit_mod))

cat("\nDONE\n")
