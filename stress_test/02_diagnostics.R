## Tier 1 Item 1 — Step 2: Diagnostics
##  (1) Is the non-PD vcov warning intercept-related (= harmless)?
##  (2) What does the Yuan-Bentler scaling factor ~5 mean here?
##  (3) Test moderator generalizability across the Z trait set.
##  (4) Sanity check: does dropping cluster = "p" inflate significance?

speeding_dir <- "C:/Users/dusti/Dropbox/Work/functional system model/creating fields from ESJT data/experimental vignettes/EXSJT data & code/speeding OSF"
load(file.path(speeding_dir, "SL prepared dataset.Rdata"))

suppressPackageStartupMessages({
  library(lavaan)
})

devSL3$Z <- scale(devSL3$SRFastDriver)[, 1]
dat <- devSL3
dat$M <- dat$Crash

## ---------- (1) Singularity ----------
cat("\n==== (1) Intercept singularity check ====\n")

ModMedModel_int <- '
M ~ 1 + B1_1A*Speed + Z + BZ_1A*Speed:Z
L ~ 1 + B1_LA*Speed + Z + BZ_LA*Speed:Z + B1_L1*M + BZ_L1*M:Z
indByZE := BZ_1A*B1_L1
indByZV := B1_1A*BZ_L1
'
ModMedModel_noint <- '
M ~ 0 + B1_1A*Speed + Z + BZ_1A*Speed:Z
L ~ 0 + B1_LA*Speed + Z + BZ_LA*Speed:Z + B1_L1*M + BZ_L1*M:Z
indByZE := BZ_1A*B1_L1
indByZV := B1_1A*BZ_L1
'

cat("\n-- With intercepts --\n")
fit_int <- sem(ModMedModel_int, dat, cluster = "p")
ev_int <- eigen(vcov(fit_int))$values
cat(sprintf("min eigenvalue of vcov: %g\n", min(ev_int)))
cat(sprintf("Test stat (Yuan-Bentler scaled): %.3f, scaling factor: %.3f\n",
            fitMeasures(fit_int)["chisq.scaled"],
            fitMeasures(fit_int)["chisq.scaling.factor"]))

cat("\n-- Without intercepts (~0 syntax) --\n")
fit_noint <- sem(ModMedModel_noint, dat, cluster = "p")
ev_noint <- eigen(vcov(fit_noint))$values
cat(sprintf("min eigenvalue of vcov: %g\n", min(ev_noint)))
peN <- parameterestimates(fit_noint)
print(subset(peN, label != "")[, c("lhs","op","rhs","label","est","se","z","pvalue")],
      row.names = FALSE)

## ---------- (2) What does the Yuan-Bentler scaling factor mean? ----------
cat("\n==== (2) Yuan-Bentler scaling factor ====\n")
fm <- fitMeasures(fit_int)
relevant <- c("chisq","df","pvalue","chisq.scaled","df.scaled","pvalue.scaled",
              "chisq.scaling.factor")
print(round(fm[relevant], 4))
cat("\nInterpretation: factor >1 means the un-scaled chi-square is inflated\n")
cat("relative to the cluster-corrected version. Here factor ~5 means the\n")
cat("naive (no-cluster) chi-square is ~5x what the cluster-corrected one is.\n")
cat("The reported SEs already incorporate this correction.\n")
cat("(In a saturated model df=0 so the chi-square itself is uninformative.)\n")

## ---------- (3) Moderator generalizability ----------
cat("\n==== (3) Generalizability across moderators ====\n")
## Trait moderators are in columns 14:36 per the original script
mods <- colnames(devSL3)[14:36]
cat("Available moderators:\n"); print(mods)

ModMedModel <- '
M ~ 1 + B1_1A*Speed + Z + BZ_1A*Speed:Z
L ~ 1 + B1_LA*Speed + Z + BZ_LA*Speed:Z + B1_L1*M + BZ_L1*M:Z
indByZE := BZ_1A*B1_L1
indByZV := B1_1A*BZ_L1
'

results <- list()
for (mvar in mods) {
  d <- devSL3
  d$Z <- scale(d[[mvar]])[, 1]
  d$M <- d$Crash
  fit <- tryCatch(sem(ModMedModel, d, cluster = "p",
                      warn = FALSE),
                  error = function(e) NULL)
  if (is.null(fit)) next
  pe <- parameterestimates(fit)
  pick <- subset(pe, label %in% c("BZ_1A","BZ_LA","BZ_L1","indByZE","indByZV"))
  pick$mod <- mvar
  results[[mvar]] <- pick[, c("mod","label","est","se","z","pvalue")]
}
all_res <- do.call(rbind, results)

cat("\n-- BZ_1A (moderation of Speed -> Crash expectation) --\n")
print(subset(all_res, label == "BZ_1A")[, c("mod","est","se","z","pvalue")],
      row.names = FALSE)
cat("\n-- BZ_L1 (moderation of Crash -> L valuation) --\n")
print(subset(all_res, label == "BZ_L1")[, c("mod","est","se","z","pvalue")],
      row.names = FALSE)
cat("\n-- indByZE (Z-moderation through expectation) --\n")
print(subset(all_res, label == "indByZE")[, c("mod","est","se","z","pvalue")],
      row.names = FALSE)
cat("\n-- indByZV (Z-moderation through valuation) --\n")
print(subset(all_res, label == "indByZV")[, c("mod","est","se","z","pvalue")],
      row.names = FALSE)

## ---------- (4) Effect of dropping cluster = "p" ----------
cat("\n==== (4) Naive (no cluster) vs cluster-robust SEs ====\n")
fit_naive  <- sem(ModMedModel, dat)            # no cluster
fit_cluster <- sem(ModMedModel, dat, cluster = "p")

pe_n <- parameterestimates(fit_naive)
pe_c <- parameterestimates(fit_cluster)
key <- c("B1_1A","BZ_1A","B1_LA","BZ_LA","B1_L1","BZ_L1","indByZE","indByZV")
cmp <- data.frame(
  param = key,
  est   = sapply(key, function(k) pe_n$est[pe_n$label == k][1]),
  SE_naive  = sapply(key, function(k) pe_n$se[pe_n$label == k][1]),
  SE_cluster = sapply(key, function(k) pe_c$se[pe_c$label == k][1])
)
cmp$ratio <- cmp$SE_cluster / cmp$SE_naive
print(cmp, row.names = FALSE)
cat("\nratio > 1 means clustering INFLATES the SE (we were over-confident)\n")
cat("ratio < 1 means clustering deflates it (rare; typically when within-cluster\n")
cat("residuals are negatively correlated)\n")

cat("\nDONE\n")
