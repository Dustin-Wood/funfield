## Tier 1 Item 1 — Step 1: Compare cluster-robust SEs from lavaan
## against three alternative estimators on the SAME moderated model.
##
## Goal: decide whether lavaan's `cluster = "p"` SEs for B1_1A, BZ_1A,
## BZ_L1 and the derived `indByZE`/`indByZV` are trustworthy.
##
## Comparators:
##   A) lavaan default (robust.cluster) — baseline from step 0
##   B) lavaan cluster bootstrap (se = "boot", bootstrap on clusters)
##   C) lmer mixed model: random intercept on p
##   D) lm with sandwich::vcovCL cluster-robust SEs (no random effects)
##
## We fit the two regressions of the ModMedModel separately so all four
## estimators are operating on the same equations:
##     M ~ Speed + Z + Speed:Z
##     L ~ Speed + Z + Speed:Z + M + M:Z
## then compute indByZE, indByZV by hand and get SEs via delta method
## (for B, C, D — lavaan already does it for A).

speeding_dir <- "C:/Users/dusti/Dropbox/Work/functional system model/creating fields from ESJT data/experimental vignettes/EXSJT data & code/speeding OSF"
load(file.path(speeding_dir, "SL prepared dataset.Rdata"))

suppressPackageStartupMessages({
  library(lavaan)
  library(sandwich)
  library(lmtest)
})
## Skip lmer in R 4.2.2 (Matrix/cholmod incompatibility) — cluster bootstrap
## provides an equally good independent SE comparator.
USE_LMER <- FALSE
cat(sprintf("\n[info] lmer comparator available: %s\n", USE_LMER))

set.seed(20260511)

devSL3$Z <- scale(devSL3$SRFastDriver)[, 1]
dat <- devSL3
dat$M <- dat$Crash

ModMedModel <- '
M ~ 1 + B1_1A*Speed + Z + BZ_1A*Speed:Z
L ~ 1 + B1_LA*Speed + Z + BZ_LA*Speed:Z + B1_L1*M + BZ_L1*M:Z
indByZE := BZ_1A*B1_L1
indByZV := B1_1A*BZ_L1
'

## ---------- A) lavaan robust.cluster (current baseline) ----------
cat("\n==== A) lavaan robust.cluster ====\n")
fitA <- sem(ModMedModel, dat, cluster = "p")
peA <- parameterestimates(fitA)
peA_keep <- subset(peA, !is.na(z))
print(peA_keep[, c("lhs","op","rhs","label","est","se","z","pvalue")], row.names = FALSE)

## ---------- B) lavaan cluster bootstrap ----------
cat("\n==== B) lavaan cluster bootstrap (R=500) ====\n")
## Manual cluster bootstrap because lavaan's se='boot' resamples rows.
boot_cluster <- function(model, data, cluster, R = 500) {
  cl_ids <- unique(data[[cluster]])
  npars  <- NULL
  par_mat <- NULL
  for (b in seq_len(R)) {
    samp <- sample(cl_ids, length(cl_ids), replace = TRUE)
    d <- do.call(rbind, lapply(seq_along(samp), function(j) {
      sub <- data[data[[cluster]] == samp[j], , drop = FALSE]
      sub[[cluster]] <- paste0("b", j)  # unique cluster id for new sample
      sub
    }))
    fitb <- tryCatch(sem(model, d, cluster = cluster, se = "none",
                         warn = FALSE), error = function(e) NULL)
    if (is.null(fitb)) next
    pb <- parameterestimates(fitb)
    if (is.null(par_mat)) {
      lab <- ifelse(pb$label == "", paste(pb$lhs, pb$op, pb$rhs), pb$label)
      par_mat <- matrix(NA_real_, R, length(lab), dimnames = list(NULL, lab))
    }
    par_mat[b, ] <- pb$est
  }
  list(par_mat = par_mat, ok = sum(!is.na(par_mat[,1])))
}
bA <- boot_cluster(ModMedModel, dat, "p", R = 500)
cat(sprintf("Successful boot reps: %d / 500\n", bA$ok))
boot_se <- apply(bA$par_mat, 2, sd, na.rm = TRUE)
boot_q  <- apply(bA$par_mat, 2, quantile, c(.025, .975), na.rm = TRUE)
labs_of_interest <- c("B1_1A","BZ_1A","B1_LA","BZ_LA","B1_L1","BZ_L1",
                      "indByZE","indByZV")
peA_lookup <- setNames(peA$est, peA$label)
cat("\n  param      est       SE_boot   95% CI (percentile)\n")
for (l in labs_of_interest) {
  if (l %in% names(boot_se)) {
    cat(sprintf("  %-9s %8.4f  %8.4f  [%7.4f, %7.4f]\n",
                l, peA_lookup[[l]], boot_se[[l]],
                boot_q[1, l], boot_q[2, l]))
  }
}

## Pre-compute the interaction term to be explicit
dat$SpeedZ <- dat$Speed * dat$Z
dat$MZ     <- dat$M * dat$Z

## ---------- C) lmer random-intercept on p ----------
if (USE_LMER) {
  cat("\n==== C) lmer random intercept on p ====\n")
  fitC_M <- lmer(M ~ Speed + Z + SpeedZ + (1 | p), data = dat, REML = FALSE)
  fitC_L <- lmer(L ~ Speed + Z + SpeedZ + M + MZ + (1 | p), data = dat, REML = FALSE)
  cat("\n-- C: M model --\n"); print(summary(fitC_M)$coefficients)
  cat("\n-- C: L model --\n"); print(summary(fitC_L)$coefficients)
} else {
  cat("\n==== C) lmer SKIPPED (package unavailable) ====\n")
}

## ---------- D) lm + cluster-robust SE (sandwich::vcovCL) ----------
cat("\n==== D) lm + sandwich::vcovCL cluster-robust ====\n")
fitD_M <- lm(M ~ Speed + Z + SpeedZ, data = dat)
fitD_L <- lm(L ~ Speed + Z + SpeedZ + M + MZ, data = dat)
vC_M <- vcovCL(fitD_M, cluster = ~ p, type = "HC1")
vC_L <- vcovCL(fitD_L, cluster = ~ p, type = "HC1")
cat("\n-- D: M model --\n"); print(coeftest(fitD_M, vcov. = vC_M))
cat("\n-- D: L model --\n"); print(coeftest(fitD_L, vcov. = vC_L))

## ---------- Build comparison table ----------
cat("\n==== COMPARISON TABLE: SEs by estimator ====\n")
get_lmer <- function(fit, param) {
  co <- summary(fit)$coefficients
  if (!(param %in% rownames(co))) return(c(NA, NA))
  co[param, c("Estimate","Std. Error")]
}
get_crlm <- function(fit, vc, param) {
  ct <- coeftest(fit, vcov. = vc)
  if (!(param %in% rownames(ct))) return(c(NA, NA))
  ct[param, c("Estimate","Std. Error")]
}
get_lavaan <- function(pe, label) {
  r <- subset(pe, label == label)
  r <- r[r$label == label, ]
  if (nrow(r) == 0) return(c(NA, NA))
  c(r$est[1], r$se[1])
}

rows <- list(
  c("B1_1A   M~Speed",       "Speed",  "M"),
  c("BZ_1A   M~Speed:Z",     "SpeedZ", "M"),
  c("B1_LA   L~Speed",       "Speed",  "L"),
  c("BZ_LA   L~Speed:Z",     "SpeedZ", "L"),
  c("B1_L1   L~M",           "M",      "L"),
  c("BZ_L1   L~M:Z",         "MZ",     "L")
)
cat(sprintf("%-22s %12s %12s %12s %12s\n",
            "param", "lavaan_SE", "boot_SE", "lmer_SE", "crlm_SE"))
for (r in rows) {
  lab <- strsplit(r[1], "  ")[[1]][1]
  lab <- sub(" .*","",lab)
  est_lav <- peA_lookup[[lab]]
  se_lav  <- subset(peA, label == lab)$se
  if (r[3] == "M") {
    se_lmer <- if (USE_LMER) get_lmer(fitC_M, r[2])[2] else NA
    se_crlm <- get_crlm(fitD_M, vC_M, r[2])[2]
  } else {
    se_lmer <- if (USE_LMER) get_lmer(fitC_L, r[2])[2] else NA
    se_crlm <- get_crlm(fitD_L, vC_L, r[2])[2]
  }
  se_boot <- if (lab %in% names(boot_se)) boot_se[[lab]] else NA
  cat(sprintf("%-22s %12.4f %12.4f %12.4f %12.4f\n",
              r[1], se_lav, se_boot, se_lmer, se_crlm))
}

## Indirect effects
cat("\n==== Indirect effects: indByZE = BZ_1A * B1_L1, indByZV = B1_1A * BZ_L1 ====\n")
## lavaan
ind_lav <- subset(peA, label %in% c("indByZE","indByZV"))
cat("\nA) lavaan (delta method):\n")
print(ind_lav[, c("label","est","se","z","pvalue")], row.names = FALSE)

## bootstrap
if (!is.null(boot_se)) {
  cat("\nB) bootstrap (percentile):\n")
  for (l in c("indByZE","indByZV")) {
    cat(sprintf("  %s: est=%.4f  SE=%.4f  95%% CI=[%.4f, %.4f]\n",
                l, peA_lookup[[l]], boot_se[[l]],
                boot_q[1, l], boot_q[2, l]))
  }
}

if (USE_LMER) {
  beta_BZ_1A <- fixef(fitC_M)["SpeedZ"]
  beta_B1_L1 <- fixef(fitC_L)["M"]
  beta_B1_1A <- fixef(fitC_M)["Speed"]
  beta_BZ_L1 <- fixef(fitC_L)["MZ"]
  V_M <- vcov(fitC_M)
  V_L <- vcov(fitC_L)
  v_BZ_1A <- V_M["SpeedZ","SpeedZ"]
  v_B1_1A <- V_M["Speed","Speed"]
  v_B1_L1 <- V_L["M","M"]
  v_BZ_L1 <- V_L["MZ","MZ"]
  ind_E_lmer <- beta_BZ_1A * beta_B1_L1
  se_E_lmer  <- sqrt(beta_B1_L1^2 * v_BZ_1A + beta_BZ_1A^2 * v_B1_L1)
  ind_V_lmer <- beta_B1_1A * beta_BZ_L1
  se_V_lmer  <- sqrt(beta_BZ_L1^2 * v_B1_1A + beta_B1_1A^2 * v_BZ_L1)
  cat("\nC) lmer (delta, assumes cov(M-eq, L-eq) = 0):\n")
  cat(sprintf("  indByZE: est=%.4f  SE=%.4f  z=%.2f\n",
              ind_E_lmer, se_E_lmer, ind_E_lmer / se_E_lmer))
  cat(sprintf("  indByZV: est=%.4f  SE=%.4f  z=%.2f\n",
              ind_V_lmer, se_V_lmer, ind_V_lmer / se_V_lmer))
}

## crlm delta method
ct_M <- coeftest(fitD_M, vcov. = vC_M)
ct_L <- coeftest(fitD_L, vcov. = vC_L)
b_BZ_1A_d <- ct_M["SpeedZ","Estimate"]; s_BZ_1A_d <- ct_M["SpeedZ","Std. Error"]
b_B1_L1_d <- ct_L["M","Estimate"];      s_B1_L1_d <- ct_L["M","Std. Error"]
b_B1_1A_d <- ct_M["Speed","Estimate"];  s_B1_1A_d <- ct_M["Speed","Std. Error"]
b_BZ_L1_d <- ct_L["MZ","Estimate"];     s_BZ_L1_d <- ct_L["MZ","Std. Error"]
ind_E_crlm <- b_BZ_1A_d * b_B1_L1_d
se_E_crlm  <- sqrt(b_B1_L1_d^2 * s_BZ_1A_d^2 + b_BZ_1A_d^2 * s_B1_L1_d^2)
ind_V_crlm <- b_B1_1A_d * b_BZ_L1_d
se_V_crlm  <- sqrt(b_BZ_L1_d^2 * s_B1_1A_d^2 + b_B1_1A_d^2 * s_BZ_L1_d^2)
cat("\nD) crlm (delta):\n")
cat(sprintf("  indByZE: est=%.4f  SE=%.4f  z=%.2f\n",
            ind_E_crlm, se_E_crlm, ind_E_crlm / se_E_crlm))
cat(sprintf("  indByZV: est=%.4f  SE=%.4f  z=%.2f\n",
            ind_V_crlm, se_V_crlm, ind_V_crlm / se_V_crlm))

cat("\nDONE\n")
