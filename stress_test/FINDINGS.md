# Tier 1, Item 1 — Stress test of the moderated EV/mediation machinery

**Date:** 2026-05-11
**Case study:** Speeding OSF (`devSL3`, n=906 = 302 persons × 3 actions)
**Model under test:**
```
M ~ 1 + B1_1A*Speed + Z + BZ_1A*Speed:Z
L ~ 1 + B1_LA*Speed + Z + BZ_LA*Speed:Z + B1_L1*M + BZ_L1*M:Z
indByZE := BZ_1A*B1_L1
indByZV := B1_1A*BZ_L1
```
fit via `lavaan::sem(..., cluster = "p")`.

Scripts and logs in `stress_test/00_*`, `01_*`, `02_*`.

## What's solid

1. **`cluster = "p"` SEs are correct.** Three independent estimators
   (lavaan robust.cluster, manual cluster bootstrap with 500 reps, `lm` +
   `sandwich::vcovCL`) agree on every regression SE to within ~0.001:

   | param   | lavaan | boot   | crlm   |
   |---------|--------|--------|--------|
   | B1_1A   | .0151  | .0158  | .0151  |
   | BZ_1A   | .0151  | .0146  | .0152  |
   | B1_LA   | .0352  | .0356  | .0353  |
   | BZ_LA   | .0336  | .0340  | .0337  |
   | B1_L1   | .0724  | .0719  | .0726  |
   | BZ_L1   | .0699  | .0713  | .0701  |

2. **The delta-method SEs on `indByZE`/`indByZV` are trustworthy.**
   - `indByZE` lavaan delta: 0.0357 (.0095); boot percentile CI [.018, .055] — same conclusion (p<.001).
   - `indByZV` lavaan delta: -0.0232 (.0221); boot percentile CI [-.064, .024] — same conclusion (p≈.29).
   The delta CI and the bootstrap percentile CI agree both in width and in the significance decision.

3. **Cross-level interactions are correctly handled.** Variance decomposition
   confirms `Speed`, `M`, `L` are 100% within-person; `Z` is 100% between-person;
   `Speed:Z` and `M:Z` inherit only within-person variance. lavaan correctly
   reports the Z main effect as exactly 0 (the deviated outcome has no
   between-person variance for Z to predict).

4. **Clustering matters substantively.** Compared to naive (no-cluster) SEs,
   cluster-robust SEs are inflated by **30-50% on every moderation parameter**:

   | param   | SE_naive | SE_cluster | ratio |
   |---------|----------|------------|-------|
   | B1_1A   | .0107    | .0151      | 1.41  |
   | BZ_1A   | .0107    | .0151      | 1.41  |
   | BZ_LA   | .0219    | .0336      | 1.54  |
   | BZ_L1   | .0499    | .0699      | 1.40  |
   | indByZE | .0073    | .0096      | 1.31  |
   | indByZV | .0157    | .0219      | 1.40  |

   The Yuan-Bentler scaling factor of 5.045 reflects the same design effect on
   the test statistic side. Translation: naive analysis would underestimate
   SEs by ~30-50% and could turn t≈1.4 into apparent t≈2.0.

5. **Pattern generalizes across moderators.** The same model fits cleanly
   across all 23 trait moderators in `devSL3` (cols 14-36); no convergence
   failures; SE magnitudes for a given parameter are stable across moderators.
   `SRFastDriver` is the cleanest case for expectation-path moderation
   (z=3.7 on `indByZE`); `DrivingSkill` for valuation-path moderation
   (z=-3.2 on `indByZV`).

## Caveats (cosmetic)

1. **Non-PD vcov warning is a numerical artifact.** Smallest eigenvalue is
   ~-1e-19, which is at machine precision (effective zero). Persists even
   when intercepts are removed (`~0` syntax). Does not affect SE estimation
   — confirmed by independent comparators above. Safe to ignore. Could be
   suppressed in user-facing wrappers via `warn = FALSE`.

2. **z-tests rather than t-tests for cluster-robust inference.** lavaan
   reports z-statistics (df → ∞). With G=302 clusters this is essentially
   identical to t(301). At low G (rule of thumb: G < 50) the z approximation
   would understate p-values and a t with G-1 df should be used. **Not a
   problem here.**

3. **Yuan-Bentler chi-square is not interpretable as a model-fit index** in
   this saturated regression (df=4 only from intercept constraints). It is
   informational about the design effect, not about model fit.

## Gaps (substantive)

1. **`cluster = "p"` is single-level SEM with cluster-corrected SEs — not a
   two-level model.** This is correct *because* the data is already
   within-person deviated. The model has no random effects, no between-person
   component. For non-deviated data, a proper two-level SEM (`level: 1` /
   `level: 2`) would be needed. **The script's reliance on `devSL3` is a
   silent prerequisite.** When this pattern is packaged, the wrapper should
   either (a) deviate internally, or (b) refuse non-deviated data, or (c)
   document the assumption loudly.

2. **Mediator-by-mediator estimation isolates each mediator.** Each fit of
   `ModMedModel` puts a single feature in the `M` slot, ignoring the other
   mediators. This is the standard EV decomposition (each mediator's
   contribution treated independently), but the joint model with all
   mediators would account for mediator correlations and give different
   (typically wider) CIs on the indirect effects. Worth flagging in any
   `evChain()` wrapper.

3. **The `Z` coefficient column in the printout is always 0 with no SE.**
   This is technically correct (no variance to predict) but visually
   confusing. The packaged function should suppress this row.

## Decision

The statistical machinery in the speeding script is **trustworthy** for
within-person deviated ESJT data with G ≳ 50 clusters. The cluster-robust
SEs are correct, the delta-method CIs on indirect effects are correct, and
the cross-level interaction structure is handled appropriately.

**Cleared to proceed to agenda item 2** (`evChain()` wrapper). The wrapper
should:
- Default to expecting deviated data and either enforce or document this.
- Suppress the `Z`-on-outcome row, the variance-of-M:Z row, and the
  non-PD warning in user-facing output.
- Return a tidy table per mediator with B1_1A, BZ_1A, B1_L1, BZ_L1,
  indByZE, indByZV (est, SE, z, p).
- Optionally allow `se = "boot"` mode for an alternative CI (especially
  useful at low G).

**Open question worth a separate small investigation later:** at what G
does the cluster-robust z-test start to overstate precision? The standard
recommendation is G ≥ 50; with G=302 we are comfortably above. But ESJT
studies with small samples (G < 50) would warrant either bootstrap or a
small-sample-corrected variant (e.g., `clubSandwich::vcovCR(type = "CR2")`).
