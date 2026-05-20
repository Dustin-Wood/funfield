## One-time migration: re-encode coworkerESJT ratings from [-1, 1] to [0, 1].
##
## Transform: x' = (x + 1) / 2   (so -1 -> 0, -.5 -> .25, 0 -> .5, .5 -> .75, 1 -> 1)
##
## This brings coworkerESJT onto the same [0, 1] Likert convention as the three
## EXSJT datasets (speedingESJT, overtimeESJT, dataHomeESJT). All Likert ratings
## are rescaled: $PSI (8 outcomes + Likelihood), $sit (4 perceptions), $IIDL
## (40 traits). The p/s/i identifiers and $cond$condition are left untouched.
##
## Guarded against double-application: aborts if the data already looks [0, 1].
##
## Source/output: data/coworkerESJT.rda  (no separate raw build exists for this
## dataset). See also: R/coworkerESJT-data.R.

load("data/coworkerESJT.rda")
ce <- coworkerESJT

rescale01 <- function(x) (x + 1) / 2

psi_rating  <- setdiff(names(ce$PSI),  c("p", "s", "i"))
sit_rating  <- setdiff(names(ce$sit),  c("p", "s"))
iidl_rating <- setdiff(names(ce$IIDL), "p")

## guard: refuse to run if ratings are not on [-1, 1]
all_min <- min(c(unlist(ce$PSI[psi_rating]),
                 unlist(ce$sit[sit_rating]),
                 unlist(ce$IIDL[iidl_rating])), na.rm = TRUE)
if (all_min >= -1e-9) {
  stop("coworkerESJT ratings already appear to be on [0, 1]; ",
       "aborting to avoid double-rescaling.")
}

for (v in psi_rating)  ce$PSI[[v]]  <- rescale01(ce$PSI[[v]])
for (v in sit_rating)  ce$sit[[v]]  <- rescale01(ce$sit[[v]])
for (v in iidl_rating) ce$IIDL[[v]] <- rescale01(ce$IIDL[[v]])

## codebook scale-anchor text
out_scale <- "0=Greatly Decrease | .25=Slightly Decrease | .5=Negligible Effect | .75=Slightly Increase | 1=Greatly Increase"
lik_scale <- "0=Very Unlikely | .25=Unlikely | .5=Equally Likely/Unlikely | .75=Likely | 1=Very Likely"
ce$codebook$outcomes$scale <- ifelse(
  ce$codebook$outcomes$var == "Likelihood", lik_scale, out_scale)

ce$codebook$sit_items$scale <-
  "0=Greatly Decreased | .25=Slightly Decreased | .5=Negligible Effect | .75=Slightly Increased | 1=Greatly Increased"

ce$codebook$IIDL_items$scale <-
  "0=Very Uncharacteristic | .25=Somewhat Uncharacteristic | .5=Neither Characteristic or Uncharacteristic | .75=Somewhat Characteristic | 1=Very Characteristic"

coworkerESJT <- ce
save(coworkerESJT, file = "data/coworkerESJT.rda", compress = "xz")
cat("coworkerESJT re-encoded to [0, 1] and saved.\n")
