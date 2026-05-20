## Runs the exact code blocks from vignettes/speeding_pathXMY.Rmd
## to confirm the vignette will knit cleanly.

suppressPackageStartupMessages(library(lavaan))
source("R/pathXMY.R")
load("data/speedingESJT.rda")

L1 <- c("Speed","Crash","Injured","Ticket","MoneyCost","OnTime",
        "IntQuality","FunDrive","Appropriate","L")
dev <- speedingESJT
for (v in L1) {
  dev[[v]] <- ave(dev[[v]], dev$p, FUN = function(x) x - mean(x))
}

cat("\n-- Normative model --\n")
mediators <- c("Crash","Injured","Ticket","MoneyCost","OnTime",
               "IntQuality","FunDrive","Appropriate")
norm <- pathXMY(dev, X = "Speed", Y = "L", M = mediators)
print(head(norm$tidy_loop, 12))

cat("\n-- Moderated model: BZ_MX --\n")
mod <- pathXMY(dev, X = "Speed", Y = "L", M = mediators,
               Z = "SRFastDriver")
print(subset(mod$tidy_loop, param == "BZ_MX")[, c("mediator","est","se","z","pvalue")])

cat("\n-- Moderated model: indZ_X --\n")
print(subset(mod$tidy_loop, param == "indZ_X")[, c("mediator","est","se","z","pvalue")])

cat("\nDONE\n")
