## Extract the raw SL3 ESJT dataset from the speeding OSF prepared file
## and save as data/speedingESJT.rda

speeding_dir <- "C:/Users/dusti/Dropbox/Work/functional system model/creating fields from ESJT data/experimental vignettes/EXSJT data & code/speeding OSF"
load(file.path(speeding_dir, "SL prepared dataset.Rdata"))

stopifnot(exists("SL3"))
speedingESJT <- SL3

## Drop attributes from any scaled/labelled columns to avoid surprises
for (j in seq_along(speedingESJT)) {
  attr(speedingESJT[[j]], "scaled:center") <- NULL
  attr(speedingESJT[[j]], "scaled:scale")  <- NULL
}

## Verify structure matches expectations
stopifnot(identical(colnames(speedingESJT)[1:3], c("p","s","i")))
stopifnot(nrow(speedingESJT) == 906)
stopifnot(length(unique(speedingESJT$p)) == 302)

cat(sprintf("speedingESJT: %d rows, %d columns\n",
            nrow(speedingESJT), ncol(speedingESJT)))
cat("Columns:\n"); print(colnames(speedingESJT))

if (!dir.exists("data")) dir.create("data")
save(speedingESJT, file = "data/speedingESJT.rda", compress = "xz")
cat("\nSaved data/speedingESJT.rda\n")
