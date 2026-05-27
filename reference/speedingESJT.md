# Speeding ESJT Dataset

Long-format Elaborated & eXperimental Situational Judgment Test (EXSJT)
data on speeding decisions, bundled as a named list with five linked
elements (four data frames plus a codebook). Used in Wood, Harms, & Cho
(2023) and as a canonical example in funfield documentation.

Each participant (N = 333 after quality screening) rated three actions
(drive at the speed limit, +5 mph, +10+ mph) on eight expected outcomes
and on the likelihood that they would take the action. Six features of
the scenario (vehicle, weather, traffic, time pressure, social context,
and police visibility) were randomly varied across participants, so that
each respondent saw exactly one fully crossed instance of the speeding
scenario. This differs from
[`coworkerESJT`](https://dustin-wood.github.io/funfield/reference/coworkerESJT.md),
in which 12 discrete situations were rated within each participant.

## Usage

``` r
speedingESJT
```

## Format

A named list with five elements:

**`$PSI`** — person × action ratings (999 rows = 333 persons × 3
actions). All Likert-rated columns are rescaled to \[0, 1\]:

- p:

  Person ID (Qualtrics response ID string).

- s:

  Situation ID (constant: `"SL"` — the single speeding scenario).

- i:

  Action text as displayed to the respondent.

- Speed:

  Action coded on \[0, 1\]: `0` = at the speed limit, `0.5` = +5 mph,
  `1` = +10+ mph.

- MoneyCost:

  *The total financial cost of your trip.* \[0, 1\]

- FunDrive:

  *The amount of enjoyment experienced from driving on this trip.* \[0,
  1\]

- IntQuality:

  *The quality of your next meeting with your \[context\].* \[0, 1\]

- Appropriate:

  *Your sense that you acted appropriately.* \[0, 1\]

- OnTime:

  *Likelihood of getting to your meeting on time.* \[0, 1\]

- Crash:

  *Likelihood of getting into a car crash.* \[0, 1\]

- Injured:

  *Likelihood of having a serious injury.* \[0, 1\]

- Ticket:

  *Likelihood of getting a speeding ticket.* \[0, 1\]

- Likelihood:

  *How likely are you to do each action in this situation?* \[0, 1\]

**`$cond`** — situation-level experimental factors (333 rows; one per
person). The six factors were independently and randomly varied across
participants:

- p:

  Person ID.

- s:

  Situation ID (`"SL"`).

- context:

  Who the respondent is meeting with: `"boss"`, `"coworkers"`, or
  `"friends"`.

- vehicle:

  Vehicle driven: `"a midsized SUV"`, `"a small compact car"`, or
  `"a large truck"`.

- weather:

  Weather: `"dry and sunny"`, `"gray and rainy"`, or `"very foggy"`.

- traffic:

  Traffic level: `"almost no other cars"` or `"many other cars"`.

- timePressure:

  Schedule pressure: `"quite late"`, `"a bit late"`, or
  `"comfortably ahead of schedule"`.

- policeFreq:

  Police visibility history on the road: `"have regularly"`,
  `"have occasionally"`, or `"don't recall having ever"` seen police.

- scenario:

  Full assembled scenario text as displayed to the respondent (the six
  factors interpolated into the template; see
  `$codebook$scenario$template`).

**`$sit`** — situation-level perceptions (333 rows). Items rated on the
stem *"How do you perceive this situation?"* on \[0, 1\] (0 = Strongly
Disagree, 1 = Strongly Agree):

- p:

  Person ID.

- s:

  Situation ID.

- Realistic:

  *The scenario is realistic.*

- Relatable:

  *A similar scenario could happen to me.*

**`$traits`** — person-level self-report ratings and demographics (333
rows). The 30 SR items use the stem *"Describe how much you agree with
each of the statements below."* on \[0, 1\] (0 = Strongly disagree, 1 =
Strongly agree). See `$codebook$trait_items` for full wording, and
`$codebook$trait_scales` for the composite recipes that reproduce the
named scales (Big Five, value orientations, self-report traits) used in
Wood, Harms, & Cho (2023):

- p:

  Person ID.

- SR_01, ..., SR_30:

  Raw self-report items, each rescaled to \[0, 1\]. See
  `$codebook$trait_items`.

- employment, education, income, ethnicity, age, gender:

  Demographics as raw Qualtrics codes; see `$codebook$demographics` for
  the mapping.

**`$codebook`** — survey wording, scale anchors, factor levels, and
composite recipes. A named list with eight elements:

- outcomes:

  Data frame (8 rows): variable name, full survey wording, \[0, 1\]
  scale anchor text, and shared stem for the eight expected-outcome
  items.

- Likelihood:

  Data frame (1 row): wording and scale for the action-likelihood
  rating.

- actions:

  Data frame (3 rows): action number, Speed code, and full action text
  as displayed.

- scenario:

  List with two elements: `template` (the scenario text with bracketed
  factor placeholders) and `factors` (a data frame listing each factor
  and its observed levels).

- sit_items:

  Data frame (2 rows): wording, scale, and stem for the
  situation-perception items.

- trait_items:

  Data frame (30 rows): wording, scale, and stem for each of the 30 SR
  items.

- trait_scales:

  Data frame (20 rows): for each named composite scale, the items it
  draws on (with `(1-SR_NN)` marking reverse-keyed items) and the number
  of items.

- demographics:

  Data frame (6 rows): variable name, original survey wording, source
  Qualtrics qname, and a note that values are preserved as raw Qualtrics
  codes.

## Source

Wood, D., Harms, P. D., & Cho, S. (2023). Speeding study OSF materials.
Raw data captured via Qualtrics; reproduced with permission for use as a
canonical example in the funfield package.

## Details

**Data cleaning.** The raw dataset contained 347 participants. Three
sequential data-quality screens were applied:

1.  **Straightlining**
    ([`fancyr::strLine()`](https://rdrr.io/pkg/fancyr/man/strLine.html),
    cut at \> 10% of (person, action) rows showing zero variance across
    the 9 rated columns).

2.  **Low overall variability**
    ([`fancyr::prMaxSD()`](https://rdrr.io/pkg/fancyr/man/prMaxSD.html),
    cut at \< 0.25 of the maximum possible SD across all 27 rated
    cells).

3.  **Response speed**
    ([`fancyr::spi()`](https://rdrr.io/pkg/fancyr/man/spi.html) combined
    across the four speeding rating blocks, cut at \< 1 second per item
    per Wood, Harms, Lowman, & DeSimone, 2017).

Respondents with missing timing data (a known intermittent Qualtrics
recording failure) are *retained* — missingness on a screen is treated
as no evidence to drop. Final analytic sample: **N = 333** participants.

**Rescaling.** All Likert items are stored on a \[0, 1\] proportion-of-
maximum-possible (POMP) scale (`fancyr::pomp(x, smin = 1, smax = 5)`).
Differences between two actions on a \[0, 1\] outcome therefore fall on
\[-1, 1\], which is the natural metric for expectancy-value reasoning.

**Typical workflow.** Within-person deviate the Level-1 columns before
fitting path models:


    data(speedingESJT)
    L1 <- c("Speed","MoneyCost","FunDrive","IntQuality","Appropriate",
            "OnTime","Crash","Injured","Ticket","Likelihood")
    dev <- speedingESJT$PSI
    for (v in L1) dev[[v]] <- ave(dev[[v]], dev$p,
                                  FUN = function(x) x - mean(x))

**Scoring named trait composites.** The composite recipes in
`$codebook$trait_scales` reproduce the named scales used in Wood, Harms,
& Cho (2023):


    tr <- speedingESJT$traits
    tr$Extra <- rowMeans(cbind(1 - tr$SR_15, tr$SR_16, tr$SR_17), na.rm = TRUE)
    tr$Open  <- rowMeans(cbind(tr$SR_27, 1 - tr$SR_28, tr$SR_29), na.rm = TRUE)
    tr$SRFastDriver <- tr$SR_30

## References

Wood, D., Harms, P., Lowman, G. H., & DeSimone, J. A. (2017). Response
speed and response consistency as mutually validating indicators of data
quality in online samples. *Social Psychological and Personality
Science, 8*(4), 454-464.

## Examples

``` r
data(speedingESJT)
## Basic structure
lapply(speedingESJT, function(x) if (is.data.frame(x)) dim(x) else length(x))
#> $PSI
#> [1] 999  13
#> 
#> $cond
#> [1] 333   9
#> 
#> $sit
#> [1] 333   4
#> 
#> $traits
#> [1] 333  37
#> 
#> $codebook
#> [1] 8
#> 
head(speedingESJT$PSI)
#>                   p  s
#> 1 R_0NF2tnitBYSVXfr SL
#> 2 R_0NF2tnitBYSVXfr SL
#> 3 R_0NF2tnitBYSVXfr SL
#> 4 R_10OxZSk6y8d0j6U SL
#> 5 R_10OxZSk6y8d0j6U SL
#> 6 R_10OxZSk6y8d0j6U SL
#>                                                                             i
#> 1                                 drive no more than the speed limit (65 mph)
#> 2  drive as much as 5 miles per hour over the speed limit (70 miles per hour)
#> 3 drive more than 10 miles per hour over the speed limit (75+ miles per hour)
#> 4                                 drive no more than the speed limit (65 mph)
#> 5  drive as much as 5 miles per hour over the speed limit (70 miles per hour)
#> 6 drive more than 10 miles per hour over the speed limit (75+ miles per hour)
#>   Speed MoneyCost FunDrive IntQuality Appropriate OnTime Crash Injured Ticket
#> 1   0.0      0.50     0.50       0.50        0.75   0.25  0.25    0.25   0.25
#> 2   0.5      0.50     0.75       0.75        0.75   0.75  0.50    0.50   0.25
#> 3   1.0      0.50     0.25       0.75        0.25   0.75  0.50    0.50   0.75
#> 4   0.0      0.25     0.00       0.50        0.75   0.00  0.00    0.00   0.00
#> 5   0.5      0.25     0.50       0.50        0.25   0.50  0.50    0.50   0.50
#> 6   1.0      0.25     0.75       0.75        0.00   0.75  0.75    1.00   0.75
#>   Likelihood
#> 1       0.75
#> 2       0.75
#> 3       0.25
#> 4       0.75
#> 5       0.75
#> 6       0.25
table(speedingESJT$cond$context)
#> 
#>      boss coworkers   friends 
#>       115       109       109 

## Outcome variable wording
speedingESJT$codebook$outcomes[, c("var","label")]
#>                       var
#> iSpeed1.Out_1   MoneyCost
#> iSpeed1.Out_2    FunDrive
#> iSpeed1.Out_3  IntQuality
#> iSpeed1.Out_4 Appropriate
#> iSpeed1.Out_5      OnTime
#> iSpeed1.Out_6       Crash
#> iSpeed1.Out_7     Injured
#> iSpeed1.Out_8      Ticket
#>                                                                       label
#> iSpeed1.Out_1                         The total financial cost of your trip
#> iSpeed1.Out_2 The amount of enjoyment experienced from driving on this trip
#> iSpeed1.Out_3 The quality of your next meeting with your [Field-SL.context]
#> iSpeed1.Out_4                       Your sense that you acted appropriately
#> iSpeed1.Out_5                 Likelihood of getting to your meeting on time
#> iSpeed1.Out_6                        Likelihood of getting into a car crash
#> iSpeed1.Out_7                         Likelihood of having a serious injury
#> iSpeed1.Out_8                       Likelihood of getting a speeding ticket

## The scenario template and observed factor levels
speedingESJT$codebook$scenario
#> $template
#> [1] "You are driving on the freeway in ${e://Field/SL.vehicle}. It is a ${e://Field/SL.weather} day with ${e://Field/SL.traffic} on the road. You are running ${e://Field/SL.timePressure} for a meeting with your ${e://Field/SL.context}. The posted speed limit of 65 miles-per-hour, and you ${e://Field/SL.policeFreq} seen police on this road."
#> 
#> $factors
#>                          var
#> SL.context           context
#> SL.vehicle           vehicle
#> SL.weather           weather
#> SL.traffic           traffic
#> SL.timePressure timePressure
#> SL.policeFreq     policeFreq
#>                                                                      levels
#> SL.context                                         coworkers; boss; friends
#> SL.vehicle               a midsized SUV; a small compact car; a large truck
#> SL.weather                        dry and sunny; very foggy; gray and rainy
#> SL.traffic                            almost no other cars; many other cars
#> SL.timePressure       comfortably ahead of schedule; a bit late; quite late
#> SL.policeFreq   have occasionally; have regularly; don't recall having ever
#> 
```
