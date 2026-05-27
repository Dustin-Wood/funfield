# Data-Home ESJT Dataset

Long-format Elaborated & eXperimental Situational Judgment Test (EXSJT)
data on the decision to skip or follow a security procedure when taking
company data home. Bundled as a named list mirroring
[`speedingESJT`](https://dustin-wood.github.io/funfield/reference/speedingESJT.md)
and
[`overtimeESJT`](https://dustin-wood.github.io/funfield/reference/overtimeESJT.md).

The same Prolific respondents who completed the speeding and overtime
scenarios also rated this data-home scenario; the three datasets share a
participant pool and can be linked by `p`.

Each participant (N = 332 after quality screening) rated two actions
(skip the password procedure; complete it) on seven expected outcomes
and on the likelihood of taking each action. Three features of the
situation were randomly varied at the person level.

## Usage

``` r
dataHomeESJT
```

## Format

A named list with five elements:

**`$PSI`** — person × action ratings (664 rows = 332 persons × 2
actions). All Likert-rated columns are rescaled to \[0, 1\]:

- p:

  Person ID.

- s:

  Situation ID (constant: `"DH"`).

- i:

  Action text as displayed.

- SkipProcedure:

  Action coded on \[0, 1\]: `1` = skip the password procedure
  (insecure), `0` = complete it.

- Drained:

  *Your feeling of being drained/tired from work.*

- CustomerWellbeing:

  *The well-being of your company's customers.*

- CompanyWellbeing:

  *Your company's well-being.*

- Punished:

  *Likelihood of company firing/punishing you in the near future.*

- Appropriate:

  *Your sense that you acted appropriately.*

- PolicyViolation:

  *Likelihood of company seeing you as violating company
  rules/policies.*

- DataExposure:

  *Likelihood of non-authorized people getting access to the data.*

- Likelihood:

  *How likely you would be to do each action.*

**`$cond`** — situation-level experimental factors (332 rows):

- p, s:

  Person and situation IDs.

- selfResponseEfficacy:

  Description of the password procedure (e.g.,
  `"fairly straightforward and easy, and very effective at making the data more secure"`).

- threatVulSev:

  Vulnerability and severity of harm if data is exposed.

- sanctionCertainSev:

  Certainty and severity of punishment if caught.

- scenario:

  Full assembled scenario text as displayed.

**`$sit`** — situation-level perceptions (332 rows): same `Realistic`
and `Relatable` items as the other two EXSJT datasets.

**`$traits`** — person-level self-report items + demographics (332
rows). Same 30 `SR_*` items and composite recipes as `speedingESJT` and
`overtimeESJT`.

**`$codebook`** — survey wording, scale anchors, factor levels, and
trait composite recipes.

## Source

Wood, D., Harms, P. D., & Cho, S. (2023). EXSJT survey (Prolific panel);
same respondents as `speedingESJT` and `overtimeESJT`.

## Details

**Data cleaning.** Three sequential screens (same as the other two EXSJT
datasets). Final N = 332.

**Rescaling.** All Likert items rescaled from 1-5 to \[0, 1\] via
[`fancyr::pomp()`](https://rdrr.io/pkg/fancyr/man/pomp.html).

## Examples

``` r
data(dataHomeESJT)
lapply(dataHomeESJT, function(x) if (is.data.frame(x)) dim(x) else length(x))
#> $PSI
#> [1] 664  12
#> 
#> $cond
#> [1] 332   6
#> 
#> $sit
#> [1] 332   4
#> 
#> $traits
#> [1] 332  37
#> 
#> $codebook
#> [1] 8
#> 
table(dataHomeESJT$cond$sanctionCertainSev)
#> 
#>  high, and if caught, the punishment would be severe 
#>                                                   68 
#> high, but if caught, the punishment would be minimal 
#>                                                  106 
#>  low, and if caught, the punishment would be minimal 
#>                                                   92 
#>   low, but if caught, the punishment would be severe 
#>                                                   66 
dataHomeESJT$codebook$scenario$template
#> [1] "You are working with data about your company's customers, and want to continue your work from home. Your company requires you to go through a procedure to apply a password to data before taking it out of the office. You find this procedure is ${e://Field/DH.selfResponseEfficacy}. You believe that if you *don't* complete the password procedure, unauthorized people will ${e://Field/DH.threatVulSev}. You believe the chances of the company knowing if you skipped the procedure are ${e://Field/DH.sanctionCertainSev}."
```
