# Work-Overtime ESJT Dataset

Long-format Elaborated & eXperimental Situational Judgment Test (EXSJT)
data on work-overtime decisions. Bundled as a named list with five
linked elements (four data frames plus a codebook).

The same Prolific respondents who completed the speeding and data-home
scenarios also rated the work-overtime scenario described here; the
three datasets
([`speedingESJT`](https://dustin-wood.github.io/funfield/reference/speedingESJT.md),
`overtimeESJT`,
[`dataHomeESJT`](https://dustin-wood.github.io/funfield/reference/dataHomeESJT.md))
share a participant pool and can be linked by `p` to examine
within-person consistency across domains.

Each participant (N = 329 after quality screening) rated two actions
(agree to work overtime; decline) on nine expected outcomes and on the
likelihood of taking each action. Eight features of the situation were
randomly varied at the person level (one fully crossed scenario per
respondent).

## Usage

``` r
overtimeESJT
```

## Format

A named list with five elements:

**`$PSI`** — person × action ratings (658 rows = 329 persons × 2
actions). All Likert-rated columns are rescaled to \[0, 1\]:

- p:

  Person ID (Qualtrics response ID string).

- s:

  Situation ID (constant: `"WO"`).

- i:

  Action text as displayed (`"agree to work overtime"` or
  `"decline to work overtime"`).

- Overtime:

  Action coded on \[0, 1\]: `0` = decline, `1` = agree.

- RelationshipQuality:

  *The quality of your relationship with \[coworker/boss\].*

- EmployerValuation:

  *The company's understanding of your value as an employee.*

- OutsideRelationships:

  *The quality of your relationships with people outside of work.*

- Drained:

  *Your feeling of being drained/tired from work.*

- CompanyHealth:

  *The health of the company.*

- Finances:

  *Your overall financial well-being.*

- Appropriate:

  *Your sense that you had acted appropriately.*

- RaisePromotion:

  *Likelihood of company giving you a raise or promotion in the near
  future.*

- Punished:

  *Likelihood of company firing/punishing you in the near future.*

- Likelihood:

  *How likely you would be to do each action.*

**`$cond`** — situation-level experimental factors (329 rows):

- p, s:

  Person and situation IDs.

- CoType:

  Who is asking: `"A coworker"` or `"Your boss"`.

- CoType2, CoType3:

  Linked variants of `CoType` used at later positions in the scenario
  template (e.g., `"The coworker"`, `"your boss"`). Same underlying
  assignment as `CoType`.

- weekday:

  `"Friday evening"` or `"Wednesday evening"`.

- priorPlans:

  `"don't have any plans"` or
  `"have previously made plans to meet with friends"`.

- workDifficulty:

  `"difficult"` or `"fairly easy"`.

- cashReward:

  Whether overtime pay is offered.

- projectImpact:

  Whether the project's value to the company is clear.

- scenario:

  Full assembled scenario text as displayed to the respondent; see
  `$codebook$scenario$template` for the canonical Qualtrics template
  with placeholders.

**`$sit`** — situation-level perceptions (329 rows):

- p, s:

  Person and situation IDs.

- Realistic:

  *The scenario is realistic.*

- Relatable:

  *A similar scenario could happen to me.*

**`$traits`** — person-level self-report items + demographics (329
rows). `SR_01`–`SR_30` are the 30 raw self-report items (shared across
all three EXSJT scenarios); see `$codebook$trait_items` for wording and
`$codebook$trait_scales` for the composite recipes used in Wood, Harms,
& Cho (2023).

**`$codebook`** — survey wording, scale anchors, factor levels, and
trait composite recipes. See
[`speedingESJT`](https://dustin-wood.github.io/funfield/reference/speedingESJT.md)
for full structural detail.

## Source

Wood, D., Harms, P. D., & Cho, S. (2023). EXSJT survey (Prolific panel);
same respondents as `speedingESJT` and `dataHomeESJT`.

## Details

**Data cleaning.** Three sequential data-quality screens (same as
`speedingESJT`):
[`fancyr::strLine()`](https://rdrr.io/pkg/fancyr/man/strLine.html) \>
0.10, [`fancyr::prMaxSD()`](https://rdrr.io/pkg/fancyr/man/prMaxSD.html)
\< 0.25, and combined
[`fancyr::spi()`](https://rdrr.io/pkg/fancyr/man/spi.html) \< 1 across
the three rating blocks (likelihood + two outcome blocks). Respondents
with missing timing data are retained. Final N = 329.

**Rescaling.** All Likert items rescaled from the original 1-5 Qualtrics
codes to \[0, 1\] via
[`fancyr::pomp()`](https://rdrr.io/pkg/fancyr/man/pomp.html).

## Examples

``` r
data(overtimeESJT)
lapply(overtimeESJT, function(x) if (is.data.frame(x)) dim(x) else length(x))
#> $PSI
#> [1] 658  14
#> 
#> $cond
#> [1] 329  11
#> 
#> $sit
#> [1] 329   4
#> 
#> $traits
#> [1] 329  37
#> 
#> $codebook
#> [1] 8
#> 
table(overtimeESJT$cond$CoType)
#> 
#> A coworker  Your boss 
#>        168        161 
overtimeESJT$codebook$scenario$template
#> [1] "${e://Field/WO.CoType} asks if you can work overtime on a project for a couple hours tomorrow, a ${e://Field/WO.weekday}. You ${e://Field/WO.priorPlans} at that time. ${e://Field/WO.CoType2} says that ${e://Field/WO.cashReward}. It seems like the work would be ${e://Field/WO.workDifficulty}. It ${e://Field/WO.projectImpact}."
```
