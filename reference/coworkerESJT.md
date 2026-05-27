# Coworker Assertiveness ESJT Dataset

Long-format Elaborated Situational Judgment Test (ESJT) data on
assertiveness decisions in workplace coworker interactions, bundled as a
named list with five linked elements (four data frames plus a codebook).

Participants (N = 249 after quality screening) each rated 12 workplace
scenarios involving a coworker making an inappropriate request. For each
scenario, two possible responses (assertive vs. non-assertive action)
were rated on their expected outcomes and likelihood of being taken. The
rank of the coworker (subordinate, coworker, or manager) was randomly
varied across scenarios within each participant.

## Usage

``` r
coworkerESJT
```

## Format

A named list with five elements:

**`$PSI`** — person × situation × action ratings (5,846 rows). Outcome
columns use the stem *"If you do this... \[action\] ...how likely is
this outcome to result?"* on a 0 to 1 scale:

- p:

  Person ID (Qualtrics response ID string).

- s:

  Situation ID (integer 1–12).

- i:

  Action ID, coded as `"s.a"` (e.g., `"1.1"` = scenario 1 action 1,
  `"1.2"` = scenario 1 action 2).

- Appropriate:

  *Having acted appropriately within your role in the company.* (0 to 1)

- ExpDisapproval:

  *Having directly/forcefully expressed disapproval with your
  \[coworker\]'s behavior.* (0 to 1)

- MajorArg:

  *Having a major argument/confrontation with your \[coworker\].* (0 to
  1)

- GoodRelationP:

  *Having a good working relationship with your \[coworker\] in the long
  run.* (0 to 1)

- Punished:

  *You being formally punished in some way (example: reprimanded or
  fired).* (0 to 1)

- CompleteJob:

  *You completing your job responsibilities/work effectively in the long
  run.* (0 to 1)

- P_Punished:

  *Your \[coworker\] being formally punished in some way (example:
  reprimanded or fired).* (0 to 1)

- CompanySuccess:

  *The overall success of the company.* (0 to 1)

- Likelihood:

  *How likely are you to do the following actions in response to this
  situation?* (0 to 1)

**`$cond`** — situational condition per person-scenario (2,988 rows):

- p:

  Person ID.

- s:

  Situation ID.

- condition:

  Rank of the coworker in the scenario: `"subordinate"`, `"coworker"`,
  or `"manager"`. Randomly varied across scenarios within each
  participant (approximately 4 scenarios per condition).

**`$sit`** — situation-level perceptions, action-independent (2,465
rows; ~82.5% coverage of person × situation combinations). Items use the
stem *"At this point in the situation, to what extent has your
\[coworker\] done something that…"* on a 0 to 1 scale (0 = Greatly
Decreased, 0.5 = Negligible Effect, 1 = Greatly Increased):

- p:

  Person ID.

- s:

  Situation ID.

- HurtCoStanding:

  *Hurt your standing or status/reputation in the group/company.*

- WasUnethical:

  *Should be regarded as unethical, morally inappropriate, or "wrong".*

- Insulted:

  *Has insulted you or shown disrespect to you.*

- HurtCo:

  *Has or will hurt the efficiency or performance of the group/company.*

**`$IIDL`** — person-level IIDL trait ratings (206 persons; not all
participants in `$PSI` completed this measure). Items use the stem *"How
much does each term describe the way you typically act, feel, or see
yourself at work?"* on a 0 to 1 scale (0 = Very Uncharacteristic, 1 =
Very Characteristic):

- p:

  Person ID.

- IIDL_outgoing, IIDL_bold, ...:

  40 IIDL (Interpersonal Identity Description List) trait adjective
  pairs (e.g., *"bold, assertive"*; *"outgoing, sociable"*). See
  `$codebook$IIDL_items` for the full list.

**`$codebook`** — survey wording and scale anchors, a named list with
four elements:

- outcomes:

  Data frame (9 rows): `var`, `label` (full survey wording), `scale`
  (anchor text), `stem` (shared question stem for all outcome items).

- scenarios:

  Data frame (12 rows): `s` (scenario number), `scenario` (vignette
  text, with `[X]` as the rank placeholder), `option_A` (assertive
  action), `option_B` (non-assertive action).

- sit_items:

  Data frame (4 rows): `var`, `label`, `scale`, `stem` for the
  situation-perception items.

- IIDL_items:

  Data frame (40 rows): `var`, `item` (adjective pair wording), `scale`,
  `stem` for the IIDL personality items.

## Source

Wood, D. (unpublished). Assertiveness and power in workplace
interactions: An ESJT approach. Data collected via Qualtrics; reproduced
for use as a canonical example in the funfield package.

## Details

**Data cleaning.** The raw dataset contained 296 participants. Three
sequential screens were applied:

1.  **Straightlining**
    ([`fancyr::strLine()`](https://rdrr.io/pkg/fancyr/man/strLine.html)):
    42 participants removed for zero-variance expected-outcome ratings
    on more than 10% of their action ratings.

2.  **Low variability**
    ([`sdESJT()`](https://dustin-wood.github.io/funfield/reference/sdESJT.md)):
    2 additional participants removed for a within-person SD below 0.25
    across the response scale (total removed by screens 1–2: 44).

3.  **Condition balance**: 3 additional participants removed for not
    having rated at least one *manager* and one *subordinate* scenario,
    or for not having all 12 scenarios represented.

Final analytic sample: **N = 249** participants.

**Typical workflow.** Deviate the Level-1 columns within person *and*
situation with
[`devPSI()`](https://dustin-wood.github.io/funfield/reference/devPSI.md)
before fitting path models — this centers each action's rating on the
person's mean across the actions rated in that same situation, isolating
the within-situation action contrast:


    data(coworkerESJT)
    dev <- devPSI(coworkerESJT$PSI)

**Using the condition moderator.**
[`devPSI()`](https://dustin-wood.github.io/funfield/reference/devPSI.md)
deviates the Level-1 columns within `(p, s)`; the `pathXMY` call uses
`Z.within = TRUE` so the situation-level moderator is within-person
deviated instead (it is constant within `(p, s)`, so it must be deviated
at the person level):


    dat <- merge(devPSI(coworkerESJT$PSI), coworkerESJT$cond, by = c("p","s"))
    dat$manager <- as.numeric(dat$condition == "manager")
    res <- pathXMY(dat, X = "ExpDisapproval", Y = "Likelihood",
                   M = "Appropriate", Z = "manager", Z.within = TRUE)

## Examples

``` r
data(coworkerESJT)
## Basic structure
lapply(coworkerESJT, dim)
#> $PSI
#> [1] 5846   12
#> 
#> $cond
#> [1] 2988    3
#> 
#> $sit
#> [1] 2465    6
#> 
#> $IIDL
#> [1] 206  41
#> 
#> $codebook
#> NULL
#> 
head(coworkerESJT$PSI)
#>                      p  s    i Appropriate ExpDisapproval MajorArg
#> 1 R_00YNbAQXI8ZSk1z     3  3.1        1.00           0.00     0.50
#> 2 R_00YNbAQXI8ZSk1z     1  1.2        1.00           1.00     1.00
#> 3 R_00YNbAQXI8ZSk1z     2  2.2        1.00           1.00     1.00
#> 4 R_00YNbAQXI8ZSk1z    10 10.2        0.00           0.75     0.75
#> 5 R_00YNbAQXI8ZSk1z     2  2.1        1.00           1.00     0.75
#> 6 R_00YNbAQXI8ZSk1z     6  6.2        0.25           0.00     0.25
#>   GoodRelationP Punished CompleteJob P_Punished CompanySuccess Likelihood
#> 1          0.50     0.50        0.50       0.50           0.50       0.75
#> 2          0.00     0.50        0.50       1.00           0.50       1.00
#> 3          0.00     0.50        0.50       0.50           0.50       0.75
#> 4          0.25     0.75        0.25       0.50           0.50       0.00
#> 5          0.25     0.50        0.25       0.75           0.75       1.00
#> 6          0.75     0.50        0.50       0.50           0.50       0.00
table(coworkerESJT$cond$condition)
#> 
#>    coworker     manager subordinate 
#>         980         997        1011 

## Survey wording for outcome variables
coworkerESJT$codebook$outcomes[, c("var", "label")]
#>              var
#> 1    Appropriate
#> 2 ExpDisapproval
#> 3       MajorArg
#> 4  GoodRelationP
#> 5       Punished
#> 6    CompleteJob
#> 7     P_Punished
#> 8 CompanySuccess
#> 9     Likelihood
#>                                                                                 label
#> 1                          Having acted appropriately within your role in the company
#> 2    Having directly/forcefully expressed disapproval with your [coworker]'s behavior
#> 3                          Having a major argument/confrontation with your [coworker]
#> 4             Having a good working relationship with your [coworker] in the long run
#> 5             You being formally punished in some way (example: reprimanded or fired)
#> 6           You completing your job responsibilities/work effectively in the long run
#> 7 Your [coworker] being formally punished in some way (example: reprimanded or fired)
#> 8                                                  The overall success of the company
#> 9       How likely are you to do the following actions in response to this situation?

## First scenario text and action options
coworkerESJT$codebook$scenarios[1, ]
#>   s
#> 1 1
#>                                                                                                                                                                                                                             scenario
#> 1 1. You suspect that your [X] has been taking credit for documents that you have prepared and ideas that you have generated. One afternoon you notice your [X] attaching his/her business card to a presentation that you prepared.
#>                                                                                                option_A
#> 1 Speak with your [X] and tell him/her that the lack of recognition makes you feel unmotivated at work.
#>                                                                                              option_B
#> 1 Tell your [X] that you think his/her behavior is unethical and that you will be filing a complaint.
```
