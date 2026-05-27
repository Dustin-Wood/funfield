# Build an ESJT/EXSJT dataset from raw Qualtrics data

Convert raw Qualtrics survey output (data + column map) into the
funfield-canonical named-list ESJT/EXSJT structure: `$PSI`, `$cond`,
`$sit`, `$traits`, `$codebook`. All Likert ratings are rescaled to a
\[0, 1\] proportion-of-maximum-possible (POMP) metric via
[`fancyr::pomp()`](https://rdrr.io/pkg/fancyr/man/pomp.html). Codebook
elements (variable wording, scale anchors, factor levels) are populated
automatically from the column map.

Designed to accept the output of
[`fancyr::fetch_survey_plus()`](https://rdrr.io/pkg/fancyr/man/fetch_survey_plus.html),
which returns a list with `$data` (the response data) and `$colmap` (a
tibble of `qname`, `main`, `sub`, `value_labels`).

## Usage

``` r
esjtFromQualtrics(
  data,
  colmap,
  scenario,
  action_text,
  likelihood,
  outcomes,
  outcome_names,
  action_code = NULL,
  action_code_name = "Speed",
  scenario_text = NULL,
  scenario_template = NULL,
  conditions = NULL,
  perceptions = NULL,
  traits = NULL,
  trait_names = NULL,
  demographics = NULL,
  timing_blocks = NULL,
  smin = 1,
  smax = 5,
  strLine_max = 0.1,
  prMaxSD_min = 0.25,
  spi_min = 1,
  id = "ResponseId"
)
```

## Arguments

- data:

  Response data frame (typically `xsjt$data`). One row per respondent.

- colmap:

  Column map data frame with columns `qname`, `main`, `sub`,
  `value_labels` (typically `xsjt$colmap`).

- scenario:

  Short tag for the scenario; used as the `s` value in `$PSI` (e.g.,
  `"SL"`, `"WO"`, `"DH"`).

- action_text:

  Character vector of qnames pointing to the columns that carry the
  action display text (one qname per action).

- likelihood:

  Character vector of qnames for the per-action likelihood ratings,
  parallel to `action_text`.

- outcomes:

  List of character vectors. `outcomes[[i]]` holds the qnames of the
  outcome ratings for action `i`. All elements should have the same
  length (= `length(outcome_names)`).

- outcome_names:

  Names to use in `$PSI` for the outcome columns, parallel to each
  element of `outcomes`.

- action_code:

  Optional numeric coding for the actions on \[0, 1\]; if `NULL`, uses
  `seq(0, 1, length.out = length(action_text))`. Stored in `$PSI` as a
  column named per `action_code_name`.

- action_code_name:

  Column name for the numeric action coding in `$PSI` (default `"Speed"`
  for backwards compatibility with the speeding example; pass a
  domain-relevant name for other scenarios).

- scenario_text:

  Qname of the assembled scenario text column (e.g., `"SL.scenario"`,
  `"overtimeScenario"`); if `NULL`, the function tries
  `paste0(scenario, ".scenario")`.

- scenario_template:

  Optional character. The canonical scenario template (typically the
  Qualtrics text with `${e://Field/...}` placeholders intact). Stored
  verbatim in `$codebook$scenario$template`. If `NULL`, the function
  falls back to a sample value from `data[[scenario_text]]`, which will
  be one specific respondent's fully-interpolated scenario.

- conditions:

  Character vector of qnames for situation-level factor columns; if
  `NULL`, auto-detected as all colmap qnames matching
  `^\Q{scenario}\E\.`, excluding `scenario_text`.

- perceptions:

  Named character vector mapping perception names to qnames (e.g.,
  `c(Realistic = "Speeding.Real_1", Relatable = "Speeding.Real_2")`).

- traits:

  Character vector of qnames for person-level rated items.

- trait_names:

  Optional names for trait columns in `$traits`; if `NULL`, derived by
  replacing spaces with underscores in the qnames.

- demographics:

  Named character vector mapping demographic variable names to qnames
  (e.g., `c(age = "Q13", gender = "Q14")`). Values are preserved as raw
  Qualtrics codes.

- timing_blocks:

  Optional list of lists, each with `first`, `last`, and `n` (number of
  items in the timed block). Combined total time / total transitions
  becomes the SPI input.

- smin, smax:

  Lower and upper bounds of the original Likert metric for all rated
  columns (default 1 and 5).

- strLine_max, prMaxSD_min, spi_min:

  Screen thresholds. Set any to `NULL` to skip that screen. NA-in-screen
  is treated as "no evidence to drop" — respondents with missing screen
  values are retained.

- id:

  Qname of the respondent ID column (default `"ResponseId"`).

## Value

A named list with elements `$PSI`, `$cond`, `$sit`, `$traits`, and
`$codebook`.
