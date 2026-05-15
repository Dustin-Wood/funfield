# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**funfield** is an R package for building and analyzing functional field models in psychology. It models expected outcomes of actions in situations as covariance structures, estimates paths from ESJT (Elaborated Situational Judgment Test) data, and visualizes results as causal path diagrams.

## Common Commands

All development happens in R. There is no Makefile or CI infrastructure.

```r
# Install dependencies and load package for development
devtools::load_all()

# Rebuild documentation from roxygen2 comments
devtools::document()

# Check package (R CMD check)
devtools::check()

# Build package
devtools::build()

# Install locally
devtools::install()
```

No formal test suite exists (`tests/` directory is absent). Testing is done interactively.

## Architecture

### Data Convention

All core functions assume input data has `p`, `s`, `i` as the **first three columns**:
- `p` = Person/respondent ID
- `s` = Situation/scenario ID
- `i` = Initiating action
- Remaining columns = features/outcomes/ratings
- the final column is generally 'Likelihood' (if we are trying to model what action a person will take)

### Core Computational Pipeline

```
Raw ESJT Data (p, s, i, ..., Likelihood)
    ↓ fancyr::strLine(), sdESJT()  # data quality screening
    ↓ sweighteddata(), zPSI()      # weighting and standardization
    ↓ ecov_long() → ecov()         # covariance matrix estimation
    ↓ prepField() + fieldResults() # field fitting via lavaan (SEM)
    ↓ medXMY(), modXY()            # mediation/moderation analysis
    ↓ fieldOverlays(), fieldPolygons() # visualization
```

### Node Type System

`prepField()` takes a layout matrix where each cell specifies a node type:
- `d` = Decision/Do node
- `o` = Object node (binary 0/1)
- `c` = Choice/Chance node
- `x` = Continuous variable
- `a` = Appraisal node
- `t` = Combined appraisal+choice
- `v`/`V` = Verbal message nodes
- `e` = Other message nodes
- `y`/`n` = Yes/No voting nodes

### Key Dependencies

- **lavaan** — SEM/path analysis; `fieldResults()` wraps lavaan for covariance fitting
- **dplyr**, **plyr**, **reshape2** — data manipulation (plyr's `ddply`/`ldply` used heavily)
- **psych**, **qgraph** — suggested, not required

### Function Groups

| Group | Key Functions |
|---|---|
| Field construction | `prepField()`, `fieldResults()`, `impliedCov()`, `expOutcomesFF()` |
| Covariance estimation | `ecov()`, `ecov_long()`, `ecov_big()`, `ecov.lmh()` |
| Mediation/moderation | `pathXMY()` (umbrella: X→M→Y, optional moderator), `devPSI()`, `diffPSI()` |
| Data quality | `sdESJT()`, `effectiveN()` (plus `fancyr::strLine()`, `fancyr::prMaxSD()`, `fancyr::spi()`) |
| Visualization | `fieldOverlays()`, `fieldPolygons()`, `vshapes()`, `vsizes()` |
| Utilities | `parView()`, `surgery()`, `lmhgroups()`, `zPSI()` |

### Documentation

All functions use roxygen2 (`#' @` tags). After editing function docs, run `devtools::document()` to regenerate `/man/*.Rd` files and `NAMESPACE`. The vignette at `vignettes/expOutcomes_vignette.Rmd` is incomplete.
