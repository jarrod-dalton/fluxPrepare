# fluxPrepare
[![Release](https://img.shields.io/github/v/release/jarrod-dalton/fluxPrepare?display_name=tag)](https://github.com/jarrod-dalton/fluxPrepare/releases)
[![Downloads](https://img.shields.io/github/downloads/jarrod-dalton/fluxPrepare/total)](https://github.com/jarrod-dalton/fluxPrepare/releases)
[![License: LGPL-3](https://img.shields.io/badge/license-LGPL--3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![Language: R](https://img.shields.io/badge/language-R-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)

`fluxPrepare` constructs **training/test/validation (TTV)** datasets from raw event and observation tables for the **flux** ecosystem.

This package is intentionally **spec-first** and **schema-aligned**:

- **Observations** are sparse, irregular, and incomplete (think: EHR tables).
- **State** is a schema-defined snapshot **as-of an anchor time** (`t0`), reconstructed under explicit rules.
- **Derived variables** are **state transformations** (not ad hoc ML features) and must be evaluated using the
  same machinery used during simulation to guarantee **training/application equivalence**.

## Scope (locked)

- **No time grids**: anchors are event-native; no discretization is imposed by default.
- **One-step only**: datasets represent `t0 -> t1` with explicit `deltat = t1 - t0`.
- Predictors are evaluated at **interval start** (`t0`); labels are evaluated at **interval end** (`t1`).

## Phased build plan

Implementation is tracked in `NEWS.md` and guarded by unit tests:

1. Splits + canonicalization helpers
2. Event-model TTV datasets
3. As-of state reconstruction (LOCF + guardrails)
4. State-transition TTV datasets
5. Batch mode + disk-backed outputs
6. Derived-variable integration at anchors (Core-backed)
7. Workflow guidance and examples

## Quick start (minimal)

```r
# eval=FALSE
library(fluxPrepare)

# 1) Canonicalize inputs
splits <- prepare_splits(splits)
events  <- prepare_events(events)
obs     <- prepare_observations(obs)

# 2) Event-model intervals
spec_event <- list(
  event_type = "mi",
  censoring = list(followup_end = "followup_end")
)

ttv_event <- build_ttv_event(
  splits = splits,
  events = events,
  event_type = "mi"
)

# 3) State reconstruction at anchors
state_at_t0 <- reconstruct_state_at(
  anchors = data.frame(entity_id = c("p1"), t0 = c(365)),
  observations = obs,
  vars = c("sbp", "ldl"),
  lookback = 365 * 2,
  staleness = 365
)

# 4) State-transition datasets
ttv_state <- build_ttv_state(
  splits = splits,
  observations = obs,
  outcome_group = "bp",
  outcome_vars = c("sbp"),
  predictor_vars = c("sbp", "ldl")
)
```

## Documentation

- `man/*.Rd` contains manually-maintained reference docs (no roxygen).
