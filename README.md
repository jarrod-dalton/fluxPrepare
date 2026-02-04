# patientSimPrepare

`patientSimPrepare` constructs **training/test/validation (TTV)** datasets from raw event and observation tables for the **patientSim** ecosystem.

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
7. Workflow vignette/user guide (docs)

## Quick start (minimal)

```r
# eval=FALSE
library(patientSimPrepare)

# 1) Canonicalize inputs
splits <- prepare_splits(splits)
events  <- prepare_events(events)
obs     <- prepare_observations(obs)

# 2) Event-model intervals
spec_event <- list(
  event_type = "mi",
  censoring = list(followup_end = "followup_end")
)

ttv_event <- ps_build_ttv_event(
  splits = splits,
  events = events,
  spec = spec_event
)

# 3) State reconstruction at anchors
state_at_t0 <- ps_reconstruct_state_at(
  anchors = data.frame(patient_id = c("p1"), t0 = c(365)),
  observations = obs,
  vars = c("sbp", "ldl"),
  lookback_t = 365 * 2,
  stale_after_t = 365
)

# 4) State-transition datasets
spec_state <- list(group = "bp")

ttv_state <- ps_build_ttv_state(
  splits = splits,
  observations = obs,
  spec = spec_state
)
```

## Documentation

- `docs/vignette.md` provides an end-to-end workflow narrative (code shown as `eval=FALSE`).
- `man/*.Rd` contains manually-maintained reference docs (no roxygen).
