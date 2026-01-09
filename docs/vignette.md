# patientSimPrepare vignette
## Building train/test/validation datasets from irregular longitudinal tables

This vignette is written for **data scientists, DBAs, and data-savvy clinicians** who work with
time-stamped EHR data that arrives irregularly: labs, vitals, diagnoses, procedures, outcomes.

patientSimPrepare helps you convert those raw tables into **train/test/validation (TTV)** datasets
that are consistent with a simulation model’s semantics.

All code blocks are **examples only** and are marked `eval = FALSE`.

---

## The big idea: model-first datasets

In this ecosystem, a “dataset” is not merely a cleaned table. It is a table whose columns have
**precise meanings** that match how a simulation model will interpret them later.

A helpful mental model is: **the model creates the lens**. Your raw tables are filtered through
that lens to produce inputs that are suitable for fitting the model and for validating the model.
patientSimPrepare is the package that applies that lens in a repeatable, testable way.

---

## Time: numeric model time with calendar-friendly boundaries

Internally, the patientSim ecosystem operates on a **numeric model time axis**. For example, your
model might measure time in *days*, *weeks*, *months* (fixed 30.4375 days), or *years* (fixed 365.25 days).

However, EHR tables typically store measurement times as calendar values:
- `Date` (a date)
- `POSIXct` / `POSIXt` (a date + time, a timestamp)

patientSimPrepare supports both. When you supply `Date` or `POSIXct` times, you must provide a
context object (`ctx`) that declares how calendar time maps onto the model time axis:

- `ctx$time$unit` (required): `"days"`, `"weeks"`, `"months"`, `"years"`, etc.
- `ctx$time$origin` (optional): the mapping origin (defaults to 1970-01-01)
- `ctx$time$zone` (optional): time zone for POSIXct mapping (defaults to `"UTC"`)

**Important:** time-only inputs (for example `difftime` or `hms`) are not supported. If you have
time-only values, attach a date first, or convert to numeric model time before calling Prepare.

---

## Specs: declaring your “lens” up front

A *spec* is a compact declaration of what you are trying to build. For state models, a spec states:
1) which observation group defines interval anchors (for example, “blood pressure visits”)
2) which variables are outcomes at the end of an interval
3) which variables are predictors evaluated at the start of an interval

Specs are validated **against the model schema** (from patientSimCore) at construction time. This
prevents subtle mismatches like typos, wrong variable types, or invalid categorical levels.

You construct specs using:

- `ps_spec_state(schema, ...)`
- `ps_spec_event(event_type, ...)`

---

## What patientSimPrepare expects as input

patientSimPrepare works with four families of tables:

1) **Splits** (patient-level membership in train/test/validation)
2) **Events** (outcomes and censoring signals)
3) **Observations** (measurements)
4) **Follow-up** (start/end of observation time for each patient, used for censoring)

These are ordinary R data frames (or tibbles). They do not need to come from any specific database
schema, but they must have the required columns described below.

### 1) Splits

A splits table assigns each patient to a split:

- required columns:
  - patient id
  - split label (`train`, `test`, `validation`)

```r
# eval = FALSE
raw_splits <- data.frame(
  pid   = c("a", "b", "c"),
  split = c("train", "test", "validation")
)

splits <- ps_prepare_splits(
  raw_splits,
  id_col    = "pid",
  split_col = "split"
)
```

### 2) Events

Events are discrete occurrences that matter to your model (e.g., MI hospitalization, stroke).
Prepare converts one or more event tables into a single canonical event stream:

- required columns:
  - patient id
  - time (`numeric`, `Date`, or `POSIXct`)
  - event type (either provided by a column or by the list element name)

```r
# eval = FALSE
ctx <- list(time = list(unit = "days", zone = "UTC"))

raw_events <- data.frame(
  pid  = c("a", "a", "b"),
  when = as.Date(c("1970-01-02", "1970-01-06", "1970-01-03")),
  type = c("visit", "mi", "visit")
)

events <- ps_prepare_events(
  raw_events,
  id_col   = "pid",
  time_col = "when",
  type_col = "type",
  ctx      = ctx
)
```

### 3) Observations

Observations are measurement tables (labs, vitals, surveys) that are irregular in time. You may
provide a single data frame or a named list of data frames. Each row is one measurement “moment”.

- required columns (per table):
  - patient id
  - time (`numeric`, `Date`, or `POSIXct`)
  - group (a measurement group label)
  - one or more measurement columns

```r
# eval = FALSE
raw_labs <- data.frame(
  pid   = c("a", "a", "b", "b"),
  when  = as.Date(c("1970-01-02", "1970-01-06", "1970-01-03", "1970-01-07")),
  group = c("bp", "bp", "bp", "bp"),
  sbp   = c(120, 130, 110, 115)
)

observations <- ps_prepare_observations(
  list(labs = raw_labs),
  id_col    = "pid",
  time_col  = "when",
  group_col = "group",
  ctx       = ctx
)
```

### 4) Follow-up

Follow-up defines when each patient is “under observation” for censoring purposes. It is used by
`ps_build_ttv_event()` and `ps_build_ttv_state()` when constructing intervals.

- required columns:
  - patient id
  - followup start (`numeric`, `Date`, or `POSIXct`)
  - followup end (`numeric`, `Date`, or `POSIXct`)
- optional:
  - death time (for censoring at death)

```r
# eval = FALSE
followup <- data.frame(
  patient_id     = c("a", "b"),
  followup_start = as.Date(c("1970-01-01", "1970-01-01")),
  followup_end   = as.Date(c("1970-01-11", "1970-01-11"))
)
```

---

## Building datasets

### Event model datasets (one-step, event-time native)

`ps_build_ttv_event()` constructs one row per patient describing the interval from a start policy
(often baseline) to either the first event occurrence or censoring.

```r
# eval = FALSE
ttv_event <- ps_build_ttv_event(
  events     = events,
  splits     = splits,
  ctx        = ctx,
  event_type = "mi",
  followup   = followup
)
```

### State model datasets (intervals anchored by observed measurements)

`ps_build_ttv_state()` constructs consecutive within-patient intervals anchored by observation
times in a chosen outcome group. Predictors are reconstructed at the start of the interval using
LOCF with optional lookback and staleness guardrails.

```r
# eval = FALSE
ttv_state <- ps_build_ttv_state(
  observations   = observations,
  splits         = splits,
  ctx            = ctx,
  outcome_group  = "bp",
  outcome_vars   = c("sbp"),
  predictor_vars = c("sbp"),
  followup       = followup
)
```

---

## Building batches from validated specs

When you scale up beyond a single dataset, you typically define one or more specs and build them
in a batch. This makes your modeling intent explicit and keeps dataset construction consistent.

```r
# eval = FALSE
schema <- patientSimCore::default_patient_schema()

spec_bp <- ps_spec_state(
  schema        = schema,
  outcome_group = "bp",
  outcome_vars  = "sbp",
  predictor_vars = c("sbp"),
  name          = "Blood pressure (SBP)"
)

batch <- ps_build_ttv_batch(
  observations = observations,
  events       = events,
  splits       = splits,
  specs        = list(spec_bp),
  followup     = followup,
  ctx          = ctx
)
```

---

## Final notes

- If you have calendar times in your raw tables, **always** supply `ctx` with `ctx$time$unit`.
- Keep `ctx$time$zone = "UTC"` unless you have a strong reason to do otherwise.
- Specs are validated against the model schema early so that dataset construction fails fast and
  fails clearly, before you invest time in downstream modeling work.
