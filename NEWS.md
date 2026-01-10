## 1.3.0

- Coordinated ecosystem release v1.3.0 (promoting `patientSimPrepare` into the coordinated version line).
- Docs: Phase 7 vignette/user guide hardened (prose-first; no semantic changes).
- Dataset construction semantics from v0.8.x remain locked; this release is version alignment + documentation.

## patientSimPrepare 0.8.2

## 0.8.4

- Docs: rewrite `docs/vignette.md` into a prose-first orientation document (Phase 7), clarifying the observation vs state distinction, one-step interval semantics (`t0 -> t1` with explicit `deltat`), reconstruction guardrails/provenance, and how Prepare fits with Core and Validation.

## 0.8.3

- Add LICENSE file to align repository structure with ecosystem standards.

- Fix follow-up calendar time coercion in `ps_build_ttv_event()` (pass compiled Core time spec to internal coercion).
- Update calendar follow-up unit test to use the current `ps_prepare_observations()` interface.

## patientSimPrepare 0.8.0

- Add spec constructors `ps_spec_state()` and `ps_spec_event()`.
- Specs are now classed objects and are validated against the Core schema at construction time.
- `ps_build_ttv_batch()` now requires spec objects (no raw list specs).

## 0.7.14

- Fix unit test assertion to ignore names on `time_classes` attribute subset.

## patientSimPrepare 0.7.13

- Fix: Phase 1 calendar-time error messaging now explicitly references `ctx$time$unit` when `ctx` is missing (improves guidance for Date/POSIXct inputs).

## patientSimPrepare 0.7.12

- Phase 1 prep functions now support `Date`/`POSIXct` time columns when `ctx$time$unit/origin/zone` is provided, converting calendar time to numeric model time via `patientSimCore` time helpers.
- `ps_prepare_events()` and `ps_prepare_observations()` gained a `ctx` argument (required when time columns are `Date`/`POSIXct`).
- Added unit tests to lock in calendar-time conversion behavior and associated error modes.

## patientSimPrepare 0.7.11

- Tests: add Phase 5 determinism coverage (same inputs/specs -> same spec_hash and identical written datasets when read back).
- Tests: add Phase 1 error-message checks for missing required columns (splits/events/observations).
- Guardrail: warn when time inputs are supplied as `Date`/`POSIXct` and coerced to numeric, reminding users to keep origin/units consistent across tooling (especially `patientSimValidation::build_obs_grid()`).
- Docs: clarify time handling in `docs/vignette.md` (numeric internal time; coercion behavior; no `ctx$time_unit` interpretation).

## patientSimPrepare 0.7.10

- Docs: rewrite `docs/vignette.md` for a non-CS audience (data scientists/DBAs/clinicians), adding explicit definitions and expected input table shapes for `raw_splits`, `raw_events`, and `raw_observations`, and clarifying how core functions behave before/after each code example.

## patientSimPrepare 0.7.9

- Fix: Phase 6 Core-derived provider now includes anchor-time (time == t0) observations/events in the reconstructed Patient history. This enables derived functions that set `include_current = TRUE` to include anchor-time observations, while derived functions with `include_current = FALSE` still exclude them via their own windowing logic.

## patientSimPrepare 0.7.8

- Tests: expand Phase 6 derived-variable test coverage (anchor boundary include/exclude current, multiple anchors, and `derived_on_missing` behavior).
- Docs: rewrite the Phase 7 vignette (`docs/vignette.md`) into a comprehensive, prose-first explanation of the model-first TTV philosophy and workflow (code blocks remain `eval=FALSE`).

## patientSimPrepare 0.7.7

- Chore: promote Phase 6 derived-variable integration to a first-class capability by requiring `patientSimCore (>= 1.2.1)` (moved from Suggests to Imports). Phase 6 tests now run unconditionally.
- Docs: add `README.md` and an initial Phase 7 vignette (`docs/vignette.md`) describing the end-to-end dataset construction workflow (code blocks use `eval=FALSE`).

