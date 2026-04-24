## fluxPrepare 1.8.0

- Coordinated ecosystem release alignment to version 1.8.0.
- Dependency floor updated to `fluxCore (>= 1.8.0)`.
- Added README release/download badges; no functional Prepare API changes.

## fluxPrepare 1.7.0

- Coordinated ecosystem release alignment to version 1.7.0.
- Dependency floor updated to `fluxCore (>= 1.7.0)`.
- No additional functional changes beyond prior 1.5.x documentation/API sync.

## fluxPrepare 1.5.0

- Release polish pass: check() note cleanup for namespace-qualified utility calls and Rd usage formatting.

- Documentation synchronization: refreshed manual Rd signatures/usages to match code and avoid codoc drift.

- Licensing update: switched package license to LGPL-3.

## fluxPrepare 1.4.0

- Documentation alignment: manual `.Rd` pages were refreshed to match current exported function names/signatures (including `build_ttv_event()`, `build_ttv_state()`, `build_ttv_batch()`, `chunk_entities()`, and `reconstruct_state_at()`).
- Docs/examples: refreshed README/vignette examples and API references to current non-`flux_` naming.
- Packaging hygiene: removed roxygen-style blocks from `R/` and standardized filenames to underscore style.

## fluxPrepare 1.3.6

- Compatibility: update tests to use `fluxCore::declare_variable()` (renamed from `fluxCore::var()` to avoid masking `stats::var()`).

## fluxPrepare 1.3.5

- Bugfix: `segment_bins()` now permits `-Inf`/`Inf` endpoints (e.g., `c(-Inf, 160, Inf)`) while still requiring strictly increasing cutpoints.

## fluxPrepare 1.3.4

- API: removed the `flux_` prefix from the user-facing function names (e.g., `prepare_events()`, `build_ttv_state()`, `spec_event_process()`), reflecting that Prepare is intended for broad use by EHR analysts.
- Event-process time segmentation: `spec_event_process()` gains `candidate_times` to control candidate interval boundaries (`"groups"`, `"vars"`, or `"groups_or_vars"`).
- New helpers: `segment_bins()`, `segment_eps()`, `segment_rel_eps()`, `segment_flip()`, and `segment_rules_combine()` make segmentation rules easier to write and reuse.

## fluxPrepare 1.3.3

- Bugfix: event-process time segmentation now initializes baseline covariate values from the first observed measurement after t0 (when t0 precedes measurement history), ensuring meaningful-change rules (e.g., bin crossings) can create interval boundaries as intended.

## fluxPrepare 1.3.2

- Event-process TTV construction: added optional time-segmentation rules based on "meaningful change" in specified covariates (e.g., bin crossings, tolerance thresholds, boolean flips), to avoid over-fragmenting slow-process risk intervals when frequent measurements change only trivially.
- Bugfix: `reconstruct_state_at()` now accepts an optional `ctx` argument for Date/POSIXt time handling.

## fluxPrepare 1.3.1

- Added `spec_event_process()` and `build_ttv_event_process()` to construct start-stop TTV datasets for cause-specific hazard and parametric competing-risk event models, with optional capped-frequency splitting via `min_dt`.

## 1.3.0

- Coordinated ecosystem release v1.3.0 (promoting `fluxPrepare` into the coordinated version line).
- Docs: Phase 7 vignette/user guide hardened (prose-first; no semantic changes).
- Dataset construction semantics from v0.8.x remain locked; this release is version alignment + documentation.

## fluxPrepare 0.8.2

## 0.8.4

- Docs: rewrite `docs/vignette.md` into a prose-first orientation document (Phase 7), clarifying the observation vs state distinction, one-step interval semantics (`t0 -> t1` with explicit `deltat`), reconstruction guardrails/provenance, and how Prepare fits with Core and Validation.

## 0.8.3

- Add LICENSE file to align repository structure with ecosystem standards.

- Fix follow-up calendar time coercion in `build_ttv_event()` (pass compiled Core time spec to internal coercion).
- Update calendar follow-up unit test to use the current `prepare_observations()` interface.

## fluxPrepare 0.8.0

- Add spec constructors `spec_state()` and `spec_event()`.
- Specs are now classed objects and are validated against the Core schema at construction time.
- `build_ttv_batch()` now requires spec objects (no raw list specs).

## 0.7.14

- Fix unit test assertion to ignore names on `time_classes` attribute subset.

## fluxPrepare 0.7.13

- Fix: Phase 1 calendar-time error messaging now explicitly references `ctx$time$unit` when `ctx` is missing (improves guidance for Date/POSIXct inputs).

## fluxPrepare 0.7.12

- Phase 1 prep functions now support `Date`/`POSIXct` time columns when `ctx$time$unit/origin/zone` is provided, converting calendar time to numeric model time via `fluxCore` time helpers.
- `prepare_events()` and `prepare_observations()` gained a `ctx` argument (required when time columns are `Date`/`POSIXct`).
- Added unit tests to lock in calendar-time conversion behavior and associated error modes.

## fluxPrepare 0.7.11

- Tests: add Phase 5 determinism coverage (same inputs/specs -> same spec_hash and identical written datasets when read back).
- Tests: add Phase 1 error-message checks for missing required columns (splits/events/observations).
- Guardrail: warn when time inputs are supplied as `Date`/`POSIXct` and coerced to numeric, reminding users to keep origin/units consistent across tooling (especially `fluxValidation::build_obs_grid()`).
- Docs: clarify time handling in `docs/vignette.md` (numeric internal time; coercion behavior; no `ctx$time_unit` interpretation).

## fluxPrepare 0.7.10

- Docs: rewrite `docs/vignette.md` for a non-CS audience (data scientists/DBAs/clinicians), adding explicit definitions and expected input table shapes for `raw_splits`, `raw_events`, and `raw_observations`, and clarifying how core functions behave before/after each code example.

## fluxPrepare 0.7.9

- Fix: Phase 6 Core-derived provider now includes anchor-time (time == t0) observations/events in the reconstructed Entity history. This enables derived functions that set `include_current = TRUE` to include anchor-time observations, while derived functions with `include_current = FALSE` still exclude them via their own windowing logic.

## fluxPrepare 0.7.8

- Tests: expand Phase 6 derived-variable test coverage (anchor boundary include/exclude current, multiple anchors, and `derived_on_missing` behavior).
- Docs: rewrite the Phase 7 vignette (`docs/vignette.md`) into a comprehensive, prose-first explanation of the model-first TTV philosophy and workflow (code blocks remain `eval=FALSE`).

## fluxPrepare 0.7.7

- Chore: promote Phase 6 derived-variable integration to a first-class capability by requiring `fluxCore (>= 1.2.1)` (moved from Suggests to Imports). Phase 6 tests now run unconditionally.
- Docs: add `README.md` and an initial Phase 7 vignette (`docs/vignette.md`) describing the end-to-end dataset construction workflow (code blocks use `eval=FALSE`).
