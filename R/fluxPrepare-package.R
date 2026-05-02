#' fluxPrepare: Prepare Training/Test/Validation datasets for flux
#'
#' Utilities to translate observational event and measurement tables, aligned to a fluxCore schema,
#' into reproducible training/test/validation datasets for one-step event and state transition models.
#' The package is spec-first and schema-aligned.
#'
#' @details
#' Key phases (see NEWS.md):
#' Phase 1: canonicalize splits, events, observations.
#' Phase 2: one-step event-model TTV datasets.
#' Phase 3: as-of state reconstruction (LOCF + guardrails + provenance).
#' Phase 4: one-step state-transition TTV datasets.
#' Phase 5: batch mode + disk-backed outputs.
#' Phase 6: schema-derived variable integration at anchors (Core-backed).
#'
#' @keywords internal
#' "_PACKAGE"
