\name{fluxPrepare-package}
\alias{fluxPrepare}
\alias{fluxPrepare-package}
\title{fluxPrepare: Prepare Training/Test/Validation datasets for flux}
\description{
Utilities to translate observational event and measurement tables, aligned to a fluxCore schema,
into reproducible training/test/validation datasets for one-step event and state transition models.
The package is spec-first and schema-aligned.
}
\details{
Key phases (see NEWS.md):
\itemize{
  \item Phase 1: canonicalize splits, events, observations.
  \item Phase 2: one-step event-model TTV datasets.
  \item Phase 3: as-of state reconstruction (LOCF + guardrails + provenance).
  \item Phase 4: one-step state-transition TTV datasets.
  \item Phase 5: batch mode + disk-backed outputs.
  \item Phase 6: schema-derived variable integration at anchors (Core-backed).
}
}
\author{Jarrod Dalton}
