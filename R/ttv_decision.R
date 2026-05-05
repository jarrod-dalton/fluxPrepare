#' Build a decision-point TTV dataset
#'
#' Constructs a decision-anchored dataset by reconstructing predictor state at
#' decision times while preserving decision metadata such as decision point id,
#' selected action, and any additional carried columns.
#'
#' @param decisions A data.frame containing at least entity id, decision time,
#' and decision point id.
#' @param observations Canonical observation store from prepare_observations().
#' @param splits Canonical split table from prepare_splits().
#' @param predictor_vars Predictor variable names reconstructed at decision time.
#' @param id_col Entity id column in decisions.
#' @param time_col Decision time column in decisions.
#' @param decision_point_col Decision point identifier column in decisions.
#' @param carry_cols Optional additional columns from decisions to preserve. NULL
#' means carry all non-id/non-time columns.
#' @param lookback Lookback passed to reconstruct_state_at().
#' @param staleness Staleness passed to reconstruct_state_at().
#' @param keep_provenance Logical; include reconstruction provenance columns.
#' @param row_policy One of "return_all" or "drop_incomplete".
#' @param time_spec Optional fluxCore time_spec object used when decision times are Date/POSIXct.
#'
#' @return A data.frame with class "flux_ttv_decision".
#'
#' @export
build_ttv_decision <- function(decisions,
                               observations,
                               splits,
                               predictor_vars,
                               id_col = "entity_id",
                               time_col = "decision_time",
                               decision_point_col = "decision_point_id",
                               carry_cols = NULL,
                               lookback = Inf,
                               staleness = Inf,
                               keep_provenance = TRUE,
                               row_policy = c("return_all", "drop_incomplete"),
                               time_spec = NULL) {
  row_policy <- match.arg(row_policy)

  .flux_assert_data_frame(decisions, "decisions")
  .flux_assert_data_frame(observations, "observations")
  .flux_assert_data_frame(splits, "splits")
  .flux_assert_has_cols(decisions, c(id_col, time_col, decision_point_col), "decisions")
  .flux_assert_has_cols(splits, c("entity_id", "split"), "splits")

  if (!is.character(predictor_vars) || length(predictor_vars) < 1L) {
    stop("build_ttv_decision(): predictor_vars must be a non-empty character vector.", call. = FALSE)
  }
  predictor_vars <- unique(as.character(predictor_vars))
  .flux_assert_has_cols(observations, c("entity_id", "time", predictor_vars), "observations")

  if (is.null(carry_cols)) {
    carry_cols <- setdiff(names(decisions), c(id_col, time_col))
  } else {
    if (!is.character(carry_cols)) {
      stop("build_ttv_decision(): carry_cols must be NULL or a character vector.", call. = FALSE)
    }
    carry_cols <- unique(as.character(carry_cols))
    .flux_assert_has_cols(decisions, carry_cols, "decisions")
    carry_cols <- setdiff(carry_cols, c(id_col, time_col))
  }

  if (!decision_point_col %in% carry_cols) {
    carry_cols <- c(decision_point_col, carry_cols)
  }
  carry_cols <- unique(carry_cols)

  anchors <- decisions[, unique(c(id_col, time_col, carry_cols)), drop = FALSE]
  anchors[[id_col]] <- as.character(anchors[[id_col]])

  out <- reconstruct_state_at(
    anchors = anchors,
    observations = observations,
    vars = predictor_vars,
    id_col = id_col,
    time_col = time_col,
    keep_anchor_cols = TRUE,
    lookback = lookback,
    staleness = staleness,
    keep_provenance = keep_provenance,
    row_policy = row_policy,
    time_spec = time_spec
  )

  split_idx <- match(out$entity_id, splits$entity_id)
  if (anyNA(split_idx)) {
    missing <- unique(out$entity_id[is.na(split_idx)])
    stop(sprintf(
      "build_ttv_decision(): splits table is missing %d entity(s) from decisions. Example(s): %s",
      length(missing), paste0(utils::head(missing, 10), collapse = ", ")
    ), call. = FALSE)
  }

  out$split <- splits$split[split_idx]
  out$decision_time <- out$t0

  base_cols <- c("entity_id", "split", "decision_time", "t0")
  carry_out <- unique(setdiff(carry_cols, c("entity_id", "split", "decision_time", "t0")))
  remaining <- setdiff(names(out), c(base_cols, carry_out))
  out <- out[, c(base_cols, carry_out, remaining), drop = FALSE]

  attr(out, "spec") <- list(
    task = "decision",
    predictor_vars = predictor_vars,
    decision_point_col = decision_point_col,
    carry_cols = carry_cols,
    lookback = lookback,
    staleness = staleness,
    keep_provenance = keep_provenance,
    row_policy = row_policy
  )

  attr(out, "metadata") <- list(
    n_rows = nrow(out),
    n_entities = length(unique(out$entity_id)),
    split_counts = as.list(table(out$split)),
    built_with = "build_ttv_decision"
  )

  class(out) <- c("flux_ttv_decision", class(out))
  out
}