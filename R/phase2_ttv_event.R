#' Build one-step TTV dataset for an event model
#'
#' Constructs an interval-based training/test/validation (TTV) dataset for a specified event type.
#' Each row corresponds to a patient-level interval starting at t0 and ending at t1, where:
#' \itemize{
#'   \item t0 is defined by a start-time policy (baseline).
#'   \item t1 is the next occurrence time of the requested event type after t0, if it occurs before censoring.
#'   \item Otherwise, t1 is the censoring time (e.g., follow-up end).
#' }
#'
#' Output includes \code{deltat = t1 - t0} and an \code{event_occurred} indicator.
#' This function is deliberately one-step and event-time native (no time grid).
#'
#' @param events A canonical event stream as returned by \code{ps_prepare_events()}.
#' @param splits A patient split table as returned by \code{ps_prepare_splits()}.
#' @param event_type Character scalar specifying the event type to model.
#' @param t0_strategy How to define t0 for each patient. One of:
#'   \itemize{
#'     \item \code{"followup_start"}: use follow-up start if provided, else the patient's first observed event time.
#'     \item \code{"first_event"}: use the patient's first observed event time.
#'     \item \code{"fixed"}: use \code{fixed_t0} for all patients.
#'   }
#' @param fixed_t0 Numeric scalar used when \code{t0_strategy = "fixed"}.
#' @param followup Optional data.frame containing follow-up boundaries. If provided, must include
#'   patient_id and columns given by \code{fu_start_col} and \code{fu_end_col}. Times may be numeric, Date, or POSIXt.
#' @param fu_start_col Follow-up start column name in \code{followup}.
#' @param fu_end_col Follow-up end column name in \code{followup}.
#' @param death_col Optional death time column name in \code{followup}. If provided, censoring time is \code{pmin(fu_end, death_time)}.
#' @return A data.frame with columns: patient_id, split, t0, t1, deltat, event_occurred, time_to_event,
#'   censoring_time. Attributes include \code{spec} and \code{metadata}.
#' @export
ps_build_ttv_event <- function(events,
                               splits,
                               ctx = NULL,
                               event_type,
                               t0_strategy = c("followup_start", "first_event", "fixed"),
                               fixed_t0 = 0,
                               followup = NULL,
                               fu_start_col = "followup_start",
                               fu_end_col = "followup_end",
                               death_col = NULL) {
  .ps_assert_data_frame(events, "events")
  .ps_assert_data_frame(splits, "splits")
  .ps_assert_has_cols(events, c("patient_id", "time", "event_type"), "events")
  .ps_assert_has_cols(splits, c("patient_id", "split"), "splits")

  if (!is.character(event_type) || length(event_type) != 1 || is.na(event_type) || event_type == "") {
    stop("ps_build_ttv_event(): event_type must be a non-empty character scalar.", call. = FALSE)
  }

  t0_strategy <- match.arg(t0_strategy)
  fixed_t0 <- as.numeric(fixed_t0)
  if (!is.finite(fixed_t0)) stop("ps_build_ttv_event(): fixed_t0 must be finite.", call. = FALSE)

  # Normalize splits to character columns
  splits <- splits[, c("patient_id", "split"), drop = FALSE]
  splits$patient_id <- as.character(splits$patient_id)
  splits$split <- as.character(splits$split)

  # Restrict events to known patients in splits
  events <- events[, c("patient_id", "time", "event_type"), drop = FALSE]
  events$patient_id <- as.character(events$patient_id)
  events$event_type <- as.character(events$event_type)
  events$time <- .ps_coerce_time_numeric(events$time)
  .ps_assert_time_numeric(events$time, "ps_build_ttv_event(): events$time")

  if (anyNA(events$event_type) || any(trimws(events$event_type) == "")) {
    stop("ps_build_ttv_event(): events$event_type contains missing/empty values.", call. = FALSE)
  }

  # Compute follow-up start/end if provided
  fu <- .ps_prepare_followup(followup, splits, fu_start_col, fu_end_col, death_col, ctx, "ps_build_ttv_event")

  # Patient universe
  pats <- splits$patient_id

  # Determine t0 per patient
  t0 <- rep(NA_real_, length(pats))
  names(t0) <- pats

  if (t0_strategy == "fixed") {
    t0[] <- fixed_t0
  } else {
    # first observed event time per patient
    # events may not be sorted; compute min time per patient
    first_time <- tapply(events$time, events$patient_id, min)

    if (t0_strategy == "first_event") {
      t0[] <- as.numeric(first_time[pats])
    } else if (t0_strategy == "followup_start") {
      if (!is.null(fu)) {
        t0[] <- fu$fu_start
      } else {
        t0[] <- as.numeric(first_time[pats])
      }
    }
  }

  # Validate t0 exists
  if (anyNA(t0)) {
    bad <- names(t0)[is.na(t0)]
    stop(sprintf("ps_build_ttv_event(): unable to define t0 for %d patient(s) (missing events and/or follow-up). Example(s): %s",
                 length(bad), paste0(head(bad, 10), collapse = ", ")), call. = FALSE)
  }

  # Censoring time
  censor_time <- rep(NA_real_, length(pats))
  names(censor_time) <- pats
  if (!is.null(fu)) {
    censor_time[] <- fu$censor_time
  } else {
    # If no follow-up, censor at max observed event time for patient
    last_time <- tapply(events$time, events$patient_id, max)
    censor_time[] <- as.numeric(last_time[pats])
  }

  if (anyNA(censor_time)) {
    bad <- names(censor_time)[is.na(censor_time)]
    stop(sprintf("ps_build_ttv_event(): unable to define censoring time for %d patient(s). Example(s): %s",
                 length(bad), paste0(head(bad, 10), collapse = ", ")), call. = FALSE)
  }

  # Find next event time after t0 for each patient
  # Approach: filter events to type and time > t0, then take min per patient
  is_target <- events$event_type == event_type
  ev_target <- events[is_target, , drop = FALSE]

  # split by patient (fast enough for phase 2; can optimize later)
  next_time <- rep(NA_real_, length(pats))
  names(next_time) <- pats
  if (nrow(ev_target) > 0) {
    ev_split <- split(ev_target$time, ev_target$patient_id)
    for (pid in intersect(names(ev_split), pats)) {
      tt <- ev_split[[pid]]
      tt <- tt[tt > t0[[pid]]]
      if (length(tt) > 0) next_time[[pid]] <- min(tt)
    }
  }

  # Determine t1 and labels
  event_occurred <- !is.na(next_time) & (next_time <= censor_time)
  t1 <- ifelse(event_occurred, next_time, censor_time)

  # If censor_time < t0, fail loudly (bad follow-up table or time scale)
  if (any(t1 < t0)) {
    bad <- names(t1)[t1 < t0]
    stop(sprintf("ps_build_ttv_event(): found %d patient(s) with t1 < t0 (check follow-up bounds/time scale). Example(s): %s",
                 length(bad), paste0(head(bad, 10), collapse = ", ")), call. = FALSE)
  }

  deltat <- t1 - t0
  time_to_event <- deltat

  out <- data.frame(
    patient_id = pats,
    split = splits$split,
    t0 = as.numeric(t0),
    t1 = as.numeric(t1),
    deltat = as.numeric(deltat),
    event_occurred = as.logical(event_occurred),
    time_to_event = as.numeric(time_to_event),
    censoring_time = as.numeric(censor_time),
    stringsAsFactors = FALSE
  )

  # Attach spec + metadata
  attr(out, "spec") <- list(
    task = "event",
    event_type = event_type,
    t0_strategy = t0_strategy,
    fixed_t0 = fixed_t0,
    followup = !is.null(followup),
    fu_start_col = fu_start_col,
    fu_end_col = fu_end_col,
    death_col = death_col
  )

  meta <- list(
    n_patients = nrow(out),
    split_counts = as.list(table(out$split)),
    event_rate = mean(out$event_occurred),
    built_with = "ps_build_ttv_event"
  )
  attr(out, "metadata") <- meta

  class(out) <- c("ps_ttv_event", class(out))
  out
}

.ps_prepare_followup <- function(followup, splits, fu_start_col, fu_end_col, death_col, ctx, fn_name) {
  if (is.null(followup)) return(NULL)
  .ps_assert_data_frame(followup, "followup")
  cols <- c("patient_id", fu_start_col, fu_end_col)
  .ps_assert_has_cols(followup, cols, "followup")
  if (!is.null(death_col)) .ps_assert_has_cols(followup, c(death_col), "followup")

  fu <- followup[, cols, drop = FALSE]

# Time handling: followup_start/followup_end/death_time may be numeric, Date, or POSIXct.
time_spec <- NULL
if (inherits(fu[[fu_start_col]], "Date") || inherits(fu[[fu_start_col]], "POSIXt") ||
    inherits(fu[[fu_end_col]], "Date") || inherits(fu[[fu_end_col]], "POSIXt") ||
    (!is.null(death_col) && (inherits(followup[[death_col]], "Date") || inherits(followup[[death_col]], "POSIXt")))) {
  time_spec <- .ps_time_spec_or_stop(ctx, fn_name)
}


  fu_time_class_start <- class(fu[[fu_start_col]])[1]
  fu_time_class_end <- class(fu[[fu_end_col]])[1]
  names(fu) <- c("patient_id", "fu_start", "fu_end")
  fu$patient_id <- as.character(fu$patient_id)
  fu$fu_start <- .ps_coerce_time_numeric(fu$fu_start, time_spec, "followup$fu_start")
  if (!fu_time_class_start %in% c("numeric", "integer") || !fu_time_class_end %in% c("numeric", "integer")) {
  }
  fu$fu_end <- .ps_coerce_time_numeric(fu$fu_end, time_spec, "followup$fu_end")

  if (!is.null(death_col)) {
    fu$death_time <- .ps_coerce_time_numeric(followup[[death_col]], time_spec, "followup$death_time")
  } else {
    fu$death_time <- NA_real_
  }

  # Join onto splits order
  idx <- match(splits$patient_id, fu$patient_id)
  if (anyNA(idx)) {
    missing <- splits$patient_id[is.na(idx)]
    stop(sprintf("ps_build_ttv_event(): followup table is missing %d patient(s) from splits. Example(s): %s",
                 length(missing), paste0(head(missing, 10), collapse = ", ")), call. = FALSE)
  }

  fu <- fu[idx, , drop = FALSE]

  # Censoring time: min(fu_end, death_time) when death provided
  censor <- fu$fu_end
  if (any(!is.na(fu$death_time))) {
    censor <- pmin(censor, fu$death_time, na.rm = TRUE)
  }

  if (anyNA(fu$fu_start) || anyNA(fu$fu_end)) {
    stop("ps_build_ttv_event(): followup_start/followup_end contains missing values after coercion.", call. = FALSE)
  }

  out <- data.frame(
    patient_id = fu$patient_id,
    fu_start = fu$fu_start,
    fu_end = fu$fu_end,
    death_time = as.numeric(fu$death_time),
    censor_time = as.numeric(censor),
    stringsAsFactors = FALSE
  )
  out
}