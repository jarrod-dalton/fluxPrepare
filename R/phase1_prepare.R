#' Prepare a patient-level split table
#'
#' Validates and normalizes a patient-level assignment table for train/test/validation splitting.
#' Splits are enforced at the patient level to prevent within-patient leakage.
#'
#' @param df A data.frame containing at least patient id and split assignment.
#' @param id_col Name of the patient id column.
#' @param split_col Name of the split column.
#' @param allowed Character vector of allowed split labels (case-insensitive).
#' @return A data.frame with columns: patient_id, split.
#' @export
ps_prepare_splits <- function(df,
                             id_col = "patient_id",
                             split_col = "split",
                             allowed = c("train", "test", "validation")) {
  .ps_assert_data_frame(df, "df")
  .ps_assert_has_cols(df, c(id_col, split_col), "df")

  out <- df[, c(id_col, split_col)]
  names(out) <- c("patient_id", "split")

  out$patient_id <- as.character(out$patient_id)
  out$split <- tolower(trimws(as.character(out$split)))

  allowed_norm <- tolower(trimws(as.character(allowed)))
  bad <- !(out$split %in% allowed_norm) | is.na(out$split) | out$split == ""
  if (any(bad)) {
    bad_vals <- unique(out$split[bad])
    stop(sprintf("ps_prepare_splits(): invalid split values: %s",
                 paste0(bad_vals, collapse = ", ")), call. = FALSE)
  }

  if (anyNA(out$patient_id) || any(out$patient_id == "")) {
    stop("ps_prepare_splits(): patient_id contains missing/empty values.", call. = FALSE)
  }

  # Enforce uniqueness: one split per patient_id
  dup <- duplicated(out$patient_id)
  if (any(dup)) {
    dups <- unique(out$patient_id[dup])
    stop(sprintf("ps_prepare_splits(): patient_id has multiple rows (must be unique). Example(s): %s",
                 paste0(head(dups, 10), collapse = ", ")), call. = FALSE)
  }

  rownames(out) <- NULL
  class(out) <- c("ps_splits", class(out))
  attr(out, "allowed_splits") <- allowed_norm
  out
}

#' Prepare a canonical event stream
#'
#' Normalizes one or more event tables into a single canonical event stream with enforced columns:
#' patient_id, time, event_type. If multiple tables are provided, their provenance is retained.
#'
#' @param events Either a data.frame of events, or a named list of event data.frames.
#' @param id_col Patient id column name (applied to all tables).
#' @param time_col Time column name (applied to all tables).
#' @param type_col Event type column name for single-table input. Ignored when `events` is a list.
#' @param table_event_type Optional named character vector mapping list element names -> event_type.
#'        If not provided, the list element name is used as the event_type.
#' @param sort Logical; if TRUE, sort events within patient by time (then event_type).
#' @return A data.frame with columns: patient_id, time, event_type, source_table.
#' @export
ps_prepare_events <- function(events,
                             id_col = "patient_id",
                             time_col = "time",
                             type_col = "event_type",
                             table_event_type = NULL,
                             ctx = NULL,
                             sort = TRUE) {
  if (is.data.frame(events)) {
    .ps_assert_has_cols(events, c(id_col, time_col, type_col), "events")
    ev <- events[, c(id_col, time_col, type_col)]
    names(ev) <- c("patient_id", "time", "event_type")
    ev$source_table <- NA_character_
  } else if (is.list(events) && length(events) > 0 && all(vapply(events, is.data.frame, logical(1)))) {
    if (is.null(names(events)) || any(names(events) == "")) {
      stop("ps_prepare_events(): when events is a list, it must be a *named* list.", call. = FALSE)
    }

    ev_list <- vector("list", length(events))
    nm <- names(events)

    # Determine event_type per table
    if (!is.null(table_event_type)) {
      if (is.null(names(table_event_type)) || any(names(table_event_type) == "")) {
        stop("ps_prepare_events(): table_event_type must be a named character vector.", call. = FALSE)
      }
    }

    for (i in seq_along(events)) {
      tbl <- events[[i]]
      .ps_assert_has_cols(tbl, c(id_col, time_col), sprintf("events[['%s']]", nm[[i]]))
      tmp <- tbl[, c(id_col, time_col)]
      names(tmp) <- c("patient_id", "time")

      et <- nm[[i]]
      if (!is.null(table_event_type) && et %in% names(table_event_type)) {
        et <- table_event_type[[et]]
      }
      tmp$event_type <- as.character(et)
      tmp$source_table <- nm[[i]]
      ev_list[[i]] <- tmp
    }

    ev <- do.call(rbind, ev_list)
  } else {
    stop("ps_prepare_events(): `events` must be a data.frame or a named list of data.frames.", call. = FALSE)
  }

  ev$patient_id <- as.character(ev$patient_id)
  ev$event_type <- as.character(ev$event_type)

# Time handling
time_class <- class(ev$time)[1]
time_spec <- NULL

if (inherits(ev$time, "Date") || inherits(ev$time, "POSIXt")) {
  time_spec <- .ps_time_spec_or_stop(ctx, "ps_prepare_events")
  ev$time <- patientSimCore::ps_time_to_model(ev$time, time_spec)
} else {
  ev$time <- .ps_coerce_time_numeric(ev$time)
}

  if (anyNA(ev$patient_id) || any(ev$patient_id == "")) {
    stop("ps_prepare_events(): patient_id contains missing/empty values.", call. = FALSE)
  }
  if (anyNA(ev$time)) {
    stop("ps_prepare_events(): time contains missing values after coercion.", call. = FALSE)
  }
  if (anyNA(ev$event_type) || any(ev$event_type == "")) {
    stop("ps_prepare_events(): event_type contains missing/empty values.", call. = FALSE)
  }

  if (isTRUE(sort)) {
    ord <- order(ev$patient_id, ev$time, ev$event_type)
    ev <- ev[ord, , drop = FALSE]
    rownames(ev) <- NULL
  }

  class(ev) <- c("ps_events", class(ev))
  attr(ev, "time_class") <- time_class
  ev
}

#' Prepare canonical observation tables
#'
#' Normalizes a list of "wide-ish" observational tables into a single canonical observation store.
#' Each table is assumed to represent a measurement group (e.g., bp, bmp) observed at a time.
#'
#' @param tables Named list of data.frames.
#' @param specs Named list describing, for each table, the id column, time column, and measured variables.
#'        Each element must be a list with fields: id_col, time_col, vars, group (optional; defaults to table name).
#' @param keep_source Logical; if TRUE, include source_table column.
#' @param sort Logical; if TRUE, sort observations within patient by time (then group).
#' @return A data.frame with at least: patient_id, time, group, (vars...), source_table (optional).
#' @export
ps_prepare_observations <- function(tables,
                                   specs,
                                   keep_source = TRUE,
                                   ctx = NULL,
                                   sort = TRUE) {
  if (!(is.list(tables) && length(tables) > 0 && all(vapply(tables, is.data.frame, logical(1))))) {
    stop("ps_prepare_observations(): `tables` must be a named list of data.frames.", call. = FALSE)
  }
  if (is.null(names(tables)) || any(names(tables) == "")) {
    stop("ps_prepare_observations(): `tables` must be a *named* list.", call. = FALSE)
  }
  if (!(is.list(specs) && length(specs) > 0)) {
    stop("ps_prepare_observations(): `specs` must be a named list.", call. = FALSE)
  }
  if (is.null(names(specs)) || any(names(specs) == "")) {
    stop("ps_prepare_observations(): `specs` must be a *named* list keyed by table name.", call. = FALSE)
  }

  tbl_names <- names(tables)
  missing_specs <- setdiff(tbl_names, names(specs))
  if (length(missing_specs) > 0) {
    stop(sprintf("ps_prepare_observations(): missing specs for table(s): %s",
                 paste0(missing_specs, collapse = ", ")), call. = FALSE)
  }

  out_list <- vector("list", length(tables))
  time_classes <- character(length(tables))
  time_spec <- NULL

  for (i in seq_along(tables)) {
    nm <- tbl_names[[i]]
    df <- tables[[i]]
    sp <- specs[[nm]]

    if (!is.list(sp)) {
      stop(sprintf("ps_prepare_observations(): specs[['%s']] must be a list.", nm), call. = FALSE)
    }
    req <- c("id_col", "time_col", "vars")
    if (!all(req %in% names(sp))) {
      stop(sprintf("ps_prepare_observations(): specs[['%s']] must contain fields: %s",
                   nm, paste0(req, collapse = ", ")), call. = FALSE)
    }

    id_col <- sp$id_col
    time_col <- sp$time_col
    vars <- sp$vars
    group <- if (!is.null(sp$group)) as.character(sp$group) else nm

    if (!is.character(vars) || length(vars) < 1) {
      stop(sprintf("ps_prepare_observations(): specs[['%s']]$vars must be a non-empty character vector.", nm),
           call. = FALSE)
    }

    .ps_assert_has_cols(df, unique(c(id_col, time_col, vars)), sprintf("tables[['%s']]", nm))

    tmp <- df[, unique(c(id_col, time_col, vars))]
    names(tmp)[match(id_col, names(tmp))] <- "patient_id"
    names(tmp)[match(time_col, names(tmp))] <- "time"

    tmp$patient_id <- as.character(tmp$patient_id)
    time_classes[[i]] <- class(tmp$time)[1]
    if (inherits(tmp$time, "Date") || inherits(tmp$time, "POSIXt")) {
      if (is.null(time_spec)) time_spec <- .ps_time_spec_or_stop(ctx, "ps_prepare_observations")
      tmp$time <- patientSimCore::ps_time_to_model(tmp$time, time_spec)
    } else {
      tmp$time <- .ps_coerce_time_numeric(tmp$time)
    }


    tmp$group <- as.character(group)
    if (isTRUE(keep_source)) tmp$source_table <- nm

    out_list[[i]] <- tmp
  }

  # Different observation groups may contribute different variable columns.
  # Bind rows using the union of all declared vars, filling missing columns with NA.
  all_vars <- unique(unlist(lapply(specs[tbl_names], function(sp) sp$vars), use.names = FALSE))

  # Establish a stable column order: identifiers first, then declared vars, then optional provenance.
  keep_cols <- c("patient_id", "time", "group", all_vars)
  if (isTRUE(keep_source)) keep_cols <- c(keep_cols, "source_table")

  # Add missing columns to each table and reorder consistently before binding.
  out_list <- lapply(out_list, function(d) {
    miss <- setdiff(keep_cols, names(d))
    if (length(miss) > 0) {
      for (m in miss) d[[m]] <- NA
    }
    d <- d[, keep_cols, drop = FALSE]
    d
  })

  out <- do.call(rbind, out_list)


  if (anyNA(out$patient_id) || any(out$patient_id == "")) {
    stop("ps_prepare_observations(): patient_id contains missing/empty values.", call. = FALSE)
  }
  if (anyNA(out$time)) {
    stop("ps_prepare_observations(): time contains missing values after coercion.", call. = FALSE)
  }
  if (anyNA(out$group) || any(out$group == "")) {
    stop("ps_prepare_observations(): group contains missing/empty values.", call. = FALSE)
  }

  if (isTRUE(sort)) {
    ord <- order(out$patient_id, out$time, out$group)
    out <- out[ord, , drop = FALSE]
    rownames(out) <- NULL
  }

  class(out) <- c("ps_observations", class(out))
  attr(out, "time_classes") <- stats::setNames(time_classes, tbl_names)
  out
}

.ps_time_spec_or_stop <- function(ctx, fn_name) {
  if (is.null(ctx) || !is.list(ctx)) {
    stop(sprintf("%s(): ctx must be provided and must include ctx$time$unit when time columns are Date/POSIXct.", fn_name),
         call. = FALSE)
  }
  if (is.null(ctx$time) || !is.list(ctx$time) ||
      is.null(ctx$time$unit) || !is.character(ctx$time$unit) ||
      length(ctx$time$unit) != 1L || is.na(ctx$time$unit) || ctx$time$unit == "") {
    stop(sprintf("%s(): ctx$time$unit must be set when time columns are Date/POSIXct.", fn_name), call. = FALSE)
  }
  patientSimCore::ps_time_spec(ctx)
}

# ---- internal helpers (not exported) ----

.ps_assert_data_frame <- function(x, name) {
  if (!is.data.frame(x)) stop(sprintf("%s must be a data.frame.", name), call. = FALSE)
}

.ps_assert_has_cols <- function(df, cols, name) {
  miss <- setdiff(cols, names(df))
  if (length(miss) > 0) {
    stop(sprintf("%s is missing required column(s): %s", name, paste0(miss, collapse = ", ")),
         call. = FALSE)
  }
}

.ps_assert_time_numeric <- function(x, context = "time") {
  if (!is.numeric(x)) {
    stop(sprintf("%s must be numeric after coercion.", context), call. = FALSE)
  }
  bad <- is.na(x) | !is.finite(x)
  if (any(bad)) {
    stop(sprintf("%s contains missing/non-finite values after coercion.", context), call. = FALSE)
  }
  invisible(TRUE)
}

.ps_coerce_time_numeric <- function(x, time_spec = NULL, where = "time") {
  # Numeric remains numeric
  if (is.numeric(x)) {
    return(as.numeric(x))
  }

  # Date/POSIXct must be converted via patientSimCore time helpers
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    if (is.null(time_spec)) {
      stop(sprintf("%s: calendar times require ctx$time$unit (and optional ctx$time$origin/ctx$time$zone).", where), call. = FALSE)
    }
    return(patientSimCore::ps_time_to_model(x, time_spec))
  }

  # Disallow character/factor time inputs to avoid silent parsing surprises.
  if (is.character(x) || is.factor(x)) {
    stop(sprintf(
      "%s: time columns must be numeric model time, Date, or POSIXct/POSIXt (date+time). Time-only or character times are not supported.",
      where
    ), call. = FALSE)
  }

  # Time-only classes (difftime, hms, etc.) are not supported.
  if (inherits(x, "difftime")) {
    stop(sprintf("%s: time-only values (difftime/hms) are not supported. Provide Date or POSIXct (date+time).", where), call. = FALSE)
  }

  stop(sprintf("%s: unsupported time column class: %s.", where, paste(class(x), collapse = "/")), call. = FALSE)
}
