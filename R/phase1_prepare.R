prepare_splits <- function(df,
                             id_col = "entity_id",
                             split_col = "split",
                             allowed = c("train", "test", "validation")) {
  .flux_assert_data_frame(df, "df")
  .flux_assert_has_cols(df, c(id_col, split_col), "df")

  out <- df[, c(id_col, split_col)]
  names(out) <- c("entity_id", "split")

  out$entity_id <- as.character(out$entity_id)
  out$split <- tolower(trimws(as.character(out$split)))

  allowed_norm <- tolower(trimws(as.character(allowed)))
  bad <- !(out$split %in% allowed_norm) | is.na(out$split) | out$split == ""
  if (any(bad)) {
    bad_vals <- unique(out$split[bad])
    stop(sprintf("prepare_splits(): invalid split values: %s",
                 paste0(bad_vals, collapse = ", ")), call. = FALSE)
  }

  if (anyNA(out$entity_id) || any(out$entity_id == "")) {
    stop("prepare_splits(): entity_id contains missing/empty values.", call. = FALSE)
  }

  # Enforce uniqueness: one split per entity_id
  dup <- duplicated(out$entity_id)
  if (any(dup)) {
    dups <- unique(out$entity_id[dup])
    stop(sprintf("prepare_splits(): entity_id has multiple rows (must be unique). Example(s): %s",
                 paste0(utils::head(dups, 10), collapse = ", ")), call. = FALSE)
  }

  rownames(out) <- NULL
  class(out) <- c("flux_splits", class(out))
  attr(out, "allowed_splits") <- allowed_norm
  out
}

prepare_events <- function(events,
                             id_col = "entity_id",
                             time_col = "time",
                             type_col = "event_type",
                             table_event_type = NULL,
                             ctx = NULL,
                             sort = TRUE) {
  if (is.data.frame(events)) {
    .flux_assert_has_cols(events, c(id_col, time_col, type_col), "events")
    ev <- events[, c(id_col, time_col, type_col)]
    names(ev) <- c("entity_id", "time", "event_type")
    ev$source_table <- NA_character_
  } else if (is.list(events) && length(events) > 0 && all(vapply(events, is.data.frame, logical(1)))) {
    if (is.null(names(events)) || any(names(events) == "")) {
      stop("prepare_events(): when events is a list, it must be a *named* list.", call. = FALSE)
    }

    ev_list <- vector("list", length(events))
    nm <- names(events)

    # Determine event_type per table
    if (!is.null(table_event_type)) {
      if (is.null(names(table_event_type)) || any(names(table_event_type) == "")) {
        stop("prepare_events(): table_event_type must be a named character vector.", call. = FALSE)
      }
    }

    for (i in seq_along(events)) {
      tbl <- events[[i]]
      .flux_assert_has_cols(tbl, c(id_col, time_col), sprintf("events[['%s']]", nm[[i]]))
      tmp <- tbl[, c(id_col, time_col)]
      names(tmp) <- c("entity_id", "time")

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
    stop("prepare_events(): `events` must be a data.frame or a named list of data.frames.", call. = FALSE)
  }

  ev$entity_id <- as.character(ev$entity_id)
  ev$event_type <- as.character(ev$event_type)

# Time handling
time_class <- class(ev$time)[1]
time_spec <- NULL

if (inherits(ev$time, "Date") || inherits(ev$time, "POSIXt")) {
  time_spec <- .flux_time_spec_or_stop(ctx, "prepare_events")
  ev$time <- fluxCore::time_to_model(ev$time, time_spec)
} else {
  ev$time <- .flux_coerce_time_numeric(ev$time)
}

  if (anyNA(ev$entity_id) || any(ev$entity_id == "")) {
    stop("prepare_events(): entity_id contains missing/empty values.", call. = FALSE)
  }
  if (anyNA(ev$time)) {
    stop("prepare_events(): time contains missing values after coercion.", call. = FALSE)
  }
  if (anyNA(ev$event_type) || any(ev$event_type == "")) {
    stop("prepare_events(): event_type contains missing/empty values.", call. = FALSE)
  }

  if (isTRUE(sort)) {
    ord <- order(ev$entity_id, ev$time, ev$event_type)
    ev <- ev[ord, , drop = FALSE]
    rownames(ev) <- NULL
  }

  class(ev) <- c("flux_events", class(ev))
  attr(ev, "time_class") <- time_class
  ev
}

prepare_observations <- function(tables,
                                   specs,
                                   keep_source = TRUE,
                                   ctx = NULL,
                                   sort = TRUE) {
  if (!(is.list(tables) && length(tables) > 0 && all(vapply(tables, is.data.frame, logical(1))))) {
    stop("prepare_observations(): `tables` must be a named list of data.frames.", call. = FALSE)
  }
  if (is.null(names(tables)) || any(names(tables) == "")) {
    stop("prepare_observations(): `tables` must be a *named* list.", call. = FALSE)
  }
  if (!(is.list(specs) && length(specs) > 0)) {
    stop("prepare_observations(): `specs` must be a named list.", call. = FALSE)
  }
  if (is.null(names(specs)) || any(names(specs) == "")) {
    stop("prepare_observations(): `specs` must be a *named* list keyed by table name.", call. = FALSE)
  }

  tbl_names <- names(tables)
  missing_specs <- setdiff(tbl_names, names(specs))
  if (length(missing_specs) > 0) {
    stop(sprintf("prepare_observations(): missing specs for table(s): %s",
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
      stop(sprintf("prepare_observations(): specs[['%s']] must be a list.", nm), call. = FALSE)
    }
    req <- c("id_col", "time_col", "vars")
    if (!all(req %in% names(sp))) {
      stop(sprintf("prepare_observations(): specs[['%s']] must contain fields: %s",
                   nm, paste0(req, collapse = ", ")), call. = FALSE)
    }

    id_col <- sp$id_col
    time_col <- sp$time_col
    vars <- sp$vars
    group <- if (!is.null(sp$group)) as.character(sp$group) else nm

    if (!is.character(vars) || length(vars) < 1) {
      stop(sprintf("prepare_observations(): specs[['%s']]$vars must be a non-empty character vector.", nm),
           call. = FALSE)
    }

    .flux_assert_has_cols(df, unique(c(id_col, time_col, vars)), sprintf("tables[['%s']]", nm))

    tmp <- df[, unique(c(id_col, time_col, vars))]
    names(tmp)[match(id_col, names(tmp))] <- "entity_id"
    names(tmp)[match(time_col, names(tmp))] <- "time"

    tmp$entity_id <- as.character(tmp$entity_id)
    time_classes[[i]] <- class(tmp$time)[1]
    if (inherits(tmp$time, "Date") || inherits(tmp$time, "POSIXt")) {
      if (is.null(time_spec)) time_spec <- .flux_time_spec_or_stop(ctx, "prepare_observations")
      tmp$time <- fluxCore::time_to_model(tmp$time, time_spec)
    } else {
      tmp$time <- .flux_coerce_time_numeric(tmp$time)
    }


    tmp$group <- as.character(group)
    if (isTRUE(keep_source)) tmp$source_table <- nm

    out_list[[i]] <- tmp
  }

  # Different observation groups may contribute different variable columns.
  # Bind rows using the union of all declared vars, filling missing columns with NA.
  all_vars <- unique(unlist(lapply(specs[tbl_names], function(sp) sp$vars), use.names = FALSE))

  # Establish a stable column order: identifiers first, then declared vars, then optional provenance.
  keep_cols <- c("entity_id", "time", "group", all_vars)
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


  if (anyNA(out$entity_id) || any(out$entity_id == "")) {
    stop("prepare_observations(): entity_id contains missing/empty values.", call. = FALSE)
  }
  if (anyNA(out$time)) {
    stop("prepare_observations(): time contains missing values after coercion.", call. = FALSE)
  }
  if (anyNA(out$group) || any(out$group == "")) {
    stop("prepare_observations(): group contains missing/empty values.", call. = FALSE)
  }

  if (isTRUE(sort)) {
    ord <- order(out$entity_id, out$time, out$group)
    out <- out[ord, , drop = FALSE]
    rownames(out) <- NULL
  }

  class(out) <- c("flux_observations", class(out))
  attr(out, "time_classes") <- stats::setNames(time_classes, tbl_names)
  out
}

.flux_time_spec_or_stop <- function(ctx, fn_name) {
  if (is.null(ctx) || !is.list(ctx)) {
    stop(sprintf("%s(): ctx must be provided and must include ctx$time$unit when time columns are Date/POSIXct.", fn_name),
         call. = FALSE)
  }
  if (is.null(ctx$time) || !is.list(ctx$time) ||
      is.null(ctx$time$unit) || !is.character(ctx$time$unit) ||
      length(ctx$time$unit) != 1L || is.na(ctx$time$unit) || ctx$time$unit == "") {
    stop(sprintf("%s(): ctx$time$unit must be set when time columns are Date/POSIXct.", fn_name), call. = FALSE)
  }
  fluxCore::time_spec(ctx)
}

# ---- internal helpers (not exported) ----

.flux_assert_data_frame <- function(x, name) {
  if (!is.data.frame(x)) stop(sprintf("%s must be a data.frame.", name), call. = FALSE)
}

.flux_assert_has_cols <- function(df, cols, name) {
  miss <- setdiff(cols, names(df))
  if (length(miss) > 0) {
    stop(sprintf("%s is missing required column(s): %s", name, paste0(miss, collapse = ", ")),
         call. = FALSE)
  }
}

.flux_assert_time_numeric <- function(x, context = "time") {
  if (!is.numeric(x)) {
    stop(sprintf("%s must be numeric after coercion.", context), call. = FALSE)
  }
  bad <- is.na(x) | !is.finite(x)
  if (any(bad)) {
    stop(sprintf("%s contains missing/non-finite values after coercion.", context), call. = FALSE)
  }
  invisible(TRUE)
}

.flux_coerce_time_numeric <- function(x, time_spec = NULL, where = "time") {
  # Numeric remains numeric
  if (is.numeric(x)) {
    return(as.numeric(x))
  }

  # Date/POSIXct must be converted via fluxCore time helpers
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    if (is.null(time_spec)) {
      stop(sprintf("%s: calendar times require ctx$time$unit (and optional ctx$time$origin/ctx$time$zone).", where), call. = FALSE)
    }
    return(fluxCore::time_to_model(x, time_spec))
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
