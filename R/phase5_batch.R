build_ttv_batch <- function(specs,
                               splits,
                               ctx = NULL,
                               events = NULL,
                               observations = NULL,
                               followup = NULL,
                               out_dir,
                               format = c("qs", "csv", "parquet", "rds"),
                               overwrite = FALSE,
                               seed = NULL,
                               strict = TRUE,
                               compress = TRUE,
                               manifest_name = "ttv_manifest",
                               chunk = list(method = "none", shuffle = TRUE)) {

  format <- match.arg(format)

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (!is.list(specs) || length(specs) == 0) {
    stop("`specs` must be a non-empty list of spec objects.")
  }

  # Required Suggests
  if (!requireNamespace("digest", quietly = TRUE)) stop("Package 'digest' is required for batch mode (Suggests).")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' is required for metadata writing (Suggests).")

  chunks <- chunk_entities(splits = splits, chunk = chunk, seed = seed)

  manifest_rows <- list()
  spec_id <- 0L

  for (s in specs) {
    .flux_assert_spec(s, "build_ttv_batch()")
    spec_id <- spec_id + 1L
    spec_name <- if (!is.null(s$name)) as.character(s$name) else NA_character_
    task <- if (!is.null(s$task)) as.character(s$task) else NA_character_
    fun <- if (!is.null(s$fun)) as.character(s$fun) else NA_character_
    args <- if (!is.null(s$args)) s$args else list()

    if (!task %in% c("event", "state")) {
      stop(sprintf("Spec %d: `task` must be 'event' or 'state'.", spec_id))
    }
    if (!fun %in% c("build_ttv_event", "build_ttv_state")) {
      stop(sprintf("Spec %d: `fun` must be 'build_ttv_event' or 'build_ttv_state'.", spec_id))
    }
    if (!is.list(args)) stop(sprintf("Spec %d: `args` must be a named list.", spec_id))

    spec_hash <- flux_hash_spec(list(task = task, fun = fun, args = args))

    for (chunk_id in seq_along(chunks)) {
      pat_ids <- chunks[[chunk_id]]

      # Filter shared inputs by chunk
      splits_k <- splits[splits$entity_id %in% pat_ids, , drop = FALSE]
      events_k <- if (!is.null(events)) events[events$entity_id %in% pat_ids, , drop = FALSE] else NULL
      obs_k <- if (!is.null(observations)) observations[observations$entity_id %in% pat_ids, , drop = FALSE] else NULL
      fu_k <- if (!is.null(followup)) followup[followup$entity_id %in% pat_ids, , drop = FALSE] else NULL

      # Merge shared args (per-spec overrides win)
      call_args <- args
      if (is.null(call_args$splits)) call_args$splits <- splits_k
      if (!is.null(ctx) && is.null(call_args$ctx)) call_args$ctx <- ctx

      if (task == "event") {
        if (is.null(events_k) && is.null(call_args$events)) stop("Event task requires `events`.")
        if (is.null(call_args$events)) call_args$events <- events_k
        if (!is.null(fu_k) && is.null(call_args$followup)) call_args$followup <- fu_k
      } else {
        if (is.null(obs_k) && is.null(call_args$observations)) stop("State task requires `observations`.")
        if (is.null(call_args$observations)) call_args$observations <- obs_k
        if (!is.null(fu_k) && is.null(call_args$followup)) call_args$followup <- fu_k
      }

      # Ensure seed is available for any per-spec sampling
      if (!is.null(seed) && is.null(call_args$seed)) call_args$seed <- seed
      # Backward/alternate arg name support (tests/specs may use older names)
      if (task != "event") {
        if (!is.null(call_args$include_provenance) && is.null(call_args$keep_provenance)) {
          call_args$keep_provenance <- isTRUE(call_args$include_provenance)
          call_args$include_provenance <- NULL
        }
      }



      ext <- switch(
        format,
        "qs" = "qs",
        "csv" = if (isTRUE(compress)) "csv.gz" else "csv",
        "parquet" = "parquet",
        "rds" = "rds"
      )

      file_base <- sprintf("%s__%s__chunk%03d", task, spec_hash, chunk_id)
      path_data <- file.path(out_dir, paste0(file_base, ".", ext))
      path_meta <- file.path(out_dir, paste0(file_base, ".metadata.json"))

      status <- "ok"
      err <- NA_character_
      n_rows <- NA_integer_
      n_entities <- NA_integer_

      # Skip existing unless overwrite
      if (!overwrite && (file.exists(path_data) || file.exists(path_meta))) {
        status <- "error"
        err <- "Output exists and overwrite=FALSE."
        if (strict) stop(err)
      } else {
        res <- tryCatch({
          data <- do.call(get(fun, mode = "function"), call_args)

          if (!is.data.frame(data)) stop("Builder did not return a data.frame.")

          n_rows <- nrow(data)
          n_entities <- length(unique(data$entity_id))

          # Write dataset
          flux_write_dataset(data = data, path = path_data, format = format, compress = compress)

          # Metadata
          meta <- list(
            spec_id = spec_id,
            spec_name = spec_name,
            task = task,
            fun = fun,
            spec_hash = spec_hash,
            chunk_id = chunk_id,
            built_at = as.character(Sys.time()),
            package = "fluxPrepare",
            package_version = as.character(utils::packageVersion("fluxPrepare")),
            n_rows = n_rows,
            n_entities = n_entities,
            split_counts = as.list(table(splits_k$split)),
            spec = list(task = task, fun = fun, args = args)
          )
          flux_write_metadata(meta = meta, path = path_meta)

          data
        }, error = function(e) {
          status <<- "error"
          err <<- conditionMessage(e)
          if (strict) stop(e)
          NULL
        })
      }

      manifest_rows[[length(manifest_rows) + 1L]] <- data.frame(
        spec_id = spec_id,
        spec_name = spec_name,
        spec_hash = spec_hash,
        task = task,
        fun = fun,
        chunk_id = chunk_id,
        path_data = path_data,
        path_metadata = path_meta,
        package_version = as.character(utils::packageVersion("fluxPrepare")),
        built_at = as.character(Sys.time()),
        n_rows = n_rows,
        n_entities = n_entities,
        status = status,
        error_message = err,
        stringsAsFactors = FALSE
      )
    }
  }

  manifest <- do.call(rbind, manifest_rows)
  class(manifest) <- c("flux_manifest", class(manifest))

  # Write manifest to disk
  man_csv <- file.path(out_dir, paste0(manifest_name, ".csv"))
  utils::write.csv(manifest, man_csv, row.names = FALSE)

  man_rds <- file.path(out_dir, paste0(manifest_name, ".rds"))
  saveRDS(manifest, man_rds)

  manifest
}

chunk_entities <- function(splits, chunk = list(method = "none", shuffle = TRUE), seed = NULL) {
  if (is.null(splits) || !is.data.frame(splits) || !"entity_id" %in% names(splits)) {
    stop("`splits` must be a data.frame with column `entity_id`.")
  }
  ids <- unique(as.character(splits$entity_id))

  # Guard: ensure chunk parameters produce at least one valid chunk.
  n_ids <- length(ids)
  if (n_ids == 0L) return(list(character(0)))

  method <- if (!is.null(chunk$method)) as.character(chunk$method) else "none"
  shuffle <- if (!is.null(chunk$shuffle)) isTRUE(chunk$shuffle) else TRUE

  if (!is.null(seed)) set.seed(seed)

  if (shuffle && method != "none") {
    ids <- sample(ids, length(ids), replace = FALSE)
  }

  if (method == "none") {
    return(list(ids))
  }

  if (method == "n_chunks") {
    n_chunks <- as.integer(chunk$n_chunks)
    if (is.na(n_chunks) || n_chunks < 1) stop("chunk$n_chunks must be >= 1.")
    if (n_chunks == 1L || length(ids) <= 1L) return(list(ids))
    n_chunks <- min(n_chunks, length(ids))
    # split into n nearly equal chunks
    idx <- split(seq_along(ids), cut(seq_along(ids), breaks = n_chunks, labels = FALSE))
    return(lapply(idx, function(i) ids[i]))
  }

  if (method == "chunk_size") {
    chunk_size <- as.integer(chunk$chunk_size)
    if (is.na(chunk_size) || chunk_size < 1) stop("chunk$chunk_size must be >= 1.")
    out <- list()
    for (i in seq(1, length(ids), by = chunk_size)) {
      out[[length(out) + 1L]] <- ids[i:min(i + chunk_size - 1L, length(ids))]
    }
    return(out)
  }

  if (method == "pct") {
    pct <- as.numeric(chunk$pct)
    if (is.na(pct) || pct <= 0 || pct > 1) stop("chunk$pct must be in (0, 1].")
    chunk_size <- max(1L, as.integer(ceiling(length(ids) * pct)))
    out <- list()
    for (i in seq(1, length(ids), by = chunk_size)) {
      out[[length(out) + 1L]] <- ids[i:min(i + chunk_size - 1L, length(ids))]
    }
    return(out)
  }

  stop("Unknown chunking method. Use 'none', 'n_chunks', 'chunk_size', or 'pct'.")
}

flux_hash_spec <- function(spec) {
  raw <- serialize(spec, NULL, ascii = FALSE, version = 2)
  digest::digest(raw, algo = "sha256")
}

flux_write_metadata <- function(meta, path) {
  txt <- jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(txt, con = path, useBytes = TRUE)
}

flux_write_dataset <- function(data, path, format, compress = TRUE) {
  if (format == "rds") {
    saveRDS(data, path)
    return(invisible(path))
  }
  if (format == "qs") {
    if (!requireNamespace("qs", quietly = TRUE)) stop("Package 'qs' required for format='qs' (Suggests).")
    qs::qsave(data, path, preset = if (compress) "high" else "fast")
    return(invisible(path))
  }
  if (format == "csv") {
    if (isTRUE(compress) || grepl("\\.gz$", path)) {
      con <- gzfile(path, open = "wt")
      on.exit(close(con), add = TRUE)
      utils::write.csv(data, con, row.names = FALSE)
    } else {
      utils::write.csv(data, path, row.names = FALSE)
    }
    return(invisible(path))
  }
  if (format == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) stop("Package 'arrow' required for format='parquet' (Suggests).")
    arrow::write_parquet(data, sink = path)
    return(invisible(path))
  }
  stop("Unknown format.")
}
