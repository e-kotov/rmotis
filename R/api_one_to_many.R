
#' Calculate one-to-many or many-to-one street-level routes
#'
#' This function computes travel time and distance from origin(s) to
#' multiple destinations (or vice versa). It supports both simple single-request
#' execution and robust parallel/batch execution for large datasets using either
#' the MOTIS API or the MOTIS CLI batch engine.
#'
#' @param one Origin(s). Can be a data frame/tibble with coordinate columns,
#'   an `sf` object, or a vector/matrix of coordinates.
#' @param many Destination(s). Same format as `one`.
#' @param one_id_col The name of the column in `one` to use for identifying
#'   the point in the output. Defaults to `"id"`.
#' @param many_id_col The name of the column in `many` to use for identifying
#'   the point in the output. Defaults to `"id"`.
#' @param mode The routing profile to use. Defaults to `"WALK"`.
#' @param arrive_by Logical. If `FALSE` (the default), calculates routes from
#'   `one` to `many`. If `TRUE`, calculates routes from `many` to `one`.
#' @param max maximum travel time in seconds
#' @param maxMatchingDistance maximum matching distance in meters
#' @param withDistance Logical. Include distance in the output? 
#'   Defaults to `FALSE`.
#' @param ... Additional MOTIS API parameters.
#' @param engine Execution engine:
#'   - `"api"` (default): Uses the MOTIS POST API. Supports parallel backends.
#'   - `"batch"`: Uses the MOTIS CLI batch command. **Recommended for very large
#'     datasets** (millions of routes). Requires `data_dir`.
#' @param output The desired output format. One of:
#'   - `"data.frame"` (default): A tidy data frame.
#'   - `"raw_list"`: The raw parsed JSON response (only for `engine='api'` and non-parallel execution).
#' @param parallel Logical. Enable parallel processing for the API engine? 
#'   Defaults to `TRUE`.
#' @param backend Parallel backend for API engine: `"auto"`, `"httr2"`, or `"mirai"`.
#' @param batch_size Number of origins to process per batch/request.
#' @param max_destinations_per_batch Optional limit to split destinations into 
#'   multiple requests to avoid memory or timeout issues.
#' @param output_path Optional path to write results incrementally (`.csv`, 
#'   `.duckdb`, or **Directory** of `.parquet` files).
#' @param checkpoint_file Optional path for checkpointing progress (API engine only).
#' @param progress Logical. Display progress bar/messages?
#' @param spatial_filter_km Numeric. Optional straight-line distance threshold
#'   (in kilometers). If provided, destinations further than this distance from
#'   an origin will be excluded from the MOTIS request for that origin. This
#'   is highly recommended for very large destination sets to reduce server
#'   load and network traffic.
#' @param data_dir Path to MOTIS data directory. Required if `engine='batch'`.
#' @param temp_dir Directory for temporary batch files. Defaults to `tempdir()`.
#' @param keep_files Logical. Keep temporary files? Defaults to `FALSE`.
#' @param eol Optional line ending for batch query files (e.g., `"\n"` for LF or
#'   `"\r\n"` for CRLF). If provided, forces this line ending even on Windows.
#'   Typically, `"\n"` is required for MOTIS batch processing.
#' @param motis_path Path to the directory containing the MOTIS binary, or
#'   `NULL` to use the system PATH.
#' @inheritDotParams motis.client::mc_oneToMany -one -many -mode -arriveBy -max -maxMatchingDistance -withDistance -.endpoint
#'
#' @return Depending on the `output` parameter and `output_path`, a `data.frame`, 
#'   a list, or the `output_path` invisibly.
#' @export
#' @importFrom httr2 req_perform resp_body_json
#' @importFrom dplyr bind_rows
#' @importFrom rlang check_installed is_installed
#' @import lifecycle
motis_one_to_many <- function(
  one,
  many,
  one_id_col = "id",
  many_id_col = "id",
  mode = c("WALK", "BIKE", "CAR"),
  arrive_by = FALSE,
  max = 7200, # 2 hours in seconds
  maxMatchingDistance = 1000, # 1 km
  withDistance = FALSE,
  spatial_filter_km = NULL,
  ...,
  spatial_filter = NULL,
  max_speed_kmh = NULL,
  engine = c("api", "batch"),
  output = c("data.frame", "raw_list"),
  parallel = TRUE,
  backend = c("auto", "httr2", "mirai"),
  batch_size = NULL,
  max_destinations_per_batch = NULL,
  output_path = NULL,
  checkpoint_file = NULL,
  progress = TRUE,
  data_dir = NULL,
  temp_dir = tempdir(),
  keep_files = FALSE,
  eol = NULL,
  motis_path = NULL
)  {
  # --- 1. Argument and Input Validation ---
  engine <- match.arg(engine)
  output <- match.arg(output)
  mode <- match.arg(mode)
  backend <- match.arg(backend)
  
  .motis_one_to_many_infra(
    one = one,
    many = many,
    one_id_col = one_id_col,
    many_id_col = many_id_col,
    mode = mode,
    arrive_by = arrive_by,
    duration_val = max,
    maxMatchingDistance = maxMatchingDistance,
    withDistance = withDistance,
    spatial_filter_km = spatial_filter_km,
    ...,
    spatial_filter = spatial_filter,
    max_speed_kmh = max_speed_kmh,
    engine = engine,
    output = output,
    parallel = parallel,
    backend = backend,
    batch_size = batch_size,
    max_destinations_per_batch = max_destinations_per_batch,
    output_path = output_path,
    checkpoint_file = checkpoint_file,
    progress = progress,
    data_dir = data_dir,
    temp_dir = temp_dir,
    keep_files = keep_files,
    eol = eol,
    motis_path = motis_path,
    api_endpoint = "/api/v1/one-to-many",
    duration_key = "max"
  )
}

#' Internal: Infrastructure for One-to-Many Routing
#' @noRd
.motis_one_to_many_infra <- function(
  one,
  many,
  one_id_col = "id",
  many_id_col = "id",
  mode = "WALK",
  arrive_by = FALSE,
  duration_val = 7200,
  maxMatchingDistance = 1000,
  withDistance = FALSE,
  spatial_filter_km = NULL,
  ...,
  spatial_filter = NULL,
  max_speed_kmh = NULL,
  engine = c("api", "batch"),
  output = c("data.frame", "raw_list"),
  parallel = TRUE,
  backend = c("auto", "httr2", "mirai"),
  batch_size = NULL,
  max_destinations_per_batch = NULL,
  output_path = NULL,
  checkpoint_file = NULL,
  progress = TRUE,
  data_dir = NULL,
  temp_dir = tempdir(),
  keep_files = FALSE,
  eol = NULL,
  motis_path = NULL,
  api_endpoint = "/api/v1/one-to-many",
  duration_key = "max",
  client_fun = motis.client::mc_oneToMany
) {
  # --- 1. Argument and Input Validation ---
  if (!is.null(spatial_filter)) {
    stop("The 'spatial_filter' (logical) argument is deprecated and has been removed. Please use 'spatial_filter_km' (numeric) instead.", call. = FALSE)
  }
  if (!is.null(max_speed_kmh)) {
    stop("The 'max_speed_kmh' argument is deprecated and has been removed. Please use 'spatial_filter_km' instead.", call. = FALSE)
  }

  engine <- if (is.character(engine)) match.arg(engine) else engine
  output <- if (is.character(output)) match.arg(output) else output
  backend <- if (is.character(backend)) match.arg(backend) else backend
  
  # Format inputs to "lat;lon" strings
  one_places <- .format_place_onemany(one, id_col = one_id_col)
  many_places_vec <- .format_place_onemany(many, id_col = many_id_col)
  
  # Check for deprecated arguments in dots (legacy fallback)
  dots <- list(...)
  if (!is.null(dots$spatial_filter)) {
    stop("The 'spatial_filter' (logical) argument is deprecated and has been removed. Please use 'spatial_filter_km' (numeric) instead.", call. = FALSE)
  }
  if (!is.null(dots$max_speed_kmh)) {
    stop("The 'max_speed_kmh' argument is deprecated and has been removed. Please use 'spatial_filter_km' instead.", call. = FALSE)
  }
  
  if (engine == "batch") {
    if (is.null(data_dir)) stop("'data_dir' is required for engine='batch'", call. = FALSE)
    
    # Remove these from dots as they are passed explicitly
    dots$api_endpoint <- NULL
    dots$duration_key <- NULL
    
    return(.motis_one_to_many_batch_cli(
      one = one, many = many, data_dir = data_dir, mode = mode,
      arrive_by = arrive_by, max = duration_val, maxMatchingDistance = maxMatchingDistance,
      one_id_col = one_id_col, many_id_col = many_id_col,
      withDistance = withDistance, ..., 
      temp_dir = temp_dir, keep_files = keep_files, progress = progress,
      batch_size = batch_size, max_destinations_per_batch = max_destinations_per_batch,
      output_path = output_path, eol = eol, motis_path = motis_path,
      api_endpoint = api_endpoint,
      duration_key = duration_key,
      client_fun = client_fun
    ))
  }

  # API Engine logic
  
  # Simple path = 1 origin AND no parallel requested AND no file streaming AND no checkpointing
  # AND no destination splitting requested AND no spatial filter requested
  is_simple <- !parallel && length(one_places) == 1 && is.null(output_path) && 
               is.null(checkpoint_file) && is.null(max_destinations_per_batch) &&
               is.null(spatial_filter_km)
  
  if (is_simple) {
    return(.motis_one_to_many_simple(
      one_place = one_places,
      many_places_vec = many_places_vec,
      one = one, many = many, # pass for ID extraction
      one_id_col = one_id_col, many_id_col = many_id_col,
      mode = mode, arrive_by = arrive_by,
      max = duration_val, maxMatchingDistance = maxMatchingDistance,
      withDistance = withDistance,
      output = output,
      ...,
      api_endpoint = api_endpoint,
      duration_key = duration_key
    ))
  } else {
    if (output == "raw_list") {
      warning("output='raw_list' is not supported in robust mode. returning data.frame.", call. = FALSE)
    }
    
    return(.motis_one_to_many_calc(
      one_places = one_places,
      many_places_vec = many_places_vec,
      one = one, 
      many = many,
      one_id_col = one_id_col, 
      many_id_col = many_id_col,
      mode = mode,
      arrive_by = arrive_by,
      max = duration_val,
      maxMatchingDistance = maxMatchingDistance,
      withDistance = withDistance,
      dots = dots, # Use already captured dots
      backend = backend,
      batch_size = batch_size %||% 16L,
      output_path = output_path,
      checkpoint_file = checkpoint_file,
      progress = progress,
      parallel = parallel,
      api_endpoint = api_endpoint,
      duration_key = duration_key,
      spatial_filter_km = spatial_filter_km
    ))
  }
}

#' Run Full One-to-Many Batch Routing Cycle via CLI
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' This function is now a wrapper around [motis_one_to_many()] with `engine = "batch"`.
#' 
#' @inheritParams motis_one_to_many
#' @param motis_path Path to the directory containing the MOTIS binary, or
#'   `NULL` to use the system PATH.
#' @param echo Logical. If `TRUE` (default), echo MOTIS batch output
#'   (timing statistics) to the console.
#' @param output_dir Directory where to save the temporary batch files. 
#'   Mapped to `temp_dir` in the new interface.
#' @param spatial_filter_km Numeric. Optional straight-line distance threshold.
#' @param spatial_sort Logical. Sort origins spatially.
#' @param split Integer. Mapped to `max_destinations_per_batch` if > 1.
#' @param chunk_size Number of lines to read and process at a time. Defaults to `10000L`.
#' @param output_callback Optional function that receives each processed chunk
#'   (a data.frame) as its argument.
#'
#' @return A data.frame or `output_path` invisibly.
#' @export
motis_one_to_many_batch <- function(
  one,
  many,
  data_dir,
  mode = c("WALK", "BIKE", "CAR"),
  arrive_by = FALSE,
  max = 7200,
  maxMatchingDistance = 1000,
  one_id_col = "id",
  many_id_col = "id",
  withDistance = FALSE,
  ...,
  motis_path = NULL,
  chunk_size = 10000L,
  output_callback = NULL,
  echo = TRUE,
  output_dir = tempdir(),
  keep_files = FALSE,
  spatial_filter_km = NULL,
  spatial_sort = TRUE,
  split = 1L,
  eol = NULL
) {
  lifecycle::deprecate_warn("0.2.0", "motis_one_to_many_batch()", "motis_one_to_many(engine = 'batch')")
  
  max_dest <- if (split > 1) ceiling(nrow(as.data.frame(many)) / split) else NULL
  
  motis_one_to_many(
    one = one, many = many, data_dir = data_dir, mode = mode,
    arrive_by = arrive_by, max = max, maxMatchingDistance = maxMatchingDistance,
    one_id_col = one_id_col, many_id_col = many_id_col,
    withDistance = withDistance, ...,
    engine = "batch", motis_path = motis_path, chunk_size = chunk_size,
    output_callback = output_callback, echo = echo, temp_dir = output_dir,
    keep_files = keep_files, spatial_filter_km = spatial_filter_km,
    spatial_sort = spatial_sort, max_destinations_per_batch = max_dest,
    eol = eol
  )
}

#' Generate MOTIS Batch Query File for One-to-Many
#'
#' Efficiently constructs a text file of MOTIS one-to-many street routing
#' queries for batch processing. Also writes a metadata sidecar file
#' (`{output_file}.meta`) that records the origin and destination IDs for each
#' query line, enabling reliable reconstruction of results via
#' [motis_one_to_many_read_batch()].
#'
#' @param one The single origin (when `arrive_by = FALSE`) or destination
#'   (when `arrive_by = TRUE`).
#' @param many The multiple destinations (when `arrive_by = FALSE`) or origins
#'   (when `arrive_by = TRUE`).
#' @param output_file The path to the output text file.
#' @param mode The routing profile to use (WALK, BIKE, CAR).
#' @param arrive_by Logical. If `FALSE` (the default), calculates routes from
#'   `one` to `many`. If `TRUE`, calculates routes from `many` to `one`.
#' @param max maximum travel time in seconds
#' @param maxMatchingDistance maximum matching distance in meters
#' @param withDistance Logical. Include distance in the query? 
#'   Defaults to `FALSE`.
#' @param one_id_col The name of the column in `one` to use as the origin
#'   identifier in the metadata file. Defaults to `"id"`. Falls back to
#'   sequential row numbers if the column is not found.
#' @param many_id_col The name of the column in `many` to use as the
#'   destination identifiers in the metadata file. Defaults to `"id"`.
#' @param ... Additional MOTIS API parameters.
#' @param append Logical. If `TRUE`, appends to `output_file` and its
#'   `.meta` sidecar.
#' @param quiet Logical. If `TRUE`, suppress status messages.
#' @param api_endpoint The API path. Defaults to `"/api/v1/one-to-many"`.
#'
#' @return Invisibly returns the number of queries written (always 1).
#' @export
motis_one_to_many_generate_batch <- function(
  one,
  many,
  output_file,
  mode = c("WALK", "BIKE", "CAR"),
  arrive_by = FALSE,
  max = 7200,
  maxMatchingDistance = 1000,
  withDistance = FALSE,
  one_id_col = "id",
  many_id_col = "id",
  ...,
  append = FALSE,
  api_endpoint = "/api/v1/one-to-many", quiet = FALSE
) {
  if (missing(output_file) || !is.character(output_file) || length(output_file) != 1) {
    stop("`output_file` must be a single string specifying the file path.", call. = FALSE)
  }

  mode <- match.arg(mode)
  dots <- .collapse_dots(list(...))

  one_place <- .format_place_onemany(one)
  if (length(one_place) != 1) stop("'one' must be a single location", call. = FALSE)

  many_places_vec <- .format_place_onemany(many)
  many_places_str <- paste(many_places_vec, collapse = ",")

  # Validation via motis.client
  tryCatch({
    .validate_batch_params(dots)
    do.call(motis.client::mc_oneToMany, c(
      list(
        one = one_place,
        many = many_places_str,
        mode = mode,
        arriveBy = arrive_by,
        max = max,
        maxMatchingDistance = maxMatchingDistance,
        withDistance = withDistance,
        .build_only = TRUE,
        .server = "http://localhost:8080"
      ),
      dots
    ))
  }, error = function(e) {
    stop("Invalid MOTIS API parameters: ", e$message, call. = FALSE)
  })

  query_str <- .build_one_to_many_query(
    one_place = one_place,
    many_places_str = many_places_str,
    mode = mode,
    arrive_by = arrive_by,
    max = max,
    maxMatchingDistance = maxMatchingDistance,
    withDistance = withDistance,
    dots = dots,
    api_endpoint = api_endpoint
  )

  # Write query line
  con <- file(output_file, open = if (isTRUE(append)) "a" else "w")
  on.exit(close(con))
  writeLines(query_str, con = con)

  # Write metadata sidecar (tab-separated: one_id, many_id_1, many_id_2, ...)
  one_id <- .get_ids(one, id_col = one_id_col)
  many_ids <- .get_ids(many, id_col = many_id_col)
  meta_line <- paste(c(one_id, many_ids), collapse = "\t")

  meta_file <- paste0(output_file, ".meta")
  meta_con <- file(meta_file, open = if (isTRUE(append)) "a" else "w")
  on.exit(close(meta_con), add = TRUE)
  writeLines(meta_line, con = meta_con)

  if (!quiet) message("Successfully wrote one-to-many batch query to '", output_file, "'.")
  invisible(1L)
}

#' Read MOTIS Batch Response File for One-to-Many
#'
#' Parses a MOTIS batch response file and maps results back to origin and
#' destination IDs using a metadata sidecar file. Processes the files in chunks
#' for memory efficiency with large files.
#'
#' @param response_file Path to the MOTIS batch response file (one JSON array
#'   per line).
#' @param metadata_file Path to the metadata sidecar file generated by
#'   [motis_one_to_many_generate_batch()] (the `.meta` file).
#' @param arrive_by Logical. If `FALSE` (default), the `one` in each query was
#'   the origin. If `TRUE`, the `one` was the destination.
#' @param chunk_size Number of lines to read and process at a time. Larger
#'   values use more memory but may be faster. Defaults to `10000L`.
#' @param output_callback Optional function that receives each processed chunk
#'   (a data.frame) as its argument. When provided, chunks are streamed to the
#'   callback instead of accumulated in memory, and the function returns
#'   `invisible(NULL)`. Useful for writing results directly to Arrow, DuckDB,
#'   or CSV.
#'
#' @return A data.frame with columns `from_id`, `to_id`, `duration_s`, and
#'   optionally `distance_m`. Returns `invisible(NULL)` when `output_callback`
#'   is used.
#' @export
#' @importFrom RcppSimdJson fparse
#' @importFrom dplyr bind_rows
motis_one_to_many_read_batch <- function(
  response_file,
  metadata_file,
  arrive_by = FALSE,
  chunk_size = 10000L,
  output_callback = NULL
) {
  stopifnot(
    is.character(response_file), length(response_file) == 1, file.exists(response_file),
    is.character(metadata_file), length(metadata_file) == 1, file.exists(metadata_file),
    is.logical(arrive_by), length(arrive_by) == 1,
    is.numeric(chunk_size), length(chunk_size) == 1, chunk_size >= 1
  )
  if (!is.null(output_callback)) {
    stopifnot(is.function(output_callback))
  }

  use_callback <- !is.null(output_callback)
  chunk_size <- as.integer(chunk_size)

  resp_con <- file(response_file, open = "r")
  on.exit(close(resp_con), add = TRUE)
  meta_con <- file(metadata_file, open = "r")
  on.exit(close(meta_con), add = TRUE)

  result_chunks <- if (!use_callback) list() else NULL
  chunk_idx <- 0L

  repeat {
    resp_lines <- readLines(resp_con, n = chunk_size, warn = FALSE)
    meta_lines <- readLines(meta_con, n = chunk_size, warn = FALSE)

    if (length(resp_lines) == 0L) break

    if (length(resp_lines) != length(meta_lines)) {
      stop(
        "Response file and metadata file have different numbers of lines.",
        call. = FALSE
      )
    }

    # Parse metadata: first field = one_id, rest = many_ids
    meta_split <- strsplit(meta_lines, "\t", fixed = TRUE)

    # Process each line in the chunk
    line_results <- vector("list", length(resp_lines))
    for (i in seq_along(resp_lines)) {
      one_id <- meta_split[[i]][1L]
      many_ids <- meta_split[[i]][-1L]
      n_many <- length(many_ids)

      parsed <- tryCatch(
        RcppSimdJson::fparse(resp_lines[i]),
        error = function(e) NULL
      )

      duration <- rep(NA_real_, n_many)
      distance <- rep(NA_real_, n_many)
      has_distance <- FALSE

      if (is.data.frame(parsed) && nrow(parsed) > 0L) {
        # fparse returned a data.frame (all elements had same structure)
        if ("duration" %in% names(parsed)) {
          duration[seq_len(nrow(parsed))] <- as.numeric(parsed$duration)
        }
        if ("distance" %in% names(parsed)) {
          distance[seq_len(nrow(parsed))] <- as.numeric(parsed$distance)
          has_distance <- TRUE
        }
      } else if (is.list(parsed)) {
        # fparse returned a list (mixed empty/non-empty objects)
        for (j in seq_along(parsed)) {
          item <- parsed[[j]]
          if (length(item) > 0L) {
            if (!is.null(item$duration)) duration[j] <- as.numeric(item$duration)
            if (!is.null(item$distance)) {
              distance[j] <- as.numeric(item$distance)
              has_distance <- TRUE
            }
          }
        }
      }

      df <- data.frame(
        from_id = rep(one_id, n_many),
        to_id = many_ids,
        duration_s = duration,
        stringsAsFactors = FALSE
      )
      if (has_distance) {
        df$distance_m <- distance
      }

      line_results[[i]] <- df
    }

    chunk_df <- dplyr::bind_rows(line_results)

    # Apply arrive_by column swap
    if (arrive_by && nrow(chunk_df) > 0L) {
      names(chunk_df)[names(chunk_df) == "from_id"] <- ".tmp_from"
      names(chunk_df)[names(chunk_df) == "to_id"] <- "from_id"
      names(chunk_df)[names(chunk_df) == ".tmp_from"] <- "to_id"
    }

    if (use_callback) {
      output_callback(chunk_df)
    } else {
      chunk_idx <- chunk_idx + 1L
      result_chunks[[chunk_idx]] <- chunk_df
    }
  }

  if (use_callback) {
    return(invisible(NULL))
  }

  dplyr::bind_rows(result_chunks)
}

#' Internal: Run Full One-to-Many Batch Routing Cycle via CLI
#' @noRd
.motis_one_to_many_batch_cli <- function(
  one,
  many,
  data_dir,
  mode = c("WALK", "BIKE", "CAR", "TRANSIT"),
  arrive_by = FALSE,
  max = 7200,
  maxMatchingDistance = 1000,
  one_id_col = "id",
  many_id_col = "id",
  withDistance = FALSE,
  ...,
  motis_path = NULL,
  chunk_size = 10000L,
  output_callback = NULL,
  echo = TRUE,
  progress = TRUE,
  temp_dir = tempdir(),
  keep_files = FALSE,
  spatial_filter_km = NULL,
  spatial_sort = TRUE,
  batch_size = NULL,
  max_destinations_per_batch = NULL,
  output_path = NULL,
  eol = NULL,
  api_endpoint = "/api/v1/one-to-many",
  duration_key = "max",
  client_fun = motis.client::mc_oneToMany
) {
  mode <- match.arg(mode)
  data_dir <- normalizePath(data_dir, mustWork = TRUE)
  
  # Heuristic: if data_dir contains a 'data' subfolder with config.yml, 
  # but data_dir itself doesn't have tt.bin, use the subfolder.
  if (!file.exists(file.path(data_dir, "tt.bin")) && 
      dir.exists(file.path(data_dir, "data")) && 
      file.exists(file.path(data_dir, "data", "tt.bin"))) {
    data_dir <- file.path(data_dir, "data")
  }
  
  dots <- .collapse_dots(list(...))

  # Resolve MOTIS binary
  cmd <- resolve_motis_cmd(motis_path)

  # Create temp files via manager
  tmp <- .temp_file_manager(temp_dir = temp_dir)
  query_file <- tmp$query
  meta_file <- tmp$meta
  response_file <- tmp$response
  
  if (!keep_files) {
    on.exit(unlink(c(query_file, meta_file, response_file)), add = TRUE)
  }

  # Format coordinates
  one_places <- .format_place_onemany(one)
  many_places_vec <- .format_place_onemany(many)
  n_many <- length(many_places_vec)
  n_origins <- length(one_places)
  
  # Extract IDs
  one_ids <- .get_ids(one, id_col = one_id_col)
  many_ids <- .get_ids(many, id_col = many_id_col)
  
  # Extract coordinates if needed for spatial operations
  if (spatial_sort || !is.null(spatial_filter_km)) {
    one_coords <- .extract_coords(one)
  }
  
  # Spatial sort origins
  if (spatial_sort) {
    sort_idx <- .spatial_sort_points(one_coords, method = "z-order")
    one_places <- one_places[sort_idx]
    one_ids <- one_ids[sort_idx]
    one_coords <- one_coords[sort_idx, , drop = FALSE]
  }
  
  # Prepare spatial filter if enabled
  if (!is.null(spatial_filter_km)) {
    many_coords <- .extract_coords(many)
    # Use 5% buffer for straight-line distance safety
    max_radius_km <- spatial_filter_km * 1.05
  }
  
  # Smart Chunking Dispatch for CLI
  dispatch <- .smart_chunk_dispatch(
    n_origins = n_origins, 
    n_dests = n_many, 
    engine = "batch",
    batch_size = batch_size,
    max_destinations_per_batch = max_destinations_per_batch
  )
  
  dest_chunks <- dispatch$dest_chunks

  # Validate with first origin (dry-run)
  tryCatch({
    .validate_batch_params(dots)
    # Correctly map duration for validation
    val_args <- list(
      one = one_places[1L],
      many = paste(many_places_vec[dest_chunks[[1]]], collapse = ","),
      arriveBy = arrive_by,
      maxMatchingDistance = maxMatchingDistance,
      .build_only = TRUE,
      .server = "http://localhost:8080"
    )
    
    # Conditionally include parameters not supported by intermodal API
    if (api_endpoint == "/api/v1/one-to-many") {
      val_args$mode <- mode
      val_args$withDistance <- withDistance
    }
    
    val_args[[duration_key]] <- max
    
    do.call(client_fun, c(val_args, dots))
  }, error = function(e) {
    stop("Invalid MOTIS API parameters: ", e$message, call. = FALSE)
  })

  # Generate all query lines and metadata lines
  total_lines <- n_origins * length(dest_chunks)
  
  query_lines <- character(total_lines)
  meta_lines <- character(total_lines)
  
  line_idx <- 0L
  for (i in seq_len(n_origins)) {
    # Apply spatial filter for this origin if enabled
    current_many_places <- many_places_vec
    current_many_ids <- many_ids
    current_dest_chunks <- dest_chunks
    
    if (!is.null(spatial_filter_km)) {
      origin_lat <- one_coords[i, "lat"]
      origin_lon <- one_coords[i, "lon"]
      
      # Professional degree conversion
      radii <- .km_to_deg(max_radius_km, origin_lat)
      
      # Bounding box filter
      lat_diff <- abs(many_coords[, "lat"] - origin_lat)
      lon_diff <- abs(many_coords[, "lon"] - origin_lon)
      keep_idx <- which(lat_diff <= radii$lat & lon_diff <= radii$lon)
      
      if (length(keep_idx) == 0) next
      
      current_many_places <- many_places_vec[keep_idx]
      current_many_ids <- many_ids[keep_idx]
      
      if (length(dest_chunks) > 1) {
         disp_tmp <- .smart_chunk_dispatch(1, length(keep_idx), "batch", 
                                           max_destinations_per_batch = max_destinations_per_batch)
         current_dest_chunks <- disp_tmp$dest_chunks
      } else {
         current_dest_chunks <- list(seq_along(keep_idx))
      }
    }
    
    for (k in seq_along(current_dest_chunks)) {
      idx <- current_dest_chunks[[k]]
      many_chk_str <- paste(current_many_places[idx], collapse = ",")
      many_ids_chk <- current_many_ids[idx]
      
      line_idx <- line_idx + 1L
      
      query_lines[line_idx] <- .build_one_to_many_query(
        one_place = one_places[i],
        many_places_str = many_chk_str,
        mode = mode,
        arrive_by = arrive_by,
        max = max,
        maxMatchingDistance = maxMatchingDistance,
        withDistance = withDistance,
        dots = dots,
        api_endpoint = api_endpoint,
        duration_key = duration_key
      )
      
      meta_lines[line_idx] <- paste(c(one_ids[i], many_ids_chk), collapse = "\t")
    }
  }

  # Remove empty lines
  non_empty <- nzchar(query_lines)
  query_lines <- query_lines[non_empty]
  meta_lines <- meta_lines[non_empty]
  actual_lines <- length(query_lines)

  # Write with custom EOL if requested
  if (!is.null(eol)) {
    q_con <- file(query_file, open = "wb")
    writeLines(query_lines, con = q_con, sep = eol)
    close(q_con)
    
    m_con <- file(meta_file, open = "wb")
    writeLines(meta_lines, con = m_con, sep = eol)
    close(m_con)
  } else {
    writeLines(query_lines, query_file)
    writeLines(meta_lines, meta_file)
  }

  if (echo) {
    .print_file_info("Query file", query_file, n_lines = actual_lines)
    .print_file_info("Metadata file", meta_file, n_lines = actual_lines)
  }

  # Execute MOTIS batch
  result <- processx::run(
    command = cmd,
    args = c("batch", "-q", query_file, "-r", response_file, "--data", data_dir),
    echo = echo,
    spinner = !echo,
    error_on_status = FALSE
  )

  if (result$status != 0L) {
    err_lines <- if (nzchar(result$stderr)) result$stderr else result$stdout
    stop("MOTIS batch failed (exit code ", result$status, "):\n", err_lines,
         call. = FALSE)
  }

  if (echo) {
    .print_file_info("Response file", response_file, n_lines = actual_lines)
  }

  # Parse responses
  res <- motis_one_to_many_read_batch(
    response_file = response_file,
    metadata_file = meta_file,
    arrive_by = arrive_by,
    chunk_size = chunk_size,
    output_callback = output_callback
  )
  
  if (is.null(output_callback) && !is.null(output_path)) {
     out_fmt <- "csv"
     if (grepl("\\.parquet$", output_path, ignore.case = TRUE) || dir.exists(output_path)) out_fmt <- "parquet"
     if (grepl("\\.duckdb$", output_path, ignore.case = TRUE)) out_fmt <- "duckdb"
     
     .unified_output_handler(res, output_path, format = out_fmt, append = FALSE)
     return(invisible(output_path))
  }
  
  res
}

#' Build a one-to-many URL query string
#'
#' @param one_place Single "lat;lon" string.
#' @param many_places_str Comma-separated "lat;lon" strings.
#' @param mode Routing mode.
#' @param arrive_by Logical.
#' @param max Maximum travel time.
#' @param maxMatchingDistance Maximum matching distance.
#' @param withDistance Logical. Include distance in the query?
#' @param dots Additional named parameters.
#' @param api_endpoint API endpoint path.
#' @return A single URL query string.
#' @noRd
.build_one_to_many_query <- function(
  one_place,
  many_places_str,
  mode,
  arrive_by,
  max,
  maxMatchingDistance,
  withDistance,
  dots,
  api_endpoint,
  duration_key = "max"
) {
  params_list <- list(
    one = one_place,
    many = many_places_str,
    arriveBy = arrive_by,
    maxMatchingDistance = maxMatchingDistance
  )
  
  # Conditionally include parameters not supported by intermodal API
  if (api_endpoint == "/api/v1/one-to-many") {
    params_list$mode <- mode
    params_list$withDistance <- withDistance
  }
  
  # Inject duration with correct key
  params_list[[duration_key]] <- max
  
  static_params <- c(params_list, dots)

  paste0(
    api_endpoint,
    "?",
    paste0(
      vapply(names(static_params), curl::curl_escape, character(1)),
      "=",
      vapply(static_params, function(v) {
        if (is.logical(v)) return(tolower(as.character(v)))
        curl::curl_escape(as.character(v))
      }, character(1)),
      collapse = "&"
    )
  )
}


#' Internal helper to print file info
#' @noRd
.print_file_info <- function(label, file, n_lines = NULL) {
  size <- file.size(file)
  size_str <- format(structure(size, class = "object_size"), units = "auto")
  
  if (is.null(n_lines)) {
    message(sprintf("%-14s %s (%s)", paste0(label, ":"), file, size_str))
  } else {
    message(sprintf("%-14s %s (%s, %d lines)", paste0(label, ":"), file, size_str, n_lines))
  }
}


#' Internal helper to format location inputs for the one-to-many endpoint
#' This endpoint requires "latitude;longitude" format.
#' @param place A data.frame, sf object, matrix, or character vector.
#' @param id_col The name of the ID column to use.
#' @return A character vector of "lat;lon" strings.
#' @noRd
.format_place_onemany <- function(place, id_col = "id") {
  if (inherits(place, "sf")) {
    rlang::check_installed("sf")

    # Verify geometry column exists and is valid, otherwise try to repair or fallback
    # The error "attr(obj, "sf_column") does not point to a geometry column" suggests metadata mismatch.
    
    # st_coordinates returns matrix with X, Y
    coords <- tryCatch({
       sf::st_coordinates(place)
    }, error = function(e) {
       # If direct extraction fails (e.g. lost geometry attribute), try to cast via st_as_sf if possible?
       # Attempt to repair by re-setting geometry if we can identify it
       if (inherits(place, "sf")) {
          # Use st_geometry to extract geometry directly if possible, or cast
          geom_col <- attr(place, "sf_column")
          if (!is.null(geom_col) && geom_col %in% names(place)) {
             sf::st_geometry(place) <- geom_col
             return(sf::st_coordinates(place))
          }
       }
       stop("Failed to extract coordinates from sf object: ", e$message, call. = FALSE)
    })
    
    if (nrow(coords) != nrow(place)) {
       # Try to get centroids if we have more coordinates than features (implies complex geometry)
       coords <- sf::st_coordinates(sf::st_centroid(place))
    }
    
    lat <- round(coords[, "Y"], 6); lon <- round(coords[, "X"], 6)
    return(paste(lat, lon, sep = ";"))
  }

  if (is.data.frame(place)) {
    p_names <- tolower(names(place))
    lat_col <- which(p_names %in% c("lat", "latitude"))
    lon_col <- which(p_names %in% c("lon", "lng", "longitude"))
    
    if (length(lat_col) == 1 && length(lon_col) == 1) {
      return(paste(place[[lat_col]], place[[lon_col]], sep = ";"))
    }
    
    # Fallback to ID if present, but for street routing coordinates are usually required
    id_col_lower <- tolower(id_col)
    if (id_col_lower %in% p_names) {
      id_col_idx <- which(p_names == id_col_lower)
      return(as.character(place[[id_col_idx]]))
    }
    
    stop("Data frame must contain coordinate columns ('lat', 'lon') or an '", id_col, "' column.", call. = FALSE)
  }

  if (is.matrix(place) && is.numeric(place)) {
    if (ncol(place) != 2) stop("Matrix must have 2 columns.", call. = FALSE)
    cnames <- tolower(colnames(place))
    if (all(c("lon", "lat") %in% cnames)) {
      return(paste(place[, "lat"], place[, "lon"], sep = ";"))
    }
    return(paste(place[, 2], place[, 1], sep = ";"))
  }

  if (is.character(place)) return(unname(place))
  stop("Unsupported input type.", call. = FALSE)
}

# --- Internal Implementations ---

#' Internal helper: Simple serial execution path
#' @noRd
.motis_one_to_many_simple <- function(
  one_place, many_places_vec,
  one, many,
  one_id_col, many_id_col,
  mode, arrive_by,
  max, maxMatchingDistance,
  withDistance,
  output,
  ...,
  api_endpoint = "/api/v1/one-to-many",
  duration_key = "max"
) {
  dots <- list(...)
  user_server <- dots[[".server"]]
  dots[c("one", "many", ".server", "output")] <- NULL 
  
  # Manual request construction to avoid motis.client::mc_oneToManyPost bug
  # The client function captures local 'req' variable into the body payload
  body_params <- list(
    one = unname(one_place),
    many = I(unname(many_places_vec)),
    arriveBy = unname(arrive_by),
    maxMatchingDistance = unname(maxMatchingDistance),
    elevationCosts = dots$elevationCosts %||% "NONE"
  )
  
  # Conditionally include parameters not supported by intermodal API
  if (api_endpoint == "/api/v1/one-to-many") {
    body_params$mode <- unname(mode)
    body_params$withDistance <- withDistance
  }
  
  # Inject duration with correct key
  body_params[[duration_key]] <- unname(max)
  
  dots$elevationCosts <- NULL
  if (length(dots) > 0) body_params <- utils::modifyList(body_params, dots)
  
  server_url <- user_server %||% .get_server_url()
  url <- paste0(sub("/$", "", server_url), api_endpoint)
  
  req <- httr2::request(url) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(body_params) |>
    httr2::req_retry(max_tries = 3)
    
  resp <- tryCatch({
    httr2::req_perform(req)
  }, error = function(e) {
    stop("MOTIS one-to-many request failed: ", e$message, call. = FALSE)
  })
  
  parsed <- httr2::resp_body_json(resp)
  
  if (output == "raw_list") return(parsed)
  
  # Use .get_ids from helpers.R
  one_ids <- .get_ids(one, id_col = one_id_col)
  many_ids <- .get_ids(many, id_col = many_id_col)
  
  return(.parse_otm_response(parsed, one_ids[1], many_ids, arrive_by))
}

#' Internal helper: Robust/Parallel execution path
#' @noRd
.motis_one_to_many_calc <- function(
  one_places, many_places_vec,
  one, many,
  one_id_col, many_id_col,
  mode, arrive_by,
  max, maxMatchingDistance,
  withDistance,
  dots,
  backend,
  batch_size,
  output_path,
  checkpoint_file,
  progress,
  parallel,
  api_endpoint = "/api/v1/one-to-many",
  duration_key = "max",
  spatial_filter_km = NULL
) { 
  # Setup code
  user_server <- dots[[".server"]]
  .server <- user_server %||% .get_server_url() %||% "http://localhost:8080"
  .server <- sub("/$", "", .server)
  
  # Remove .server from dots to strictly match API
  dots[[".server"]] <- NULL
  
  if (getOption("rmotis.wait_for_server", TRUE)) .wait_for_server(.server)
  
  one_ids <- .get_ids(one, id_col = one_id_col)
  many_ids <- .get_ids(many, id_col = many_id_col)
  n_origins <- length(one_places)
  n_dests <- length(many_places_vec)
  
  if (progress) {
    message(sprintf("Processing %s origins x %s destinations",
                    format(n_origins, big.mark = ","),
                    format(n_dests, big.mark = ",")))
  }
  
  # Extract coordinates for spatial sort/filter
  one_coords <- .extract_coords(one)
  
  # Spatial sort origins
  spatial_sort <- dots[["spatial_sort"]] %||% TRUE
  dots[["spatial_sort"]] <- NULL # Consume
  
  if (spatial_sort) {
    sort_idx <- .spatial_sort_points(one_coords, method = "z-order")
    one_places <- one_places[sort_idx]
    one_ids <- one_ids[sort_idx]
    one_coords <- one_coords[sort_idx, , drop = FALSE]
    if (progress) message("v Sorted origins spatially (z-order)")
  }
  
  # Smart Chunking Dispatch
  max_dest_per_batch <- dots[["max_destinations_per_batch"]]
  dots[["max_destinations_per_batch"]] <- NULL
  
  dispatch <- .smart_chunk_dispatch(
    n_origins = n_origins, 
    n_dests = n_dests, 
    engine = "api",
    batch_size = batch_size,
    max_destinations_per_batch = max_dest_per_batch
  )
  
  batch_size <- dispatch$batch_size
  dest_chunks <- dispatch$dest_chunks
  n_dest_chunks <- length(dest_chunks)
  
  if (progress && n_dest_chunks > 1) {
    message(sprintf("v Split destinations into %d chunks", n_dest_chunks))
  }
  
  if (!is.null(output_path) && file.exists(output_path) && !dir.exists(output_path)) {
    # ... existing logic or remove if handled by unified_output_handler
  }
  
  if (!is.null(checkpoint_file) && file.exists(checkpoint_file)) {
    completed_ids <- readLines(checkpoint_file, warn = FALSE)
    pending_mask <- !(one_ids %in% completed_ids)
    
    if (all(!pending_mask)) {
      if (progress) message("v All origins already completed (checkpoint)")
      if (is.null(output_path)) {
        return(data.frame(
          from_id = character(0), to_id = character(0), 
          duration_s = numeric(0), stringsAsFactors = FALSE
        ))
      } else {
        return(invisible(output_path))
      }
    } 
    
    # Filter
    one_places <- one_places[pending_mask]
    one_ids <- one_ids[pending_mask]
    one_coords <- one_coords[pending_mask, , drop = FALSE]
    n_origins <- length(one_places)
    if (progress) message(sprintf("v Resuming from checkpoint: %s pending", format(n_origins, big.mark = ",")))
  }
  
  # Spatial Filter Logic
  many_coords <- NULL
  max_radius_km <- NULL
  
  if (!is.null(spatial_filter_km)) {
    many_coords <- .extract_coords(many)
    # Use 5% buffer for straight-line distance safety
    max_radius_km <- spatial_filter_km * 1.05
    if (progress) message(sprintf("v Spatial filter enabled (radius: %.2f km)", spatial_filter_km))
  }
  
  dot_params <- .collapse_dots(dots)
  
  # Output Setup
  result_chunks <- list()
  chunk_idx <- 0L
  
  # Backend selection
  use_mirai <- FALSE
  if (parallel) {
    if (backend == "mirai") {
       rlang::check_installed("mirai")
       use_mirai <- TRUE
    } else if (backend == "auto") {
       # Robust detection: check if package is installed and daemons are set
       if (rlang::is_installed("mirai") && mirai::daemons_set() > 0) {
         use_mirai <- TRUE
       }
    }
  }
  
  if (progress) {
    if (use_mirai) {
      n_workers <- mirai::status()$connections
      message(sprintf("v Using 'mirai' backend for process-based parallelism (%d workers)", n_workers))
    } else if (parallel) {
      message("v Using 'httr2' backend for threaded parallelism")
    } else {
      message("v Using sequential execution")
    }
  }
  
  if (use_mirai) rlang::check_installed("mirai")
  
  # Processing Loop
  n_batches <- ceiling(n_origins / batch_size)
  
  for (batch_i in seq_len(n_batches)) {
    start_idx <- (batch_i - 1) * batch_size + 1
    end_idx <- min(batch_i * batch_size, n_origins)
    batch_indices <- start_idx:end_idx
    
    if (progress) {
      message(sprintf("Batch %d/%d: origins %d-%d", batch_i, n_batches, start_idx, end_idx))
    }
    
    # Nested loop for destination chunks
    for (dest_i in seq_along(dest_chunks)) {
      idx_d <- dest_chunks[[dest_i]]
      
      if (progress && n_dest_chunks > 1) {
        message(sprintf("  -> Dest chunk %d/%d (%d destinations)", dest_i, n_dest_chunks, length(idx_d)))
      }
      
      if (use_mirai) {
        batch_results <- .process_batch_mirai(
          origin_indices = batch_indices,
          one_places = one_places,
          many_places_vec = many_places_vec[idx_d],
          one_ids = one_ids,
          many_ids = many_ids[idx_d],
          one_coords = one_coords,
          many_coords = if(!is.null(many_coords)) many_coords[idx_d, , drop=FALSE] else NULL,
          max_radius_km = max_radius_km,
          mode = mode, arrive_by = arrive_by,
          max = max, maxMatchingDistance = maxMatchingDistance,
          withDistance = withDistance,
          dot_params = dot_params,
          .server = .server,
          api_endpoint = api_endpoint,
          duration_key = duration_key
        )
      } else {
        batch_results <- .process_parallel_batch(
          origin_indices = batch_indices,
          one_places = one_places,
          many_places_vec = many_places_vec[idx_d],
          one_ids = one_ids,
          many_ids = many_ids[idx_d],
          one_coords = one_coords,
          many_coords = if(!is.null(many_coords)) many_coords[idx_d, , drop=FALSE] else NULL,
          max_radius_km = max_radius_km,
          mode = mode, arrive_by = arrive_by,
          max = max, maxMatchingDistance = maxMatchingDistance,
          withDistance = withDistance,
          dots = dot_params, 
          .server = .server,
          progress = progress && (n_dest_chunks == 1), # reduce noise if many chunks
          api_endpoint = api_endpoint,
          duration_key = duration_key
        )
      }
      
      # Save results via unified_output_handler
      if (is.null(output_path)) {
        chunk_idx <- chunk_idx + 1L
        result_chunks[[chunk_idx]] <- batch_results
      } else {
        # Determine format from extension or directory existence
        out_fmt <- "csv"
        # If it ends in .parquet OR it's a directory (intended for partitions)
        # OR it doesn't have an extension and we want partitioned parquet by default for directories
        if (grepl("\\.parquet$", output_path, ignore.case = TRUE)) {
          out_fmt <- "parquet"
        } else if (grepl("\\.duckdb$", output_path, ignore.case = TRUE)) {
          out_fmt <- "duckdb"
        } else if (grepl("\\.csv$", output_path, ignore.case = TRUE)) {
          out_fmt <- "csv"
        } else if (!grepl("\\\\.[a-zA-Z0-9]+$", output_path)) {
          # No extension -> assume partitioned parquet directory
          out_fmt <- "parquet"
        }
        
        .unified_output_handler(
          results = batch_results,
          output_path = output_path,
          format = out_fmt,
          append = TRUE # always append in the loop
        )
      }
    }
    
    if (!is.null(checkpoint_file)) {
      cat(one_ids[batch_indices], file = checkpoint_file, sep = "\n", append = TRUE)
    }
  }
  
  if (progress) message("v Processing complete")
  
  if (is.null(output_path)) {
    dplyr::bind_rows(result_chunks)
  } else {
    invisible(output_path)
  }
} 

#' Internal: Process batch using mirai
#' @noRd
.process_batch_mirai <- function(
  origin_indices, one_places, many_places_vec, one_ids, many_ids,
  one_coords, many_coords, max_radius_km,
  mode, arrive_by, max, maxMatchingDistance, withDistance, dot_params, .server,
  api_endpoint = "/api/v1/one-to-many",
  duration_key = "max"
) {
  
  # Define worker function
  # Must satisfy self-contained execution
  # The worker function is defined inside .process_batch_mirai to capture
  # common arguments via closure, but mirai_map passes them via .args
  # so we need to extract them from i
  worker_fun <- function(i, ...) {
      tryCatch({
        # Ensure necessary packages are available in the worker environment
        requireNamespace("httr2", quietly = TRUE)
        requireNamespace("utils", quietly = TRUE)
        
        # Extract arguments passed via .args in mirai_map
        args <- list(...)
        
        # Extract task-specific arguments from i
        one_place <- i$one_place
        one_id <- i$one_id
        
        many_places_vec <- args$many_places_vec
        many_ids <- args$many_ids
        many_coords <- args$many_coords
        max_radius_km <- args$max_radius_km
        mode <- args$mode
        arrive_by <- args$arrive_by
        max <- args$max
        maxMatchingDistance <- args$maxMatchingDistance
        withDistance <- args$withDistance
        dot_params <- args$dot_params
        .server <- args$.server
        api_endpoint <- args$api_endpoint
        duration_key <- args$duration_key
        
        # --- Worker Logic ---
        
        # Professional degree conversion
        .km_to_deg <- function(radius_km, lat) {
          # Use a conservative radius (polar radius ~6357km) 
          # to ensure we are more inclusive at the equator.
          R <- 6357
          lat_deg <- (radius_km / R) * (180 / pi)
          lat_rad <- abs(lat) * (pi / 180)
          if (lat_rad > 1.55) lat_rad <- 1.55 # ~89 degrees
          lon_deg <- lat_deg / cos(lat_rad)
          list(lat = lat_deg, lon = lon_deg)
        }
        
        # Spatial Filter Logic (Same as before)
        keep_idx <- seq_along(many_places_vec) # Default to keeping all
        filtered_many_places <- many_places_vec
        filtered_many_ids <- many_ids
        
        if (!is.null(many_coords) && !is.null(max_radius_km)) {
           # Calculate distance approximation
           # one_place "lat;lon" -> parse
           parts <- as.numeric(strsplit(one_place, ";")[[1]])
           origin_lat <- parts[1]
           origin_lon <- parts[2]
           
           # Professional degree conversion
           radii <- .km_to_deg(max_radius_km, origin_lat)
           
           lat_diff <- abs(many_coords[, "lat"] - origin_lat)
           lon_diff <- abs(many_coords[, "lon"] - origin_lon)
           keep_idx <- which(lat_diff <= radii$lat & lon_diff <= radii$lon)
           
           if (length(keep_idx) == 0) {
              # Return empty result
              return(data.frame(
                from_id = if (arrive_by) character(0) else one_id,
                to_id = if (arrive_by) one_id else character(0),
                duration_s = numeric(0), distance_m = numeric(0),
                stringsAsFactors = FALSE
              ))
           }
           filtered_many_places <- many_places_vec[keep_idx]
           filtered_many_ids <- many_ids[keep_idx]
        }
        
        
        # POST Request - Manual construction due to motis.client capture bug
        body_params <- list(
          one = unname(one_place),
          many = I(unname(filtered_many_places)),
          arriveBy = unname(arrive_by),
          maxMatchingDistance = unname(maxMatchingDistance),
          elevationCosts = dot_params$elevationCosts %||% "NONE"
        )
        
        # Conditionally include parameters not supported by intermodal API
        if (api_endpoint == "/api/v1/one-to-many") {
          body_params$mode <- unname(mode)
          body_params$withDistance <- withDistance
        }
        
        # Inject duration with correct key
        body_params[[duration_key]] <- unname(max)
        
        # Remove consumed parameters from dot_params for the next call
        dot_params$elevationCosts <- NULL
        if (length(dot_params) > 0) body_params <- utils::modifyList(body_params, dot_params)
        
        url <- paste0(.server, api_endpoint)
        
        req <- httr2::request(url) |>
          httr2::req_method("POST") |>
          httr2::req_body_json(body_params) |>
          httr2::req_retry(max_tries = 3) |>
          httr2::req_timeout(600)
          
        resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
        
        if (is.null(resp)) {
           # If request failed, return NA for all destinations
           return(data.frame(
              from_id = if(arrive_by) filtered_many_ids else one_id,
              to_id = if(arrive_by) one_id else filtered_many_ids,
              duration_s = rep(NA_real_, length(filtered_many_ids)),
              distance_m = rep(NA_real_, length(filtered_many_ids)),
              stringsAsFactors = FALSE
           ))
        }
        
        # Parse Response
        # Replicate logic from .parse_otm_response to avoid dependency on internal function
        parsed <- httr2::resp_body_json(resp)
        
        n_dests_filtered <- length(filtered_many_ids)
        durations <- rep(NA_real_, n_dests_filtered)
        distances <- rep(NA_real_, n_dests_filtered) # Initialize distances
        
        if (is.list(parsed) && length(parsed) > 0) {
          for (k in seq_along(parsed)) {
            route <- parsed[[k]]
            if (!is.null(route$duration)) durations[k] <- as.numeric(route$duration)
            if (!is.null(route$distance)) distances[k] <- as.numeric(route$distance)
          }
        }
        
        res <- data.frame(
          from_id = rep(one_id, n_dests_filtered),
          to_id = filtered_many_ids,
          duration_s = durations,
          distance_m = distances,
          stringsAsFactors = FALSE
        )
        
        if (arrive_by) {
          names(res)[names(res) == "from_id"] <- ".tmp_from"
          names(res)[names(res) == "to_id"] <- "from_id"
          names(res)[names(res) == ".tmp_from"] <- "to_id"
        }
        
        return(res)
        
      }, error = function(e) {
         # If any error occurs within the worker, return NULL
         # This allows the main process to filter out failed tasks
         warning(sprintf("Error in mirai worker for origin '%s': %s", args$one_id, e$message), call. = FALSE)
         return(NULL)
      })
    }
  
  # Prepare arguments list
  # We map over indices
  args_list <- vector("list", length(origin_indices))
  for (i in seq_along(origin_indices)) {
     idx <- origin_indices[i]
     args_list[[i]] <- list(
        # idx = idx, # Not used in new worker_fun signature
        one_place = one_places[idx],
        one_id = one_ids[idx]
        # one_lat = one_coords[idx, "lat"], # Parsed inside worker
        # one_lon = one_coords[idx, "lon"]  # Parsed inside worker
     )
  }
  
  # Execute mirai_map
  # We pass common args via .args
  # Note: passing large `many_coords` to every worker might be heavy?
  # mirai handles it efficiently if passed once?
  # .args are passed to all.
  
  results <- mirai::mirai_map(
    args_list,
    worker_fun,
    .args = list(
       many_places_vec = many_places_vec,
       many_ids = many_ids,
       many_coords = many_coords,
       max_radius_km = max_radius_km,
       mode = mode, arrive_by = arrive_by,
       max = max, maxMatchingDistance = maxMatchingDistance,
       withDistance = withDistance,
       dot_params = dot_params, .server = .server,
       api_endpoint = api_endpoint,
       duration_key = duration_key
    )
  )[] # collect
  
  # Filter valid results (data frames) and handle errors
  valid_results <- Filter(is.data.frame, results)
  
  if (length(valid_results) < length(results)) {
     warning("motis_one_to_many: Some mirai batches failed.", call. = FALSE)
  }
  
  if (length(valid_results) == 0) {
      # Check if any errors occurred and stop if all failed
      if (length(results) > 0 && inherits(results[[1]], "errorValue")) {
          stop("motis_one_to_many: All mirai batches failed. First error: ", results[[1]], call. = FALSE)
      }
      # Return empty frame if no valid results (e.g., all filtered out or all failed gracefully)
       return(data.frame(
          from_id = character(0), to_id = character(0), 
          duration_s = numeric(0), distance_m = numeric(0),
          stringsAsFactors = FALSE
       ))
  }
  
  dplyr::bind_rows(valid_results)
}

#' Internal: Process batch using httr2
#' @noRd
.process_parallel_batch <- function(
  origin_indices, one_places, many_places_vec, one_ids, many_ids,
  one_coords, many_coords, max_radius_km,
  mode, arrive_by, max, maxMatchingDistance,
  withDistance, dots, .server, progress,
  api_endpoint = "/api/v1/one-to-many",
  duration_key = "max"
) {
  requests <- vector("list", length(origin_indices))
  origin_metadata <- vector("list", length(origin_indices))
  
  for (i in seq_along(origin_indices)) {
    idx <- origin_indices[i]
    origin_id <- one_ids[idx]
    origin_place <- one_places[idx]
    
    if (!is.null(many_coords) && !is.null(max_radius_km)) {
      origin_lat <- one_coords[idx, "lat"]
      origin_lon <- one_coords[idx, "lon"]
      
      # Professional degree conversion
      radii <- .km_to_deg(max_radius_km, origin_lat)
      
      lat_diff <- abs(many_coords[, "lat"] - origin_lat)
      lon_diff <- abs(many_coords[, "lon"] - origin_lon)
      keep_idx <- which(lat_diff <= radii$lat & lon_diff <= radii$lon)
      
      if (length(keep_idx) == 0) {
        origin_metadata[[i]] <- list(origin_id = origin_id, dest_ids = character(0), request_idx = NA_integer_)
        requests[[i]] <- NULL
        next
      }
      filtered_many_places <- many_places_vec[keep_idx]
      filtered_many_ids <- many_ids[keep_idx]
    } else {
      filtered_many_places <- many_places_vec
      filtered_many_ids <- many_ids
    }
    
    origin_metadata[[i]] <- list(origin_id = origin_id, dest_ids = filtered_many_ids, request_idx = i)
    
    # Manual request construction due to motis.client capture bug
    body_params <- list(
      one = origin_place, many = I(filtered_many_places),
      arriveBy = arrive_by,
      maxMatchingDistance = maxMatchingDistance,
      elevationCosts = dots$elevationCosts %||% "NONE"
    )
    
    # Conditionally include parameters not supported by intermodal API
    if (api_endpoint == "/api/v1/one-to-many") {
      body_params$mode <- mode
      body_params$withDistance <- withDistance
    }
    
    # Inject duration with correct key
    body_params[[duration_key]] <- unname(max)
    
    dots$elevationCosts <- NULL
    if (length(dots) > 0) body_params <- utils::modifyList(body_params, dots)
    
    req <- httr2::request(.server) |>
      httr2::req_url_path_append(api_endpoint) |>
      httr2::req_method("POST") |>
      httr2::req_body_json(body_params) |>
      httr2::req_retry(max_tries = getOption("rmotis.retry_max_tries", 3), backoff = getOption("rmotis.retry_backoff", ~ 2)) |>
      httr2::req_timeout(600)
    requests[[i]] <- req
  }
  
  valid_requests <- Filter(Negate(is.null), requests)
  valid_metadata <- Filter(function(m) !is.na(m$request_idx), origin_metadata)
  
  if (length(valid_requests) == 0) {
    return(data.frame(from_id = character(0), to_id = character(0), duration_s = numeric(0), stringsAsFactors = FALSE))
  }
  
  if (progress) message(sprintf("  -> Sending %d parallel HTTP requests...", length(valid_requests)))
  
  responses <- httr2::req_perform_parallel(valid_requests, on_error = "continue")
  results_list <- vector("list", length(responses))
  
  for (i in seq_along(responses)) {
    resp <- responses[[i]]
    meta <- valid_metadata[[i]]
    
    if (inherits(resp, "httr2_error") || httr2::resp_is_error(resp)) {
      results_list[[i]] <- data.frame(
        from_id = if (arrive_by) meta$dest_ids else meta$origin_id,
        to_id = if (arrive_by) meta$origin_id else meta$dest_ids,
        duration_s = NA_real_, distance_m = NA_real_, stringsAsFactors = FALSE
      )
      next
    }
    
    parsed <- tryCatch(httr2::resp_body_json(resp), error = function(e) list())
    results_list[[i]] <- .parse_otm_response(parsed, meta$origin_id, meta$dest_ids, arrive_by)
  }
  dplyr::bind_rows(results_list)
}

#' Parse one-to-many response JSON
#' @noRd
.parse_otm_response <- function(response_body, origin_id, dest_ids, arrive_by) {
  n_dests <- length(dest_ids)
  durations <- rep(NA_real_, n_dests)
  distances <- NULL
  has_distance <- FALSE
  
  if (is.list(response_body) && length(response_body) > 0) {
    # Only iterate up to n_dests to avoid length mismatch if response is longer
    for (i in seq_len(min(length(response_body), n_dests))) {
      route <- response_body[[i]]
      if (!is.null(route$duration)) durations[i] <- as.numeric(route$duration)
      if (!is.null(route$distance)) {
        if (!has_distance) {
          distances <- rep(NA_real_, n_dests)
          has_distance <- TRUE
        }
        distances[i] <- as.numeric(route$distance)
      }
    }
  }
  
  df <- data.frame(from_id = rep(origin_id, n_dests), to_id = dest_ids, duration_s = durations, stringsAsFactors = FALSE)
  if (has_distance) df$distance_m <- distances
  
  if (arrive_by) {
    names(df)[names(df) == "from_id"] <- ".tmp_from"
    names(df)[names(df) == "to_id"] <- "from_id"
    names(df)[names(df) == ".tmp_from"] <- "to_id"
  }
  df
}

#' Wait for server helper
#' @noRd
.wait_for_server <- function(server_url, timeout = 120, poll_interval = 2) {
  deadline <- Sys.time() + timeout
  while (Sys.time() < deadline) {
    tryCatch({
      resp <- httr2::request(paste0(server_url, "/")) |> httr2::req_timeout(5) |> httr2::req_perform()
      if (httr2::resp_status(resp) < 500) return(invisible(TRUE))
    }, error = function(e) NULL)
    Sys.sleep(poll_interval)
  }
  warning(sprintf("MOTIS server at %s did not respond.", server_url), call. = FALSE)
}
