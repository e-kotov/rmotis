#' Parallel One-to-Many Routing via HTTP
#'
#' Execute one-to-many routing requests in parallel using httr2. This function
#' bypasses the MOTIS batch CLI and sends HTTP POST requests directly to a
#' running MOTIS server, enabling checkpointing and incremental output.
#'
#' @param one Origins. Can be sf, data.frame with lat/lon columns, matrix, or
#'   character vector of `"lat;lon"` strings.
#' @param many Destinations. Same format as `one`.
#' @param mode Travel mode: `"WALK"`, `"BIKE"`, or `"CAR"`.
#' @param arrive_by Logical. If `TRUE`, treat `one` as destinations and `many`
#'   as origins (many-to-one).
#' @param max Maximum travel time in seconds. Defaults to `7200` (2 hours).
#' @param maxMatchingDistance Maximum map-matching distance in meters.
#'   Defaults to `1000`.
#' @param one_id_col Column name for origin IDs. Defaults to `"id"`.
#' @param many_id_col Column name for destination IDs. Defaults to `"id"`.
#' @param ... Additional API parameters passed to the MOTIS one-to-many endpoint.
#' @param .server MOTIS server URL. Defaults to `getOption("rmotis.server")` or
#'   `"http://localhost:8080"`.
#' @param batch_size Number of origins to process in parallel per batch.
#'   Defaults to `16`. Set based on server capacity and network conditions.
#' @param output_file Optional output file path:
#'   - `.csv`: Appended incrementally with `data.table::fwrite()` (or
#'     `utils::write.table()` fallback).
#'   - `.parquet`: Creates a *directory* at this path containing
#'     `batch_0001.parquet`, `batch_0002.parquet`, etc. Read with
#'     `arrow::open_dataset(path) |> collect()`.
#'   - `NULL` (default): Accumulate results in memory and return as data.frame.
#' @param checkpoint_file Optional checkpoint file path. If provided, the
#'   function will:
#'   - On first run: create the checkpoint file and log completed origin IDs.
#'   - On subsequent runs: skip already-completed origins and resume from where
#'     it left off.
#'   Checkpoint format is plain text (one origin ID per line) for human
#'   readability.
#' @param spatial_filter Logical. If `TRUE` (default), pre-filter destinations
#'   per origin to a bounding box based on `max` travel time and mode speed.
#' @param spatial_sort Logical. If `TRUE` (default), sort origins by latitude
#'   before processing. Improves cache locality if the MOTIS server maintains
#'   graph caches.
#' @param max_speed_kmh Optional custom max speed in km/h for spatial filtering.
#'   If `NULL`, uses mode defaults (WALK=6, BIKE=20, CAR=130).
#' @param progress Logical. If `TRUE` (default), display progress messages.
#'
#' @return
#' - If `output_file = NULL`: Returns a data.frame with columns `from_id`,
#'   `to_id`, `duration_s`, and optionally `distance_m`.
#' - If `output_file` is specified: Returns the file path invisibly.
#'
#' @section Checkpointing:
#' Checkpoints are crucial for long-running jobs. If the process is interrupted,
#' simply re-run the same command with the same `checkpoint_file` path to resume.
#'
#' @section Output Formats:
#' - **CSV**: Efficient for streaming appends. Header written once.
#' - **Parquet directory**: Ideal for large datasets. Each batch is a separate
#'   file. Use `arrow::open_dataset()` to read all batches at once.
#' - **In-memory**: Convenient for small datasets or interactive use.
#'
#' @seealso [motis_one_to_many_batch()], [motis_one_to_many()]
#' @export
motis_one_to_many_parallel <- function(
  one,
  many,
  mode = c("WALK", "BIKE", "CAR"),
  arrive_by = FALSE,
  max = 7200,
  maxMatchingDistance = 1000,
  one_id_col = "id",
  many_id_col = "id",
  ...,
  .server = getOption("rmotis.server", "http://localhost:8080"),
  batch_size = 16L,
  output_file = NULL,
  checkpoint_file = NULL,
  spatial_filter = TRUE,
  spatial_sort = TRUE,
  max_speed_kmh = NULL,
  progress = TRUE
) {
  mode <- match.arg(mode)
  batch_size <- as.integer(batch_size)
  if (batch_size < 1) stop("batch_size must be >= 1", call. = FALSE)
  
  # Validate server URL
  if (is.null(.server) || nchar(.server) == 0) {
    stop("MOTIS server URL is required. Set .server or options(rmotis.server = '...')", 
         call. = FALSE)
  }
  
  # Ensure trailing slash is removed
  .server <- sub("/$", "", .server)
  
  # Wait for server readiness
  if (getOption("rmotis.wait_for_server", TRUE)) .wait_for_server(.server)
  
  # Format coordinates and extract IDs
  one_places <- .format_place_onemany(one)
  many_places_vec <- .format_place_onemany(many)
  one_ids <- .get_ids(one, id_col = one_id_col)
  many_ids <- .get_ids(many, id_col = many_id_col)
  n_origins <- length(one_places)
  n_dests <- length(many_places_vec)
  
  if (progress) {
    message(sprintf("Processing %s origins × %s destinations",
                    format(n_origins, big.mark = ","),
                    format(n_dests, big.mark = ",")))
  }
  
  # Extract coordinates
  one_coords <- .extract_coords(one)
  
  # Spatial sort origins by latitude
  if (spatial_sort) {
    sort_idx <- order(one_coords[, "lat"])
    one_places <- one_places[sort_idx]
    one_ids <- one_ids[sort_idx]
    one_coords <- one_coords[sort_idx, , drop = FALSE]
    
    if (progress) {
      message("✓ Sorted origins by latitude")
    }
  }
  
  # Load checkpoint if exists
  completed_ids <- character(0)
  if (!is.null(checkpoint_file)) {
    if (file.exists(checkpoint_file)) {
      completed_ids <- readLines(checkpoint_file, warn = FALSE)
      pending_mask <- !(one_ids %in% completed_ids)
      
      if (all(!pending_mask)) {
        if (progress) {
          message("✓ All origins already completed (checkpoint)")
        }
        if (is.null(output_file)) {
          # Return empty data.frame with correct structure
          return(data.frame(
            from_id = character(0),
            to_id = character(0),
            duration_s = numeric(0),
            stringsAsFactors = FALSE
          ))
        } else {
          return(invisible(output_file))
        }
      }
      
      # Filter to pending origins
      one_places <- one_places[pending_mask]
      one_ids <- one_ids[pending_mask]
      one_coords <- one_coords[pending_mask, , drop = FALSE]
      n_origins <- length(one_places)
      
      if (progress) {
        message(sprintf("✓ Resuming from checkpoint: %s of %s origins pending",
                        format(n_origins, big.mark = ","),
                        format(length(pending_mask), big.mark = ",")))
      }
    }
  }
  
  # Prepare spatial filter if enabled
  if (spatial_filter) {
    many_coords <- .extract_coords(many)
    # Speed estimates (km/h)
    if (is.null(max_speed_kmh)) {
      max_speed <- switch(mode, WALK = 6, BIKE = 20, CAR = 130)
    } else {
      max_speed <- max_speed_kmh
    }
    # Max travel distance in km with 20% buffer
    max_radius_km <- (max * max_speed / 3600) * 1.2
    # Convert to degrees (rough approximation: 1 degree ≈ 111 km)
    max_radius_deg <- max_radius_km / 111.0
    
    if (progress) {
      message(sprintf("✓ Spatial filter enabled (radius: %.2f km)", max_radius_km))
    }
  }
  
  # Build API parameter template (will be filled per origin)
  dots <- .collapse_dots(list(...))
  
  # Initialize output accumulator
  if (is.null(output_file)) {
    result_chunks <- list()
    chunk_idx <- 0L
  } else {
    # Detect output format
    is_parquet <- grepl("\\.parquet$", output_file, ignore.case = TRUE)
    is_csv <- grepl("\\.csv$", output_file, ignore.case = TRUE)
    
    if (is_parquet) {
      # Create directory for parquet files
      if (!dir.exists(output_file)) {
        dir.create(output_file, recursive = TRUE)
      }
      parquet_batch_num <- length(list.files(output_file, pattern = "\\.parquet$"))
    } else if (!is_csv) {
      warning("output_file should end in .csv or .parquet; assuming CSV format",
              call. = FALSE)
    }
    
    csv_written <- file.exists(output_file) && file.size(output_file) > 0
  }
  
  # Process origins in batches
  n_batches <- ceiling(n_origins / batch_size)
  
  for (batch_i in seq_len(n_batches)) {
    start_idx <- (batch_i - 1) * batch_size + 1
    end_idx <- min(batch_i * batch_size, n_origins)
    batch_indices <- start_idx:end_idx
    
    if (progress) {
      message(sprintf("Batch %d/%d: origins %d-%d",
                      batch_i, n_batches, start_idx, end_idx))
    }
    
    # Build requests for this batch
    batch_results <- .process_parallel_batch(
      origin_indices = batch_indices,
      one_places = one_places,
      many_places_vec = many_places_vec,
      one_ids = one_ids,
      many_ids = many_ids,
      one_coords = one_coords,
      many_coords = if (spatial_filter) many_coords else NULL,
      max_radius_deg = if (spatial_filter) max_radius_deg else NULL,
      mode = mode,
      arrive_by = arrive_by,
      max = max,
      maxMatchingDistance = maxMatchingDistance,
      dots = dots,
      .server = .server,
      progress = progress
    )
    
    # Append results
    if (!is.null(output_file)) {
      .append_results(
        results = batch_results,
        output_file = output_file,
        is_parquet = if (exists("is_parquet")) is_parquet else FALSE,
        is_csv = if (exists("is_csv")) is_csv else TRUE,
        csv_written = if (exists("csv_written")) csv_written else FALSE,
        parquet_batch_num = if (exists("parquet_batch_num")) parquet_batch_num else 0
      )
      
      if (exists("is_parquet") && is_parquet) {
        parquet_batch_num <- parquet_batch_num + 1
      }
      if (exists("is_csv") && is_csv) {
        csv_written <- TRUE
      }
    } else {
      chunk_idx <- chunk_idx + 1L
      result_chunks[[chunk_idx]] <- batch_results
    }
    
    # Update checkpoint
    if (!is.null(checkpoint_file)) {
      completed_batch_ids <- one_ids[batch_indices]
      cat(completed_batch_ids, file = checkpoint_file, sep = "\n", append = TRUE)
    }
  }
  
  if (progress) {
    message("✓ Processing complete")
  }
  
  # Return results
  if (is.null(output_file)) {
    dplyr::bind_rows(result_chunks)
  } else {
    invisible(output_file)
  }
}

#' Process a batch of origins in parallel with httr2
#' @param origin_indices Indices of origins to process in this batch
#' @param one_places Formatted origin coordinate strings
#' @param many_places_vec Formatted destination coordinate strings
#' @param one_ids Origin IDs
#' @param many_ids Destination IDs
#' @param one_coords Origin coordinate matrix
#' @param many_coords Destination coordinate matrix (NULL if no spatial filter)
#' @param max_radius_deg Max radius in degrees for spatial filter (NULL if disabled)
#' @param mode Travel mode
#' @param arrive_by Arrive by flag
#' @param max Max travel time
#' @param maxMatchingDistance Max matching distance
#' @param dots Additional API parameters
#' @param .server MOTIS server URL
#' @param progress Show progress
#' @return data.frame with routing results
#' @noRd
.process_parallel_batch <- function(
  origin_indices,
  one_places,
  many_places_vec,
  one_ids,
  many_ids,
  one_coords,
  many_coords,
  max_radius_deg,
  mode,
  arrive_by,
  max,
  maxMatchingDistance,
  dots,
  .server,
  progress
) {
  # Build requests for each origin in this batch
  requests <- vector("list", length(origin_indices))
  origin_metadata <- vector("list", length(origin_indices))
  
  for (i in seq_along(origin_indices)) {
    idx <- origin_indices[i]
    origin_id <- one_ids[idx]
    origin_place <- one_places[idx]
    
    # Apply spatial filter if enabled
    if (!is.null(many_coords) && !is.null(max_radius_deg)) {
      origin_lat <- one_coords[idx, "lat"]
      origin_lon <- one_coords[idx, "lon"]
      
      # Bounding box filter
      lat_diff <- abs(many_coords[, "lat"] - origin_lat)
      lon_diff <- abs(many_coords[, "lon"] - origin_lon)
      keep_idx <- which(lat_diff <= max_radius_deg & lon_diff <= max_radius_deg)
      
      if (length(keep_idx) == 0) {
        # No destinations in range - store metadata but skip request
        origin_metadata[[i]] <- list(
          origin_id = origin_id,
          dest_ids = character(0),
          request_idx = NA_integer_
        )
        requests[[i]] <- NULL
        next
      }
      
      filtered_many_places <- many_places_vec[keep_idx]
      filtered_many_ids <- many_ids[keep_idx]
    } else {
      # No filtering
      filtered_many_places <- many_places_vec
      filtered_many_ids <- many_ids
    }
    
    # Store metadata for response parsing
    origin_metadata[[i]] <- list(
      origin_id = origin_id,
      dest_ids = filtered_many_ids,
      request_idx = i
    )
    
    # Build POST request body
    body_params <- list(
      one = origin_place,
      many = paste(filtered_many_places, collapse = ","),
      mode = mode,
      arriveBy = arrive_by,
      max = max,
      maxMatchingDistance = maxMatchingDistance
    )
    
    # Add additional parameters from dots
    if (length(dots) > 0) {
      body_params <- c(body_params, dots)
    }
    
    # Build httr2 request
    req <- httr2::request(.server) |>
      httr2::req_url_path_append("api/v1/one-to-many") |>
      httr2::req_method("POST") |>
      httr2::req_headers("Content-Type" = "application/x-www-form-urlencoded") |>
      httr2::req_body_form(!!!body_params) |>
      # Add retry logic
      httr2::req_retry(max_tries = getOption("rmotis.retry_max_tries", 3), backoff = getOption("rmotis.retry_backoff", ~ 2)) |>
      httr2::req_timeout(600)
    
    requests[[i]] <- req
  }
  
  # Filter out NULL requests (origins with no destinations in range)
  valid_requests <- Filter(Negate(is.null), requests)
  valid_metadata <- Filter(function(m) !is.na(m$request_idx), origin_metadata)
  
  if (length(valid_requests) == 0) {
    # All origins filtered out - return empty data.frame
    return(data.frame(
      from_id = character(0),
      to_id = character(0),
      duration_s = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Execute requests in parallel
  if (progress) {
    message(sprintf("  → Sending %d parallel HTTP requests...", length(valid_requests)))
  }
  
  responses <- httr2::req_perform_parallel(
    valid_requests,
    on_error = "continue"
  )
  
  # Parse responses
  results_list <- vector("list", length(responses))
  
  for (i in seq_along(responses)) {
    resp <- responses[[i]]
    meta <- valid_metadata[[i]]
    
    # Check for errors - use tryCatch as resp may not be a proper response object
    is_error <- tryCatch(
      httr2::resp_is_error(resp),
      error = function(e) TRUE  # If check fails, treat as error
    )
    
    if (is_error) {
      status_msg <- tryCatch(
        httr2::resp_status_desc(resp),
        error = function(e) "Unknown error"
      )
      warning(sprintf("Request failed for origin %s: %s",
                      meta$origin_id, status_msg),
              call. = FALSE)
      # Return NA results for this origin
      results_list[[i]] <- data.frame(
        from_id = if (arrive_by) meta$dest_ids else meta$origin_id,
        to_id = if (arrive_by) meta$origin_id else meta$dest_ids,
        duration_s = NA_real_,
        distance_m = NA_real_,
        stringsAsFactors = FALSE
      )
      next
    }
    
    # Parse JSON response with error handling
    body <- tryCatch(
      httr2::resp_body_json(resp),
      error = function(e) {
        warning(sprintf("JSON parse error for origin %s: %s",
                        meta$origin_id, e$message),
                call. = FALSE)
        list()  # Return empty list
      }
    )
    
    # Extract results
    parsed <- .parse_otm_response(
      response_body = body,
      origin_id = meta$origin_id,
      dest_ids = meta$dest_ids,
      arrive_by = arrive_by
    )
    
    results_list[[i]] <- parsed
  }
  
  # Combine all results
  dplyr::bind_rows(results_list)
}

#' Parse one-to-many response JSON
#' @param response_body Parsed JSON body from httr2::resp_body_json()
#' @param origin_id Origin ID
#' @param dest_ids Destination IDs
#' @param arrive_by Arrive by flag (swaps from/to if TRUE)
#' @return data.frame with from_id, to_id, duration_s, distance_m (optional)
#' @noRd
.parse_otm_response <- function(response_body, origin_id, dest_ids, arrive_by) {
  n_dests <- length(dest_ids)
  
  # Initialize vectors
  durations <- rep(NA_real_, n_dests)
  distances <- NULL
  has_distance <- FALSE
  
  # Check if response is a list of route objects
  if (!is.list(response_body) || length(response_body) == 0) {
    # Empty or invalid response
    return(data.frame(
      from_id = rep(origin_id, n_dests),
      to_id = dest_ids,
      duration_s = durations,
      stringsAsFactors = FALSE
    ))
  }
  
  # Extract duration and distance from  
  # Parse routes
  for (i in seq_along(response_body)) {
    route <- response_body[[i]]
    
    # Handle NULL or missing duration
    if (!is.null(route$duration) && length(route$duration) > 0) {
      durations[i] <- as.numeric(route$duration)
    }
    
    # Handle NULL or missing distance
    if (!is.null(route$distance) && length(route$distance) > 0) {
      if (!has_distance) {
        distances <- rep(NA_real_, n_dests)
        has_distance <- TRUE
      }
      distances[i] <- as.numeric(route$distance)
    }
  }
  
  # Build result data.frame
  df <- data.frame(
    from_id = rep(origin_id, n_dests),
    to_id = dest_ids,
    duration_s = durations,
    stringsAsFactors = FALSE
  )
  
  if (has_distance) {
    df$distance_m <- distances
  }
  
  # Apply arrive_by column swap
  if (arrive_by) {
    names(df)[names(df) == "from_id"] <- ".tmp_from"
    names(df)[names(df) == "to_id"] <- "from_id"
    names(df)[names(df) == ".tmp_from"] <- "to_id"
  }
  
  df
}

#' Append results to output file (CSV or parquet)
#' @param results data.frame to append
#' @param output_file Output file path
#' @param is_parquet Logical, is this parquet output?
#' @param is_csv Logical, is this CSV output?
#' @param csv_written Has CSV header been written?
#' @param parquet_batch_num Current parquet batch number
#' @noRd
.append_results <- function(results, output_file, is_parquet, is_csv,
                            csv_written, parquet_batch_num) {
  if (is_parquet) {
    # Write to parquet directory
    batch_file <- file.path(
      output_file,
      sprintf("batch_%04d.parquet", parquet_batch_num + 1)
    )
    
    if (rlang::is_installed("arrow")) {
      arrow::write_parquet(results, batch_file)
    } else {
      stop("Package 'arrow' is required for parquet output", call. = FALSE)
    }
  } else {
    # Write to CSV
    if (rlang::is_installed("data.table") && requireNamespace("data.table", quietly = TRUE)) {
      # Use data.table::fwrite for fast writes
      data.table::fwrite(
        results,
        file = output_file,
        append = csv_written,
        col.names = !csv_written
      )
    } else {
      # Fallback to utils::write.table
      utils::write.table(
        results,
        file = output_file,
        append = csv_written,
        col.names = !csv_written,
        row.names = FALSE,
        sep = ",",
        quote = TRUE
      )
    }
  }
}

#' @noRd
.wait_for_server <- function(server_url, timeout = 120, poll_interval = 2) {
  deadline <- Sys.time() + timeout
  while (Sys.time() < deadline) {
    tryCatch({
      resp <- httr2::request(paste0(server_url, "/")) |>
        httr2::req_timeout(5) |>
        httr2::req_perform()
      if (httr2::resp_status(resp) < 500) return(invisible(TRUE))
    }, error = function(e) NULL)
    Sys.sleep(poll_interval)
  }
  warning(sprintf("MOTIS server at %s did not respond within %.0f seconds. Proceeding anyway...", 
                  server_url, timeout), call. = FALSE)
}
