#' Calculate one-to-many or many-to-one street-level routes
#'
#' This function is a user-friendly wrapper for the MOTIS `one-to-many` street
#' routing API. It computes travel time and distance from a single origin to
#' multiple destinations (or from multiple origins to a single destination)
#' using a specified travel mode (e.g., walking, cycling, or driving).
#'
#' This function uses a `POST` request to the MOTIS server, allowing for a large
#' number of destinations (1000+) without hitting URL length limitations.
#'
#' @param one The single origin (when `arrive_by = FALSE`) or destination
#'   (when `arrive_by = TRUE`). Can be a data frame/tibble with coordinate
#'   columns, an `sf` object with a single POINT geometry, or a numeric
#'   vector/matrix (`lon`, `lat`).
#' @param many The multiple destinations (when `arrive_by = FALSE`) or origins
#'   (when `arrive_by = TRUE`). Can be a data frame/tibble with coordinate
#'   columns, an `sf` object with POINT geometry, or a numeric matrix
#'   (`lon`, `lat`).
#' @param many_id_col The name of the column in `many` to use for identifying
#'   column, a sequence of numbers is used.
#' @param one_id_col The name of the column in `one` to use for identifying
#'   the point in the output. Defaults to `"id"`.
#' @param max maximum travel time in seconds
#' @param maxMatchingDistance maximum matching distance in meters to match geo coordinates to the street network
#' @param mode The routing profile to use. Defaults to `"WALK"`.
#' @param arrive_by Logical. If `FALSE` (the default), calculates routes from
#'   `one` to `many`. If `TRUE`, calculates routes from `many` to `one`.
#' @param output The desired output format. One of:
#'   - `"data.frame"` (default): A tidy data frame with travel times and distances.
#'   - `"raw_list"`: The raw parsed JSON response as a list.
#' @inheritDotParams motis.client::mc_oneToMany -one -many -mode -arriveBy -max -maxMatchingDistance -.endpoint
#' @return Depending on the `output` parameter, a `data.frame` or a list.
#'   The data frame will contain columns:
#'   - `from_id`: identifier of the origin
#'   - `to_id`: identifier of the destination
#'   - `duration_s`: travel time in seconds
#'   - `distance_m`: travel distance in meters (only included if `distance = TRUE` in `...`)
#' @export
#' @importFrom httr2 req_perform resp_body_json
#' @importFrom dplyr bind_rows
#' @importFrom rlang check_installed
motis_one_to_many <- function(
  one,
  one_id_col = "id",
  many,
  many_id_col = "id",
  mode = c("WALK", "BIKE", "CAR"),
  arrive_by = FALSE,
  max = 7200, # 2 hours in seconds
  maxMatchingDistance = 1000, # 1 km
  ...,
  output = c("data.frame", "raw_list")
) {
  # --- 1. Argument and Input Validation ---
  output <- match.arg(output)
  mode <- match.arg(mode)
  stopifnot("'one' must be a single location" = NROW(one) == 1)

  # --- 2. Format Inputs ---
  one_place <- .format_place_onemany(one)
  many_places_vec <- .format_place_onemany(many)

  # --- 3. Build Request Body ---
  dots <- list(...)
  user_server <- dots[[".server"]]
  dots[c("one", "many", "many_id_col", "mode", "arrive_by", "output", ".server")] <- NULL

  # Collapse any vector arguments in dots (though POST usually handles JSON lists better)
  # But for consistency with MOTIS params, we follow the schema.
  
  body_params <- list(
    one = unname(one_place),
    many = unname(many_places_vec),
    mode = unname(mode),
    arriveBy = unname(arrive_by),
    max = unname(max),
    maxMatchingDistance = unname(maxMatchingDistance),
    elevationCosts = dots$elevationCosts %||% "NONE"
  )
  dots$elevationCosts <- NULL
  
  # Merge dots into body
  if (length(dots) > 0) {
    body_params <- utils::modifyList(body_params, dots)
  }

  server_url <- user_server %||% .get_server_url()
  url <- paste0(sub("/$", "", server_url), "/api/v1/one-to-many")

  # --- 4. Perform POST Request ---
  req <- httr2::request(url) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(body_params) |>
    httr2::req_retry(max_tries = 3)

  resp <- tryCatch({
    httr2::req_perform(req)
  }, error = function(e) {
    stop("MOTIS one-to-many request failed: ", e$message, call. = FALSE)
  })

  # --- 5. Process and Parse Response ---
  parsed_response <- httr2::resp_body_json(resp)

  if (output == "raw_list") return(parsed_response)

  if (length(parsed_response) == 0) {
    empty_df <- data.frame(
      from_id = character(0),
      to_id = character(0),
      duration_s = numeric(0)
    )
    if ("distance" %in% names(dots) && isTRUE(dots$distance)) {
      empty_df$distance_m <- numeric(0)
    }
    return(empty_df)
  }

  one_id <- .get_ids(one, id_col = one_id_col)
  many_ids <- .get_ids(many, id_col = many_id_col)

  # Process responses while ensuring alignment with input many_ids
  res_list <- lapply(seq_along(parsed_response), function(i) {
    item <- parsed_response[[i]]
    if (length(item) == 0) {
      df <- data.frame(one_id = one_id, many_id = many_ids[i], duration = NA_real_)
    } else {
      df <- as.data.frame(item)
      df$one_id <- one_id
      df$many_id <- many_ids[i]
    }
    
    # Rename according to arrive_by
    if (!arrive_by) {
      names(df)[names(df) == "one_id"] <- "from_id"
      names(df)[names(df) == "many_id"] <- "to_id"
    } else {
      names(df)[names(df) == "one_id"] <- "to_id"
      names(df)[names(df) == "many_id"] <- "from_id"
    }
    
    # Standardize duration and distance names
    if ("duration" %in% names(df)) names(df)[names(df) == "duration"] <- "duration_s"
    if ("distance" %in% names(df)) names(df)[names(df) == "distance"] <- "distance_m"
    
    # Reorder columns
    cols <- c("from_id", "to_id", "duration_s", "distance_m")
    df <- df[, intersect(cols, names(df)), drop = FALSE]
    
    df
  })

  dplyr::bind_rows(res_list)
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
#' @param one_id_col The name of the column in `one` to use as the origin
#'   identifier in the metadata file. Defaults to `"id"`. Falls back to
#'   sequential row numbers if the column is not found.
#' @param many_id_col The name of the column in `many` to use as the
#'   destination identifiers in the metadata file. Defaults to `"id"`.
#' @param ... Additional MOTIS API parameters.
#' @param append Logical. If `TRUE`, appends to `output_file` and its
#'   `.meta` sidecar.
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
  one_id_col = "id",
  many_id_col = "id",
  ...,
  append = FALSE,
  api_endpoint = "/api/v1/one-to-many"
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

  message("Successfully wrote one-to-many batch query to '", output_file, "'.")
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

#' Run Full One-to-Many Batch Routing Cycle
#'
#' Generates batch query files, executes the MOTIS batch routing engine, and
#' parses the responses into a tidy table. This is a convenience wrapper that
#' combines [motis_one_to_many_generate_batch()] and
#' [motis_one_to_many_read_batch()] with MOTIS CLI execution.
#'
#' @param one The origins. Can be an `sf` object or data frame with coordinate
#'   columns. Multiple rows are supported — each row becomes a separate
#'   one-to-many query.
#' @param many The destinations (shared across all origins). Same input types
#'   as `one`.
#' @param data_dir Path to the MOTIS data directory (the directory containing
#'   the `data/` subfolder with imported routing data).
#' @param mode The routing profile. One of `"WALK"`, `"BIKE"`, `"CAR"`.
#' @param arrive_by Logical. If `FALSE` (default), routes from `one` to `many`.
#' @param max Maximum travel time in seconds.
#' @param maxMatchingDistance Maximum matching distance in meters.
#' @param one_id_col Column name in `one` to use as origin identifiers.
#' @param many_id_col Column name in `many` to use as destination identifiers.
#' @param ... Additional MOTIS API parameters (e.g., `withDistance = TRUE`).
#' @param motis_path Path to the directory containing the MOTIS binary, or
#'   `NULL` to use the system PATH.
#' @param chunk_size Number of response lines to process at a time.
#' @param output_callback Optional function for streaming results (see
#'   [motis_one_to_many_read_batch()]).
#' @param echo Logical. If `TRUE` (default), echo MOTIS batch output
#'   (timing statistics) to the console.
#'
#' @return A data.frame with columns `from_id`, `to_id`, `duration_s`, and
#'   optionally `distance_m`. Returns `invisible(NULL)` when `output_callback`
#'   is used.
#' @export
#' @importFrom processx run
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
  ...,
  motis_path = NULL,
  chunk_size = 10000L,
  output_callback = NULL,
  echo = TRUE
) {
  mode <- match.arg(mode)
  data_dir <- normalizePath(data_dir, mustWork = TRUE)
  dots <- .collapse_dots(list(...))

  # Resolve MOTIS binary
  cmd <- resolve_motis_cmd(motis_path)

  # Create temp files
  query_file <- tempfile(fileext = ".txt")
  meta_file <- paste0(query_file, ".meta")
  response_file <- tempfile(fileext = ".txt")
  on.exit(unlink(c(query_file, meta_file, response_file)), add = TRUE)

  # Format coordinates
  one_places <- .format_place_onemany(one)
  many_places_vec <- .format_place_onemany(many)
  many_places_str <- paste(many_places_vec, collapse = ",")

  # Extract IDs
  one_ids <- .get_ids(one, id_col = one_id_col)
  many_ids <- .get_ids(many, id_col = many_id_col)

  # Validate with first origin (dry-run)
  tryCatch({
    .validate_batch_params(dots)
    do.call(motis.client::mc_oneToMany, c(
      list(
        one = one_places[1L],
        many = many_places_str,
        mode = mode,
        arriveBy = arrive_by,
        max = max,
        maxMatchingDistance = maxMatchingDistance,
        .build_only = TRUE,
        .server = "http://localhost:8080"
      ),
      dots
    ))
  }, error = function(e) {
    stop("Invalid MOTIS API parameters: ", e$message, call. = FALSE)
  })

  # Generate all query lines and metadata lines
  n_origins <- length(one_places)
  query_lines <- character(n_origins)
  meta_lines <- character(n_origins)

  for (i in seq_len(n_origins)) {
    query_lines[i] <- .build_one_to_many_query(
      one_place = one_places[i],
      many_places_str = many_places_str,
      mode = mode,
      arrive_by = arrive_by,
      max = max,
      maxMatchingDistance = maxMatchingDistance,
      dots = dots,
      api_endpoint = "/api/v1/one-to-many"
    )
    meta_lines[i] <- paste(c(one_ids[i], many_ids), collapse = "\t")
  }

  writeLines(query_lines, query_file)
  writeLines(meta_lines, meta_file)

  message("Generated ", n_origins, " batch queries.")

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

  # Parse responses
  motis_one_to_many_read_batch(
    response_file = response_file,
    metadata_file = meta_file,
    arrive_by = arrive_by,
    chunk_size = chunk_size,
    output_callback = output_callback
  )
}

#' Build a one-to-many URL query string
#'
#' @param one_place Single "lat;lon" string.
#' @param many_places_str Comma-separated "lat;lon" strings.
#' @param mode Routing mode.
#' @param arrive_by Logical.
#' @param max Maximum travel time.
#' @param maxMatchingDistance Maximum matching distance.
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
  dots,
  api_endpoint
) {
  static_params <- c(
    list(
      one = one_place,
      many = many_places_str,
      mode = mode,
      arriveBy = arrive_by,
      max = max,
      maxMatchingDistance = maxMatchingDistance
    ),
    dots
  )

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
