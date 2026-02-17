#' Multi-origin Batch Planning for One-to-Many Routing
#'
#' Generate a batch query file for multiple origins with spatial optimizations.
#' This function wraps [motis_one_to_many_generate_batch()] and iterates over
#' origins, applying spatial sorting and filtering for improved performance.
#'
#' @param one Origins. Same as [motis_one_to_many_batch()].
#' @param many Destinations. Same as [motis_one_to_many_batch()].
#' @param output_file Path to the output query file.
#' @param mode Travel mode: `"WALK"`, `"BIKE"`, or `"CAR"`.
#' @param arrive_by Logical. If `TRUE`, treat `one` as destinations and
#'   `many` as origins (many-to-one).
#' @param max Maximum travel time in seconds. Defaults to `7200` (2 hours).
#' @param maxMatchingDistance Maximum map-matching distance in meters.
#'   Defaults to `1000`.
#' @param one_id_col Column name for origin IDs. Defaults to `"id"`.
#' @param many_id_col Column name for destination IDs. Defaults to `"id"`.
#' @param ... Additional API parameters passed to the MOTIS one-to-many endpoint.
#' @param spatial_filter Logical. If `TRUE` (default), pre-filter destinations
#'   per origin to a bounding box based on `max` travel time and typical mode
#'   speed. Reduces match memory and I/O for unreachable destinations.
#' @param spatial_sort Logical. If `TRUE` (default), sort origins by latitude
#'   before generating queries. Improves MOTIS graph cache locality when
#'   processing the batch file.
#' @param progress Logical. If `TRUE` (default), display progress messages.
#' @param api_endpoint API endpoint path. Defaults to `"/api/v1/one-to-many"`.
#'
#' @return Invisibly returns a list with:
#'   - `file`: Path to the output file
#'   - `n_lines`: Number of query lines generated
#'   - `file_size`: File size in bytes
#'
#' @section Relationship to Other Functions:
#' This function is a convenience wrapper that:
#' 1. Sorts origins by latitude (`spatial_sort = TRUE`)
#' 2. For each origin:
#'    - Filters destinations to a bounding box (`spatial_filter = TRUE`)
#'    - Calls [motis_one_to_many_generate_batch()] with `append = TRUE`
#' 3. Returns file metadata
#'
#' @section Spatial Optimizations:
#' - **Spatial sort**: Origins are sorted by latitude before iteration. This
#'   improves MOTIS graph cache hits when processing the batch file sequentially.
#' - **Spatial filter**: For each origin, destinations are filtered to a
#'   bounding box based on `max` travel time, mode speed, and a 20% buffer.
#'   This reduces unnecessary map-matching attempts and response I/O.
#'
#' @seealso [motis_one_to_many_batch()], [motis_one_to_many_generate_batch()]
#' @export
motis_one_to_many_plan_batch <- function(
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
  spatial_filter = TRUE,
  spatial_sort = TRUE,
  progress = TRUE,
  api_endpoint = "/api/v1/one-to-many"
) {
  mode <- match.arg(mode)
  
  # Format coordinates and extract IDs
  one_places <- .format_place_onemany(one)
  many_places_vec <- .format_place_onemany(many)
  one_ids <- .get_ids(one, id_col = one_id_col)
  many_ids <- .get_ids(many, id_col = many_id_col)
  n_origins <- length(one_places)
  
  # Extract coordinates - always needed for building origin data frames
  one_coords <- .extract_coords(one)
  
  # Spatial sort origins by latitude
  if (spatial_sort) {
    sort_idx <- order(one_coords[, "lat"])
    one_places <- one_places[sort_idx]
    one_ids <- one_ids[sort_idx]
    one_coords <- one_coords[sort_idx, , drop = FALSE]
  }
  
  # Prepare spatial filter if enabled
  if (spatial_filter) {
    many_coords <- .extract_coords(many)
    # Speed estimates (km/h)
    max_speed <- switch(mode, WALK = 6, BIKE = 20, CAR = 130)
    # Max travel distance in km with 20% buffer
    max_radius_km <- (max * max_speed / 3600) * 1.2
    # Convert to degrees (rough approximation: 1 degree ≈ 111 km)
    max_radius_deg <- max_radius_km / 111.0
  }
  
  # Iterate over origins
  for (i in seq_len(n_origins)) {
    # Apply spatial filter for this origin if enabled
    if (spatial_filter) {
      origin_lat <- one_coords[i, "lat"]
      origin_lon <- one_coords[i, "lon"]
      
      # Bounding box filter
      lat_diff <- abs(many_coords[, "lat"] - origin_lat)
      lon_diff <- abs(many_coords[, "lon"] - origin_lon)
      keep_idx <- which(lat_diff <= max_radius_deg & lon_diff <= max_radius_deg)
      
      # Skip if no destinations in range
      if (length(keep_idx) == 0) {
        if (progress) {
          message(sprintf("Origin %d/%d (%.2f%%) — 0 destinations (skipped)",
                          i, n_origins, i / n_origins * 100))
        }
        next
      }
      
      # Use filtered destinations for this origin
      origin_many <- data.frame(
        lat = many_coords[keep_idx, "lat"],
        lon = many_coords[keep_idx, "lon"],
        stringsAsFactors = FALSE
      )
      origin_many[[many_id_col]] <- many_ids[keep_idx]
    } else {
      # No filtering, use all destinations
      origin_many <- many
    }
    
    if (progress) {
      n_dests <- if (spatial_filter) length(keep_idx) else length(many_ids)
      message(sprintf("Origin %d/%d (%.2f%%) — %s destinations",
                      i, n_origins, i / n_origins * 100,
                      format(n_dests, big.mark = ",")))
    }
    
    # Generate batch query for this origin
    origin_one <- data.frame(
      lat = one_coords[i, "lat"],
      lon = one_coords[i, "lon"],
      stringsAsFactors = FALSE
    )
    origin_one[[one_id_col]] <- one_ids[i]
    
    motis_one_to_many_generate_batch(
      one = origin_one,
      many = origin_many,
      output_file = output_file,
      mode = mode,
      arrive_by = arrive_by,
      max = max,
      maxMatchingDistance = maxMatchingDistance,
      one_id_col = one_id_col,
      many_id_col = many_id_col,
      ...,
      append = (i > 1),  # First iteration creates file, rest append
      api_endpoint = api_endpoint
    )
  }
  
  # Return file info
  meta_file <- paste0(output_file, ".meta")
  file_info <- list(
    file = output_file,
    n_lines = length(readLines(output_file)),
    file_size = file.size(output_file)
  )
  
  if (progress) {
    message(sprintf("✓ Generated %s query lines (%s bytes)",
                    format(file_info$n_lines, big.mark = ","),
                    format(file_info$file_size, big.mark = ",")))
  }
  
  invisible(file_info)
}
