#' Calculate one-to-many or many-to-one street-level routes
#'
#' This function is a user-friendly wrapper for the MOTIS `one-to-many` street
#' routing API. It computes travel time and distance from a single origin to
#' multiple destinations (or from multiple origins to a single destination)
#' using a specified travel mode (e.g., walking, cycling, or driving).
#'
#' @param one The single origin (when `arrive_by = FALSE`) or destination
#'   (when `arrive_by = TRUE`). Can be a character vector of an ID,
#'   a data frame/tibble with coordinate columns, an `sf` object with a
#'   single POINT geometry, or a numeric vector/matrix (`lon`, `lat`).
#' @param many The multiple destinations (when `arrive_by = FALSE`) or origins
#'   (when `arrive_by = TRUE`). Can be a character vector of IDs, a data
#'   frame/tibble with ID or coordinate columns, an `sf` object with POINT
#'   geometry, or a numeric matrix (`lon`, `lat`).
#' @param many_id_col The name of the column in `many` to use for identifying
#'   the points in the output. Defaults to `"id"`. If the input has no such
#'   column, a sequence of numbers is used.
#' @param mode The routing profile to use. Defaults to `"WALK"`.
#' @param arrive_by Logical. If `FALSE` (the default), calculates routes from
#'   `one` to `many`. If `TRUE`, calculates routes from `many` to `one`.
#' @param output The desired output format. One of:
#'   - `"data.frame"` (default): A tidy data frame with travel times and distances.
#'   - `"raw_list"`: The raw parsed JSON response as a list.
#' @inheritDotParams motis.client::mc_oneToMany -one -many -mode -arriveBy
#' @return Depending on the `output` parameter, a `data.frame` or a list.
#'   The data frame will contain columns for the identifier of each point in `many`,
#'   the `duration_seconds`, and the `distance_meters`.
#' @export
#' @importFrom httr2 req_perform resp_body_json
#' @importFrom purrr map_dfr
#' @importFrom rlang check_installed
motis_one_to_many <- function(
  one,
  many,
  many_id_col = "id",
  mode = c("WALK", "BIKE", "CAR"),
  arrive_by = FALSE,
  ...,
  output = c("data.frame", "raw_list")
) {
  # --- 1. Argument and Input Validation ---
  output <- match.arg(output)
  mode <- match.arg(mode)
  stopifnot(
    "'one' must be a single location" = NROW(one) == 1
  )

  # --- 2. Format Inputs ---
  # This endpoint uses a different format ("lat;lon") than the plan endpoint
  one_place <- .format_place_onemany(one)
  many_places_vec <- .format_place_onemany(many)
  many_places_str <- paste(many_places_vec, collapse = ",")

  # --- 3. Build Request ---
  # Capture extra arguments for the backend API call
  dots <- list(...)
  # Remove arguments that are part of this wrapper's signature
  dots[c("one", "many", "many_id_col", "mode", "arrive_by", "output")] <- NULL

  # Build the request object
  api_args <- c(
    list(
      one = one_place,
      many = many_places_str,
      mode = mode,
      arriveBy = arrive_by
    ),
    dots
  )
  req <- do.call(motis.client::mc_oneToMany, api_args)

  # dry run to catch errors early
  httr2::req_dry_run(req)

  # --- 4. Perform Request ---
  # This API endpoint is synchronous and doesn't require parallel execution
  resp <- httr2::req_perform(req)

  # --- 5. Process and Parse Response ---
  parsed_response <- httr2::resp_body_json(resp)

  if (output == "raw_list") {
    return(parsed_response)
  }

  # The response is an ordered list matching the input `many` locations
  results_df <- purrr::map_dfr(parsed_response, ~ as.data.frame(.x), .id = NULL)

  # Get identifiers from the original `many` input
  many_ids <- .get_ids(many, id_col = many_id_col)

  # Combine IDs with results
  final_df <- data.frame(
    many_id = many_ids
  )
  # Rename columns for clarity
  names(results_df) <- c("duration_seconds", "distance_meters")

  return(cbind(final_df, results_df))
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
    coords <- sf::st_coordinates(place)
    lat <- round(coords[, "Y"], 6)
    lon <- round(coords[, "X"], 6)
    return(paste(lat, lon, sep = ";"))
  }

  if (is.data.frame(place)) {
    p_names <- tolower(names(place))
    id_col_lower <- tolower(id_col)
    if (id_col_lower %in% p_names) {
      id_col_idx <- which(p_names == id_col_lower)
      return(as.character(place[[id_col_idx]]))
    }
    lat_col <- which(p_names %in% c("lat", "latitude"))
    lon_col <- which(p_names %in% c("lon", "lng", "longitude"))
    if (length(lat_col) == 1 && length(lon_col) == 1) {
      return(paste(place[[lat_col]], place[[lon_col]], sep = ";"))
    }
    stop(
      "Data frame must contain either an '",
      id_col,
      "' column or coordinate columns ('lat', 'lon').",
      call. = FALSE
    )
  }

  if (is.matrix(place) && is.numeric(place)) {
    if (ncol(place) != 2) {
      stop("Matrix must have 2 columns.", call. = FALSE)
    }
    cnames <- tolower(colnames(place))
    if (all(c("lon", "lat") %in% cnames)) {
      return(paste(place[, "lat"], place[, "lon"], sep = ";"))
    }
    # Fallback to assuming [lon, lat] order if no names
    return(paste(place[, 2], place[, 1], sep = ";"))
  }

  if (is.character(place)) {
    return(place)
  }

  stop("Unsupported input type for 'one'/'many'.", call. = FALSE)
}
