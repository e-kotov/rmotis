# R/motis_one_to_many_txt_1.R

# --- Prerequisite: Helper to format location inputs for one-to-many ---
# This endpoint requires a "latitude;longitude" format.
#' @importFrom rlang check_installed
.format_place_onemany <- function(place) {
  if (inherits(place, "sf")) {
    rlang::check_installed("sf")
    coords <- sf::st_coordinates(place)
    lat <- round(coords[, "Y"], 6)
    lon <- round(coords[, "X"], 6)
    return(paste(lat, lon, sep = ";"))
  }

  if (is.data.frame(place)) {
    p_names <- tolower(names(place))
    lat_col <- which(p_names %in% c("lat", "latitude"))
    lon_col <- which(p_names %in% c("lon", "lng", "longitude"))
    if (length(lat_col) == 1 && length(lon_col) == 1) {
      return(paste(place[[lat_col]], place[[lon_col]], sep = ";"))
    }
    stop(
      "Data frame must contain coordinate columns ('lat', 'lon').",
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
    # Assuming character is already in "lat;lon" format or is an ID
    # The one-to-many API seems to handle station IDs directly here
    return(place)
  }

  stop("Unsupported input type for 'one'/'many'.", call. = FALSE)
}


# --- Prerequisite: Helper to build a URL query string from a list ---
#' @importFrom purrr compact map2_chr
#' @importFrom curl curl_escape
.build_query_string <- function(params) {
  # Remove any NULL elements from the list
  params <- purrr::compact(params)
  if (length(params) == 0) {
    return("")
  }

  # Process each key-value pair
  query_parts <- purrr::map2_chr(
    names(params),
    params,
    ~ {
      key <- .x
      val <- .y

      # Convert logicals to uppercase strings (e.g., TRUE -> "TRUE")
      if (is.logical(val)) {
        val <- toupper(as.character(val))
      }

      # Collapse vectors/lists into a single comma-separated string
      if (length(val) > 1) {
        val <- paste(val, collapse = ",")
      }

      # URL-encode the key and the final value and combine them
      paste0(curl::curl_escape(key), "=", curl::curl_escape(as.character(val)))
    }
  )

  # Join all parts with '&' and prepend '?'
  paste0("?", paste(query_parts, collapse = "&"))
}


# --- The New motis_one_to_many_txt_1 Function ---

#' Builds a MOTIS one-to-many request manually and saves it to a text file.
#'
#' This function efficiently constructs the API request for a one-to-many or
#' many-to-one query by building the URL string manually. It writes the
#' resulting URL (path and query string) to a specified text file. This is
#' useful for scenarios where requests are generated in one step and executed
#' later, for example with a bulk processing tool.
#'
#' @param one The single origin (when `arrive_by = FALSE`) or destination
#'   (when `arrive_by = TRUE`). Can be a character string of an ID,
#'   a data frame/tibble with one row and coordinate columns, an `sf` object with a
#'   single POINT geometry, or a numeric vector/matrix (`lon`, `lat`).
#' @param many The multiple destinations (when `arrive_by = FALSE`) or origins
#'   (when `arrive_by = TRUE`). Can be a character vector of IDs, a data
#'   frame/tibble with ID or coordinate columns, an `sf` object with POINT
#'   geometry, or a numeric matrix (`lon`, `lat`).
#' @param output_file The path to the output text file where the request URL
#'   will be written.
#' @param mode The routing profile to use. Defaults to `"WALK"`.
#' @param arrive_by Logical. If `FALSE` (the default), calculates routes from
#'   `one` to `many`. If `TRUE`, calculates routes from `many` to `one`.
#' @param ... Additional parameters to be passed to the MOTIS One-to-Many API,
#'   (e.g., `max`, `maxMatchingDistance`).
#' @return Invisibly returns a character vector containing the generated request
#'   string. The primary side effect is writing this string to `output_file`.
#' @export
#' @examples
#' \dontrun{
#' # --- Example Usage ---
#'
#' # Define a single origin point and multiple destination points
#' origin_point <- data.frame(lat = 52.525, lon = 13.369) # Berlin Hbf
#'
#' destination_points <- data.frame(
#'   name = c("Brandenburg Gate", "Reichstag Building"),
#'   lat = c(52.516, 52.518),
#'   lon = c(13.377, 13.376)
#' )
#'
#' # --- Generate a request to find walking times from the origin to destinations ---
#' motis_one_to_many_txt_1(
#'   one = origin_point,
#'   many = destination_points,
#'   output_file = "onemany_request.txt",
#'   mode = "WALK",
#'   max = 3600 # Max travel time of 1 hour in seconds
#' )
#'
#' # The file "onemany_request.txt" will contain a single line like:
#' # api/v1/one-to-many?one=52.525;13.369&many=52.516;13.377,52.518;13.376&...
#'
#' }
motis_one_to_many_txt_1 <- function(
  one,
  many,
  output_file,
  mode = c("WALK", "BIKE", "CAR"),
  arrive_by = FALSE,
  max = 7200, # 2 hours in seconds
  maxMatchingDistance = 1000, # 1 km
  ...
) {
  # --- 1. Argument Validation ---
  if (
    missing(output_file) ||
      !is.character(output_file) ||
      length(output_file) != 1
  ) {
    stop(
      "`output_file` must be a single string specifying the file path.",
      call. = FALSE
    )
  }
  mode <- match.arg(mode)
  stopifnot(
    "'one' must be a single location" = NROW(one) == 1
  )

  # --- 2. Prepare Parameters ---
  # Capture all additional API arguments passed via '...'
  additional_params <- list(...)

  # Format the 'one' and 'many' locations into the required "lat;lon" strings
  one_place <- .format_place_onemany(one)
  many_places_vec <- .format_place_onemany(many)

  # The API requires the 'many' parameter to be a single, comma-separated string
  many_places_str <- paste(many_places_vec, collapse = ",")

  # --- 3. Build Request String Manually ---
  base_path <- "api/v1/one-to-many"

  # Combine static and dynamic parameters for the request
  all_params <- c(
    list(
      one = one_place,
      many = many_places_str,
      mode = mode,
      arriveBy = arrive_by,
      max = max,
      maxMatchingDistance = maxMatchingDistance
    ),
    additional_params
  )

  # Build the query string part (e.g., "?key=val&...")
  query_string <- .build_query_string(all_params)

  # Combine the base path and the query string
  request_string <- paste0(base_path, query_string)

  # --- 4. Write to File and Return ---
  writeLines(request_string, con = output_file)
  message(
    "Successfully wrote 1 request to '",
    output_file,
    "'."
  )

  invisible(request_string)
}
