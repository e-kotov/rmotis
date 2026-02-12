# --- Prerequisite: Helper to format location inputs ---
# This helper is used to process the 'one' argument.
#' @noRd
.format_place <- function(place, id_col = "id") {
  if (inherits(place, "sf")) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("Package 'sf' must be installed to use sf objects.", call. = FALSE)
    }
    coords <- sf::st_coordinates(place)
    lat <- round(coords[, "Y"], 6)
    lon <- round(coords[, "X"], 6)
    return(paste(lat, lon, sep = ","))
  }
  if (is.data.frame(place)) {
    p_names <- tolower(names(place))
    lat_col <- which(p_names %in% c("lat", "latitude"))
    lon_col <- which(p_names %in% c("lon", "lng", "longitude"))
    if (length(lat_col) == 1 && length(lon_col) == 1) {
      lat <- round(place[[lat_col]], 6)
      lon <- round(place[[lon_col]], 6)
      return(paste(lat, lon, sep = ","))
    }
    id_col_lower <- tolower(id_col)
    if (id_col_lower %in% p_names) {
      id_col_idx <- which(p_names == id_col_lower)
      return(as.character(place[[id_col_idx]]))
    }
    stop(
      "Data frame must contain either coordinate columns ('lat', 'lon') or an '",
      id_col,
      "' column.",
      call. = FALSE
    )
  }
  if (is.matrix(place) && is.numeric(place)) {
    if (ncol(place) != 2) {
      stop("Matrix must have 2 columns.", call. = FALSE)
    }
    cnames <- tolower(colnames(place))
    if (all(c("lon", "lat") %in% cnames)) {
      lat <- round(place[, "lat"], 6)
      lon <- round(place[, "lon"], 6)
      return(paste(lat, lon, sep = ","))
    }
    lat <- round(place[, 2], 6)
    lon <- round(place[, 1], 6)
    return(paste(lat, lon, sep = ","))
  }
  if (is.character(place)) {
    return(place)
  }
  stop("Unsupported input type for 'one'.", call. = FALSE)
}


# --- Prerequisite: Helper to build a URL query string from a list ---
#' @importFrom purrr compact map2_chr
#' @importFrom curl curl_escape
#' @noRd
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


#' Builds a MOTIS one-to-all request manually and saves it to a text file.
#'
#' This function efficiently constructs the API request for a one-to-all
#' query by building the URL string manually. It writes the resulting URL
#' (path and query string) to a specified text file. This is useful for
#' scenarios where requests are generated in one step and executed later,
#' for example with a bulk processing tool.
#'
#' @param one The single origin (when `arrive_by = FALSE`) or destination
#'   (when `arrive_by = TRUE`).
#' @param output_file The path to the output text file where the request URL
#'   will be written.
#' @param time The departure or arrival time. Defaults to `Sys.time()`.
#' @param arrive_by Logical. If `FALSE` (the default), calculates routes from
#'   `one`. If `TRUE`, calculates routes to `one`.
#' @param max_travel_time Integer. The maximum travel time in minutes. Defaults to 120.
#' @param ... Additional parameters to be passed to the MOTIS One-to-All API,
#'   (e.g., `maxTransfers`, `transitModes`).
#' @return Invisibly returns a character vector containing the generated request
#'   string. The primary side effect is writing this string to `output_file`.
#' @export
motis_one_to_all_txt_1 <- function(
  one,
  output_file,
  time = Sys.time(),
  arrive_by = FALSE,
  max_travel_time = 90,
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
  stopifnot(
    "'one' must be a single location" = NROW(one) == 1
  )

  # --- 2. Prepare Parameters ---
  time_str <- paste0(
    format(as.POSIXct(time), "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    "Z"
  )
  additional_params <- list(...)
  one_place <- .format_place(one)

  # --- 3. Build Request String Manually ---
  base_path <- "api/v1/one-to-all"

  # Combine static and dynamic parameters for the request
  all_params <- c(
    list(
      one = one_place,
      time = time_str,
      maxTravelTime = max_travel_time,
      arriveBy = arrive_by
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
