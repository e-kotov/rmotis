# --- Prerequisite: Helper to format location inputs ---
# This function is still needed to process the 'from' and 'to' arguments.
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
  stop("Unsupported input type for 'from'/'to'.", call. = FALSE)
}


# --- NEW: Internal helper to build a URL query string from a list ---
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
      paste0(curl::curl_escape(key), "=", curl::curl_escape(val))
    }
  )

  # Join all parts with '&' and prepend '?'
  paste0("?", paste(query_parts, collapse = "&"))
}


# --- The New, Faster motis_plan_txt_1 Function ---

#' Builds MOTIS plan requests manually and dumps them to a text file.
#'
#' This function efficiently constructs API requests for planning paired or
#' matrix journeys by building the URL strings manually, avoiding the overhead
#' of `httr2`. It writes the resulting URLs (path and query string) to a
#' specified text file, with each request on a new line.
#'
#' @param from The origin location(s).
#' @param to The destination location(s).
#' @param output_file The path to the output text file.
#' @param time The departure/arrival time. Defaults to `Sys.time()`.
#' @param arrive_by If `TRUE`, `time` is arrival time. Defaults to `FALSE`.
#' @param from_id_col Name of the ID column in `from`. Defaults to `"id"`.
#' @param to_id_col Name of the ID column in `to`. Defaults to `"id"`.
#' @param output The mode of operation: `"itineraries"` for paired journeys or
#'   `"travel_time_matrix_long"` for a full matrix.
#' @param ... Additional parameters to be passed to the MOTIS Plan API,
#'   (e.g., `maxTransfers`, `transitModes`).
#' @return Invisibly returns a character vector of the generated request strings.
#'   The primary side effect is writing these strings to `output_file`.
#' @export
#' @examples
#' \dontrun{
#' # --- Example Usage ---
#'
#' # Define some origin and destination points
#' origins <- data.frame(
#'   id = c("berlin_hbf", "hamburg_hbf"),
#'   lat = c(52.525, 53.552),
#'   lon = c(13.369, 10.006)
#' )
#'
#' destinations <- data.frame(
#'   name = c("Munich", "Frankfurt"),
#'   lat = c(48.140, 50.107),
#'   lon = c(11.560, 8.662)
#' )
#'
#' # --- 1. Paired Journeys (Berlin -> Munich, Hamburg -> Frankfurt) ---
#' motis_plan_txt_1(
#'   from = origins,
#'   to = destinations,
#'   output_file = "paired_requests.txt",
#'   output = "itineraries",
#'   transitModes = c("RAIL", "COACH"), # Example of a vector parameter
#'   maxTransfers = 1
#' )
#'
#' # --- 2. Travel Time Matrix (all origins to all destinations) ---
#' motis_plan_txt_1(
#'   from = origins,
#'   to = destinations,
#'   output_file = "matrix_requests.txt",
#'   output = "travel_time_matrix_long",
#'   maxTravelTime = 300 # 5 hours
#' )
#'
#' }
motis_plan_txt_1 <- function(
  from,
  to,
  output_file,
  time = Sys.time(),
  arrive_by = FALSE,
  from_id_col = "id",
  to_id_col = "id",
  ...,
  output = c(
    "itineraries",
    "travel_time_matrix_long"
  )
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
  output <- match.arg(output)

  # --- 2. Prepare Parameters ---
  time_str <- paste0(
    format(as.POSIXct(time), "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    "Z"
  )

  # Capture all additional API arguments passed via '...'
  additional_params <- list(...)

  # Format the 'from' and 'to' locations into character strings
  from_place <- .format_place(from, id_col = from_id_col)
  to_place <- .format_place(to, id_col = to_id_col)

  # --- 3. Generate From/To Combinations ---
  is_matrix_mode <- output == "travel_time_matrix_long"

  if (is_matrix_mode) {
    # MANY-TO-MANY: Create all combinations of from and to
    combinations <- expand.grid(
      from = from_place,
      to = to_place,
      stringsAsFactors = FALSE
    )
  } else {
    # ONE-TO-ONE: Check for equal length and create paired combinations
    if (length(from_place) != length(to_place)) {
      stop(
        "For 'itineraries' output, 'from' and 'to' must have the same number of elements.",
        call. = FALSE
      )
    }
    combinations <- data.frame(from = from_place, to = to_place)
  }

  # --- 4. Build Request Strings Manually ---
  base_path <- "api/v4/plan"

  request_strings <- purrr::map2_chr(
    combinations$from,
    combinations$to,
    ~ {
      # Combine static and dynamic parameters for this specific request
      all_params <- c(
        list(
          fromPlace = .x,
          toPlace = .y,
          time = time_str,
          arriveBy = arrive_by
        ),
        additional_params
      )

      # Build the query string part (e.g., "?key=val&...")
      query_string <- .build_query_string(all_params)

      # Combine the base path and the query string
      paste0(base_path, query_string)
    }
  )

  # --- 5. Write to File and Return ---
  writeLines(request_strings, con = output_file)
  message(
    "Successfully wrote ",
    length(request_strings),
    " request(s) to '",
    output_file,
    "'."
  )

  invisible(request_strings)
}
