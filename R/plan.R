#' Plan a journey between two points
#'
#' This function is a user-friendly wrapper around the MOTIS `plan` API.
#' It accepts various common R data formats for locations, can plan
#' multiple journeys at once, and can execute requests in parallel.
#'
#' @param from The origin location(s). Can be a character vector of station IDs,
#'   a data frame/tibble with ID or coordinate columns, an `sf`
#'   object with POINT geometry, or a numeric matrix (`lon`, `lat`).
#' @param to The destination location(s). Must be of the same type and length as `from`.
#' @param time The departure or arrival time. Can be a POSIXct object (like from
#'   `Sys.time()`) or a character string in ISO 8601 format (e.g., "2025-08-15T15:11:00Z").
#'   Defaults to the current time.
#' @param arrive_by Logical. If `TRUE`, `time` is treated as the arrival time.
#'   Defaults to `FALSE` (departure time).
#' @param from_id_col The name of the column in `from` containing station IDs.
#'   Defaults to `"id"`.
#' @param to_id_col The name of the column in `to` containing station IDs.
#'   Defaults to `"id"`.
#' @param output The desired output format. One of:
#'   - `"itineraries"` (default): An `sf` data frame of itineraries.
#'   - `"legs"`: An `sf` data frame of individual journey legs.
#'   - `"travel_time_matrix_long"`: A long-format data frame with travel times.
#'     For large queries, using `[motis_table()]` is recommended.
#'   - `"travel_time_matrix_wide"`: A wide-format data frame (matrix) with travel times.
#'     For large queries, using `[motis_table()]` is recommended.
#'   - `"raw_list"`: The raw parsed JSON response as a list.
#' @param parallel Logical. If `TRUE`, executes multiple requests in parallel.
#'   Defaults to `FALSE`.
#' @inheritDotParams motis.client::mc_plan -fromPlace -toPlace -time -arriveBy
#' @return Depending on the `output` parameter, an `sf` data frame, a regular
#'   data frame, or a list.
#' @export
#' @importFrom httr2 req_perform_parallel req_perform_sequential resp_body_json
#' @importFrom purrr map map2 list_rbind map_dbl
#' @importFrom rlang check_installed
#' @importFrom tidyr pivot_wider
motis_plan <- function(
  from,
  to,
  time = Sys.time(),
  arrive_by = FALSE,
  from_id_col = "id",
  to_id_col = "id",
  ...,
  output = c(
    "itineraries",
    "legs",
    "travel_time_matrix_long",
    "travel_time_matrix_wide",
    "raw_list"
  ),
  parallel = FALSE
) {
  # --- 1. Argument and Input Validation ---
  output <- match.arg(output)
  stopifnot(
    "Length of 'from' and 'to' must be equal" = NROW(from) == NROW(to)
  )

  time <- as.POSIXct(time)

  # --- 2. Format Inputs using the new helper ---
  from_place <- .format_place(from, id_col = from_id_col)
  to_place <- .format_place(to, id_col = to_id_col)

  # --- 3. Build Requests ---
  time_str <- paste0(format(time, "%Y-%m-%dT%H:%M:%S", tz = "UTC"), "Z")

  # --- FIX: Safely handle ... to avoid passing unintended arguments ---
  # Capture extra arguments intended for the backend API call
  dots <- list(...)
  # Remove arguments that are part of this wrapper's signature to avoid passing them down
  dots[c(
    "from",
    "to",
    "time",
    "arrive_by",
    "from_id_col",
    "to_id_col",
    "output",
    "parallel"
  )] <- NULL

  requests <- purrr::map2(
    from_place,
    to_place,
    ~ {
      # Combine the core arguments with the cleaned extra arguments
      api_args <- c(
        list(
          fromPlace = .x,
          toPlace = .y,
          time = time_str,
          arriveBy = arrive_by,
          .build_only = TRUE
        ),
        dots
      )
      # Use do.call to safely pass the arguments to the client function
      do.call(motis.client::mc_plan, api_args)
    }
  )

  # --- 4. Perform Requests ---
  responses <- if (isTRUE(parallel)) {
    httr2::req_perform_parallel(requests)
  } else {
    httr2::req_perform_sequential(requests)
  }

  # --- 5. Process and Parse Responses ---
  parsed_responses <- purrr::map(responses, httr2::resp_body_json)

  if (output == "raw_list") {
    return(parsed_responses)
  }

  names(parsed_responses) <- seq_along(parsed_responses)

  if (output == "itineraries") {
    itineraries <- purrr::list_rbind(
      purrr::map(parsed_responses, .flatten_itineraries, include_direct = TRUE),
      names_to = "request_id"
    )
    return(itineraries)
  } else if (output == "legs") {
    legs <- purrr::list_rbind(
      purrr::map(
        parsed_responses,
        .flatten_legs,
        decode_geom = TRUE,
        include_direct = TRUE
      ),
      names_to = "request_id"
    )
    return(legs)
  } else if (
    output %in% c("travel_time_matrix_long", "travel_time_matrix_wide")
  ) {
    # Add warning for large number of requests
    if (NROW(from) > 10) {
      warning(
        "For large travel time matrices, using the dedicated `motis_table()` function is much more efficient.",
        call. = FALSE
      )
    }

    from_ids <- .get_ids(from, id_col = from_id_col)
    to_ids <- .get_ids(to, id_col = to_id_col)

    min_durations <- purrr::map_dbl(parsed_responses, function(resp) {
      if (length(resp$journeys) == 0) {
        return(NA_real_)
      }
      durations <- purrr::map_dbl(resp$journeys, "duration")
      min(durations, na.rm = TRUE) / 60
    })

    long_matrix <- data.frame(
      from_id = from_ids,
      to_id = to_ids,
      duration_minutes = min_durations
    )

    if (output == "travel_time_matrix_long") {
      return(long_matrix)
    } else {
      # travel_time_matrix_wide
      return(tidyr::pivot_wider(
        long_matrix,
        names_from = "to_id",
        values_from = "duration_minutes"
      ))
    }
  }
}

#' Internal helper to format location inputs
#' @param place A data.frame, sf object, matrix, or character vector.
#' @param id_col The name of the ID column to use.
#' @return A character vector of station IDs or "lat,lon" strings.
#' @noRd
.format_place <- function(place, id_col = "id") {
  if (inherits(place, "sf")) {
    rlang::check_installed("sf")
    coords <- sf::st_coordinates(place)
    # --- FIX: Round coordinates to 6 decimal places to avoid server-side parsing errors ---
    lat <- round(coords[, "Y"], 6)
    lon <- round(coords[, "X"], 6)
    return(paste(lat, lon, sep = ","))
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
      return(paste(place[[lat_col]], place[[lon_col]], sep = ","))
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
      return(paste(place[, "lat"], place[, "lon"], sep = ","))
    }
    # Fallback to assuming [lon, lat] order if no names
    return(paste(place[, 2], place[, 1], sep = ","))
  }

  if (is.character(place)) {
    return(place)
  }

  stop("Unsupported input type for 'from'/'to'.", call. = FALSE)
}

#' Internal helper to extract IDs from various inputs
#' @param place A data.frame, sf object, matrix, or character vector.
#' @param id_col The name of the ID column to use.
#' @return A vector of IDs.
#' @noRd
.get_ids <- function(place, id_col) {
  if (is.data.frame(place) && id_col %in% names(place)) {
    return(place[[id_col]])
  }
  if (is.character(place)) {
    return(place)
  }
  # For matrices or sf objects without an ID col, create a sequence
  return(seq_len(NROW(place)))
}
