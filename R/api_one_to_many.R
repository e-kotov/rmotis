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
#'   The data frame will contain columns for the identifier of each point in `many`,
#'   the `duration_seconds`, and the `distance_meters`.
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
  many_places_str <- paste(many_places_vec, collapse = ",")

  # --- 3. Build Request ---
  dots <- list(...)
  user_server <- dots[[".server"]]
  dots[c("one", "many", "many_id_col", "mode", "arrive_by", "output", ".server")] <- NULL

  # Collapse any vector arguments in dots to comma-separated strings
  dots <- lapply(dots, function(x) {
    if (length(x) > 1 && is.atomic(x)) {
      paste(unname(x), collapse = ",")
    } else if (is.atomic(x)) {
      unname(x)
    } else {
      x
    }
  })

  api_args <- c(
    list(
      one = unname(one_place),
      many = unname(many_places_str),
      mode = unname(mode),
      arriveBy = unname(arrive_by),
      max = unname(max),
      maxMatchingDistance = unname(maxMatchingDistance),
      .server = unname(user_server %||% .get_server_url())
    ),
    dots
  )
  
  req <- tryCatch({
    do.call(motis.client::mc_oneToMany, api_args)
  }, error = function(e) {
    stop("Failed to construct one-to-many request: ", e$message, call. = FALSE)
  })

  # --- 4. Perform Request ---
  resp <- tryCatch({
    httr2::req_perform(req)
  }, error = function(e) {
    stop("MOTIS one-to-many request failed: ", e$message, call. = FALSE)
  })

  # --- 5. Process and Parse Response ---
  parsed_response <- httr2::resp_body_json(resp)

  if (output == "raw_list") return(parsed_response)

  if (length(parsed_response) == 0) {
    return(data.frame(
      one_id = .get_ids(one, id_col = one_id_col),
      many_id = .get_ids(many, id_col = many_id_col),
      duration = numeric(0)
    ))
  }

  one_id <- .get_ids(one, id_col = one_id_col)
  many_ids <- .get_ids(many, id_col = many_id_col)

  # Process responses while ensuring alignment with input many_ids
  res_list <- lapply(seq_along(parsed_response), function(i) {
    item <- parsed_response[[i]]
    if (length(item) == 0) {
      return(data.frame(one_id = one_id, many_id = many_ids[i], duration = NA_real_))
    }
    df <- as.data.frame(item)
    df$one_id <- one_id
    df$many_id <- many_ids[i]
    df
  })

  dplyr::bind_rows(res_list)
}

#' Generate MOTIS Batch Query File for One-to-Many
#'
#' Efficiently constructs a text file of MOTIS one-to-many street routing
#' queries for batch processing.
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
#' @param ... Additional MOTIS API parameters.
#' @param append Logical. If `TRUE`, appends to `output_file`.
#' @param api_endpoint The API path. Defaults to `"/api/v1/one-to-many"`.
#'
#' @return Invisibly returns the number of queries written (always 1 for this endpoint, but vectorized over many).
#' @export
motis_one_to_many_generate_batch <- function(
  one,
  many,
  output_file,
  mode = c("WALK", "BIKE", "CAR"),
  arrive_by = FALSE,
  max = 7200,
  maxMatchingDistance = 1000,
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
  
  # For one-to-many, we usually generate ONE line that contains ALL 'many' locations
  # unless we wanted to split it, but the endpoint supports many-at-once.
  # So we build the query string.
  
  query_str <- paste0(
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

  con <- file(output_file, open = if (isTRUE(append)) "a" else "w")
  on.exit(close(con))
  writeLines(query_str, con = con)

  message("Successfully wrote one-to-many batch query to '", output_file, "'.")
  invisible(1L)
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
    lat <- round(coords[, "Y"], 6); lon <- round(coords[, "X"], 6)
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
    stop("Data frame must contain an '", id_col, "' column or coordinate columns.", call. = FALSE)
  }

  if (is.matrix(place) && is.numeric(place)) {
    if (ncol(place) != 2) stop("Matrix must have 2 columns.", call. = FALSE)
    cnames <- tolower(colnames(place))
    if (all(c("lon", "lat") %in% cnames)) {
      return(paste(place[, "lat"], place[, "lon"], sep = ";"))
    }
    return(paste(place[, 2], place[, 1], sep = ";"))
  }

  if (is.character(place)) return(place)
  stop("Unsupported input type.", call. = FALSE)
}
