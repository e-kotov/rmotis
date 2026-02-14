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
