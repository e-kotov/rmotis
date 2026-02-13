#' Calculate reachable locations from a single point within a given travel time
#'
#' This function is a user-friendly wrapper for the MOTIS `one-to-all` API.
#' It computes the travel time to all reachable transit stops from a single
#' origin (or to a single destination from all reachable stops) within a
#' specified maximum travel time.
#'
#' @param one The single origin (when `arrive_by = FALSE`) or destination
#'   (when `arrive_by = TRUE`). Can be a character vector of an ID,
#'   a data frame/tibble with coordinate columns, an `sf` object with a
#'   single POINT geometry, or a numeric vector/matrix (`lon`, `lat`).
#' @param one_id_col The name of the column in `one` to use for identifying
#'   the point in the output. Defaults to `"id"`.
#' @param time The departure or arrival time. Can be a POSIXct object (like from
#'   `Sys.time()`) or a character string in ISO 8601 format (e.g., "2025-08-15T15:11:00Z").
#'   Defaults to the current time.
#' @param arrive_by Logical. If `FALSE` (the default), calculates routes from
#'   `one` to all reachable stops. If `TRUE`, calculates routes from all
#'   reachable stops to `one`.
#' @param max_travel_time Integer. The maximum travel time in minutes. Defaults to 120.
#' @param output The desired output format. One of:
#'   - `"data.frame"` (default): A tidy data frame with travel times and transfers.
#'   - `"raw_list"`: The raw parsed JSON response as a list.
#' @inheritDotParams motis.client::mc_oneToAll -one -time -maxTravelTime -arriveBy -.endpoint
#' @return Depending on the `output` parameter, a `data.frame` or a list.
#'   The data frame will contain columns for the identifier of the `one` point,
#'   the `target_id` of the reachable stop, the `duration_seconds`, and the
#'   number of `transfers`.
#' @export
#' @importFrom httr2 req_perform resp_body_json
#' @importFrom dplyr bind_rows
#' @importFrom rlang check_installed
motis_one_to_all <- function(
  one,
  one_id_col = "id",
  time = Sys.time(),
  arrive_by = FALSE,
  max_travel_time = 90,
  ...,
  output = c("data.frame", "raw_list")
) {
  # --- 1. Argument and Input Validation ---
  output <- match.arg(output)
  time <- as.POSIXct(time)
  stopifnot("'one' must be a single location" = NROW(one) == 1)

  # --- 2. Format Inputs ---
  one_place <- .format_place(one)
  time_str <- .format_time_utc(time)

  # --- 3. Build Request ---
  dots <- list(...)
  user_server <- dots[[".server"]]
  dots[c("one", "one_id_col", "time", "arrive_by", "max_travel_time", "output", ".server")] <- NULL

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
      time = unname(time_str),
      maxTravelTime = unname(max_travel_time),
      arriveBy = unname(arrive_by),
      .server = unname(user_server %||% .get_server_url())
    ),
    dots
  )
  
  req <- tryCatch({
    do.call(motis.client::mc_oneToAll, api_args)
  }, error = function(e) {
    stop("Failed to construct one-to-all request: ", e$message, call. = FALSE)
  })

  # --- 4. Perform Request ---
  resp <- tryCatch({
    httr2::req_perform(req)
  }, error = function(e) {
    stop("MOTIS one-to-all request failed: ", e$message, call. = FALSE)
  })

  # --- 5. Process and Parse Response ---
  parsed_response <- httr2::resp_body_json(resp)

  if (output == "raw_list") return(parsed_response)

  # Standard MOTIS one-to-all response structure is a list with "one" and "all" keys
  if (is.list(parsed_response) && !is.null(parsed_response$all)) {
    all_reachable <- parsed_response$all
  } else {
    # Fallback if it returns a flat list
    all_reachable <- parsed_response
  }

  if (length(all_reachable) == 0) {
    return(data.frame(
      one_id = .get_ids(one, id_col = one_id_col),
      target_id = character(0),
      duration_seconds = numeric(0),
      transfers = integer(0)
    ))
  }

  one_id <- .get_ids(one, id_col = one_id_col)

  # Convert each ReachablePlace to a data frame row
  res_list <- lapply(all_reachable, function(item) {
    if (!is.list(item)) return(NULL)
    data.frame(
      one_id = one_id,
      target_id = as.character(item$place$stopId %||% item$place$name %||% NA_character_),
      duration_seconds = as.numeric(item$duration %||% NA_real_),
      transfers = as.integer(item$k %||% NA_integer_) - 1L # k=1 is direct, so transfers = k-1
    )
  })
  
  dplyr::bind_rows(res_list)
}

#' Generate MOTIS Batch Query File for One-to-All
#'
#' Efficiently constructs a text file of MOTIS one-to-all queries for batch
#' processing.
#'
#' @param one The origin location (when `arrive_by = FALSE`) or destination
#'   (when `arrive_by = TRUE`).
#' @param output_file The path to the output text file.
#' @param time The departure or arrival time. Defaults to `Sys.time()`.
#' @param max_travel_time Integer. The maximum travel time in minutes. Defaults to 90.
#' @param arrive_by Logical. If `FALSE` (the default), calculates routes from
#'   `one` to all reachable stops. If `TRUE`, calculates routes from all
#'   reachable stops to `one`.
#' @param ... Additional MOTIS API parameters.
#' @param append Logical. If `TRUE`, appends to `output_file`.
#' @param api_endpoint The API path. Defaults to `"/api/v1/one-to-all"`.
#'
#' @return Invisibly returns the number of queries written.
#' @export
motis_one_to_all_generate_batch <- function(
  one,
  output_file,
  time = Sys.time(),
  max_travel_time = 90,
  arrive_by = FALSE,
  ...,
  append = FALSE,
  api_endpoint = "/api/v1/one-to-all"
) {
  if (missing(output_file) || !is.character(output_file) || length(output_file) != 1) {
    stop("`output_file` must be a single string specifying the file path.", call. = FALSE)
  }

  dots <- .collapse_dots(list(...))
  
  one_place <- .format_place(one)
  
  time_vec <- .format_time_utc(time)

  # Validation via motis.client
  tryCatch({
    .validate_batch_params(dots)
    do.call(motis.client::mc_oneToAll, c(
      list(
        one = one_place[1],
        time = time_vec[1],
        maxTravelTime = max_travel_time,
        arriveBy = arrive_by,
        .build_only = TRUE,
        .server = "http://localhost:8080"
      ),
      dots
    ))
  }, error = function(e) {
    stop("Invalid MOTIS API parameters: ", e$message, call. = FALSE)
  })

  # For one-to-all, we might have multiple 'one' locations if we wanted to 
  # vectorize over them, similar to motis_plan_generate_batch.
  # Let's support multiple 'one' and 'time' inputs.

  n_queries <- max(length(one_place), length(time_vec))
  if (length(one_place) == 1) one_place <- rep(one_place, n_queries)
  if (length(time_vec) == 1) time_vec <- rep(time_vec, n_queries)

  if (length(one_place) != n_queries || length(time_vec) != n_queries) {
    stop("'one' and 'time' must have compatible lengths.", call. = FALSE)
  }

  static_params <- c(
    list(
      maxTravelTime = max_travel_time,
      arriveBy = arrive_by
    ),
    dots
  )
  static_suffix <- .build_static_suffix(static_params)

  # Vectorised generation
  one_enc <- curl::curl_escape(one_place)

  lines <- paste0(
    api_endpoint,
    "?one=", one_enc,
    "&time=", time_vec,
    static_suffix
  )

  con <- file(output_file, open = if (isTRUE(append)) "a" else "w")
  on.exit(close(con))
  writeLines(lines, con = con)

  message("Successfully wrote ", n_queries, " query(ies) to '", output_file, "'.")
  invisible(n_queries)
}
