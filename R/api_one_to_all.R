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
#' @inheritDotParams motis.client::mc_oneToAll -one -time -maxTravelTime -arriveBy
#' @return Depending on the `output` parameter, a `data.frame` or a list.
#'   The data frame will contain columns for the identifier of the `one` point,
#'   the `target_id` of the reachable stop, the `duration_seconds`, and the
#'   number of `transfers`.
#' @export
#' @importFrom httr2 req_perform resp_body_json
#' @importFrom purrr map_dfr
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
  stopifnot(
    "'one' must be a single location" = NROW(one) == 1
  )

  # --- 2. Format Inputs ---
  one_place <- .format_place(one)
  time_str <- paste0(format(time, "%Y-%m-%dT%H:%M:%S", tz = "UTC"), "Z")

  # --- 3. Build Request ---
  # Capture extra arguments for the backend API call
  dots <- list(...)
  dots[c(
    "one",
    "one_id_col",
    "time",
    "arrive_by",
    "max_travel_time",
    "output"
  )] <- NULL

  # Build the request object
  api_args <- c(
    list(
      one = one_place,
      time = time_str,
      maxTravelTime = max_travel_time,
      arriveBy = arrive_by
    ),
    dots
  )
  req <- do.call(motis.client::mc_oneToAll, api_args)

  # dry run to catch errors early
  httr2::req_dry_run(req)

  # --- 4. Perform Request ---
  resp <- httr2::req_perform(req)

  # --- 5. Process and Parse Response ---
  parsed_response <- httr2::resp_body_json(resp)

  if (output == "raw_list") {
    return(parsed_response)
  }

  # The response is a list of results, each with a target, duration, and transfers
  results_df <- purrr::map_dfr(parsed_response, as.data.frame)

  if (nrow(results_df) == 0) {
    message("No reachable locations found within the specified travel time.")
    return(data.frame(
      one_id = .get_ids(one, id_col = one_id_col),
      target_id = character(0),
      duration_seconds = numeric(0),
      transfers = integer(0)
    ))
  }

  one_id <- .get_ids(one, id_col = one_id_col)

  # Combine IDs with results and rename columns for consistency
  final_df <- data.frame(
    one_id = one_id,
    target_id = results_df$target,
    duration_seconds = results_df$duration,
    transfers = results_df$transfers
  )

  return(final_df)
}
