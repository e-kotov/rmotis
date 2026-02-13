#' Plan a journey between two points or create a travel time matrix
#'
#' This function is a user-friendly wrapper around the MOTIS `plan` API.
#' It can plan paired journeys (`from[1]` to `to[1]`, etc.) or compute a
#' full travel time matrix (from all origins to all destinations).
#'
#' @param from The origin location(s). Can be a character vector of station IDs,
#'   a data frame/tibble with ID or coordinate columns, an `sf`
#'   object with POINT geometry, or a numeric matrix (`lon`, `lat`).
#' @param to The destination location(s). Must be of the same type as `from`.
#'   For paired journey planning, must be the same length as `from`. For
#'   travel time matrix calculation, length can be different.
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
#'   - `"itineraries"` (default): An `sf` data frame of paired itineraries.
#'   - `"legs"`: An `sf` data frame of individual journey legs for paired journeys.
#'   - `"travel_time_matrix_long"`: A long-format data frame with travel times
#'     from all origins to all destinations.
#'   - `"travel_time_matrix_wide"`: A wide-format data frame (matrix) with
#'     travel times from all origins to all destinations.
#'   - `"raw_list"`: The raw parsed JSON response as a list.
#' @param parallel Logical. If `TRUE`, executes multiple requests in parallel.
#'   Defaults to `FALSE`.
#' @inheritDotParams motis.client::mc_plan -fromPlace -toPlace -time -arriveBy
#' @return Depending on the `output` parameter, an `sf` data frame, a regular
#'   data frame, or a list.
#' @export
#' @importFrom httr2 req_perform_parallel req_perform_sequential resp_body_json
#' @importFrom dplyr bind_rows
#' @importFrom purrr list_rbind
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
  # --- 1. Argument Validation ---
  output <- match.arg(output)
  time <- as.POSIXct(time)

  # --- 2. Initialize common variables ---
  time_str <- unname(format(time, "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
  if (!grepl("Z$", time_str)) time_str <- paste0(time_str, "Z")
  
  dots <- list(...)
  user_server <- dots[[".server"]]
  # Clean dots
  dots[c("from", "to", "time", "arrive_by", "from_id_col", "to_id_col", "output", "parallel", ".server")] <- NULL

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

  # Helper to build a single request
  build_req <- function(f, t) {
    api_args <- c(
      list(
        fromPlace = unname(f),
        toPlace = unname(t),
        time = unname(time_str),
        arriveBy = unname(arrive_by),
        .build_only = TRUE,
        .server = unname(user_server %||% .get_server_url())
      ),
      dots
    )
    tryCatch({
      do.call(motis.client::mc_plan, api_args)
    }, error = function(e) {
      stop("Failed to construct MOTIS request: ", e$message, call. = FALSE)
    })
  }

  # --- 3. MANY-TO-MANY logic for Travel Time Matrix ---
  if (output %in% c("travel_time_matrix_long", "travel_time_matrix_wide")) {
    from_place <- .format_place(from, id_col = from_id_col)
    to_place <- .format_place(to, id_col = to_id_col)

    if ((length(from_place) * length(to_place)) > 100) {
      warning("For large travel time matrices, use `motis_table()` for better performance.", call. = FALSE)
    }

    from_ids <- .get_ids(from, id_col = from_id_col)
    to_ids <- .get_ids(to, id_col = to_id_col)
    combinations <- expand.grid(from_idx = seq_along(from_place), to_idx = seq_along(to_place))

    requests <- vector("list", nrow(combinations))
    for (i in seq_len(nrow(combinations))) {
      requests[[i]] <- build_req(from_place[combinations$from_idx[i]], to_place[combinations$to_idx[i]])
    }

    responses <- tryCatch({
      if (isTRUE(parallel)) httr2::req_perform_parallel(requests) else httr2::req_perform_sequential(requests)
    }, error = function(e) {
      stop("MOTIS travel time matrix request failed: ", e$message, call. = FALSE)
    })

    if (length(responses) == 0) return(data.frame(from_id = character(0), to_id = character(0), duration_minutes = numeric(0)))
    if (output == "raw_list") return(lapply(responses, .as_plan_list))

    min_durations <- vapply(responses, function(resp) {
      parsed <- tryCatch(.as_plan_list(resp), error = function(e) NULL)
      if (is.null(parsed) || is.null(parsed$itineraries) || length(parsed$itineraries) == 0) return(NA_real_)
      durs <- vapply(parsed$itineraries, function(it) as.numeric(it$duration %||% NA_real_), numeric(1))
      min(durs, na.rm = TRUE) / 60
    }, numeric(1))

    long_matrix <- data.frame(
      from_id = from_ids[combinations$from_idx],
      to_id = to_ids[combinations$to_idx],
      duration_minutes = min_durations
    )

    if (output == "travel_time_matrix_long") return(long_matrix)
    return(tidyr::pivot_wider(long_matrix, names_from = "to_id", values_from = "duration_minutes"))

  # --- 4. ONE-TO-ONE logic for Itineraries, Legs, and Raw List ---
  } else {
    stopifnot("Length of 'from' and 'to' must be equal" = NROW(from) == NROW(to))
    from_place <- .format_place(from, id_col = from_id_col)
    to_place <- .format_place(to, id_col = to_id_col)

    requests <- vector("list", length(from_place))
    for (i in seq_along(from_place)) {
      requests[[i]] <- build_req(from_place[i], to_place[i])
    }

    responses <- tryCatch({
      if (isTRUE(parallel)) httr2::req_perform_parallel(requests) else httr2::req_perform_sequential(requests)
    }, error = function(e) {
      stop("MOTIS routing request failed: ", e$message, call. = FALSE)
    })

    if (length(responses) == 0) {
      if (output == "raw_list") return(list())
      return(.st_as_sf_plan(if (output == "itineraries") .itins_template() else .legs_template()))
    }

    if (output == "raw_list") return(lapply(responses, .as_plan_list))

    results_list <- vector("list", length(responses))
    for (i in seq_along(responses)) {
      results_list[[i]] <- tryCatch({
        if (output == "itineraries") {
          .flatten_itineraries(responses[[i]], include_direct = TRUE, decode_geom = TRUE)
        } else {
          .flatten_legs(responses[[i]], include_direct = TRUE, decode_geom = TRUE)
        }
      }, error = function(e) {
        warning("Flattening failed for result ", i, ": ", e$message)
        if (output == "itineraries") .itins_template() else .legs_template()
      })
    }
    
    names(results_list) <- seq_along(results_list)
    combined <- dplyr::bind_rows(results_list, .id = "request_id")
    return(.st_as_sf_plan(combined))
  }
}
