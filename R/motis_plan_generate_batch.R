#' Generate MOTIS Batch Query File
#'
#' Efficiently constructs a text file of MOTIS routing queries for batch
#' processing. This function is designed for performance, using vectorised
#' operations to generate potentially millions of query URLs in seconds.
#'
#' @param from The origin location(s). Can be a character vector of station IDs,
#'   a data frame/tibble with ID or coordinate columns, an `sf` object, or a
#'   numeric matrix (`lon`, `lat`).
#' @param to The destination location(s). Same types as `from`.
#' @param output_file The path to the output text file.
#' @param time The departure or arrival time. Can be a single `POSIXct` object
#'   (applied to all queries) or a vector of the same length as the number of
#'   queries. Defaults to `Sys.time()`.
#' @param arrive_by Logical. If `TRUE`, `time` is treated as the arrival time.
#'   Defaults to `FALSE`.
#' @param from_id_col Column name for station IDs in `from`. Defaults to `"id"`.
#' @param to_id_col Column name for station IDs in `to`. Defaults to `"id"`.
#' @param all_pairs Logical. If `TRUE`, generates queries for all combinations
#'   of `from` and `to` (Cartesian product). If `FALSE` (default), `from` and
#'   `to` must have the same length and are paired 1-to-1.
#' @param ... Additional MOTIS API parameters (e.g., `directModes`, `maxTravelTime`).
#'   Vector arguments (e.g. `c("WALK", "CAR")`) are automatically collapsed to
#'   comma-separated strings. Parameters are validated against
#'   [motis.client::mc_plan()] before generation begins.
#' @param append Logical. If `TRUE`, appends to `output_file` instead of
#'   overwriting. Defaults to `FALSE`.
#' @param api_endpoint The API path. Defaults to `"/api/v1/plan"`.
#'
#' @return Invisibly returns the number of queries written to the file.
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate paired queries
#' origins <- data.frame(lat = c(59.3, 59.4), lon = c(18.0, 18.1))
#' dests <- data.frame(lat = c(59.5, 59.6), lon = c(18.2, 18.3))
#' motis_plan_generate_batch(origins, dests, "queries.txt")
#'
#' # Generate all-pairs matrix
#' motis_plan_generate_batch(origins, dests, "matrix.txt", all_pairs = TRUE)
#' }
motis_plan_generate_batch <- function(
  from,
  to,
  output_file,
  time = Sys.time(),
  arrive_by = FALSE,
  from_id_col = "id",
  to_id_col = "id",
  all_pairs = FALSE,
  ...,
  append = FALSE,
  api_endpoint = "/api/v1/plan"
) {
  # --- 1. Argument Validation & Pre-processing ---
  if (missing(output_file) || !is.character(output_file) || length(output_file) != 1) {
    stop("`output_file` must be a single string specifying the file path.", call. = FALSE)
  }

  # Match logic from api_plan.R: collapse vector arguments in ... to comma-separated strings
  dots <- list(...)
  dots <- lapply(dots, function(x) {
    if (length(x) > 1 && is.atomic(x)) {
      paste(unname(x), collapse = ",")
    } else if (is.atomic(x)) {
      unname(x)
    } else {
      x
    }
  })

  from_place <- .format_place(from, id_col = from_id_col)
  to_place <- .format_place(to, id_col = to_id_col)

  # --- New: Argument Validation via motis.client ---
  # Construct a single dummy request to validate parameters in `...`
  # using the OpenAPI-based logic in the client.
  tryCatch({
    # Strict custom validation for key parameters (since client is permissive)
    .validate_batch_params(dots)

    do.call(motis.client::mc_plan, c(
      list(
        fromPlace = from_place[1],
        toPlace = to_place[1],
        time = format(as.POSIXct(time[1]), "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
        arriveBy = arrive_by,
        .build_only = TRUE,
        .server = "http://localhost:8080" # Dummy server for building
      ),
      dots
    ))
  }, error = function(e) {
    stop("Invalid MOTIS API parameters: ", e$message, call. = FALSE)
  })

  if (isTRUE(all_pairs)) {
    # Cartesian product
    n_from <- length(from_place)
    n_to <- length(to_place)
    # expand.grid is slower than manual rep/rep.int but safer/clears intent
    # For large datasets we use rep/rep.int for raw speed
    from_vec <- rep(from_place, each = n_to)
    to_vec <- rep(to_place, times = n_from)
  } else {
    # Paired
    if (length(from_place) != length(to_place)) {
      stop("`from` and `to` must have the same length for paired queries.", call. = FALSE)
    }
    from_vec <- from_place
    to_vec <- to_place
  }

  n_queries <- length(from_vec)

  # --- 2. Format Time ---
  # Vectorise time formatting
  format_t <- function(t) {
    fmt <- format(as.POSIXct(t), "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    paste0(fmt, "Z")
  }

  if (length(time) == 1) {
    time_vec <- format_t(time)
  } else {
    if (length(time) != n_queries) {
      stop("`time` vector length must match number of queries (", n_queries, ").", call. = FALSE)
    }
    time_vec <- format_t(time)
  }

  # --- 3. Build Static Suffix ---
  # Parameters that are constant across all queries in this call
  static_params <- c(list(arriveBy = arrive_by), dots)
  static_suffix <- .build_static_suffix(static_params)

  # --- 4. Vectorised Generation ---
  # URL-encode places in case they are station IDs
  from_enc <- curl::curl_escape(from_vec)
  to_enc <- curl::curl_escape(to_vec)

  # Blazingly fast vectorised paste0
  lines <- paste0(
    api_endpoint,
    "?fromPlace=", from_enc,
    "&toPlace=", to_enc,
    "&time=", time_vec,
    static_suffix
  )

  # --- 5. Write to File ---
  con <- file(output_file, open = if (isTRUE(append)) "a" else "w")
  on.exit(close(con))
  writeLines(lines, con = con)

  message("Successfully wrote ", n_queries, " query(ies) to '", output_file, "'.")
  invisible(n_queries)
}

#' Internal helper to build URL parameters string from a list
#' @param params List of parameters
#' @return String starting with '&'
#' @noRd
.build_static_suffix <- function(params) {
  params <- Filter(Negate(is.null), params)
  if (length(params) == 0) return("")

  keys <- names(params)
  # Vectorised processing of values
  vals <- vapply(params, function(v) {
    if (is.logical(v)) return(tolower(as.character(v)))
    if (length(v) > 1) return(paste(v, collapse = ","))
    as.character(v)
  }, character(1))

  paste0("&", paste(curl::curl_escape(keys), curl::curl_escape(vals), sep = "=", collapse = "&"))
}

#' Internal helper to strictly validate key parameter types
#' @param params List of parameters to check
#' @noRd
.validate_batch_params <- function(params) {
  # Direct check for known boolean parameters
  bool_params <- c(
    "arriveBy", "wheelchair", "detailedTransfers", "useRoutedTransfers", 
    "requireBikeTransport", "requireCarTransport", "withFares", 
    "withScheduledSkippedStops", "slowDirect"
  )
  for (p in bool_params) {
    if (!is.null(params[[p]]) && !is.logical(params[[p]])) {
      stop("Parameter '", p, "' must be logical (TRUE/FALSE), not ", typeof(params[[p]]), ".", call. = FALSE)
    }
  }

  # Direct check for known integer/numeric parameters
  int_params <- c(
    "maxTransfers", "maxTravelTime", "minTransferTime", "additionalTransferTime",
    "maxMatchingDistance", "passengers", "numItineraries", "maxItineraries"
  )
  for (p in int_params) {
    if (!is.null(params[[p]]) && !is.numeric(params[[p]])) {
      stop("Parameter '", p, "' must be numeric/integer, not ", typeof(params[[p]]), ".", call. = FALSE)
    }
  }
}
