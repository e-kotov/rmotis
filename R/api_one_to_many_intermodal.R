#' Calculate one-to-many or many-to-one intermodal (public transit) routes
#'
#' This function computes travel time and distance from origin(s) to
#' multiple destinations (or vice versa) using public transit and other
#' intermodal modes (walking, cycling, etc.). It supports both the MOTIS
#' POST API and the MOTIS CLI batch engine.
#'
#' @param one Origin(s). Can be a data frame/tibble with coordinate columns,
#'   an `sf` object, or a vector/matrix of coordinates.
#' @param many Destination(s). Same format as `one`.
#' @param time The departure or arrival time. Can be a POSIXct object (like from
#'   `Sys.time()`) or a character string in ISO 8601 format (e.g., "2025-08-15T15:11:00Z").
#'   Defaults to the current time.
#' @param arrive_by Logical. If `FALSE` (the default), calculates routes from
#'   `one` to `many`. If `TRUE`, calculates routes from `many` to `one`.
#' @param max_travel_time The maximum travel time in **minutes**.
#' @param one_id_col The name of the column in `one` to use for identifying
#'   the point in the output. Defaults to `"id"`.
#' @param many_id_col The name of the column in `many` to use for identifying
#'   the point in the output. Defaults to `"id"`.
#' @param maxMatchingDistance maximum matching distance in meters to match geo
#'   coordinates to the street network
#' @param withDistance Logical. Include distance in the output? 
#'   Defaults to `FALSE`. **Note:** Currently ignored by the intermodal API.
#' @param ... Additional MOTIS API parameters.
#' @param engine Execution engine:
#'   - `"api"` (default): Uses the MOTIS POST API. Supports parallel backends.
#'   - `"batch"`: Uses the MOTIS CLI batch command. **Recommended for very large
#'     datasets** (millions of routes). Requires `data_dir`.
#' @param output The desired output format. One of:
#'   - `"data.frame"` (default): A tidy data frame.
#'   - `"raw_list"`: The raw parsed JSON response (only for `engine='api'` and non-parallel execution).
#' @param parallel Logical. Enable parallel processing for the API engine? 
#'   Defaults to `TRUE`.
#' @param backend Parallel backend for API engine: `"auto"`, `"httr2"`, or `"mirai"`.
#' @param batch_size Number of origins to process per batch/request.
#' @param max_destinations_per_batch Optional limit to split destinations into 
#'   multiple requests to avoid memory or timeout issues.
#' @param output_path Optional path to write results incrementally (`.csv`, 
#'   `.duckdb`, or **Directory** of `.parquet` files).
#' @param checkpoint_file Optional path for checkpointing progress (API engine only).
#' @param progress Logical. Display progress bar/messages?
#' @param data_dir Path to MOTIS data directory. Required if `engine='batch'`.
#' @param temp_dir Directory for temporary batch files. Defaults to `tempdir()`.
#' @param keep_files Logical. Keep temporary files? Defaults to `FALSE`.
#' @param eol Optional line ending for batch query files (e.g., `"\n"` for LF or
#'   `"\r\n"` for CRLF).
#' @param motis_path Path to the directory containing the MOTIS binary, or
#'   `NULL` to use the system PATH.
#' @inheritDotParams motis.client::mc_oneToManyIntermodalPost -one -many -time -arriveBy -maxTravelTime -maxMatchingDistance -.endpoint
#'
#' @return Depending on the `output` parameter and `output_path`, a `data.frame`, 
#'   a list, or the `output_path` invisibly.
#' @export
motis_one_to_many_intermodal <- function(
  one,
  many,
  time = Sys.time(),
  arrive_by = FALSE,
  max_travel_time = 60,
  one_id_col = "id",
  many_id_col = "id",
  maxMatchingDistance = 1000,
  withDistance = FALSE,
  ...,
  engine = c("api", "batch"),
  output = c("data.frame", "raw_list"),
  parallel = TRUE,
  backend = c("auto", "httr2", "mirai"),
  batch_size = NULL,
  max_destinations_per_batch = NULL,
  output_path = NULL,
  checkpoint_file = NULL,
  progress = TRUE,
  data_dir = NULL,
  temp_dir = tempdir(),
  keep_files = FALSE,
  eol = NULL,
  motis_path = NULL
) {
  # --- 1. Argument and Input Validation ---
  engine <- match.arg(engine)
  output <- match.arg(output)
  backend <- match.arg(backend)
  
  time_str <- .format_time_utc(time)
  
  .motis_one_to_many_infra(
    one = one,
    many = many,
    one_id_col = one_id_col,
    many_id_col = many_id_col,
    mode = "TRANSIT", # Standard default for intermodal
    arrive_by = arrive_by,
    duration_val = max_travel_time,
    maxMatchingDistance = maxMatchingDistance,
    withDistance = withDistance,
    time = time_str,
    ...,
    engine = engine,
    output = output,
    parallel = parallel,
    backend = backend,
    batch_size = batch_size,
    max_destinations_per_batch = max_destinations_per_batch,
    output_path = output_path,
    checkpoint_file = checkpoint_file,
    progress = progress,
    data_dir = data_dir,
    temp_dir = temp_dir,
    keep_files = keep_files,
    eol = eol,
    motis_path = motis_path,
    api_endpoint = "/api/experimental/one-to-many-intermodal",
    duration_key = "maxTravelTime",
    client_fun = motis.client::mc_oneToManyIntermodalPost
  )
}
