# Calculate one-to-many or many-to-one street-level routes

This function computes travel time and distance from origin(s) to
multiple destinations (or vice versa). It supports both simple
single-request execution and robust parallel/batch execution for large
datasets using either the MOTIS API or the MOTIS CLI batch engine.

## Usage

``` r
motis_one_to_many(
  one,
  many,
  one_id_col = "id",
  many_id_col = "id",
  mode = c("WALK", "BIKE", "CAR"),
  arrive_by = FALSE,
  max = 7200,
  maxMatchingDistance = 1000,
  withDistance = FALSE,
  spatial_filter_km = NULL,
  ...,
  spatial_filter = NULL,
  max_speed_kmh = NULL,
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
)
```

## Arguments

- one:

  Origin(s). Can be a data frame/tibble with coordinate columns, an `sf`
  object, or a vector/matrix of coordinates.

- many:

  Destination(s). Same format as `one`.

- one_id_col:

  The name of the column in `one` to use for identifying the point in
  the output. Defaults to `"id"`.

- many_id_col:

  The name of the column in `many` to use for identifying the point in
  the output. Defaults to `"id"`.

- mode:

  The routing profile to use. Defaults to `"WALK"`.

- arrive_by:

  Logical. If `FALSE` (the default), calculates routes from `one` to
  `many`. If `TRUE`, calculates routes from `many` to `one`.

- max:

  maximum travel time in seconds

- maxMatchingDistance:

  maximum matching distance in meters

- withDistance:

  Logical. Include distance in the output? Defaults to `FALSE`.

- spatial_filter_km:

  Numeric. Optional straight-line distance threshold (in kilometers). If
  provided, destinations further than this distance from an origin will
  be excluded from the MOTIS request for that origin. This is highly
  recommended for very large destination sets to reduce server load and
  network traffic.

- ...:

  Arguments passed on to
  [`motis.client::mc_oneToMany`](https://rdrr.io/pkg/motis.client/man/mc_oneToMany.html)

  `elevationCosts`

  :   Optional. Default is `NONE`.

      Set an elevation cost profile, to penalize routes with incline.

      - `NONE`: No additional costs for elevations. This is the default
        behavior

      - `LOW`: Add a low cost for increase in elevation and incline
        along the way. This will prefer routes with less ascent, if
        small detours are required.

      - `HIGH`: Add a high cost for increase in elevation and incline
        along the way. This will prefer routes with less ascent, if
        larger detours are required.

      As using an elevation costs profile will increase the travel
      duration, routing through steep terrain may exceed the maximal
      allowed duration, causing a location to appear unreachable.
      Increasing the maximum travel time for these segments may resolve
      this issue.

      Elevation cost profiles are currently used by following street
      modes:

      - `BIKE`

      Allowed values: NONE, LOW, HIGH.

  `.return_as`

  :   A string specifying the return format. Defaults to 'list'. Options
      are 'list' for a parsed R list, 'raw' for the raw httr2_response
      object, or 'string' for the raw JSON string.

  `.json_parser`

  :   A string specifying which parser to use when .return_as = 'list'.
      Defaults to 'RcppSimdJson' (faster) or 'jsonlite'. Beware that
      their output may differ slightly.

  `.headers`

  :   A named list of extra HTTP headers to add to the request. If no
      'Accept' header is provided anywhere, 'Accept: */*' will be sent
      by default.

  `.auth`

  :   An authentication token or method (e.g., a bearer token string).

  `.throttle_rate`

  :   A number to pass to
      [`httr2::req_throttle()`](https://httr2.r-lib.org/reference/req_throttle.html)
      to limit the rate of requests. For example, `15/60` means 15
      requests per minute. This overrides any default rate set at client
      generation time.

  `.build_only`

  :   A logical. If TRUE, the function will build and return the
      httr2_request object without performing it. Defaults to FALSE.

  `.server`

  :   A string to override the base URL for this specific request. If
      provided, it will be used instead of the default server URL.

  `.referer`

  :   A string to set the Referer HTTP header.

  `.req_options`

  :   A named list of curl options passed to
      [`httr2::req_options()`](https://httr2.r-lib.org/reference/req_options.html).
      Useful for timeouts, proxies, SSL, etc.

  `.handle_response`

  :   A function taking an `httr2_response` and returning a value. If
      supplied, it overrides `.return_as` / `.json_parser` handling. See
      helpers like `oa3_content_or_stop()`.

  `.json_auto_unbox`

  :   Logical. If TRUE, JSON bodies are encoded with `auto_unbox = TRUE`
      (jsonlite). Defaults to FALSE unless overridden.

  `.paginate`

  :   A logical, character string, or function to enable pagination. If
      TRUE (or "link_header"), uses Link headers. Other options:
      "page_param", "cursor_param", or a custom function. See
      `oa3_paginate()`.

- spatial_filter, max_speed_kmh:

  Deprecated. Use `spatial_filter_km` instead.

- engine:

  Execution engine:

  - `"api"` (default): Uses the MOTIS POST API. Supports parallel
    backends.

  - `"batch"`: Uses the MOTIS CLI batch command. **Recommended for very
    large datasets** (millions of routes). Requires `data_dir`.

- output:

  The desired output format. One of:

  - `"data.frame"` (default): A tidy data frame.

  - `"raw_list"`: The raw parsed JSON response (only for `engine='api'`
    and non-parallel execution).

- parallel:

  Logical. Enable parallel processing for the API engine? Defaults to
  `TRUE`.

- backend:

  Parallel backend for API engine: `"auto"`, `"httr2"`, or `"mirai"`.

- batch_size:

  Number of origins to process per batch/request.

- max_destinations_per_batch:

  Optional limit to split destinations into multiple requests to avoid
  memory or timeout issues.

- output_path:

  Optional path to write results incrementally (`.csv`, `.duckdb`, or
  **Directory** of `.parquet` files).

- checkpoint_file:

  Optional path for checkpointing progress (API engine only).

- progress:

  Logical. Display progress bar/messages?

- data_dir:

  Path to MOTIS data directory. Required if `engine='batch'`.

- temp_dir:

  Directory for temporary batch files. Defaults to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- keep_files:

  Logical. Keep temporary files? Defaults to `FALSE`.

- eol:

  Optional line ending for batch query files (e.g., `"\n"` for LF or
  `"\r\n"` for CRLF). If provided, forces this line ending even on
  Windows. Typically, `"\n"` is required for MOTIS batch processing.

- motis_path:

  Path to the directory containing the MOTIS binary, or `NULL` to use
  the system PATH.

## Value

Depending on the `output` parameter and `output_path`, a `data.frame`, a
list, or the `output_path` invisibly.
