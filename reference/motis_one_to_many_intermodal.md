# Calculate one-to-many or many-to-one intermodal (public transit) routes

This function computes travel time and distance from origin(s) to
multiple destinations (or vice versa) using public transit and other
intermodal modes (walking, cycling, etc.). It supports both the MOTIS
POST API and the MOTIS CLI batch engine.

## Usage

``` r
motis_one_to_many_intermodal(
  one,
  many,
  time = Sys.time(),
  arrive_by = FALSE,
  max_travel_time = 60,
  one_id_col = "id",
  many_id_col = "id",
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

- time:

  The departure or arrival time. Can be a POSIXct object (like from
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html)) or a character
  string in ISO 8601 format (e.g., "2025-08-15T15:11:00Z"). Defaults to
  the current time.

- arrive_by:

  Logical. If `FALSE` (the default), calculates routes from `one` to
  `many`. If `TRUE`, calculates routes from `many` to `one`.

- max_travel_time:

  The maximum travel time in **minutes**.

- one_id_col:

  The name of the column in `one` to use for identifying the point in
  the output. Defaults to `"id"`.

- many_id_col:

  The name of the column in `many` to use for identifying the point in
  the output. Defaults to `"id"`.

- maxMatchingDistance:

  maximum matching distance in meters to match geo coordinates to the
  street network

- withDistance:

  Logical. Include distance in the output? Defaults to `FALSE`.
  **Note:** Currently ignored by the intermodal API.

- spatial_filter_km:

  Numeric. Optional straight-line distance threshold (in kilometers). If
  provided, destinations further than this distance from an origin will
  be excluded from the MOTIS request for that origin. This is highly
  recommended for very large destination sets to reduce server load and
  network traffic.

- ...:

  Arguments passed on to
  [`motis.client::mc_oneToManyIntermodalPost`](https://rdrr.io/pkg/motis.client/man/mc_oneToManyIntermodalPost.html)

  `maxTransfers`

  :   The maximum number of allowed transfers (i.e. interchanges between
      transit legs, pre- and postTransit do not count as transfers).
      `maxTransfers=0` searches for direct transit connections without
      any transfers. If you want to search only for non-transit
      connections (`FOOT`, `CAR`, etc.), send an empty `transitModes`
      parameter instead. If not provided, the routing uses the
      server-side default value which is hardcoded and very high to
      cover all use cases. *Warning*: Use with care. Setting this too
      low can lead to optimal (e.g. the fastest) journeys not being
      found. If this value is too low to reach the destination at all,
      it can lead to slow routing performance.

  `minTransferTime`

  :   Optional. Default is 0 minutes. Minimum transfer time for each
      transfer in minutes.

  `additionalTransferTime`

  :   Optional. Default is 0 minutes. Additional transfer time reserved
      for each transfer in minutes.

  `transferTimeFactor`

  :   Optional. Default is 1.0 Factor to multiply minimum required
      transfer times with. Values smaller than 1.0 are not supported.

  `useRoutedTransfers`

  :   Optional. Default is `false`. Whether to use transfers routed on
      OpenStreetMap data.

  `pedestrianProfile`

  :   Different accessibility profiles for pedestrians. Allowed values:
      FOOT, WHEELCHAIR.

  `pedestrianSpeed`

  :   Average speed for pedestrian routing in meters per second

  `cyclingSpeed`

  :   Average speed for bike routing in meters per second

  `elevationCosts`

  :   Different elevation cost profiles for street routing. Using a
      elevation cost profile will prefer routes with a smaller incline
      and smaller difference in elevation, even if the routed way is
      longer. - `NONE`: Ignore elevation data for routing. This is the
      default behavior - `LOW`: Add a low penalty for inclines. This
      will favor longer paths, if the elevation increase and incline are
      smaller. - `HIGH`: Add a high penalty for inclines. This will
      favor even longer paths, if the elevation increase and incline are
      smaller. Allowed values: NONE, LOW, HIGH.

  `transitModes`

  :   Optional. Default is `TRANSIT` which allows all transit modes (no
      restriction). Allowed modes for the transit part. If empty, no
      transit connections will be computed. For example, this can be
      used to allow only `SUBURBAN,SUBWAY,TRAM`. Allowed values: WALK,
      BIKE, RENTAL, CAR, CAR_PARKING, CAR_DROPOFF, ODM, RIDE_SHARING,
      FLEX, DEBUG_BUS_ROUTE, DEBUG_RAILWAY_ROUTE, DEBUG_FERRY_ROUTE,
      TRANSIT, TRAM, SUBWAY, FERRY, AIRPLANE, BUS, COACH, RAIL,
      HIGHSPEED_RAIL, LONG_DISTANCE, NIGHT_RAIL, REGIONAL_FAST_RAIL,
      REGIONAL_RAIL, SUBURBAN, FUNICULAR, AERIAL_LIFT, OTHER,
      AREAL_LIFT, METRO, CABLE_CAR.

  `preTransitModes`

  :   Optional. Default is `WALK`. Does not apply to direct connections
      (see `directMode`). A list of modes that are allowed to be used
      for the first mile, i.e. from the coordinates to the first transit
      stop. Example: `WALK,BIKE_SHARING`. Allowed values: WALK, BIKE,
      RENTAL, CAR, CAR_PARKING, CAR_DROPOFF, ODM, RIDE_SHARING, FLEX,
      DEBUG_BUS_ROUTE, DEBUG_RAILWAY_ROUTE, DEBUG_FERRY_ROUTE, TRANSIT,
      TRAM, SUBWAY, FERRY, AIRPLANE, BUS, COACH, RAIL, HIGHSPEED_RAIL,
      LONG_DISTANCE, NIGHT_RAIL, REGIONAL_FAST_RAIL, REGIONAL_RAIL,
      SUBURBAN, FUNICULAR, AERIAL_LIFT, OTHER, AREAL_LIFT, METRO,
      CABLE_CAR.

  `postTransitModes`

  :   Optional. Default is `WALK`. Does not apply to direct connections
      (see `directMode`). A list of modes that are allowed to be used
      for the last mile, i.e. from the last transit stop to the target
      coordinates. Example: `WALK,BIKE_SHARING`. Allowed values: WALK,
      BIKE, RENTAL, CAR, CAR_PARKING, CAR_DROPOFF, ODM, RIDE_SHARING,
      FLEX, DEBUG_BUS_ROUTE, DEBUG_RAILWAY_ROUTE, DEBUG_FERRY_ROUTE,
      TRANSIT, TRAM, SUBWAY, FERRY, AIRPLANE, BUS, COACH, RAIL,
      HIGHSPEED_RAIL, LONG_DISTANCE, NIGHT_RAIL, REGIONAL_FAST_RAIL,
      REGIONAL_RAIL, SUBURBAN, FUNICULAR, AERIAL_LIFT, OTHER,
      AREAL_LIFT, METRO, CABLE_CAR.

  `directMode`

  :   \# Street modes - `WALK` - `BIKE` - `RENTAL` Experimental. Expect
      unannounced breaking changes (without version bumps) for all
      parameters and returned structs. - `CAR` - `CAR_PARKING`
      Experimental. Expect unannounced breaking changes (without version
      bumps) for all parameters and returned structs. - `CAR_DROPOFF`
      Experimental. Expect unannounced breaking changes (without version
      bumps) for all perameters and returned structs. - `ODM` on-demand
      taxis from the Prima+ÖV Project - `RIDE_SHARING` ride sharing from
      the Prima+ÖV Project - `FLEX` flexible transports \# Transit
      modes - `TRANSIT`: translates to
      `TRAM,FERRY,AIRPLANE,BUS,COACH,RAIL,ODM,FUNICULAR,AERIAL_LIFT,OTHER` -
      `TRAM`: trams - `SUBWAY`: subway trains (Paris Metro, London
      Underground, but also NYC Subway, Hamburger Hochbahn, and other
      non-underground services) - `FERRY`: ferries - `AIRPLANE`: airline
      flights - `BUS`: short distance buses (does not include `COACH`) -
      `COACH`: long distance buses (does not include `BUS`) - `RAIL`:
      translates to
      `HIGHSPEED_RAIL,LONG_DISTANCE,NIGHT_RAIL,REGIONAL_RAIL,SUBURBAN,SUBWAY` -
      `HIGHSPEED_RAIL`: long distance high speed trains (e.g. TGV) -
      `LONG_DISTANCE`: long distance inter city trains - `NIGHT_RAIL`:
      long distance night trains - `REGIONAL_FAST_RAIL`: deprecated,
      `REGIONAL_RAIL` will be used - `REGIONAL_RAIL`: regional train -
      `SUBURBAN`: suburban trains (e.g. S-Bahn, RER, Elizabeth Line,
      ...) - `ODM`: demand responsive transport - `FUNICULAR`:
      Funicular. Any rail system designed for steep inclines. -
      `AERIAL_LIFT`: Aerial lift, suspended cable car (e.g., gondola
      lift, aerial tramway). Cable transport where cabins, cars,
      gondolas or open chairs are suspended by means of one or more
      cables. - `AREAL_LIFT`: deprecated - `METRO`: deprecated -
      `CABLE_CAR`: deprecated Allowed values: WALK, BIKE, RENTAL, CAR,
      CAR_PARKING, CAR_DROPOFF, ODM, RIDE_SHARING, FLEX,
      DEBUG_BUS_ROUTE, DEBUG_RAILWAY_ROUTE, DEBUG_FERRY_ROUTE, TRANSIT,
      TRAM, SUBWAY, FERRY, AIRPLANE, BUS, COACH, RAIL, HIGHSPEED_RAIL,
      LONG_DISTANCE, NIGHT_RAIL, REGIONAL_FAST_RAIL, REGIONAL_RAIL,
      SUBURBAN, FUNICULAR, AERIAL_LIFT, OTHER, AREAL_LIFT, METRO,
      CABLE_CAR.

  `maxPreTransitTime`

  :   Optional. Default is 15min which is `900`. Maximum time in seconds
      for the first street leg. Is limited by server config variable
      `street_routing_max_prepost_transit_seconds`.

  `maxPostTransitTime`

  :   Optional. Default is 15min which is `900`. Maximum time in seconds
      for the last street leg. Is limited by server config variable
      `street_routing_max_prepost_transit_seconds`.

  `maxDirectTime`

  :   Optional. Default is 30min which is `1800`. Maximum time in
      seconds for direct connections. If a value smaller than either
      `maxPreTransitTime` or `maxPostTransitTime` is used, their maximum
      is set instead. Is limited by server config variable
      `street_routing_max_direct_seconds`.

  `requireBikeTransport`

  :   Optional. Default is `false`. If set to `true`, all used transit
      trips are required to allow bike carriage.

  `requireCarTransport`

  :   Optional. Default is `false`. If set to `true`, all used transit
      trips are required to allow car carriage.

  `.body`

  :   A list or object to be sent as the request body. If NULL (the
      default), the body will be constructed from any other top-level
      arguments you provide.

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
  `"\r\n"` for CRLF).

- motis_path:

  Path to the directory containing the MOTIS binary, or `NULL` to use
  the system PATH.

## Value

Depending on the `output` parameter and `output_path`, a `data.frame`, a
list, or the `output_path` invisibly.
