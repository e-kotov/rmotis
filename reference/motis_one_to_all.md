# Calculate reachable locations from a single point within a given travel time

This function is a user-friendly wrapper for the MOTIS `one-to-all` API.
It computes the travel time to all reachable transit stops from a single
origin (or to a single destination from all reachable stops) within a
specified maximum travel time.

## Usage

``` r
motis_one_to_all(
  one,
  one_id_col = "id",
  time = Sys.time(),
  arrive_by = FALSE,
  max_travel_time = 90,
  ...,
  output = c("data.frame", "raw_list")
)
```

## Arguments

- one:

  The single origin (when `arrive_by = FALSE`) or destination (when
  `arrive_by = TRUE`). Can be a character vector of an ID, a data
  frame/tibble with coordinate columns, an `sf` object with a single
  POINT geometry, or a numeric vector/matrix (`lon`, `lat`).

- one_id_col:

  The name of the column in `one` to use for identifying the point in
  the output. Defaults to `"id"`.

- time:

  The departure or arrival time. Can be a POSIXct object (like from
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html)) or a character
  string in ISO 8601 format (e.g., "2025-08-15T15:11:00Z"). Defaults to
  the current time.

- arrive_by:

  Logical. If `FALSE` (the default), calculates routes from `one` to all
  reachable stops. If `TRUE`, calculates routes from all reachable stops
  to `one`.

- max_travel_time:

  Integer. The maximum travel time in minutes. Defaults to 120.

- ...:

  Arguments passed on to
  [`motis.client::mc_oneToAll`](https://rdrr.io/pkg/motis.client/man/mc_oneToAll.html)

  `maxTransfers`

  :   The maximum number of allowed transfers (i.e. interchanges between
      transit legs, pre- and postTransit do not count as transfers).
      `maxTransfers=0` searches for direct transit connections without
      any transfers. If you want to search only for non-transit
      connections (`FOOT`, `CAR`, etc.), send an empty `transitModes`
      parameter instead.

      If not provided, the routing uses the server-side default value
      which is hardcoded and very high to cover all use cases.

      *Warning*: Use with care. Setting this too low can lead to optimal
      (e.g. the fastest) journeys not being found. If this value is too
      low to reach the destination at all, it can lead to slow routing
      performance.

      In plan endpoints before v3, the behavior is off by one, i.e.
      `maxTransfers=0` only returns non-transit connections.

  `minTransferTime`

  :   Optional. Default is 0 minutes.

      Minimum transfer time for each transfer in minutes.

  `additionalTransferTime`

  :   Optional. Default is 0 minutes.

      Additional transfer time reserved for each transfer in minutes.

  `transferTimeFactor`

  :   Optional. Default is 1.0

      Factor to multiply minimum required transfer times with. Values
      smaller than 1.0 are not supported.

  `maxMatchingDistance`

  :   Optional. Default is 25 meters.

      Maximum matching distance in meters to match geo coordinates to
      the street network.

  `useRoutedTransfers`

  :   Optional. Default is `false`.

      Whether to use transfers routed on OpenStreetMap data.

  `pedestrianProfile`

  :   Optional. Default is `FOOT`.

      Accessibility profile to use for pedestrian routing in transfers
      between transit connections and the first and last mile
      respectively.

      Allowed values: FOOT, WHEELCHAIR.

  `pedestrianSpeed`

  :   Optional

      Average speed for pedestrian routing.

  `cyclingSpeed`

  :   Optional

      Average speed for bike routing.

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

      The profile is used for routing on both the first and last mile.

      Elevation cost profiles are currently used by following street
      modes:

      - `BIKE`

      Allowed values: NONE, LOW, HIGH.

  `transitModes`

  :   Optional. Default is `TRANSIT` which allows all transit modes (no
      restriction). Allowed modes for the transit part. If empty, no
      transit connections will be computed. For example, this can be
      used to allow only `SUBURBAN,SUBWAY,TRAM`.

      Allowed values: WALK, BIKE, RENTAL, CAR, CAR_PARKING, CAR_DROPOFF,
      ODM, RIDE_SHARING, FLEX, DEBUG_BUS_ROUTE, DEBUG_RAILWAY_ROUTE,
      DEBUG_FERRY_ROUTE, TRANSIT, TRAM, SUBWAY, FERRY, AIRPLANE, BUS,
      COACH, RAIL, HIGHSPEED_RAIL, LONG_DISTANCE, NIGHT_RAIL,
      REGIONAL_FAST_RAIL, REGIONAL_RAIL, SUBURBAN, FUNICULAR,
      AERIAL_LIFT, OTHER, AREAL_LIFT, METRO, CABLE_CAR.

  `preTransitModes`

  :   Optional. Default is `WALK`. The behavior depends on whether
      `arriveBy` is set:

      - `arriveBy=true`: Currently not used

      - `arriveBy=false`: Only applies if the `one` place is a
        coordinate (not a transit stop).

      A list of modes that are allowed to be used from the last transit
      stop to the `to` coordinate. Example: `WALK,BIKE_SHARING`.

      Allowed values: WALK, BIKE, RENTAL, CAR, CAR_PARKING, CAR_DROPOFF,
      ODM, RIDE_SHARING, FLEX, DEBUG_BUS_ROUTE, DEBUG_RAILWAY_ROUTE,
      DEBUG_FERRY_ROUTE, TRANSIT, TRAM, SUBWAY, FERRY, AIRPLANE, BUS,
      COACH, RAIL, HIGHSPEED_RAIL, LONG_DISTANCE, NIGHT_RAIL,
      REGIONAL_FAST_RAIL, REGIONAL_RAIL, SUBURBAN, FUNICULAR,
      AERIAL_LIFT, OTHER, AREAL_LIFT, METRO, CABLE_CAR.

  `postTransitModes`

  :   Optional. Default is `WALK`. The behavior depends on whether
      `arriveBy` is set:

      - `arriveBy=true`: Only applies if the `one` place is a coordinate
        (not a transit stop).

      - `arriveBy=false`: Currently not used

      A list of modes that are allowed to be used from the last transit
      stop to the `to` coordinate. Example: `WALK,BIKE_SHARING`.

      Allowed values: WALK, BIKE, RENTAL, CAR, CAR_PARKING, CAR_DROPOFF,
      ODM, RIDE_SHARING, FLEX, DEBUG_BUS_ROUTE, DEBUG_RAILWAY_ROUTE,
      DEBUG_FERRY_ROUTE, TRANSIT, TRAM, SUBWAY, FERRY, AIRPLANE, BUS,
      COACH, RAIL, HIGHSPEED_RAIL, LONG_DISTANCE, NIGHT_RAIL,
      REGIONAL_FAST_RAIL, REGIONAL_RAIL, SUBURBAN, FUNICULAR,
      AERIAL_LIFT, OTHER, AREAL_LIFT, METRO, CABLE_CAR.

  `requireBikeTransport`

  :   Optional. Default is `false`.

      If set to `true`, all used transit trips are required to allow
      bike carriage.

  `requireCarTransport`

  :   Optional. Default is `false`.

      If set to `true`, all used transit trips are required to allow car
      carriage.

  `maxPreTransitTime`

  :   Optional. Default is 15min which is `900`.

      - `arriveBy=true`: Currently not used

      - `arriveBy=false`: Maximum time in seconds for the street leg at
        `one` location. Is limited by server config variable
        `street_routing_max_prepost_transit_seconds`.

  `maxPostTransitTime`

  :   Optional. Default is 15min which is `900`.

      - `arriveBy=true`: Maximum time in seconds for the street leg at
        `one` location.

      - `arriveBy=false`: Currently not used Is limited by server config
        variable `street_routing_max_prepost_transit_seconds`.

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

- output:

  The desired output format. One of:

  - `"data.frame"` (default): A tidy data frame with travel times and
    transfers.

  - `"raw_list"`: The raw parsed JSON response as a list.

## Value

Depending on the `output` parameter, a `data.frame` or a list. The data
frame will contain columns for the identifier of the `one` point, the
`target_id` of the reachable stop, the `duration_seconds`, and the
number of `transfers`.
