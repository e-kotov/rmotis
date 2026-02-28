# Plan a journey between two points or create a travel time matrix

This function is a user-friendly wrapper around the MOTIS `plan` API. It
can plan paired journeys (`from[1]` to `to[1]`, etc.) or compute a full
travel time matrix (from all origins to all destinations).

## Usage

``` r
motis_plan(
  from,
  to,
  time = Sys.time(),
  arrive_by = FALSE,
  from_id_col = "id",
  to_id_col = "id",
  ...,
  output = c("itineraries", "legs", "travel_time_matrix_long", "travel_time_matrix_wide",
    "raw_list"),
  parallel = FALSE
)
```

## Arguments

- from:

  The origin location(s). Can be a character vector of station IDs, a
  data frame/tibble with ID or coordinate columns, an `sf` object with
  POINT geometry, or a numeric matrix (`lon`, `lat`).

- to:

  The destination location(s). Must be of the same type as `from`. For
  paired journey planning, must be the same length as `from`. For travel
  time matrix calculation, length can be different.

- time:

  The departure or arrival time. Can be a POSIXct object (like from
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html)) or a character
  string in ISO 8601 format (e.g., "2025-08-15T15:11:00Z"). Defaults to
  the current time.

- arrive_by:

  Logical. If `TRUE`, `time` is treated as the arrival time. Defaults to
  `FALSE` (departure time).

- from_id_col:

  The name of the column in `from` containing station IDs. Defaults to
  `"id"`.

- to_id_col:

  The name of the column in `to` containing station IDs. Defaults to
  `"id"`.

- ...:

  Arguments passed on to
  [`motis.client::mc_plan`](https://rdrr.io/pkg/motis.client/man/mc_plan.html)

  `via`

  :   List of via stops to visit (only stop IDs, no coordinates allowed
      for now). Also see the optional parameter `viaMinimumStay` to set
      a set a minimum stay duration for each via stop.

  `viaMinimumStay`

  :   Optional. If not set, the default is `0,0` - no stay required.

      For each `via` stop a minimum stay duration in minutes.

      The value `0` signals that it's allowed to stay in the same trip.
      This enables via stays without counting a transfer and can lead to
      better connections with less transfers. Transfer connections can
      still be found with `viaMinimumStay=0`.

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

  `maxTravelTime`

  :   The maximum travel time in minutes. If not provided, the routing
      to uses the value hardcoded in the server which is usually quite
      high.

      *Warning*: Use with care. Setting this too low can lead to optimal
      (e.g. the least transfers) journeys not being found. If this value
      is too low to reach the destination at all, it can lead to slow
      routing performance.

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

  `pedestrianProfile`

  :   Optional. Default is `FOOT`.

      Accessibility profile to use for pedestrian routing in transfers
      between transit connections, on the first mile, and last mile.

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

      The profile is used for direct routing, on the first mile, and
      last mile.

      Elevation cost profiles are currently used by following street
      modes:

      - `BIKE`

      Allowed values: NONE, LOW, HIGH.

  `useRoutedTransfers`

  :   Optional. Default is `false`.

      Whether to use transfers routed on OpenStreetMap data.

  `detailedTransfers`

  :   - true: Compute transfer polylines and step instructions.

      - false: Only return basic information (start time, end time,
        duration) for transfers.

  `joinInterlinedLegs`

  :   Optional. Default is `true`.

      Controls if a journey section with stay-seated transfers is
      returned:

      - `joinInterlinedLegs=false`: as several legs (full information
        about all trip numbers, headsigns, etc.). Legs that do not
        require a transfer (stay-seated transfer) are marked with
        `interlineWithPreviousLeg=true`.

      - `joinInterlinedLegs=true` (default behavior): as only one joined
        leg containing all stops

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

  `directModes`

  :   Optional. Default is `WALK` which will compute walking routes as
      direct connections.

      Modes used for direction connections from start to destination
      without using transit. Results will be returned on the `direct`
      key.

      Note: Direct connections will only be returned on the first call.
      For paging calls, they can be omitted.

      Note: Transit connections that are slower than the fastest direct
      connection will not show up. This is being used as a cut-off
      during transit routing to speed up the search. To prevent this,
      it's possible to send two separate requests (one with only
      `transitModes` and one with only `directModes`).

      Note: the output `direct` array will stay empty if the input param
      `maxDirectTime` makes any direct trip impossible.

      Only non-transit modes such as `WALK`, `BIKE`, `CAR`,
      `BIKE_SHARING`, etc. can be used.

      Allowed values: WALK, BIKE, RENTAL, CAR, CAR_PARKING, CAR_DROPOFF,
      ODM, RIDE_SHARING, FLEX, DEBUG_BUS_ROUTE, DEBUG_RAILWAY_ROUTE,
      DEBUG_FERRY_ROUTE, TRANSIT, TRAM, SUBWAY, FERRY, AIRPLANE, BUS,
      COACH, RAIL, HIGHSPEED_RAIL, LONG_DISTANCE, NIGHT_RAIL,
      REGIONAL_FAST_RAIL, REGIONAL_RAIL, SUBURBAN, FUNICULAR,
      AERIAL_LIFT, OTHER, AREAL_LIFT, METRO, CABLE_CAR.

  `preTransitModes`

  :   Optional. Default is `WALK`. Only applies if the `from` place is a
      coordinate (not a transit stop). Does not apply to direct
      connections (see `directModes`).

      A list of modes that are allowed to be used from the `from`
      coordinate to the first transit stop. Example:
      `WALK,BIKE_SHARING`.

      Allowed values: WALK, BIKE, RENTAL, CAR, CAR_PARKING, CAR_DROPOFF,
      ODM, RIDE_SHARING, FLEX, DEBUG_BUS_ROUTE, DEBUG_RAILWAY_ROUTE,
      DEBUG_FERRY_ROUTE, TRANSIT, TRAM, SUBWAY, FERRY, AIRPLANE, BUS,
      COACH, RAIL, HIGHSPEED_RAIL, LONG_DISTANCE, NIGHT_RAIL,
      REGIONAL_FAST_RAIL, REGIONAL_RAIL, SUBURBAN, FUNICULAR,
      AERIAL_LIFT, OTHER, AREAL_LIFT, METRO, CABLE_CAR.

  `postTransitModes`

  :   Optional. Default is `WALK`. Only applies if the `to` place is a
      coordinate (not a transit stop). Does not apply to direct
      connections (see `directModes`).

      A list of modes that are allowed to be used from the last transit
      stop to the `to` coordinate. Example: `WALK,BIKE_SHARING`.

      Allowed values: WALK, BIKE, RENTAL, CAR, CAR_PARKING, CAR_DROPOFF,
      ODM, RIDE_SHARING, FLEX, DEBUG_BUS_ROUTE, DEBUG_RAILWAY_ROUTE,
      DEBUG_FERRY_ROUTE, TRANSIT, TRAM, SUBWAY, FERRY, AIRPLANE, BUS,
      COACH, RAIL, HIGHSPEED_RAIL, LONG_DISTANCE, NIGHT_RAIL,
      REGIONAL_FAST_RAIL, REGIONAL_RAIL, SUBURBAN, FUNICULAR,
      AERIAL_LIFT, OTHER, AREAL_LIFT, METRO, CABLE_CAR.

  `directRentalFormFactors`

  :   Experimental. Expect unannounced breaking changes (without version
      bumps).

      Optional. Only applies to direct connections.

      A list of vehicle type form factors that are allowed to be used
      for direct connections. If empty (the default), all form factors
      are allowed. Example: `BICYCLE,SCOOTER_STANDING`.

      Allowed values: BICYCLE, CARGO_BICYCLE, CAR, MOPED,
      SCOOTER_STANDING, SCOOTER_SEATED, OTHER.

  `preTransitRentalFormFactors`

  :   Experimental. Expect unannounced breaking changes (without version
      bumps).

      Optional. Only applies if the `from` place is a coordinate (not a
      transit stop). Does not apply to direct connections (see
      `directRentalFormFactors`).

      A list of vehicle type form factors that are allowed to be used
      from the `from` coordinate to the first transit stop. If empty
      (the default), all form factors are allowed. Example:
      `BICYCLE,SCOOTER_STANDING`.

      Allowed values: BICYCLE, CARGO_BICYCLE, CAR, MOPED,
      SCOOTER_STANDING, SCOOTER_SEATED, OTHER.

  `postTransitRentalFormFactors`

  :   Experimental. Expect unannounced breaking changes (without version
      bumps).

      Optional. Only applies if the `to` place is a coordinate (not a
      transit stop). Does not apply to direct connections (see
      `directRentalFormFactors`).

      A list of vehicle type form factors that are allowed to be used
      from the last transit stop to the `to` coordinate. If empty (the
      default), all form factors are allowed. Example:
      `BICYCLE,SCOOTER_STANDING`.

      Allowed values: BICYCLE, CARGO_BICYCLE, CAR, MOPED,
      SCOOTER_STANDING, SCOOTER_SEATED, OTHER.

  `directRentalPropulsionTypes`

  :   Experimental. Expect unannounced breaking changes (without version
      bumps).

      Optional. Only applies to direct connections.

      A list of vehicle type form factors that are allowed to be used
      for direct connections. If empty (the default), all propulsion
      types are allowed. Example: `HUMAN,ELECTRIC,ELECTRIC_ASSIST`.

      Allowed values: HUMAN, ELECTRIC_ASSIST, ELECTRIC, COMBUSTION,
      COMBUSTION_DIESEL, HYBRID, PLUG_IN_HYBRID, HYDROGEN_FUEL_CELL.

  `preTransitRentalPropulsionTypes`

  :   Experimental. Expect unannounced breaking changes (without version
      bumps).

      Optional. Only applies if the `from` place is a coordinate (not a
      transit stop). Does not apply to direct connections (see
      `directRentalPropulsionTypes`).

      A list of vehicle propulsion types that are allowed to be used
      from the `from` coordinate to the first transit stop. If empty
      (the default), all propulsion types are allowed. Example:
      `HUMAN,ELECTRIC,ELECTRIC_ASSIST`.

      Allowed values: HUMAN, ELECTRIC_ASSIST, ELECTRIC, COMBUSTION,
      COMBUSTION_DIESEL, HYBRID, PLUG_IN_HYBRID, HYDROGEN_FUEL_CELL.

  `postTransitRentalPropulsionTypes`

  :   Experimental. Expect unannounced breaking changes (without version
      bumps).

      Optional. Only applies if the `to` place is a coordinate (not a
      transit stop). Does not apply to direct connections (see
      `directRentalPropulsionTypes`).

      A list of vehicle propulsion types that are allowed to be used
      from the last transit stop to the `to` coordinate. If empty (the
      default), all propulsion types are allowed. Example:
      `HUMAN,ELECTRIC,ELECTRIC_ASSIST`.

      Allowed values: HUMAN, ELECTRIC_ASSIST, ELECTRIC, COMBUSTION,
      COMBUSTION_DIESEL, HYBRID, PLUG_IN_HYBRID, HYDROGEN_FUEL_CELL.

  `directRentalProviders`

  :   Experimental. Expect unannounced breaking changes (without version
      bumps).

      Optional. Only applies to direct connections.

      A list of rental providers that are allowed to be used for direct
      connections. If empty (the default), all providers are allowed.

  `directRentalProviderGroups`

  :   Experimental. Expect unannounced breaking changes (without version
      bumps).

      Optional. Only applies to direct connections.

      A list of rental provider groups that are allowed to be used for
      direct connections. If empty (the default), all providers are
      allowed.

  `preTransitRentalProviders`

  :   Experimental. Expect unannounced breaking changes (without version
      bumps).

      Optional. Only applies if the `from` place is a coordinate (not a
      transit stop). Does not apply to direct connections (see
      `directRentalProviders`).

      A list of rental providers that are allowed to be used from the
      `from` coordinate to the first transit stop. If empty (the
      default), all providers are allowed.

  `preTransitRentalProviderGroups`

  :   Experimental. Expect unannounced breaking changes (without version
      bumps).

      Optional. Only applies if the `from` place is a coordinate (not a
      transit stop). Does not apply to direct connections (see
      `directRentalProviderGroups`).

      A list of rental provider groups that are allowed to be used from
      the `from` coordinate to the first transit stop. If empty (the
      default), all providers are allowed.

  `postTransitRentalProviders`

  :   Experimental. Expect unannounced breaking changes (without version
      bumps).

      Optional. Only applies if the `to` place is a coordinate (not a
      transit stop). Does not apply to direct connections (see
      `directRentalProviders`).

      A list of rental providers that are allowed to be used from the
      last transit stop to the `to` coordinate. If empty (the default),
      all providers are allowed.

  `postTransitRentalProviderGroups`

  :   Experimental. Expect unannounced breaking changes (without version
      bumps).

      Optional. Only applies if the `to` place is a coordinate (not a
      transit stop). Does not apply to direct connections (see
      `directRentalProviderGroups`).

      A list of rental provider groups that are allowed to be used from
      the last transit stop to the `to` coordinate. If empty (the
      default), all providers are allowed.

  `ignoreDirectRentalReturnConstraints`

  :   Experimental. Expect unannounced breaking changes (without version
      bumps).

      Optional. Default is `false`.

      If set to `true`, the routing will ignore rental return
      constraints for direct connections, allowing the rental vehicle to
      be parked anywhere.

  `ignorePreTransitRentalReturnConstraints`

  :   Experimental. Expect unannounced breaking changes (without version
      bumps).

      Optional. Default is `false`.

      If set to `true`, the routing will ignore rental return
      constraints for the part from the `from` coordinate to the first
      transit stop, allowing the rental vehicle to be parked anywhere.

  `ignorePostTransitRentalReturnConstraints`

  :   Experimental. Expect unannounced breaking changes (without version
      bumps).

      Optional. Default is `false`.

      If set to `true`, the routing will ignore rental return
      constraints for the part from the last transit stop to the `to`
      coordinate, allowing the rental vehicle to be parked anywhere.

  `numItineraries`

  :   The minimum number of itineraries to compute. This is only
      relevant if `timetableView=true`. The default value is 5.

  `maxItineraries`

  :   Optional. By default all computed itineraries will be returned

      The maximum number of itineraries to compute. This is only
      relevant if `timetableView=true`.

      Note: With the current implementation, setting this to a lower
      number will not result in any speedup.

      Note: The number of returned itineraries might be slightly higher
      than `maxItineraries` as there might be several itineraries with
      the same departure time but different number of transfers. In
      order to not miss any itineraries for paging, either none or all
      itineraries with the same departure time have to be returned.

  `pageCursor`

  :   Use the cursor to go to the next "page" of itineraries. Copy the
      cursor from the last response and keep the original request as is.
      This will enable you to search for itineraries in the next or
      previous time-window.

  `timetableView`

  :   Optional. Default is `true`.

      Search for the best trip options within a time window. If true two
      itineraries are considered optimal if one is better on arrival
      time (earliest wins) and the other is better on departure time
      (latest wins). In combination with arriveBy this parameter cover
      the following use cases:

      `timetable=false` = waiting for the first transit
      departure/arrival is considered travel time:

      - `arriveBy=true`: event (e.g. a meeting) starts at 10:00 am,
        compute the best journeys that arrive by that time (maximizes
        departure time)

      - `arriveBy=false`: event (e.g. a meeting) ends at 11:00 am,
        compute the best journeys that depart after that time

      `timetable=true` = optimize "later departure" + "earlier arrival"
      and give all options over a time window:

      - `arriveBy=true`: the time window around `date` and `time` refers
        to the arrival time window

      - `arriveBy=false`: the time window around `date` and `time`
        refers to the departure time window

  `searchWindow`

  :   Optional. Default is 15 minutes which is `900`.

      The length of the search-window in seconds. Default value 15
      minutes.

      - `arriveBy=true`: number of seconds between the earliest
        departure time and latest departure time

      - `arriveBy=false`: number of seconds between the earliest arrival
        time and the latest arrival time

  `requireBikeTransport`

  :   Optional. Default is `false`.

      If set to `true`, all used transit trips are required to allow
      bike carriage.

  `requireCarTransport`

  :   Optional. Default is `false`.

      If set to `true`, all used transit trips are required to allow car
      carriage.

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
      seconds for direct connections. Is limited by server config
      variable `street_routing_max_direct_seconds`.

  `fastestDirectFactor`

  :   Optional. Experimental. Default is `1.0`. Factor with which the
      duration of the fastest direct non-public-transit connection is
      multiplied. Values \> 1.0 allow transit connections that are
      slower than the fastest direct non-public-transit connection to be
      found.

  `timeout`

  :   Optional. Query timeout in seconds.

  `passengers`

  :   Optional. Experimental. Number of passengers (e.g. for ODM or
      price calculation)

  `luggage`

  :   Optional. Experimental. Number of luggage pieces; base unit:
      airline cabin luggage (e.g. for ODM or price calculation)

  `slowDirect`

  :   Optional. Experimental. Adds overtaken direct public transit
      connections.

  `fastestSlowDirectFactor`

  :   Optional. Factor with which the duration of the fastest slowDirect
      connection is multiplied. Values \> 1.0 allow connections that are
      slower than the fastest direct transit connection to be found.
      Values \< 1.0 will return all slowDirect connections.

  `withFares`

  :   Optional. Experimental. If set to true, the response will contain
      fare information.

  `withScheduledSkippedStops`

  :   Optional. Include intermediate stops where passengers can not
      alight/board according to schedule.

  `language`

  :   language tags as used in OpenStreetMap / GTFS (usually BCP-47 /
      ISO 639-1, or ISO 639-2 if there's no ISO 639-1)

  `algorithm`

  :   algorithm to use

      Allowed values: RAPTOR, PONG, TB.

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

  - `"itineraries"` (default): An `sf` data frame of paired itineraries.

  - `"legs"`: An `sf` data frame of individual journey legs for paired
    journeys.

  - `"travel_time_matrix_long"`: A long-format data frame with travel
    times from all origins to all destinations.

  - `"travel_time_matrix_wide"`: A wide-format data frame (matrix) with
    travel times from all origins to all destinations.

  - `"raw_list"`: The raw parsed JSON response as a list.

- parallel:

  Logical. If `TRUE`, executes multiple requests in parallel. Defaults
  to `FALSE`.

## Value

Depending on the `output` parameter, an `sf` data frame, a regular data
frame, or a list.
