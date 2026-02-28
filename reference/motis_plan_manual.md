# Plan a journey (MOTIS /api/v4/plan)

Computes optimal connections from one place to another using the MOTIS
routing API v4. This wraps GET /api/v4/plan and exposes all documented
query parameters. Arrays are encoded as comma-separated values (per
OpenAPI explode: false).

## Usage

``` r
motis_plan_manual(
  fromPlace,
  toPlace,
  via = NULL,
  viaMinimumStay = NULL,
  time = NULL,
  maxTransfers = NULL,
  maxTravelTime = NULL,
  minTransferTime = NULL,
  additionalTransferTime = NULL,
  transferTimeFactor = NULL,
  maxMatchingDistance = NULL,
  pedestrianProfile = NULL,
  elevationCosts = NULL,
  useRoutedTransfers = NULL,
  detailedTransfers = TRUE,
  joinInterlinedLegs = NULL,
  transitModes = NULL,
  directModes = NULL,
  preTransitModes = NULL,
  postTransitModes = NULL,
  directRentalFormFactors = NULL,
  preTransitRentalFormFactors = NULL,
  postTransitRentalFormFactors = NULL,
  directRentalPropulsionTypes = NULL,
  preTransitRentalPropulsionTypes = NULL,
  postTransitRentalPropulsionTypes = NULL,
  directRentalProviders = NULL,
  preTransitRentalProviders = NULL,
  postTransitRentalProviders = NULL,
  ignoreDirectRentalReturnConstraints = NULL,
  ignorePreTransitRentalReturnConstraints = NULL,
  ignorePostTransitRentalReturnConstraints = NULL,
  numItineraries = NULL,
  pageCursor = NULL,
  timetableView = NULL,
  arriveBy = NULL,
  searchWindow = NULL,
  requireBikeTransport = NULL,
  requireCarTransport = NULL,
  maxPreTransitTime = NULL,
  maxPostTransitTime = NULL,
  maxDirectTime = NULL,
  fastestDirectFactor = NULL,
  timeout = NULL,
  passengers = NULL,
  luggage = NULL,
  slowDirect = NULL,
  fastestSlowDirectFactor = NULL,
  withFares = NULL,
  withScheduledSkippedStops = NULL,
  language = NULL,
  base_url = getOption("rmotis.base_url", "https://api.transitous.org/"),
  api_version = getOption("rmotis.api_version", "3"),
  req = NULL,
  return = "list"
)
```

## Arguments

- fromPlace:

  Character. Origin place ("lat,lon`[,level]`" or stop id). Required.

- toPlace:

  Character. Destination place ("lat,lon`[,level]`" or stop id).
  Required.

- via:

  Character vector (max length 2). Stop IDs to visit in order (coords
  not allowed). See also viaMinimumStay.

- viaMinimumStay:

  Integer vector (max length 2). Minimum stay (minutes) for each via. If
  omitted, server uses 0,0 (staying in the same trip allowed).

- time:

  POSIXct or RFC3339 string. Departure time (arriveBy=FALSE) or arrival
  time (arriveBy=TRUE). Defaults to server "now" if unset.

- maxTransfers:

  Integer. Max allowed transfers (see spec warnings: too low may miss
  optimal journeys / slow performance).

- maxTravelTime:

  Integer minutes. Max travel time. See warnings in spec.

- minTransferTime:

  Integer minutes. Default 0. Minimum transfer time.

- additionalTransferTime:

  Integer minutes. Default 0. Extra time reserved per transfer.

- transferTimeFactor:

  Numeric. Default 1.0 (\>=1). Multiplies min transfer times.

- maxMatchingDistance:

  Numeric meters. Default 25. Max distance to match coords to street
  network.

- pedestrianProfile:

  Character enum: "FOOT" (default), "WHEELCHAIR". Used for
  transfers/first-last mile.

- elevationCosts:

  Character enum: "NONE" (default), "LOW", "HIGH". Penalize incline for
  street segments (esp. BIKE).

- useRoutedTransfers:

  Logical. Default FALSE. Use OSM-routed transfers.

- detailedTransfers:

  Logical. Required by API. Default TRUE. If TRUE, compute transfer
  polylines and step instructions.

- joinInterlinedLegs:

  Logical. Default TRUE. If FALSE, keep stay-seated legs separate (marks
  interlineWithPreviousLeg=TRUE).

- transitModes:

  Character vector of modes. Default TRANSIT (all transit). Empty vector
  disables transit. See Mode enum in spec.

- directModes:

  Character vector of non-transit modes for direct trips (default WALK).
  Direct results returned under direct. See notes in spec.

- preTransitModes:

  Character vector. Default WALK. Allowed modes from from coordinate to
  first transit stop.

- postTransitModes:

  Character vector. Default WALK. Allowed modes from last transit stop
  to to coordinate.

- directRentalFormFactors, preTransitRentalFormFactors,
  postTransitRentalFormFactors:

  Character vectors (experimental). Allowed rental form factors (e.g.,
  BICYCLE, SCOOTER_STANDING).

- directRentalPropulsionTypes, preTransitRentalPropulsionTypes,
  postTransitRentalPropulsionTypes:

  Character vectors (experimental). Allowed propulsion types (e.g.,
  HUMAN,ELECTRIC).

- directRentalProviders, preTransitRentalProviders,
  postTransitRentalProviders:

  Character vectors (experimental). Allowed rental providers.

- ignoreDirectRentalReturnConstraints,
  ignorePreTransitRentalReturnConstraints,
  ignorePostTransitRentalReturnConstraints:

  Logical (experimental). If TRUE, ignore rental return constraints for
  the respective segments.

- numItineraries:

  Integer. Default 5. Used when timetableView=TRUE.

- pageCursor:

  Character. Cursor for paging (copy from previous response).

- timetableView:

  Logical. Default TRUE. Optimize "later departure" & "earlier arrival"
  over a time window (see spec details & examples).

- arriveBy:

  Logical. Default FALSE. If TRUE, time is arrival time; else departure
  time.

- searchWindow:

  Integer seconds. Default 7200 (2h). Window length (interaction with
  arriveBy per spec).

- requireBikeTransport, requireCarTransport:

  Logical. If TRUE, only trips that allow carriage of bike/car.

- maxPreTransitTime, maxPostTransitTime:

  Integer seconds. Default 900 (15m) for first/last street legs.

- maxDirectTime:

  Integer seconds. Default 1800 (30m) for direct connections.

- fastestDirectFactor:

  Numeric (experimental). Default 1.0. Allow transit options slower than
  the fastest direct non-transit by this factor.

- timeout:

  Integer seconds. API-side query timeout. (Not the HTTP client
  timeout.)

- passengers, luggage:

  Integer (experimental). Passenger and luggage counts (for ODM/price).

- slowDirect:

  Logical (experimental). Default TRUE. Add overtaken direct public
  transit connections.

- fastestSlowDirectFactor:

  Numeric (experimental). Default 3.0. Factor applied to fastest
  slowDirect connection.

- withFares:

  Logical (experimental). If TRUE, include fare information in response.

- withScheduledSkippedStops:

  Logical. If TRUE, include intermediate stops where alight/board is not
  allowed.

- language:

  Character BCP-47 (e.g., "en"). Label language for OSM/GTFS names.

- base_url:

  Base URL of the MOTIS server. Defaults to
  "https://api.transitous.org". See servers in the spec.

- api_version:

  API version to use. Defaults to "v3".

- req:

  Optional. An existing httr2 request to add path/query onto (advanced).

- return:

  Character. Specifies the return type. One of "list" (default, parsed
  from JSON), "itineraries" (a table of itineraries with combined leg
  geometries), "legs" (a table of individual journey legs), "response"
  (the full httr2 response object), "raw" (the raw response body), or
  "json" (the response body as a JSON string).

## Value

Varies based on the `return` parameter. See the documentation for the
`return` parameter for details.

## Required

- fromPlace, toPlace: Either "lat,lon`[,level]`" or a stop id. Level is
  optional (defaults to 0).

## Response

A named list with (at least) the following elements (see schema details
in the spec): requestParameters, debugOutput, from, to, direct,
itineraries, previousPageCursor, nextPageCursor.

## Examples

``` r
if (FALSE) { # \dontrun{
# Berlin Brandenburg Gate to Hamburg Hbf, walking as direct fallback:
res <- motis_plan(
fromPlace = "52.5163,13.3777",
toPlace = "53.5526,9.9943",
directModes = "WALK",
detailedTransfers = TRUE,
return = "list"
)
length(res$itineraries)

# Get itineraries as an sf data frame
itineraries_sf <- motis_plan(
fromPlace = "52.5163,13.3777",
toPlace = "53.5526,9.9943",
return = "itineraries"
)

# Get individual legs as an sf data frame
legs_sf <- motis_plan(
fromPlace = "52.5163,13.3777",
toPlace = "53.5526,9.9943",
return = "legs"
)
} # }
```
