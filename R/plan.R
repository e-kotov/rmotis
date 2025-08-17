#' Plan a journey (MOTIS /api/v4/plan)
#'
#' Computes optimal connections from one place to another using the MOTIS routing API v4.
#' This wraps `GET /api/v4/plan` and exposes all documented query parameters.
#' Arrays are encoded as comma-separated values (per OpenAPI `explode: false`).
#'
#' @section Required:
#' - `fromPlace`, `toPlace`: Either `"lat,lon[,level]"` or a stop id. Level is optional (defaults to 0). :contentReference[oaicite:1]{index=1}
#'
#' @section Response:
#' A named list with (at least) the following elements (see schema details in the spec):
#' `requestParameters`, `debugOutput`, `from`, `to`, `direct`, `itineraries`, `previousPageCursor`, `nextPageCursor`. :contentReference[oaicite:2]{index=2}
#'
#' @param fromPlace Character. Origin place (`"lat,lon[,level]"` or stop id). **Required**. :contentReference[oaicite:3]{index=3}
#' @param toPlace Character. Destination place (`"lat,lon[,level]"` or stop id). **Required**. :contentReference[oaicite:4]{index=4}
#' @param via Character vector (max length 2). Stop IDs to visit in order (coords not allowed). See also `viaMinimumStay`. :contentReference[oaicite:5]{index=5}
#' @param viaMinimumStay Integer vector (max length 2). Minimum stay (minutes) for each `via`. If omitted, server uses `0,0` (staying in the same trip allowed). :contentReference[oaicite:6]{index=6}
#' @param time POSIXct or RFC3339 string. Departure time (`arriveBy=FALSE`) or arrival time (`arriveBy=TRUE`). Defaults to server “now” if unset. :contentReference[oaicite:7]{index=7}
#' @param maxTransfers Integer. Max allowed transfers (see spec warnings: too low may miss optimal journeys / slow performance). :contentReference[oaicite:8]{index=8}
#' @param maxTravelTime Integer minutes. Max travel time. See warnings in spec. :contentReference[oaicite:9]{index=9}
#' @param minTransferTime Integer minutes. Default 0. Minimum transfer time. :contentReference[oaicite:10]{index=10}
#' @param additionalTransferTime Integer minutes. Default 0. Extra time reserved per transfer. :contentReference[oaicite:11]{index=11}
#' @param transferTimeFactor Numeric. Default 1.0 (≥1). Multiplies min transfer times. :contentReference[oaicite:12]{index=12}
#' @param maxMatchingDistance Numeric meters. Default 25. Max distance to match coords to street network. :contentReference[oaicite:13]{index=13}
#' @param pedestrianProfile Character enum: `"FOOT"` (default), `"WHEELCHAIR"`. Used for transfers/first–last mile. :contentReference[oaicite:14]{index=14}
#' @param elevationCosts Character enum: `"NONE"` (default), `"LOW"`, `"HIGH"`. Penalize incline for street segments (esp. `BIKE`). :contentReference[oaicite:15]{index=15}
#' @param useRoutedTransfers Logical. Default `FALSE`. Use OSM-routed transfers. :contentReference[oaicite:16]{index=16}
#' @param detailedTransfers Logical. **Required by API**. Default `TRUE`. If `TRUE`, compute transfer polylines and step instructions. :contentReference[oaicite:17]{index=17}
#' @param joinInterlinedLegs Logical. Default `TRUE`. If `FALSE`, keep stay-seated legs separate (marks `interlineWithPreviousLeg=TRUE`). :contentReference[oaicite:18]{index=18}
#' @param transitModes Character vector of modes. Default `TRANSIT` (all transit). Empty vector disables transit. See `Mode` enum in spec. :contentReference[oaicite:19]{index=19}
#' @param directModes Character vector of non-transit modes for direct trips (default `WALK`). Direct results returned under `direct`. See notes in spec. :contentReference[oaicite:20]{index=20}
#' @param preTransitModes Character vector. Default `WALK`. Allowed modes from `from` coordinate to first transit stop. :contentReference[oaicite:21]{index=21}
#' @param postTransitModes Character vector. Default `WALK`. Allowed modes from last transit stop to `to` coordinate. :contentReference[oaicite:22]{index=22}
#' @param directRentalFormFactors,preTransitRentalFormFactors,postTransitRentalFormFactors Character vectors (experimental). Allowed rental form factors (e.g., `BICYCLE`, `SCOOTER_STANDING`). :contentReference[oaicite:23]{index=23}
#' @param directRentalPropulsionTypes,preTransitRentalPropulsionTypes,postTransitRentalPropulsionTypes Character vectors (experimental). Allowed propulsion types (e.g., `HUMAN`,`ELECTRIC`). :contentReference[oaicite:24]{index=24}
#' @param directRentalProviders,preTransitRentalProviders,postTransitRentalProviders Character vectors (experimental). Allowed rental providers. :contentReference[oaicite:25]{index=25}
#' @param ignoreDirectRentalReturnConstraints,ignorePreTransitRentalReturnConstraints,ignorePostTransitRentalReturnConstraints Logical (experimental). If `TRUE`, ignore rental return constraints for the respective segments. :contentReference[oaicite:26]{index=26}
#' @param numItineraries Integer. Default 5. Used when `timetableView=TRUE`. :contentReference[oaicite:27]{index=27}
#' @param pageCursor Character. Cursor for paging (copy from previous response). :contentReference[oaicite:28]{index=28}
#' @param timetableView Logical. Default `TRUE`. Optimize “later departure” & “earlier arrival” over a time window (see spec details & examples). :contentReference[oaicite:29]{index=29}
#' @param arriveBy Logical. Default `FALSE`. If `TRUE`, `time` is arrival time; else departure time. :contentReference[oaicite:30]{index=30}
#' @param searchWindow Integer seconds. Default 7200 (2h). Window length (interaction with `arriveBy` per spec). :contentReference[oaicite:31]{index=31}
#' @param requireBikeTransport,requireCarTransport Logical. If `TRUE`, only trips that allow carriage of bike/car. :contentReference[oaicite:32]{index=32}
#' @param maxPreTransitTime,maxPostTransitTime Integer seconds. Default 900 (15m) for first/last street legs. :contentReference[oaicite:33]{index=33}
#' @param maxDirectTime Integer seconds. Default 1800 (30m) for direct connections. :contentReference[oaicite:34]{index=34}
#' @param fastestDirectFactor Numeric (experimental). Default 1.0. Allow transit options slower than the fastest direct non-transit by this factor. :contentReference[oaicite:35]{index=35}
#' @param timeout Integer seconds. API-side query timeout. (Not the HTTP client timeout.) :contentReference[oaicite:36]{index=36}
#' @param passengers,luggage Integer (experimental). Passenger and luggage counts (for ODM/price). :contentReference[oaicite:37]{index=37}
#' @param slowDirect Logical (experimental). Default `TRUE`. Add overtaken direct public transit connections. :contentReference[oaicite:38]{index=38}
#' @param fastestSlowDirectFactor Numeric (experimental). Default 3.0. Factor applied to fastest `slowDirect` connection. :contentReference[oaicite:39]{index=39}
#' @param withFares Logical (experimental). If `TRUE`, include fare information in response. :contentReference[oaicite:40]{index=40}
#' @param withScheduledSkippedStops Logical. If `TRUE`, include intermediate stops where alight/board is not allowed. :contentReference[oaicite:41]{index=41}
#' @param language Character BCP-47 (e.g., `"en"`). Label language for OSM/GTFS names. :contentReference[oaicite:42]{index=42}
#' @param api_version API version to use. Defaults to `"v3"`.
#' @param base_url Base URL of the MOTIS server. Defaults to `"https://api.transitous.org"`. See servers in the spec. :contentReference[oaicite:43]{index=43}
#' @param req Optional. An existing httr2 request to add path/query onto (advanced).
#' @param return Character. Defaults to `"parsed"`. If `"response"`, returns the full httr2 response object; if `"raw"`, returns the raw body (JSON). :contentReference[oaicite:44]{index=44}
#' @return List parsed from JSON. See spec for object shapes (Itinerary, Leg, Place, etc.). :contentReference[oaicite:44]{index=44}
#'
#' @examples
#' \dontrun{
#' # Berlin Brandenburg Gate to Hamburg Hbf, walking as direct fallback:
#' res <- motis_plan(
#'   fromPlace = "52.5163,13.3777",
#'   toPlace   = "53.5526,9.9943",
#'   directModes = "WALK",
#'   detailedTransfers = TRUE
#' )
#' length(res$itineraries)
#' }
#' @seealso [rm_trip()], [rm_stoptimes()] (endpoints `/api/v4/trip`, `/api/v4/stoptimes`)
#' @export
motis_plan <- function(
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
  return = "parsed" # "response", "raw""
) {
  if (missing(fromPlace) || missing(toPlace)) {
    stop("`fromPlace` and `toPlace` are required.", call. = FALSE)
  }

  # helpers ---------------------------------------------------------------
  collapse <- function(x) {
    if (is.null(x)) NULL else paste(as.character(x), collapse = ",")
  }
  compact <- function(x) Filter(function(y) !is.null(y), x)
  fmt_time <- function(x) {
    if (inherits(x, "POSIXt")) {
      # RFC3339 (Z) in UTC
      format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    } else if (is.null(x)) {
      NULL
    } else {
      as.character(x)
    }
  }

  # Prepare query (arrays use comma-separated encoding)
  q <- compact(list(
    fromPlace = fromPlace,
    toPlace = toPlace,
    via = collapse(via),
    viaMinimumStay = collapse(viaMinimumStay),
    time = fmt_time(time),
    maxTransfers = maxTransfers,
    maxTravelTime = maxTravelTime,
    minTransferTime = minTransferTime,
    additionalTransferTime = additionalTransferTime,
    transferTimeFactor = transferTimeFactor,
    maxMatchingDistance = maxMatchingDistance,
    pedestrianProfile = pedestrianProfile,
    elevationCosts = elevationCosts,
    useRoutedTransfers = useRoutedTransfers,
    detailedTransfers = detailedTransfers,
    joinInterlinedLegs = joinInterlinedLegs,
    transitModes = collapse(transitModes),
    directModes = collapse(directModes),
    preTransitModes = collapse(preTransitModes),
    postTransitModes = collapse(postTransitModes),
    directRentalFormFactors = collapse(directRentalFormFactors),
    preTransitRentalFormFactors = collapse(preTransitRentalFormFactors),
    postTransitRentalFormFactors = collapse(postTransitRentalFormFactors),
    directRentalPropulsionTypes = collapse(directRentalPropulsionTypes),
    preTransitRentalPropulsionTypes = collapse(preTransitRentalPropulsionTypes),
    postTransitRentalPropulsionTypes = collapse(
      postTransitRentalPropulsionTypes
    ),
    directRentalProviders = collapse(directRentalProviders),
    preTransitRentalProviders = collapse(preTransitRentalProviders),
    postTransitRentalProviders = collapse(postTransitRentalProviders),
    ignoreDirectRentalReturnConstraints = ignoreDirectRentalReturnConstraints,
    ignorePreTransitRentalReturnConstraints = ignorePreTransitRentalReturnConstraints,
    ignorePostTransitRentalReturnConstraints = ignorePostTransitRentalReturnConstraints,
    numItineraries = numItineraries,
    pageCursor = pageCursor,
    timetableView = timetableView,
    arriveBy = arriveBy,
    searchWindow = searchWindow,
    requireBikeTransport = requireBikeTransport,
    requireCarTransport = requireCarTransport,
    maxPreTransitTime = maxPreTransitTime,
    maxPostTransitTime = maxPostTransitTime,
    maxDirectTime = maxDirectTime,
    fastestDirectFactor = fastestDirectFactor,
    timeout = timeout,
    passengers = passengers,
    luggage = luggage,
    slowDirect = slowDirect,
    fastestSlowDirectFactor = fastestSlowDirectFactor,
    withFares = withFares,
    withScheduledSkippedStops = withScheduledSkippedStops,
    language = language
  ))

  # Build request ---------------------------------------------------------
  if (is.null(req)) {
    req <- httr2::request(base_url)
  }
  req <- req |>
    httr2::req_url_path_append(glue::glue("api/v{api_version}/plan")) |>
    httr2::req_url_query(!!!q) |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_user_agent(paste0("rmotis (R/", getRversion(), ")"))

  # Perform ---------------------------------------------------------------
  resp <- httr2::req_perform(req)
  httr2::resp_check_status(resp)

  if (return == "response") {
    return(resp)
  } else if (return == "raw") {
    return(httr2::resp_body_raw(resp))
  } else if (return == "json") {
    return(httr2::resp_body_string(resp))
  } else if (return == "list") {
    json_parsed_to_list <- RcppSimdJson::fparse(
      httr2::resp_body_raw(resp),
      max_simplify_lvl = "list",
      type_policy = "numbers",
      int64_policy = "double",
      always_list = FALSE
    )
    return(json_parsed_to_list)
  } else if (return == "tables") {
    # Flatten both itineraries and legs, ensuring direct routes are included in both
    itineraries <- .flatten_itineraries(resp, include_direct = TRUE)
    legs <- .flatten_legs(resp, decode_geom = TRUE, include_direct = TRUE)

    # Proceed only if there are itineraries to process
    if (nrow(itineraries) == 0) {
      return(itineraries) # Return empty tibble if no results
    }

    # Ensure legs were returned and have geometry to process
    if (nrow(legs) > 0 && "geom" %in% names(legs)) {
      # Aggregate leg geometries for each itinerary
      itinerary_geoms <- legs |>
        # Group by the unique identifier for an itinerary
        dplyr::group_by(.data$kind, .data$itin_id) |>
        # Combine all LINESTRINGs within a group into a MULTILINESTRING
        dplyr::summarise(geom = sf::st_combine(.data$geom), .groups = "drop")

      # Join the aggregated geometries back to the itineraries table
      itineraries <- itineraries |>
        dplyr::left_join(itinerary_geoms, by = c("kind", "itin_id")) |>
        sf::st_as_sf() # Ensure the final object is an sf data frame
    } else {
      # If no legs/geometries, create an empty geometry column
      # to ensure the function returns a consistent object type (sf)
      itineraries$geom <- sf::st_sfc(
        lapply(1:nrow(itineraries), function(i) sf::st_multilinestring()),
        crs = 4326
      )
      itineraries <- sf::st_as_sf(itineraries)
    }
    return(itineraries)
  }
}
