#' Plan a journey (MOTIS /api/v4/plan)
#'
#' Computes optimal connections from one place to another using the MOTIS routing API v4.
#' This wraps GET /api/v4/plan and exposes all documented query parameters.
#' Arrays are encoded as comma-separated values (per OpenAPI explode: false).
#'
#' @section Required:
#' - fromPlace, toPlace: Either "lat,lon[,level]" or a stop id. Level is optional (defaults to 0).
#'
#' @section Response:
#' A named list with (at least) the following elements (see schema details in the spec):
#' requestParameters, debugOutput, from, to, direct, itineraries, previousPageCursor, nextPageCursor.
#'
#' @param fromPlace Character. Origin place ("lat,lon[,level]" or stop id). Required.
#' @param toPlace Character. Destination place ("lat,lon[,level]" or stop id). Required.
#' @param via Character vector (max length 2). Stop IDs to visit in order (coords not allowed). See also viaMinimumStay.
#' @param viaMinimumStay Integer vector (max length 2). Minimum stay (minutes) for each via. If omitted, server uses 0,0 (staying in the same trip allowed).
#' @param time POSIXct or RFC3339 string. Departure time (arriveBy=FALSE) or arrival time (arriveBy=TRUE). Defaults to server ‚Äúnow‚Äù if unset.
#' @param maxTransfers Integer. Max allowed transfers (see spec warnings: too low may miss optimal journeys / slow performance).
#' @param maxTravelTime Integer minutes. Max travel time. See warnings in spec.
#' @param minTransferTime Integer minutes. Default 0. Minimum transfer time.
#' @param additionalTransferTime Integer minutes. Default 0. Extra time reserved per transfer.
#' @param transferTimeFactor Numeric. Default 1.0 (‚â•1). Multiplies min transfer times.
#' @param maxMatchingDistance Numeric meters. Default 25. Max distance to match coords to street network.
#' @param pedestrianProfile Character enum: "FOOT" (default), "WHEELCHAIR". Used for transfers/first‚Äìlast mile.
#' @param elevationCosts Character enum: "NONE" (default), "LOW", "HIGH". Penalize incline for street segments (esp. BIKE).
#' @param useRoutedTransfers Logical. Default FALSE. Use OSM-routed transfers.
#' @param detailedTransfers Logical. Required by API. Default TRUE. If TRUE, compute transfer polylines and step instructions.
#' @param joinInterlinedLegs Logical. Default TRUE. If FALSE, keep stay-seated legs separate (marks interlineWithPreviousLeg=TRUE).
#' @param transitModes Character vector of modes. Default TRANSIT (all transit). Empty vector disables transit. See Mode enum in spec.
#' @param directModes Character vector of non-transit modes for direct trips (default WALK). Direct results returned under direct. See notes in spec.
#' @param preTransitModes Character vector. Default WALK. Allowed modes from from coordinate to first transit stop.
#' @param postTransitModes Character vector. Default WALK. Allowed modes from last transit stop to to coordinate.
#' @param directRentalFormFactors,preTransitRentalFormFactors,postTransitRentalFormFactors Character vectors (experimental). Allowed rental form factors (e.g., BICYCLE, SCOOTER_STANDING).
#' @param directRentalPropulsionTypes,preTransitRentalPropulsionTypes,postTransitRentalPropulsionTypes Character vectors (experimental). Allowed propulsion types (e.g., HUMAN,ELECTRIC).
#' @param directRentalProviders,preTransitRentalProviders,postTransitRentalProviders Character vectors (experimental). Allowed rental providers.
#' @param ignoreDirectRentalReturnConstraints,ignorePreTransitRentalReturnConstraints,ignorePostTransitRentalReturnConstraints Logical (experimental). If TRUE, ignore rental return constraints for the respective segments.
#' @param numItineraries Integer. Default 5. Used when timetableView=TRUE.
#' @param pageCursor Character. Cursor for paging (copy from previous response).
#' @param timetableView Logical. Default TRUE. Optimize ‚Äúlater departure‚Äù & ‚Äúearlier arrival‚Äù over a time window (see spec details & examples).
#' @param arriveBy Logical. Default FALSE. If TRUE, time is arrival time; else departure time.
#' @param searchWindow Integer seconds. Default 7200 (2h). Window length (interaction with arriveBy per spec).
#' @param requireBikeTransport,requireCarTransport Logical. If TRUE, only trips that allow carriage of bike/car.
#' @param maxPreTransitTime,maxPostTransitTime Integer seconds. Default 900 (15m) for first/last street legs.
#' @param maxDirectTime Integer seconds. Default 1800 (30m) for direct connections.
#' @param fastestDirectFactor Numeric (experimental). Default 1.0. Allow transit options slower than the fastest direct non-transit by this factor.
#' @param timeout Integer seconds. API-side query timeout. (Not the HTTP client timeout.)
#' @param passengers,luggage Integer (experimental). Passenger and luggage counts (for ODM/price).
#' @param slowDirect Logical (experimental). Default TRUE. Add overtaken direct public transit connections.
#' @param fastestSlowDirectFactor Numeric (experimental). Default 3.0. Factor applied to fastest slowDirect connection.
#' @param withFares Logical (experimental). If TRUE, include fare information in response.
#' @param withScheduledSkippedStops Logical. If TRUE, include intermediate stops where alight/board is not allowed.
#' @param language Character BCP-47 (e.g., "en"). Label language for OSM/GTFS names.
#' @param api_version API version to use. Defaults to "v3".
#' @param base_url Base URL of the MOTIS server. Defaults to "https://api.transitous.org". See servers in the spec.
#' @param req Optional. An existing httr2 request to add path/query onto (advanced).
#' @param return Character. Specifies the return type. One of "list" (default, parsed from JSON), "itineraries" (a table of itineraries with combined leg geometries), "legs" (a table of individual journey legs), "response" (the full httr2 response object), "raw" (the raw response body), or "json" (the response body as a JSON string).
#' @return Varies based on the `return` parameter. See the documentation for the `return` parameter for details.
#'
#' @examples
#' \dontrun{
#' # Berlin Brandenburg Gate to Hamburg Hbf, walking as direct fallback:
#' res <- motis_plan(
#' fromPlace = "52.5163,13.3777",
#' toPlace = "53.5526,9.9943",
#' directModes = "WALK",
#' detailedTransfers = TRUE,
#' return = "list"
#' )
#' length(res$itineraries)
#'
#' # Get itineraries as an sf data frame
#' itineraries_sf <- motis_plan(
#' fromPlace = "52.5163,13.3777",
#' toPlace = "53.5526,9.9943",
#' return = "itineraries"
#' )
#'
#' # Get individual legs as an sf data frame
#' legs_sf <- motis_plan(
#' fromPlace = "52.5163,13.3777",
#' toPlace = "53.5526,9.9943",
#' return = "legs"
#' )
#' }
#' @seealso [rm_trip()], [rm_stoptimes()] (endpoints /api/v4/trip, /api/v4/stoptimes)
#' @export
motis_plan_manual <- function(
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
) {
  if (missing(fromPlace) || missing(toPlace)) {
    stop("fromPlace and toPlace are required.", call. = FALSE)
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
  } else if (return %in% c("list", "parsed")) {
    return(RcppSimdJson::fparse(
      httr2::resp_body_raw(resp),
      max_simplify_lvl = "list",
      type_policy = "numbers",
      int64_policy = "double"
    ))
  } else if (return == "itineraries") {
    itineraries <- .flatten_itineraries(resp, include_direct = TRUE)
    legs <- .flatten_legs(resp, decode_geom = TRUE, include_direct = TRUE)

    if (nrow(itineraries) == 0) {
      return(itineraries)
    }

    if (nrow(legs) > 0 && "geom" %in% names(legs)) {
      itinerary_geoms <- legs |>
        dplyr::group_by(.data$kind, .data$itin_id) |>
        dplyr::summarise(geom = sf::st_combine(.data$geom), .groups = "drop")

      itineraries <- itineraries |>
        dplyr::left_join(itinerary_geoms, by = c("kind", "itin_id")) |>
        sf::st_as_sf()
    } else {
      itineraries$geom <- sf::st_sfc(
        lapply(1:nrow(itineraries), function(i) sf::st_multilinestring()),
        crs = 4326
      )
      itineraries <- sf::st_as_sf(itineraries)
    }
    return(itineraries)
  } else if (return == "legs") {
    return(.flatten_legs(resp, decode_geom = TRUE, include_direct = TRUE))
  } else {
    stop(paste("Unsupported return type:", return), call. = FALSE)
  }
}
