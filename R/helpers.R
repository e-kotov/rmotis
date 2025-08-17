# minimal internal %||% (no extra deps)
`%||%` <- function(x, y) if (!is.null(x)) x else y

# decode returns data.frame(lat, lon) or NULL
.decode_polyline <- function(points, precision = NULL) {
  if (is.null(points) || is.na(points) || !nzchar(points)) {
    return(NULL)
  }
  factor <- as.integer(precision %||% 5L)
  if (!requireNamespace("gepaf", quietly = TRUE)) {
    warning("Package 'gepaf' not available; returning NULL geometry.")
    return(NULL)
  }
  df <- gepaf::decodePolyline(enc_polyline = points, factor = factor)
  names(df) <- c("lat", "lon") # gepaf returns lat/lon in this order
  df
}

# Build sf LINESTRING if 'sf' is available (lon,lat order for sf)
.coords_to_sfc <- function(df) {
  if (is.null(df) || nrow(df) < 2L) {
    return(NA)
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    return(df)
  } # fall back to coords list
  m <- cbind(df$lon, df$lat)
  sf::st_sfc(sf::st_linestring(m), crs = 4326)
}

# get leg polyline points + precision from either 'legGeometry' or legacy 'polyline'
.leg_polyline <- function(leg) {
  if (!is.null(leg$legGeometry$points)) {
    list(
      points = leg$legGeometry$points,
      precision = leg$legGeometry$precision %||% 5L
    )
  } else if (!is.null(leg$polyline)) {
    # spec notes "polyline ... precision 5" for this field
    list(points = leg$polyline, precision = 5L) # fallback for older/alternative field
  } else {
    NULL
  }
}

# Parse httr2_response | raw | JSON string | already-parsed list
.as_plan_list <- function(x) {
  if (inherits(x, "httr2_response")) {
    return(RcppSimdJson::fparse(
      httr2::resp_body_raw(x),
      max_simplify_lvl = "list",
      type_policy = "numbers",
      int64_policy = "double"
    ))
  }
  if (is.raw(x) || (is.character(x) && length(x) == 1L)) {
    return(RcppSimdJson::fparse(
      x,
      max_simplify_lvl = "list",
      type_policy = "numbers",
      int64_policy = "double"
    ))
  }
  if (is.list(x)) {
    return(x)
  }
  stop(
    "Unsupported input to .as_plan_list(): ",
    paste(class(x), collapse = "/")
  )
}

.itins_template <- function() {
  dplyr::tibble(
    kind = character(),
    itin_id = integer(),
    duration = numeric(),
    startTime = character(),
    endTime = character(),
    transfers = integer()
  )
}

.legs_template <- function() {
  dplyr::tibble(
    kind = character(),
    itin_id = integer(),
    leg_index = integer(),
    mode = character(),
    from_name = character(),
    from_lat = numeric(),
    from_lon = numeric(),
    to_name = character(),
    to_lat = numeric(),
    to_lon = numeric(),
    startTime = character(),
    endTime = character(),
    distance = numeric()
  )
}

#' @keywords internal
.flatten_itineraries <- function(x, include_direct = FALSE) {
  res <- .as_plan_list(x)

  build <- function(itins, kind_label) {
    if (is.null(itins) || length(itins) == 0) {
      return(.itins_template())
    }
    purrr::imap_dfr(itins, function(it, i) {
      dplyr::tibble(
        kind = kind_label,
        itin_id = i,
        duration = it$duration %||% NA_real_,
        startTime = it$startTime %||% NA_character_,
        endTime = it$endTime %||% NA_character_,
        transfers = it$transfers %||% NA_integer_
      )
    })
  }

  main <- build(res$itineraries, "itinerary")
  direct <- if (isTRUE(include_direct)) {
    build(res$direct, "direct")
  } else {
    .itins_template()
  }
  dplyr::bind_rows(main, direct)
}


#' @keywords internal
.flatten_legs <- function(
  x,
  include_direct = FALSE,
  decode_geom = FALSE
) {
  res <- .as_plan_list(x)

  process_itineraries <- function(itins, kind_label) {
    if (is.null(itins) || length(itins) == 0) {
      return(NULL)
    }

    # 1. Get a single flat list of all legs
    all_legs <- purrr::list_flatten(purrr::map(itins, "legs"))
    n <- length(all_legs)
    if (n == 0) {
      return(NULL)
    }

    # 2. Pre-calculate IDs vectorially (fast setup)
    leg_counts <- purrr::map_int(itins, ~ length(.x$legs %||% list()))

    # 3. Pre-allocate all vectors (the key to speed)
    kind <- rep(kind_label, n)
    itin_id <- rep(seq_along(leg_counts), times = leg_counts)
    leg_index <- sequence(leg_counts)
    mode <- character(n)
    from_name <- character(n)
    from_lat <- numeric(n)
    from_lon <- numeric(n)
    to_name <- character(n)
    to_lat <- numeric(n)
    to_lon <- numeric(n)
    startTime <- character(n)
    endTime <- character(n)
    distance <- numeric(n)

    polyline_points <- if (isTRUE(decode_geom)) character(n) else NULL
    polyline_precision <- if (isTRUE(decode_geom)) integer(n) else NULL

    # 4. Single, fast loop to fill the vectors
    for (i in seq_len(n)) {
      l <- all_legs[[i]]
      mode[i] <- l$mode %||% NA_character_
      from_name[i] <- l$from$name %||% NA_character_
      from_lat[i] <- l$from$lat %||% NA_real_
      from_lon[i] <- l$from$lon %||% NA_real_
      to_name[i] <- l$to$name %||% NA_character_
      to_lat[i] <- l$to$lat %||% NA_real_
      to_lon[i] <- l$to$lon %||% NA_real_
      startTime[i] <- l$startTime %||%
        l$from$departure %||%
        l$from$scheduledDeparture %||%
        NA_character_
      endTime[i] <- l$endTime %||%
        l$to$arrival %||%
        l$to$scheduledArrival %||%
        NA_character_
      distance[i] <- l$distance %||% NA_real_
      if (isTRUE(decode_geom)) {
        polyline_points[i] <- l$legGeometry$points %||%
          l$polyline %||%
          NA_character_
        polyline_precision[i] <- l$legGeometry$precision %||% 5L
      }
    }

    # 5. Create the tibble once at the end
    out <- dplyr::tibble(
      kind,
      itin_id,
      leg_index,
      mode,
      from_name,
      from_lat,
      from_lon,
      to_name,
      to_lat,
      to_lon,
      startTime,
      endTime,
      distance
    )
    if (isTRUE(decode_geom)) {
      out$polyline_points <- polyline_points
      out$polyline_precision <- polyline_precision
    }
    out
  }

  main_legs <- process_itineraries(res$itineraries, "itinerary")
  direct_legs <- if (isTRUE(include_direct)) {
    process_itineraries(res$direct, "direct")
  } else {
    NULL
  }

  out <- dplyr::bind_rows(main_legs, direct_legs)

  if (nrow(out) == 0) {
    return(.legs_template())
  }

  if (isTRUE(decode_geom)) {
    polylines <- out$polyline_points
    precisions <- out$polyline_precision
    out <- dplyr::select(out, -polyline_points, -polyline_precision)

    valid_poly <- !is.na(polylines) & nzchar(polylines)

    if (
      any(valid_poly) && requireNamespace("googlePolylines", quietly = TRUE)
    ) {
      prec_vals <- precisions[valid_poly]
      prec_vals[is.na(prec_vals)] <- 5L
      u_prec <- unique(prec_vals)
      use_prec <- if (length(u_prec) > 1L) 5L else u_prec[1L]
      if (length(u_prec) > 1L) {
        warning("Mixed precisions found; decoding with 5.")
      }

      decoded_coords <- googlePolylines::decode(
        polylines[valid_poly],
        precision = use_prec
      )

      is_valid_geom <- vapply(
        decoded_coords,
        function(d) !is.null(d) && is.data.frame(d) && nrow(d) >= 2,
        logical(1)
      )

      if (
        any(is_valid_geom) &&
          requireNamespace("sfheaders", quietly = TRUE) &&
          requireNamespace("sf", quietly = TRUE)
      ) {
        valid_coords <- decoded_coords[is_valid_geom]
        ids <- rep(
          seq_along(valid_coords),
          times = vapply(valid_coords, nrow, integer(1))
        )

        coords_df <- dplyr::bind_rows(valid_coords)
        if (!all(c("lon", "lat") %in% names(coords_df))) {
          names(coords_df)[1:2] <- c("lon", "lat")
        }
        coords_df$id <- ids

        sfc <- sfheaders::sfc_linestring(
          coords_df,
          x = "lon",
          y = "lat",
          linestring_id = "id"
        )
        sfc <- sf::st_set_crs(sfc, 4326)

        geom_col <- sf::st_sfc(
          replicate(nrow(out), sf::st_linestring(), simplify = FALSE),
          crs = 4326
        )

        original_valid_indices <- which(valid_poly)[is_valid_geom]
        geom_col[original_valid_indices] <- sfc

        out$geom <- geom_col
        out <- sf::st_as_sf(out)
      } else {
        geom_list <- vector("list", nrow(out))
        geom_list[which(valid_poly)[is_valid_geom]] <- decoded_coords[
          is_valid_geom
        ]
        out$geom <- geom_list
      }
    } else {
      if (requireNamespace("sf", quietly = TRUE)) {
        out$geom <- sf::st_sfc(
          replicate(nrow(out), sf::st_linestring(), simplify = FALSE),
          crs = 4326
        )
        out <- sf::st_as_sf(out)
      } else {
        out$geom <- vector("list", nrow(out))
      }
    }
  }

  return(out)
}
