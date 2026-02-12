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
    distance = numeric(),
    startTime = character(),
    endTime = character(),
    transfers = integer(),
    geom = list()
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
    duration = numeric(),
    distance = numeric(),
    geom = list()
  )
}

#' @keywords internal
.flatten_itineraries <- function(x, include_direct = FALSE, decode_geom = FALSE) {
  res <- .as_plan_list(x)

  build <- function(itins, kind_label) {
    if (is.null(itins) || length(itins) == 0) {
      return(.itins_template())
    }
    
    itins_df <- purrr::imap_dfr(itins, function(it, i) {
      dplyr::tibble(
        kind = kind_label,
        itin_id = i,
        duration = it$duration %||% NA_real_,
        distance = sum(purrr::map_dbl(it$legs, ~ .x$distance %||% 0), na.rm = TRUE),
        startTime = it$startTime %||% NA_character_,
        endTime = it$endTime %||% NA_character_,
        transfers = it$transfers %||% NA_integer_
      )
    })

    itins_df$geom <- vector("list", nrow(itins_df))

    if (isTRUE(decode_geom) && requireNamespace("googlePolylines", quietly = TRUE)) {
      itins_df$geom <- purrr::map(itins, function(it) {
        leg_polys <- purrr::map(it$legs, .leg_polyline) |> purrr::compact()
        if (length(leg_polys) == 0) return(NULL)
        
        decoded <- purrr::map(leg_polys, function(lp) {
          d <- googlePolylines::decode(lp$points, precision = lp$precision)[[1]]
          if (!all(c("lon", "lat") %in% names(d))) {
             if (ncol(d) >= 2) names(d)[1:2] <- c("lat", "lon")
          }
          d
        })
        dplyr::bind_rows(decoded)
      })
    }
    itins_df
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

    # 3. Create the tibble
    out <- dplyr::tibble(
      kind = rep(kind_label, n),
      itin_id = rep(seq_along(leg_counts), times = leg_counts),
      leg_index = sequence(leg_counts),
      mode = character(n),
      from_name = character(n),
      from_lat = numeric(n),
      from_lon = numeric(n),
      to_name = character(n),
      to_lat = numeric(n),
      to_lon = numeric(n),
      startTime = character(n),
      endTime = character(n),
      duration = numeric(n),
      distance = numeric(n),
      geom = vector("list", n)
    )

    # 4. Single, fast loop to fill the vectors
    for (i in seq_len(n)) {
      l <- all_legs[[i]]
      out$mode[i] <- l$mode %||% NA_character_
      out$from_name[i] <- l$from$name %||% NA_character_
      out$from_lat[i] <- l$from$lat %||% NA_real_
      out$from_lon[i] <- l$from$lon %||% NA_real_
      out$to_name[i] <- l$to$name %||% NA_character_
      out$to_lat[i] <- l$to$lat %||% NA_real_
      out$to_lon[i] <- l$to$lon %||% NA_real_
      out$startTime[i] <- l$startTime %||%
        l$from$departure %||%
        l$from$scheduledDeparture %||%
        NA_character_
      out$endTime[i] <- l$endTime %||%
        l$to$arrival %||%
        l$to$scheduledArrival %||%
        NA_character_
      out$duration[i] <- l$duration %||% NA_real_
      out$distance[i] <- l$distance %||% NA_real_
      
      if (isTRUE(decode_geom) && requireNamespace("googlePolylines", quietly = TRUE)) {
        lp <- .leg_polyline(l)
        if (!is.null(lp)) {
          d <- googlePolylines::decode(lp$points, precision = lp$precision)[[1]]
          if (!all(c("lon", "lat") %in% names(d))) {
             if (ncol(d) >= 2) names(d)[1:2] <- c("lat", "lon")
          }
          out$geom[[i]] <- d
        }
      }
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
  if (is.null(out) || nrow(out) == 0) return(.legs_template())
  out
}

#' Internal helper to convert plan tibble to sf
#' @param df A tibble from .flatten_itineraries or .flatten_legs
#' @return An sf object if possible, otherwise the original tibble
#' @noRd
.st_as_sf_plan <- function(df) {
  if (!requireNamespace("sf", quietly = TRUE) || !"geom" %in% names(df)) {
    return(df)
  }
  
  if (inherits(df$geom, "sfc")) {
    return(sf::st_as_sf(df))
  }
  
  if (is.list(df$geom)) {
    # Check if we have valid geometries (data frames with at least 2 points)
    valid_idx <- which(vapply(df$geom, function(x) !is.null(x) && is.data.frame(x) && nrow(x) >= 2, logical(1)))
    
    if (length(valid_idx) == 0) {
      df$geom <- sf::st_sfc(replicate(nrow(df), sf::st_linestring(), simplify = FALSE), crs = 4326)
      return(sf::st_as_sf(df))
    }
    
    if (requireNamespace("sfheaders", quietly = TRUE)) {
      geom_dfs <- df$geom[valid_idx]
      names(geom_dfs) <- valid_idx
      long_geoms <- dplyr::bind_rows(geom_dfs, .id = "id")
      long_geoms$id <- as.character(long_geoms$id)
      
      sfc <- sfheaders::sfc_linestring(
        long_geoms,
        x = "lon",
        y = "lat",
        linestring_id = "id"
      )
      sfc <- sf::st_set_crs(sfc, 4326)
      
      full_sfc <- sf::st_sfc(replicate(nrow(df), sf::st_linestring(), simplify = FALSE), crs = 4326)
      full_sfc[valid_idx] <- sfc
      df$geom <- full_sfc
      return(sf::st_as_sf(df))
    } else {
      df$geom <- purrr::map(df$geom, function(x) {
        if (is.null(x) || !is.data.frame(x) || nrow(x) < 2) {
          sf::st_linestring()
        } else {
          # Assume lat/lon are first two columns if names mismatch
          mat <- if (all(c("lon", "lat") %in% names(x))) {
            as.matrix(x[, c("lon", "lat")])
          } else {
            as.matrix(x[, 2:1, drop = FALSE]) # googlePolylines returns lat, lon
          }
          sf::st_linestring(mat)
        }
      }) |> sf::st_sfc(crs = 4326)
      return(sf::st_as_sf(df))
    }
  }
  
  return(df)
}
