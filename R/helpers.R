# R/helpers.R optimized for memory

# minimal internal %||% (no extra deps)
`%||%` <- function(x, y) if (!is.null(x)) x else y

# get leg polyline points + precision from either 'legGeometry' or legacy 'polyline'
.leg_polyline <- function(leg) {
  p <- leg[["legGeometry"]][["points"]]
  if (!is.null(p)) {
    return(list(points = p, precision = leg[["legGeometry"]][["precision"]] %||% 5L))
  }
  p <- leg[["polyline"]]
  if (!is.null(p)) {
    return(list(points = p, precision = 5L))
  }
  NULL
}

# Parse httr2_response | raw | JSON string | already-parsed list
.as_plan_list <- function(x) {
  if (inherits(x, "httr2_response")) {
    return(RcppSimdJson::fparse(httr2::resp_body_raw(x), max_simplify_lvl = "list", type_policy = "numbers", int64_policy = "double"))
  }
  if (is.raw(x) || (is.character(x) && length(x) == 1L)) {
    return(RcppSimdJson::fparse(x, max_simplify_lvl = "list", type_policy = "numbers", int64_policy = "double"))
  }
  if (is.list(x)) return(x)
  stop("Unsupported input to .as_plan_list()")
}

.itins_template <- function() {
  dplyr::tibble(
    kind = character(), itin_id = integer(), duration = numeric(), distance = numeric(),
    startTime = character(), endTime = character(), transfers = integer(), geom = list()
  )
}

.legs_template <- function() {
  dplyr::tibble(
    kind = character(), itin_id = integer(), leg_index = integer(), mode = character(),
    from_name = character(), from_lat = numeric(), from_lon = numeric(),
    to_name = character(), to_lat = numeric(), to_lon = numeric(),
    startTime = character(), endTime = character(), duration = numeric(), distance = numeric(), geom = list()
  )
}

#' @keywords internal
.flatten_itineraries <- function(x, include_direct = FALSE, decode_geom = FALSE) {
  res <- .as_plan_list(x)
  
  process <- function(itins, kind_label) {
    if (is.null(itins) || length(itins) == 0) return(.itins_template())
    n <- length(itins)
    
    duration <- numeric(n); distance <- numeric(n); startTime <- character(n)
    endTime <- character(n); transfers <- integer(n); geom_info <- vector("list", n)
    
    for (i in seq_len(n)) {
      it <- itins[[i]]
      duration[i] <- it[["duration"]] %||% NA_real_
      startTime[i] <- it[["startTime"]] %||% NA_character_
      endTime[i] <- it[["endTime"]] %||% NA_character_
      transfers[i] <- it[["transfers"]] %||% NA_integer_
      
      dist_sum <- 0
      legs <- it[["legs"]]
      n_legs <- length(legs)
      it_polys <- if (decode_geom) vector("list", n_legs) else NULL
      
      if (n_legs > 0) {
        for (j in seq_len(n_legs)) {
          l <- legs[[j]]
          dist_sum <- dist_sum + (l[["distance"]] %||% 0)
          if (decode_geom) {
            lp <- .leg_polyline(l)
            if (!is.null(lp)) it_polys[[j]] <- lp
          }
        }
      }
      distance[i] <- dist_sum
      if (decode_geom) geom_info[[i]] <- if (all(vapply(it_polys, is.null, logical(1)))) NULL else it_polys
    }
    
    dplyr::tibble(
      kind = kind_label, itin_id = seq_len(n), duration, distance, startTime, endTime, transfers, geom = geom_info
    )
  }
  
  dplyr::bind_rows(
    process(res$itineraries, "itinerary"),
    if (include_direct && !is.null(res$direct)) process(res$direct, "direct") else NULL
  )
}

#' @keywords internal
.flatten_legs <- function(x, include_direct = FALSE, decode_geom = FALSE) {
  res <- .as_plan_list(x)
  
  itins <- res$itineraries
  kinds_label <- rep("itinerary", length(itins))
  if (include_direct && !is.null(res$direct)) {
    itins <- c(itins, res$direct)
    kinds_label <- c(kinds_label, rep("direct", length(res$direct)))
  }
  if (length(itins) == 0) return(.legs_template())
  
  total_legs <- 0
  for (it in itins) total_legs <- total_legs + length(it[["legs"]])
  if (total_legs == 0) return(.legs_template())
  
  kind <- character(total_legs); itin_id <- integer(total_legs); leg_index <- integer(total_legs)
  mode <- character(total_legs); from_name <- character(total_legs); from_lat <- numeric(total_legs); from_lon <- numeric(total_legs)
  to_name <- character(total_legs); to_lat <- numeric(total_legs); to_lon <- numeric(total_legs)
  startTime <- character(total_legs); endTime <- character(total_legs); duration <- numeric(total_legs); distance <- numeric(total_legs)
  geom_info <- vector("list", total_legs)
  
  curr <- 1
  for (i in seq_along(itins)) {
    it <- itins[[i]]; k <- kinds_label[i]; legs <- it[["legs"]]
    for (j in seq_along(legs)) {
      l <- legs[[j]]
      kind[curr] <- k; itin_id[curr] <- i; leg_index[curr] <- j
      mode[curr] <- l[["mode"]] %||% NA_character_
      f <- l[["from"]]; from_name[curr] <- f[["name"]] %||% NA_character_
      from_lat[curr] <- f[["lat"]] %||% NA_real_; from_lon[curr] <- f[["lon"]] %||% NA_real_
      t <- l[["to"]]; to_name[curr] <- t[["name"]] %||% NA_character_
      to_lat[curr] <- t[["lat"]] %||% NA_real_; to_lon[curr] <- t[["lon"]] %||% NA_real_
      startTime[curr] <- l[["startTime"]] %||% f[["departure"]] %||% f[["scheduledDeparture"]] %||% NA_character_
      endTime[curr] <- l[["endTime"]] %||% t[["arrival"]] %||% t[["scheduledArrival"]] %||% NA_character_
      duration[curr] <- l[["duration"]] %||% NA_real_; distance[curr] <- l[["distance"]] %||% NA_real_
      if (decode_geom) geom_info[[curr]] <- .leg_polyline(l)
      curr <- curr + 1
    }
  }
  
  dplyr::tibble(
    kind, itin_id, leg_index, mode, from_name, from_lat, from_lon, to_name, to_lat, to_lon,
    startTime, endTime, duration, distance, geom = geom_info
  )
}

#' Internal helper to convert plan tibble to sf
#' @param df A tibble from .flatten_itineraries or .flatten_legs
#' @return An sf object if possible, otherwise the original tibble
#' @noRd
.st_as_sf_plan <- function(df) {
  if (!requireNamespace("sf", quietly = TRUE) || !"geom" %in% names(df)) return(df)
  if (inherits(df[["geom"]], "sfc")) return(sf::st_as_sf(df))
  
  if (!is.list(df[["geom"]])) return(df)
  n <- nrow(df)
  if (n == 0) {
    df[["geom"]] <- sf::st_sfc(crs = 4326)
    return(sf::st_as_sf(df))
  }

  has_google <- requireNamespace("googlePolylines", quietly = TRUE)
  decoded_list <- vector("list", n)
  
  for (i in seq_len(n)) {
    item <- df[["geom"]][[i]]
    if (is.null(item)) next
    
    # Itineraries: list of leg info
    if (is.list(item) && length(item) > 0 && is.list(item[[1]]) && !is.null(item[[1]][["points"]])) {
      if (!has_google) next
      it_coords <- lapply(item, function(lp) {
        if (is.null(lp)) return(NULL)
        d <- googlePolylines::decode(lp$points, precision = lp$precision)[[1]]
        if (ncol(d) >= 2) names(d)[1:2] <- c("lat", "lon")
        d
      })
      decoded_list[[i]] <- dplyr::bind_rows(it_coords)
    }
    # Legs: single leg info
    else if (is.list(item) && !is.null(item[["points"]])) {
      if (!has_google) next
      d <- googlePolylines::decode(item$points, precision = item$precision)[[1]]
      if (ncol(d) >= 2) names(d)[1:2] <- c("lat", "lon")
      decoded_list[[i]] <- d
    }
    # Data frame or matrix
    else if (is.data.frame(item) || is.matrix(item)) {
      decoded_list[[i]] <- item
    }
  }

  valid_idx <- which(vapply(decoded_list, function(x) !is.null(x) && nrow(x) >= 2, logical(1)))
  if (length(valid_idx) == 0) {
    df[["geom"]] <- sf::st_sfc(replicate(n, sf::st_linestring(), simplify = FALSE), crs = 4326)
    return(sf::st_as_sf(df))
  }

  if (requireNamespace("sfheaders", quietly = TRUE)) {
    geom_dfs <- decoded_list[valid_idx]
    names(geom_dfs) <- as.character(valid_idx)
    long_geoms <- dplyr::bind_rows(geom_dfs, .id = "id")
    
    sfc <- sfheaders::sfc_linestring(long_geoms, x = "lon", y = "lat", linestring_id = "id")
    sfc <- sf::st_set_crs(sfc, 4326)
    
    full_sfc <- sf::st_sfc(replicate(n, sf::st_linestring(), simplify = FALSE), crs = 4326)
    full_sfc[valid_idx] <- sfc
    df[["geom"]] <- full_sfc
  } else {
    df[["geom"]] <- purrr::map(decoded_list, function(x) {
      if (is.null(x) || nrow(x) < 2) return(sf::st_linestring())
      mat <- if (all(c("lon", "lat") %in% names(x))) as.matrix(x[, c("lon", "lat")]) else as.matrix(x[, 2:1, drop = FALSE])
      sf::st_linestring(mat)
    }) |> sf::st_sfc(crs = 4326)
  }
  
  sf::st_as_sf(df)
}
