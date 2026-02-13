# R/helpers.R optimized for memory

# minimal internal %||% (no extra deps)
`%||%` <- function(x, y) if (!is.null(x)) x else y

# Internal debug helper
debug_msg <- function(...) {
  if (isTRUE(getOption("rmotis.debug", FALSE))) {
    message("DEBUG: ", ...)
  }
}

# Internal helper to get the server URL from options
.get_server_url <- function() {
  getOption("rmotis.url")
}

# get leg polyline points + precision from either 'legGeometry' or legacy 'polyline'
.leg_polyline <- function(leg) {
  if (!is.list(leg)) return(NULL)
  
  # Try legGeometry first
  lg <- leg[["legGeometry"]]
  if (is.list(lg)) {
    p <- lg[["points"]]
    if (is.character(p) && length(p) == 1L && nzchar(p)) {
      return(list(points = p, precision = as.integer(lg[["precision"]] %||% 5L)))
    }
  }
  
  # Try legacy polyline
  p <- leg[["polyline"]]
  if (is.character(p) && length(p) == 1L && nzchar(p)) {
    return(list(points = p, precision = 5L))
  }
  
  NULL
}

# Parse httr2_response | raw | JSON string | already-parsed list
.as_plan_list <- function(x) {
  if (inherits(x, "httr2_response")) {
    body <- httr2::resp_body_raw(x)
    if (length(body) == 0) return(list(itineraries = list()))
    
    return(tryCatch({
      RcppSimdJson::fparse(body, max_simplify_lvl = "list", type_policy = "numbers", int64_policy = "double")
    }, error = function(e) {
      stop("Failed to parse MOTIS response as JSON: ", e$message, call. = FALSE)
    }))
  }
  if (is.raw(x) || (is.character(x) && length(x) == 1L)) {
    if (length(x) == 0 || (is.character(x) && !nzchar(x))) return(list(itineraries = list()))
    return(tryCatch({
      RcppSimdJson::fparse(x, max_simplify_lvl = "list", type_policy = "numbers", int64_policy = "double")
    }, error = function(e) {
      stop("Failed to parse input as JSON: ", e$message, call. = FALSE)
    }))
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
  res <- tryCatch(.as_plan_list(x), error = function(e) {
    warning("Error parsing response: ", e$message)
    return(NULL)
  })
  
  if (is.null(res)) {
    debug_msg(".flatten_itineraries: parsed response is NULL")
    return(.itins_template())
  }
  
  # Ensure we have lists even if keys are missing
  itins <- res$itineraries
  if (is.null(itins)) itins <- list()
  directs <- if (include_direct) res$direct else NULL
  if (is.null(directs)) directs <- list()

  process <- function(itins_list, kind_label) {
    if (is.null(itins_list) || length(itins_list) == 0) {
      return(.itins_template())
    }
    if (!is.list(itins_list)) {
      warning("Itineraries field is not a list for kind: ", kind_label)
      return(.itins_template())
    }
    
    n <- length(itins_list)
    debug_msg(".flatten_itineraries: processing ", n, " ", kind_label, "(s)")
    
    duration <- numeric(n); distance <- numeric(n); startTime <- character(n)
    endTime <- character(n); transfers <- integer(n); geom_info <- vector("list", n)
    
    for (i in seq_len(n)) {
      it <- itins_list[[i]]
      if (!is.list(it)) {
        duration[i] <- NA_real_; distance[i] <- NA_real_; startTime[i] <- NA_character_
        endTime[i] <- NA_character_; transfers[i] <- NA_integer_; geom_info[[i]] <- NULL
        next
      }
      
      duration[i] <- as.numeric(it[["duration"]] %||% NA_real_)
      startTime[i] <- as.character(it[["startTime"]] %||% NA_character_)
      endTime[i] <- as.character(it[["endTime"]] %||% NA_character_)
      transfers[i] <- as.integer(it[["transfers"]] %||% 0L)
      
      dist_sum <- 0
      legs <- it[["legs"]]
      
      if (is.list(legs) && length(legs) > 0) {
        n_legs <- length(legs)
        it_polys <- if (decode_geom) vector("list", n_legs) else NULL
        
        for (j in seq_len(n_legs)) {
          l <- legs[[j]]
          if (!is.list(l)) next
          dist_sum <- dist_sum + as.numeric(l[["distance"]] %||% 0)
          if (decode_geom) {
            lp <- .leg_polyline(l)
            if (!is.null(lp)) it_polys[[j]] <- lp
          }
        }
        if (decode_geom) {
          geom_info[[i]] <- if (all(vapply(it_polys, is.null, logical(1)))) NULL else it_polys
        }
      } else {
        if (decode_geom) geom_info[[i]] <- NULL
      }
      distance[i] <- dist_sum
    }
    
    dplyr::tibble(
      kind = kind_label, itin_id = seq_len(n), duration, distance, startTime, endTime, transfers, geom = geom_info
    )
  }
  
  it_df <- process(itins, "itinerary")
  dir_df <- process(directs, "direct")
  
  combined <- dplyr::bind_rows(it_df, dir_df)
  debug_msg(".flatten_itineraries: returning ", nrow(combined), " total rows")
  combined
}

#' @keywords internal
.flatten_legs <- function(x, include_direct = FALSE, decode_geom = FALSE) {
  res <- tryCatch(.as_plan_list(x), error = function(e) {
    warning("Error parsing response: ", e$message)
    return(NULL)
  })
  
  if (is.null(res)) return(.legs_template())
  
  itins <- res$itineraries
  if (!is.list(itins)) itins <- list()
  
  directs <- if (include_direct) res$direct else NULL
  if (is.null(directs)) directs <- list()

  all_itins <- c(itins, directs)
  kinds_label <- c(rep("itinerary", length(itins)), rep("direct", length(directs)))
  
  if (length(all_itins) == 0) return(.legs_template())
  
  # Pre-calculate total legs to avoid dynamic growth
  total_legs <- 0
  for (it in all_itins) {
    if (is.list(it) && is.list(it[["legs"]])) {
      total_legs <- total_legs + length(it[["legs"]])
    }
  }
  
  if (total_legs == 0) return(.legs_template())
  
  kind <- character(total_legs); itin_id <- integer(total_legs); leg_index <- integer(total_legs)
  mode <- character(total_legs); from_name <- character(total_legs); from_lat <- numeric(total_legs); from_lon <- numeric(total_legs)
  to_name <- character(total_legs); to_lat <- numeric(total_legs); to_lon <- numeric(total_legs)
  startTime <- character(total_legs); endTime <- character(total_legs); duration <- numeric(total_legs); distance <- numeric(total_legs)
  geom_info <- vector("list", total_legs)
  
  curr <- 1
  for (i in seq_along(all_itins)) {
    it <- all_itins[[i]]; k <- kinds_label[i]
    if (!is.list(it) || !is.list(it[["legs"]])) next
    
    legs <- it[["legs"]]
    for (j in seq_along(legs)) {
      l <- legs[[j]]
      if (!is.list(l)) next
      
      kind[curr] <- k; itin_id[curr] <- i; leg_index[curr] <- j
      mode[curr] <- as.character(l[["mode"]] %||% NA_character_)
      
      f <- l[["from"]]
      if (is.list(f)) {
        from_name[curr] <- as.character(f[["name"]] %||% NA_character_)
        from_lat[curr] <- as.numeric(f[["lat"]] %||% NA_real_)
        from_lon[curr] <- as.numeric(f[["lon"]] %||% NA_real_)
      } else {
        from_name[curr] <- NA_character_; from_lat[curr] <- NA_real_; from_lon[curr] <- NA_real_
      }
      
      t <- l[["to"]]
      if (is.list(t)) {
        to_name[curr] <- as.character(t[["name"]] %||% NA_character_)
        to_lat[curr] <- as.numeric(t[["lat"]] %||% NA_real_)
        to_lon[curr] <- as.numeric(t[["lon"]] %||% NA_real_)
      } else {
        to_name[curr] <- NA_character_; to_lat[curr] <- NA_real_; to_lon[curr] <- NA_real_
      }
      
      startTime[curr] <- as.character(l[["startTime"]] %||% f[["departure"]] %||% f[["scheduledDeparture"]] %||% NA_character_)
      endTime[curr] <- as.character(l[["endTime"]] %||% t[["arrival"]] %||% t[["scheduledArrival"]] %||% NA_character_)
      duration[curr] <- as.numeric(l[["duration"]] %||% NA_real_)
      distance[curr] <- as.numeric(l[["distance"]] %||% NA_real_)
      
      if (decode_geom) geom_info[[curr]] <- .leg_polyline(l)
      curr <- curr + 1
    }
  }
  
  # Trim if we skipped any non-list legs
  if (curr <= total_legs) {
    idx <- seq_len(curr - 1)
    kind <- kind[idx]; itin_id <- itin_id[idx]; leg_index <- leg_index[idx]
    mode <- mode[idx]; from_name <- from_name[idx]; from_lat <- from_lat[idx]; from_lon <- from_lon[idx]
    to_name <- to_name[idx]; to_lat <- to_lat[idx]; to_lon <- to_lon[idx]
    startTime <- startTime[idx]; endTime <- endTime[idx]; duration <- duration[idx]; distance <- distance[idx]
    geom_info <- geom_info[idx]
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
        if (!is.list(lp) || is.null(lp$points)) return(NULL)
        d <- tryCatch(googlePolylines::decode(lp$points, precision = lp$precision %||% 5L)[[1]], error = function(e) NULL)
        if (!is.null(d) && is.data.frame(d) && ncol(d) >= 2) {
          names(d)[1:2] <- c("lat", "lon")
          return(d)
        }
        NULL
      })
      valid_coords <- it_coords[!vapply(it_coords, is.null, logical(1))]
      if (length(valid_coords) > 0) {
        decoded_list[[i]] <- dplyr::bind_rows(valid_coords)
      }
    }
    # Legs: single leg info
    else if (is.list(item) && !is.null(item[["points"]])) {
      if (!has_google) next
      d <- tryCatch(googlePolylines::decode(item$points, precision = item$precision %||% 5L)[[1]], error = function(e) NULL)
      if (!is.null(d) && is.data.frame(d) && ncol(d) >= 2) {
        names(d)[1:2] <- c("lat", "lon")
        decoded_list[[i]] <- d
      }
    }
    # Data frame or matrix
    else if (is.data.frame(item) || is.matrix(item)) {
      decoded_list[[i]] <- item
    }
  }

  valid_idx <- which(vapply(decoded_list, function(x) !is.null(x) && is.data.frame(x) && nrow(x) >= 2, logical(1)))
  
  if (length(valid_idx) == 0) {
    df[["geom"]] <- sf::st_sfc(replicate(n, sf::st_linestring(), simplify = FALSE), crs = 4326)
    return(sf::st_as_sf(df))
  }

  if (requireNamespace("sfheaders", quietly = TRUE)) {
    geom_dfs <- decoded_list[valid_idx]
    names(geom_dfs) <- as.character(valid_idx)
    long_geoms <- dplyr::bind_rows(geom_dfs, .id = "id")
    
    sfc <- tryCatch({
      sfheaders::sfc_linestring(long_geoms, x = "lon", y = "lat", linestring_id = "id")
    }, error = function(e) {
      # Fallback to manual creation
      lapply(geom_dfs, function(x) sf::st_linestring(as.matrix(x[, c("lon", "lat")]))) |> sf::st_sfc(crs = 4326)
    })
    sfc <- sf::st_set_crs(sfc, 4326)
    
    full_sfc <- sf::st_sfc(replicate(n, sf::st_linestring(), simplify = FALSE), crs = 4326)
    full_sfc[valid_idx] <- sfc
    df[["geom"]] <- full_sfc
  } else {
    df[["geom"]] <- purrr::map(decoded_list, function(x) {
      if (is.null(x) || !is.data.frame(x) || nrow(x) < 2) return(sf::st_linestring())
      mat <- if (all(c("lon", "lat") %in% names(x))) as.matrix(x[, c("lon", "lat")]) else as.matrix(x[, 2:1, drop = FALSE])
      sf::st_linestring(mat)
    }) |> sf::st_sfc(crs = 4326)
  }
  
  sf::st_as_sf(df)
}

.get_ids <- function(place, id_col = "id") {
  if (inherits(place, "sf")) {
    if (id_col %in% names(place)) {
      return(unname(as.character(sf::st_drop_geometry(place)[[id_col]])))
    } else {
      return(as.character(seq_len(nrow(place))))
    }
  }
  if (is.data.frame(place)) {
    if (id_col %in% names(place)) {
      return(unname(as.character(place[[id_col]])))
    } else {
      return(as.character(seq_len(nrow(place))))
    }
  }
  if (is.matrix(place)) {
    return(as.character(seq_len(nrow(place))))
  }
  if (is.character(place)) {
    return(unname(place))
  }
  return("1")
}

.format_place <- function(place, id_col = "id") {
  if (inherits(place, "sf")) {
    coords <- sf::st_coordinates(place)
    lat <- round(coords[, "Y"], 6)
    lon <- round(coords[, "X"], 6)
    return(unname(paste(lat, lon, sep = ",")))
  }
  if (is.data.frame(place)) {
    p_names <- tolower(names(place))
    lat_col <- which(p_names %in% c("lat", "latitude"))
    lon_col <- which(p_names %in% c("lon", "lng", "longitude"))
    if (length(lat_col) == 1 && length(lon_col) == 1) {
      lat <- round(place[[lat_col]], 6)
      lon <- round(place[[lon_col]], 6)
      return(unname(paste(lat, lon, sep = ",")))
    }
    id_col_lower <- tolower(id_col)
    if (id_col_lower %in% p_names) {
      id_col_idx <- which(p_names == id_col_lower)
      return(unname(as.character(place[[id_col_idx]])))
    }
    stop("Data frame must contain coordinate columns ('lat', 'lon') or an '", id_col, "' column.", call. = FALSE)
  }
  if (is.matrix(place) && is.numeric(place)) {
    if (ncol(place) != 2) stop("Matrix must have 2 columns.", call. = FALSE)
    cnames <- tolower(colnames(place))
    if (all(c("lon", "lat") %in% cnames)) {
      lat <- round(place[, "lat"], 10); lon <- round(place[, "lon"], 10)
      return(unname(paste(lat, lon, sep = ",")))
    }
    return(unname(paste(place[, 2], place[, 1], sep = ",")))
  }
  if (is.character(place)) return(unname(place))
  stop("Unsupported input type.", call. = FALSE)
}

# Recursive list update helper
.deep_update <- function(base, updates) {
  if (!is.list(updates)) return(updates)
  for (name in names(updates)) {
    if (is.list(updates[[name]]) && is.list(base[[name]])) {
      base[[name]] <- .deep_update(base[[name]], updates[[name]])
    } else {
      base[[name]] <- updates[[name]]
    }
  }
  base
}

#' Resolve path to config.yml with smart detection
#' @param path Character. Path to file or directory.
#' @param type Character. Either "import" (root) or "server" (data).
#' @return Character path to config.yml or stops with error.
#' @noRd
.resolve_config_path <- function(path, type = c("import", "server")) {
  type <- match.arg(type)
  path <- normalizePath(path, mustWork = TRUE)
  
  # 1. Path is already config.yml
  if (!dir.exists(path) && basename(path) == "config.yml") {
    return(path)
  }
  
  if (!dir.exists(path)) {
    stop("Path must be a directory or a 'config.yml' file.", call. = FALSE)
  }
  
  # 2. Check for "server" (data) directory
  is_data_dir <- function(d) {
    has_osr <- dir.exists(file.path(d, "osr"))
    has_meta <- any(file.exists(file.path(d, "meta", c("tt.json", "tiles.json", "adr.json"))))
    has_config <- file.exists(file.path(d, "config.yml"))
    has_osr && (has_meta || has_config) # At minimum osr + config or osr + meta
  }
  
  # 3. Check for "import" (root) directory
  is_root_dir <- function(d) {
    pbfs <- list.files(d, pattern = "\\.osm\\.pbf$", full.names = TRUE)
    has_config <- file.exists(file.path(d, "config.yml"))
    length(pbfs) == 1 && has_config
  }
  
  if (type == "server") {
    # Is the current path a data dir?
    if (is_data_dir(path)) return(file.path(path, "config.yml"))
    
    # Is there a data/ subfolder that is a data dir?
    data_sub <- file.path(path, "data")
    if (dir.exists(data_sub) && is_data_dir(data_sub)) {
      return(file.path(data_sub, "config.yml"))
    }
    
    stop("Could not identify a MOTIS data directory at: ", path, 
         "\nA data directory should contain an 'osr/' folder and 'config.yml'.", call. = FALSE)
  } else {
    # type == "import"
    if (is_root_dir(path)) return(file.path(path, "config.yml"))
    
    # If we are inside 'data', the root is one level up
    parent <- dirname(path)
    if (basename(path) == "data" && is_root_dir(parent)) {
      return(file.path(parent, "config.yml"))
    }
    
    stop("Could not identify a MOTIS root (import) directory at: ", path,
         "\nA root directory should contain exactly one .osm.pbf file and 'config.yml'.", call. = FALSE)
  }
}
