
options(rmotis.wait_for_server = FALSE)

test_that("motis_one_to_many works with mocked POST request", {
  skip_if_not_installed("withr")
  # Define the mock response data
  mock_response_json <- '[
    {"one_id": "one_coords", "many_id": "1", "duration": 600, "distance": 1000},
    {"one_id": "one_coords", "many_id": "2", "duration": 1200, "distance": 2000}
  ]'
  
  mock_resp <- httr2::response(
    status_code = 200, 
    headers = list("Content-Type" = "application/json"),
    body = charToRaw(mock_response_json)
  )

  mock_perform <- function(req, ...) {
    expect_equal(req$url, "http://motis.fake/api/v1/one-to-many")
    expect_equal(req$method, "POST")
    body_data <- req$body$data
    expect_type(body_data, "list")
    expect_equal(body_data$mode, "CAR")
    expect_equal(body_data$elevationCosts, "NONE")
    expect_equal(body_data$one, "59.3304;18.0583")
    expect_length(body_data$many, 2)
    mock_resp
  }

  testthat::with_mocked_bindings(
    req_perform = mock_perform,
    code = {
      dest <- data.frame(lat = 59.3304, lon = 18.0583)
      origins <- data.frame(id = 1:2, lat = c(59.3304, 59.3305), lon = c(18.0583, 18.0584))

      # Test arrive_by = FALSE (One to Many)
      x_out <- motis_one_to_many(
        one = dest, many = origins, mode = "CAR", distance = TRUE,
        .server = "http://motis.fake", max = 3600, parallel = FALSE
      )
      expect_s3_class(x_out, "data.frame")
      expect_equal(names(x_out), c("from_id", "to_id", "duration_s", "distance_m"))
      expect_equal(nrow(x_out), 2)
      expect_equal(x_out$duration_s, c(600, 1200))
      expect_equal(x_out$from_id, c("1", "1"))
      expect_equal(x_out$to_id, c("1", "2"))

      # Test arrive_by = TRUE (Many to One)
      x_in <- motis_one_to_many(
        one = dest, many = origins, mode = "CAR", arrive_by = TRUE, distance = TRUE,
        .server = "http://motis.fake", parallel = FALSE
      )
      expect_equal(x_in$to_id, c("1", "1"))
      expect_equal(x_in$from_id, c("1", "2"))
    },
    .package = "httr2"
  )
})

test_that("motis_one_to_many handles sf objects correctly (mocked)", {
  skip_if_not_installed("sf")
  
  mock_response_json <- '[
    {"one_id": "1", "many_id": "1", "duration": 500, "distance": 800},
    {"one_id": "1", "many_id": "2", "duration": 600, "distance": 900},
    {"one_id": "1", "many_id": "3", "duration": 700, "distance": 1000}
  ]'
  
  mock_resp <- httr2::response(
    status_code = 200, 
    headers = list("Content-Type" = "application/json"),
    body = charToRaw(mock_response_json)
  )
  
  mock_perform <- function(req, ...) {
    body_data <- req$body$data
    expect_equal(body_data$one, "59.3304;18.0583")
    expect_equal(body_data$many[[1]], "59.3304;18.0583")
    mock_resp
  }
  
  testthat::with_mocked_bindings(
    req_perform = mock_perform,
    code = {
      dest_sf <- sf::st_as_sf(data.frame(lat = 59.3304, lon = 18.0583), coords = c("lon", "lat"), crs = 4326)
      origins_sf <- sf::st_as_sf(data.frame(id = 1:3, lat = rep(59.3304, 3), lon = rep(18.0583, 3)), coords = c("lon", "lat"), crs = 4326)
      
      res <- motis_one_to_many(
        one = dest_sf, many = origins_sf, mode = "CAR",
        .server = "http://motis.fake", parallel = FALSE
      )
      
      expect_s3_class(res, "data.frame")
      expect_equal(nrow(res), 3)
      expect_equal(res$duration_s, c(500, 600, 700))
    },
    .package = "httr2"
  )
})

# --- Migrated Parallel Tests ---

options(rmotis.retry_max_tries = 1L)

test_that("basic parallel execution works (mocked)", {
  options(rmotis.wait_for_server = FALSE)
  
  one <- data.frame(lat = c(59.3, 59.4), lon = c(18.0, 18.1), id = c("O1", "O2"))
  many <- data.frame(lat = c(60.0, 60.1, 60.2), lon = c(18.5, 18.6, 18.7), id = c("D1", "D2", "D3"))
  
  mock_fn <- function(req) {
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw(jsonlite::toJSON(list(
        list(duration = 100, distance = 1000),
        list(duration = 200, distance = 2000),
        list(duration = 300, distance = 3000)
      ), auto_unbox = TRUE))
    )
  }
  
  result <- httr2::with_mocked_responses(mock_fn, {
    motis_one_to_many(
      one, many, mode = "WALK", .server = "http://motis.fake",
      parallel = TRUE, batch_size = 2, progress = FALSE,
      spatial_filter_km = NULL, spatial_sort = FALSE
    )
  })
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("from_id", "to_id", "duration_s") %in% names(result)))
  expect_equal(nrow(result), 6) # 2 origins * 3 dests
})

test_that("parquet output directory works", {
  skip_if_not_installed("arrow")
  
  one <- data.frame(lat = c(59.3, 59.4), lon = c(18.0, 18.1), id = c("O1", "O2"))
  many <- data.frame(lat = 60.0, lon = 18.5, id = "D1")
  
  # Use a path WITHOUT .parquet extension to ensure it is treated as a directory
  parquet_dir <- tempfile("output_parquet_dir_")
  on.exit(unlink(parquet_dir, recursive = TRUE), add = TRUE)
  
  mock_fn <- function(req) {
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw(jsonlite::toJSON(list(list(duration = 100, distance = 1000)), auto_unbox = TRUE))
    )
  }
  
  result_path <- httr2::with_mocked_responses(mock_fn, {
    motis_one_to_many(
      one, many, mode = "WALK", .server = "http://motis.fake",
      parallel = TRUE, output_path = parquet_dir, batch_size = 1,
      progress = FALSE, spatial_filter_km = NULL, spatial_sort = FALSE
    )
  })
  
  expect_true(dir.exists(parquet_dir))
  expect_equal(result_path, parquet_dir)
  parquet_files <- list.files(parquet_dir, pattern = "\\.parquet$")
  expect_gte(length(parquet_files), 1)
  
  ds <- arrow::open_dataset(parquet_dir)
  data <- dplyr::collect(ds)
  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 2)
})

test_that("HTTP error handling - 500 server error", {
  one <- data.frame(lat = 59.3, lon = 18.0, id = "O1")
  many <- data.frame(lat = 60.0, lon = 18.5, id = "D1")
  
  mock_fn <- function(req) {
    httr2::response(
      status_code = 500,
      headers = list(`Content-Type` = "text/plain"),
      body = charToRaw("Internal Server Error")
    )
  }
  
  result <- httr2::with_mocked_responses(mock_fn, {
    suppressWarnings(motis_one_to_many(
      one, many, mode = "WALK", .server = "http://motis.fake",
      parallel = TRUE, progress = FALSE,
      spatial_filter_km = NULL, spatial_sort = FALSE
    ))
  })
  
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$duration_s[1]))
})

test_that("partial batch failures handled correctly", {
  one <- data.frame(lat = c(59.3, 59.4, 59.5), lon = c(18, 18, 18), id = c("O1", "O2", "O3"))
  many <- data.frame(lat = 60, lon = 18.5, id = "D1")
  
  call_count <- 0
  mock_fn <- function(req) {
    call_count <<- call_count + 1
    if (call_count == 2) {
      httr2::response(status_code = 500, headers = list(`Content-Type` = "text/plain"), body = charToRaw("Error"))
    } else {
      httr2::response(
        status_code = 200, headers = list(`Content-Type` = "application/json"),
        body = charToRaw(jsonlite::toJSON(list(list(duration = call_count * 100)), auto_unbox = TRUE))
      )
    }
  }
  
  result <- httr2::with_mocked_responses(mock_fn, {
    suppressWarnings(motis_one_to_many(
      one, many, mode = "WALK", .server = "http://motis.fake",
      parallel = TRUE, batch_size = 10, progress = FALSE,
      spatial_filter_km = NULL, spatial_sort = FALSE
    ))
  })
  
  expect_equal(nrow(result), 3)
  expect_equal(sum(is.na(result$duration_s)), 1)
  expect_equal(sum(!is.na(result$duration_s)), 2)
})

test_that("checkpoint create and resume works", {
  one <- data.frame(lat = c(59.3, 59.4, 59.5), lon = c(18, 18, 18), id = c("A", "B", "C"))
  many <- data.frame(lat = 60, lon = 18.5, id = "X")
  
  checkpoint_file <- tempfile("checkpoint_", fileext = ".txt")
  on.exit(unlink(checkpoint_file), add = TRUE)
  
  mock_fn <- function(req) {
    httr2::response(
      status_code = 200, headers = list(`Content-Type` = "application/json"),
      body = charToRaw(jsonlite::toJSON(list(list(duration = 100)), auto_unbox = TRUE))
    )
  }
  
  httr2::with_mocked_responses(mock_fn, {
    motis_one_to_many(
      one, many, mode = "WALK", .server = "http://motis.fake",
      parallel = TRUE, checkpoint_file = checkpoint_file, batch_size = 10,
      progress = FALSE, spatial_filter_km = NULL, spatial_sort = FALSE
    )
  })
  
  expect_true(file.exists(checkpoint_file))
  checkpoint_ids <- readLines(checkpoint_file)
  expect_equal(sort(checkpoint_ids), c("A", "B", "C"))
  
  # Resume
  result2 <- httr2::with_mocked_responses(mock_fn, {
    motis_one_to_many(
      one, many, mode = "WALK", .server = "http://motis.fake",
      parallel = TRUE, checkpoint_file = checkpoint_file,
      progress = FALSE, spatial_filter_km = NULL, spatial_sort = FALSE
    )
  })
  
  expect_equal(nrow(result2), 0)
})

test_that("CSV output works", {
  one <- data.frame(lat = 59.3, lon = 18.0, id = "O1")
  many <- data.frame(lat = c(60.0, 60.1), lon = c(18.5, 18.6), id = c("D1", "D2"))
  
  csv_file <- tempfile("output_", fileext = ".csv")
  on.exit(unlink(csv_file), add = TRUE)
  
  mock_fn <- function(req) {
    httr2::response(
      status_code = 200, headers = list(`Content-Type` = "application/json"),
      body = charToRaw(jsonlite::toJSON(list(
        list(duration = 100, distance = 1000),
        list(duration = 200, distance = 2000)
      ), auto_unbox = TRUE))
    )
  }
  
  result_path <- httr2::with_mocked_responses(mock_fn, {
    motis_one_to_many(
      one, many, mode = "WALK", .server = "http://motis.fake",
      parallel = TRUE, output_path = csv_file, progress = FALSE,
      spatial_filter_km = NULL, spatial_sort = FALSE
    )
  })
  
  expect_true(file.exists(csv_file))
  expect_equal(result_path, csv_file)
  csv_data <- utils::read.csv(csv_file, stringsAsFactors = FALSE)
  expect_equal(nrow(csv_data), 2)
})

test_that("motis_one_to_many calls server polling", {
  one <- data.frame(lat = 59.3, lon = 18.0, id = "O1")
  many <- data.frame(lat = 60.0, lon = 18.5, id = "D1")
  
  calls <- 0
  mock_fn <- function(req) {
    calls <<- calls + 1
    if (grepl("api/v1/one-to-many", req$url)) {
      return(httr2::response(
        status_code = 200, headers = list(`Content-Type` = "application/json"),
        body = charToRaw(jsonlite::toJSON(list(list(duration = 100)), auto_unbox = TRUE))
      ))
    }
    httr2::response(status_code = 200)
  }
  
  calls <- 0
  mock_wait <- function(...) {
    calls <<- calls + 1
    invisible(TRUE)
  }
  
  testthat::with_mocked_bindings(
    .wait_for_server = mock_wait,
    code = {
      httr2::with_mocked_responses(mock_fn, {
        motis_one_to_many(
          one, many, .server = "http://motis.fake",
          parallel = TRUE, progress = FALSE,
          spatial_filter_km = NULL, spatial_sort = FALSE
        )
      })
    },
    .package = "rmotis"
  )
  
  expect_gte(calls, 1)
})
