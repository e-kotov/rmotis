options(rmotis.retry_max_tries = 1L); test_that("basic parallel execution works (mocked)", { options(rmotis.wait_for_server = FALSE);
  # Test data: 2 origins × 3 destinations
  one <- data.frame(lat = c(59.3, 59.4), lon = c(18.0, 18.1), id = c("O1", "O2"))
  many <- data.frame(lat = c(60.0, 60.1, 60.2), lon = c(18.5, 18.6, 18.7),
                     id = c("D1", "D2", "D3"))
  
  # Mock responses: return fake JSON for each request
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
  
  # Execute with mocked responses
  result <- httr2::with_mocked_responses(mock_fn, {
    motis_one_to_many_parallel(
      one, many,
      mode = "WALK",
      .server = "http://localhost:8080",
      batch_size = 2,
      progress = FALSE,
      spatial_filter = FALSE,
      spatial_sort = FALSE
    )
  })
  
  # Verify result structure
  expect_s3_class(result, "data.frame")
  expect_true("from_id" %in% names(result))
  expect_true("to_id" %in% names(result))
  expect_true("duration_s" %in% names(result))
  
  # Should have 2 origins × 3 dests = 6 rows
  expect_equal(nrow(result), 6)
})

test_that("parquet output directory works", {
  skip_if_not_installed("arrow")
  
  one <- data.frame(lat = c(59.3, 59.4), lon = c(18.0, 18.1), id = c("O1", "O2"))
  many <- data.frame(lat = 60.0, lon = 18.5, id = "D1")
  
  parquet_dir <- tempfile("output_", fileext = ".parquet")
  on.exit(unlink(parquet_dir, recursive = TRUE), add = TRUE)
  
  # Mock response
  mock_fn <- function(req) {
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw(jsonlite::toJSON(list(
        list(duration = 100, distance = 1000)
      ), auto_unbox = TRUE))
    )
  }
  
  result_path <- httr2::with_mocked_responses(mock_fn, {
    motis_one_to_many_parallel(
      one, many,
      mode = "WALK",
      .server = "http://localhost:8080",
      output_file = parquet_dir,
      batch_size = 1,  # Force 2 files (one per origin)
      progress = FALSE,
      spatial_filter = FALSE,
      spatial_sort = FALSE
    )
  })
  
  # Verify directory was created
  expect_true(dir.exists(parquet_dir))
  expect_equal(result_path, parquet_dir)
  
  # Verify parquet files exist
  parquet_files <- list.files(parquet_dir, pattern = "\\.parquet$")
  expect_gte(length(parquet_files), 1)
  
  # Verify arrow can read it
  ds <- arrow::open_dataset(parquet_dir)
  data <- dplyr::collect(ds)
  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 2)  # 2 origins × 1 dest
})

test_that("HTTP error handling - 500 server error", {
  one <- data.frame(lat = 59.3, lon = 18.0, id = "O1")
  many <- data.frame(lat = 60.0, lon = 18.5, id = "D1")
  
  # Mock 500 error
  mock_fn <- function(req) {
    httr2::response(
      status_code = 500,
      headers = list(`Content-Type` = "text/plain"),
      body = charToRaw("Internal Server Error")
    )
  }
  
  # Should handle error gracefully and return NA
  result <- httr2::with_mocked_responses(mock_fn, {
    suppressWarnings(motis_one_to_many_parallel(
      one, many,
      mode = "WALK",
      .server = "http://localhost:8080",
      progress = FALSE,
      spatial_filter = FALSE,
      spatial_sort = FALSE
    ))
  })
  
  # Should still return a row with NA values
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$duration_s[1]))
})

test_that("partial batch failures handled correctly", {
  # 3 origins, 1 destination - simulate 1/3 failures
  one <- data.frame(lat = c(59.3, 59.4, 59.5), lon = c(18, 18, 18),
                    id = c("O1", "O2", "O3"))
  many <- data.frame(lat = 60, lon = 18.5, id = "D1")
  
  call_count <- 0
  mock_fn <- function(req) {
    call_count <<- call_count + 1
    
    # First request succeeds, second fails, third succeeds
    if (call_count == 2) {
      httr2::response(
        status_code = 500,
        headers = list(`Content-Type` = "text/plain"),
        body = charToRaw("Error")
      )
    } else {
      httr2::response(
        status_code = 200,
        headers = list(`Content-Type` = "application/json"),
        body = charToRaw(jsonlite::toJSON(list(
          list(duration = call_count * 100)
        ), auto_unbox = TRUE))
      )
    }
  }
  
  result <- httr2::with_mocked_responses(mock_fn, {
    suppressWarnings(motis_one_to_many_parallel(
      one, many,
      mode = "WALK",
      .server = "http://localhost:8080",
      batch_size = 10,  # Process all in one batch
      progress = FALSE,
      spatial_filter = FALSE,
      spatial_sort = FALSE
    ))
  })
  
  # Should have 3 rows: 2 with data, 1 with NA
  expect_equal(nrow(result), 3)
  expect_equal(sum(is.na(result$duration_s)), 1)  # One NA value
  expect_equal(sum(!is.na(result$duration_s)), 2)  # Two valid values
})

test_that("edge case - single origin, single destination", {
  one <- data.frame(lat = 59.3, lon = 18.0, id = "O1")
  many <- data.frame(lat = 60.0, lon = 18.5, id = "D1")
  
  mock_fn <- function(req) {
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw(jsonlite::toJSON(list(
        list(duration = 100, distance = 1000)
      ), auto_unbox = TRUE))
    )
  }
  
  result <- httr2::with_mocked_responses(mock_fn, {
    motis_one_to_many_parallel(
      one, many,
      mode = "WALK",
      .server = "http://localhost:8080",
      progress = FALSE,
      spatial_filter = FALSE,
      spatial_sort = FALSE
    )
  })
  
  expect_equal(nrow(result), 1)
  expect_equal(result$from_id[1], "O1")
  expect_equal(result$to_id[1], "D1")
  expect_equal(result$duration_s[1], 100)
  expect_equal(result$distance_m[1], 1000)
})

test_that("edge case - empty results (no reachable destinations)", {
  one <- data.frame(lat = 59.3, lon = 18.0, id = "O1")
  many <- data.frame(lat = c(60.0, 60.1), lon = c(18.5, 18.6), id = c("D1", "D2"))
  
  # Mock response with nulls (unreachable)
  mock_fn <- function(req) {
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw(jsonlite::toJSON(list(
        list(duration = NULL, distance = NULL),
        list(duration = NULL, distance = NULL)
      ), auto_unbox = TRUE))
    )
  }
  
  result <- httr2::with_mocked_responses(mock_fn, {
    motis_one_to_many_parallel(
      one, many,
      mode = "WALK",
      .server = "http://localhost:8080",
      progress = FALSE,
      spatial_filter = FALSE,
      spatial_sort = FALSE
    )
  })
  
  # Should have rows with NA values
  expect_equal(nrow(result), 2)
  expect_true(all(is.na(result$duration_s)))
})

test_that("batch_size logic - verifies correct batching", {
  # 5 origins, batch_size = 2 -> should make 3 batches
  one <- data.frame(
    lat = seq(59, 60, length.out = 5),
    lon = rep(18, 5),
    id = paste0("O", 1:5)
  )
  many <- data.frame(lat = 60, lon = 18.5, id = "D1")
  
  batch_count <- 0
  mock_fn <- function(req) {
    batch_count <<- batch_count + 1
    
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw(jsonlite::toJSON(list(
        list(duration = 100)
      ), auto_unbox = TRUE))
    )
  }
  
  result <- httr2::with_mocked_responses(mock_fn, {
    motis_one_to_many_parallel(
      one, many,
      mode = "WALK",
      .server = "http://localhost:8080",
      batch_size = 2,
      progress = FALSE,
      spatial_filter = FALSE,
      spatial_sort = FALSE
    )
  })
  
  # Verify 5 origins were processed
  expect_equal(nrow(result), 5)
  
  # Should have made ceil(5/2) = 3 batch calls in parallel batches
  # Note: with parallel processing, the exact number depends on httr2 implementation
  expect_gte(batch_count, 3)
})

test_that("invalid JSON response handling", {
  one <- data.frame(lat = 59.3, lon = 18.0, id = "O1")
  many <- data.frame(lat = 60.0, lon = 18.5, id = "D1")
  
  # Mock invalid JSON
  mock_fn <- function(req) {
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw("{invalid json")
    )
  }
  
  # Should handle parse error gracefully
  result <- httr2::with_mocked_responses(mock_fn, {
    suppressWarnings(motis_one_to_many_parallel(
      one, many,
      mode = "WALK",
      .server = "http://localhost:8080",
      progress = FALSE,
      spatial_filter = FALSE,
      spatial_sort = FALSE
    ))
  })
  
  # Should return row with NA
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$duration_s[1]))
})

test_that("spatial filter reduces destinations (mocked)", {
  # 1 origin in Stockholm, destinations: 2 near, 1 far
  one <- data.frame(lat = 59.3, lon = 18.0, id = "Stockholm")
  many <- data.frame(
    lat = c(59.4, 59.5, 35.0),  # Stockholm, Stockholm, Tokyo
    lon = c(18.1, 18.2, 139.7),
    id = c("Near1", "Near2", "Tokyo")
  )
  
  # Mock response for nearby destinations only
  mock_fn <- function(req) {
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw(jsonlite::toJSON(list(
        list(duration = 100, distance = 1000),
        list(duration = 200, distance = 2000)
      ), auto_unbox = TRUE))
    )
  }
  
  result <- httr2::with_mocked_responses(mock_fn, {
    motis_one_to_many_parallel(
      one, many,
      mode = "WALK",
      max = 7200,
      .server = "http://localhost:8080",
      progress = FALSE,
      spatial_filter = TRUE,
      spatial_sort = FALSE
    )
  })
  
  # Tokyo should be filtered out - expect at most 2 destinations
  expect_lte(nrow(result), 2)
  if (nrow(result) > 0) {
    expect_false("Tokyo" %in% result$to_id)
  }
})

test_that("checkpoint create and resume works", {
  one <- data.frame(lat = c(59.3, 59.4, 59.5), lon = c(18, 18, 18),
                    id = c("A", "B", "C"))
  many <- data.frame(lat = 60, lon = 18.5, id = "X")
  
  checkpoint_file <- tempfile("checkpoint_", fileext = ".txt")
  on.exit(unlink(checkpoint_file), add = TRUE)
  
  # Mock response
  mock_fn <- function(req) {
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw(jsonlite::toJSON(list(
        list(duration = 100)
      ), auto_unbox = TRUE))
    )
  }
  
  # First run: process all origins
  httr2::with_mocked_responses(mock_fn, {
    motis_one_to_many_parallel(
      one, many,
      mode = "WALK",
      .server = "http://localhost:8080",
      checkpoint_file = checkpoint_file,
      batch_size = 10,
      progress = FALSE,
      spatial_filter = FALSE,
      spatial_sort = FALSE
    )
  })
  
  # Verify checkpoint file was created
  expect_true(file.exists(checkpoint_file))
  checkpoint_ids <- readLines(checkpoint_file)
  expect_equal(sort(checkpoint_ids), c("A", "B", "C"))
  
  # Second run: should skip all origins (already completed)
  result2 <- httr2::with_mocked_responses(mock_fn, {
    motis_one_to_many_parallel(
      one, many,
      mode = "WALK",
      .server = "http://localhost:8080",
      checkpoint_file = checkpoint_file,
      progress = FALSE,
      spatial_filter = FALSE,
      spatial_sort = FALSE
    )
  })
  
  # Should return empty data.frame (all completed)
  expect_equal(nrow(result2), 0)
})

test_that("CSV output works", {
  one <- data.frame(lat = 59.3, lon = 18.0, id = "O1")
  many <- data.frame(lat = c(60.0, 60.1), lon = c(18.5, 18.6), id = c("D1", "D2"))
  
  csv_file <- tempfile("output_", fileext = ".csv")
  on.exit(unlink(csv_file), add = TRUE)
  
  # Mock response
  mock_fn <- function(req) {
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw(jsonlite::toJSON(list(
        list(duration = 100, distance = 1000),
        list(duration = 200, distance = 2000)
      ), auto_unbox = TRUE))
    )
  }
  
  result_path <- httr2::with_mocked_responses(mock_fn, {
    motis_one_to_many_parallel(
      one, many,
      mode = "WALK",
      .server = "http://localhost:8080",
      output_file = csv_file,
      progress = FALSE,
      spatial_filter = FALSE,
      spatial_sort = FALSE
    )
  })
  
  # Verify file was created
  expect_true(file.exists(csv_file))
  expect_equal(result_path, csv_file)
  
  # Read and verify contents
  csv_data <- utils::read.csv(csv_file, stringsAsFactors = FALSE)
  expect_equal(nrow(csv_data), 2)
  expect_true("from_id" %in% names(csv_data))
  expect_true("to_id" %in% names(csv_data))
  expect_true("duration_s" %in% names(csv_data))
})

test_that("arrive_by swaps from_id and to_id", {
  one <- data.frame(lat = 59.3, lon = 18.0, id = "O1")
  many <- data.frame(lat = 60.0, lon = 18.5, id = "D1")
  
  # Mock response
  mock_fn <- function(req) {
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw(jsonlite::toJSON(list(
        list(duration = 100)
      ), auto_unbox = TRUE))
    )
  }
  
  result <- httr2::with_mocked_responses(mock_fn, {
    motis_one_to_many_parallel(
      one, many,
      mode = "WALK",
      arrive_by = TRUE,  # Swap direction
      .server = "http://localhost:8080",
      progress = FALSE,
      spatial_filter = FALSE,
      spatial_sort = FALSE
    )
  })
  
  # With arrive_by=TRUE, "one" becomes destination (to_id), "many" becomes origin (from_id)
  expect_equal(result$from_id[1], "D1")
  expect_equal(result$to_id[1], "O1")
})

test_that("empty response handling returns NA", {
  one <- data.frame(lat = 59.3, lon = 18.0, id = "O1")
  many <- data.frame(lat = 60.0, lon = 18.5, id = "D1")
  
  # Mock empty response
  mock_fn <- function(req) {
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw("[]")  # Empty JSON array
    )
  }
  
  result <- httr2::with_mocked_responses(mock_fn, {
    motis_one_to_many_parallel(
      one, many,
      mode = "WALK",
      .server = "http://localhost:8080",
      progress = FALSE,
      spatial_filter = FALSE,
      spatial_sort = FALSE
    )
  })
  
  # Should return 1 row with NA duration
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$duration_s[1]))
  expect_equal(result$from_id[1], "O1")
  expect_equal(result$to_id[1], "D1")
})

test_that("spatial sort orders origins by latitude", {
  # Origins at different latitudes (unsorted)
  one <- data.frame(
    lat = c(60, 58, 59),
    lon = c(18, 18, 18),
    id = c("C", "A", "B")
  )
  many <- data.frame(lat = 59.5, lon = 18.0, id = "X")
  
  # Track the order of requests by capturing origin IDs
  request_order <- character(0)
  mock_fn <- function(req) {
    # Extract the "one" parameter from request body to track order
    # This is a simplified check - in reality we'd parse the body
    request_order <<- c(request_order, length(request_order) + 1)
    
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw(jsonlite::toJSON(list(
        list(duration = 100)
      ), auto_unbox = TRUE))
    )
  }
  
  result <- httr2::with_mocked_responses(mock_fn, {
    motis_one_to_many_parallel(
      one, many,
      mode = "WALK",
      .server = "http://localhost:8080",
      progress = FALSE,
      spatial_filter = FALSE,
      spatial_sort = TRUE
    )
  })
  
  # With spatial_sort=TRUE, origins should be sorted by lat: A (58), B (59), C (60)
  expect_equal(nrow(result), 3)
  expect_equal(result$from_id, c("A", "B", "C"))
})

test_that("invalid mode parameter is rejected", {
  one <- data.frame(lat = 59.3, lon = 18.0, id = "O1")
  many <- data.frame(lat = 60.0, lon = 18.5, id = "D1")
  
  # Invalid mode should error
  expect_error(
    motis_one_to_many_parallel(
      one, many,
      mode = "INVALID_MODE",
      .server = "http://localhost:8080",
      progress = FALSE
    ),
    "should be one of"
  )
})

test_that("invalid batch_size parameter is rejected", {
  one <- data.frame(lat = 59.3, lon = 18.0, id = "O1")
  many <- data.frame(lat = 60.0, lon = 18.5, id = "D1")
  
  # Zero or negative batch_size should error
  expect_error(
    motis_one_to_many_parallel(
      one, many,
      mode = "WALK",
      batch_size = 0,
      .server = "http://localhost:8080",
      progress = FALSE
    )
  )
  
  expect_error(
    motis_one_to_many_parallel(
      one, many,
      mode = "WALK",
      batch_size = -5,
      .server = "http://localhost:8080",
      progress = FALSE
    )
  )
})

test_that("server readiness polling - success after delay", {
  # Mock success on 2nd attempt
  calls <- 0
  mock_fn <- function(req) {
    calls <<- calls + 1
    if (calls == 1) stop("Server not ready")
    httr2::response(status_code = 200)
  }
  
  httr2::with_mocked_responses(mock_fn, {
    # Use small timeout and interval for testing
    expect_invisible(rmotis:::.wait_for_server("http://localhost:8080", timeout = 5, poll_interval = 0.01))
  })
  expect_equal(calls, 2)
})

test_that("server readiness polling - timeout warning", {
  mock_fn <- function(req) stop("Server down")
  
  httr2::with_mocked_responses(mock_fn, {
    expect_warning(
      rmotis:::.wait_for_server("http://localhost:8080", timeout = 0.2, poll_interval = 0.05),
      "did not respond"
    )
  })
})

test_that("motis_one_to_many_parallel calls server polling", {
  one <- data.frame(lat = 59.3, lon = 18.0, id = "O1")
  many <- data.frame(lat = 60.0, lon = 18.5, id = "D1")
  
  calls <- 0
  mock_fn <- function(req) {
    calls <<- calls + 1
    # First call is readiness check (root path), second is API call
    if (grepl("api/v1/one-to-many", req$url)) {
      return(httr2::response(
        status_code = 200,
        headers = list(`Content-Type` = "application/json"),
        body = charToRaw(jsonlite::toJSON(list(list(duration = 100)), auto_unbox = TRUE))
      ))
    }
    # Readiness check
    httr2::response(status_code = 200)
  }
  
  withr::with_options(list(rmotis.wait_for_server = TRUE), { httr2::with_mocked_responses(mock_fn, {
    motis_one_to_many_parallel(
      one, many,
      .server = "http://localhost:8080",
      progress = FALSE,
      spatial_filter = FALSE,
      spatial_sort = FALSE
    )
  })})
  
  # At least 2 calls: 1 readiness check + 1 API call
  expect_gte(calls, 2)
})
