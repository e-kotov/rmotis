options(rmotis.wait_for_server = FALSE)

test_that("motis_one_to_many_intermodal function exists", {
  expect_true(exists("motis_one_to_many_intermodal"))
})

test_that("motis_one_to_many_intermodal works with mocked POST request", {
  # Mocking Luxembourg coordinates
  origins <- data.frame(
    id = "LuxCentral",
    lat = 49.599984,
    lon = 6.134208
  )
  dests <- data.frame(
    id = c("Esch", "Ettelbruck"),
    lat = c(49.4938, 49.8471),
    lon = c(5.9814, 6.1041)
  )
  
  mock_resp_json <- '[
    {"duration": 1800, "distance": 20000},
    {"duration": 2400, "distance": 35000}
  ]'
  
  mock_resp <- httr2::response(
    status_code = 200,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(mock_resp_json)
  )
  
  mock_perform <- function(req, ...) {
    # Check that the URL is correct
    expect_equal(req$url, "http://localhost:8080/api/experimental/one-to-many-intermodal")
    
    # Check body
    body_data <- req$body$data
    expect_equal(body_data$maxTravelTime, 60)
    expect_equal(body_data$one, "49.599984;6.134208")
    expect_equal(body_data$mode, "TRANSIT")
    
    mock_resp
  }
  
  testthat::with_mocked_bindings(
    req_perform = mock_perform,
    code = {
      res <- motis_one_to_many_intermodal(
        one = origins,
        many = dests,
        max_travel_time = 60,
        withDistance = TRUE,
        .server = "http://localhost:8080",
        parallel = FALSE,
        progress = FALSE,
        spatial_filter = FALSE,
        spatial_sort = FALSE
      )
      
      expect_s3_class(res, "data.frame")
      expect_equal(nrow(res), 2)
      expect_equal(res$from_id, rep("LuxCentral", 2))
      expect_equal(res$to_id, c("Esch", "Ettelbruck"))
      expect_equal(res$duration_s, c(1800, 2400))
      expect_equal(res$distance_m, c(20000, 35000))
    },
    .package = "httr2"
  )
})

test_that("motis_one_to_many_intermodal works with batch engine (CLI)", {
  skip_on_cran()
  skip_on_os("windows")

  one <- data.frame(lat = 49.6, lon = 6.1, id = "Lux")
  many <- data.frame(lat = 49.5, lon = 6.0, id = "Esch")
  
  # Create a dummy MOTIS script
  dummy_bin_dir <- tempfile("motis_bin_")
  dir.create(dummy_bin_dir)
  dummy_motis <- file.path(dummy_bin_dir, "motis")
  
  # The dummy script will just echo a mock response for each query line
  sys_script <- c(
    "#!/bin/sh",
    "QUERY_FILE=$3",
    "RESP_FILE=$5",
    "# Verify that the query contains maxTravelTime",
    "if grep -q 'maxTravelTime=' \"$QUERY_FILE\"; then",
    "  lines=$(wc -l < \"$QUERY_FILE\")",
    "  for i in $(seq 1 $lines); do",
    "    echo '[{\"duration\":1200,\"distance\":20000}]' >> \"$RESP_FILE\"",
    "  done",
    "else",
    "  echo 'Error: maxTravelTime missing' >&2",
    "  exit 1",
    "fi"
  )
  writeLines(sys_script, dummy_motis)
  Sys.chmod(dummy_motis, "0755")
  on.exit(unlink(dummy_bin_dir, recursive = TRUE), add = TRUE)
  
  mock_intermodal_build <- function(...) {
    list(...) # just return args
  }
  
  testthat::with_mocked_bindings(
    mc_oneToManyIntermodalPost = mock_intermodal_build,
    code = {
      res <- motis_one_to_many_intermodal(
        one, many, 
        engine = "batch",
        data_dir = ".", 
        motis_path = dummy_bin_dir,
        progress = FALSE,
        spatial_filter = FALSE,
        spatial_sort = FALSE
      )
      
      expect_s3_class(res, "data.frame")
      expect_equal(nrow(res), 1)
      expect_equal(res$duration_s, 1200)
    },
    .package = "motis.client"
  )
})

test_that("motis_one_to_many_intermodal works in parallel (mocked)", {
  one <- data.frame(lat = c(49.6, 49.7), lon = c(6.1, 6.2), id = c("O1", "O2"))
  many <- data.frame(lat = 49.5, lon = 6.0, id = "D1")
  
  mock_resp_json <- '[{"duration": 1000}]'
  mock_resp <- httr2::response(
    status_code = 200,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(mock_resp_json)
  )
  
  # httr2::req_perform_parallel returns a list of responses
  mock_parallel <- function(reqs, ...) {
    replicate(length(reqs), mock_resp, simplify = FALSE)
  }
  
  testthat::with_mocked_bindings(
    req_perform_parallel = mock_parallel,
    code = {
      res <- motis_one_to_many_intermodal(
        one, many, 
        parallel = TRUE,
        backend = "httr2",
        batch_size = 1,
        progress = FALSE,
        spatial_filter = FALSE,
        spatial_sort = FALSE,
        .server = "http://localhost:8080"
      )
      
      expect_equal(nrow(res), 2)
      expect_equal(res$duration_s, c(1000, 1000))
    },
    .package = "httr2"
  )
})

test_that("motis_one_to_many_intermodal spatial filter works", {
  # Origin at Lux Central
  one <- data.frame(lat = 49.5999, lon = 6.1342, id = "Lux")
  # Dest 1: Esch (close enough for 100km/h and 60 min)
  # Dest 2: Paris (far away)
  many <- data.frame(
    lat = c(49.4938, 48.8566),
    lon = c(5.9814, 2.3522),
    id = c("Esch", "Paris")
  )
  
  mock_resp_json <- '[{"duration": 1200}]'
  mock_resp <- httr2::response(
    status_code = 200,
    body = charToRaw(mock_resp_json)
  )
  
  mock_perform <- function(req, ...) {
    body_data <- req$body$data
    # Should only have 1 destination (Esch)
    expect_length(body_data$many, 1)
    expect_match(body_data$many[[1]], "49.4938;5.9814")
    mock_resp
  }
  
  testthat::with_mocked_bindings(
    req_perform = mock_perform,
    code = {
      res <- motis_one_to_many_intermodal(
        one, many,
        max_travel_time = 60, # 60 min * 100 km/h = 100 km radius
        parallel = FALSE,
        progress = FALSE,
        spatial_filter = TRUE,
        spatial_sort = FALSE,
        .server = "http://localhost:8080"
      )
      
      expect_equal(nrow(res), 1)
      expect_equal(res$to_id, "Esch")
    },
    .package = "httr2"
  )
})
