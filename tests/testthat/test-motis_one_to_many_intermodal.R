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
    expect_equal(req$url, "http://motis.fake/api/experimental/one-to-many-intermodal")
    
    # Check body
    body_data <- req$body$data
    expect_equal(body_data$maxTravelTime, 60)
    expect_equal(body_data$one, "49.599984;6.134208")
    
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
        .server = "http://motis.fake",
        parallel = FALSE,
        progress = FALSE,
        spatial_filter_km = NULL, spatial_sort = FALSE
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

  one <- data.frame(lat = 49.6, lon = 6.1, id = "Lux")
  many <- data.frame(lat = 49.5, lon = 6.0, id = "Esch")
  
  # Mock processx::run to simulate MOTIS CLI
  mock_run <- function(command, args, ...) {
    # Find response and query files in args
    q_idx <- which(args == "-q") + 1
    r_idx <- which(args == "-r") + 1
    
    query_file <- args[q_idx]
    response_file <- args[r_idx]
    
    # Verify that the query contains maxTravelTime
    queries <- readLines(query_file)
    expect_true(any(grepl("maxTravelTime", queries)))
    
    # Generate mock response
    writeLines('[{"duration":1200,"distance":20000}]', response_file)
    
    list(status = 0L, stdout = "MOTIS Batch Success", stderr = "")
  }
  
  # Mock resolve_motis_cmd to return a fake path
  mock_resolve <- function(...) "/usr/local/bin/motis"
  
  testthat::with_mocked_bindings(
    run = mock_run,
    .package = "processx",
    code = {
      testthat::with_mocked_bindings(
        resolve_motis_cmd = mock_resolve,
        .package = "rmotis",
        code = {
      # Use withr to ensure polling is off
      withr::with_options(list(rmotis.wait_for_server = FALSE), {
        res <- motis_one_to_many_intermodal(
          one, many, 
          engine = "batch",
          data_dir = ".", 
          progress = FALSE,
          spatial_filter_km = NULL, spatial_sort = FALSE,
          parallel = FALSE
        )
      })
      
      expect_s3_class(res, "data.frame")
      expect_equal(nrow(res), 1)
      expect_equal(res$duration_s, 1200)
    })
    }
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
        spatial_filter_km = NULL, spatial_sort = FALSE,
        .server = "http://motis.fake"
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
  # Dest 1: Esch (close enough for 100km radius)
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
        max_travel_time = 60,
        parallel = FALSE,
        progress = FALSE,
        spatial_filter_km = 100,
        spatial_sort = FALSE,
        .server = "http://motis.fake"
      )
      
      expect_equal(nrow(res), 1)
      expect_equal(res$to_id, "Esch")
    },
    .package = "httr2"
  )
})
