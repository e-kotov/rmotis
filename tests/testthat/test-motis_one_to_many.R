test_that("motis_one_to_many works with mocked POST request", {
  # Define the mock response data
  # The API returns a list of objects
  mock_response_json <- '[
    {"one_id": "one_coords", "many_id": "1", "duration": 600, "distance": 1000},
    {"one_id": "one_coords", "many_id": "2", "duration": 1200, "distance": 2000}
  ]'
  
  # Create a mock response object
  # We use httr2::response to create a valid response object
  mock_resp <- httr2::response(
    status_code = 200, 
    headers = list("Content-Type" = "application/json"),
    body = charToRaw(mock_response_json)
  )

  # Validate inputs and return mock response
  mock_perform <- function(req, ...) {
    # Verify request details to ensure request is constructed correctly
    expect_equal(req$url, "http://localhost:8080/api/v1/one-to-many")
    expect_equal(req$method, "POST")
    
    # Check body content
    body_data <- req$body$data
    expect_type(body_data, "list")
    
    # Check specific fields
    expect_equal(body_data$mode, "CAR")
    expect_equal(body_data$elevationCosts, "NONE")
    # Verify coordinates formatting
    # one: 59.3304;18.0583
    # many: 59.3304;18.0583, 59.3305;18.0584
    expect_equal(body_data$one, "59.3304;18.0583")
    expect_length(body_data$many, 2)
    expect_equal(body_data$many[[1]], "59.3304;18.0583")
    expect_equal(body_data$many[[2]], "59.3305;18.0584")

    mock_resp
  }

  # Use testthat's mocking to intercept httr2::req_perform
  # We use with_mocked_bindings to mock the function in the package's namespace or where it's called.
  # Since motis_one_to_many calls httr2::req_perform, we mock it.
  
  testthat::with_mocked_bindings(
    req_perform = mock_perform,
    code = {
      dest <- data.frame(lat = 59.3304, lon = 18.0583)
      origins <- data.frame(
        id = 1:2,
        lat = c(59.3304, 59.3305),
        lon = c(18.0583, 18.0584)
      )

      # Test arrive_by = FALSE (One to Many)
      x_out <- motis_one_to_many(
        one = dest,
        many = origins,
        mode = "CAR",
        distance = TRUE,
        .server = "http://localhost:8080",
        max = 3600
      )

      expect_s3_class(x_out, "data.frame")
      expect_equal(names(x_out), c("from_id", "to_id", "duration_s", "distance_m"))
      expect_equal(nrow(x_out), 2)
      expect_equal(x_out$duration_s, c(600, 1200))
      expect_equal(x_out$distance_m, c(1000, 2000))
      
      # Verify ID mapping logic
      expect_equal(x_out$from_id, c("1", "1")) 
      expect_equal(x_out$to_id, c("1", "2"))

      # Test arrive_by = TRUE (Many to One)
      x_in <- motis_one_to_many(
        one = dest,
        many = origins,
        mode = "CAR",
        arrive_by = TRUE,
        distance = TRUE,
        .server = "http://localhost:8080"
      )

      expect_equal(names(x_in), c("from_id", "to_id", "duration_s", "distance_m"))
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
    # Verify sf coordinate extraction
    body_data <- req$body$data
    # Check formatted string from sf
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
        one = dest_sf,
        many = origins_sf,
        mode = "CAR",
        .server = "http://localhost:8080"
      )
      
      expect_s3_class(res, "data.frame")
      expect_equal(nrow(res), 3)
      expect_equal(res$duration_s, c(500, 600, 700))
    },
    .package = "httr2"
  )
})
