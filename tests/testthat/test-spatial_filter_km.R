
options(rmotis.wait_for_server = FALSE)

test_that("spatial_filter_km correctly filters destinations (mocked)", {
  # Origin: Frankfurt (50.11, 8.68)
  one <- data.frame(lat = 50.11, lon = 8.68, id = "Frankfurt")
  
  # Destinations: 
  # 1. Close (approx 10km away) -> 50.2, 8.68
  # 2. Far (approx 200km away) -> 52.0, 8.68
  many <- data.frame(
    id = c("Close", "Far"),
    lat = c(50.2, 52.0),
    lon = c(8.68, 8.68)
  )
  
  mock_fn <- function(req) {
    body_data <- req$body$data
    # We expect only ONE destination ("Close") to be sent to the API
    # because "Far" is 200km away and we will set filter to 50km.
    expect_equal(length(body_data$many), 1)
    expect_equal(body_data$many[[1]], "50.2;8.68")
    
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw(jsonlite::toJSON(list(
        list(duration = 600, distance = 10000)
      ), auto_unbox = TRUE))
    )
  }
  
  httr2::with_mocked_responses(mock_fn, {
    res <- motis_one_to_many(
      one = one, 
      many = many, 
      spatial_filter_km = 50, # 50km radius
      parallel = FALSE,
      progress = FALSE,
      .server = "http://motis.fake"
    )
    
    expect_equal(nrow(res), 1)
    expect_equal(res$to_id, "Close")
  })
})

test_that("Old spatial_filter and max_speed_kmh arguments are deprecated/removed", {
  one <- data.frame(lat = 50.11, lon = 8.68, id = "O1")
  many <- data.frame(lat = 50.12, lon = 8.69, id = "D1")
  
  # Should either error or warn depending on implementation choice
  # Spec said "Remove", so let's expect an error if they are passed
  expect_error(
    motis_one_to_many(one, many, spatial_filter = TRUE),
    "deprecated"
  )
  expect_error(
    motis_one_to_many(one, many, max_speed_kmh = 100),
    "deprecated"
  )
})

test_that("spatial_filter_km works with intermodal API (mocked)", {
  one <- data.frame(lat = 50.11, lon = 8.68, id = "O1")
  many <- data.frame(lat = 50.12, lon = 8.69, id = "D1")
  
  mock_fn <- function(req) {
    # Verify it uses the intermodal endpoint
    expect_match(req$url, "one-to-many-intermodal")
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw(jsonlite::toJSON(list(
        list(duration = 300)
      ), auto_unbox = TRUE))
    )
  }
  
  httr2::with_mocked_responses(mock_fn, {
    res <- motis_one_to_many_intermodal(
      one = one, 
      many = many, 
      spatial_filter_km = 10,
      parallel = FALSE,
      progress = FALSE,
      .server = "http://motis.fake"
    )
    expect_equal(nrow(res), 1)
  })
})
