test_that(".extract_coords handles data.frame with lat/lon columns", {
  df <- data.frame(lat = c(59.3, 59.4), lon = c(18.0, 18.1))
  coords <- rmotis:::.extract_coords(df)
  
  expect_type(coords, "double")
  expect_equal(dim(coords), c(2, 2))
  expect_equal(colnames(coords), c("lat", "lon"))
  expect_equal(coords[, "lat"], c(59.3, 59.4))
  expect_equal(coords[, "lon"], c(18.0, 18.1))
})

test_that(".extract_coords handles data.frame with latitude/longitude columns", {
  df <- data.frame(latitude = c(50, 51), longitude = c(8, 9))
  coords <- rmotis:::.extract_coords(df)
  
  expect_equal(coords[, "lat"], c(50, 51))
  expect_equal(coords[, "lon"], c(8, 9))
})

test_that(".extract_coords errors on data.frame without coordinate columns", {
  df <- data.frame(x = 1:3, y = 4:6)
  expect_error(
    rmotis:::.extract_coords(df),
    "must contain coordinate columns"
  )
})

test_that(".extract_coords handles sf POINT geometry", {
  skip_if_not_installed("sf")
  
  pts <- sf::st_as_sf(
    data.frame(lat = c(59.3, 59.4), lon = c(18.0, 18.1)),
    coords = c("lon", "lat"),
    crs = 4326
  )
  
  coords <- rmotis:::.extract_coords(pts)
  
  expect_equal(dim(coords), c(2, 2))
  expect_equal(colnames(coords), c("lat", "lon"))
  expect_equal(coords[, "lat"], c(59.3, 59.4))
  expect_equal(coords[, "lon"], c(18.0, 18.1))
})

test_that(".extract_coords handles matrix with named columns", {
  mat <- matrix(c(59.3, 59.4, 18.0, 18.1), ncol = 2)
  colnames(mat) <- c("lat", "lon")
  
  coords <- rmotis:::.extract_coords(mat)
  
  expect_equal(coords[, "lat"], c(59.3, 59.4))
  expect_equal(coords[, "lon"], c(18.0, 18.1))
})

test_that(".extract_coords handles matrix without names by value range", {
  # First column is latitude (in -90 to 90 range)
  mat1 <- matrix(c(59.3, 59.4, 18.0, 18.1), ncol = 2)
  coords1 <- rmotis:::.extract_coords(mat1)
  expect_equal(coords1[, "lat"], c(59.3, 59.4))
  expect_equal(coords1[, "lon"], c(18.0, 18.1))
  
  # First column is longitude (outside -90 to 90)
  mat2 <- matrix(c(118.0, 119.0, 39.9, 40.0), ncol = 2)
  coords2 <- rmotis:::.extract_coords(mat2)
  expect_equal(coords2[, "lat"], c(39.9, 40.0))
  expect_equal(coords2[, "lon"], c(118.0, 119.0))
})

test_that(".extract_coords errors on matrix with wrong number of columns", {
  mat <- matrix(1:9, ncol = 3)
  expect_error(
    rmotis:::.extract_coords(mat),
    "2 columns"
  )
})

test_that(".extract_coords handles character lat;lon strings", {
  places <- c("59.3;18.0", "59.4;18.1")
  coords <- rmotis:::.extract_coords(places)
  
  expect_equal(dim(coords), c(2, 2))
  expect_equal(coords[, "lat"], c(59.3, 59.4))
  expect_equal(coords[, "lon"], c(18.0, 18.1))
})

test_that(".extract_coords handles character coordinates with various separators", {
  # Standard semicolon
  places_semicolon <- c("59.3;18.0", "59.4;18.1")
  coords_semicolon <- rmotis:::.extract_coords(places_semicolon)
  expect_equal(coords_semicolon[, "lat"], c(59.3, 59.4))
  expect_equal(coords_semicolon[, "lon"], c(18.0, 18.1))
  
  # Comma (Google Maps style)
  places_comma <- c("49.5,6.1", "49.6,6.2")
  # This should not trigger warnings
  expect_no_warning({
    coords_comma <- rmotis:::.extract_coords(places_comma)
  })
  expect_equal(coords_comma[, "lat"], c(49.5, 49.6))
  expect_equal(coords_comma[, "lon"], c(6.1, 6.2))
  
  # Mixed separators
  places_mixed <- c("59.3,18.0", "59.4;18.1")
  coords_mixed <- rmotis:::.extract_coords(places_mixed)
  expect_equal(coords_mixed[, "lat"], c(59.3, 59.4))
  expect_equal(coords_mixed[, "lon"], c(18.0, 18.1))
})

test_that(".extract_coords errors on unsupported input type", {
  expect_error(
    rmotis:::.extract_coords(list(a = 1, b = 2)),
    "Unsupported input type"
  )
})

test_that(".extract_coords handles NA coordinates in data.frame", {
  df <- data.frame(lat = c(59.3, NA, 59.5), lon = c(18.0, 18.1, NA))
  coords <- rmotis:::.extract_coords(df)
  
  expect_equal(dim(coords), c(3, 2))
  expect_true(is.na(coords[2, "lat"]))
  expect_true(is.na(coords[3, "lon"]))
  expect_equal(as.numeric(coords[1, "lat"]), 59.3)
})

test_that(".extract_coords handles out-of-range coordinates", {
  # Latitude > 90 should still work (function doesn't validate ranges)
  df <- data.frame(lat = c(95, -100), lon = c(200, -200))
  coords <- rmotis:::.extract_coords(df)
  
  expect_equal(coords[, "lat"], c(95, -100))
  expect_equal(coords[, "lon"], c(200, -200))
})

test_that(".extract_coords handles NULL input gracefully", {
  expect_error(
    rmotis:::.extract_coords(NULL),
    "Unsupported input type"
  )
})
