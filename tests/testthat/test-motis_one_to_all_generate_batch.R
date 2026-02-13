test_that("motis_one_to_all_generate_batch works", {
  skip_if_not_installed("curl")
  
  output_file <- tempfile(fileext = ".txt")
  on.exit(unlink(output_file))
  
  one <- c("59.3,18.0", "59.4,18.1")
  time <- as.POSIXct("2026-02-13 12:00:00", tz = "UTC")
  
  n <- motis_one_to_all_generate_batch(one, output_file, time = time, max_travel_time = 60)
  expect_equal(n, 2L)
  
  lines <- readLines(output_file)
  expect_length(lines, 2)
  
  # Check for endpoint
  expect_match(lines[1], "/api/v1/one-to-all")
  # Check for escaped coordinates in 'one' (59.3,18.0) -> 59.3%2C18
  # Note: .format_place might round or change format slightly.
  # "59.3,18.0" as character is kept as is by .format_place if it matches format? 
  # Actually .format_place for character: if (is.character(place)) return(unname(place))
  expect_match(lines[1], "one=59.3%2C18.0")
  expect_match(lines[1], "time=2026-02-13T12:00:00Z")
  expect_match(lines[1], "maxTravelTime=60")
})

test_that("motis_one_to_all_generate_batch validates params", {
  output_file <- tempfile(fileext = ".txt")
  on.exit(unlink(output_file))
  
  one <- "59.3,18.0"
  
  # Invalid additional param type
  expect_error(
    motis_one_to_all_generate_batch(one, output_file, maxTransfers = "a"),
    "Parameter 'maxTransfers' must be numeric/integer"
  )
})
