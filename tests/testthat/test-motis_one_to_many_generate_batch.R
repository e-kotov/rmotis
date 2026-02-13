test_that("motis_one_to_many_generate_batch works", {
  skip_if_not_installed("curl")
  
  output_file <- tempfile(fileext = ".txt")
  on.exit(unlink(output_file))
  
  one <- data.frame(lat = 59.3, lon = 18.0)
  many <- data.frame(lat = c(59.4, 59.5), lon = c(18.1, 18.2))
  
  n <- motis_one_to_many_generate_batch(one, many, output_file, mode = "WALK", max = 3600)
  expect_equal(n, 1L)
  
  lines <- readLines(output_file)
  expect_length(lines, 1)
  
  # Check for endpoint
  expect_match(lines, "/api/v1/one-to-many")
  # Check for escaped coordinates in 'one' (59.3;18) -> 59.3%3B18
  expect_match(lines, "one=59.3%3B18")
  # Check for escaped coordinates in 'many' (59.4;18.1,59.5;18.2)
  expect_match(lines, "many=59.4%3B18.1%2C59.5%3B18.2")
  # Check for other params
  expect_match(lines, "mode=WALK")
  expect_match(lines, "max=3600")
})

test_that("motis_one_to_many_generate_batch validates params", {
  output_file <- tempfile(fileext = ".txt")
  on.exit(unlink(output_file))
  
  one <- "59.3,18.0"
  many <- "59.4,18.1"
  
  # Invalid mode
  expect_error(
    motis_one_to_many_generate_batch(one, many, output_file, mode = "INVALID"),
    "'arg' should be one of"
  )
  
  # Invalid additional param type
  expect_error(
    motis_one_to_many_generate_batch(one, many, output_file, mode = "WALK", maxTransfers = "a"),
    "Parameter 'maxTransfers' must be numeric/integer"
  )
})
