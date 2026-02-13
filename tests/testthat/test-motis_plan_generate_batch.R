# tests/testthat/test-motis_plan_generate_batch.R

test_that("motis_plan_generate_batch generates paired queries correctly", {
  tmp <- tempfile()
  on.exit(unlink(tmp))

  origins <- data.frame(lat = c(59.1, 59.2), lon = c(18.1, 18.2))
  dests <- data.frame(lat = c(59.3, 59.4), lon = c(18.3, 18.4))

  n <- motis_plan_generate_batch(origins, dests, tmp, time = "2025-01-01 12:00:00")
  expect_equal(n, 2)

  lines <- readLines(tmp)
  expect_length(lines, 2)
  # The comma is encoded as %2C by curl::curl_escape
  expect_match(lines[1], "^/api/v1/plan\\?fromPlace=59\\.1%2C18\\.1")
  expect_match(lines[1], "toPlace=59\\.3%2C18\\.3")
  # Time format should be ISO8601 UTC
  expect_match(lines[1], "time=\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z")
})

test_that("motis_plan_generate_batch handles all_pairs (Cartesian product)", {
  tmp <- tempfile()
  on.exit(unlink(tmp))

  origins <- c("A", "B")
  dests <- c("C", "D", "E")

  n <- motis_plan_generate_batch(origins, dests, tmp, all_pairs = TRUE)
  expect_equal(n, 6)

  lines <- readLines(tmp)
  expect_length(lines, 6)
  # A->C, A->D, A->E, B->C, B->D, B->E
  expect_match(lines[1], "fromPlace=A&toPlace=C")
  expect_match(lines[3], "fromPlace=A&toPlace=E")
  expect_match(lines[4], "fromPlace=B&toPlace=C")
})

test_that("motis_plan_generate_batch handles vectorised time", {
  tmp <- tempfile()
  on.exit(unlink(tmp))

  origins <- c("A", "B")
  dests <- c("C", "D")
  times <- as.POSIXct(c("2025-01-01 10:00:00", "2025-01-01 11:00:00"), tz = "UTC")

  motis_plan_generate_batch(origins, dests, tmp, time = times)
  lines <- readLines(tmp)

  expect_match(lines[1], "time=2025-01-01T10:00:00Z")
  expect_match(lines[2], "time=2025-01-01T11:00:00Z")
})

test_that("motis_plan_generate_batch handles additional parameters and URL encoding", {
  tmp <- tempfile()
  on.exit(unlink(tmp))

  # Station IDs with spaces/special chars
  origins <- c("Stockholm Central")
  dests <- c("Göteborg Central")

  motis_plan_generate_batch(
    origins, dests, tmp,
    directModes = c("WALK", "CAR"), # Character vector should be collapsed
    maxTravelTime = 3600
  )

  lines <- readLines(tmp)
  # Stockholm Central -> Stockholm%20Central
  # Göteborg Central -> G%C3%B6teborg%20Central
  expect_match(lines[1], "fromPlace=Stockholm%20Central")
  expect_match(lines[1], "toPlace=G%C3%B6teborg%20Central")
  expect_match(lines[1], "directModes=WALK%2CCAR")
  expect_match(lines[1], "maxTravelTime=3600")
})

test_that("motis_plan_generate_batch supports appending", {
  tmp <- tempfile()
  on.exit(unlink(tmp))

  motis_plan_generate_batch("A", "B", tmp)
  motis_plan_generate_batch("C", "D", tmp, append = TRUE)

  lines <- readLines(tmp)
  expect_length(lines, 2)
  expect_match(lines[1], "fromPlace=A")
  expect_match(lines[2], "fromPlace=C")
})

test_that("motis_plan_generate_batch fails on invalid API parameters", {
  tmp <- tempfile()
  on.exit(unlink(tmp))

  # Wrong type for a known parameter
  # `detailedTransfers` expects a boolean, passing a string should fail verification
  expect_error(
    motis_plan_generate_batch("A", "B", tmp, detailedTransfers = "invalid_boolean"),
    "Parameter 'detailedTransfers' must be logical"
  )

  # Wrong type for a numeric parameter
  expect_error(
    motis_plan_generate_batch("A", "B", tmp, maxTravelTime = "invalid_number"),
    "Parameter 'maxTravelTime' must be numeric"
  )
})
