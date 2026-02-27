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
  
  # Use with_mocked_bindings to bypass mc_oneToMany validation if needed,
  # but here we rely on the fact that infra should work.
  # mc_oneToManyIntermodalPost validation might fail if it tries to call real server.
  # So I will mock motis.client::mc_oneToManyIntermodalPost too.
  
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
