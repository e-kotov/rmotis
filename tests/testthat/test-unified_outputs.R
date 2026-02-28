
options(rmotis.wait_for_server = FALSE)

# Mock origins and destinations
one <- data.frame(lat = c(59.3, 59.4), lon = c(18.0, 18.1), id = c("O1", "O2"))
many <- data.frame(lat = c(60.0, 60.1), lon = c(18.5, 18.6), id = c("D1", "D2"))

# Mock API response function
mock_api_fn <- function(req) {
  httr2::response(
    status_code = 200,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(jsonlite::toJSON(list(
      list(duration = 100, distance = 1000),
      list(duration = 200, distance = 2000)
    ), auto_unbox = TRUE))
  )
}

test_that("CSV output works via unified function", {
  csv_file <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_file), add = TRUE)
  
  httr2::with_mocked_responses(mock_api_fn, {
    motis_one_to_many(one, many, engine = "api", .server = "http://motis.fake", 
                      output_path = csv_file, parallel = FALSE, spatial_filter = FALSE)
  })
  
  expect_true(file.exists(csv_file))
  res <- utils::read.csv(csv_file)
  expect_equal(nrow(res), 4)
  expect_true(all(c("from_id", "to_id", "duration_s") %in% names(res)))
})

test_that("Partitioned Parquet output works", {
  skip_if_not_installed("arrow")
  
  pq_dir <- tempfile("pq_")
  on.exit(unlink(pq_dir, recursive = TRUE), add = TRUE)
  
  httr2::with_mocked_responses(mock_api_fn, {
    # Use batch_size = 1 to force multiple partitions
    motis_one_to_many(one, many, engine = "api", .server = "http://motis.fake", 
                      output_path = pq_dir, parallel = FALSE, batch_size = 1, spatial_filter = FALSE)
  })
  
  expect_true(dir.exists(pq_dir))
  pq_files <- list.files(pq_dir, pattern = "\\.parquet$")
  expect_gte(length(pq_files), 2)
  
  ds <- arrow::open_dataset(pq_dir)
  res <- dplyr::collect(ds)
  expect_equal(nrow(res), 4)
})

test_that("DuckDB output works", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  
  db_file <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db_file), add = TRUE)
  
  httr2::with_mocked_responses(mock_api_fn, {
    motis_one_to_many(one, many, engine = "api", .server = "http://motis.fake", 
                      output_path = db_file, parallel = FALSE, spatial_filter = FALSE)
  })
  
  expect_true(file.exists(db_file))
  con <- DBI::dbConnect(duckdb::duckdb(), db_file)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  res <- DBI::dbReadTable(con, "routing_results")
  expect_equal(nrow(res), 4)
})
