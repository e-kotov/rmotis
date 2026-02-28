test_that("motis_one_to_many_plan_batch creates query file for multiple origins", {
  # Test data: 3 origins, 2 destinations
  one <- data.frame(
    lat = c(59.3, 59.4, 59.5),
    lon = c(18.0, 18.1, 18.2),
    id = c("A", "B", "C")
  )
  many <- data.frame(
    lat = c(60.0, 60.1),
    lon = c(18.5, 18.6),
    id = c("X", "Y")
  )
  
  temp_file <- tempfile("plan_batch_", fileext = ".txt")
  on.exit(unlink(c(temp_file, paste0(temp_file, ".meta"))), add = TRUE)
  
  result <- motis_one_to_many_plan_batch(
    one, many,
    output_file = temp_file,
    mode = "WALK",
    spatial_filter_km = NULL,  # Disable to test basic functionality
    spatial_sort = FALSE,
    progress = FALSE
  )
  
  expect_true(file.exists(temp_file))
  expect_true(file.exists(paste0(temp_file, ".meta")))
  
  # Check result structure
  expect_equal(result$file, temp_file)
  expect_equal(result$n_lines, 3)  # 3 origins -> 3 query lines
  expect_type(result$file_size, "double")
  
  # Check query file content
  query_lines <- readLines(temp_file)
  expect_length(query_lines, 3)
  
  # Check metadata file content
  meta_lines <- readLines(paste0(temp_file, ".meta"))
  expect_length(meta_lines, 3)
  
  # Each meta line should have 1 origin ID + 2 destination IDs
  for (meta_line in meta_lines) {
    parts <- strsplit(meta_line, "\t")[[1]]
    expect_length(parts, 3)  # 1 origin + 2 destinations
  }
})

test_that("spatial_sort reorders origins in plan_batch", {
  # Origins at different latitudes (unsorted)
  one <- data.frame(
    lat = c(60, 58, 59),
    lon = c(18, 18, 18),
    id = c("C", "A", "B")
  )
  many <- data.frame(lat = 59.5, lon = 18.0, id = "X")
  
  temp_file <- tempfile("plan_batch_sort_", fileext = ".txt")
  on.exit(unlink(c(temp_file, paste0(temp_file, ".meta"))), add = TRUE)
  
  result <- motis_one_to_many_plan_batch(
    one, many,
    output_file = temp_file,
    mode = "WALK",
    spatial_filter_km = NULL,
    spatial_sort = TRUE,
    progress = FALSE
  )
  
  # Check metadata file for ordering (A, B, C by latitude 58, 59, 60)
  meta_lines <- readLines(paste0(temp_file, ".meta"))
  origin_ids <- vapply(strsplit(meta_lines, "\t"), `[`, character(1), 1)
  
  expect_equal(origin_ids, c("A", "B", "C"))
})

test_that("spatial_filter reduces destinations in plan_batch", {
  # 1 origin in Stockholm, destinations: 2 nearby, 1 very far
  one <- data.frame(lat = 59.3, lon = 18.0, id = "Stockholm")
  many <- data.frame(
    lat = c(59.4, 59.5, 35.0),  # Stockholm, Stockholm, Tokyo
    lon = c(18.1, 18.2, 139.7),
    id = c("Near1", "Near2", "Tokyo")
  )
  
  temp_file <- tempfile("plan_batch_filter_", fileext = ".txt")
  on.exit(unlink(c(temp_file, paste0(temp_file, ".meta"))), add = TRUE)
  
  result <- motis_one_to_many_plan_batch(
    one, many,
    output_file = temp_file,
    mode = "WALK",
    max = 7200,
    spatial_filter_km = 50,
    spatial_sort = FALSE,
    progress = FALSE
  )
  
  # Check metadata: should have Near1 and/or Near2 (at least 1, filter out Tokyo)
  meta_lines <- readLines(paste0(temp_file, ".meta"))
  meta_parts <- strsplit(meta_lines, "\t")[[1]]
  
  # First element is origin, rest are destinations
  dest_ids <- meta_parts[-1]
  expect_gte(length(dest_ids), 1)  # At least 1 nearby destination
  expect_lte(length(dest_ids), 2)  # At most 2 (both near destinations)
  expect_false("Tokyo" %in% dest_ids)  # Tokyo should definitely be filtered out
})

test_that("plan_batch skips origins with no destinations in range", {
  # 2 origins: one with nearby dests, one with none
  one <- data.frame(
    lat = c(59.3, 35.0),  # Stockholm, far south
    lon = c(18.0, 139.7),
    id = c("Stockholm", "Far")
  )
  many <- data.frame(
    lat = c(59.4, 59.5),  # Both near Stockholm
    lon = c(18.1, 18.2),
    id = c("Near1", "Near2")
  )
  
  temp_file <- tempfile("plan_batch_skip_", fileext = ".txt")
  on.exit(unlink(c(temp_file, paste0(temp_file, ".meta"))), add = TRUE)
  
  result <- motis_one_to_many_plan_batch(
    one, many,
    output_file = temp_file,
    mode = "WALK",
    max = 7200,
    spatial_filter_km = 50,
    spatial_sort = FALSE,
    progress = FALSE
  )
  
  # Should only have 1 query line (Stockholm), Far is skipped
  expect_equal(result$n_lines, 1)
  
  meta_lines <- readLines(paste0(temp_file, ".meta"))
  expect_length(meta_lines, 1)
  origin_id <- strsplit(meta_lines, "\t")[[1]][1]
  expect_equal(origin_id, "Stockholm")
})

test_that("plan_batch generates correct query file content", {
  one <- data.frame(lat = 59.3, lon = 18.0, id = "Origin1")
  many <- data.frame(
    lat = c(60.0, 60.1),
    lon = c(18.5, 18.6),
    id = c("Dest1", "Dest2")
  )
  
  temp_file <- tempfile("plan_batch_content_", fileext = ".txt")
  on.exit(unlink(c(temp_file, paste0(temp_file, ".meta"))), add = TRUE)
  
  result <- motis_one_to_many_plan_batch(
    one, many,
    output_file = temp_file,
    mode = "BIKE",
    max = 3600,
    spatial_filter_km = NULL,
    spatial_sort = FALSE,
    progress = FALSE
  )
  
  # Read and check query file exists and has content
  query_lines <- readLines(temp_file)
  expect_length(query_lines, 1)
  
  # Validate query contains key parameters (coordinate formatting may vary)
  query <- query_lines[1]
  expect_true(grepl("BIKE", query))
  expect_true(grepl("3600", query))
  expect_true(nchar(query) > 0)
 })
