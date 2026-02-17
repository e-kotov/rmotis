test_that("motis_one_to_many_batch requires valid data_dir", {
  one <- data.frame(lat = 59.3, lon = 18.0, id = "A")
  many <- data.frame(lat = c(59.4, 59.5), lon = c(18.1, 18.2),
                     id = c("X", "Y"))

  expect_error(
    motis_one_to_many_batch(one, many, data_dir = "/nonexistent/path"),
    "No such file or directory"
  )
})

test_that("motis_one_to_many_batch respects output_dir and keep_files", {
  skip_on_cran()
  skip_on_os("windows") # Script creation is easier on unix for now

  one <- data.frame(lat = 59.3, lon = 18.0, id = "A")
  many <- data.frame(lat = c(59.4, 59.5), lon = c(18.1, 18.2),
                     id = c("X", "Y"))
  
  # Create a dummy MOTIS script
  dummy_bin_dir <- tempfile("motis_bin_")
  dir.create(dummy_bin_dir)
  dummy_motis <- file.path(dummy_bin_dir, "motis")
  
  sys_script <- c(
    "#!/bin/sh",
    "QUERY_FILE=$3",
    "RESP_FILE=$5",
    "cat $QUERY_FILE > /dev/null", 
    "lines=$(wc -l < \"$QUERY_FILE\")",
    "for i in $(seq 1 $lines); do",
    "  echo '[{\"duration\":123,\"distance\":456},{\"duration\":789,\"distance\":1011}]' >> \"$RESP_FILE\"",
    "done"
  )
  writeLines(sys_script, dummy_motis)
  Sys.chmod(dummy_motis, "0755")
  on.exit(unlink(dummy_bin_dir, recursive = TRUE), add = TRUE)
  
  custom_dir <- tempfile("motis_batch_test_")
  dir.create(custom_dir)
  on.exit(unlink(custom_dir, recursive = TRUE), add = TRUE)
  
  # Capture output
  out <- capture.output({
    msgs <- capture.output({
      res <- motis_one_to_many_batch(
        one, many, 
        data_dir = ".", 
        motis_path = dummy_bin_dir,
        output_dir = custom_dir,
        keep_files = TRUE,
        echo = TRUE
      )
    }, type = "message")
  })
  
  # Check logs
  expect_true(any(grepl("Query file: .* bytes, .* lines", msgs)))
  expect_true(any(grepl("Metadata file: .* bytes, .* lines", msgs)))
  expect_true(any(grepl("Response file: .* bytes, .* lines", msgs)))
  
  # Check files
  files <- list.files(custom_dir)
  expect_gte(length(files), 3) # query, meta, response
  expect_true(any(grepl("motis_query_.*\\.txt$", files)))
  expect_true(any(grepl("motis_response_.*\\.txt$", files)))
  
  # Check result content
  expect_equal(nrow(res), 2)
  expect_equal(res$duration_s, c(123, 789))
})

test_that("motis_one_to_many_batch respects split parameter", {
  skip_on_cran()
  skip_on_os("windows") 

  # 1 origin, 4 destinations
  one <- data.frame(lat = 50, lon = 8, id = "1")
  many <- data.frame(lat = c(50.1, 50.2, 50.3, 50.4), 
                     lon = c(8.1, 8.2, 8.3, 8.4),
                     id = c("A", "B", "C", "D"))
  
  dummy_bin_dir <- tempfile("motis_bin_")
  dir.create(dummy_bin_dir)
  dummy_motis <- file.path(dummy_bin_dir, "motis")
  
  # Return 2 results per line (chunk)
  sys_script <- c(
    "#!/bin/sh",
    "QUERY_FILE=$3",
    "RESP_FILE=$5",
    "lines=$(wc -l < \"$QUERY_FILE\")",
    "for i in $(seq 1 $lines); do",
    "  echo '[{\"duration\":100},{\"duration\":200}]' >> \"$RESP_FILE\"",
    "done"
  )
  writeLines(sys_script, dummy_motis)
  Sys.chmod(dummy_motis, "0755")
  on.exit(unlink(dummy_bin_dir, recursive = TRUE), add = TRUE)
  
  custom_dir <- tempfile("motis_batch_split_")
  dir.create(custom_dir)
  on.exit(unlink(custom_dir, recursive = TRUE), add = TRUE)

  # Test with split = 2 -> 2 chunks -> 2 lines query for 1 origin
  msgs <- capture.output({
      res <- motis_one_to_many_batch(
        one, many, 
        data_dir = ".", 
        motis_path = dummy_bin_dir,
        output_dir = custom_dir,
        split = 2L,
        spatial_filter = FALSE,  # Disable to test split behavior
        spatial_sort = FALSE,    # Disable to focus on split
        echo = TRUE
      )
  }, type = "message")
  
  # Verify 2 lines in query file
  expect_true(any(grepl("Query file: .* 2 lines", msgs)))
  
  # Verify 4 results (2 lines * 2 results/line)
  expect_equal(nrow(res), 4)
  expect_equal(res$to_id, c("A", "B", "C", "D"))
})

test_that("spatial_sort reorders origins by latitude", {
  skip_on_cran()
  skip_on_os("windows")
  
  # 3 origins at different latitudes (unsorted)
  one <- data.frame(
    lat = c(60, 58, 59),
    lon = c(18, 18, 18),
    id = c("C", "A", "B")
  )
  many <- data.frame(lat = 59.5, lon = 18.0, id = "X")
  
  dummy_bin_dir <- tempfile("motis_bin_")
  dir.create(dummy_bin_dir)
  dummy_motis <- file.path(dummy_bin_dir, "motis")
  
  sys_script <- c(
    "#!/bin/sh",
    "lines=$(wc -l < \"$3\")",
    "for i in $(seq 1 $lines); do",
    "  echo '[{\"duration\":100}]' >> \"$5\"",
    "done"
  )
  writeLines(sys_script, dummy_motis)
  Sys.chmod(dummy_motis, "0755")
  on.exit(unlink(dummy_bin_dir, recursive = TRUE), add = TRUE)
  
  custom_dir <- tempfile("motis_batch_sort_")
  dir.create(custom_dir)
  on.exit(unlink(custom_dir, recursive = TRUE), add = TRUE)
  
  res <- motis_one_to_many_batch(
    one, many,
    data_dir = ".",
    motis_path = dummy_bin_dir,
    output_dir = custom_dir,
    keep_files = TRUE,
    spatial_sort = TRUE,
    spatial_filter = FALSE,  # Disable filter to test only sort
    echo = FALSE
  )
  
  # Check metadata file for ordering (A, B, C by latitude 58, 59, 60)
  query_files <- list.files(custom_dir, pattern = "motis_query.*\\.txt$", full.names = TRUE)
  meta_file <- paste0(query_files[1], ".meta")
  meta_lines <- readLines(meta_file)
  
  # Extract first column (origin ID) from each line
  origin_ids <- vapply(strsplit(meta_lines, "\t"), `[`, character(1), 1)
  expect_equal(origin_ids, c("A", "B", "C"))
})

test_that("spatial_filter reduces destinations per origin", {
  skip_on_cran()
  skip_on_os("windows")
  
  # 1 origin in Stockholm, destinations: 2 nearby, 2 very far
  one <- data.frame(lat = 59.3, lon = 18.0, id = "Stockholm")
  many <- data.frame(
    lat = c(59.4, 59.5, 35.0, 51.5),  # Stockholm, Stockholm, Tokyo, London
    lon = c(18.1, 18.2, 139.7, -0.1),
    id = c("Near1", "Near2", "Tokyo", "London")
  )
  
  dummy_bin_dir <- tempfile("motis_bin_")
  dir.create(dummy_bin_dir)
  dummy_motis <- file.path(dummy_bin_dir, "motis")
  
  sys_script <- c(
    "#!/bin/sh",
    "lines=$(wc -l < \"$3\")",
    "for i in $(seq 1 $lines); do",
    "  echo '[{\"duration\":100}]' >> \"$5\"",
    "done"
  )
  writeLines(sys_script, dummy_motis)
  Sys.chmod(dummy_motis, "0755")
  on.exit(unlink(dummy_bin_dir, recursive = TRUE), add = TRUE)
  
  custom_dir <- tempfile("motis_batch_filter_")
  dir.create(custom_dir)
  on.exit(unlink(custom_dir, recursive = TRUE), add = TRUE)
  
  # Use default max = 7200s (2 hours), WALK mode (6 km/h)
  # Max radius ≈ (7200 * 6 / 3600) * 1.2 / 111 ≈ 0.13 degrees
  # Tokyo and London should be filtered out
  res <- motis_one_to_many_batch(
    one, many,
    data_dir = ".",
    motis_path = dummy_bin_dir,
    output_dir = custom_dir,
    keep_files = TRUE,
    mode = "WALK",
    spatial_filter = TRUE,
    echo = FALSE
  )
  
  # Check metadata: should only have Near1, Near2
  query_files <- list.files(custom_dir, pattern = "motis_query.*\\.txt$", full.names = TRUE)
  meta_file <- paste0(query_files[1], ".meta")
  meta_lines <- readLines(meta_file)
  meta_parts <- strsplit(meta_lines, "\t")[[1]]
  
  # First element is origin, rest are destinations
  dest_ids <- meta_parts[-1]
  # Only Near1 (0.1° away) passes the ~0.13° filter; Near2 (0.2° away) is filtered out
  expect_length(dest_ids, 1)
  expect_equal(dest_ids, "Near1")
  expect_false("Tokyo" %in% dest_ids)   # Tokyo definitely filtered
  expect_false("London" %in% dest_ids)  # London definitely filtered
})

test_that("spatial_filter=FALSE includes all destinations", {
  skip_on_cran()
  skip_on_os("windows")
  
  one <- data.frame(lat = 59.3, lon = 18.0, id = "Stockholm")
  many <- data.frame(
    lat = c(59.4, 35.0),
    lon = c(18.1, 139.7),
    id = c("Near", "Tokyo")
  )
  
  dummy_bin_dir <- tempfile("motis_bin_")
  dir.create(dummy_bin_dir)
  dummy_motis <- file.path(dummy_bin_dir, "motis")
  
  sys_script <- c(
    "#!/bin/sh",
    "lines=$(wc -l < \"$3\")",
    "for i in $(seq 1 $lines); do",
    "  echo '[{\"duration\":100},{\"duration\":200}]' >> \"$5\"",
    "done"
  )
  writeLines(sys_script, dummy_motis)
  Sys.chmod(dummy_motis, "0755")
  on.exit(unlink(dummy_bin_dir, recursive = TRUE), add = TRUE)
  
  custom_dir <- tempfile("motis_batch_nofilter_")
  dir.create(custom_dir)
  on.exit(unlink(custom_dir, recursive = TRUE), add = TRUE)
  
  res <- motis_one_to_many_batch(
    one, many,
    data_dir = ".",
    motis_path = dummy_bin_dir,
    output_dir = custom_dir,
    keep_files = TRUE,
    spatial_filter = FALSE,
    echo = FALSE
  )
  
  # Check metadata: should have both Near and Tokyo
  query_files <- list.files(custom_dir, pattern = "motis_query.*\\.txt$", full.names = TRUE)
  meta_file <- paste0(query_files[1], ".meta")
  meta_lines <- readLines(meta_file)
  meta_parts <- strsplit(meta_lines, "\t")[[1]]
  
  dest_ids <- meta_parts[-1]
  expect_length(dest_ids, 2)
  expect_setequal(dest_ids, c("Near", "Tokyo"))
})
