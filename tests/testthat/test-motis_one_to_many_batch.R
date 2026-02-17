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

test_that("motis_one_to_many_batch respects cores parameter", {
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
  
  custom_dir <- tempfile("motis_batch_cores_")
  dir.create(custom_dir)
  on.exit(unlink(custom_dir, recursive = TRUE), add = TRUE)

  # Test with cores = 2 -> 2 chunks -> 2 lines query for 1 origin
  msgs <- capture.output({
      res <- motis_one_to_many_batch(
        one, many, 
        data_dir = ".", 
        motis_path = dummy_bin_dir,
        output_dir = custom_dir,
        cores = 2L,
        echo = TRUE
      )
  }, type = "message")
  
  # Verify 2 lines in query file
  expect_true(any(grepl("Query file: .* 2 lines", msgs)))
  
  # Verify 4 results (2 lines * 2 results/line)
  expect_equal(nrow(res), 4)
  expect_equal(res$to_id, c("A", "B", "C", "D"))
})
